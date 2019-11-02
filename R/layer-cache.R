
# ********************************************************************
# ********************   FILE UTILITY FUNCTIONS   ********************
# ********************************************************************

get_header_txt <- function (from, to)
{
    txt <- "New York pedestrian calibration:"

    if (from == "residential")
        txt <- paste (txt, "residential to subway")
    else if (to == "disperse")
        txt <- paste (txt, "dispersal from subway")
    else
        txt <- paste0 (txt, " subway to ", to)

    return (txt)
}

get_file_name <- function (data_dir, from, to, k, k_scale)
{
    prfx <- ""
    if (k_scale < 0)
    {
        k_scale <- abs (k_scale)
        prfx <- "n"
    }
    ks_p <- sprintf ("%s%03d", prfx, round (k_scale * 100))
    f <- paste0 ("flow-", substr (from, 1, 3), "-", substr (to, 1, 3),
                 "-k", k, "-s", ks_p, ".Rds")
    f <- file.path (data_dir, "calibration", f)
}

is_layer_cached <- function (data_dir, from, to, k, k_scale)
{
    file.exists (get_file_name (data_dir, from, to, k, k_scale))
}

cache_layer <- function (res, data_dir, from, to, k, k_scale)
{
    # save only the necessary columns
    res <- res [, c (".vx0", ".vx1", "edge_", "flow")]

    f <- get_file_name (data_dir, from, to, k, k_scale)
    saveRDS (res, f)
}

format_time_int <- function (st0)
{
    st <- as.integer (difftime (Sys.time (), st0, units = "sec"))
    hh <- floor (st / 3600)
    mm <- floor ((st - hh * 3600) / 60)
    ss <- st - hh * 3600 - mm * 60
    mm <- sprintf ("%02d", mm)
    ss <- sprintf ("%02d", ss)
    paste0 (hh, ":", mm, ":", ss)
}

# ********************************************************************
# *****************   LAYER CALCULATION FUNCTIONS   ******************
# ********************************************************************


get_popdens_data <- function (v, data_dir)
{
    # pop values go up to around 3,000, so ones below this value are removed
    dens_limit <- 0.01

    cache_dir <- file.path (data_dir, "calibration")
    f <- file.path (cache_dir, "popdens-layer.Rds")
    if (file.exists (f))
        res <- readRDS (f)
    else
    {
        nodes_new <- readRDS (file.path (data_dir, "worldpop",
                                         "pop-points.Rds"))
        layer_name <- names (nodes_new) [grep ("ppp_", names (nodes_new))]
        nodes_new$id <- v$id [dodgr::match_points_to_graph (v, nodes_new)]
        index <- which (!(nodes_new [[layer_name]] < dens_limit |
                          is.na (nodes_new [[layer_name]])))
        res <- nodes_new [index, ]
        saveRDS (res, file = f)
    }
    return (res)
}

d_subway_res <- function (net, s, nodes_new, data_dir)
{
    cache_dir <- file.path (data_dir, "calibration")
    f <- file.path (cache_dir, "dmat-subway-res.Rds")
    if (file.exists (f))
        dmat <- readRDS (f)
    else
    {
        dmat <- dodgr::dodgr_distances (net, from = s$id, to = nodes_new$id)
        dmat [is.na (dmat)] <- max (dmat, na.rm = TRUE)
        saveRDS (dmat, file = f)
    }

    return (dmat)
}


# ********************************************************************
# *********************   MISC EXTRA FUNCTIONS   *********************
# ********************************************************************

# This function now takes a *REALLY* long time - like 1 minute or so - with
# several flow columns -> TODO: shuck off to C++-land
flow_to_ped_pts <- function (net_f, p, dp, get_nearest = TRUE)
{
    fcols <- grep ("flow", names (net_f))
    flows <- array (NA, dim = c (nrow (p), length (fcols)))

    for (i in seq (nrow (p)))
    {
        # find edges that flow in and out of that point - these commonly return
        # only NA values, so second approach is implemented
        i1 <- which (net_f$.vx0 == p$id [i])
        i2 <- which (net_f$.vx1 == p$id [i])
        for (j in seq (fcols))
        {
            flows [i, j] <- sum (net_f [i1, fcols [j]], na.rm = TRUE) +
                sum (net_f [i2, fcols [j]], na.rm = TRUE)

            # OR: choose first edges near that observation vertex that have non-zero
            # flows
            if (flows [i, j] == 0 & get_nearest)
            {
                di <- dp [i, ] [order (dp [i, ])]
                f_ord <- net_f [match (net_f$.vx0, names (di)), fcols [j]]
                flow0 <- f_ord [which (f_ord > 0)] [1]
                f_ord <- net_f [match (net_f$.vx1, names (di)), fcols [j]]
                flow1 <- f_ord [which (f_ord > 0)] [1]
                flows [i, j] <- flow0 + flow1
            }
        } # end for j over flow columns
    } # end for i

    return (flows)
}

get_attractor_layer <- function (data_dir, v, type = "education")
{
    type <- match.arg (type, c ("residential", "education", "entertainment",
                                "healthcare", "sustenance", "transportation",
                                "disperse", "centrality"))
    a <- NULL
    if (type == "centrality")
    {
        a <- readRDS (file.path (data_dir, "ny-centrality-vertex.Rds"))
        a$category <- "centrality"
    } else
        a <- readRDS (file.path (data_dir, "osm", "ny-attractors.Rds"))
    # The attractors data contains lots of points outside the bbox of the street
    # network, so have to be reduced to only those within. (Otherwise *ALL*
    # points beyond get aggregated to nearest points, producing anomalously huge
    # values at boundaries.)
    pts <- sf::st_as_sf (a, coords = c ("x", "y"), crs = 4326)
    f <- file.path (tempdir (), "nyc-bb.Rds")
    if (!file.exists (f))
    {
        bb <- osmdata::getbb ("new york city", format_out = "sf_polygon")
        saveRDS (bb, file = f)
    } else
        bb <- readRDS (f)
    suppressMessages (index <- sf::st_contains (bb, pts) [[1]])
    a <- a [index, ]
    # a for ny-attractors has OSM id's, but these need to be re-matched to
    # values in the actual street network
    if (type != "centrality")
        a$id <- v$id [dodgr::match_points_to_graph (v, a [, c ("x", "y")])]
    id <- capacity <- NULL # no visible binding note
    a <- a [a$category == type, ]
    if (type == "transportation")
    {
        suppressWarnings (a$capacity <- as.integer (a$capacity))
        # get median size of parking facilities to replace NA values
        index <- grep ("parking", a$amenity)
        med_parks <- stats::median (a$capacity [index], na.rm = TRUE)
        a$capacity [index] [is.na (a$capacity [index])] <- med_parks
        # all other non-parking transportation give n = 1
        index <- seq (nrow (a)) [which (!seq (nrow (a))) %in% index]
        if (length (index) > 0)
            a$capacity [index, ] <- 1
        a$capacity [is.na (a$capacity)] <- 1
        a <- dplyr::select (a, c ("id", "capacity")) %>%
            dplyr::group_by (id) %>%
            dplyr::summarise (n = sum (capacity))
    } else
    {
        if (type == "centrality")
        {
            names (a) [which (names (a) == "centrality")] <- "n"
            a <- a [which (is.finite (a$n)), ]
            a$n <- a$n / max (a$n)
        } else
        {
            if (type == "entertainment")
                a <- a [a$amenity != "fountain", ]

            a <- dplyr::select (a, c ("id", "x", "y")) %>%
                dplyr::group_by (id) %>%
                dplyr::summarise (n = length (id))
        }

        # then put the coordinates back from the graph vertices
        index <- match (a$id, v$id)
        a$x <- v$x [index]
        a$y <- v$y [index]
    }

    return (a)
}
