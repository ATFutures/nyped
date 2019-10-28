#' ny_layer
#'
#' Calculate one flow layer of pedestrian densities for New York City
#'
#' @param net Weighted street network; loaded from `data_dir` if not provided
#' @param from Category of origins for pedestrian flows; one of "subway" or
#' "residential"
#' @param to Category of destinations for pedestrian flows; one of
#' "residential", "education", "entertainment", "healthcare", "sustenance",
#' "transportation", or "disperse" for a general dispersal model.
#' @param k Width of exponential decay (in m) for spatial interaction models
#' @param k_scale Scale `k` to size of origins (`s`), so `k = k ^ (1 + s /
#' smax)`.
#' @param data_dir The directory in which data are to be, or have previously
#' been, downloaded.
#' @param cache If `TRUE`, layers are cached in a sub-directory of `data_dir`
#' for later reloading.
#' @param quiet If `FALSE`, display progress information on screen
#' @export
ny_layer <- function (net = NULL, from = "subway", to = "activity",
                      k = 700, k_scale = 0, data_dir, cache = TRUE, quiet = FALSE)
{
    targets <- c ("residential", "education", "entertainment", "healthcare",
                  "sustenance", "transportation", "disperse", "subway")
    to <- match.arg (to, targets)
    from <- match.arg (from, targets)

    if (from == "residential")
        to <- "subway"

    if (!quiet)
        message (cli::rule (center = get_header_txt (from, to),
                            line = 2, col = "green"))

    st0 <- Sys.time ()
    if (is.null (net))
    {
        f <- file.path (data_dir, "osm", "ny-hw.Rds")
        hw <- readRDS (f)
        dodgr::dodgr_cache_off ()
        if (!quiet)
            message (cli::symbol$pointer, " Weighting street network",
                     appendLF = FALSE)
        net <- dodgr::weight_streetnet (hw, wt_profile = "foot")
        if (!quiet)
            message ("\r", cli::symbol$tick, " Weighted street network ")
    }

    chk <- FALSE
    if (cache)
        chk <- is_layer_cached (data_dir, from, to, k, k_scale)
    res <- get_layer (net, data_dir, from, to, k, k_scale, cache, quiet)

    st <- formatC (as.numeric (difftime (Sys.time (), st0, units = "sec")),
                   format = "f", digits = 1)
    if (!quiet & !chk)
        message (cli::rule (center = paste0 ("Finished in ", st, "s"),
                            line = 2, col = "green"))

    return (res)
}

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

get_layer <- function (net, data_dir, from, to, k, k_scale, cache, quiet)
{
    f <- get_file_name (data_dir, from, to, k, k_scale)

    if (file.exists (f) & cache)
    {
        if (!quiet)
            message (cli::symbol$tick, " Loaded cached layer")
        res <- readRDS (f)
    } else
    {
        p <- ped_osm_id (data_dir = data_dir, net = net, quiet = TRUE)
        s <- subway_osm_id (data_dir = data_dir, net = net, quiet = TRUE)
        dp <- dodgr::dodgr_dists (net, from = p$id)

        if (!quiet)
            message (cli::symbol$pointer, " Contracting street network",
                     appendLF = FALSE)
        net <- dodgr::dodgr_contract_graph (net,
                                            verts = unique (c (p$id, s$id)))
        if (!quiet)
            message ("\r", cli::symbol$tick, " Contracted street network ")

        res <- calc_layer (net, data_dir, from, to, k, k_scale, p, dp, s,
                           cache, quiet)
    }

    return (res)
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

calc_layer <- function (net, data_dir, from, to, k, k_scale, p, dp, s, cache, quiet)
{
    if (from == "residential")
        res <- layer_subway_res (net, data_dir, p, dp, s, k = k, k_scale = k_scale,
                                 reverse = TRUE, quiet = quiet)
    else if (to == "residential")
        res <- layer_subway_res (net, data_dir, p, dp, s, k = k, k_scale = k_scale,
                                 reverse = FALSE, quiet = quiet)
    else if (to == "disperse")
        res <- layer_disperse (net, from = from, data_dir, p, dp, s, k = k,
                               k_scale = k_scale, quiet = quiet)
    else if (from == "subway")
    {
        if (to == "subway")
            res <- layer_subway_subway (net, data_dir, p, dp, s,
                                        k = k, k_scale = k_scale, quiet = quiet)
        else
            res <- layer_subway_attr (net, to = to, data_dir, p, dp, s,
                                      k = k, k_scale = k_scale, quiet = quiet)
    } else
        res <- layer_attr_attr (net, from = from, to = to, data_dir, p, dp, s,
                                k = k, k_scale = k_scale, quiet = quiet)

    if (cache)
        cache_layer (res, data_dir, from, to, k, k_scale)

    return (res)
}

cache_layer <- function (res, data_dir, from, to, k, k_scale)
{
    # save only id and flow columns - this removes (X,Y) + pedestrian count cols
    res <- res [, c ("id", "flows")]

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
        nodes_new <- readRDS (file.path (data_dir, "worldpop", "pop-points.Rds"))
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

flow_to_ped_pts <- function (net_f, p, dp, get_nearest = TRUE)
{
    flows <- rep (NA, nrow (p))

    for (i in seq (nrow (p)))
    {
        # find edges that flow in and out of that point - these commonly return
        # only NA values, so second approach is implemented
        i1 <- which (net_f$.vx0 == p$id [i])
        i2 <- which (net_f$.vx1 == p$id [i])
        flows [i] <- sum (net_f$flow [i1], na.rm = TRUE) +
            sum (net_f$flow [i2], na.rm = TRUE)

        # OR: choose first edges near that observation vertex that have non-zero
        # flows
        if (flows [i] == 0 & get_nearest)
        {
            di <- dp [i, ] [order (dp [i, ])]
            f_ord <- net_f$flow [match (net_f$.vx0, names (di))]
            flow0 <- f_ord [which (f_ord > 0)] [1]
            f_ord <- net_f$flow [match (net_f$.vx1, names (di))]
            flow1 <- f_ord [which (f_ord > 0)] [1]
            flows [i] <- flow0 + flow1
        }
    }

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
    bb <- osmdata::getbb ("new york city", format_out = "sf_polygon")
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
        a <- dplyr::select (a, c ("id", "capacity")) %>%
            dplyr::group_by (id) %>%
            dplyr::summarise (n = sum (capacity))
    } else
    {
        if (type == "centrality")
            names (a) [which (names (a) == "centrality")] <- "n"
        else
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


#' fit_one_layer
#'
#' Calculate fit for one layer with observed pedestrian counts, and return both
#' sum of squared errors, and R-squared value
#'
#' @inheritParams ny_layer
#' @return Vector containing R-squared statistic and sum of squared errors for
#' fitted mode (divided by 1e6)
#' @export
fit_one_layer <- function (net = NULL, from = "subway", to = "activity",
                           k = 700, k_scale = 0, data_dir, cache = FALSE,
                           quiet = FALSE)
{
    res <- ny_layer (net = net, from = from, to = to, k = k, k_scale = k_scale,
                     data_dir = data_dir, cache = cache, quiet = quiet)
    p <- ped_osm_id (data_dir = data_dir, net = net, quiet = TRUE)
    mod <- summary (stats::lm (p$week ~ res$flows))
    c (k = k,
       k_scale = k_scale,
       r2 = mod$adj.r.squared,
       ss = sum (mod$residuals ^ 2) / length (mod$residuals) / 1e6)
}
