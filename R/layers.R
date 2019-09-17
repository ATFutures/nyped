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

        if (!quiet)
            message (cli::symbol$pointer, " Contracting street network",
                     appendLF = FALSE)
        net <- dodgr::dodgr_contract_graph (net,
                                            verts = unique (c (p$id, s$id)))
        if (!quiet)
            message ("\r", cli::symbol$tick, " Contracted street network ")

        res <- calc_layer (net, data_dir, from, to, k, k_scale, p, s,
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

calc_layer <- function (net, data_dir, from, to, k, k_scale, p, s, cache, quiet)
{
    if (from == "residential")
        res <- layer_subway_res (net, data_dir, p, s, k = k, k_scale = k_scale,
                                 reverse = TRUE, quiet = quiet)
    else if (to == "residential")
        res <- layer_subway_res (net, data_dir, p, s, k = k, k_scale = k_scale,
                                 reverse = FALSE, quiet = quiet)
    else if (to == "disperse")
        res <- layer_disperse (net, from = from, data_dir, p, s, k = k,
                               k_scale = k_scale, quiet = quiet)
    else if (from == "subway")
    {
        if (to == "subway")
            res <- layer_subway_subway (net, data_dir, p, s,
                                        k = k, k_scale = k_scale, quiet = quiet)
        else
            res <- layer_subway_attr (net, to = to, data_dir, p, s,
                                      k = k, k_scale = k_scale, quiet = quiet)
    } else
        res <- layer_attr_attr (net, from = from, to = to, data_dir, p, s,
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

layer_subway_attr <- function (net, to = "disperse", data_dir, p, s,
                               k = 700, k_scale = 0, quiet = FALSE)
{
    if (!quiet)
        message (cli::symbol$pointer, " Preparing spatial interaction matrices",
                 appendLF = FALSE)
    v <- dodgr::dodgr_vertices (net)

    a <- get_attractor_layer (data_dir, v, type = to)

    k = k ^ (1 + k_scale * s$count2018 / max (s$count2018))
    kmat <- matrix (k, nrow = nrow (s), ncol = nrow (a))

    # calculate spatial interaction model between subway and attraction centres,
    # where the latter are weighted by number of centres allocated to each
    # contracted vertex
    smat <- matrix (s$count2018, nrow = nrow (s), ncol = nrow (a))
    amat <- t (matrix (as.double (a$n), nrow = nrow (a), ncol = nrow (s)))
    dmat <- dodgr::dodgr_dists (net, from = s$id, to = a$id)
    dmat [is.na (dmat)] <- max (dmat, na.rm = TRUE)

    # Double-constrain interaction matrix to unit sum over all possible
    # origins and destinations s->a. For each origin (row), the sum over all
    # destinations (columns) has to equal one:
    fmat <- amat * exp (-dmat / kmat)
    cmat <- t (matrix (colSums (fmat), nrow = nrow (a), ncol = nrow (s)))
    fmat <- fmat / cmat
    # Each origin (row) should then only allocate the total amount to all
    # destinations (columns), so fmat is multiplied by smat divided by
    # ncol(smat) == nrow (a)
    fmat <- smat * fmat / nrow (a)

    if (!quiet)
    {
        message ("\r", cli::symbol$tick,
                 " Prepared spatial interaction matrices  ")
        message (cli::symbol$pointer, " Aggregating flows ", appendLF = FALSE)
    }
    st0 <- Sys.time ()
    net_f <- dodgr::dodgr_flows_aggregate (net, from = s$id, to = a$id,
                                           flows = fmat)
    st <- formatC (as.numeric (difftime (Sys.time (), st0, units = "sec")),
                   format = "f", digits = 1)
    if (!quiet)
    {
        message ("\r", cli::symbol$tick, " Aggregated flows in ", st, "s")

        message (cli::symbol$pointer,
                 " Aligning flows to pedestrian count points",
                 appendLF = FALSE)
    }

    flows <- flow_to_ped_pts (net_f, p, get_nearest = TRUE)

    if (!quiet)
        message ("\r", cli::symbol$tick,
                 " Aligned flows to pedestrian count points ")

    p$flows <- flows
    return (p)
}

layer_subway_subway <- function (net, data_dir, p, s,
                                 k = 700, k_scale = 0, quiet = FALSE)
{
    if (!quiet)
        message (cli::symbol$pointer, " Preparing spatial interaction matrices",
                 appendLF = FALSE)
    v <- dodgr::dodgr_vertices (net)

    k = k ^ (1 + k_scale * s$count2018 / max (s$count2018))
    kmat <- matrix (k, nrow = nrow (s), ncol = nrow (s))

    ns <- nrow (s)

    # calculate spatial interaction model between subway and attraction centres,
    # where the latter are weighted by number of centres allocated to each
    # contracted vertex
    smat <- matrix (s$count2018, nrow = ns, ncol = ns)
    dmat <- dodgr::dodgr_dists (net, from = s$id, to = s$id)
    dmat [is.na (dmat)] <- max (dmat, na.rm = TRUE)

    # Double-constrain interaction matrix to unit sum over all possible
    # origins and destinations s->a. For each origin (row), the sum over all
    # destinations (columns) has to equal one:
    fmat <- smat * exp (-dmat / kmat)
    cmat <- t (matrix (colSums (fmat), nrow = ns, ncol = ns))
    fmat <- fmat / cmat
    # Each origin (row) should then only allocate the total amount to all
    # destinations (columns), so fmat is multiplied by smat divided by
    # ncol(smat) == nrow (a)
    fmat <- smat * fmat / ns

    if (!quiet)
    {
        message ("\r", cli::symbol$tick,
                 " Prepared spatial interaction matrices  ")
        message (cli::symbol$pointer, " Aggregating flows ", appendLF = FALSE)
    }
    st0 <- Sys.time ()
    net_f <- dodgr::dodgr_flows_aggregate (net, from = s$id, to = s$id,
                                           flows = fmat)
    st <- formatC (as.numeric (difftime (Sys.time (), st0, units = "sec")),
                   format = "f", digits = 1)
    if (!quiet)
    {
        message ("\r", cli::symbol$tick, " Aggregated flows in ", st, "s")

        message (cli::symbol$pointer,
                 " Aligning flows to pedestrian count points",
                 appendLF = FALSE)
    }

    flows <- flow_to_ped_pts (net_f, p, get_nearest = TRUE)

    if (!quiet)
        message ("\r", cli::symbol$tick,
                 " Aligned flows to pedestrian count points ")

    p$flows <- flows
    return (p)
}

layer_attr_attr <- function (net, from = "health", to = "disperse", data_dir,
                             p, s, k = 700, k_scale = 0, quiet = FALSE)
{
    if (!quiet)
        message (cli::symbol$pointer, " Preparing spatial interaction matrices",
                 appendLF = FALSE)
    v <- dodgr::dodgr_vertices (net)

    fr_a <- get_attractor_layer (data_dir, v, type = from)
    to_a <- get_attractor_layer (data_dir, v, type = to)
    nfr <- nrow (fr_a)
    nto <- nrow (to_a)

    k = k ^ (1 + k_scale * fr_a$n / max (fr_a$n))
    kmat <- matrix (k, nrow = nfr, ncol = nto)

    # calculation spatial interaction model between from and to centres,
    # weighting both by numbers of centres allocated to each contracted vertex.
    fr_mat <- matrix (fr_a$n, nrow = nfr, ncol = nto)
    to_mat <- t (matrix (as.double (to_a$n), nrow = nto, ncol = nfr))
    dmat <- dodgr::dodgr_dists (net, from = fr_a$id, to = to_a$id)
    dmat [is.na (dmat)] <- max (dmat, na.rm = TRUE)

    # Double-constrain interaction matrix to unit sum over all possible
    # origins and destinations. For each origin (row), the sum over all
    # destinations (columns) has to equal one:
    fmat <- fr_mat * exp (-dmat / kmat)
    cmat <- t (matrix (colSums (fmat), nrow = nto, ncol = nfr))
    fmat <- fmat / cmat
    # Each origin (row) should then only allocate the total amount to all
    # destinations (columns), so fmat is multiplied by fr_mat divided by
    # ncol (to_mat) == nto
    fmat <- fr_mat * fmat / nto

    if (!quiet)
    {
        message ("\r", cli::symbol$tick,
                 " Prepared spatial interaction matrices  ")
        message (cli::symbol$pointer, " Aggregating flows ", appendLF = FALSE)
    }
    st0 <- Sys.time ()
    net_f <- dodgr::dodgr_flows_aggregate (net, from = fr_a$id, to = to_a$id,
                                           flows = fmat)
    st <- formatC (as.numeric (difftime (Sys.time (), st0, units = "sec")),
                   format = "f", digits = 1)
    if (!quiet)
    {
        message ("\r", cli::symbol$tick, " Aggregated flows in ", st, "s")

        message (cli::symbol$pointer,
                 " Aligning flows to pedestrian count points",
                 appendLF = FALSE)
    }

    flows <- flow_to_ped_pts (net_f, p, get_nearest = TRUE)

    if (!quiet)
        message ("\r", cli::symbol$tick,
                 " Aligned flows to pedestrian count points ")

    p$flows <- flows
    return (p)
}

layer_disperse <- function (net, from = "subway", data_dir, p, s,
                            k = 700, k_scale = 0, quiet = FALSE)
{
    v <- dodgr::dodgr_vertices (net)

    if (from == "subway")
    {
        k = k ^ (1 + k_scale * s$count2018 / max (s$count2018))
        dens <- s$count2018
        pts <- s$id
    } else
    {
        a <- get_attractor_layer (data_dir, v, type = from)
        k = k ^ (1 + k_scale * a$n / max (a$n))
        dens <- a$n
        pts <- a$id
    }

    if (!quiet)
        message (cli::symbol$pointer, " Dispersing flows ", appendLF = FALSE)

    st0 <- Sys.time ()
    net_f <- dodgr::dodgr_flows_disperse (net, from = pts, dens = dens, k = k)


    st <- formatC (as.numeric (difftime (Sys.time (), st0, units = "sec")),
                   format = "f", digits = 1)
    if (!quiet)
    {
        message ("\r", cli::symbol$tick, " Dispersed flows in ", st, "s")

        message (cli::symbol$pointer,
                 " Aligning flows to pedestrian count points", appendLF = FALSE)
    }

    flows <- flow_to_ped_pts (net_f, p, get_nearest = TRUE)

    if (!quiet)
        message ("\r", cli::symbol$tick,
                 " Aligned flows to pedestrian count points ")

    p$flows <- flows
    return (p)
}

# !reverse calculates flows from subways to residential areas, with origins
# weighted by subway counts and destinations by population density.
# reverse calculates flows from residential areas to subways, with destinations
# (subways) simply weighted equally and *NOT* by subway counts
layer_subway_res <- function (net, data_dir, p, s, k = 700, k_scale = 0,
                              reverse = FALSE, quiet = FALSE)
{
    k = k ^ (1 + k_scale * s$count2018 / max (s$count2018))

    if (!quiet)
        message (cli::symbol$pointer, " Preparing residential density data ",
                 appendLF = FALSE)
    st0 <- Sys.time ()
    v <- dodgr::dodgr_vertices (net)

    if (reverse) # reverse the graph
    {
        vx0 <- net$.vx0
        net$.vx0 <- net$.vx1
        net$.vx1 <- vx0

        vx0_x <- net$.vx0_x
        net$.vx0_x <- net$.vx1_x
        net$.vx1_x <- vx0_x

        vx0_y <- net$.vx0_y
        net$.vx0_y <- net$.vx1_y
        net$.vx1_y <- vx0_y

        vx1_x <- net$.vx1_x
        net$.vx1_x <- net$.vx1_x
        net$.vx1_x <- vx1_x

        vx1_y <- net$.vx1_y
        net$.vx1_y <- net$.vx1_y
        net$.vx1_y <- vx1_y
    }

    nodes_new <- get_popdens_data (v, data_dir)

    dmat <- d_subway_res (net, s, nodes_new)

    kmat <- matrix (k, nrow = nrow (s), ncol = nrow (nodes_new))

    st <- formatC (as.numeric (difftime (Sys.time (), st0, units = "sec")),
                   format = "f", digits = 1)
    if (!quiet)
    {
        message ("\r", cli::symbol$tick,
                 " Prepared residential density data in ", st, "s")

        message (cli::symbol$pointer,
                 " Preparing spatial interaction matrices", appendLF = FALSE)
    }
    fmat <- exp (-dmat / kmat)
    cmat <- matrix (rowSums (fmat), nrow = nrow (s), ncol = nrow (nodes_new))
    fmat <- fmat / cmat # all rowSums over stations == 1
    if (!reverse)
    {
        smat <- matrix (s$count2018, nrow = nrow (s), ncol = nrow (nodes_new))
        fmat <- fmat * smat # rowSums then equal station counts
    }

    # The final flow matrix is then just this fmat times the population densities,
    # constrained so that each column (sum over all rows/origins/stations) sums to 
    # the specified population density
    layer_name <- names (nodes_new) [grep ("ppp_", names (nodes_new))]
    pmat <- t (matrix (nodes_new [[layer_name]],
                       nrow = nrow (nodes_new), ncol = nrow (s)))
    csmat <- t (matrix (colSums (pmat), nrow = ncol (pmat), ncol = nrow (pmat)))
    pmat <- pmat / csmat

    # then the final spatial interaction matrix is simply:
    fmat <- pmat * fmat
    if (!quiet)
    {
        message ("\r", cli::symbol$tick,
                 " Prepared spatial interaction matrices ")

        # this then takes only 2-3 minutes
        message (cli::symbol$pointer, " Aggregating flows ", appendLF = FALSE)
    }
    st0 <- Sys.time ()
    net_f <- dodgr::dodgr_flows_aggregate (net, from = s$id, to = nodes_new$id,
                                           flows = fmat)
    st <- formatC (as.numeric (difftime (Sys.time (), st0, units = "sec")),
                   format = "f", digits = 1)
    if (!quiet)
    {
        message ("\r", cli::symbol$tick, " Aggregated flows in ", st, "s")

        message (cli::symbol$pointer,
                 " Aligning flows to pedestrian count points", appendLF = FALSE)
    }

    flows <- flow_to_ped_pts (net_f, p, get_nearest = TRUE)

    if (!quiet)
        message ("\r", cli::symbol$tick,
                 " Aligned flows to pedestrian count points ")

    p$flows <- flows
    return (p)
}

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

d_subway_res <- function (net, s, nodes_new)
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

flow_to_ped_pts <- function (net_f, p, get_nearest = TRUE)
{
    flows <- rep (NA, nrow (p))

    # Calculate dmat from all pedestrian count points - this takes very little
    # time compared with the code below, so it's okay to repeat it each time
    dp <- dodgr::dodgr_dists (net_f, from = p$id)

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
                                "disperse"))
    a <- readRDS (file.path (data_dir, "osm", "ny-attractors.Rds"))
    # The attractors data contains lots of points outside the bbox of the street
    # network, so have to be reduced to only those within. (Otherwise *ALL*
    # points beyond get aggregated to nearest points, producing anomalously huge
    # values at boundaries.)
    pts <- sf::st_as_sf (a, coords = c ("x", "y"), crs = 4326)
    bb <- osmdata::getbb ("new york city", format_out = "sf_polygon")
    suppressMessages (index <- sf::st_contains (bb, pts) [[1]])
    a <- a [index, ]
    # a has OSM id's, but these need to be re-matched to values in the actual
    # street network
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

    return (a)
}


#' all_ny_layers
#'
#' Calculate all flow layer of pedestrian densities for New York City, for a
#' range of widths of exponential spatial interaction functions (`k`-values).
#'
#' @param net Weighted street network; loaded from `data_dir` if not provided
#' @param k Vector of widths of exponential decay (in m) for spatial interaction
#' models
#' @param data_dir The directory in which data are to be, or have previously
#' been, downloaded.
#' @export
all_ny_layers <- function (net = NULL, k = 2:9 * 100, data_dir)
{
    # NOTE: At the moment, these all use k_scale = 0
    to <- c ("residential", "education", "entertainment", "healthcare",
             "sustenance", "transportation", "disperse")
    from <- rep ("subway", length (to))
    to <- c ("subway", to)
    from <- c ("residential", from)

    # temporary reduction
    to <- c ("education", "entertainment", "healthcare", "sustenance")
    from <- rep ("subway", length (to))

    my_arrow <- paste0 (cli::symbol$em_dash, cli::symbol$arrow_right)
    t0 <- Sys.time ()
    for (i in seq (from))
    {
        txt <- paste0 (from [i], " ", my_arrow, " ", to [i], " : ")
        msg0 <- paste0 (cli::col_green (my_arrow), " ",
                        cli::col_blue (txt))

        for (j in k)
        {
            msg <- paste0 (msg0, " k = ", j, "m", collapse = "")
            message (msg, appendLF = FALSE)
            cat (stdout ()) # necessary to flush the buffer here - but why?
            st0 <- Sys.time ()
            x <- ny_layer (net, data_dir = data_dir, k = j, quiet = TRUE,
                           from = from [i], to = to [i])
            st <- formatC (as.numeric (difftime (Sys.time (), st0,
                                                 units = "sec")),
                           format = "f", digits = 1)
            fname <- file.path (data_dir, "calibration",
                                paste0 ("flow-",
                                        substring (from [i], 1, 3), "-",
                                        substring (to [i], 1, 3), "-k",
                                        j, ".Rds"))
            saveRDS (x, file = fname)
            message ("\r", msg, "; done in ", st, "s")
        }
    }
    message ("Total elapsed time = ", format_time_int (t0))
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
    mod <- summary (lm (p$week ~ res$flows))
    c (k = k,
       k_scale = k_scale,
       r2 = mod$adj.r.squared,
       ss = sum (mod$residuals ^ 2) / length (mod$residuals) / 1e6)
}
