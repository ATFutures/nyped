#' ny_layer
#'
#' Calculate one flow layer of pedestrian densities for New York City
#'
#' @param net Weighted street network; loaded from `data_dir` if not provided
#' @param from Category of origins for pedestrian flows; one of "subway" or
#' "residential"
#' @param to Category of destinations for pedestrian flows; one of "activity",
#' "parking", "residential", or anything else to estimate dispersal.
#' @param k Width of exponential decay (in m) for spatial interaction models
#' @param data_dir The directory in which data are to be, or have previously
#' been, downloaded.
#' @export
ny_layer <- function (net = NULL, from = "subway", to = "activity",
                      k = 700, data_dir)
{
    txt <- "New York pedestrian calibration:"
    if (from == "residential")
    {
        to <- "subway"
        txt <- paste (txt, "residential to subway")
    } else if (to == "activity")
        txt <- paste (txt, "subway to activity centres")
    else if (to == "parking")
        txt <- paste (txt, "subway to parking")
    else if (to == "residential")
        txt <- paste (txt, "subway to residential")
    else
        txt <- paste (txt, "dispersal from subway")
    message (cli::rule (center = txt, line = 2, col = "green"))

    st0 <- Sys.time ()
    if (is.null (net))
    {
        f <- file.path (data_dir, "osm", "ny-hw.Rds")
        hw <- readRDS (f)
        dodgr::dodgr_cache_off ()
        message (cli::symbol$pointer, " Weighting street network",
                 appendLF = FALSE)
        net <- dodgr::weight_streetnet (hw, wt_profile = "foot")
        message ("\r", cli::symbol$tick, " Weighted street network ")
    }

    p <- ped_osm_id (net = net)
    s <- subway_osm_id (net = net)
    message (cli::symbol$pointer, " Contracting street network",
             appendLF = FALSE)
    net <- dodgr::dodgr_contract_graph (net, verts = unique (c (p$id, s$id)))
    message ("\r", cli::symbol$tick, " Contracted street network ")

    if (from == "residential")
        res <- layer_subway_res (net, data_dir, p, s, k = k, reverse = TRUE)
    else if (to == "residential")
        res <- layer_subway_res (net, data_dir, p, s, k = k, reverse = FALSE)
    else if (to == "activity")
        res <- layer_subway_attr (net, data_dir, p, s, k = k)
    else if (to == "parking")
        res <- layer_subway_attr (net, data_dir, p, s, k = k, parking = TRUE)
    else
        res <- layer_subway_disperse (net, data_dir, p, s, k = k)
    st <- formatC (as.numeric (difftime (Sys.time (), st0, units = "sec")),
                   format = "f", digits = 1)
    message (cli::rule (center = paste0 ("Finished in ", st, "s"),
                        line = 2, col = "green"))

    return (res)
}

layer_subway_attr <- function (net, data_dir, p, s, k = 700,
                               parking = FALSE)
{
    message (cli::symbol$pointer, " Preparing spatial interaction matrices",
             appendLF = FALSE)
    v <- dodgr::dodgr_vertices (net)

    a <- readRDS (file.path (data_dir, "osm", "ny-attractors.Rds"))
    # a has OSM id's, but these need to be re-matched to values in the actual
    # street network
    a$id <- v$id [dodgr::match_points_to_graph (v, a [, c ("x", "y")])]
    id <- capacity <- NULL # no visible binding note
    if (!parking)
    {
        a <- a [a$category != "transportation", ]
        a <- dplyr::select (a, c ("id", "x", "y")) %>%
            dplyr::group_by (id) %>%
            dplyr::summarise (n = length (id))
    } else
    {
        a <- a [a$category == "transportation", ]
        suppressWarnings (a$capacity <- as.integer (a$capacity))
        a$capacity [is.na (a$capacity)] <- stats::median (a$capacity, na.rm = TRUE)
        a <- dplyr::select (a, c ("id", "capacity")) %>%
            dplyr::group_by (id) %>%
            dplyr::summarise (n = sum (capacity))
    }
    # then put the coordinates back from the graph vertices
    index <- match (a$id, v$id)
    a$x <- v$x [index]
    a$y <- v$y [index]

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
    fmat <- amat * exp (-dmat / k)
    cmat <- t (matrix (colSums (fmat), nrow = nrow (a), ncol = nrow (s)))
    fmat <- fmat / cmat
    # Each origin (row) should then only allocate the total amount to all
    # destinations (columns), so fmat is multiplied by smat divided by
    # ncol(smat) == nrow (a)
    fmat <- smat * fmat / nrow (a)

    message ("\r", cli::symbol$tick, " Prepared spatial interaction matrices  ")
    message (cli::symbol$pointer, " Aggregating flows ", appendLF = FALSE)
    st0 <- Sys.time ()
    net_f <- dodgr::dodgr_flows_aggregate (net, from = s$id, to = a$id,
                                           flows = fmat)
    st <- formatC (as.numeric (difftime (Sys.time (), st0, units = "sec")),
                   format = "f", digits = 1)
    message ("\r", cli::symbol$tick, " Aggregated flows in ", st, "s")

    message (cli::symbol$pointer, " Aligning flows to pedestrian count points",
             appendLF = FALSE)
    flows <- rep (NA, nrow (p))
    # dlim <- 12 * k # limit of exp (-d / k) = 1e-12
    # This is only used in the commented-out lines below for cutting the graph

    # Calculate dmat from all pedestrian count points
    dp <- dodgr::dodgr_dists (net, from = p$id)

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
        if (flows [i] == 0)
        {
            di <- dp [i, ] [order (dp [i, ])]
            f_ord <- net_f$flow [match (net_f$.vx0, names (di))]
            flow0 <- f_ord [which (f_ord > 0)] [1]
            f_ord <- net_f$flow [match (net_f$.vx1, names (di))]
            flow1 <- f_ord [which (f_ord > 0)] [1]
            flows [i] <- flow0 + flow1
        }
    }

    message ("\r", cli::symbol$tick,
             " Aligned flows to pedestrian count points ")

    p$flows <- flows
    return (p)
}

layer_subway_disperse <- function (net, data_dir, p, s, k = 700)
{
    message (cli::symbol$pointer, " Dispersing flows ", appendLF = FALSE)
    v <- dodgr::dodgr_vertices (net)

    st0 <- Sys.time ()
    net_f <- dodgr::dodgr_flows_disperse (net, from = s$id,
                                          dens = s$count2018, k = k)



    st <- formatC (as.numeric (difftime (Sys.time (), st0, units = "sec")),
                   format = "f", digits = 1)
    message ("\r", cli::symbol$tick, " Dispersed flows in ", st, "s")

    message (cli::symbol$pointer, " Aligning flows to pedestrian count points")
    flows <- rep (NA, nrow (p))

    # Calculate dmat from all pedestrian count points
    dp <- dodgr::dodgr_dists (net, from = p$id)

    for (i in seq (nrow (p)))
    {
        # find edges that flow in and out of that point - these commonly return
        # only NA values, so second approach is implemented
        i1 <- which (net_f$.vx0 == p$id [i])
        i2 <- which (net_f$.vx1 == p$id [i])
        flows [i] <- sum (net_f$flow [i1], na.rm = TRUE) +
            sum (net_f$flow [i2], na.rm = TRUE)
    }

    message ("\r", cli::symbol$tick,
             " Aligned flows to pedestrian count points ")

    p$flows <- flows
    return (p)
}

# !reverse calculates flows from subways to residential areas, with origins
# weighted by subway counts and destinations by population density.
# reverse calculates flows from residential areas to subways, with destinations
# (subways) simply weighted equally and *NOT* by subway counts
layer_subway_res <- function (net, data_dir, p, s, k = 700, reverse = FALSE)
{
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

    nodes_new <- readRDS (file.path (data_dir, "worldpop", "pop-points.Rds"))
    layer_name <- names (nodes_new) [grep ("ppp_", names (nodes_new))]
    nodes_new$id <- v$id [dodgr::match_points_to_graph (v, nodes_new)]
    # pop values go up to around 3,000, so tiny ones are removed
    index <- which (!(nodes_new [[layer_name]] < 0.01 |
                      is.na (nodes_new [[layer_name]])))
    nodes_new <- nodes_new [index, ]

    dmat <- dodgr::dodgr_distances (net, from = s$id, to = nodes_new$id)
    dmat [is.na (dmat)] <- max (dmat, na.rm = TRUE)

    st <- formatC (as.numeric (difftime (Sys.time (), st0, units = "sec")),
                   format = "f", digits = 1)
    message ("\r", cli::symbol$tick, " Prepared residential density data in ",
             st, "s")

    message (cli::symbol$pointer, " Preparing spatial interaction matrices",
             appendLF = FALSE)
    fmat <- exp (-dmat / k)
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
    pmat <- t (matrix (nodes_new [[layer_name]],
                       nrow = nrow (nodes_new), ncol = nrow (s)))
    csmat <- t (matrix (colSums (pmat), nrow = ncol (pmat), ncol = nrow (pmat)))
    pmat <- pmat / csmat

    # then the final spatial interaction matrix is simply:
    fmat <- pmat * fmat
    message ("\r", cli::symbol$tick, " Prepared spatial interaction matrices ")

    # this then takes only 2-3 minutes
    message (cli::symbol$pointer, " Aggregating flows ", appendLF = FALSE)
    st0 <- Sys.time ()
    net_f <- dodgr::dodgr_flows_aggregate (net, from = s$id, to = nodes_new$id,
                                           flows = fmat)
    st <- formatC (as.numeric (difftime (Sys.time (), st0, units = "sec")),
                   format = "f", digits = 1)
    message ("\r", cli::symbol$tick, " Aggregated flows in ", st, "s")

    message (cli::symbol$pointer, " Aligning flows to pedestrian count points",
             appendLF = FALSE)
    flows <- rep (NA, nrow (p))
    # dlim <- 12 * k # limit of exp (-d / k) = 1e-12
    # This is only used in the commented-out lines below for cutting the graph

    # Calculate dmat from all pedestrian count points
    dp <- dodgr::dodgr_dists (net, from = p$id)

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
        if (flows [i] == 0)
        {
            di <- dp [i, ] [order (dp [i, ])]
            f_ord <- net_f$flow [match (net_f$.vx0, names (di))]
            flow0 <- f_ord [which (f_ord > 0)] [1]
            f_ord <- net_f$flow [match (net_f$.vx1, names (di))]
            flow1 <- f_ord [which (f_ord > 0)] [1]
            flows [i] <- flow0 + flow1
        }
    }

    message ("\r", cli::symbol$tick,
             " Aligned flows to pedestrian count points ")

    p$flows <- flows
    return (p)
}
