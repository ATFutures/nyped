#' ny_layer
#'
#' Calculate one flow layer of pedestrian densities for New York City
#'
#' @param net Weighted street network; loaded from `data_dir` if not provided
#' @param data_dir The directory in which data are to be, or have previously
#' been, downloaded.
#' @param from Category of origins for pedestrian flows
#' @param to Category of destinations for pedestrian flows
#' @export
ny_layer <- function (net = NULL, from = "subway", to = "activity", data_dir)
{
    message (cli::rule (left = "New York pedestrian calibration",
                        line = 2, col = "green"))
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
    v <- dodgr::dodgr_vertices (net)

    p <- ped_osm_id (net = net)
    s <- subway_osm_id (net = net)
    message (cli::symbol$pointer, " Contracting street network",
             appendLF = FALSE)
    net <- dodgr::dodgr_contract_graph (net, verts = unique (c (p$id, s$id)))
    message ("\r", cli::symbol$tick, " Contracted street network ")

    layer_subway_attr (net, data_dir, p, s, k = 700)
}

layer_subway_attr <- function (net, data_dir, p, s, k = 700)
{
    message (cli::symbol$pointer, " Preparing spatial interaction matrices",
             appendLF = FALSE)
    v <- dodgr::dodgr_vertices (net)

    a <- readRDS (file.path (data_dir, "osm", "ny-attractors.Rds"))
    # a has OSM id's, but these need to be re-matched to values in the actual
    # street network
    a$id <- v$id [dodgr::match_points_to_graph (v, a [, c ("x", "y")])]
    id <- NULL # no visible binding note
    a <- dplyr::select (a, c ("id", "x", "y")) %>%
        dplyr::group_by (id) %>%
        dplyr::summarise (n = length (id))
    # then put the coordinates back from the graph vertices
    index <- match (a$id, v$id)
    a$x <- v$x [index]
    a$y <- v$y [index]

    # calculate spatial interaction model between subway and attraction centres
    smat <- matrix (s$count2018, nrow = nrow (s), ncol = nrow (a))
    amat <- t (matrix (as.double (a$n), nrow = nrow (a), ncol = nrow (s)))
    dmat <- dodgr::dodgr_dists (net, from = s$id, to = a$id)

    # constrain interaction matrix to unit sum over all possible destinations s->a
    cmat <- t (matrix (colSums (amat), nrow = nrow (a), ncol = nrow (s)))
    fmat <- smat * amat * exp (-dmat / k) / cmat

    message ("\r", cli::symbol$tick, " Prepared spatial interaction matrices  ")
    message (cli::symbol$pointer, " Aggregating flows ", appendLF = FALSE)
    st0 <- Sys.time ()
    net_f <- dodgr::dodgr_flows_aggregate (net, from = s$id, to = a$id,
                                           flows = fmat)
    st <- formatC (as.numeric (difftime (Sys.time (), st0, units = "sec")),
                   format = "f", digits = 1)
    message ("\r", cli::symbol$tick, " Aggregated flows in ", st, "s")

    message (cli::symbol$pointer, " Aligning flows to pedestrian count points")
    flows <- rep (NA, nrow (p))
    # dlim <- 12 * k # limit of exp (-d / k) = 1e-12
    # This is only used in the commented-out lines below for cutting the graph
    st0 <- Sys.time ()
    pb <- utils::txtProgressBar (style = 3)
    for (i in seq (nrow (p)))
    {
        # find edges that flow in and out of that point - these commonly return
        # only NA values, so second approach is implemented
        #i1 <- which (net_f$.vx0 == p$id [i])
        #i2 <- which (net_f$.vx1 == p$id [i])
        #flows [i] <- sum (net_i$flow [i1], na.rm = TRUE) +
        #    sum (net_i$flow [i2], na.rm = TRUE)

        # choose first edges near that observation vertex that have non-zero
        # flows
        di <- dodgr::dodgr_dists (net, from = p$id [i]) [1, ]
        di <- di [order (di)]
        # --> code to first cut graph down via dodgr_isoverts. This takes 2-3
        # times longer
        #iv <- dodgr::dodgr_isoverts (net, from = p$id [i], dlim = dlim)
        #et_i <- net [which (net$.vx0 %in% iv$id | net$.vx1 %in% iv$id), ]
        #i <- dodgr::dodgr_dists (net, from = p$id [i]) [1, ]
        #i <- di [order (di)]

        f_ord <- net_f$flow [match (net_f$.vx0, names (di))]
        flow0 <- f_ord [which (f_ord > 0)] [1]
        f_ord <- net_f$flow [match (net_f$.vx1, names (di))]
        flow1 <- f_ord [which (f_ord > 0)] [1]
        flows [i] <- flow0 + flow1
        utils::setTxtProgressBar (pb, i / nrow (p))
    }

    close (pb)
    st <- formatC (as.numeric (difftime (Sys.time (), st0, units = "sec")),
                   format = "f", digits = 1)
    message (cli::symbol$tick, " Aligned flows to pedestrian count points in ",
             st, "s")
    message (cli::rule (left = "Finished", line = 2, col = "green"))

    p$flows <- flows
    return (p)
}
