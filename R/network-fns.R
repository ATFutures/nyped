#' ped_osm_id
#'
#' Get OSM IDs nearest to pedestrian count points
#' @param net Weighted street network; loaded from `data_dir` if not provided
#' @param data_dir The directory in which data are to be, or have previously
#' been, downloaded.
#' @return A `data.frame` of pedestrian counts, associated spatial coordinates,
#' and OSM IDs of nearest points on network
#' @export
ped_osm_id <- function (data_dir, net = NULL)
{
    p <- nyped_data ()
    xy <- data.frame (sf::st_coordinates (p$geometry))

    if (is.null (net))
    {
        f <- file.path (data_dir, "osm", "ny-hw.Rds")
        hw <- readRDS (f)
        requireNamespace ("dodgr")
        dodgr::dodgr_cache_off ()
        message (cli::symbol$pointer, " Weighting street network",
                 appendLF = FALSE)
        net <- dodgr::weight_streetnet (hw, wt_profile = "foot")
    }

    v <- dodgr::dodgr_vertices (net)
    xy$id <- v$id [dodgr::match_points_to_graph (v, xy)]
    message ("\r", cli::symbol$tick, " Weighted street network ")

    return (xy)
}


#' cut_network_to_pts
#'
#' Cut the New York City street network into portions surrounding each
#' pedestrian counting point.
#'
#' @param net Uncontracted street network of New York City
#' @param k Exponential decay parameter to be used in spatial interaction
#' models. This deterines the radius at which to cut the network.
#' @param p Pedestrian counts including OSM IDs of nearest points, as returned
#' from \link{ped_osm_id}.
#' @param data_dir The directory in which data are to be, or have previously
#' been, downloaded - only needed if `p` is not provided.
#' @export
cut_network_to_pts <- function (net, k = 700, p = NULL, data_dir)
{
    if (is.null (p))
        p <- ped_osm_id (data_dir)

    message (cli::symbol$pointer, " Cutting into ", nrow (p), " components")
    dlim <- k * 12 # equivalent to point at which exp(-d/k) <= 1e-12

    # set up parallel job over origin points
    message (cli::symbol$pointer, " Setting up parallel job ", appendLF = FALSE)
    no_cores <- parallel::detectCores () - 1
    cl <- parallel::makeCluster (no_cores)
    parallel::clusterExport (cl, c ("net", "dlim", "data_dir"),
                             envir = environment ())
    message ("\r", cli::symbol$tick, " Successfully set up parallel job ")

    st0 <- Sys.time ()
    chk <- parallel::parLapply (cl, p$id, function (i) {

        res <- dodgr::dodgr_isoverts (net, from = i, dlim = dlim)
        index <- which (net$.vx0 %in% res$id | net$.vx1 %in% res$id)
        net_cut <- net [index, ]
        nm <- file.path (data_dir, "osm", "ny-cut",
                         paste0 ("ny-hw", i, ".Rds"))
        saveRDS (net_cut, nm)

             })

    parallel::stopCluster (cl)

    st <- as.numeric (difftime (Sys.time (), st0, units = "secs"))
    message (cli::symbol$tick, " Successfully cut network into ",
             nrow (p), " components in ",
             formatC (st, format = "f", digits = 1), "s")
}
