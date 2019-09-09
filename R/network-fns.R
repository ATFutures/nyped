
#' cut_network_to_pts
#'
#' Cut the New York City street network into portions surrounding each
#' pedestrian counting point.
#'
#' @param data_dir The directory in which data are to be, or have previously
#' been, downloaded.
#' @param k Exponential decay parameter to be used in spatial interaction
#' models. This deterines the radius at which to cut the network.
#' @export
cut_network_to_pts <- function (data_dir, k = 700)
{
    p <- nyped_data ()
    xy <- data.frame (sf::st_coordinates (p$geometry))

    f <- file.path (data_dir, "osm", "ny-hw.Rds")
    hw <- readRDS (f)
    dodgr::dodgr_cache_off ()
    message (cli::symbol$pointer, " Weighting street network",
             appendLF = FALSE)
    net <- dodgr::weight_streetnet (hw, wt_profile = "foot")
    v <- dodgr::dodgr_vertices (net)
    ids <- v$id [dodgr::match_points_to_graph (v, xy)]
    xy$id <- v$id [dodgr::match_points_to_graph (v, xy)]
    message ("\r", cli::symbol$tick, " Weighted street network ")

    message (cli::symbol$pointer, " Cutting into ", nrow (p), " components")
    dlim <- k * 12 # equivalent to point at which exp(-d/k) <= 1e-12
    pb <- utils::txtProgressBar (style = 3)
    for (i in nrow (xy))
    {
        id <- xy$id [i]
        res <- dodgr::dodgr_isoverts (net, from = id, dlim = dlim)
        index <- which (net$.vx0 %in% res$id | net$.vx1 %in% res$id)
        net_cut <- net [index, ]
        nm <- file.path (data_dir, "osm", "ny-cut",
                         paste0 ("ny-hw", id, ".Rds"))
        saveRDS (net_cut, nm)
        utils::setTxtProgressBar (pb, i / nrow (xy))
    }
    close (pb)
    message (cli::symbol$tick, " Successfully cut network into ",
             nrow (p), " components")
}
