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
        net <- get_ny_network (data_dir)

    v <- dodgr::dodgr_vertices (net)
    xy$id <- v$id [dodgr::match_points_to_graph (v, xy)]
    xy$weekday <- p$weekday
    xy$weekend <- p$weekend
    xy$week <- p$week

    return (xy)
}

get_ny_network <- function (data_dir)
{
    f <- file.path (data_dir, "osm", "ny-hw.Rds")
    hw <- readRDS (f)
    dodgr::dodgr_cache_off ()
    dodgr::weight_streetnet (hw, wt_profile = "foot")
}

#' subway_osm_id
#'
#' Get OSM IDs nearest to subway count points
#' @param net Weighted street network; loaded from `data_dir` if not provided
#' @param data_dir The directory in which data are to be, or have previously
#' been, downloaded.
#' @return A `data.frame` of pedestrian counts, associated spatial coordinates,
#' and OSM IDs of nearest points on network
#' @export
subway_osm_id <- function (data_dir, net = NULL)
{
    if (is.null (net))
        net <- get_ny_network (data_dir)

    s <- nysubway_data ()
    s$count2018 <- s$count2018 / 365000 # convert to 1000's per day
    sxy <- sf::st_coordinates (s$geom)
    v <- dodgr::dodgr_vertices (net)
    s$id <- v$id [dodgr::match_points_to_graph (v, sxy)]
    s [which (s$id %in% v$id), ]
}
