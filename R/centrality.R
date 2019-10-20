#' centrality_edge_to_point
#'
#' Convert edge-based centrality measures to equivalent point-based.
#'
#' @param data_dir The data directory
#' @return Equivalent point- or vertex-based measures of centrality
#' @export
centrality_edge_to_point <- function (data_dir)
{
    hw <- readRDS (file.path (data_dir, "osm", "ny-hw.Rds"))
    dodgr::dodgr_cache_off ()
    net <- dodgr::weight_streetnet (hw, wt_profile = "foot")
    v <- dodgr::dodgr_vertices (net)

    cent_e <- readRDS (file.path (data_dir, "ny-centrality-edge.Rds"))
    cent_v <- dplyr::group_by (cent_e, .vx1) %>%
        dplyr::summarise (id = min (.vx1),
                          x = min (.vx1_x),
                          y = min (.vx1_y),
                          centrality = sum (centrality))
    
    cent_v [cent_v$.vx1 %in% v$id, ]
}
