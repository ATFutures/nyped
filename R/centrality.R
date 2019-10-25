#' centrality_edge_to_point
#'
#' Convert edge-based centrality measures to equivalent point-based.
#'
#' @param data_dir The data directory
#' @param save If `TRUE`, save resultant vertex-based centrality measures in
#' `data_dir`.
#' @return Equivalent point- or vertex-based measures of centrality
#' @export
centrality_edge_to_point <- function (data_dir, save = FALSE)
{
    hw <- readRDS (file.path (data_dir, "osm", "ny-hw.Rds"))
    dodgr::dodgr_cache_off ()
    net <- dodgr::weight_streetnet (hw, wt_profile = "foot")
    p <- ped_osm_id (data_dir = data_dir, net = net, quiet = TRUE)
    s <- subway_osm_id (data_dir = data_dir, net = net, quiet = TRUE)
    net <- dodgr::dodgr_contract_graph (net, verts = unique (c (p$id, s$id)))
    v <- dodgr::dodgr_vertices (net)

    .vx1 <- .vx1_x <- .vx1_y <- NULL # no visible binding notes
    cent_e <- readRDS (file.path (data_dir, "ny-centrality-edge.Rds"))
    cent_v <- dplyr::group_by (cent_e, .vx1) %>%
        dplyr::summarise (id = min (.vx1),
                          x = min (.vx1_x),
                          y = min (.vx1_y),
                          centrality = sum (centrality))
    
    res <- cent_v [cent_v$.vx1 %in% v$id, ]
    saveRDS (res, file.path (data_dir, "ny-centrality-vertex.Rds"))
    return (res)
}
