#' nypopdens
#'
#' Get population density data for New York City
#'
#' @param data_dir The directory in which data are to be, or have previously
#' been, downloaded.
#' @return Population density data
#' @export
nypopdens <- function (data_dir = tempdir ())
{
    check_uspop_file (data_dir)
    cut_pop_raster (data_dir)
}

pop_file_name <- function (data_dir, type = "usa")
{
    if (!type == "usa")
        type <- "ny"
    file.path (data_dir, paste0 (type, "_ppp_2019.tif"))
}

check_uspop_file <- function (data_dir = tempdir ())
{
    f <- pop_file_name (data_dir, type = "usa")
    if (!file.exists (f))
    {
        message (cli::symbol$pointer, " Downloading population density data",
                 appendLF = FALSE)
        u <- paste0 ("ftp://ftp.worldpop.org.uk/GIS/Population/",
                     "Global_2000_2020/2019/USA/", basename (f))
        check <- utils::download.file (url = u, destfile = f, quiet = TRUE)
        if (check != 0)
            stop ("Failed to download station location data") # nocov
        message ("\r", cli::symbol$tick, " Downloaded population density data  ")
    } else
    {
        message (cli::symbol$tick, " Population density data already downloaded ")
    }
}

cut_pop_raster <- function (data_dir)
{
    fus <- pop_file_name (data_dir, type = "usa")
    if (!file.exists (fus))
        stop ("Population density file not found") # nocov
    fny <- pop_file_name (data_dir, type = "ny")
    if (!file.exists (fny))
    {
        message (cli::symbol$pointer, " Cropping US data to NYC only",
                 appendLF = FALSE)
        city <- "new york"
        expand <- 0.1
        bb <- osmdata::getbb (city)
        bb_exp <- t (apply (bb, 1, function (i)
                            i + c (-expand, expand) * diff (i)))
        r <- raster::raster (fus) %>%
            raster::crop (bb_exp)
        raster::writeRaster (r, fny)
        message ("\r", cli::symbol$tick, " Cropped US data to NYC only ")
    } else
    {
        message (cli::symbol$tick,
                 " Population density data already cropped to NYC")
    }
}

popdens_to_points <- function (data_dir)
{
    fny <- pop_file_name (data_dir, type = "ny")
    if (!file.exists (fny))
        stop ("New York population density file not found") # nocov

    ras <- raster::raster(fny)
    hw <- readRDS (file.path (data_dir, "ny-hw.Rds"))
    hw <- dodgr::weight_streetnet (hw, wt_profile = "foot")
    hw_c <- dodgr::dodgr_contract_graph (hw)
    nodes <- dodgr::dodgr_vertices (hw_c)
    osm_ids <- nodes$id # only used to check below that all worked
    nodes <- sf::st_as_sf (nodes, coords = c ("x", "y"), crs = 4326)
    #nodes_new <- assign_points (ras, nodes)

    pd_sf <- ras %>%
        raster::rasterToPolygons () %>%
        sf::st_as_sf ()
    pd_sf$n <- 1:nrow (pd_sf)
    # pd_sf has crs with offset values of all 0, but still different so:
    nodes <- sf::st_transform (nodes, sf::st_crs (pd_sf))
    nodes_joined <- sf::st_join (nodes, pd_sf)
    layer_name <- names (nodes_joined) [grep ("ppp_", names (nodes_joined))]

    # with nodes now all vertices with osm_id values courtesy of dodgr, the
    # desired values are simply
    nodes_new <- nodes_joined
    nodes_new$pop <- nodes_new [[layer_name]] /
        sum (nodes_new [[layer_name]], na.rm = TRUE)
    saveRDS (nodes_new, file = "/data/data/moveability/nyc/worldpop/pop-points-all.Rds")
}
