#' nyosm_data
#'
#' Get OpenStreetMap data for New York City
#'
#' @param data_dir The directory in which data are to be, or have previously
#' been, downloaded.
#' @return A list of three items: (1) the street network; (2) building polygons;
#' and (3) green space polygons
#' @export
nyosm_data <- function (data_dir)
{
    list (osm = ny_osm_hw (data_dir),
          green = ny_green (data_dir),
          attr = ny_attractors (data_dir))
}

ny_osm_hw <- function (data_dir)
{
    f <- file.path (data_dir, "osm", "ny-hw.Rds")
    if (!file.exists (f))
    {
        message (cli::symbol$pointer, " Downloading openstreetmap data",
                 appendLF = FALSE)
        bb <- osmdata::getbb ("new york city", format_out = "polygon")
        q <- osmdata::opq (bb) %>%
            osmdata::add_osm_feature (key = "highway")
        doc <- file.path (data_dir, "osm", "ny.osm")
        hw <- osmdata::osmdata_xml (q, filename = doc, quiet = FALSE)
        hw <- osmdata::osmdata_sc (q, doc = doc)
        hw <- osmdata::trim_osmdata (hw, bb)
        saveRDS (hw, f)
        file.remove (doc)
        message ("\r", cli::symbol$tick, " Downloaded openstreetmap data  ")
    } else
    {
        message (cli::symbol$tick, " Openstreetmap data already downloaded")
    }
}

ny_green <- function (data_dir)
{
    f <- file.path (data_dir, "osm", "ny-green.Rds")
    if (!file.exists (f))
    {
        message (cli::symbol$pointer, " Downloading green space data",
                 appendLF = FALSE)
        green_polys <- moveability::get_green_space ("new york city")
        saveRDS (green_polys, f)
        message ("\r", cli::symbol$tick, " Downloaded green space data  ")
    } else
    {
        message (cli::symbol$tick, " Green space data already downloaded")
    }
}

ny_attractors <- function (data_dir)
{
    f <- file.path (data_dir, "osm", "ny-attractors.Rds")
    if (!file.exists (f))
    {
        message (cli::symbol$pointer, " Downloading trip attractor data",
                 appendLF = FALSE)
        attractors <- moveability::get_attractors ("new york city")
        saveRDS (attractors, f)
        message ("\r", cli::symbol$tick, " Downloaded trip attractor data  ")
    } else
    {
        message (cli::symbol$tick, " Trip attractor data already downloaded")
    }
}
