#' plot_ny_popdens
#'
#' Plot the population density of New York City via 'mapdeck'
#'
#' @inheritParams nypopdens
#' @note This function presumes that a mapdeck token exists as an environmental
#' variable with a name that includes "mapbox"
#' @export
plot_ny_popdens <- function (data_dir)
{
    f <- file.path (data_dir, "worldpop", "pop-points-all.Rds")
    if (!file.exists (f))
        stop ("Population density data not found, ",    # nocov
              "or have not been processed.")            # nocov
    x <- readRDS (f)

    requireNamespace ("mapdeck")
    mapdeck::set_token (get_mapbox_token ())

    x$cols <- ceiling (100 * x$pop / max (x$pop, na.rm = TRUE))
    mapdeck::mapdeck (style = 'mapbox://styles/mapbox/light-v10') %>%
        mapdeck::add_pointcloud (x, fill_colour = "cols", legend = TRUE)
}

get_mapbox_token <- function ()
{
    e <- Sys.getenv ()
    i <- e [grep ("mapbox", names (e), ignore.case = TRUE)]
    tok <- unique (as.character (i))
    if (length (tok) != 1)
        stop ("There is no unique envvar with name including 'mapbox'")
    return (tok)
}

#' plot_ped_counts
#'
#' Plot pedestrian count data for New York City
#' @param type One of "weekday", "weekend", or "week" (aggregate of both)
#' @export
plot_ped_counts <- function (type = "week")
{
    type <- match.arg (type, c ("weekday", "weekend", "week"))
    x <- nyped_data ()
    mapview::mapview (x, cex = type, zcol = type)
}

#' plot_subway_counts
#'
#' Plot subway entrace/exit count data for New York City
#' @param year Year in [2013:2018]
#' @export
plot_subway_counts <- function (year = 2018L)
{
    if (!is.integer (year))
        year <- as.integer (year)
    if (length (year) != 1)
        stop ("year must be a single value in [2013:2018]")

    x <- nysubway_data ()
    ycol <- paste0 ("count", year)
    mapview::mapview (x, cex = ycol, zcol = ycol)
}
