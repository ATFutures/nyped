#' nyped_data
#'
#' Download, clean, and return New York City pedestrian count data
#' @param data_dir The directory in which data are to be, or have previously
#' been, downloaded.
#' @return A `data.frame` of pedestrian counts, and geographical coordinates,
#' with counts for weekdays, weekends, and "week" derived as a weighted
#' combintaion of both.
#'
#' @examples
#' dat <- nyped_data ()
#' # library (mapview)
#' # mapview (dat, cex = "week", zcol = "week")
#' @export
nyped_data <- function (data_dir = tempdir ())
{
    message (cli::rule (left = "calibration", line = 2, col = "green"))
    check_ped_file (data_dir)
    files <- load_ped_file (data_dir)
    f <- files [grep ("\\.shp", files)]

    message (cli::symbol$pointer, " Loading pedestrian data",
             appendLF = FALSE)
    x <- sf::st_read (f, stringsAsFactors = FALSE, quiet = TRUE)
    x <- sf::st_transform (x, 4326)
    xdat <- x
    xdat$geometry <- NULL
    index <- grep ("\\_AM|\\_PM|\\_MD", names (xdat))
    for (i in index)
    {
        xdat [, i] [xdat [, i] == "-"] <- "0"
        xdat [, i] <- as.integer (gsub (",", "", xdat [, i]))
    }
    index <- grep ("\\_AM|\\_PM", names (xdat))
    weekday <- rowSums (xdat [, index])
    index <- grep ("\\_MD", names (xdat))
    weekend <- rowSums (xdat [, index])

    message ("\r", cli::symbol$tick, " Loaded pedestrian data  ")

    sf::st_sf (weekday = weekday,
               weekend = weekend,
               week = round ((5 * weekday + 2 * weekend) / 7),
               geometry = x$geometry)
}

ped_file_name <- function ()
{
    "nycdot-bi-annual-pedestrian-index.zip"
}

check_ped_file <- function (data_dir = tempdir ())
{
    f <- file.path (data_dir, ped_file_name ())
    if (!file.exists (f))
    {
        message (cli::symbol$pointer, " Downloading pedestrian data",
                 appendLF = FALSE)
        u <- paste0 ("https://www1.nyc.gov/html/dot/downloads/misc/",
                     "nycdot-bi-annual-pedestrian-index.zip")
        check <- utils::download.file (url = u, destfile = f, quiet = TRUE)
        if (check != 0)
            stop ("Failed to download station location data") # nocov
        message ("\r", cli::symbol$tick, " Downloaded pedestrian data  ")
    }
}

load_ped_file <- function (data_dir = tempdir ())
{
    f <- file.path (data_dir, ped_file_name ())
    if (!file.exists (f))
        stop ("pedestrian data file does not exist") # nocov

    utils::unzip (f, exdir = data_dir)
}
