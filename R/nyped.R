#' nyped_data
#'
#' Download, clean, and return New York City pedestrian count data
#' @param data_dir The directory in which data are to be, or have previously
#' been, downloaded.
#' @param quiet If `FALSE`, display progress information on screen
#' @return A `data.frame` of pedestrian counts, and geographical coordinates,
#' with counts for weekdays, weekends, and "week" derived as a weighted
#' combintaion of both.
#'
#' @examples
#' dat <- nyped_data ()
#' # library (mapview)
#' # mapview (dat, cex = "week", zcol = "week")
#' @export
nyped_data <- function (data_dir = tempdir (), quiet = FALSE)
{
    # pedestrian count data from
    # https://www1.nyc.gov/html/dot/html/about/datafeeds.shtml#Pedestrians
    # counts are weekdays ("\_D") with "\_AM" and "\_PM" counts, and weekends
    # ("\_D2") with single counts ("\_MD")
    check_ped_file (data_dir, quiet = quiet)
    files <- load_ped_file (data_dir)
    f <- files [grep ("\\.shp", files)]

    if (!quiet)
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
    # average measures over the various count days, obtaining single estimates
    # of pedestrian volumes within a 3-hour window
    index <- grep ("\\_AM|\\_PM", names (xdat))
    weekday <- round (rowSums (xdat [, index]) / length (index))
    index <- grep ("\\_MD", names (xdat))
    weekend <- round (rowSums (xdat [, index]) / length (index))

    if (!quiet)
        message ("\r", cli::symbol$tick, " Loaded pedestrian data  ")

    sf::st_sf (borough = xdat$Borough,
               street = xdat$Street_Nam,
               from = xdat$From_Stree,
               to = xdat$To_Street,
               weekday = weekday,
               weekend = weekend,
               week = round ((5 * weekday + 2 * weekend) / 7),
               geometry = x$geometry,
               stringsAsFactors = FALSE)
}

ped_file_name <- function ()
{
    "nycdot-bi-annual-pedestrian-index.zip"
}

check_ped_file <- function (data_dir = tempdir (), quiet = FALSE)
{
    f <- file.path (data_dir, ped_file_name ())
    if (!file.exists (f))
    {
        if (!quiet)
            message (cli::symbol$pointer, " Downloading pedestrian data",
                     appendLF = FALSE)
        u <- paste0 ("https://www1.nyc.gov/html/dot/downloads/misc/",
                     "nycdot-bi-annual-pedestrian-index.zip")
        check <- utils::download.file (url = u, destfile = f, quiet = TRUE)
        if (check != 0)
            stop ("Failed to download station location data") # nocov
        if (!quiet)
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
