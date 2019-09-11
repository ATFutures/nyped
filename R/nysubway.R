#' nysubway_data
#'
#' Download, clean, and join New York City subway station counts and coordinates
#' @param quiet If `FALSE`, display progress information on screen
#' @return A `data.frame` of subway names, annual counts, and geographical
#' coordinates.
#'
#' @examples
#' dat <- nysubway_data ()
#' # library (mapview)
#' # mapview (dat, cex = "count2018", zcol = "count2018")
#' @export
nysubway_data <- function (quiet = FALSE)
{
    dl_ridership (quiet = quiet)
    strip_to_table ()
    clean_ridership ()
    counts <- read_ridership ()

    xy <- dl_locations (quiet = quiet)
    x <- match_counts_to_stns (counts, xy)
    sf::st_as_sf (x, wkt = "geom", crs = 4326)
}

dl_ridership <- function (quiet = FALSE)
{
    f <- file.path (tempdir (), "ridership.html")
    if (!file.exists (f))
    {
        if (!quiet)
            message (cli::symbol$pointer, " Downloading ridership data",
                     appendLF = FALSE)

        # http://web.mta.info/nyct/facts/ridership/#chart_s
        u <- "http://web.mta.info/nyct/facts/ridership/ridership_sub_annual.htm"
        x <- xml2::read_html (u)
        xml2::write_xml (x, file.path (tempdir (), "ridership.html"))
        if (!quiet)
            message ("\r", cli::symbol$tick, " Downloaded ridership data  ")
    } else if (!quiet)
    {
        message (cli::symbol$tick, " Ridership data already downloaded ")
    }
}

# strip the raw ridership data down to main table
strip_to_table <- function ()
{
    x <- readLines (file.path (tempdir (), "ridership.html"))
    lstart <- grep ("<table id=\"subway\"", x)
    n <- which (grep ("<table", x) == lstart)
    lend <- grep ("</table>", x)
    lend <- lend [length (lend) - n + 1]
    lbody <- grep ("<body", x)
    x <- x [c (1:lbody, lstart:lend)]
    writeLines (x, file.path (tempdir (), "junk.html"))
}

# search for all instances of "colspan" and remove the full span dividers. The
# symbols for the subways lines are then also read as table columns, so these
# must be converted to equivalent numbers and letters only as part of the
# station names.
clean_ridership <- function ()
{
    x <- readLines (file.path (tempdir (), "junk.html"))
    p <- '<img src="../../service/images/'
    x <- gsub (paste0 (p, '1_16.gif" alt="1 subway" align="texttop">'),
               "(1)", x)
    x <- gsub (paste0 (p, '1_16.gif" alt="1 subway" ',
                       'border="0" align="texttop">'), "(1)", x)

    for (i in 2:7)
        x <- gsub (paste0 (p, i, '.png" alt="', i, ' subway" align="texttop">'), 
                   paste0 ("(", i, ")"), x)
    for (i in letters)
        x <- gsub (paste0 (p, i, '.png" alt="', toupper (i),
                           ' subway" align="texttop">'),
                   paste0 ("(", toupper (i), ")"), x)
    # compound the lines to within a single pair of brackets
    x <- gsub ("\\)\\(", ",", x)
    writeLines (x, file.path (tempdir (), "junk.html"))
}

read_ridership <- function ()
{
    x <- xml2::read_html (file.path (tempdir (), "junk.html")) %>%
        rvest::html_table (fill = TRUE)
    x <- x [[1]]
    nms <- as.character (2013:2018)
    for (n in nms)
        x [[n]] <- suppressWarnings (as.integer (gsub (",", "", x [[n]])))
    x <- x [, 1:which (names (x) == nms [length (nms)])]
    index <- apply (x, 1, function (i) length (which (is.na (i))))
    x <- x [which (index < 6), ]

    # Then put the line information in a different column
    stations <- x [, 1]
    # remove funny terminal characters arising in some stations:
    #stations <- gsub ("Â", "", stations)
    # Some station locations share 2 names, but only one is needed, so remove 2nd:
    stations <- gsub ("|\\/.*", "", stations)
    # concatenate all line IDs
    stations <- gsub ("\\)(\\s*)\\(", ",", stations)
    ids <- gsub (".*\\(|\\)*", "", stations)
    ids [grep ("[a-z]", ids)] <- ""
    stations <- gsub ("|\\(.*", "", stations)
    stations <- trimws (stations) # terminal whitespace
    # reduce multiple whitespaces to single
    stations <- gsub ("(?<=[\\s])\\s*|^\\s+|\\s+$", "", stations, perl=TRUE)

    names (x) [1] <- "Station"
    x$Station <- stations
    x <- data.frame ("Station" = x$Station,
                     ids, 
                     x [, 2:ncol (x)],
                     stringsAsFactors = FALSE)
    x <- x [!x$Station %in% c ("Brooklyn", "Bronx", "Manhattan", "Queens",
                               "Systemwide Adjustment", "System Total"), ]
    return (x)
}

dl_locations <- function (quiet = FALSE)
{
    f <- file.path (tempdir (), "nystations.csv")
    if (!file.exists (f))
    {
        if (!quiet)
            message (cli::symbol$pointer, " Downloading station location data",
                     appendLF = FALSE)
        # https://data.cityofnewyork.us/Transportation/Subway-Stations/arq3-7z49
        u <- paste0 ("https://data.cityofnewyork.us/api/",
                     "views/kk4q-3rt2/rows.csv?accessType=DOWNLOAD")
        check <- utils::download.file (url = u, destfile = f, quiet = TRUE)
        if (check != 0)
            stop ("Failed to download station location data") # nocov
        if (!quiet)
            message ("\r", cli::symbol$tick, " Downloaded station location data")
    } else if (!quiet)
    {
        message (cli::symbol$tick, " Station location data already downloaded")
    }

    utils::read.csv (f, stringsAsFactors = FALSE)
}

match_counts_to_stns <- function (counts, xy)
{
    xy$NAME <- gsub("(\\d)(st|nd|rd|th)\\b", "\\1", xy$NAME)
    xy$NAME <- gsub ("\\s-\\s", "-", xy$NAME)
    xy$NAME <- gsub ("Av-", "Ave-", xy$NAME)
    counts$Station <- gsub ("Av-", "Ave-", counts$Station)
    xy$NAME <- gsub ("Av$", "Ave", xy$NAME)
    counts$Station <- gsub ("Av$", "Ave", counts$Station)
    xy$NAME <- gsub ("Avs", "Aves", xy$NAME)
    counts$Station <- gsub ("Avs", "Aves", counts$Station)

    xy$NAME <- gsub ("(E)\\s+([1-9])", "East \\2", xy$NAME)
    xy$NAME <- gsub ("(N)\\s+([1-9])", "North \\2", xy$NAME)
    xy$NAME <- gsub ("(W)\\s+([1-9])", "West \\2", xy$NAME)
    xy$NAME <- gsub ("(S)\\s+([1-9])", "South \\2", xy$NAME)
    xy$NAME <- gsub ("-E\\s", "-East ", xy$NAME)
    xy$NAME <- gsub ("-N\\s", "-North ", xy$NAME)
    xy$NAME <- gsub ("-W\\s", "-West ", xy$NAME)
    xy$NAME <- gsub ("-S\\s", "-South ", xy$NAME)

    xy$NAME <- gsub ("(Ave)\\s+([a-zA-Z])", "Avenue \\2", xy$NAME)

    xy$NAME <- gsub ("Plz", "Plaza", xy$NAME)
    xy$NAME <- gsub ("Ctr", "Center", xy$NAME)
    counts$Station <- gsub ("Ctr", "Center", counts$Station)
    xy$NAME <- gsub ("'s\\s", "s ", xy$NAME)
    counts$Station <- gsub ("'s\\s", "s ", counts$Station)
    xy$NAME <- gsub ("Pky|Pkwy", "Parkway", xy$NAME)
    counts$Station <- gsub ("Pky|Pkwy", "Parkway", counts$Station)
    xy$NAME <- gsub ("Hts", "Heights", xy$NAME)
    counts$Station <- gsub ("Hts", "Heights", counts$Station)
    xy$NAME <- gsub ("Bklyn", "Brooklyn", xy$NAME)
    counts$Station <- gsub ("Bklyn", "Brooklyn", counts$Station)
    xy$NAME <- gsub ("Ft", "Fort", xy$NAME)
    counts$Station <- gsub ("Ft", "Fort", counts$Station)
    xy$NAME <- gsub ("Rd", "Road", xy$NAME)
    counts$Station <- gsub ("Rd", "Road", counts$Station)
    xy$NAME <- gsub ("NY", "New York", xy$NAME)
    counts$Station <- gsub ("NY", "New York", counts$Station)
    xy$NAME <- gsub ("Pl$", "Place", xy$NAME)
    counts$Station <- gsub ("Pl$", "Place", counts$Station)
    xy$NAME <- gsub ("Tpke", "Turnpike", xy$NAME)
    counts$Station <- gsub ("Tpke", "Turnpike", counts$Station)

    # This is a typo:
    counts$Station <- gsub ("Beverley", "Beverly", counts$Station)
    # And these have to be custom-matched:
    s <- c ("Bushwick", "Flatbush", "Union Sq", "Herald Sq", "^81 St", "Cathedral",
            "Central Park North", "^33 St", "^39 Ave", "^40 St", "^46 St",
            "^75 St-", "Beach 67 St", "Jamaica Center", "Sutphin Blvd-Archer Ave",
            "Newkirk")
    for (i in s)
        xy$NAME [grep (i, xy$NAME)] <- counts$Station [grep (i, counts$Station)]

    xy$NAME [grepl ("^57 St", xy$NAME) & xy$LINE == "F"] <- 
        counts$Station [grepl ("^57 St", counts$Station) & counts$ids == "F"] <-
            "57 St F"
    xy [grepl ("^57 St", xy$NAME), ]
    xy$NAME [grepl ("^57 St", xy$NAME) & grepl ("N", xy$LINE)] <- 
        counts$Station [grepl ("^57 St", counts$Station) & counts$ids == "N,Q,R,W"] <-
            "57 St NQRW"

    xy$NAME [grep ("New YorkU", xy$NAME)] <-
        counts$Station [grep ("New York University", counts$Station)]
    counts$Station [grep ("WTC", counts$Station)] <- "World Trade Center"
    counts$Station [grep ("^74", counts$Station)] <- xy$NAME [grep ("^74 St", xy$NAME)]

    xy$LINE <- gsub (" Express|Express-", "", xy$LINE)
    xy$LINE <- gsub ("-", "", xy$LINE)

    index <- apply (counts, 1, function (i) {
        index <- grep (i [1], xy$NAME)
        if (length (index) == 0)
            return (NA)
        if (length (index) == 1)
            return (index)
        ids <- gsub (",", "", i [2])
        index2 <- grep (substr (ids, 1, 1), xy$LINE [index])
        if (length (index2) == 0)
            return (NA)
        return (index [index2] [1])
    })
    index <- unname (index)

    length (which (is.na (index))) # 136 -> 111 -> 1
    index2 <- which (is.na (index))
    i <- index2 [1]
    counts$Station [i]
    xy$NAME [index [i]]


    data.frame (name = counts$Station,
                count2013 = counts$X2013,
                count2014 = counts$X2014,
                count2015 = counts$X2015,
                count2016 = counts$X2016,
                count2017 = counts$X2017,
                count2018 = counts$X2018,
                geom = xy$the_geom [index],
                stringsAsFactors = FALSE)
}
