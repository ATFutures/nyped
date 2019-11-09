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
    x <- sf::st_as_sf (x, wkt = "geom", crs = 4326)

    exit_map <- stn_to_subway_exits ()
    # remove stations to which no counts are allocated:
    exit_map <- exit_map [which (exit_map$stn %in% unique (x$index)), ]
    # re-allocate (x,y) for stations which have no closest exits back to the
    # original station coordinates (there are only around 5 of these)
    x_x <- vapply (x$geom, function (i) i [1], numeric (1))
    x_y <- vapply (x$geom, function (i) i [2], numeric (1))
    index <- which (is.na (exit_map$x))
    exit_map$x [index] <- x_x [exit_map$stn [index]]
    exit_map$y [index] <- x_y [exit_map$stn [index]]

    # get number of exits for each station
    stn_counts <- data.frame (stn = x$index,
                              count = x$count2018)
    n_exits <- dplyr::left_join (exit_map, stn_counts, by = "stn") %>%
        dplyr::group_by (stn) %>%
        dplyr::summarise (n = length (exit))
    exit_map$n_exits <- n_exits$n [match (exit_map$stn, n_exits$stn)]
    exit_map$count <- stn_counts$count [match (exit_map$stn, stn_counts$stn)]
    exit_map$count <- exit_map$count / exit_map$n_exits

    sf::st_as_sf (exit_map [, c ("count", "x", "y")], coords = c ("x", "y"),
                  crs = 4326)
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

stn_to_subway_exits <- function ()
{
    # match locations to lines:
    xy <- dl_locations (quiet = TRUE)
    xy <- sf::st_as_sf (xy, wkt = "the_geom", crs = 4326)
    xy$x <- vapply (xy$the_geom, function (i) i [1], numeric (1))
    xy$y <- vapply (xy$the_geom, function (i) i [2], numeric (1))
    xy_lines <- strsplit (gsub (" Express", "", xy$LINE), "-")
    xy_lines <- lapply (xy_lines, function (i) unique (i))

    # download entrances and exits
    f <- file.path (tempdir (), "ny-subway-exits.csv")
    if (!file.exists (f))
    {
        u <- paste0 ("https://data.cityofnewyork.us/api/views/",
                     "he7q-3hwy/rows.csv?accessType=DOWNLOAD")
        check <- utils::download.file (url = u, destfile = f, quiet = TRUE)
    }
    exits <- read.csv (f, stringsAsFactors = FALSE)
    exits <- sf::st_as_sf (exits, wkt = "the_geom", crs = 4326)
    exits$x <- vapply (exits$the_geom, function (i) i [1], numeric (1))
    exits$y <- vapply (exits$the_geom, function (i) i [2], numeric (1))
    # one exit has "e" instead of "E", so toupper is needed:
    exit_lines <- strsplit (toupper (exits$LINE), "-")

    all_lines <- unique (unlist (xy_lines))
    nearest <- vector ("list", length (all_lines))
    for (i in seq (all_lines))
    {
        stn_index <- which (vapply (xy_lines, function (j)
                                    all_lines [[i]] %in% j, logical (1)))
        exit_index <- which (vapply (exit_lines, function (j)
                                     all_lines [[i]] %in% j, logical (1)))

        xy_stn <- xy [stn_index, c ("x", "y"), drop = TRUE]
        xy_exit <- exits [exit_index, c ("x", "y"), drop = TRUE]
        dmat <- geodist::geodist (xy_stn, xy_exit)
        # nearest station to each exit of that line:
        stn_index_i <- stn_index [apply (dmat, 2, which.min)]
        nearest [[i]] <- cbind (stn_index_i, exit_index)
    }
    nearest <- data.frame (do.call (rbind, nearest))
    nearest <- nearest [which (!duplicated (nearest)), ]
    names (nearest) <- c ("stn", "exit")

    # not all stations have closest exits, so those are subsequent mapped back
    # on to the station location itself
    index <- which (!seq (nrow (xy)) %in% sort (unique (nearest$stn)))
    nearest <- rbind (nearest, data.frame (stn = index,
                                           exit = rep (NA, length (index))))
    nearest <- nearest [order (nearest [, 1]), ]

    # finally, append coordinates of exits
    nearest$x <- exits$x [nearest$exit]
    nearest$y <- exits$y [nearest$exit]

    return (nearest)
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
    #xy [grepl ("^57 St", xy$NAME), ]
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

    data.frame (name = counts$Station,
                count2013 = counts$X2013,
                count2014 = counts$X2014,
                count2015 = counts$X2015,
                count2016 = counts$X2016,
                count2017 = counts$X2017,
                count2018 = counts$X2018,
                index = index, # index into original xy station file
                geom = xy$the_geom [index],
                stringsAsFactors = FALSE)
}
