#' nysubway
#'
#' Get New York City subway station counts and coordinates
#' @return A `data.frame` of subway naes, counts, and geographical coordinates
#' @export
nysubway <- function ()
{
    dl_ridership ()
    strip_to_table ()
    clean_ridership ()
    counts <- read_ridership ()

    xy <- dl_locations ()
    match_counts_to_stns (counts, xy)
}

dl_ridership <- function ()
{
    u <- "http://web.mta.info/nyct/facts/ridership/ridership_sub_annual.htm"
    x <- xml2::read_html (u)
    xml2::write_xml (x, file.path (tempdir (), "junk.html"))
}

# strip the raw ridership data down to main table
strip_to_table <- function ()
{
    x <- readLines (file.path (tempdir (), "junk.html"))
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
    x <- gsub (paste0 (p, '1_16.gif" alt="1 subway" align="texttop">'), "(1)", x)
    x <- gsub (paste0 (p, '1_16.gif" alt="1 subway" border="0" align="texttop">'), "(1)", x)
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
        x [[n]] <- as.integer (gsub (",", "", x [[n]]))
    x <- x [, 1:which (names (x) == nms [length (nms)])]
    index <- apply (x, 1, function (i) length (which (is.na (i))))
    x <- x [which (index < 6), ]

    # Then put the line information in a different column
    stations <- x [, 1]
    # remove funny terminal characters arising in some stations:
    stations <- gsub ("Â", "", stations)
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

dl_locations <- function ()
{
    message ("Downloading station location data")
    u <- "https://data.cityofnewyork.us/api/views/kk4q-3rt2/rows.csv?accessType=DOWNLOAD"
    utils::download.file (url = u,
                          destfile = file.path (tempdir (), "nystations.csv"))
    utils::read.csv (file.path (tempdir (), "nystations.csv"),
                     stringsAsFactors = FALSE)
}

match_counts_to_stns <- function (counts, xy)
{
    xy$NAME <- gsub("(\\d)(st|nd|rd|th)\\b", "\\1", xy$NAME)
    xy$NAME <- gsub ("\\s-\\s", "-", xy$NAME)
    xy$NAME <- gsub ("Av-", "Ave-", xy$NAME)
    x$Station <- gsub ("Av-", "Ave-", x$Station)
    xy$NAME <- gsub ("Av$", "Ave", xy$NAME)
    x$Station <- gsub ("Av$", "Ave", x$Station)
    xy$NAME <- gsub ("Avs", "Aves", xy$NAME)
    x$Station <- gsub ("Avs", "Aves", x$Station)

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
    x$Station <- gsub ("Ctr", "Center", x$Station)
    xy$NAME <- gsub ("'s\\s", "s ", xy$NAME)
    x$Station <- gsub ("'s\\s", "s ", x$Station)
    xy$NAME <- gsub ("Pky|Pkwy", "Parkway", xy$NAME)
    x$Station <- gsub ("Pky|Pkwy", "Parkway", x$Station)
    xy$NAME <- gsub ("Hts", "Heights", xy$NAME)
    x$Station <- gsub ("Hts", "Heights", x$Station)
    xy$NAME <- gsub ("Bklyn", "Brooklyn", xy$NAME)
    x$Station <- gsub ("Bklyn", "Brooklyn", x$Station)
    xy$NAME <- gsub ("Ft", "Fort", xy$NAME)
    x$Station <- gsub ("Ft", "Fort", x$Station)
    xy$NAME <- gsub ("Rd", "Road", xy$NAME)
    x$Station <- gsub ("Rd", "Road", x$Station)
    xy$NAME <- gsub ("NY", "New York", xy$NAME)
    x$Station <- gsub ("NY", "New York", x$Station)
    xy$NAME <- gsub ("Pl$", "Place", xy$NAME)
    x$Station <- gsub ("Pl$", "Place", x$Station)
    xy$NAME <- gsub ("Tpke", "Turnpike", xy$NAME)
    x$Station <- gsub ("Tpke", "Turnpike", x$Station)

    # This is a typo:
    x$Station <- gsub ("Beverley", "Beverly", x$Station)
    # And these have to be custom-matched:
    s <- c ("Bushwick", "Flatbush", "Union Sq", "Herald Sq", "^81 St", "Cathedral",
            "Central Park North", "^33 St", "^39 Ave", "^40 St", "^46 St",
            "^75 St-", "Beach 67 St", "Jamaica Center", "Sutphin Blvd-Archer Ave",
            "Newkirk")
    for (i in s)
        xy$NAME [grep (i, xy$NAME)] <- x$Station [grep (i, x$Station)]

    xy$NAME [grepl ("^57 St", xy$NAME) & xy$LINE == "F"] <- 
        x$Station [grepl ("^57 St", x$Station) & x$ids == "F"] <-
            "57 St F"
    xy [grepl ("^57 St", xy$NAME), ]
    xy$NAME [grepl ("^57 St", xy$NAME) & grepl ("N", xy$LINE)] <- 
        x$Station [grepl ("^57 St", x$Station) & x$ids == "N,Q,R,W"] <-
            "57 St NQRW"

    xy$NAME [grep ("New YorkU", xy$NAME)] <-
        x$Station [grep ("New York University", x$Station)]
    x$Station [grep ("WTC", x$Station)] <- "World Trade Center"
    x$Station [grep ("^74", x$Station)] <- xy$NAME [grep ("^74 St", xy$NAME)]

    xy$LINE <- gsub (" Express|Express-", "", xy$LINE)
    xy$LINE <- gsub ("-", "", xy$LINE)

    index <- apply (x, 1, function (i) {
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
    x$Station [i]
    xy$NAME [index [i]]


    data.frame (name = x$Station,
                count2013 = x$X2013,
                count2014 = x$X2014,
                count2015 = x$X2015,
                count2016 = x$X2016,
                count2017 = x$X2017,
                count2018 = x$X2018,
                geom = xy$the_geom [index],
                stringsAsFactors = FALSE)
}
