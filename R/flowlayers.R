#' get_layer
#'
#' Append network with flow columns between nominated places
#' @param net Weighted street network; loaded from `data_dir` if not provided
#' @param from Category of origins for pedestrian flows; one of "subway" or
#' "residential"
#' @param to Category of destinations for pedestrian flows; one of
#' "residential", "education", "entertainment", "healthcare", "sustenance",
#' "transportation", or "disperse" for a general dispersal model.
#' @param data_dir The directory in which data are to be, or have previously
#' been, downloaded.
#' @export
get_layer <- function (net, from = "subway", to = "disperse", data_dir)
{
    f <- file.path (data_dir, "calibration",
                    paste0 ("net-", substr (from, 1, 3), "-",
                            substr (to, 1, 3), ".Rds"))
    net_f <- NULL
    if (!file.exists (f))
    {
        net_f <- get_layer_internal (net, from = from, to = to, data_dir)
        saveRDS (net_f, file = f)
    }
    return (net_f)
}

get_layer_internal <- function (net, from = "subway", to = "disperse", data_dir)
{
    p <- ped_osm_id (data_dir = data_dir, net = net, quiet = TRUE)
    s <- subway_osm_id (data_dir = data_dir, net = net, quiet = TRUE)
    net <- dodgr::dodgr_contract_graph (net, verts = unique (c (p$id, s$id)))
    v <- dodgr::dodgr_vertices (net)

    txt <- paste0 (from, " to ", to)
    message (cli::rule (center = txt, line = 2, col = "green"))
    message (cli::col_green (cli::symbol$star), " Started at ",
             Sys.time ())

    ks <- 0
    k <- 1:30 * 100

    if (from == "subway")
        fr_dat <- get_subway_dat (s)
    else if (from == "residential")
        fr_dat <- get_res_dat (v, data_dir)
    else
        fr_dat <- get_attractor_layer (data_dir, v, type = from)

    nvals <- rep (fr_dat$n, length (k))
    kvals <- t (matrix (k ^ (1 + ks * nvals / max (nvals)), nrow = length (k)))

    if (to == "disperse")
    {
        message (cli::col_cyan (cli::symbol$star), " Disersing layer ... ",
                 appendLF = FALSE)
        st0 <- Sys.time ()
        net_f <- dodgr::dodgr_flows_disperse (net, from = fr_dat$id,
                                              dens = fr_dat$n, k = kvals)
        ti <- format_time_int (st0)
        message ("\r", cli::col_green (cli::symbol$tick),
                 " Disersed layer in ", ti)
    } else
    {
        if (to == "residential")
            to_dat <- get_res_dat (v, data_dir)
        else if (to == "subway")
            to_dat <- get_subway_dat (s)
        else
            to_dat <- get_attractor_layer (data_dir, v, type = to)

        if (any (is.na (fr_dat$n)))
            stop ("From-data contains NA values")
        if (any (is.na (to_dat$n)))
            stop ("To-data contains NA values")

        message (cli::col_cyan (cli::symbol$star), " Aggregating layer ... ",
                 appendLF = FALSE)
        st0 <- Sys.time ()
        net_f <- dodgr::dodgr_flows_si (net, from = fr_dat$id,
                                        to = to_dat$id, k = kvals,
                                        dens_from = fr_dat$n,
                                        dens_to = to_dat$n)
        ti <- format_time_int (st0)
        message ("\r", cli::col_green (cli::symbol$tick),
                 " Aggregated layer in ", ti)
    }

    return (net_f)
}

# return all pairwise combinations of flow layer categories, removing any that
# have already been calculated
from_to_pairs <- function (data_dir)
{
    categories <- c ("subway", "centrality", "residential", "transportation",
                     "sustenance", "entertainment", "education", "healthcare")
    ft <- data.frame (from = categories,
                      to = "disperse",
                      stringsAsFactors = FALSE)
    index <- t (utils::combn (length (categories), 2))
    ft <- rbind (ft,
                 data.frame (from = c (categories [index [, 1]],
                                       categories [index [, 2]]),
                             to = c (categories [index [, 2]],
                                     categories [index [, 1]]),
                             stringsAsFactors = FALSE))

    ftout <- NULL
    for (i in seq (nrow (ft)))
    {
        f <- file.path (data_dir, "calibration",
                        paste0 ("net-", substr (ft$from [i], 1, 3), "-",
                                substr (ft$to [i], 1, 3), ".Rds"))
        if (!file.exists (f))
            ftout <- rbind (ftout, c (ft$from [i], ft$to [i]))
    }
    ft <- data.frame (from = ftout [, 1],
                      to = ftout [, 2],
                      stringsAsFactors = FALSE)

    return (ft)
}



reverse_net <- function (net)
{
    vx0 <- net$.vx0
    net$.vx0 <- net$.vx1
    net$.vx1 <- vx0

    vx0_x <- net$.vx0_x
    net$.vx0_x <- net$.vx1_x
    net$.vx1_x <- vx0_x

    vx0_y <- net$.vx0_y
    net$.vx0_y <- net$.vx1_y
    net$.vx1_y <- vx0_y

    vx1_x <- net$.vx1_x
    net$.vx1_x <- net$.vx1_x
    net$.vx1_x <- vx1_x

    vx1_y <- net$.vx1_y
    net$.vx1_y <- net$.vx1_y
    net$.vx1_y <- vx1_y

    return (net)
}

get_res_dat <- function (v, data_dir)
{
    # pop values go up to around 3,000, so ones below this value are removed
    dens_limit <- 0.01

    nodes_new <- readRDS (file.path (data_dir, "worldpop", "pop-points.Rds"))
    layer_name <- names (nodes_new) [grep ("ppp_", names (nodes_new))]
    nodes_new$id <- v$id [dodgr::match_points_to_graph (v, nodes_new)]
    index <- which (!(nodes_new [[layer_name]] < dens_limit |
                      is.na (nodes_new [[layer_name]])))
    nodes_new <- nodes_new [index, ]
    data.frame (id = nodes_new$id,
                n = nodes_new [[grep ("ppp", names (nodes_new))]],
                stringsAsFactors = FALSE)
}

get_subway_dat <- function (s)
{
    data.frame (id = s$id,
                n = s$count,
                stringsAsFactors = FALSE)
}

get_attractor_layer <- function (data_dir, v, type = "education")
{
    type <- match.arg (type, c ("residential", "education", "entertainment",
                                "healthcare", "sustenance", "transportation",
                                "disperse", "centrality"))
    a <- NULL
    if (type == "centrality")
    {
        a <- readRDS (file.path (data_dir, "ny-centrality-vertex.Rds"))
        # 5 centrality values are negative for some reason:
        a$centrality [a$centrality < 0] <- 0
        a$category <- "centrality"
    } else
        a <- readRDS (file.path (data_dir, "osm", "ny-attractors.Rds"))
    # The attractors data contains lots of points outside the bbox of the street
    # network, so have to be reduced to only those within. (Otherwise *ALL*
    # points beyond get aggregated to nearest points, producing anomalously huge
    # values at boundaries.)
    pts <- sf::st_as_sf (a, coords = c ("x", "y"), crs = 4326)
    f <- file.path (tempdir (), "nyc-bb.Rds")
    if (!file.exists (f))
    {
        bb <- osmdata::getbb ("new york city", format_out = "sf_polygon")
        saveRDS (bb, file = f)
    } else
        bb <- readRDS (f)
    suppressMessages (index <- sf::st_contains (bb, pts) [[1]])
    a <- a [index, ]
    # a for ny-attractors has OSM id's, but these need to be re-matched to
    # values in the actual street network
    if (type != "centrality")
        a$id <- v$id [dodgr::match_points_to_graph (v, a [, c ("x", "y")])]
    id <- capacity <- NULL # no visible binding note
    a <- a [a$category == type, ]
    if (type == "transportation")
    {
        suppressWarnings (a$capacity <- as.integer (a$capacity))
        # get median size of parking facilities to replace NA values
        index <- grep ("parking", a$amenity)
        med_parks <- stats::median (a$capacity [index], na.rm = TRUE)
        a$capacity [index] [is.na (a$capacity [index])] <- med_parks
        # all other non-parking transportation give n = 1
        index <- seq (nrow (a)) [which (!seq (nrow (a))) %in% index]
        if (length (index) > 0)
            a$capacity [index, ] <- 1
        a$capacity [is.na (a$capacity)] <- 1
        a <- dplyr::select (a, c ("id", "capacity")) %>%
            dplyr::group_by (id) %>%
            dplyr::summarise (n = sum (capacity))
    } else
    {
        if (type == "centrality")
        {
            names (a) [which (names (a) == "centrality")] <- "n"
            a <- a [which (is.finite (a$n)), ]
            a <- a [which (a$n > 0), ]
            a$n <- a$n / max (a$n)
        } else
        {
            if (type == "entertainment")
                a <- a [a$amenity != "fountain", ]

            a <- dplyr::select (a, c ("id", "x", "y")) %>%
                dplyr::group_by (id) %>%
                dplyr::summarise (n = length (id))
        }

        # then put the coordinates back from the graph vertices
        index <- match (a$id, v$id)
        a$x <- v$x [index]
        a$y <- v$y [index]
    }

    return (a)
}

get_popdens_data <- function (v, data_dir)
{
    # pop values go up to around 3,000, so ones below this value are removed
    dens_limit <- 0.01

    cache_dir <- file.path (data_dir, "calibration")
    f <- file.path (cache_dir, "popdens-layer.Rds")
    if (file.exists (f))
        res <- readRDS (f)
    else
    {
        nodes_new <- readRDS (file.path (data_dir, "worldpop",
                                         "pop-points.Rds"))
        layer_name <- names (nodes_new) [grep ("ppp_", names (nodes_new))]
        nodes_new$id <- v$id [dodgr::match_points_to_graph (v, nodes_new)]
        index <- which (!(nodes_new [[layer_name]] < dens_limit |
                          is.na (nodes_new [[layer_name]])))
        res <- nodes_new [index, ]
        saveRDS (res, file = f)
    }
    return (res)
}

