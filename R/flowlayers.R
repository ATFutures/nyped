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

    # get fr_dat with columns of (id, n), where id is matched to v$id
    reverse <- FALSE
    if (from == "centrality")
    {
        net <- reverse_net (net)
        reverse <- TRUE
        from <- to
        to <- "centrality"
        fr_dat <- get_attractor_layer (data_dir, v, type = from)
    } else if (from == "residential" & to != "centrality")
    {
        net <- reverse_net (net)
        reverse <- TRUE
        from <- to
        to <- "residential"
        if (from == "subway")
            fr_dat <- get_subway_dat (s)
        else
            fr_dat <- get_attractor_layer (data_dir, v, type = from)
    } else if (from == "subway")
        fr_dat <- get_subway_dat (s)
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

    if (reverse)
        net_f <- reverse_net (net_f)

    return (net_f)
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
                n = s$count2018,
                stringsAsFactors = FALSE)
}

#' fit_flows_to_ped
#'
#' Fit one network with multiple flow columns (from multiple k-values) to
#' pedestrian counts, by aggregating each flow column across a variable number
#' of edges ('n') nearest to each pedestrian count station.
#'
#' @param net_f A network with flow columns, obtained from `get_layer`
#' @inheritParams get_layer
#' @return A list containing vectors of 'k' and 'n' values, and a matrix of
#' 30x20 = 600 columns, one for each combination of 30 'k'- and 20 'n'-values.
#'
#' @export
fit_flows_to_ped <- function (net_f, data_dir)
{
    p <- ped_osm_id (data_dir = data_dir, net = net_f, quiet = TRUE)
    dp <- dodgr::dodgr_dists (net_f, from = p$id)

    fcols <- grep ("flow", names (net_f))
    # convert dp to equivalent matrix of vertex names sorted in order of
    # increasing distance from each p
    sorted_verts <- function (dmat)
    {
        res <- array (NA, dim = dim (dmat))
        for (i in seq (nrow (dmat)))
        {
            res [i, ] <- colnames (dmat) [order (dmat [i, ])]
        }
        return (res)
    }
    dp_verts <- sorted_verts (dp)
    # then convert that to equivalent indices into both .vx0 and .vx1 of net_f
    index0 <- t (apply (dp_verts, 1, function (i) match (i, net_f$.vx0)))
    index1 <- t (apply (dp_verts, 1, function (i) match (i, net_f$.vx1)))

    flowmat <- as.matrix (net_f [, fcols])
    n <- 1:20
    pb <- utils::txtProgressBar (style = 3)
    temp <- list ()
    for (i in seq (n))
    {
        temp [[i]] <- rcpp_match_flow_mats (flowmat, index0 - 1, index1 - 1,
                                            fcols - 1, n [i])
        utils::setTxtProgressBar (pb, i / length (n))
    }
    close (pb)

    temp <- do.call (cbind, temp)

    k <- 1:30 * 100
    nk <- length (k) # 30
    n <- 1:20
    k <- rep (k, length (n))
    n <- rep (n, each = nk)

    list (k = k, n = n, flows = temp)
}

#' all_flows_to_ped
#'
#' Batch-convert all flow layers to equivalent values matched to pedestrian
#' counting stations via the \link{fit_flows_to_ped} function.
#'
#' @inheritParams fit_flows_to_ped
#' @export
all_flows_to_ped <- function (data_dir)
{
    categories <- c ("subway", "centrality", "residential", "transportation",
                     "sustenance", "entertainment", "education", "healthcare")
    index <- t (utils::combn (length (categories), 2))
    ft <- data.frame (from = c (categories [index [, 1]],
                                categories [index [, 2]]),
                      to = c (categories [index [, 2]],
                              categories [index [, 1]]),
                      stringsAsFactors = FALSE)
    ft <- rbind (ft, data.frame (from = categories,
                                 to = "disperse",
                                 stringsAsFactors = FALSE))
    for (i in seq (nrow (ft)))
    {
        fi <- ft$from [i]
        ti <- ft$to [i]
        f <- file.path (data_dir, "calibration",
                        paste0 ("net-", substring (fi, 1, 3), "-",
                                substring (ti, 1, 3), ".Rds"))
        if (file.exists (f))
        {
            fout <- file.path (data_dir, "calibration",
                               paste0 ("ped-flows-", substring (fi, 1, 3), "-",
                                       substring (ti, 1, 3), ".Rds"))
            if (!file.exists (fout))
            {
                message (cli::col_cyan (cli::symbol$star), " ", fi, " -> ", ti, ":")
                net_f <- readRDS (f)
                flows <- fit_flows_to_ped (net_f, data_dir)
                saveRDS (flows, file = fout)
                message ("\r", cli::col_green (cli::symbol$tick),
                         " ", fi, " -> ", ti, "\n")
            }
        }
    }
}

#' build_ped_model
#'
#' Build final pedestrian model
#'
#' @param dat Output of previous run of `build_ped_model`
#' @param sig Desired level of statistical significance
#' @inheritParams get_layer
#' @export
build_ped_model <- function (data_dir, dat = NULL, sig = 0.01)
{
    # dummy-load a network graph to extract pedestrian counts:
    files <- list.files (file.path (data_dir, "calibration"),
                         pattern = "net-", full.names = TRUE)
    net_f <- readRDS (files [1]) # doesn't matter which
    p <- ped_osm_id (data_dir = data_dir, net = net_f, quiet = TRUE)
    yvar <- p$weekday

    # Then loop over the flows as mapped on to those pedestrian counts
    files <- list.files (file.path (data_dir, "calibration"),
                         pattern = "ped-flows-", full.names = TRUE)
    ssmin <- .Machine$double.xmax
    r2 <- from <- to <- flowvars <- n <- k <- NULL
    if (!is.null (dat))
    {
        r2 <- dat$r2
        from <- dat$from
        to <- dat$to
        flowvars <- dat$flowvars
        # and remove those pairs from list of files to be added
        ft <- paste0 (dat$from, "-", dat$to)
        for (f in ft)
            files <- files [!grepl (f, files)]
    }
    ssmin <- .Machine$double.xmax
    pb <- utils::txtProgressBar (style = 3)
    to_out <- r2_out <- from_out <- k_out <- n_out <- NULL
    for (f in seq (files))
    {
        dat <- readRDS (files [f])
        stats <- apply (dat$flows, 2, function (j) {
                            ivs <- cbind (flowvars, j)
                            mod <- summary (stats::lm (yvar ~ ivs))
                            c (sum (mod$residuals ^ 2) / 1e6,
                               mod$r.squared)  })
        stats <- t (stats) # then 2 columns of (ss, r2)
        
        if (min (stats [, 1]) < ssmin)
        {
            i <- which.min (stats [, 1])
            ssmin <- stats [i, 1]
            r2_out <- stats [i, 2]
            n_out <- stats$n [i]
            k_out <- stats$k [i]
            ft <- strsplit (strsplit (files [f], "ped-flows-") [[1]] [2],
                            ".Rds") [[1]]
            from_out <- strsplit (ft, "-") [[1]] [1]
            to_out <- strsplit (ft, "-") [[1]] [2]
            flows <- dat$flows [, which.min (stats [, 1])]
        }
        utils::setTxtProgressBar (pb, f / length (files))
    }
    close (pb)

    mod <- summary (stats::lm (yvar ~ cbind (flowvars, flows)))
    pvals <- mod$coefficients [2:nrow (mod$coefficients), 4]
    is_significant <- FALSE
    if (utils::tail (pvals, 1) < sig)
    {
        is_significant <- TRUE
        flowvars <- cbind (flowvars, flows)
        r2 <- c (r2, r2_out)
        from <- c (from, from_out)
        to <- c (to, to_out)
        n <- c (n, n_out)
        k <- c (k, k_out)
        colnames (flowvars) [ncol (flowvars)] <- paste0 (from_out, "-", to_out)

        # Then remove any variables rendered non-significant during that step
        if (any (pvals > sig))
        {
            index <- which (pvals > sig)
            s <- ifelse (length (index) == 1, "", "s")
            message (cli::col_red (cli::symbol$warning), " removed ",
                     length (index), " layer", s, ": ", cli::col_blue (
                     paste (colnames (flowvars) [index], collapse = " + ")))
        }
        index <- which (pvals < sig)
        from <- from [index]
        to <- to [index]
        r2 <- r2 [index]
        n <- n [index]
        k <- k [index]
        flowvars <- flowvars [, index, drop = FALSE]
        # Then re-calculate model and update r2 value
        mod <- summary (stats::lm (yvar ~ flowvars))
        r2out <- r2 [length (r2)] <- mod$r.squared

        message (cli::col_green (cli::symbol$tick),
                 " R2 = ", signif (r2_out, 4), " for ", cli::col_red (from_out),
                 " to ", cli::col_red (to_out))
    } else
        message (cli::col_red (cli::symbol$cross),
                 " No flows make any further significant contributions to model")


    return (list (r2 = r2,
                  from = from,
                  to = to,
                  n = n,
                  k = k,
                  is_significant = is_significant,
                  ped_counts = yvar,
                  flowvars = flowvars))
}