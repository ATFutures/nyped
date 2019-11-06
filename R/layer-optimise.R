#' optim_layer1
#'
#' Optimise flow layer to observed pedestrian densities according to single `k`
#' parameter. Note that optimisation can't be implemented directly because
#' results are too noisy, so a series of custom ranges is scanned, and optimal
#' values found from loess fits.
#'
#' @param net Weighted street network; loaded from `data_dir` if not provided
#' @param from Category of origins for pedestrian flows; one of "subway" or
#' "residential"
#' @param to Category of destinations for pedestrian flows; one of
#' "residential", "education", "entertainment", "healthcare", "sustenance",
#' "transportation", or "disperse" for a general dispersal model.
#' @param flowvars A `data.frame` of flows from previously calculated layers. If
#' provided, these are included in the model, and the target layer is optimised
#' as part of a multiple linear regression which includes these variables. Must
#' have same number of rows as `net`.
#' @param data_dir The directory in which data are to be, or have previously
#' been, downloaded.
#' @export
optim_layer1 <- function (net, from = "subway", to = "disperse",
                          flowvars = NULL, data_dir)
{
    p <- ped_osm_id (data_dir = data_dir, net = net, quiet = TRUE)
    s <- subway_osm_id (data_dir = data_dir, net = net, quiet = TRUE)
    net <- dodgr::dodgr_contract_graph (net, verts = unique (c (p$id, s$id)))
    dp <- dodgr::dodgr_dists (net, from = p$id)

    txt <- paste0 ("Optimising model fit from ", from, " to ", to)
    message (cli::rule (center = txt, line = 2, col = "green"))

    #k_scale <- list (1:30 * 100, -20:20 * 10)
    #ks <- 0.0
    #k <- 0
    #k_old <- 9999
    #niters <- 1
    #index <- 1 # index in to k/ks_scale
    #while (k_old != k)
    #{
    #    k_old <- k

    #    kvals <- k + k_scale [[index]]
    #    kvals <- kvals [which (kvals > 0)]
    #    message (cli::col_cyan (cli::symbol$star), " Iteration#", niters,
    #             " : k values [",
    #             paste0 (range (kvals), collapse = " -> "), "]")
    #    ss <- fit_one_ks (net, from, to, p, dp, s, k, ks, flowvars, data_dir,
    #                      kvals, fitk = TRUE)
    #    k <- ss$k
    #    message (cli::col_green (cli::symbol$star), " Iteration#", niters,
    #             " : k value = ", k, "; r2 = ", signif (ss$r2, 4))

    #    niters2 <- 1
    #    while (k == min (kvals))
    #    {
    #        kvals <- kvals - diff (range (kvals))
    #        message (cli::col_cyan (cli::symbol$star), " Iteration#", niters,
    #                 " - Loop (", niters2, LETTERS [niters2], ") k search [",
    #                 paste0 (range (kvals), collapse = " -> "), "]")
    #        ss <- fit_one_ks (net, from, to, p, dp, s, k, ks, flowvars,
    #                          data_dir, kvals, fitk = TRUE)
    #        k <- ss$k
    #        message (cli::col_green (cli::symbol$star), " Iteration#", niters,
    #                 " - Loop (", niters2, LETTERS [niters2], ") k = ", k)
    #        niters2 <- niters2 + 1
    #        if (niters2 > 10)
    #            break
    #    }

    #    index <- 2
    #}
    kvals <- 1:30 * 100
    ks <- 0.0
    k <- 0
    ss <- fit_one_ks (net, from, to, p, dp, s, k, ks, flowvars, data_dir,
                      kvals, fitk = TRUE)

    star <- cli::symbol$star
    label <- c (paste (star, from, " -> ", to, star),
                paste ("k = ", ss$stats$k),
                paste (expression (R^2), " = ", signif (ss$stats$r2, 4)))
    message (cli::boxx (
                        cli::col_black (label),
                        border_style = "round",
                        padding = 1,
                        float = "center",
                        border_col = "tomato3",
                        background_col = "yellow"
                        ))
    message (cli::rule (center = "FINISHED", line = 2, col = "green"), "\n")

    return (ss)
}

#' get_layer
#'
#' Append network with flow columns between nominated places
#' @inheritParams optim_layer1
#' @export
get_layer <- function (net, from = "subway", to = "disperse", data_dir)
{
    p <- ped_osm_id (data_dir = data_dir, net = net, quiet = TRUE)
    s <- subway_osm_id (data_dir = data_dir, net = net, quiet = TRUE)
    net <- dodgr::dodgr_contract_graph (net, verts = unique (c (p$id, s$id)))
    dp <- dodgr::dodgr_dists (net, from = p$id)
    v <- dodgr::dodgr_vertices (net)

    txt <- paste0 (from, " to ", to)
    message (cli::rule (center = txt, line = 2, col = "green"))

    ks <- 0
    k <- 1:30 * 100
    nk <- length (k)


    # get fr_dat with columns of (id, n), where id is matched to v$id
    reverse <- FALSE
    if (from == "centrality")
    {
        net <- reverse_net (net)
        reverse <- TRUE
        fr_dat <- get_attractor_layer (data_dir, v, type = from)
    } else if (from == "residential" & to != "centrality")
    {
        net <- reverse_net (net)
        fr_dat <- get_res_dat (v, data_dir)
        reverse <- TRUE
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

    f <- file.path (data_dir, "calibration",
                    paste0 ("net-", substr (from, 1, 3), "-",
                            substr (to, 1, 3), ".Rds"))

    saveRDS (net_f, file = f)

    return (net_f)
}

#' optim_layer2
#'
#' Optimise flow layer to observed pedestrian densities according to single `k`
#' parameter. Note that optimisation can't be implemented directly because
#' results are too noisy, so a series of custom ranges is scanned, and optimal
#' values found from loess fits.
#'
#' @param net Weighted street network; loaded from `data_dir` if not provided
#' @param from Category of origins for pedestrian flows; one of "subway" or
#' "residential"
#' @param to Category of destinations for pedestrian flows; one of
#' "residential", "education", "entertainment", "healthcare", "sustenance",
#' "transportation", or "disperse" for a general dispersal model.
#' @param flowvars A `data.frame` of flows from previously calculated layers. If
#' provided, these are included in the model, and the target layer is optimised
#' as part of a multiple linear regression which includes these variables. Must
#' have same number of rows as `net`.
#' @param data_dir The directory in which data are to be, or have previously
#' been, downloaded.
#' @export
optim_layer2 <- function (net, from = "subway", to = "disperse",
                          flowvars = NULL, data_dir)
{
    p <- ped_osm_id (data_dir = data_dir, net = net, quiet = TRUE)
    s <- subway_osm_id (data_dir = data_dir, net = net, quiet = TRUE)
    net <- dodgr::dodgr_contract_graph (net, verts = unique (c (p$id, s$id)))
    dp <- dodgr::dodgr_dists (net, from = p$id)

    txt <- paste0 ("Optimising model fit from ", from, " to ", to)
    message (cli::rule (center = txt, line = 2, col = "green"))

    k_scale <- list (1:30 * 100, -20:20 * 10)
    ks_scale <- list (-10:20 / 10, -10:10 / 10)
    ks <- 0.0
    k <- 0
    k_old <- ks_old <- 9999
    niters <- 1
    index <- 1 # index in to k/ks_scale
    while (k_old != k & ks_old != ks)
    {
        k_old <- k
        ks_old <- ks

        kvals <- k + k_scale [[index]]
        kvals <- kvals [which (kvals > 0)]
        message (cli::col_cyan (cli::symbol$star), " Iteration#", niters,
                 " : k values [",
                 paste0 (range (kvals), collapse = " -> "), "]")
        ss <- fit_one_ks (net, from, to, p, dp, s, k, ks, flowvars, data_dir,
                          kvals, fitk = TRUE)
        k <- ss$k
        message (cli::col_green (cli::symbol$star), " Iteration#", niters,
                 " : k value = ", k, "; r2 = ", signif (ss$r2, 4))

        niters2 <- 1
        while (k == min (kvals))
        {
            kvals <- kvals - diff (range (kvals))
            message (cli::col_cyan (cli::symbol$star), " Iteration#", niters,
                     " - Loop (", niters2, LETTERS [niters2], ") k search [",
                     paste0 (range (kvals), collapse = " -> "), "]")
            ss <- fit_one_ks (net, from, to, p, dp, s, k, ks, flowvars,
                              data_dir, kvals, fitk = TRUE)
            k <- ss$k
            message (cli::col_green (cli::symbol$star), " Iteration#", niters,
                     " - Loop (", niters2, LETTERS [niters2], ") k = ", k)
            niters2 <- niters2 + 1
            if (niters2 > 10)
                break
        }

        kvals <- ks + ks_scale [[index]]
        message (cli::col_cyan (cli::symbol$star), " Iteration#", niters,
                 " : ks values [",
                 paste0 (range (kvals), collapse = " -> "), "]")
        ss <- fit_one_ks (net, from, to, p, dp, s, k, ks, flowvars, data_dir,
                          kvals, fitk = FALSE)
        ks <- ss$ks
        message (cli::col_green (cli::symbol$star), " Iteration#", niters,
                 " : ks value = ", ks, "; r2 = ", signif (ss$r2, 4))

        index <- 2
        niters <- niters + 1
        if (niters > 10)
        {
            warning ("Failed to converge after 10 iterations")
            break
        }
    }

    star <- cli::symbol$star
    label <- c (paste (star, from, " -> ", to, star),
                paste ("k = ", ss$k),
                paste ("ks = ", ss$ks),
                paste (expression (R^2), " = ", signif (ss$r2, 4)))
    message (cli::boxx (
                        cli::col_white (label),
                        border_style = "round",
                        padding = 1,
                        float = "center",
                        border_col = "tomato3",
                        background_col = "deepskyblue"
                        ))
    message (cli::rule (center = "FINISHED", line = 2, col = "green"), "\n")

    ss$stats <- NULL
    return (ss)
}


fit_one_ks <- function (net, from, to, p, dp, s, k, ks, flowvars, data_dir,
                        kvals, fitk = TRUE)
{
    lspan <- 0.75 # fixed span of loess fits
    if (fitk)
    {
        ki <- x <- kvals
        ksi <- rep (ks, length (ki))
    } else
    {
        ksi <- x <- kvals
        ki <- rep (k, length (ksi))
    }

    v <- dodgr::dodgr_vertices (net)

    # get fr_dat with columns of (id, n), where id is matched to v$id
    if (from == "residential")
    {
        net <- reverse_net (net)
        fr_dat <- get_res_dat (v, data_dir)
    } else if (from == "subway")
        fr_dat <- get_subway_dat (s)
    else
        fr_dat <- get_attractor_layer (data_dir, v, type = from)

    if (to == "disperse")
    {
        temp <- disperse_one_layer (net, from, fr_dat, ki, ksi, p, dp,
                                    flowvars, data_dir)
    } else
    {
        if (to == "residential")
            to_dat <- get_res_dat (v, data_dir)
        else if (to == "subway")
            to_dat <- get_subway_dat (s)
        else
            to_dat <- get_attractor_layer (data_dir, v, type = to)

        temp <- aggregate_one_layer (net, from, to, fr_dat, to_dat,
                                     ki, ksi, p, dp, flowvars, data_dir)
    }

    return (temp)
}

#' calc_layer
#' Calculate a single layer from specified origin and destination categories,
#' using specific values of 'k' and 'ks'.
#'
#' @inheritParams optim_layer2
#' @param k Width of exponential decay in metres
#' @param k_scale Scale `k` to size of origins (`s`), so
#' `k = k ^ (1 + s / smax)`.
#' @export
calc_layer <- function (net, from = "subway", to = "disperse", k, k_scale,
                        data_dir)
{
    p <- ped_osm_id (data_dir = data_dir, net = net, quiet = TRUE)
    s <- subway_osm_id (data_dir = data_dir, net = net, quiet = TRUE)
    net <- dodgr::dodgr_contract_graph (net, verts = unique (c (p$id, s$id)))
    dp <- dodgr::dodgr_dists (net, from = p$id)
    v <- dodgr::dodgr_vertices (net)

    if (from == "residential")
    {
        net <- reverse_net (net)
        fr_dat <- get_res_dat (v, data_dir)
    } else if (from == "subway")
        fr_dat <- get_subway_dat (s)
    else
        fr_dat <- get_attractor_layer (data_dir, v, type = from)

    if (to == "disperse")
    {
        temp <- disperse_one_layer (net, from, fr_dat, k, k_scale, p, dp,
                                    flowvars = NULL, data_dir)
    } else
    {
        if (to == "residential")
            to_dat <- get_res_dat (v, data_dir)
        else if (to == "subway")
            to_dat <- get_subway_dat (s)
        else
            to_dat <- get_attractor_layer (data_dir, v, type = to)

        temp <- aggregate_one_layer (net, from, to, fr_dat, to_dat,
                                     k, k_scale, p, dp,
                                     flowvars = NULL, data_dir)
    }

    return (temp)
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

disperse_one_layer <- function (net, from, fr_dat, k, ks, p, dp, flowvars,
                                data_dir)
{
    nk <- length (k)
    nvals <- rep (fr_dat$n, length (k))
    kvals <- t (matrix (k ^ (1 + ks * nvals / max (nvals)), nrow = length (k)))

    message (cli::col_cyan (cli::symbol$star), " Disersing layer ... ",
             appendLF = FALSE)
    net_f <- dodgr::dodgr_flows_disperse (net, from = fr_dat$id,
                                          dens = fr_dat$n, k = kvals)
    message ("\r", cli::col_green (cli::symbol$tick), " Disersed layer     ")

    message (cli::col_cyan (cli::symbol$star),
             " Optimising fit to pedestrian data ... ", appendLF = FALSE)
    fit <- opt_fit_to_ped (net_f, p, dp, flowvars)
    message ("\r", cli::col_green (cli::symbol$tick),
             " Optimised fit to pedestrian data : R2 = ",
             formatC (fit$r2, format = "f", digits = 4), " at k = ",
             k [fit$k], "m")
    flow <- fit$flow

    flowvars <- cbind (flowvars, fit$flow)

    return (list (stats = stats, p = cbind (p, flowvars)))
}

aggregate_one_layer <- function (net, from, to, fr_dat, to_dat, k, ks, p, dp,
                                 flowvars, data_dir)
{
    nk <- length (k)
    nvals <- rep (fr_dat$n, length (k))
    kvals <- matrix (k ^ (1 + ks * nvals / max (nvals)), ncol = length (k))

    message (cli::col_cyan (cli::symbol$star), " Aggregating layer ... ",
             appendLF = FALSE)
    net_f <- dodgr::dodgr_flows_si (net, from = fr_dat$id,
                                    to = to_dat$id, k = kvals,
                                    dens_from = fr_dat$n,
                                    dens_to = to_dat$n)
    message ("\r", cli::col_green (cli::symbol$tick), " Aggregated layer     ")

    message (cli::col_cyan (cli::symbol$star),
             " Optimising fit to pedestrian data ... ", appendLF = FALSE)
    fit <- opt_fit_to_ped (net_f, p, dp, flowvars)
    message ("\r", cli::col_green (cli::symbol$tick),
             " Optimised fit to pedestrian data : R2 = ",
             formatC (fit$r2, format = "f", digits = 4), " at k = ",
             k [fit$k], "m")

    flowvars <- cbind (flowvars, fit$flow)

    stats <- list (k = k [fit$k], r2 = fit$r2)

    return (list (stats = stats, flowvars = flowvars))
}

# fit loess smoother to result of aggregate/disperse_one_layer, and return
# parameters at minimum of smoothed fit
loess_fit <- function (x, stats)
{
    lspan <- 0.75
    ss <- stats [, 1]
    r2 <- stats [, 2]
    mod <- stats::loess (ss ~ x, span = lspan)
    fit <- stats::predict (mod)
    sdlim <- 2 * stats::sd (mod$residuals)
    n <- length (which (abs (mod$residuals) > sdlim))
    if (n > 0 & n < 3 & length (x) > 6) # remove extreme values
    {
        index <- which (abs (mod$residuals) <= sdlim)
        x2 <- x [index]
        y <- ss [index]
        mod2 <- tryCatch ({
            stats::loess (y ~ x2, span = lspan)},
            warning = function (w) { NULL },
            error = function (e) { NULL })
        if (!is.null (mod2))
            fit <- stats::predict (mod2, newdata = data.frame (x2 = x))
    }
    i <- which.min (fit)
    c (i = i, k = x [i], ss = ss [i], r2 = r2 [i])
}

opt_fit_to_ped <- function (net_f, p, dp, flowvars = NULL)
{
    fcols <- grep ("flow", names (net_f))
    #flows <- array (NA, dim = c (nrow (p), length (fcols)))
    # convert dp to equivalent matrix of vertex names sorted in order of increasing
    # distance from each p
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
    ss <- rep (NA, length (n))
    ssmin <- .Machine$double.xmax
    res <- n_out <- k_out <- r2_out <- NULL
    pb <- utils::txtProgressBar (style = 3)
    for (i in seq (n))
    {
        temp <- rcpp_match_flow_mats (flowmat, index0 - 1, index1 - 1,
                                     fcols - 1, n [i])
        stats <- apply (temp, 2, function (j) {
                            ivs <- cbind (flowvars, j)
                            mod <- summary (lm (p$week ~ ivs))
                            c (sum (mod$residuals ^ 2) / 1e6,
                               mod$r.squared)  })
        ss <- stats [1, ]
        r2 <- stats [2, ]

        if (min (ss) < ssmin)
        {
            res <- temp
            n_out <- i
            k_out <- which.min (ss)
            ssmin <- min (ss)
            r2_out <- r2 [k_out]
        }
        utils::setTxtProgressBar (pb, i / length (n))
    }
    close (pb)

    return (list (n = n_out,
                  k = k_out,
                  r2 = r2_out,
                  flow = res [, k_out]))
}
