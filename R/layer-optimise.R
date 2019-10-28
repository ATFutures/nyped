#' optim_layer1
#'
#' Optimise flow layer to observed pedestrian densities according to single `k`
#' parameter. Note that optimisation can't be implemented directly because
#' results are too noisy, so a series of custom ranges is scanned, and optimal
#' values found from loess fits.
#'
#' @inheritParams ny_layer
#' @export
optim_layer1 <- function (net, from = "subway", to = "disperse", data_dir)
{
    p <- ped_osm_id (data_dir = data_dir, net = net, quiet = TRUE)
    s <- subway_osm_id (data_dir = data_dir, net = net, quiet = TRUE)
    net <- dodgr::dodgr_contract_graph (net, verts = unique (c (p$id, s$id)))
    dp <- dodgr::dodgr_dists (net, from = p$id)

    txt <- paste0 ("Optimising model fit from ", from, " to ", to)
    message (cli::rule (center = txt, line = 2, col = "green"))

    kscale <- list (1:15 * 200, -5:5 * 100, -5:5 * 20, -5:5 * 10)
    prfx <- c ("Initial", "Second", "Third", "Final")
    ks <- 0.0
    k <- 0
    for (i in seq (kscale))
    {
        kvals <- k + kscale [[i]]
        kvals <- kvals [which (kvals > 0)]
        message (cli::col_cyan (cli::symbol$star), " ", prfx [i], " k values [", 
                 paste0 (range (kvals), collapse = " -> "), "]")
        ss <- fit_one_ks (net, from, to, p, dp, s, k, ks, data_dir, kvals,
                          fitk = TRUE)
        k <- ss$kmin
        message (cli::col_green (cli::symbol$star), " ", prfx [i],
                 " k value = ", k, "; r2 = ", ss$r2)
    }

    message (cli::rule (center = "FINISHED", line = 2, col = "green"))

    return (ss [3:4]) # (ss, r2)
}

#' optim_layer2
#'
#' Optimise `k` and `k_scale` parameters to optimally fit flow layer to observed
#' pedestrian densities. Note that optimisation can't be implemented directly
#' because results are too noisy, so a series of custom ranges is scanned, and
#' optimal values found from loess fits.
#'
#' @inheritParams ny_layer
#' @export
optim_layer2 <- function (net, from = "subway", to = "disperse", k = NULL,
                          data_dir)
{
    if (is.null (k))
        stop ("Initial value of 'k' must be provided, as returned from ",
              "'optim_layer1'")
    p <- ped_osm_id (data_dir = data_dir, net = net, quiet = TRUE)
    s <- subway_osm_id (data_dir = data_dir, net = net, quiet = TRUE)
    net <- dodgr::dodgr_contract_graph (net, verts = unique (c (p$id, s$id)))
    dp <- dodgr::dodgr_dists (net, from = p$id)

    txt <- paste0 ("Optimising model fit from ", from, " to ", to)
    message (cli::rule (center = txt, line = 2, col = "green"))

    # initial k values 100:3000
    message (cli::col_cyan (cli::symbol$star),
             " Initial k values [100 -> 3000]")
    ks <- 0.0
    kvals <- 1:15 * 200
    ss <- fit_one_ks (net, from, to, p, dp, s, k, ks, data_dir, kvals,
                      fitk = TRUE)
    k <- k_old <- ss$kmin
    message (cli::col_green (cli::symbol$star), " Initial k value = ", k)

    # initial ks values -1:2
    message (cli::col_cyan (cli::symbol$star), " Initial ks values [-1 -> 2]")
    ksvals <- (-5:10) / 5
    ss <- fit_one_ks (net, from, to, p, dp, s, k, ks, data_dir, ksvals,
                      fitk = FALSE)
    ks <- ks_old <- ss$kmin # -0.2
    message (cli::col_green (cli::symbol$star), " Initial ks value = ", ks)

    # second, slightly finer k-values
    if (ks > 0)
    {
        kvals <- 1:30 * 100
        kvals <- kvals [kvals <= k]
    } else
        kvals <- k + -2:8 * 100
    message (cli::col_cyan (cli::symbol$star), " Second k values [",
             min (kvals), " -> ", max (kvals), "]")
    kvals <- kvals [kvals > 0]
    ss <- fit_one_ks (net, from, to, p, dp, s, k, ks, data_dir, kvals,
                      fitk = TRUE)
    k_old <- k
    k <- ss$kmin
    message (cli::col_green (cli::symbol$star), " Second k value = ", k)

    # second ks-values
    ksvals <- ks + (-5:5) / 10
    message (cli::col_cyan (cli::symbol$star), " Second ks values [",
             min (ksvals), " -> ", max (ksvals), "]")
    ss <- fit_one_ks (net, from, to, p, dp, s, k, ks, data_dir, ksvals,
                      fitk = FALSE)
    ks_old <- ks
    ks <- ss$kmin # -0.2
    message (cli::col_green (cli::symbol$star), " Second ks value = ", ks)

    # 3rd round of k-values
    if (ks > 0)
    {
        kvals <- 1:30 * 100
        kvals <- kvals [kvals <= k]
    } else
        kvals <- k + -2:8 * 100
    kvals <- kvals [kvals > 0]
    message (cli::col_cyan (cli::symbol$star), " Third k values [",
             min (kvals), " -> ", max (kvals), "]")
    ss <- fit_one_ks (net, from, to, p, dp, s, k, ks, data_dir, kvals,
                      fitk = TRUE)
    k_old <- k
    k <- ss$kmin
    message (cli::col_green (cli::symbol$star), " Third k value = ", k)

    #if (ks == ks_old) k_old <- k

    # Then loop over both until convergence
    niters <- 1
    while (abs (k - k_old) > 0 & abs (ks - ks_old) > 0)
    {
        k_old <- k
        ks_old <- ks

        kvals <- k + (-5:5) * 10
        kvals <- kvals [kvals > 0]
        message (cli::col_cyan (cli::symbol$star), " Loop (", niters,
                 ") k values [", min (kvals), " -> ", max (kvals), "]")
        ss <- fit_one_ks (net, from, to, p, dp, s, k, ks, data_dir,
                     kvals, fitk = TRUE)
        if (sd (ss$ss, na.rm = TRUE) > 0.1)
            k <- ss$kmin
        message (cli::col_green (cli::symbol$star), " Loop (", niters,
                 ") k = ", k)

        niters2 <- 1
        while (k == min (ss$k))
        {
            kvals <- kvals - 100
            kvals <- kvals [kvals > 0]
            message (cli::col_cyan (cli::symbol$star), " Loop (", niters,
                     LETTERS [niters2], ") k values [", min (kvals), " -> ",
                     max (kvals), "]")
            ss <- fit_one_ks (net, from, to, p, dp, s, k, ks, data_dir,
                         kvals, fitk = TRUE)
            k <- ss$kmin
            message (cli::col_green (cli::symbol$star), " Loop (", niters,
                     LETTERS [niters2], ") k = ", k)
            niters2 <- niters2 + 1
            if (niters2 > 10)
                break
        }
        if (sd (ss$ss, na.rm = TRUE) > 0.1)
            k <- ss$kmin # 200

        ksvals <- ks + (-5:5) / 10
        message (cli::col_cyan (cli::symbol$star), " Loop (", niters,
                 ") ks values [", min (ksvals), " -> ", max (ksvals), "]")
        ss <- fit_one_ks (net, from, to, p, dp, s, k, ks, data_dir,
                     ksvals, fitk = FALSE)
        if (sd (ss$ss, na.rm = TRUE) > 0.1)
            ks <- ss$kmin
        message (cli::col_green (cli::symbol$star), " Loop (", niters,
                 ") ks = ", ks)

        niters <- niters + 1
        if (niters > 10)
            break
    }
    message (cli::rule (center = "FINISHED", line = 2, col = "green"))

    c (k = k, ks = ks)
}

fit_one_ks <- function (net, from, to, p, dp, s, k, ks, data_dir, kvals,
                        fitk = TRUE, plot = FALSE)
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

    ss <- r2 <- rep (NA, length (x))
    i <- NULL # suppress no visible binding note
    pb <- txtProgressBar (style = 3)
    if (to == "disperse")
    {
        for (i in seq (x)) {
            temp <- disperse_one_layer (net, fr_dat, ki [i], ksi [i], p, dp)
            r2 [i] <- temp$stats [3]
            ss [i] <- temp$stats [4]
            setTxtProgressBar (pb, i / length (x))
            if (i > 2 & all (diff (ss [!is.na (ss)]) > 0))
                break # lowest SS is first k
        }
    } else
    {
        if (to == "residential")
            to_dat <- get_res_dat (v, data_dir)
        else if (to == "subway")
            to_dat <- get_subway_dat (s)
        else
            to_dat <- get_attractor_layer (data_dir, v, type = to)

        dmat <- dodgr::dodgr_distances (net, from = fr_dat$id, to = to_dat$id)
        dmat [is.na (dmat)] <- max (dmat, na.rm = TRUE)

        for (i in seq (x)) {
            temp <- aggregate_one_layer (net, fr_dat, to_dat, ki [i],
                                         ksi [i], p, dp, dmat)
            r2 [i] <- temp$stats [3]
            ss [i] <- temp$stats [4]
            setTxtProgressBar (pb, i / length (x))
            if (i > 2 & all (diff (ss [!is.na (ss)]) > 0))
                break # lowest SS is first k
        }
    }
    close (pb)

    if (any (is.na (ss))) # if SS progressively increases with first k-values
        return (list (k = x, ss = ss, kmin = x [1]))

    mod <- stats::loess (ss ~ x, span = lspan)
    fit <- stats::predict (mod, newdata = data.frame (x2 = x))
    if (plot)
    {
        plot (x, ss, pch = 2, col = "orange")
        lines (x, fit, col = "red", lwd = 2)
    }
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
        if (plot)
            lines (x, fit, col = "red", lwd = 2, lty = 2)
    }
    i <- which.min (fit)
    if (plot)
        points (x [i], ss [i], pch = 19, col = "red", cex = 2)

    return (list (k = x, ss = ss, kmin = x [i], r2 = r2 [i]))
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

disperse_one_layer <- function (net, fr_dat, k, ks, p, dp)
{
    kvals <- k ^ (1 + ks * fr_dat$n / max (fr_dat$n))
    net_f <- NULL
    while (is.null (net_f))
    {
        # dispersal sometimes errors in parallel aggregation
        net_f <- tryCatch ({
            dodgr::dodgr_flows_disperse (net, from = fr_dat$id,
                                         dens = fr_dat$n, k = kvals) },
            error = function (e) { NULL },
            warning = function (w) { NULL })
    }
    #net_f <- dodgr::dodgr_flows_disperse (net, from = fr_dat$id,
    #                                      dens = fr_dat$n, k = kvals)
    flows <- flow_to_ped_pts (net_f, p, dp, get_nearest = TRUE)
    p$flows <- flows

    mod <- summary (stats::lm (p$week ~ p$flows))
    list (stats = c (k = k,
                     k_scale = ks,
                     r2 = mod$adj.r.squared,
                     ss = sum (mod$residuals ^ 2) /
                         length (mod$residuals) / 1e6),
          p = p)
}

aggregate_one_layer <- function (net, fr_dat, to_dat, k, ks, p, dp, dmat)
{
    nfr <- nrow (fr_dat)
    nto <- nrow (to_dat)

    kvals <- k ^ (1 + ks * fr_dat$n / max (fr_dat$n))
    kmat <- matrix (kvals, nrow = nfr, ncol = nto)

    frmat <- matrix (fr_dat$n, nrow = nfr, ncol = nto)

    emat <- frmat * exp (-dmat / kmat)
    # the first constraint, to unit sums over all destinations (columns)
    # from each origin (row)
    cmat <- t (matrix (colSums (emat), nrow = nto, ncol = nfr))
    emat <- emat / cmat
    # the second constraint, so each origin (row) allocates fr$n to all
    # destinations (columns)
    fmat <- frmat * emat / nto

    net_f <- NULL
    while (is.null (net_f))
    {
        # aggregation sometimes errors in parallel aggregation
        net_f <- tryCatch ({
            dodgr::dodgr_flows_aggregate (net, from = fr_dat$id,
                                          to = to_dat$id, flows = fmat) },
            error = function (e) { NULL },
            warning = function (w) { NULL })
    }
    #net_f <- dodgr::dodgr_flows_aggregate (net, from = fr_dat$id,
    #                                       to = to_dat$id, flows = fmat)
    flows <- flow_to_ped_pts (net_f, p, dp, get_nearest = TRUE)
    p$flows <- flows

    mod <- summary (stats::lm (p$week ~ p$flows))
    list (stats = c (k = k,
                     k_scale = ks,
                     r2 = mod$adj.r.squared,
                     ss = sum (mod$residuals ^ 2) /
                         length (mod$residuals) / 1e6),
          p = p)
}
