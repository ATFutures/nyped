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
#' @param cache If `TRUE`, layers are cached in a sub-directory of `data_dir`
#' for later reloading.
#' @export
optim_layer1 <- function (net, from = "subway", to = "disperse", flowvars = NULL,
                          data_dir, cache = TRUE)
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
        ss <- fit_one_ks (net, from, to, p, dp, s, k, ks, flowvars, data_dir,
                          kvals, fitk = TRUE, cache = cache)
        k <- ss$kmin
        message (cli::col_green (cli::symbol$star), " ", prfx [i],
                 " k value = ", k, "; r2 = ", signif (ss$r2, 3))
    }

    star <- cli::symbol$star
    label <- c (paste (star, from, " -> ", to, star),
                paste ("k = ", ss$kmin),
                paste (expression (R^2), " = ", signif (ss$r2, 3)))
    message (cli::boxx (
                        cli::col_white (label),
                        border_style="round",
                        padding = 1,
                        float = "center",
                        border_col = "tomato3",
                        background_col = "deepskyblue"
                        ))
    message (cli::rule (center = "FINISHED", line = 2, col = "green"), "\n")

    return (ss [3:4]) # (k, r2)
}

#' optim_layer2
#'
#' Optimise `k` and `k_scale` parameters to optimally fit flow layer to observed
#' pedestrian densities. Note that optimisation can't be implemented directly
#' because results are too noisy, so a series of custom ranges is scanned, and
#' optimal values found from loess fits.
#'
#' @param k Width of exponential decay (in m) for spatial interaction models
#' @inheritParams optim_layer1
#' @export
optim_layer2 <- function (net, from = "subway", to = "disperse", k = NULL,
                          flowvars = NULL, data_dir)
{
    if (is.null (k))
        stop ("Initial value of 'k' must be provided, as returned from ",
              "'optim_layer1'")

    p <- ped_osm_id (data_dir = data_dir, net = net, quiet = TRUE)
    s <- subway_osm_id (data_dir = data_dir, net = net, quiet = TRUE)
    net <- dodgr::dodgr_contract_graph (net, verts = unique (c (p$id, s$id)))
    dp <- dodgr::dodgr_dists (net, from = p$id)

    txt <- paste0 ("Optimising 2-parameter model fit from ", from, " to ", to)
    message (cli::rule (center = txt, line = 2, col = "green"))

    loop1 <- function (net, from, to, d, dp, s, k, ks, data_dir, flowvars,
                       kvals, ksvals, prfx)
    {
        # Fit ks:
        message (cli::col_cyan (cli::symbol$star), " ", prfx, " ks search [",
                 paste (range (ksvals), collapse = " -> "), "]")
        ss <- fit_one_ks (net, from, to, p, dp, s, k, ks, flowvars,
                          data_dir, ksvals, fitk = FALSE)
        ks_old <- ks
        ks <- ss$kmin
        message (cli::col_green (cli::symbol$star), " ", prfx,
                 " ks value = ", ks)

        # Then re-fit k:
        k_old <- k
        if (ks != ks_old)
        {
            kvals <- k + kvals
            message (cli::col_cyan (cli::symbol$star), " ", prfx, " k search [",
                     paste (range (kvals), collapse = " -> "), "]")
            ss <- fit_one_ks (net, from, to, p, dp, s, k, ks, flowvars,
                              data_dir, kvals, fitk = TRUE)

            niters2 <- 1
            while (ss$kmin == min (ss$k))
            {
                kvals <- kvals - diff (range (kvals))
                kvals <- kvals [kvals > 0]
                message (cli::col_cyan (cli::symbol$star), " Loop (", niters,
                         LETTERS [niters2], ") k search [",
                         paste0 (range (kvals), collapse = " -> "), "]")
                ss <- fit_one_ks (net, from, to, p, dp, s, k, ks, flowvars,
                                  data_dir, kvals, fitk = TRUE)
                k <- ss$kmin
                message (cli::col_green (cli::symbol$star), " Loop (", niters,
                         LETTERS [niters2], ") k = ", k)
                niters2 <- niters2 + 1
                if (niters2 > 10)
                    break
            }
            k <- ss$kmin
            message (cli::col_green (cli::symbol$star), " ", prfx, " k value = ", k)
        }

        return (list (k = k, k_old = k_old, ks = ks, ks_old = ks_old, ss = ss))
    }

    # Initial loop:
    ks <- 0
    res <- loop1 (net, from, to, d, dp, s, k, ks, data_dir, flowvars,
                  kvals = -2:8 * 100, ksvals = -5:10 / 5, "Initial")
    k_old <- k
    ks_old <- ks
    k <- res$k
    ks <- res$ks

    niters <- 1
    while (abs (k - k_old) > 0 & abs (ks - ks_old) > 0)
    {
        kvals <- k + -5:5 * 10
        ksvals <- ks + -5:5 / 10
        res <- loop1 (net, from, to, do, dp, s, k, ks, data_dir, flowvars,
                      kvals = kvals, ksvals = ksvals,
                      paste0 ("Loop [", niters, "]"))
        niters <- niters + 1
        k_old <- k
        ks_old <- ks
        k <- res$k
        ks <- res$ks

        if (niters > 10)
        {
            message ("failed to converge after ", niters, " iterations")
            break
        }
    }

    star <- cli::symbol$star
    label <- c (paste (star, from, " -> ", to, star),
                paste ("k = ", k),
                paste ("ks = ", ks),
                paste (expression (R^2), " = ", signif (res$ss$r2, 3)))
    message (cli::boxx (
                        cli::col_white (label),
                        border_style="round",
                        padding = 1,
                        float = "center",
                        border_col = "tomato3",
                        background_col = "deepskyblue"
                        ))
    message (cli::rule (center = "FINISHED", line = 2, col = "green"), "\n")

    c (k = k, ks = ks, r2 = res$ss$r2)
}

fit_one_ks <- function (net, from, to, p, dp, s, k, ks, flowvars, data_dir,
                        kvals, fitk = TRUE, cache = TRUE, plot = FALSE)
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
    pb <- utils::txtProgressBar (style = 3)
    if (to == "disperse")
    {
        for (i in seq (x)) {
            temp <- disperse_one_layer (net, from, fr_dat, ki [i], ksi [i], p, dp,
                                        flowvars, data_dir, cache = cache)
            r2 [i] <- temp$stats [3]
            ss [i] <- temp$stats [4]
            utils::setTxtProgressBar (pb, i / length (x))
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
            temp <- aggregate_one_layer (net, from, to, fr_dat, to_dat,
                                         ki [i], ksi [i], p, dp, dmat,
                                         flowvars, data_dir, cache = cache)
            r2 [i] <- temp$stats [3]
            ss [i] <- temp$stats [4]
            utils::setTxtProgressBar (pb, i / length (x))
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
        graphics::lines (x, fit, col = "red", lwd = 2)
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
            graphics::lines (x, fit, col = "red", lwd = 2, lty = 2)
    }
    i <- which.min (fit)
    if (plot)
        graphics::points (x [i], ss [i], pch = 19, col = "red", cex = 2)

    return (list (k = x, ss = ss, kmin = x [i], r2 = r2 [i]))
}

#' calc_layer
#' Calculate a single layer from specified origin and destination categories,
#' using specific values of 'k' and 'ks'.
#'
#' @inheritParams optim_layer2
#' @param k_scale Scale `k` to size of origins (`s`), so
#' `k = k ^ (1 + s / smax)`.
#' @export
calc_layer <- function (net, from = "subway", to = "disperse", k, k_scale, data_dir)
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
                                    flowvars = NULL, data_dir, cache = TRUE)
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

        temp <- aggregate_one_layer (net, from, to, fr_dat, to_dat,
                                     k, k_scale, p, dp, dmat,
                                     flowvars = NULL, data_dir, cache = TRUE)
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
                                data_dir, cache = TRUE)
{
    to <- "disperse"
    f <- get_file_name (data_dir, from, to, k, ks)
    if (file.exists (f) & cache)
    {
        net_f <- readRDS (f)
    } else
    {
        kvals <- k ^ (1 + ks * fr_dat$n / max (fr_dat$n))
        net_f <- dodgr::dodgr_flows_disperse (net, from = fr_dat$id,
                                              dens = fr_dat$n, k = kvals)
        if (cache)
            cache_layer (net_f, data_dir, from, to, k, ks)
    }
    flows <- flow_to_ped_pts (net_f, p, dp, get_nearest = TRUE)
    flowvars <- rbind (flowvars, flows)
    p$flow <- flows

    mod <- summary (stats::lm (p$week ~ flows))
    list (stats = c (k = k,
                     k_scale = ks,
                     r2 = mod$adj.r.squared,
                     ss = sum (mod$residuals ^ 2) /
                         length (mod$residuals) / 1e6),
          p = p)
}

aggregate_one_layer <- function (net, from, to, fr_dat, to_dat, k, ks, p, dp,
                                 dmat, flowvars, data_dir, cache = TRUE)
{
    f <- get_file_name (data_dir, from, to, k, ks)
    if (file.exists (f) & cache)
    {
        net_f <- readRDS (f)
    } else
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
        if (cache)
            cache_layer (net_f, data_dir, from, to, k, ks)
    }
    flows <- flow_to_ped_pts (net_f, p, dp, get_nearest = TRUE)
    flowvars <- cbind (flowvars, flows)
    p$flow <- flows

    mod <- summary (stats::lm (p$week ~ flowvars))
    list (stats = c (k = k,
                     k_scale = ks,
                     r2 = mod$adj.r.squared,
                     ss = sum (mod$residuals ^ 2) /
                         length (mod$residuals) / 1e6),
          p = p)
}
