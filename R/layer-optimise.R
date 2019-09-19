#' optimise_layer
#'
#' Optimise `k` and `k_scale` parameters to optimally fit flow layer to observed
#' pedestrian densities
#' @inheritParams ny_layer
#' @export
optimise_layer <- function (net, from = "subway", to = "disperse", data_dir)
{

    p <- ped_osm_id (data_dir = data_dir, net = net, quiet = TRUE)
    s <- subway_osm_id (data_dir = data_dir, net = net, quiet = TRUE)
    net <- dodgr::dodgr_contract_graph (net,
                                        verts = unique (c (p$id, s$id)))
    v <- dodgr::dodgr_vertices (net)
    dp <- dodgr::dodgr_dists (net, from = p$id)

    txt <- paste0 ("Optimising model fit from ", from, " to ", to)
    message (cli::rule (center = txt, line = 2, col = "green"))

    f_k <- function (k = 700, k_scale = 0.0, from = from, to = to,
                     data_dir = data_dir)
    {
        res <- fit_one_layer (net, from = from, to = to, k = k,
                              k_scale = k_scale, data_dir = data_dir,
                              quiet = TRUE)
        res [which (names (res) == "ss")]
    }
    f_ks <- function (k_scale = 0, k = 700, from = from, to = to,
                      data_dir = data_dir)
    {
        res <- fit_one_layer (net, from = from, to = to, k = k,
                              k_scale = k_scale, data_dir = data_dir,
                              quiet = TRUE)
        res [which (names (res) == "ss")]
    }

    # Use optimise to establish initial values
    message (cli::symbol$pointer, " Establishing initial model parameters",
                     appendLF = FALSE)
    st0 <- Sys.time ()
    k <- stats::optimise (f_k, interval = c (200, 1500), k_scale = 0.0,
                          from = from, to = to,
                          data_dir = data_dir, tol = 10)$minimum
    ks <- stats::optimise (f_ks, interval = c (-0.2, 0.3), k = k, from = from,
                           to = to, data_dir = data_dir, tol = 0.1)$minimum
    st <- formatC (as.numeric (difftime (Sys.time (), st0, units = "sec")),
                   format = "f", digits = 1)
    message ("\r", cli::symbol$tick,
             " Established initial model parameters in ", st, "s")
    k <- 10 * round (k / 10)
    ks <- round (ks * 100) / 100 # round(.,digits = 2) keeps all decimals

    # The main optimisation loop, which does not use optimise because the
    # functions are actually very noisy.

    # parallel here appears to muck with the RcppParalllel operation of dodgr
    # used for flow aggregation, so can't be (reliably) used.
    #nc <- parallel::detectCores () - 1
    #nc <- floor (parallel::detectCores () / 2)
    #cl <- parallel::makeCluster (nc)
    #doParallel::registerDoParallel (nc)

    kold <- ksold <- 1e12
    st1 <- Sys.time ()
    niters <- 1
    message (cli::symbol$pointer, " Optimising fit ", appendLF = FALSE)
    opt <- FALSE
    while (!(k == kold & ks == ksold))
    {
        kold <- k
        ksold <- ks

        kvals <- k + (-5:5) * 10
        k <- fit_one_ks (net, from, to, p, dp, s, k, ks, data_dir, kvals, fitk = TRUE)

        ksvals <- ks + (-5:5) / 100
        ks <- fit_one_ks (net, from, to, p, dp, s, k, ks, data_dir, ksvals, fitk = FALSE)

        st <- formatC (as.numeric (difftime (Sys.time (), st1, units = "sec")),
                       format = "f", digits = 1)
        message ("\r", cli::symbol$pointer, " Optimising fit; Iteration [",
                 niters, " in ", st, "s]", appendLF = FALSE)

        niters <- niters + 1
        if (k == kold & ks == ksold)
            opt <- TRUE
        if (niters > 10)
            break
    }
    message ("\r", cli::symbol$tick, "Optimised fit; Iteration [",
             niters, " in ", st, "s  ")
    st <- formatC (as.numeric (difftime (Sys.time (), st0, units = "sec")),
                   format = "f", digits = 1)
    message ()
    if (opt)
        txt <- "Optimised fit after "
    else
        txt <- "Failed to optimise fit after "
    message (cli::rule (center = paste0 (txt, niters, " iterations and ", st,
                                         "s: (k, ks) = (", k, ", ", ks, ")"),
                        line = 2, col = "green"))
    #parallel::stopCluster (cl)

    c ("k" = k, "k_scale" = ks)
}

fit_one_ks <- function (net, from, to, p, dp, s, k, ks, data_dir, kvals, fitk = TRUE)
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

    ss <- rep (NA, length (x))
    i <- NULL # suppress no visible binding note
    if (to == "disperse")
    {
        for (i in seq (x)) {
            ss [i] <- disperse_one_layer (net, fr_dat, ki [i], ksi [i],
                                          p, dp) [4]
        }
        #ss <- foreach::foreach (i = seq (x)) %dopar%
        #    disperse_one_layer (net, fr_dat, ki [i], ksi [i], p, dp) [4]
        #ss <- unlist (ss)
    } else
    {
        if (to == "residential")
            to_dat <- get_res_dat (v, data_dir)
        else if (to == "subway")
            to_dat <- get_subway_dat (s)
        else
            to_dat <- get_attractor_layer (data_dir, v, type = to)

        nfr <- nrow (fr_dat)
        nto <- nrow (to_dat)

        dmat <- dodgr::dodgr_distances (net, from = fr_dat$id, to = to_dat$id)
        dmat [is.na (dmat)] <- max (dmat, na.rm = TRUE)

        for (i in seq (x)) {
            ss [i] <- aggregate_one_layer (net, fr_dat, to_dat, ki [i],
                                           ksi [i], p, dp, dmat)
        }
        #ss <- foreach::foreach (i = seq (x)) %dopar%
        #    aggregate_one_layer (net, fr_dat, to_dat, ki [i],
        #                         ksi [i], p, dp, dmat)
        #ss <- unlist (ss)
    }

    mod <- stats::loess (ss ~ x, span = lspan)
    sdlim <- 2 * stats::sd (mod$residuals)
    n <- length (which (abs (mod$residuals) > sdlim))
    if (n > 0 & n < 3) # remove extreme values
    {
        index <- which (abs (mod$residuals) <= sdlim)
        x2 <- x [index]
        y <- ss [index]
        mod2 <- tryCatch ({
            stats::loess (y ~ x2, span = lspan)},
            warning = function (w) { NULL },
            error = function (e) { NULL })
        if (!is.null (mod2))
            mod <- mod2
        fit <- stats::predict (mod, newdata = data.frame (x2 = x))
        res <- x [which.min (fit)]
    } else
    {
        res <- x [which.min (mod$fitted)]
    }
    return (res)
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
    net_f <- dodgr::dodgr_flows_disperse (net, from = fr_dat$id,
                                          dens = fr_dat$n, k = kvals)
    flows <- flow_to_ped_pts (net_f, p, dp, get_nearest = TRUE)
    p$flows <- flows

    mod <- summary (stats::lm (p$week ~ p$flows))
    c (k = k,
       k_scale = ks,
       r2 = mod$adj.r.squared,
       ss = sum (mod$residuals ^ 2) / length (mod$residuals) / 1e6)
}

aggregate_one_layer <- function (net, fr_dat, to_dat, k, ks, p, dp, dmat)
{
    nfr <- nrow (fr_dat)
    nto <- nrow (to_dat)

    kvals <- k ^ (1 + ks * fr_dat$n / max (fr_dat$n))
    kmat <- matrix (kvals, nrow = nfr, ncol = nto)

    frmat <- matrix (fr_dat$n, nrow = nfr, ncol = nto)
    tomat <- t (matrix (to_dat$n, nrow = nto, ncol = nfr))

    emat <- frmat * exp (-dmat / kmat)
    # the first constraint, to unit sums over all destinations (columns)
    # from each origin (row)
    cmat <- t (matrix (colSums (emat), nrow = nto, ncol = nfr))
    emat <- emat / cmat
    # the second constraint, so each origin (row) allocates fr$n to all
    # destinations (columns)
    fmat <- frmat * emat / nto

    net_f <- dodgr::dodgr_flows_aggregate (net, from = fr_dat$id,
                                           to = to_dat$id, flows = fmat)
    flows <- flow_to_ped_pts (net_f, p, dp, get_nearest = TRUE)
    p$flows <- flows

    mod <- summary (stats::lm (p$week ~ p$flows))
    c (k = k,
       k_scale = ks,
       r2 = mod$adj.r.squared,
       ss = sum (mod$residuals ^ 2) / length (mod$residuals) / 1e6)
}
