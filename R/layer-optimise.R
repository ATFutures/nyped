#' optimise_layer
#'
#' Optimise `k` and `k_scale` parameters to optimally fit flow layer to observed
#' pedestrian densities
#' @inheritParams ny_layer
#' @export
optimise_layer <- function (net, from, to, data_dir = data_dir)
{
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
    k <- stats::optimise (f_k, interval = c (200, 1500), from = from, to = to,
                          data_dir = data_dir, tol = 10)$minimum
    ks <- stats::optimise (f_ks, interval = c (-0.2, 0.3), k = k, from = from,
                           to = to, data_dir = data_dir, tol = 0.1)$minimum
    st <- formatC (as.numeric (difftime (Sys.time (), st0, units = "sec")),
                   format = "f", digits = 1)
    message ("\r", cli::symbol$tick,
             " Established initial model parameters in ", st, "s")
    k <- 10 * round (k / 10)
    #ks <- round (ks, digits = 2) # that keeps all decimals, but just sets them to 0
    ks <- round (ks * 100) / 100

    # The main optimisation loop, which does not use optimise because the
    # functions are actually very noisy.

    #nc <- parallel::detectCores () - 1
    #cl <- parallel::makeCluster (nc)
    #parallel::clusterExport (cl, c ("net", "from", "to", "data_dir",
    #                                "fit_one_layer", "ny_layer", "ped_osm_id",
    #                                "get_layer", "subway_osm_id",
    #                                "get_file_name", "calc_layer",
    #                                "nyped_data", "nysubway_data",
    #                                "layer_disperse", "layer_subway_attr",
    #                                "layer_subway_subway", "layer_attr_attr",
    #                                "layer_subway_res", "d_subway_res",
    #                                "flow_to_ped_pts", "get_attractor_layer"))
    ##                         envir = environment ())

    kold <- ksold <- 1e12
    st0 <- Sys.time ()
    niters <- 1
    while (!(k == kold & ks == ksold))
    {
        kold <- k
        ksold <- ks

        kvals <- k + (-5:5) * 10
        k <- fit_one_ks (net, from, to, ks, k, data_dir, kvals, fitk = TRUE)

        ksvals <- ks + (-5:5) / 100
        ks <- fit_one_ks (net, from, to, ks, k, data_dir, ksvals, fitk = FALSE)

        st <- formatC (as.numeric (difftime (Sys.time (), st0, units = "sec")),
                       format = "f", digits = 1)
        message ("\r", cli::symbol$tick,
                 " Iteration [", niters, " in ", st, "s")
        niters <- niters + 1
    }
    #parallel::stopCluster (cl)

    c ("k" = k, "k_scale" = ks)
}

fit_one_ks <- function (net, from, to, ks, k, data_dir, kvals, fitk = TRUE)
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
    ss <- rep (NA, length (x))
    for (i in seq (x)) {
        ss [i] <- fit_one_layer (net, from = from, to = to,
                                 k = ki [i], k_scale = ksi [i],
                                 data_dir, quiet = TRUE) [4]
    }

    # attempt at parallel, which fails:
    #kvals <- data.frame (k = ki, ks = ksi)
    ## convert to list of rows:
    #kvals <- as.list (as.data.frame (t (kvals)))
    #ss <- parallel::parLapply (cl, kvals, function (i) {
    #                               fit_one_layer (net, from, to, i [1], i [2],
    #                                              data_dir, quiet = TRUE) [4]
    #        })
    mod <- stats::loess (ss ~ x, span = lspan)
    sdlim <- 2 * stats::sd (mod$residuals)
    n <- length (which (abs (mod$residuals) > sdlim))
    if (n > 0 & n < 3) # remove extreme values
    {
        index <- which (abs (mod$residuals) <= sdlim)
        x2 <- x [index]
        y <- ss [index]
        suppressWarnings (mod <- stats::loess (y ~ x2, span = lspan))
        fit <- stats::predict (mod, newdata = data.frame (x2 = x))
        res <- x [which.min (fit)]
    } else
    {
        res <- x [which.min (mod$fitted)]
    }
    return (res)
}
