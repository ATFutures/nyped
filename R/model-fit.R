#' add_layer_to_model
#'
#' Build final pedestrian model
#'
#' @param dat Output of previous run of `add_layer_to_model`
#' @param sig Desired level of statistical significance
#' @param pos_only Only include layers that make a positive contribution to
#' final model (and so exclude any layers that are negatively correlated)?
#' @inheritParams get_layer
#' @export
add_layer_to_model <- function (data_dir, dat = NULL, sig = 0.01, pos_only = TRUE)
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
        n <- dat$n
        k <- dat$k
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
                            tval <- utils::tail (mod$coefficients [, 3], 1)
                            c (sum (mod$residuals ^ 2) / 1e6,
                               mod$r.squared,
                               tval)  })
        stats <- t (stats) # then 3 columns of (ss, r2, T-statistic)
        # Find min eror only for those models with positive T-statistics:
        if (all (stats [, 3] < 0)) # no positive-correlated layers
            next

        ssmin_i <- min (stats [, 1])
        if (pos_only)
            ssmin_i <- min (stats [, 1] [stats [, 3] > 0])
        
        if (length (ssmin_i) > 1)
            stop ("nope#1 at f = ", f)
        if (length (ssmin) > 1)
            stop ("nope#2 at f = ", f)
        if (ssmin_i < ssmin)
        {
            i <- which (stats [, 1] == ssmin_i) [1]
            ssmin <- stats [i, 1]
            r2_out <- stats [i, 2]
            n_out <- dat$n [i]
            k_out <- dat$k [i]
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

        message (cli::col_green (cli::symbol$tick), " ",
                 cli::col_blue (paste0 ("Layer#", ncol (flowvars))),
                 " : R2 = ", signif (r2_out, 4), " for ",
                 cli::col_red (from_out), " to ", cli::col_red (to_out))
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

#' build_ped_model
#'
#' @inheritParams add_layer_to_model
#' @param sig_build Significance threshold for initial constructing of model
#' through adding successive layers
#' @param sig_model Desired significance of final model
#' @return The final model reduced to only layers with significance below
#' `sig_model`.
#'
#' @note This function can take several minutes to run, and saves the final
#' model in two forms, both stored in `data_dir`: the initial, full model with
#' all layers, called 'ped-model-full.Rds', and the final model with only layers
#' with significance below `sig_model`, called `ped-model-final.Rds`.
#' @export
build_ped_model <- function (data_dir, sig_build = 0.05, sig_model = 0.01,
                             pos_only = FALSE)
{
    all_flows_to_ped (data_dir)
    dat <- add_layer_to_model (data_dir, dat = NULL, sig = sig_build)
    while (dat$is_significant)
        dat <- add_layer_to_model (data_dir, dat = dat, pos_only = pos_only,
                                sig = sig_build)

    saveRDS (dat, file.path (data_dir, "ped-model-full.Rds"))

    # Then reduce layers down to `sig_model`
    f_ <- dat$flowvars
    mod <- summary (stats::lm (dat$p ~ f_))
    index <- which (mod$coefficients [, 4] [-1] < sig_model)
    while (length (index) < ncol (f_)) {
        f_ <- dat$flowvars [, index]
        mod <- summary (stats::lm (dat$p ~ f_))
        index <- which (mod$coefficients [, 4] [-1] < sig_model)
    }

    # reduce model down 
    layers <- gsub ("f_", "", rownames (mod$coefficients) [-1])
    index <- match (layers, colnames (dat$flowvars))
    dat$flowvars <- dat$flowvars [, index]
    dat$r2 <- dat$r2 [index]
    dat$from <- dat$from [index]
    dat$to <- dat$to [index]
    dat$n <- dat$n [index]
    dat$k <- dat$k [index]
    saveRDS (dat, file = file.path (data_dir, "ped-model-final.Rds"))

    return (dat)
}

#' calc_layer_scales
#'
#' Calculate relative scales of contributions of each flow layer to final
#' model of pedestrian flows generated by successive calls to
#' \link{build_ped_model}.
#'
#' @inheritParams get_layer
#' @param model Default "final" uses the final model of layers reduced only to
#' those with significance less than `sig_model` in \link{build_ped_model},
#' otherwise the full model with potentially less significant layers is used.
#' @return A `data.frame` with six columns of (1) 'model' with the layer names;
#' (2) 'estimates' holding the estimates for each layer from the multiple linear
#' regression model; (3) 'full_rel' with the relative scaling coefficient for
#' the contribution of each layer weighted by the full flow layers; (4)
#' 'ped_rel' with equivalent values weighted by values at pedestrian count
#' stations only; (5) 'full_abs' with the absolute scaling coefficient for each
#' layer weighted by full flow layers; and (6) 'ped_abs' with equivalent values
#' weighted by values at pedestrian count stations only.
#'
#' @export
calc_layer_scales <- function (data_dir, model = "final")
{
    if (model == "final")
        f <- file.path (data_dir, "ped-model-final.Rds")
    else
        f <- file.path (data_dir, "ped-model-full.Rds")
    if (!file.exists (f))
        stop ("Final model file [", f, "] not found; ",
              "have you run 'build_ped_model'?")

    dat <- readRDS (f)
    f_ <- dat$flowvars # put as separate variable because print method uses full name
    lmod <- summary (stats::lm (dat$ped_counts ~ f_))
    estimates <- lmod$coefficients [2:nrow (lmod$coefficients), 1]
    tvals <- lmod$coefficients [2:nrow (lmod$coefficients), 3]
    pvals <- lmod$coefficients [2:nrow (lmod$coefficients), 4]

    f <- ped_model_to_full_flow (dat, data_dir)

    index <- grep ("flow", names (f))
    full_rel <- ped_rel <- rep (NA, ncol (f_))
    for (i in seq (index))
    {
        x <- f [, index [i]] [f [index [i]] > 0]
        full_rel [i] <- mean (x * estimates [i])
        ped_rel [i] <- mean (f_ [, i] * estimates [i])
    }

    full_rel <- full_rel / sum (full_rel)
    ped_rel <- ped_rel / sum (ped_rel)

    layer_names <- paste0 (dat$from, "-", dat$to)

    res <- data.frame (layer = layer_names,
                       estimates = estimates,
                       full_rel = full_rel,
                       ped_rel = ped_rel,
                       full_abs = full_rel * estimates,
                       ped_abs = ped_rel * estimates,
                       t = tvals,
                       p = pvals,
                       stringsAsFactors = FALSE)
    rownames (res) <- NULL
    res [order (abs (res$t), decreasing = TRUE), ]
}

#' ped_model_to_full_flow
#'
#' Convert a final model of pedestrian flows at the pedestrian count stations
#' back into a model of full flows along each edge of the entire network
#'
#' @param mod A final pedestrian model produced by successive calls to
#' \link{build_ped_model}
#' @inheritParams get_layer
#' @export
ped_model_to_full_flow <- function (mod, data_dir)
{
    from <- mod$from
    to <- mod$to
    files <- file.path (data_dir, "calibration",
                        paste0 ("net-", from, "-", to, ".Rds"))
    flows <- NULL
    kvals <- 1:30 * 100
    message (cli::col_green (cli::symbol$pointer,
             " Extracting final layers from full network:"))
    pb <- utils::txtProgressBar (style = 3)
    for (i in seq (files))
    {
        x <- readRDS (files [i])
        nf <- which (kvals == mod$k [i])
        flows <- cbind (flows, x [[paste0 ("flow", nf)]])
        utils::setTxtProgressBar (pb, i / length (files))
    }
    close (pb)
    message (cli::col_green (cli::symbol$tick,
             " Extracted final layers from full network"))

    fnames <- paste0 ("flow-", mod$from, "-", mod$to)
    colnames (flows) <- fnames

    # Then get all the non-flow data from one file, and cbind those flows:
    index <- which (!grepl ("flow", names (x)))
    return (cbind (x [, index], flows))
}
