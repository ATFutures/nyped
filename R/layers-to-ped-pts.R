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
    # then convert that to equivalent indices into both .vx0 and .vx1 of net_f,
    # so index0 is a matrix with one row for each ped station, and columns
    # containing indices into .vx0 of net_f in increasing order of distance.
    index0 <- t (apply (dp_verts, 1, function (i) match (i, net_f$.vx0)))
    index1 <- t (apply (dp_verts, 1, function (i) match (i, net_f$.vx1)))

    flowmat <- as.matrix (net_f [, fcols])
    index <- which (colSums (flowmat) > 0)
    k <- n <- NA
    temp <- list ()
    if (length (index) == 0)
    {
        message (cli::col_red (cli::symbol$cross, 
                               " Flow layer has no non-zero values"))
    } else
    {
        n <- 1:20
        pb <- utils::txtProgressBar (style = 3)
        for (i in seq (n))
        {
            temp [[i]] <- rcpp_match_flow_mats (flowmat, index0 - 1, index1 - 1,
                                                fcols - 1, n [i])
            utils::setTxtProgressBar (pb, i / length (n))
        }
        close (pb)

        temp <- do.call (cbind, temp)

        if (max (temp) > 0)
        {
            k <- 1:30 * 100
            nk <- length (k) # 30
            n <- 1:20
            k <- rep (k, length (n))
            n <- rep (n, each = nk)
        }
    }

    list (k = k, n = n, flows = temp)
}

#' fit_cen_to_ped
#'
#' Fit network centrality to pedestrian counts, by aggregating across a variable
#' number of edges ('n') nearest to each pedestrian count station.
#'
#' @inheritParams get_layer
#' @return A list containing vectors of 'k' and 'n' values, and a matrix of
#' 30x20 = 600 columns, one for each combination of 30 'k'- and 20 'n'-values.
#'
#' @export
fit_cen_to_ped <- function (data_dir)
{
    # plus centrality to pedestrian stations
    f <- file.path (data_dir, "ny-centrality-edge.Rds")
    if (file.exists (f))
    {
        # called cen-cen to conform with expected names in building final model
        fout <- file.path (data_dir, "calibration", "ped-flows-cen-cen.Rds")
        if (!file.exists (fout))
        {
            message (cli::col_cyan (cli::symbol$star), " centrality:")
            flows <- fit_cen_to_ped_intern (f, data_dir)
            saveRDS (flows, file = fout)
            message ("\r", cli::col_green (cli::symbol$tick), " centrality \n")
        }
    }
}

fit_cen_to_ped_intern <- function (f, data_dir)
{
    net_f <- readRDS (f)
    net_f$centrality [net_f$centrality < 0] <- 0
    net_f$centrality [!is.finite (net_f$centrality)] <- 0
    net_f$centrality  <- net_f$centrality / max (net_f$centrality)

    p <- ped_osm_id (data_dir = data_dir, net = net_f, quiet = TRUE)
    dp <- dodgr::dodgr_dists (net_f, from = p$id)

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
    # then convert that to equivalent indices into both .vx0 and .vx1 of net_f,
    # so index0 is a matrix with one row for each ped station, and columns
    # containing indices into .vx0 of net_f in increasing order of distance.
    index0 <- t (apply (dp_verts, 1, function (i) match (i, net_f$.vx0)))
    index1 <- t (apply (dp_verts, 1, function (i) match (i, net_f$.vx1)))

    flowmat <- as.matrix (net_f$centrality)
    temp <- list ()
    n <- 1:20
    pb <- utils::txtProgressBar (style = 3)
    for (i in seq (n))
    {
        temp [[i]] <- rcpp_match_flow_mats (flowmat, index0 - 1, index1 - 1,
                                            1, n [i])
        utils::setTxtProgressBar (pb, i / length (n))
    }
    close (pb)

    temp <- do.call (cbind, temp)

    n <- 1:20

    list (k = NA, n = n, flows = temp)
}

#' all_flows_to_ped
#'
#' Batch-convert all flow layers to equivalent values matched to pedestrian
#' counting stations via the \link{fit_flows_to_ped} function.
#'
#' @inheritParams get_layer
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
                message (cli::col_cyan (cli::symbol$star), " ",
                         fi, " -> ", ti, ":")
                net_f <- readRDS (f)
                flows <- fit_flows_to_ped (net_f, data_dir)
                if (length (flows$flows) > 0)
                {
                    saveRDS (flows, file = fout)
                    message ("\r", cli::col_green (cli::symbol$tick),
                             " ", fi, " -> ", ti, "\n")
                } else
                    message ("\r", cli::col_red (cli::symbol$cross),
                             " ", fi, " -> ", ti, "\n")
            }
        }
    }

    fit_cen_to_ped (data_dir)
}
