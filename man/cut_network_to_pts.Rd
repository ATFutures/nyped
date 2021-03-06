% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/network-fns.R
\name{cut_network_to_pts}
\alias{cut_network_to_pts}
\title{cut_network_to_pts}
\usage{
cut_network_to_pts(net, k = 1000, p = NULL, data_dir)
}
\arguments{
\item{net}{Weighted street network; loaded from `data_dir` if not provided}

\item{k}{Exponential decay parameter to be used in spatial interaction
models. This deterines the radius at which to cut the network.}

\item{p}{Pedestrian counts including OSM IDs of nearest points, as returned
from \link{ped_osm_id}.}

\item{data_dir}{The directory in which data are to be, or have previously
been, downloaded - only needed if `p` is not provided.}
}
\description{
Cut the New York City street network into portions surrounding each
pedestrian counting point.
}
