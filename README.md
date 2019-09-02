<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build
Status](https://travis-ci.org/moveability/calibration.svg)](https://travis-ci.org/moveability/calibration)
[![Project Status: Concept - Minimal or no implementation has been done
yet.](http://www.repostatus.org/badges/0.1.0/concept.svg)](http://www.repostatus.org/#concept)

calibration
===========

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/moveability/calibration.svg?branch=master)](https://travis-ci.org/moveability/calibration)
<!-- badges: end -->

Calibration of [moveability](https://github.com/moveability/moveability)
measures for walking and cycling.

Accra calibration
-----------------

### Get base OSM data

``` r
data_dir <- "/data/data/moveability/accra"
elev_dir <- "/data/data/elevation"
elev_file <- list.files (elev_dir, full.names = TRUE)
elev_file <- elev_file [grep ("36_11.tif", elev_file)]

library (osmdata)
bb <- getbb ("accra ghana", format_out = "polygon") # no polygon available
hw <- opq (bb) %>%
    add_osm_feature (key = "highway") %>%
    osmdata_sc (quiet = FALSE)
hw <- osm_elevation (hw, elev_file = elev_file)
saveRDS (hw, file = file.path (data_dir, "accra-hw.Rds"))

library (moveability)
attractors <- get_attractors (bb)
saveRDS (attractors, file = file.path (data_dir, "accra-attractors.Rds"))
green_polys <- get_green_space (bb)
saveRDS (green_polys, file = file.path (data_dir, "accra-green.Rds"))
```

``` r
library (moveability)
data_dir <- "/data/data/moveability/accra"
streetnet <- readRDS (file.path (data_dir, "accra-hw.Rds"))
green_polys <- readRDS (file.path (data_dir, "accra-green.Rds"))
attractors <- readRDS (file.path (data_dir, "accra-attractors.Rds"))
graph <- dodgr::weight_streetnet (streetnet, wt_profile = "foot")
graphc <- dodgr::dodgr_contract_graph (graph)
v <- dodgr::dodgr_vertices (graphc)
ids <- split (v$id, cut (v$n, breaks = 6))
mf <- lapply (ids, function (i)
              {
                moveability (graphc, from = i,
                             green_polys = green_polys,
                             activity_points = attractors,
                             mode = "foot", d_threshold = 0.7)
              })
mf <- do.call (rbind, mf)
rownames (mf) <- NULL
mf$green_area [is.nan (mf$green_area)] <- 0
saveRDS (mf, file.path (data_dir, "accra-moveability.Rds"))
# _to_lines scales moveability by sqrt of num. activity centres
mfsf <- moveability_to_lines (mf, streetnet)
saveRDS (mfsf, file.path (data_dir, "accra-moveability-sf.Rds"))
```

The following code scales moveability to population density, but that
doesn’t help here.

``` r
library (moveability)
data_dir <- "/data/data/moveability/accra"
m <- readRDS (file.path (data_dir, "accra-moveability.Rds"))
popdens <- readRDS ("/data/mega/code/repos/atfutures/who-data/accra/osm/popdens_nodes.Rds")
popdens <- popdens [popdens$osm_id %in% m$id, ]
m$popdens <- NA
m$popdens [match (popdens$osm_id, m$id)] <- popdens$pop
m <- m [!is.na (m$popdens), ]
m$m <- m$m * m$popdens
streetnet <- readRDS (file.path (data_dir, "accra-hw.Rds"))
msf <- moveability_to_lines (m, streetnet)
saveRDS (msf, file.path (data_dir, "accra-moveability-sf.Rds"))
```

Have to use google rather than OSM places, by loading the file from the
binaries in the `who-data` repo.

``` r
attractors <- readRDS ("<...>/atfutures/who-data/accra/osm/accra-gplaces.Rds")
index <- dodgr::match_points_to_graph (v, attractors [, c ("lon", "lat")])
attractors$id <- v$id [index]
names (attractors) [which (names (attractors) %in% c ("lon", "lat"))] <- c ("x", "y")
```

then repeat the above code to calculating moveability using these
`attractors`. Also scale moveability to the sqrt of numbers of activity
centres.

``` r
m <- readRDS (file.path (data_dir, "accra-moveability-sf.Rds"))
m <- mfsf
m$flow <- m$flow * 50 / max (m$flow)
library (mapdeck)
set_token (Sys.getenv ("MAPBOX_TOKEN"))
mapdeck (style = 'mapbox://styles/mapbox/dark-v9') %>%
    add_path (data = m,
              layer_id = "foot",
              stroke_colour = "flow",
              stroke_width = "flow",
              palette = "inferno")
```
