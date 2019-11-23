<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build
Status](https://travis-ci.org/ATFutures/nyped.svg)](https://travis-ci.org/ATFutures/nyped)
[![Project Status: Concept - Minimal or no implementation has been done
yet.](http://www.repostatus.org/badges/0.1.0/concept.svg)](http://www.repostatus.org/#concept)

# nyped

Model of pedestrian flows against empirical [pedestrian counts for New
York
City](https://www1.nyc.gov/html/dot/html/about/datafeeds.shtml#Pedestrians),
constructed from “flow layers” formed from pair-wise matching between
the following seven categories of origins and destinations:

1.  subway
2.  residential
3.  transportation
4.  sustenance
5.  entertainment
6.  education
7.  healthcare

An eighth category is network centrality, with additional layers
modelling dispersal from each of these categories. The model explains
R<sup>2</sup>= 83.9 of the observed variation in pedestrian counts.
Final results, with significantly explanatory layers named according to
the first three letters of the above categories, looks like this:

| Layer Name | Estimate | Std. Error | t value | Pr(\>t) |
| :--------- | -------: | ---------: | ------: | ------: |
| edu-tra    |    23977 |       4484 |    5.35 |  0.0000 |
| edu-sus    |    16904 |       5572 |    3.03 |  0.0031 |
| edu-dis    |  \-78057 |      24521 |  \-3.18 |  0.0020 |
| edu-hea    |  \-24921 |       4445 |  \-5.61 |  0.0000 |
| ent-tra    |    38179 |      12019 |    3.18 |  0.0020 |
| hea-dis    |   105658 |      10706 |    9.87 |  0.0000 |
| sub-dis    |       23 |          3 |    8.99 |  0.0000 |
| sub-hea    |        8 |          1 |    6.66 |  0.0000 |
| sub-tra    |        6 |          1 |    5.08 |  0.0000 |
| sub-cen    |     \-10 |          1 |  \-6.99 |  0.0000 |
| sus-res    |     6258 |       1232 |    5.08 |  0.0000 |
| sus-ent    |     1446 |        361 |    4.00 |  0.0001 |
| sus-sub    |   \-1337 |        331 |  \-4.04 |  0.0001 |
| sus-edu    |   \-5924 |        978 |  \-6.06 |  0.0000 |

Table 1. Statistical parameters of final model of pedestrian flows
through New York City.

A sample of actual flows looks like this:

![](./man/figures/flowmap.png)

And a final statistical relationship between modelled and observed
pedestrian counts looks like this:
<img src="man/figures/README-model-plot-1.png" width="100%" />
