#pragma once

#include <Rcpp.h>

Rcpp::NumericMatrix rcpp_match_flow_mats (Rcpp::NumericMatrix flows,
        Rcpp::IntegerMatrix index0, Rcpp::IntegerMatrix index1,
        Rcpp::IntegerVector fcols, const int n);
