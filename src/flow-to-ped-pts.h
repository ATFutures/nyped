#pragma once

#include <unordered_map>

#include <Rcpp.h>

double count_flows (const std::unordered_map <std::string, double> &map,
        std::vector <std::pair <double, int> > &dord,
        Rcpp::CharacterVector &colnames, const int n);

Rcpp::NumericVector rcpp_flow_to_ped_pts (Rcpp::DataFrame graph,
        Rcpp::DataFrame ped, Rcpp::NumericMatrix dmat, 
        const std::string flow_col_name, const int n);

Rcpp::NumericMatrix rcpp_match_flow_mats (Rcpp::NumericMatrix flows,
        Rcpp::IntegerMatrix index0, Rcpp::IntegerMatrix index1,
        Rcpp::IntegerVector fcols, const int n);
