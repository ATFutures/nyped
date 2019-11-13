#include "flow-to-ped-pts.h"


//' rcpp_match_flow_mats
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_match_flow_mats (Rcpp::NumericMatrix flows,
        Rcpp::IntegerMatrix index0, Rcpp::IntegerMatrix index1,
        Rcpp::IntegerVector fcols, const int n)
{
    const int ncol = fcols.size ();
    const int nrow = index0.nrow ();
    Rcpp::NumericMatrix result (nrow, ncol);

    for (int i = 0; i < nrow; i++) // the 113 pedestrian count stations
    {
        for (int j = 0; j < ncol; j++) // the 30 k-values
        {
            double fmax = 0.0;
            for (auto rr: flows (Rcpp::_, j))
                if (rr > fmax)
                    fmax = rr;
            if (fmax == 0.0)
                continue;

            Rcpp::NumericVector flows_j = flows (Rcpp::_, j);
            int count = 0;
            size_t k = 0;
            while (count < n)
            {
                double f = flows_j (index0 (i, k++));
                if (k >= index0.nrow ())
                    break;
                if (f > 0.0)
                {
                    result (i, j) += f;
                    count++;
                }
            }
            count = 0;
            k = 0;
            while (count < n)
            {
                double f = flows_j (index1 (i, k++));
                if (k >= index1.nrow ())
                    break;
                if (f > 0.0)
                {
                    result (i, j) += f;
                    count++;
                }
            }
            result (i, j) = result (i, j) / static_cast <double> (n);
        }
    }

    return result;
}
