#include "flow-to-ped-pts.h"


//' rcpp_flow_to_ped_pts
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericVector rcpp_flow_to_ped_pts (Rcpp::DataFrame graph,
        Rcpp::DataFrame ped, Rcpp::NumericMatrix dmat, const int n)
{
    Rcpp::CharacterVector vx0 = graph [".vx0"];
    Rcpp::CharacterVector vx1 = graph [".vx1"];
    Rcpp::NumericVector flow = graph ["flow1"];

    // make maps from .vx0 and .vx1 to flow values
    std::unordered_map <std::string, double> vx0_flow, vx1_flow;
    for (size_t i = 0; i < vx0.size (); i++)
    {
        vx0_flow.emplace (vx0 [i], flow [i]);
        vx1_flow.emplace (vx1 [i], flow [i]);
    }

    Rcpp::List dimnames = dmat.attr ("dimnames");
    Rcpp::CharacterVector colnames = dimnames [1];

    const size_t nrow = dmat.nrow (), ncol = dmat.ncol ();
    Rcpp::NumericVector res (nrow);
    
    for (size_t i = 0; i < nrow; i++)
    {
        // Get sort order for distances in that row of dmat:
        std::vector <std::pair <double, int> > di_v (ncol);
        for (size_t j = 0; j < ncol; j++)
            di_v [j] = std::make_pair (dmat (i, j), j);
        std::sort (di_v.begin (), di_v.end ());

        // Extract first n non-zero flows from .vx0:
        double flow0 = count_flows (vx0_flow, di_v, colnames, n);
        double flow1 = count_flows (vx1_flow, di_v, colnames, n);

        res [i] = flow0 + flow1;
    }

    return (res);
}

double count_flows (const std::unordered_map <std::string, double> &map,
        std::vector <std::pair <double, int> > &dord,
        Rcpp::CharacterVector &colnames, const int n)
{
        int count = 0;
        size_t i = 0;
        double flow = 0.0;
        while (count < n)
        {
            size_t index = dord [i++].second;
            std::string vname = Rcpp::as <std::string> (colnames [index]);
            double f = map.at (vname);
            if (f > 0.0)
            {
                flow += f;
                count++;
            }
        }
        return flow / static_cast <double> (n);
}
