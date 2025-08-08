#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix C_remap_values(NumericMatrix& data, int source, int target) {
    int npixel = data.nrow();
    int nyear = data.ncol();

    if (nyear != 1) {
        stop("Expected exactly 1 year (columns), but got " + std::to_string(nyear));
    }

    for (int i = 0; i < npixel; i++) {
        if (data(i, 0) == source) {
            data(i, 0) = target;
        }
    }

    return data;
}
