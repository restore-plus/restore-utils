#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix C_trajectory_transition_analysis(NumericMatrix data, int reference_class, int neighbor_class) {
    int npixel = data.nrow();
    int nyear = data.ncol();

    if (nyear != 10) {
        stop("Expected exactly 10 years (columns), but got " + std::to_string(nyear));
    }

    for (int i = 0; i < npixel; i++) {
        for (int j = 0; j + 2 < nyear; j += 3) {

            bool valid_neihbor = data(i, j) == neighbor_class && data(i, j + 2) == neighbor_class;
            bool valid_class = data(i, j + 1) == reference_class;

            if (valid_class && valid_neihbor) {
                data(i, j + 1) = neighbor_class;

                // 9 means the last time step
                // 6 means the last iteration for the moving window
                if (j == 6 && data(i, 9) == reference_class) {
                    data(i, nyear - 1) = neighbor_class;
                }
            }
        }
    }

    return data;
}


// [[Rcpp::export]]
NumericMatrix C_trajectory_neighbor_analysis(NumericMatrix data, int reference_class, int replacement_class) {
    int npixel = data.nrow();
    int nyear = data.ncol();

    if (nyear < 3) {
        stop("Expected at least 3 years (columns), but got " + std::to_string(nyear));
    }

    for (int i = 0; i < npixel; i++) {
        // remove edges (start: j = 1; end = j - 1)
        for (int j = 1; j < nyear - 1; j++) {

            if (data(i, j) == reference_class) {
                bool is_left_diff = data(i, j - 1) != reference_class;
                bool is_right_diff = data(i, j + 1) != reference_class;

                if (is_left_diff && is_right_diff) {
                    data(i, j) = replacement_class;
                }
            }
        }
    }

    return data;
}
