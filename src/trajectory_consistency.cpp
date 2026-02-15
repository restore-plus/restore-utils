#include <Rcpp.h>
#include <algorithm>

using namespace Rcpp;

/**
 * @title Temporal consistency reference
 * 
 * @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
 * @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
 * 
 * @description The function checks over time whether a class is 
 *              present and, if so, reclassifies previous years 
 *              to the target class. This temporal consistency is useful for 
 *              classes that can not be presented in the future if they are not 
 *              present in the past (e.g., forest in INPE-PRODES maps, we can't 
 *              have something else in the past and forest in the future as we are 
 *              not creating original forest).
 * 
 * @param data NumericMatrix representing the data.
 * @param reference_class Integer representing the reference class.
 * @param target_class Integer representing the target class.
 * @returns NumericMatrix representing the temporal consistency reference.
 * 
 * @keywords internal
 */
// [[Rcpp::export]]
NumericMatrix C_trajectory_temporal_consistency_reference(NumericMatrix data, int reference_class, int target_class) {
    // This rule was originally implemented to:
    // > "Se temos desmatamento no ano x, todos os anos 1:x-1 deverao ser floresta"

    int npixel = data.nrow();
    int nyear = data.ncol();

    if (nyear < 3) {
        stop("Expected at least 3 years (columns), but got " + std::to_string(nyear));
    }

    for (int i = 0; i < npixel; i++) {
        for (int j = 0; j < nyear; j++) {
            // If the current year is `reference_class`, apply consistency rule
            if (static_cast<int>(data(i, j)) == reference_class) {
                // Fill past years with target value
                for (int t = 0; t < j; t++) {
                    data(i, t) = target_class;
                }
            }
        }
    }
    return data;
}