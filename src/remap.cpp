#include <Rcpp.h>

using namespace Rcpp;

/**
 * @title Remap values
 * 
 * @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
 * @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}    
 * 
 * @description The function remaps the values of a raster file from a source to a target.
 * 
 * @param data NumericMatrix representing the data.
 * @param source Integer representing the source value.
 * @param target Integer representing the target value.
 * 
 * @returns NumericMatrix representing the remapped data.
 * 
 * @keywords internal
 */
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
