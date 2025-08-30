#include <gdal_priv.h>
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector C_Raster_Get_Scale(std::string filename) {
    GDALAllRegister();
    GDALDataset *poDataset = (GDALDataset *) GDALOpen(filename.c_str(), GA_ReadOnly);

    if (poDataset == nullptr) {
        Rcpp::stop("Cannot open raster file");
    }

    NumericVector res;

    int nBands = poDataset->GetRasterCount();
    for (int idx = 1; idx <= nBands; idx++) {
        GDALRasterBand *poBand = poDataset->GetRasterBand(idx);

        res.push_back(poBand->GetScale());
    }

    return res;
}

// [[Rcpp::export]]
NumericVector C_Raster_Get_Offset(std::string filename) {
    GDALAllRegister();
    GDALDataset *poDataset = (GDALDataset *) GDALOpen(filename.c_str(), GA_ReadOnly);

    if (poDataset == nullptr) {
        Rcpp::stop("Cannot open raster file");
    }

    NumericVector res;

    int nBands = poDataset->GetRasterCount();
    for (int idx = 1; idx <= nBands; idx++) {
        GDALRasterBand *poBand = poDataset->GetRasterBand(idx);

        res.push_back(poBand->GetOffset());
    }

    return res;
}

// [[Rcpp::export]]
void C_Raster_Set_Scale_Offset(std::string filename, double scale, double offset) {

    GDALAllRegister();
    GDALDataset *poDataset = (GDALDataset *) GDALOpen(filename.c_str(), GA_Update);

    if (poDataset == nullptr) {
        Rcpp::stop("Cannot open raster file");
    }

    int nBands = poDataset->GetRasterCount();
    for (int idx = 1; idx <= nBands; idx++) {
        GDALRasterBand *poBand = poDataset->GetRasterBand(idx);

        poBand->SetScale(scale);
        poBand->SetOffset(offset);
    }

    GDALClose(poDataset);
}
