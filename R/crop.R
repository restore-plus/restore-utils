#' @export
classification_crop <- function(year, roi_file, multicores, memsize, output_dir, version) {
    # Define output directory
    output_dir <- fs::path(output_dir)

    # Create output directory
    fs::create_dir(output_dir)

    # Base classification dir
    base_classifications_dir <- project_classifications_dir()

    # Classification year directory
    classification_dir <- (
        base_classifications_dir / version / year
    )

    # Loader function
    load_fn <- load_restore_map_bdc
    if (year < 2015) {
        load_fn <- load_restore_map_glad
    }

    # Define data cube
    class <- load_fn(
        data_dir   = classification_dir,
        multicores = multicores,
        memsize    = memsize,
        version    = version,
        tiles      = "MOSAIC"
    )

    # Define output file name
    in_file  <- class[["file_info"]][[1]][["path"]]
    out_file <- output_dir / fs::path_file(in_file)

    # Crop gdal
    sits:::.gdal_crop_image(
        file       = in_file,
        out_file   = out_file,
        roi_file   = roi_file,
        data_type  = "INT1U",
        as_crs     = "EPSG:4674",
        multicores = multicores,
        overwrite  = TRUE,
        miss_value = 255
    )
    # Return cropped file
    out_file
}

#' @export
terraclass_crop <- function(year, roi_file, multicores, memsize, output_dir) {
    # Stop if the year not in terraclass maps
    stopifnot(year %in% c(2008, 2010, 2012, 2014, 2018, 2020, 2022))

    # Define output directory
    output_dir <- fs::path(output_dir)

    # Create output directory
    fs::create_dir(output_dir)

    # Define loader function
    tc_loader <- get(paste0("load_terraclass_", year))
    tc_loader <- tc_loader(multicores = multicores, memsize = memsize)

    # Define output file name
    in_file <- tc_loader[["file_info"]][[1]][["path"]]
    out_file <- output_dir / fs::path_file(in_file)

    # Crop gdal
    sits:::.gdal_crop_image(
        file       = in_file,
        out_file   = out_file,
        roi_file   = roi_file,
        data_type  = "INT1U",
        as_crs     = "EPSG:4674",
        multicores = multicores,
        overwrite  = TRUE,
        miss_value = 255
    )
    # Return cropped file
    out_file
}
