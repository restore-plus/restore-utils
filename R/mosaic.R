#' @title Mosaic a cube
#' 
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' 
#' @description Mosaic a cube.
#' 
#' @param cube A \code{sits} cube.
#' @param output_dir Character path for intermediate files.
#' @param multicores Integer specifying the number of cores for parallel
#'                   processing (default: 64).
#' @param bands      Character vector of band names to include in the mosaic.
#'                   Default is NULL, which includes all bands.
#' @param roi_file   Optional region of interest (ROI) to filter chunks.
#'                   Default is NULL.
#' @param mbtiles    Logical indicating whether to create MBTILES files.
#'                   Default is FALSE.
#'
#' @returns A tibble with the mosaic files.
#'
#' @export
mosaic_cube_glad <- function(cube,
                                   output_dir,
                                   multicores = 64,
                                   bands = NULL,
                                   roi_file = NULL,
                                   mbtiles = FALSE) {
    # convert output_dir to fs
    output_dir <- fs::path(output_dir)

    # create base output dirs
    output_mosaic_tile_dir <- output_dir / "tiles"
    output_mosaic_complete_dir <- output_dir / "cube"

    # mosaic by tile
    print("Processing tiles")
    mosaic <- purrr::map_dfr(seq_len(nrow(cube)), function(tile_idx) {
        # select tile
        tile <- cube[tile_idx, ]

        # extract tile crs
        tile_crs <- unique(cube[["crs"]])

        # define output directory
        tile_dir <- output_mosaic_tile_dir / tile[["tile"]]

        # create dir
        fs::dir_create(tile_dir, recurse = TRUE)

        # mosaic
        sits_mosaic(
            cube       = tile,
            multicores = multicores,
            output_dir = tile_dir,
            crs        = tile_crs
        )
    })

    print("Finished - tiles are now ready")

    # create output dir to the whole area
    fs::dir_create(output_mosaic_complete_dir, recurse = TRUE)

    # get mosaic timeline
    mosaic_timeline <- sits::sits_timeline(mosaic)

    # merge mosaic file infos
    mosaic_files <- dplyr::bind_rows(mosaic[["file_info"]])

    # mosaic by date
    mosaic_files <- purrr::map_vec(mosaic_timeline, function(timeline_date) {
        # define output file
        mosaic_file <- output_mosaic_complete_dir / paste0(timeline_date, ".tif")

        # if file exists, return it
        if (fs::file_exists(mosaic_file)) {
            # create mbtiles if needed
            mosaic_mbtiles <- output_mosaic_complete_dir / paste0(timeline_date, ".mbtiles")
            # generate mbtiles if file doesn't exist
            if (mbtiles && !fs::file_exists(mosaic_mbtiles)) {
                system(paste(
                    "gdal_translate -of MBTILES",
                    mosaic_file,
                    mosaic_mbtiles,
                    sep = " "
                ))
                # generate cogs
                system(paste("gdaladdo -r average ", mosaic_file, "2 4 8 16 32", sep = " "))
                # update mosaic file name
                mosaic_file <- mosaic_mbtiles
            }
            return(mosaic_file)
        }

        # filter files by date
        tiles_in_date <- mosaic_files  |>
            dplyr::filter(date == timeline_date) |>
            dplyr::mutate(band = factor(band, levels = bands)) |>
            dplyr::arrange(band)

        # process files by `fid` (assuming 3 bands per fid)
        vrt_files <- dplyr::group_by(tiles_in_date, .data[["fid"]]) |>
            dplyr::group_map(function(group_fid, key) {
                # define vrt file path
                vrt_file <- fs::path(paste0(fs::file_temp(), ".vrt"))

                # create vrt
                sf::gdal_utils(
                    util = "buildvrt",
                    source = group_fid[["path"]],
                    destination = vrt_file,
                    options = c("-separate")
                )

                # return!
                vrt_file
            }) |>
            unlist()

        # define vrt list file
        vrt_files_lst <- fs::file_temp(ext = "txt")

        # write vrt files to list file
        readr::write_lines(vrt_files, file = vrt_files_lst)

        # build vrt (using system as sf was raising errors)
        vrt_merged <- fs::file_temp(ext = "vrt")

        system(paste(
            "gdalbuildvrt -input_file_list",
            vrt_files_lst,
            vrt_merged,
            sep = " "
        ))

        # create tif file
        sf::gdal_utils(
            util = "translate",
            source = vrt_merged,
            destination = mosaic_file,
            options = c(
                "-of", "GTiff",
                "-ot", "Byte",
                "-a_nodata", "0",
                "-b", "1",
                "-b", "2",
                "-b", "3",
                "-co", "BIGTIFF=YES",
                "-co", "TILED=YES",
                "-co", "COMPRESS=ZSTD",
                "-co", "PREDICTOR=1",
                "-co", "NUM_THREADS=ALL_CPUS"
            ),
            config_options = c(
                "GDAL_CACHEMAX" = "4096"
            ),
            quiet = FALSE
        )

        if (!is.null(roi_file)) {
            mosaic_cropped <- output_mosaic_complete_dir / paste0(timeline_date, "-crop.tif")
            sf::gdal_utils(
                util = "warp",
                source = mosaic_file,
                destination = mosaic_cropped,
                options = c(
                    "-cutline", roi_file,
                    "-overwrite",
                    "-crop_to_cutline",
                    "-co", "BIGTIFF=YES",
                    "-co", "TILED=YES",
                    "-co", "COMPRESS=ZSTD",
                    "-co", "PREDICTOR=1",
                    "-co", "NUM_THREADS=ALL_CPUS"
                ),
                config_options = c(
                    "GDAL_CACHEMAX" = "4096"
                ),
                quiet = FALSE
            )

            # Move files
            fs::file_move(
                path     = mosaic_cropped,
                new_path = mosaic_file
            )
        }

        # create mbtiles
        if (mbtiles) {
            mosaic_mbtiles <- output_mosaic_complete_dir / paste0(timeline_date, ".mbtiles")
            system(paste(
                "gdal_translate -of MBTILES",
                mosaic_file,
                mosaic_mbtiles,
                sep = " "
            ))
            # update mosaic file name
            mosaic_file <- mosaic_mbtiles
        }

        # add mbtiles zoom
        system(paste("gdaladdo -r average ", mosaic_file, "2 4 8 16 32", sep = " "))

        # return
        mosaic_file
    })

    # return!
    return(mosaic_files)
}

#' @title Mosaic a cube
#' 
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' 
#' @description Mosaic a cube.
#' 
#' @param cube A \code{sits} cube.
#' @param output_dir Character path for intermediate files.
#' @param multicores Integer specifying the number of cores for parallel
#'                   processing (default: 64).
#' @param bands      Character vector of band names to include in the mosaic.
#'                   Default is NULL, which includes all bands.
#' @param roi_file   Optional region of interest (ROI) to filter chunks.
#'                   Default is NULL.
#' @param mbtiles    Logical indicating whether to create MBTILES files.
#'                   Default is FALSE.
#'
#' @returns A tibble with the mosaic files.
#'
#' @export
mosaic_cube_bdc <- function(cube,
                                   output_dir,
                                   multicores = 64,
                                   bands = NULL,
                                   roi_file = NULL,
                                   mbtiles = FALSE) {
    # convert output_dir to fs
    output_dir <- fs::path(output_dir)

    # create base output dirs
    output_mosaic_tile_dir <- output_dir / "tiles"
    output_mosaic_complete_dir <- output_dir / "cube"

    # mosaic by tile
    print("Processing tiles")
    mosaic <- purrr::map_dfr(seq_len(nrow(cube)), function(tile_idx) {
        # select tile
        tile <- cube[tile_idx, ]

        # extract tile crs
        tile_crs <- unique(cube[["crs"]])

        # define output directory
        tile_dir <- output_mosaic_tile_dir / tile[["tile"]]

        # create dir
        fs::dir_create(tile_dir, recurse = TRUE)

        # mosaic
        sits::sits_mosaic(
            cube       = tile,
            multicores = multicores,
            output_dir = tile_dir,
            crs        = tile_crs
        )
    })

    print("Finished - tiles are now ready")

    # create output dir to the whole area
    fs::dir_create(output_mosaic_complete_dir, recurse = TRUE)

    # get mosaic timeline
    mosaic_timeline <- sits::sits_timeline(mosaic)

    # merge mosaic file infos
    mosaic_files <- dplyr::bind_rows(mosaic[["file_info"]])

    # mosaic by date
    mosaic_files <- purrr::map_vec(mosaic_timeline, function(timeline_date) {
        # define output file
        mosaic_file <- output_mosaic_complete_dir / paste0(timeline_date, ".tif")

        # if file exists, return it
        if (fs::file_exists(mosaic_file)) {
            # create mbtiles if needed
            mosaic_mbtiles <- output_mosaic_complete_dir / paste0(timeline_date, ".mbtiles")
            # generate mbtiles if file doesn't exist
            if (mbtiles && !fs::file_exists(mosaic_mbtiles)) {
                system(paste(
                    "gdal_translate -of MBTILES",
                    mosaic_file,
                    mosaic_mbtiles,
                    sep = " "
                ))
                # generate cogs
                system(paste("gdaladdo -r average ", mosaic_file, "2 4 8 16 32", sep = " "))
                # update mosaic file name
                mosaic_file <- mosaic_mbtiles
            }
            return(mosaic_file)
        }

        # filter files by date
        tiles_in_date <- mosaic_files  |>
            dplyr::filter(date == timeline_date) |>
            dplyr::mutate(band = factor(band, levels = bands)) |>
            dplyr::arrange(band)

        # process files by `fid` (assuming 3 bands per fid)
        vrt_files <- dplyr::group_by(tiles_in_date, .data[["fid"]]) |>
            dplyr::group_map(function(group_fid, key) {
                # define vrt file path
                vrt_file <- fs::path(paste0(fs::file_temp(), ".vrt"))

                # create vrt
                sf::gdal_utils(
                    util = "buildvrt",
                    source = group_fid[["path"]],
                    destination = vrt_file,
                    options = c("-separate")
                )

                # return!
                vrt_file
            }) |>
            unlist()

        # define vrt list file
        vrt_files_lst <- fs::file_temp(ext = "txt")

        # write vrt files to list file
        readr::write_lines(vrt_files, file = vrt_files_lst)

        # build vrt (using system as sf was raising errors)
        vrt_merged <- fs::file_temp(ext = "vrt")

        # create vrt file
        system(paste(
            "gdalbuildvrt -input_file_list",
            vrt_files_lst,
            vrt_merged,
            sep = " "
        ))

        # create tif file
        sf::gdal_utils(
            util = "translate",
            source = vrt_merged,
            destination = mosaic_file,
            options = c(
                "-of", "GTiff",
                "-ot", "Byte",
                "-a_nodata", "0",
                "-exponent", "0.7",
                "-scale",
                "-b", "1",
                "-b", "2",
                "-b", "3",
                "-co", "BIGTIFF=YES",
                "-co", "TILED=YES",
                "-co", "COMPRESS=ZSTD",
                "-co", "PREDICTOR=1",
                "-co", "NUM_THREADS=ALL_CPUS"
            ),
            config_options = c(
                "GDAL_CACHEMAX" = "4096"
            ),
            quiet = FALSE
        )

        # warp
        if (!is.null(roi_file)) {
            mosaic_cropped <- output_mosaic_complete_dir / paste0(timeline_date, "-crop.tif")
            sf::gdal_utils(
                util = "warp",
                source = mosaic_file,
                destination = mosaic_cropped,
                options = c(
                    "-cutline", roi_file,
                    "-overwrite",
                    "-crop_to_cutline",
                    "-co", "BIGTIFF=YES",
                    "-co", "TILED=YES",
                    "-co", "COMPRESS=ZSTD",
                    "-co", "PREDICTOR=1",
                    "-co", "NUM_THREADS=ALL_CPUS"
                ),
                config_options = c(
                    "GDAL_CACHEMAX" = "4096"
                ),
                quiet = FALSE
            )

            # Move files
            fs::file_move(
                path     = mosaic_cropped,
                new_path = mosaic_file
            )
        }

        # create mbtiles
        if (mbtiles) {
            mosaic_mbtiles <- output_mosaic_complete_dir / paste0(timeline_date, ".mbtiles")
            system(paste(
                "gdal_translate -of MBTILES",
                mosaic_file,
                mosaic_mbtiles,
                sep = " "
            ))
            # update mosaic file name
            mosaic_file <- mosaic_mbtiles
        }
        system(paste("gdaladdo -r average ", mosaic_file, "2 4 8 16 32", sep = " "))

        # return
        mosaic_file
    })

    # return!
    return(mosaic_files)
}
