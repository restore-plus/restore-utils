
#' @export
cube_generate_indices <- function(cube, output_dir, multicores, memsize) {
    # Generate NDVI
    cube <- sits_apply(
        data       = cube,
        NDVI       = (NIR08 - RED) / (NIR08 + RED),
        output_dir = output_dir,
        multicores = multicores,
        memsize    = memsize,
        progress   = TRUE
    )

    # Generate EVI (https://www.usgs.gov/landsat-missions/landsat-enhanced-vegetation-index)
    cube <- sits_apply(
        data       = cube,
        EVI        = 2.5 * ((NIR08 - RED) / (NIR08 + 6 * RED - 7.5 * BLUE + 1)),
        output_dir = output_dir,
        multicores = multicores,
        memsize    = memsize,
        progress   = TRUE
    )

    # Generate MNDWI
    cube <- sits_apply(
        data       = cube,
        MNDWI      = (GREEN - SWIR16) / (GREEN + SWIR16),
        output_dir = output_dir,
        multicores = multicores,
        memsize    = memsize,
        progress   = TRUE
    )

    # Generate NBR (https://www.usgs.gov/landsat-missions/landsat-normalized-burn-ratio)
    cube <- sits_apply(
        data       = cube,
        NBR        = (NIR08 - SWIR22) / (NIR08 + SWIR22),
        output_dir = output_dir,
        multicores = multicores,
        memsize    = memsize,
        progress   = TRUE
    )

    return(cube)
}

#' @export
cube_to_rgb_mosaic <- function(cube,
                               output_dir,
                               multicores = 64,
                               bands = NULL,
                               roi_file = NULL) {
    # convert output_dir to fs
    output_dir <- fs::path(output_dir)

    # create base output dirs
    output_mosaic_tile_dir <- output_dir / "tiles"
    output_mosaic_complete_dir <- output_dir / "cube"

    # mosaic by tile
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

    # create output dir to the whole area
    fs::dir_create(output_mosaic_complete_dir, recurse = TRUE)

    # get mosaic timeline
    mosaic_timeline <- sits_timeline(mosaic)

    # merge mosaic file infos
    mosaic_files <- dplyr::bind_rows(mosaic[["file_info"]])

    # mosaic by date
    mosaic_files <- purrr::map_vec(mosaic_timeline, function(timeline_date) {
        # define output file
        mosaic_file <- output_mosaic_complete_dir / paste0(timeline_date, ".tif")
        mosaic_mbtiles <- output_mosaic_complete_dir / paste0(timeline_date, ".mbtiles")

        # if file exists, return it
        if (fs::file_exists(mosaic_mbtiles)) {
            return(mosaic_mbtiles)
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

        # translate
        rgb_file <- output_mosaic_complete_dir / "mosaic-rgb.tif"

        system(
            paste(
                "gdal_translate -ot Byte -a_nodata 0 -scale 0 10000 0 255 -b 1 -b 2 -b 3 ",
                vrt_merged,
                rgb_file,
                sep = " "
            )
        )

        # warp
        system(paste(
            "gdalwarp -dstalpha -cutline", roi_file, "-crop_to_cutline", rgb_file, mosaic_file, sep = " "
        ))

        # create mbtiles
        system(paste(
            "gdal_translate -of MBTILES",
            mosaic_file,
            mosaic_mbtiles,
            sep = " "
        ))

        # add mbtiles zoom
        system(paste("gdaladdo -r average ", mosaic_mbtiles, "2 4 8 16 32", sep = " "))

        # delete temp files
        fs::file_delete(rgb_file)

        # return
        mosaic_mbtiles
    })

    # return!
    return(mosaic_files)
}
