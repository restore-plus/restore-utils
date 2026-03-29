.reclassify_perene_filename <- function(version) {
    paste0(version, "-perene-reclass", ".tif")
}

.reclassify_temp_filename <- function(version) {
    paste0(version, "-temp-reclass", ".tif")
}

.reclassify_sits_name <- function(version, year) {
    # Prepare output file
    template_name <- "LANDSAT_OLI_MOSAIC_%d-01-01_%d-01-31_class_%s.tif"
    sprintf(template_name, year, year, version)
}

.reclassify_save_rds <- function(cube, output_dir, version) {
    file <- fs::path(output_dir) / "rds" / paste0(version, ".rds")
    fs::dir_create(fs::path_dir(file))

    saveRDS(cube, file)

    return(cube)
}

#' @export
reclassify_rule0_forest <- function(cube, mask, multicores, memsize, output_dir, version, exclude_mask_na = FALSE) {
    cube <- sits::sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "Forest" = cube == "Forest" | mask == "Vegetação Nativa"
        ),
        exclude_mask_na = exclude_mask_na,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule1_secundary_vegetation <- function(cube, mask, multicores, memsize, output_dir, version, exclude_mask_na = FALSE) {
    cube <- sits::sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "vegetacao_secundaria" = cube == "vegetacao_secundaria" |
                cube %in% c("Forest", "Riparian_Forest", "Mountainside_Forest") &
                mask != "Vegetação Nativa"
        ),
        exclude_mask_na = exclude_mask_na,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule2_current_deforestation <- function(cube, mask, multicores, memsize, output_dir, version, rarg_year, exclude_mask_na = FALSE) {
    # build args for expression
    deforestation_year <- paste0("d", rarg_year)

    # build expression
    rules_expression <- bquote(
        list(
            "deforest_year" = mask == .(deforestation_year)
        )
    )

    # reclassify!
    cube <- eval(bquote(
        sits::sits_reclassify(
            cube = cube,
            mask = mask,
            rules = .(rules_expression),
            exclude_mask_na = exclude_mask_na,
            multicores = multicores,
            memsize = memsize,
            output_dir = output_dir,
            version = version
        )
    ))

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule3_pasture_wetland <- function(cube, mask, multicores, memsize, output_dir, version, rarg_year, exclude_mask_na = FALSE) {
    # build args for expression
    residuals_years <- c()
    if (rarg_year >= 2010) {
        residuals_years <- paste0("r", 2010:rarg_year)
    }
    deforestation_years <- paste0("d", 2000:(rarg_year - 1))
    deforestation_years <- c(deforestation_years, residuals_years)

    # build expression
    rules_expression <- bquote(
        list(
            "Pasture_Wetland" = (
                cube %in% c("Seasonally_Flooded_ICS", "Wetland_ICS") &
                    mask %in% .(deforestation_years)
            )
        )
    )

    # reclassify!
    cube <- eval(bquote(
        sits::sits_reclassify(
            cube = cube,
            mask = mask,
            rules = .(rules_expression),
            exclude_mask_na = exclude_mask_na,
            multicores = multicores,
            memsize = memsize,
            output_dir = output_dir,
            version = version
        )
    ))

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule4_silviculture <- function(cube, mask, multicores, memsize, output_dir, version, exclude_mask_na = FALSE) {
    cube <- sits::sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "Silvicultura" = cube == "Silvicultura" | mask == "SILVICULTURA"
        ),
        exclude_mask_na = exclude_mask_na,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule5_silviculture_pasture <- function(cube, mask, multicores, memsize, output_dir, version, exclude_mask_na = FALSE) {
    cube <- sits::sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "pasto_silvicultura" = cube == "Silvicultura" & mask != "SILVICULTURA"
        ),
        exclude_mask_na = exclude_mask_na,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule6_semiperennial <- function(cube, mask, multicores, memsize, output_dir, version, exclude_mask_na = FALSE) {
    cube <- sits::sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "Agr. Semiperene" = cube == "Agr. Semiperene" | mask == "CULTURA AGRICOLA SEMIPERENE"
        ),
        exclude_mask_na = exclude_mask_na,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule7_semiperennial_pasture <- function(cube, mask, multicores, memsize, output_dir, version, exclude_mask_na = FALSE) {
    cube <- sits::sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "pasto_semiperene" = cube == "Agr. Semiperene" &
                !(mask %in% c("CULTURA AGRICOLA SEMIPERENE",
                              "CULTURA AGRICOLA TEMPORARIA DE 1 CICLO",
                              "CULTURA AGRICOLA TEMPORARIA DE MAIS DE 1 CICLO"))
        ),
        exclude_mask_na = exclude_mask_na,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule8_annual_agriculture <- function(cube, mask, multicores, memsize, output_dir, version, exclude_mask_na = FALSE) {
    cube <- sits::sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "2ciclos" = cube == "2ciclos"  |
                (cube == "Agr. Semiperene" & mask %in% c(
                    "CULTURA AGRICOLA TEMPORARIA DE 1 CICLO",
                    "CULTURA AGRICOLA TEMPORARIA DE MAIS DE 1 CICLO",
                    "CULTURA AGRICOLA TEMPORARIA" # terraclass 2008, 2010
                ))
        ),
        exclude_mask_na = exclude_mask_na,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule8_annual_agriculture_v2 <- function(cube, mask, multicores, memsize, output_dir, version, exclude_mask_na = FALSE) {
    cube <- sits::sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "2ciclos" = cube == "2ciclos" | cube != "2ciclos" & (mask %in% c(
                "CULTURA AGRICOLA TEMPORARIA DE 1 CICLO",
                "CULTURA AGRICOLA TEMPORARIA DE MAIS DE 1 CICLO",
                "CULTURA AGRICOLA TEMPORARIA" # terraclass 2008, 2010
            ))
        ),
        exclude_mask_na = exclude_mask_na,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule9_minning <- function(cube, mask, multicores, memsize, output_dir, version, exclude_mask_na = FALSE) {
    cube <- sits::sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "mineracao" = mask == "MINERACAO"
        ),
        exclude_mask_na = exclude_mask_na,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule10_urban_area <- function(cube, mask, multicores, memsize, output_dir, version, exclude_mask_na = FALSE) {
    cube <- sits::sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "area_urbanizada" = mask == "URBANIZADA"
        ),
        exclude_mask_na = exclude_mask_na,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule11_water <- function(cube, mask, multicores, memsize, output_dir, version, exclude_mask_na = FALSE) {
    cube <- sits::sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "agua" = (
                mask == "CORPO DAGUA" &
                    !cube %in% c("Wetland_ICS", "Seasonally_Flooded_ICS")
            )
        ),
        exclude_mask_na = exclude_mask_na,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule11_water_prodes <- function(cube, mask, multicores, memsize, output_dir, version, exclude_mask_na = FALSE) {
    cube <- sits::sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "agua" = (
                mask == "Hidrografia" &
                    !cube %in% c("Wetland_ICS", "Seasonally_Flooded_ICS")
            )
        ),
        exclude_mask_na = exclude_mask_na,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule12_non_forest <- function(cube, mask, multicores, memsize, output_dir, version, exclude_mask_na = FALSE) {
    cube <- sits::sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "nat_non_forest" = mask == "NAO FLORESTA"
        ),
        exclude_mask_na = exclude_mask_na,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule13_temporal_trajectory_perene <- function(files,
                                                         perene_class_id,
                                                         vs_class_id,
                                                         version,
                                                         multicores,
                                                         memsize,
                                                         output_dir) {
    # Create output directory
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)
    # Define output file
    out_filename <- .reclassify_perene_filename(version)
    out_file <- fs::path(output_dir) / out_filename
    # If result already exists, return it!
    if (file.exists(out_file)) {
        return(out_file)
    }
    # The following functions define optimal parameters for parallel processing
    rast_template <- sits:::.raster_open_rast(files)
    image_size <- list(
        nrows = sits:::.raster_nrows(rast_template),
        ncols = sits:::.raster_ncols(rast_template)
    )
    # Get block size
    block <- sits:::.raster_file_blocksize(sits:::.raster_open_rast(files))
    # Check minimum memory needed to process one block
    job_block_memsize <- sits:::.jobs_block_memsize(
        block_size = sits:::.block_size(block = block, overlap = 0),
        npaths = length(files),
        nbytes = 8,
        proc_bloat = sits:::.conf("processing_bloat")
    )
    # Update multicores parameter based on size of a single block
    multicores <- sits:::.jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter based on the size of memory and number of cores
    block <- sits:::.jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = image_size,
        memsize = memsize,
        multicores = multicores
    )
    # Create chunks
    chunks <- sits:::.chunks_create(
        block = block,
        overlap = 0,
        image_size = image_size,
        image_bbox = sits:::.bbox(
            sits:::.raster_bbox(rast_template),
            default_crs = terra::crs(rast_template)
        )
    )
    # Update chunk to save extra information
    chunks[["files"]] <- rep(list(files), nrow(chunks))
    chunks[["out_filename"]] <- out_filename
    chunks[["vs_class_id"]] <- vs_class_id
    chunks[["perene_class_id"]] <- perene_class_id
    # Start workers
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)
    # Process data!
    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
        # Get chunk block
        block <- sits:::.block(chunk)
        # Get extra context defined by restoreutils
        files <- chunk[["files"]][[1]]
        out_filename <- chunk[["out_filename"]]
        vs_class_id <- chunk[["vs_class_id"]]
        perene_class_id <- chunk[["perene_class_id"]]
        # Define block file name / path
        block_file <- sits:::.file_block_name(
            pattern = tools::file_path_sans_ext(out_filename),
            block = block,
            output_dir = output_dir
        )
        # If block already exists, return it!
        if (file.exists(block_file)) {
            return(block_file)
        }
        # Read raster values
        values <- sits:::.raster_read_rast(files = files, block = block)
        # Process data
        values <- restoreutils:::C_trajectory_transition_analysis(
            data = values,
            reference_class = vs_class_id,
            neighbor_class  = perene_class_id
        )
        values <- restoreutils:::C_trajectory_transition_analysis(
            data = values,
            reference_class = perene_class_id,
            neighbor_class  = vs_class_id
        )
        # Prepare and save results as raster
        sits:::.raster_write_block(
            files = block_file,
            block = block,
            bbox = sits:::.bbox(chunk),
            values = values,
            data_type = "INT1U",
            missing_value = 255,
            crop_block = NULL
        )
        # Free memory
        gc()
        # Returned block file
        block_file
    }, progress = TRUE)
    # Merge raster blocks
    sits:::.raster_merge_blocks(
        out_files = out_file,
        base_file = files,
        block_files = block_files,
        data_type = "INT1U",
        missing_value = 255,
        multicores = multicores
    )
    # Remove block files
    unlink(block_files)
    # Return!
    return(out_file)
}

#' @export
reclassify_rule14_temporal_neighbor_perene <- function(files,
                                                       perene_class_id,
                                                       replacement_class_id,
                                                       version,
                                                       multicores,
                                                       memsize,
                                                       output_dir) {
    # Create output directory
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)
    # Define output file
    out_filename <- .reclassify_perene_filename(version)
    out_file <- fs::path(output_dir) / out_filename
    # If result already exists, return it!
    if (file.exists(out_file)) {
        return(out_file)
    }
    # The following functions define optimal parameters for parallel processing
    rast_template <- sits:::.raster_open_rast(files)
    image_size <- list(
        nrows = sits:::.raster_nrows(rast_template),
        ncols = sits:::.raster_ncols(rast_template)
    )
    # Get block size
    block <- sits:::.raster_file_blocksize(sits:::.raster_open_rast(files))
    # Check minimum memory needed to process one block
    job_block_memsize <- sits:::.jobs_block_memsize(
        block_size = sits:::.block_size(block = block, overlap = 0),
        npaths = length(files),
        nbytes = 8,
        proc_bloat = sits:::.conf("processing_bloat")
    )
    # Update multicores parameter based on size of a single block
    multicores <- sits:::.jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter based on the size of memory and number of cores
    block <- sits:::.jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = image_size,
        memsize = memsize,
        multicores = multicores
    )
    # Create chunks
    chunks <- sits:::.chunks_create(
        block = block,
        overlap = 0,
        image_size = image_size,
        image_bbox = sits:::.bbox(
            sits:::.raster_bbox(rast_template),
            default_crs = terra::crs(rast_template)
        )
    )
    # Update chunk to save extra information
    chunks[["files"]] <- rep(list(files), nrow(chunks))
    chunks[["out_filename"]] <- out_filename
    chunks[["perene_class_id"]] <- perene_class_id
    chunks[["replacement_class_id"]] <- replacement_class_id
    # Start workers
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)
    # Process data!
    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
        # Get chunk block
        block <- sits:::.block(chunk)
        # Get extra context defined by restoreutils
        files <- chunk[["files"]][[1]]
        out_filename <- chunk[["out_filename"]]
        perene_class_id <- chunk[["perene_class_id"]]
        replacement_class_id <- chunk[["replacement_class_id"]]
        # Define block file name / path
        block_file <- sits:::.file_block_name(
            pattern = tools::file_path_sans_ext(out_filename),
            block = block,
            output_dir = output_dir
        )
        # If block already exists, return it!
        if (file.exists(block_file)) {
            return(block_file)
        }
        # Read raster values
        values <- sits:::.raster_read_rast(files = files, block = block)
        # Process data
        values <- restoreutils:::C_trajectory_neighbor_analysis(
            data = values,
            reference_class = perene_class_id,
            replacement_class = replacement_class_id
        )
        # Prepare and save results as raster
        sits:::.raster_write_block(
            files = block_file,
            block = block,
            bbox = sits:::.bbox(chunk),
            values = values,
            data_type = "INT1U",
            missing_value = 255,
            crop_block = NULL
        )
        # Free memory
        gc()
        # Returned block file
        block_file
    }, progress = TRUE)
    # Merge raster blocks
    sits:::.raster_merge_blocks(
        out_files = out_file,
        base_file = files,
        block_files = block_files,
        data_type = "INT1U",
        missing_value = 255,
        multicores = multicores
    )
    # Remove block files
    unlink(block_files)
    # Return!
    return(out_file)
}

#' @export
reclassify_rule15_urban_area_glad <- function(cube,
                                              mask,
                                              reference_mask,
                                              multicores,
                                              memsize,
                                              output_dir,
                                              version,
                                              exclude_mask_na = FALSE) {
    mask_ref <- sits::sits_reclassify(
        cube = mask,
        mask = reference_mask,
        rules = list(
            "URBANIZADA" = cube == "URBANIZADA" & mask == "URBANIZADA"
        ),
        exclude_mask_na = exclude_mask_na,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = paste0(version, "-intermed-reference-mask")
    )

    cube <- sits::sits_reclassify(
        cube = cube,
        mask = mask_ref,
        rules = list(
            "area_urbanizada" = mask == "URBANIZADA"
        ),
        exclude_mask_na = exclude_mask_na,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule16_water_glad <- function(cube, mask, multicores, memsize, output_dir, version, exclude_mask_na = FALSE) {
    cube <- sits::sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "agua" = (
                mask == "CORPO DAGUA" &
                    !cube %in% c("Wetland_ICS", "Seasonally_Flooded_ICS")
            )
        ),
        exclude_mask_na = exclude_mask_na,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule17_semiperennial_glad <- function(cube, mask, multicores, memsize, output_dir, version, exclude_mask_na = FALSE) {
    cube <- sits::sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "pasto_semiperene_2" = cube == "Agr. Semiperene" &
                mask != "CULTURA AGRICOLA SEMIPERENE" # TC 2008
        ),
        exclude_mask_na = exclude_mask_na,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule18_annual_agriculture_glad <- function(cube, mask, multicores, memsize, output_dir, version, exclude_mask_na = FALSE) {
    cube <- sits::sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "2ciclos" = cube == "2ciclos" | cube == "pasto_semiperene_2" &
                mask %in% c("AGRICULTURA_ANUAL", "CULTURA AGRICOLA TEMPORARIA")
        ),
        exclude_mask_na = exclude_mask_na,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule19_perene <- function(cube, mask, multicores, memsize,
                                     output_dir, version, rarg_year,
                                     exclude_mask_na = FALSE) {
    # build args for expression
    terraclass_years <- c(2008, 2010, 2012, 2014, 2018, 2020, 2022, 2024)

    if (!rarg_year %in% terraclass_years) {
        rules_expression <- bquote(
            list(
                "Perene" = ((
                    cube == "vegetacao_secundaria" |
                        cube == "past_arbustiva" |
                        cube == "vs_pasture" |
                        cube == "vs_herbacea_pasture"
                ) & mask == "CULTURA AGRICOLA PERENE")
            )
        )
    } else {
        rules_expression <- bquote(
            list(
                "Perene" = (
                    mask == "CULTURA AGRICOLA PERENE"
                )
            )
        )
    }

    # reclassify!
    cube <- eval(bquote(
        sits::sits_reclassify(
            cube = cube,
            mask = mask,
            rules = .(rules_expression),
            exclude_mask_na = exclude_mask_na,
            multicores = multicores,
            memsize = memsize,
            output_dir = output_dir,
            version = version
        )
    ))

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule20_temporal_trajectory_urban <- function( files,
                                                         files_mask,
                                                         file_out,
                                                         urban_class_id,
                                                         forest_class_id,
                                                         forest_class_id_mask,
                                                         version,
                                                         multicores,
                                                         memsize,
                                                         output_dir) {
    # Create output directory
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)
    # If result already exists, return it!
    if (file.exists(file_out)) {
        return(file_out)
    }
    out_filename <- fs::path_file(file_out)
    # The following functions define optimal parameters for parallel processing
    rast_template <- sits:::.raster_open_rast(files)
    image_size <- list(
        nrows = sits:::.raster_nrows(rast_template),
        ncols = sits:::.raster_ncols(rast_template)
    )
    # Get block size
    block <- sits:::.raster_file_blocksize(sits:::.raster_open_rast(files))
    # Check minimum memory needed to process one block
    job_block_memsize <- sits:::.jobs_block_memsize(
        block_size = sits:::.block_size(block = block, overlap = 0),
        npaths = length(files),
        nbytes = 8,
        proc_bloat = sits:::.conf("processing_bloat")
    )
    # Update multicores parameter based on size of a single block
    multicores <- sits:::.jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter based on the size of memory and number of cores
    block <- sits:::.jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = image_size,
        memsize = memsize,
        multicores = multicores
    )
    # Create chunks
    chunks <- sits:::.chunks_create(
        block = block,
        overlap = 0,
        image_size = image_size,
        image_bbox = sits:::.bbox(
            sits:::.raster_bbox(rast_template),
            default_crs = terra::crs(rast_template)
        )
    )
    # Update chunk to save extra information
    chunks[["output_dir"]] <- output_dir
    chunks[["files"]] <- rep(list(files), nrow(chunks))
    chunks[["files_mask"]] <- rep(list(files_mask), nrow(chunks))
    chunks[["urban_class_id"]] <- urban_class_id
    chunks[["forest_class_id"]] <- forest_class_id
    chunks[["forest_class_id_mask"]] <- forest_class_id_mask
    # Start workers
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)
    # Process data!
    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
        # Get chunk block
        block <- sits:::.block(chunk)
        # Get extra context defined by restoreutils
        output_dir <- chunk[["output_dir"]]
        files <- chunk[["files"]][[1]]
        files_mask <- chunk[["files_mask"]][[1]]
        urban_class_id <- chunk[["urban_class_id"]]
        forest_class_id <- chunk[["forest_class_id"]]
        forest_class_id_mask <- chunk[["forest_class_id_mask"]]
        # Define block file name / path
        block_file <- sits:::.file_block_name(
            pattern = tools::file_path_sans_ext(out_filename),
            block = block,
            output_dir = output_dir
        )
        # If block already exists, return it!
        if (file.exists(block_file)) {
            return(block_file)
        }
        # Read raster values
        values <- sits:::.raster_read_rast(
            files = files,
            block = block
        )
        values_mask <- sits:::.raster_read_rast(
            files = files_mask,
            block = block
        )
        # Process data
        values <- restoreutils:::C_trajectory_urban_analysis(
            data                 = values,
            mask                 = values_mask,
            urban_class_id       = urban_class_id,
            forest_class_id      = forest_class_id,
            forest_class_id_mask = forest_class_id_mask
        )
        # Prepare and save results as raster
        sits:::.raster_write_block(
            files = block_file,
            block = block,
            bbox = sits:::.bbox(chunk),
            values = values,
            data_type = "INT1U",
            missing_value = 255,
            crop_block = NULL
        )
        # Free memory
        gc()
        # Returned block file
        block_file
    }, progress = TRUE)
    # Merge raster blocks
    sits:::.raster_merge_blocks(
        out_files = file_out,
        base_file = files,
        block_files = block_files,
        data_type = "INT1U",
        missing_value = 255,
        multicores = multicores
    )
    # Remove block files
    unlink(block_files)
    # Return!
    return(file_out)
}

#' @export
reclassify_rule21_pasture_annual_agriculture <- function(cube, mask, multicores, memsize, output_dir, version, rarg_year, exclude_mask_na = FALSE) {
    if (rarg_year == 2022) {
        rules_expression <- bquote(
            list(
                # "Nao-urbano" vem da máscara modificada do terraclass
                "pasture_annual_agriculture" = mask == "URBANIZADA" & cube == "2ciclos"
            )
        )
    } else {
        rules_expression <- bquote(
            list(
                # "Nao-urbano" vem da máscara modificada do terraclass
                # Blending urbanizada with NAO-URBANO we got the original Urbanizada
                "pasture_annual_agriculture" = mask %in% c("URBANIZADA", "NAO-URBANO") & cube == "2ciclos"
            )
        )
    }

    # reclassify!
    cube <- eval(bquote(
        sits::sits_reclassify(
            cube = cube,
            mask = mask,
            rules = .(rules_expression),
            exclude_mask_na = exclude_mask_na,
            multicores = multicores,
            memsize = memsize,
            output_dir = output_dir,
            version = version
        )
    ))

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule22_temporal_annual_agriculture <- function(files,
                                                          annual_agriculture_class_id,
                                                          target_class_map,
                                                          version,
                                                          multicores,
                                                          memsize,
                                                          output_dir) {
    # Create output directory
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)
    # Define output file
    out_filename <- .reclassify_temp_filename(version)
    out_file <- fs::path(output_dir) / out_filename
    # If result already exists, return it!
    if (file.exists(out_file)) {
        return(out_file)
    }
    # The following functions define optimal parameters for parallel processing
    rast_template <- sits:::.raster_open_rast(files)
    image_size <- list(
        nrows = sits:::.raster_nrows(rast_template),
        ncols = sits:::.raster_ncols(rast_template)
    )
    # Get block size
    block <- sits:::.raster_file_blocksize(sits:::.raster_open_rast(files))
    # Check minimum memory needed to process one block
    job_block_memsize <- sits:::.jobs_block_memsize(
        block_size = sits:::.block_size(block = block, overlap = 0),
        npaths = length(files),
        nbytes = 8,
        proc_bloat = sits:::.conf("processing_bloat")
    )
    # Update multicores parameter based on size of a single block
    multicores <- sits:::.jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter based on the size of memory and number of cores
    block <- sits:::.jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = image_size,
        memsize = memsize,
        multicores = multicores
    )
    # Create chunks
    chunks <- sits:::.chunks_create(
        block = block,
        overlap = 0,
        image_size = image_size,
        image_bbox = sits:::.bbox(
            sits:::.raster_bbox(rast_template),
            default_crs = terra::crs(rast_template)
        )
    )
    # Update chunk to save extra information
    chunks[["files"]] <- rep(list(files), nrow(chunks))
    chunks[["out_filename"]] <- out_filename
    chunks[["annual_agriculture_class_id"]] <- annual_agriculture_class_id
    chunks[["target_class_map"]] <- rep(list(target_class_map), nrow(chunks))
    # Start workers
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)
    # Process data!
    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
        # Get chunk block
        block <- sits:::.block(chunk)
        # Get extra context defined by restoreutils
        files <- chunk[["files"]][[1]]
        out_filename <- chunk[["out_filename"]]
        target_class_map <- chunk[["target_class_map"]][[1]]
        annual_agriculture_class_id <- chunk[["annual_agriculture_class_id"]]
        # Define block file name / path
        block_file <- sits:::.file_block_name(
            pattern = tools::file_path_sans_ext(out_filename),
            block = block,
            output_dir = output_dir
        )
        # If block already exists, return it!
        if (file.exists(block_file)) {
            return(block_file)
        }
        # Read raster values
        values <- sits:::.raster_read_rast(files = files, block = block)
        # Process data
        # // This rule was originally implemented to:
        # // > "Se tem agr anual, qualquer classe, agr anual, vira agr anual (2ciclos)"
        values <- restoreutils:::C_trajectory_neighbor_consistency_analysis(
            data = values,
            reference_class = annual_agriculture_class_id
        )
        # // This rule was originally implemented to:
        # // > "Se temos classe x, ag anual (2ciclos), classe x, o valor do meio (ag anual), vira classe x"
        values <- restoreutils:::C_trajectory_neighbor_majority_analysis(
            data = values,
            reference_class = annual_agriculture_class_id,
            target_class_map = target_class_map
        )
        # Prepare and save results as raster
        sits:::.raster_write_block(
            files = block_file,
            block = block,
            bbox = sits:::.bbox(chunk),
            values = values,
            data_type = "INT1U",
            missing_value = 255,
            crop_block = NULL
        )
        # Free memory
        gc()
        # Returned block file
        block_file
    }, progress = TRUE)
    # Merge raster blocks
    sits:::.raster_merge_blocks(
        out_files = out_file,
        base_file = files,
        block_files = block_files,
        data_type = "INT1U",
        missing_value = 255,
        multicores = multicores
    )
    # Remove block files
    unlink(block_files)
    # Return!
    return(out_file)
}

#' @export
reclassify_rule23_pasture_deforestation_in_nonforest <- function(cube, mask, multicores, memsize, output_dir, version, exclude_mask_na = FALSE) {
    cube <- sits::sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "pasture_deforestation_in_nonforest" = (
                mask == "DeforestationInNonForest" &
                    cube %in% c("Seasonally_Flooded_ICS", "Wetland_ICS")
            )
        ),
        exclude_mask_na = exclude_mask_na,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule24_temporal_water_consistency <- function(files,
                                                         water_class_id,
                                                         target_class_map,
                                                         year,
                                                         version,
                                                         multicores,
                                                         memsize,
                                                         output_dir,
                                                         excluded_values = NULL) {
    # Create output directory
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)
    # Define output file
    out_filename <- .reclassify_sits_name(version, year)

    out_file <- fs::path(output_dir) / out_filename
    # If result already exists, return it!
    if (file.exists(out_file)) {
        return(out_file)
    }
    # The following functions define optimal parameters for parallel processing
    rast_template <- sits:::.raster_open_rast(files)
    image_size <- list(
        nrows = sits:::.raster_nrows(rast_template),
        ncols = sits:::.raster_ncols(rast_template)
    )
    # Get block size
    block <- sits:::.raster_file_blocksize(sits:::.raster_open_rast(files))
    # Check minimum memory needed to process one block
    job_block_memsize <- sits:::.jobs_block_memsize(
        block_size = sits:::.block_size(block = block, overlap = 0),
        npaths = length(files),
        nbytes = 8,
        proc_bloat = sits:::.conf("processing_bloat")
    )
    # Update multicores parameter based on size of a single block
    multicores <- sits:::.jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter based on the size of memory and number of cores
    block <- sits:::.jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = image_size,
        memsize = memsize,
        multicores = multicores
    )
    # Create chunks
    chunks <- sits:::.chunks_create(
        block = block,
        overlap = 0,
        image_size = image_size,
        image_bbox = sits:::.bbox(
            sits:::.raster_bbox(rast_template),
            default_crs = terra::crs(rast_template)
        )
    )
    # Update chunk to save extra information
    chunks[["files"]] <- rep(list(files), nrow(chunks))
    chunks[["out_filename"]] <- out_filename
    chunks[["water_class_id"]] <- water_class_id
    chunks[["target_class_map"]] <- rep(list(target_class_map), nrow(chunks))
    # If excluded_values is NULL, set it to an empty integer vector
    if (is.null(excluded_values)) {
        excluded_values <- integer(-1L)
    }
    chunks[["excluded_values"]] <- rep(list(excluded_values), nrow(chunks))
    # Start workers
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)
    # Process data!
    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
        # Get chunk block
        block <- sits:::.block(chunk)
        # Get extra context defined by restoreutils
        files <- chunk[["files"]][[1]]
        out_filename <- chunk[["out_filename"]]
        water_class_id <- chunk[["water_class_id"]]
        target_class_map <- chunk[["target_class_map"]][[1]]
        excluded_values <- chunk[["excluded_values"]][[1]]
        # Define block file name / path
        block_file <- sits:::.file_block_name(
            pattern = tools::file_path_sans_ext(out_filename),
            block = block,
            output_dir = output_dir
        )
        # If block already exists, return it!
        if (file.exists(block_file)) {
            return(block_file)
        }
        # Read raster values
        values <- sits:::.raster_read_rast(files = files, block = block)
        # Process data
        values <- restoreutils:::C_trajectory_water_analysis(
            data = values,
            water_class = water_class_id,
            target_class_map = target_class_map,
            excluded_values = excluded_values
        )
        # Prepare and save results as raster
        sits:::.raster_write_block(
            files = block_file,
            block = block,
            bbox = sits:::.bbox(chunk),
            values = values[, 2],
            data_type = "INT1U",
            missing_value = 255,
            crop_block = NULL
        )
        # Free memory
        gc()
        # Returned block file
        block_file
    }, progress = TRUE)
    # Merge raster blocks
    sits:::.raster_merge_blocks(
        out_files = out_file,
        base_file = files,
        block_files = block_files,
        data_type = "INT1U",
        missing_value = 255,
        multicores = multicores
    )
    # Remove block files
    unlink(block_files)
    # Return!
    return(out_file)
}

#' @export
reclassify_rule25_static_water_mask <- function(cube, mask, multicores, memsize, output_dir, version, exclude_mask_na = FALSE) {
    cube <- sits::sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "agua" = cube %in% c("Wetland_ICS", "Seasonally_Flooded_ICS") & mask == "Water"
        ),
        exclude_mask_na = exclude_mask_na,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule26_silviculture_pasture_vs <- function(cube, mask, multicores, memsize, output_dir, version, exclude_mask_na = FALSE) {
    cube <- sits::sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "vegetacao_secundaria" = cube == "pasto_silvicultura" & mask %in% c(
                "VEGETACAO NATURAL FLORESTAL SECUNDARIA", "VEGETACAO_SECUNDARIA"
            )
        ),
        exclude_mask_na = exclude_mask_na,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule27_temporal_trajectory_perene_mask <- function(files,
                                                              files_mask,
                                                              year,
                                                              perene_class_id,
                                                              perene_mask_class_id,
                                                              version,
                                                              multicores,
                                                              memsize,
                                                              output_dir) {
    # Create output directory
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)

    # Define output file
    out_filename <- .reclassify_sits_name(version, year)

    file_out <- fs::path(output_dir) / out_filename
    # If result already exists, return it!
    if (file.exists(file_out)) {
        return(file_out)
    }
    # The following functions define optimal parameters for parallel processing
    rast_template <- sits:::.raster_open_rast(files)
    image_size <- list(
        nrows = sits:::.raster_nrows(rast_template),
        ncols = sits:::.raster_ncols(rast_template)
    )
    # Get block size
    block <- sits:::.raster_file_blocksize(sits:::.raster_open_rast(files))
    # Check minimum memory needed to process one block
    job_block_memsize <- sits:::.jobs_block_memsize(
        block_size = sits:::.block_size(block = block, overlap = 0),
        npaths = length(files) + length(files_mask),
        nbytes = 8,
        proc_bloat = sits:::.conf("processing_bloat")
    )
    # Update multicores parameter based on size of a single block
    multicores <- sits:::.jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter based on the size of memory and number of cores
    block <- sits:::.jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = image_size,
        memsize = memsize,
        multicores = multicores
    )
    # Create chunks
    chunks <- sits:::.chunks_create(
        block = block,
        overlap = 0,
        image_size = image_size,
        image_bbox = sits:::.bbox(
            sits:::.raster_bbox(rast_template),
            default_crs = terra::crs(rast_template)
        )
    )
    # Update chunk to save extra information
    chunks[["output_dir"]] <- output_dir
    chunks[["files"]] <- rep(list(files), nrow(chunks))
    chunks[["files_mask"]] <- rep(list(files_mask), nrow(chunks))
    chunks[["perene_class_id"]] <- perene_class_id
    chunks[["perene_mask_class_id"]] <- perene_mask_class_id
    # Start workers
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)
    # Process data!
    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
        # Get chunk block
        block <- sits:::.block(chunk)
        # Get extra context defined by restoreutils
        output_dir <- chunk[["output_dir"]]
        files <- chunk[["files"]][[1]]
        files_mask <- chunk[["files_mask"]][[1]]
        perene_class_id <- chunk[["perene_class_id"]]
        perene_mask_class_id <- chunk[["perene_mask_class_id"]]
        # Define block file name / path
        block_file <- sits:::.file_block_name(
            pattern = tools::file_path_sans_ext(out_filename),
            block = block,
            output_dir = output_dir
        )
        # If block already exists, return it!
        if (file.exists(block_file)) {
            return(block_file)
        }
        # Output mask file name
        mask_block_file1 <- sits:::.file_block_name(
            pattern = sits:::.file_pattern(file_out, suffix = "_mask1"),
            block = block, output_dir = output_dir
        )

        mask_block_file2 <- sits:::.file_block_name(
            pattern = sits:::.file_pattern(file_out, suffix = "_mask2"),
            block = block, output_dir = output_dir
        )

        # Project mask block to template block
        # Get band conf missing value
        band_conf <- sits:::.conf_derived_band(
            derived_class = "class_cube", band = "class"
        )
        # Create template block for mask
        sits:::.gdal_template_block(
            block = block, bbox = sits:::.bbox(chunk), file = mask_block_file1,
            nlayers = 1L, miss_value = sits:::.miss_value(band_conf),
            data_type = sits:::.data_type(band_conf)
        )
        sits:::.gdal_template_block(
            block = block, bbox = sits:::.bbox(chunk), file = mask_block_file2,
            nlayers = 1L, miss_value = sits:::.miss_value(band_conf),
            data_type = sits:::.data_type(band_conf)
        )
        # Copy values from mask cube into mask template
        sits:::.gdal_merge_into(
            file = mask_block_file1,
            base_files = files_mask[[1]], multicores = 1L
        )
        # Copy values from mask cube into mask template
        sits:::.gdal_merge_into(
            file = mask_block_file2,
            base_files = files_mask[[2]], multicores = 1L
        )

        # Read raster values
        values <- sits:::.raster_read_rast(
            files = files,
            block = block
        )
        values_mask <- sits:::.raster_read_rast(
            files = c(mask_block_file1, mask_block_file2),
            block = NULL
        )
        # Process data
        values <- restoreutils:::C_trajectory_neighbor_consistency_analysis_with_mask(
            data        = values,
            mask        = values_mask,
            data_class  = perene_class_id,
            mask_class  = perene_mask_class_id
        )
        # Prepare and save results as raster
        sits:::.raster_write_block(
            files = block_file,
            block = block,
            bbox = sits:::.bbox(chunk),
            values = values,
            data_type = "INT1U",
            missing_value = 255,
            crop_block = NULL
        )
        # Free memory
        gc()
        # Unlink files
        unlink(c(mask_block_file1, mask_block_file2))
        # Returned block file
        block_file
    }, progress = TRUE)
    # Merge raster blocks
    sits:::.raster_merge_blocks(
        out_files = file_out,
        base_file = files,
        block_files = block_files,
        data_type = "INT1U",
        missing_value = 255,
        multicores = multicores
    )
    # Remove block files
    unlink(block_files)
    # Return!
    return(file_out)
}

#' @export
reclassify_rule28_secundary_vegetation_tc <- function(cube, mask, multicores, memsize,
                                                      output_dir, version, rarg_year, exclude_mask_na = FALSE) {
    # build args for expression
    terraclass_years <- c(2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024)

    if (rarg_year %in% terraclass_years) {
        cube <- sits::sits_reclassify(
            cube = cube,
            mask = mask,
            rules = list(
                "vs_herbacea_pasture" = cube == "vegetacao_secundaria" & mask == "PASTAGEM HERBACEA"
            ),
            exclude_mask_na = exclude_mask_na,
            multicores = multicores,
            memsize = memsize,
            output_dir = output_dir,
            version = glue::glue("{version}-intermediate")
        )

        cube <- sits::sits_reclassify(
            cube = cube,
            mask = mask,
            rules = list(
                "vs_pasture" = cube == "vegetacao_secundaria" & mask != "VEGETACAO NATURAL FLORESTAL SECUNDARIA"
            ),
            exclude_mask_na = exclude_mask_na,
            multicores = multicores,
            memsize = memsize,
            output_dir = output_dir,
            version = version
        )
    }

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule29_temporal_trajectory_vs_pasture <- function(files,
                                                             year,
                                                             vs_class_id,
                                                             pasture_class_id,
                                                             target_class_id,
                                                             version,
                                                             multicores,
                                                             memsize,
                                                             output_dir) {
    # Create output directory
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)

    # Define output file
    out_filename <- .reclassify_sits_name(version, year)

    file_out <- fs::path(output_dir) / out_filename
    # If result already exists, return it!
    if (file.exists(file_out)) {
        return(file_out)
    }
    # The following functions define optimal parameters for parallel processing
    rast_template <- sits:::.raster_open_rast(files)
    image_size <- list(
        nrows = sits:::.raster_nrows(rast_template),
        ncols = sits:::.raster_ncols(rast_template)
    )
    # Get block size
    block <- sits:::.raster_file_blocksize(sits:::.raster_open_rast(files))
    # Check minimum memory needed to process one block
    job_block_memsize <- sits:::.jobs_block_memsize(
        block_size = sits:::.block_size(block = block, overlap = 0),
        npaths = length(files),
        nbytes = 8,
        proc_bloat = sits:::.conf("processing_bloat")
    )
    # Update multicores parameter based on size of a single block
    multicores <- sits:::.jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter based on the size of memory and number of cores
    block <- sits:::.jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = image_size,
        memsize = memsize,
        multicores = multicores
    )
    # Create chunks
    chunks <- sits:::.chunks_create(
        block = block,
        overlap = 0,
        image_size = image_size,
        image_bbox = sits:::.bbox(
            sits:::.raster_bbox(rast_template),
            default_crs = terra::crs(rast_template)
        )
    )
    # Update chunk to save extra information
    chunks[["output_dir"]] <- output_dir
    chunks[["files"]] <- rep(list(files), nrow(chunks))
    chunks[["vs_class_id"]] <- vs_class_id
    chunks[["pasture_class_id"]] <- pasture_class_id
    chunks[["target_class_id"]] <- target_class_id
    # Start workers
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)
    # Process data!
    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
        # Get chunk block
        block <- sits:::.block(chunk)
        # Get extra context defined by restoreutils
        output_dir <- chunk[["output_dir"]]
        files <- chunk[["files"]][[1]]
        vs_class_id <- chunk[["vs_class_id"]]
        target_class_id <- chunk[["target_class_id"]]
        pasture_class_id <- unlist(chunk[["pasture_class_id"]])
        # Define block file name / path
        block_file <- sits:::.file_block_name(
            pattern = tools::file_path_sans_ext(out_filename),
            block = block,
            output_dir = output_dir
        )
        # If block already exists, return it!
        if (file.exists(block_file)) {
            return(block_file)
        }

        # Read raster values
        values <- sits:::.raster_read_rast(
            files = files,
            block = block
        )
        # Process data
        values <- restoreutils:::C_trajectory_vs_analysis(
            data          = values,
            vs_class      = vs_class_id,
            pasture_class = pasture_class_id,
            target_class  = target_class_id
        )
        # Prepare and save results as raster
        sits:::.raster_write_block(
            files = block_file,
            block = block,
            bbox = sits:::.bbox(chunk),
            values = values[, 2],
            data_type = "INT1U",
            missing_value = 255,
            crop_block = NULL
        )
        # Free memory
        gc()
        # Returned block file
        block_file
    }, progress = TRUE)
    # Merge raster blocks
    sits:::.raster_merge_blocks(
        out_files = file_out,
        base_file = files,
        block_files = block_files,
        data_type = "INT1U",
        missing_value = 255,
        multicores = multicores
    )
    # Remove block files
    unlink(block_files)
    # Return!
    return(file_out)
}

#' @export
reclassify_rule30_control_forest_under_2008 <- function(cube, multicores, memsize,
                                                        output_dir, version, rarg_year, exclude_mask_na = FALSE) {
    # build args for expression
    valid_years <- 2000:2007

    if (rarg_year %in% valid_years) {
        # Load prodes 2008
        prodes_2008 <- load_prodes_2008(multicores = multicores, memsize = memsize)

        # Reclassify!
        cube <- sits::sits_reclassify(
            cube = cube,
            mask = prodes_2008,
            rules      = list(
                "Forest" = cube %in% c("Forest", "Mountainside_Forest", "Riparian_Forest") |
                    mask %in% c("Vegetação Nativa", "d2008")
            ),
            exclude_mask_na = exclude_mask_na,
            multicores = multicores,
            memsize = memsize,
            output_dir = output_dir,
            version = version
        )
    }

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule31_cropand_pasture <- function(files,
                                              annual_agriculture_class_id,
                                              target_class,
                                              version,
                                              multicores,
                                              memsize,
                                              output_dir) {
    # Create output directory
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)
    # Define output file
    out_filename <- .reclassify_temp_filename(version)
    out_file <- fs::path(output_dir) / out_filename
    # If result already exists, return it!
    if (file.exists(out_file)) {
        return(out_file)
    }
    # The following functions define optimal parameters for parallel processing
    rast_template <- sits:::.raster_open_rast(files)
    image_size <- list(
        nrows = sits:::.raster_nrows(rast_template),
        ncols = sits:::.raster_ncols(rast_template)
    )
    # Get block size
    block <- sits:::.raster_file_blocksize(sits:::.raster_open_rast(files))
    # Check minimum memory needed to process one block
    job_block_memsize <- sits:::.jobs_block_memsize(
        block_size = sits:::.block_size(block = block, overlap = 0),
        npaths = length(files),
        nbytes = 8,
        proc_bloat = sits:::.conf("processing_bloat")
    )
    # Update multicores parameter based on size of a single block
    multicores <- sits:::.jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter based on the size of memory and number of cores
    block <- sits:::.jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = image_size,
        memsize = memsize,
        multicores = multicores
    )
    # Create chunks
    chunks <- sits:::.chunks_create(
        block = block,
        overlap = 0,
        image_size = image_size,
        image_bbox = sits:::.bbox(
            sits:::.raster_bbox(rast_template),
            default_crs = terra::crs(rast_template)
        )
    )
    # Update chunk to save extra information
    chunks[["files"]] <- rep(list(files), nrow(chunks))
    chunks[["out_filename"]] <- out_filename
    chunks[["annual_agriculture_class_id"]] <- annual_agriculture_class_id
    chunks[["target_class"]] <- target_class
    # Start workers
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)
    # Process data!
    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
        # Get chunk block
        block <- sits:::.block(chunk)
        # Get extra context defined by restoreutils
        files <- chunk[["files"]][[1]]
        out_filename <- chunk[["out_filename"]]
        target_class <- chunk[["target_class"]][[1]]
        annual_agriculture_class_id <- chunk[["annual_agriculture_class_id"]]
        # Define block file name / path
        block_file <- sits:::.file_block_name(
            pattern = tools::file_path_sans_ext(out_filename),
            block = block,
            output_dir = output_dir
        )
        # If block already exists, return it!
        if (file.exists(block_file)) {
            return(block_file)
        }
        # Read raster values
        values <- sits:::.raster_read_rast(files = files, block = block)
        # // This rule was originally implemented to:
        # // > "Se temos qualquer pastagem, ag anual (2ciclos), qualquer pastagem, o valor do meio (ag anual), vira pastagem"
        values <- restoreutils:::C_trajectory_neighbor_majority_analysis_target(
            data = values,
            reference_class = annual_agriculture_class_id,
            target_class = target_class
        )
        # Prepare and save results as raster
        sits:::.raster_write_block(
            files = block_file,
            block = block,
            bbox = sits:::.bbox(chunk),
            values = values,
            data_type = "INT1U",
            missing_value = 255,
            crop_block = NULL
        )
        # Free memory
        gc()
        # Returned block file
        block_file
    }, progress = TRUE)
    # Merge raster blocks
    sits:::.raster_merge_blocks(
        out_files = out_file,
        base_file = files,
        block_files = block_files,
        data_type = "INT1U",
        missing_value = 255,
        multicores = multicores
    )
    # Remove block files
    unlink(block_files)
    # Return!
    return(out_file)
}

#' @export
reclassify_rule32_deforestation_consistency <- function(files,
                                                        deforestation_id,
                                                        target_id,
                                                        version,
                                                        multicores,
                                                        memsize,
                                                        output_dir) {
    # // > "Se temos desmatamento no ano x, todos os anos 1:x-1 deverao ser floresta"
    .reclassify_temporal_consistency_reference(
        files = files,
        reference_id = deforestation_id,
        target_id = target_id,
        version = version,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir
    )
}

#' @export
reclassify_rule33_forest_consistency <- function(files,
                                                 forest_id,
                                                 version,
                                                 multicores,
                                                 memsize,
                                                 output_dir) {
    # // > "Se temos floresta no ano x, todos os anos 1:x-1 deverao ser floresta"
    .reclassify_temporal_consistency_reference(
        files        = files,
        reference_id = forest_id,
        target_id    = forest_id,
        version      = version,
        multicores   = multicores,
        memsize      = memsize,
        output_dir   = output_dir
    )
}

#' @export
reclassify_rule34_cropland_consistency_tc <- function(cube,
                                                      mask,
                                                      roi,
                                                      multicores,
                                                      memsize,
                                                      output_dir,
                                                      version,
                                                      rarg_year,
                                                      exclude_mask_na = FALSE) {
    # build args for expression
    terraclass_years <- c(2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024)

    stopifnot(all(sits::sits_labels(cube) %in% labels_amazon_mcti()))

    if (rarg_year %in% terraclass_years) {
        rules_expression <- bquote(
            list(
                "Pastagem" = (
                    cube == "Agricultura anual" & (
                        mask == "PASTAGEM ARBUSTIVA/ARBOREA" |
                            mask == "PASTAGEM HERBACEA"
                    )
                ),
                "Vegetação secundária" = (
                    cube == "Agricultura anual" &
                        mask == "VEGETACAO NATURAL FLORESTAL SECUNDARIA"
                ),
                "Silvicultura" = (
                    cube == "Agricultura anual" & mask == "SILVICULTURA"
                ),
                "Agricultura semi-perene" = (
                    cube == "Agricultura anual" & mask == "CULTURA AGRICOLA SEMIPERENE"
                ),
                "Agricultura anual" = (
                    cube != "Agricultura anual" & mask %in% c(
                        "CULTURA AGRICOLA TEMPORARIA",
                        "CULTURA AGRICOLA TEMPORARIA DE 1 CICLO",
                        "CULTURA AGRICOLA TEMPORARIA DE MAIS DE 1 CICLO"
                    )
                )
            )
        )

        # reclassify!
        cube <- eval(bquote(
            sits::sits_reclassify(
                cube = cube,
                mask = mask,
                rules = .(rules_expression),
                exclude_mask_na = exclude_mask_na,
                roi = roi,
                multicores = multicores,
                memsize = memsize,
                output_dir = output_dir,
                version = version
            )
        ))
    }

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule35_cropland_transitions <- function(files,
                                                   source_classes,
                                                   target_classes,
                                                   cropland_id,
                                                   pasture_id,
                                                   version,
                                                   multicores,
                                                   memsize,
                                                   output_dir) {
    # Create output directory
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)
    # Define output file
    out_filename <- .reclassify_temp_filename(version)
    out_file <- fs::path(output_dir) / out_filename
    # If result already exists, return it!
    if (file.exists(out_file)) {
        return(out_file)
    }
    # The following functions define optimal parameters for parallel processing
    rast_template <- sits:::.raster_open_rast(files)
    image_size <- list(
        nrows = sits:::.raster_nrows(rast_template),
        ncols = sits:::.raster_ncols(rast_template)
    )
    # Get block size
    block <- sits:::.raster_file_blocksize(sits:::.raster_open_rast(files))
    # Check minimum memory needed to process one block
    job_block_memsize <- sits:::.jobs_block_memsize(
        block_size = sits:::.block_size(block = block, overlap = 0),
        npaths = length(files),
        nbytes = 8,
        proc_bloat = sits:::.conf("processing_bloat")
    )
    # Update multicores parameter based on size of a single block
    multicores <- sits:::.jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter based on the size of memory and number of cores
    block <- sits:::.jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = image_size,
        memsize = memsize,
        multicores = multicores
    )
    # Create chunks
    chunks <- sits:::.chunks_create(
        block = block,
        overlap = 0,
        image_size = image_size,
        image_bbox = sits:::.bbox(
            sits:::.raster_bbox(rast_template),
            default_crs = terra::crs(rast_template)
        )
    )
    # Update chunk to save extra information
    chunks[["files"]] <- rep(list(files), nrow(chunks))
    chunks[["out_filename"]] <- out_filename
    chunks[["source_classes"]] <- rep(list(source_classes), nrow(chunks))
    chunks[["target_classes"]] <- rep(list(target_classes), nrow(chunks))
    chunks[["cropland_id"]] <- cropland_id
    chunks[["pasture_id"]] <- pasture_id
    # Start workers
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)
    # Process data!
    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
        # Get chunk block
        block <- sits:::.block(chunk)
        # Get extra context defined by restoreutils
        files <- chunk[["files"]][[1]]
        out_filename <- chunk[["out_filename"]]
        source_classes <- chunk[["source_classes"]][[1]]
        target_classes <- chunk[["target_classes"]][[1]]
        cropland_id <- chunk[["cropland_id"]]
        pasture_id <- chunk[["pasture_id"]]
        # Define block file name / path
        block_file <- sits:::.file_block_name(
            pattern = tools::file_path_sans_ext(out_filename),
            block = block,
            output_dir = output_dir
        )
        # If block already exists, return it!
        if (file.exists(block_file)) {
            return(block_file)
        }
        # Read raster values
        values <- sits:::.raster_read_rast(files = files, block = block)
        # // This rule was originally implemented to:
        # // > "Se temos pastagem agricultura e desmatamento, ag anual (2ciclos), qualquer pastagem agricultura e desmatamento, o valor do meio (ag anual), vira pastagem"
        values <- restoreutils:::C_trajectory_cropland_transitions(
            data = values,
            source_classes = source_classes,
            target_classes = target_classes,
            cropland_id = cropland_id,
            pasture_id = pasture_id
        )
        # Get only middle year
        values <- values[,2]
        # Prepare and save results as raster
        sits:::.raster_write_block(
            files = block_file,
            block = block,
            bbox = sits:::.bbox(chunk),
            values = values,
            data_type = "INT1U",
            missing_value = 255,
            crop_block = NULL
        )
        # Free memory
        gc()
        # Returned block file
        block_file
    }, progress = TRUE)
    # Merge raster blocks
    sits:::.raster_merge_blocks(
        out_files = out_file,
        base_file = files,
        block_files = block_files,
        data_type = "INT1U",
        missing_value = 255,
        multicores = multicores
    )
    # Remove block files
    unlink(block_files)
    # Return!
    return(out_file)
}

#' @export
reclassify_rule36_wetlands_cleaning <- function(cube,
                                                reference_mask,
                                                roi,
                                                multicores,
                                                memsize,
                                                output_dir,
                                                version,
                                                rarg_year,
                                                exclude_mask_na = FALSE) {
    # build args for expression
    terraclass_years <- c(2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024)

    # stopifnot(all(sits::sits_labels(cube) %in% labels_amazon_mcti()))

    if (rarg_year %in% terraclass_years) {
        # Load currently TerraClass
        tc <- get(paste0("load_terraclass_", rarg_year))
        tc <- tc(
            multicores = multicores,
            memsize    = memsize
        )
        # Create a temporary version
        temp_version <- paste0(version, "-temp")
        temp_cube <- sits::sits_reclassify(
            cube = cube,
            mask = reference_mask,
            rules = list(
                "Invalid-Wetlands" = cube == "Sazonalmente inundada" &
                    !(mask == "Sazonalmente inundada" | mask == "Água")
            ),
            exclude_mask_na = exclude_mask_na,
            roi = roi,
            multicores = multicores,
            memsize = memsize,
            output_dir = output_dir,
            version = temp_version
        )
        # Apply rules for cleaning
        rules_expression <- bquote(
            list(
                "Área urbanizada" = c(
                    cube == "Invalid-Wetlands" &
                        mask == "URBANIZADA"
                ),
                "Mineração" = c(
                    cube == "Invalid-Wetlands" &
                        mask == "MINERACAO"
                ),
                "Agricultura Perene" = c(
                    cube == "Invalid-Wetlands" &
                        mask == "CULTURA AGRICOLA PERENE"
                ),
                "Vegetação secundária" = c(
                    cube == "Invalid-Wetlands" &
                        mask == "VEGETACAO NATURAL FLORESTAL SECUNDARIA"
                ),
                "Floresta" = c(
                    cube == "Invalid-Wetlands" &
                        mask == "VEGETACAO NATURAL FLORESTAL PRIMARIA"
                ),
                "Água" = c(
                    cube == "Invalid-Wetlands" & mask == "CORPO DAGUA"
                ),
                "Pastagem" = (
                    cube == "Invalid-Wetlands" & (
                        mask == "PASTAGEM ARBUSTIVA/ARBOREA" |
                            mask == "PASTAGEM HERBACEA"
                    )
                ),
                "Vegetação secundária" = (
                    cube == "Invalid-Wetlands" &
                        mask == "VEGETACAO NATURAL FLORESTAL SECUNDARIA"
                ),
                "Silvicultura" = (
                    cube == "Invalid-Wetlands" & mask == "SILVICULTURA"
                ),
                "Agricultura semi-perene" = (
                    cube == "Invalid-Wetlands" & mask == "CULTURA AGRICOLA SEMIPERENE"
                ),
                "Agricultura anual" = (
                    cube == "Invalid-Wetlands" & mask %in% c(
                        "CULTURA AGRICOLA TEMPORARIA",
                        "CULTURA AGRICOLA TEMPORARIA DE 1 CICLO",
                        "CULTURA AGRICOLA TEMPORARIA DE MAIS DE 1 CICLO"
                    )
                )
            )
        )

        # reclassify!
        cube <- eval(bquote(
            sits::sits_reclassify(
                cube = temp_cube,
                mask = tc,
                rules = .(rules_expression),
                exclude_mask_na = exclude_mask_na,
                roi = roi,
                multicores = multicores,
                memsize = memsize,
                output_dir = output_dir,
                version = version
            )
        ))
        # Delete temporary mask
        fs::file_delete(temp_cube[["file_info"]][[1]][["path"]])
    } else {
        # Apply rules for cleaning
        rules_expression <- bquote(
            list(
                "Água" = c(
                    cube == "Sazonalmente inundada" & mask == "Água"
                ),
                "Pastagem" = (
                    cube == "Sazonalmente inundada" &
                        (mask == "Pastagem" | mask == "Desmatamento do ano")
                ),
                "Vegetação secundária" = (
                    cube == "Sazonalmente inundada" &
                        mask == "Vegetação secundária"
                ),
                "Silvicultura" = (
                    cube == "Sazonalmente inundada" &
                        mask == "Silvicultura"
                ),
                "Agricultura semi-perene" = (
                    cube == "Sazonalmente inundada" &
                        mask == "Agricultura semi-perene"
                ),
                "Agricultura anual" = (
                    cube == "Sazonalmente inundada" &
                        mask == "Agricultura anual"
                )
            )
        )

        # reclassify!
        cube <- eval(bquote(
            sits::sits_reclassify(
                cube = cube,
                mask = reference_mask,
                rules = .(rules_expression),
                exclude_mask_na = exclude_mask_na,
                roi = roi,
                multicores = multicores,
                memsize = memsize,
                output_dir = output_dir,
                version = version
            )
        ))
    }

    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule37_wetlands_remaining <- function(cube,
                                                 reference_mask,
                                                 roi,
                                                 multicores,
                                                 memsize,
                                                 output_dir,
                                                 version,
                                                 rarg_year,
                                                 exclude_mask_na = FALSE) {
    if (rarg_year == 2024) {
        # Apply rules for cleaning
        rules_expression <- bquote(
            list(
                "Agricultura anual" = (
                    cube == "Invalid-Wetlands" &
                        mask == "Agricultura anual"
                ),
                "Agricultura semi-perene" = (
                    cube == "Invalid-Wetlands" &
                        mask == "Agricultura semi-perene"
                ),
                "Água" = c(
                    cube == "Invalid-Wetlands" &
                        mask == "Água"
                ),
                "Floresta" = c(
                    cube == "Invalid-Wetlands" &
                        mask == "Floresta"
                ),
                "Silvicultura" = (
                    cube == "Invalid-Wetlands" &
                        mask == "Silvicultura"
                ),
                "Vegetação secundária" = (
                    cube == "Invalid-Wetlands" &
                        mask == "Vegetação secundária"
                ),
                "Mineração" = (
                    cube == "Invalid-Wetlands" &
                        mask == "Mineração"
                ),
                "Área urbanizada" = (
                    cube == "Invalid-Wetlands" &
                        mask == "Área urbanizada"
                ),
                "Natural não florestal" = (
                    cube == "Invalid-Wetlands" &
                        mask == "Natural não florestal"
                ),
                "Pastagem" = (
                    cube == "Invalid-Wetlands" &
                        (mask == "Pastagem" | mask == "Desmatamento do ano")
                ),
                "Sazonalmente inundada" = (
                    cube == "Invalid-Wetlands" &
                        mask == "Sazonalmente inundada"
                ),
                "Agricultura Perene" = (
                    cube == "Invalid-Wetlands" &
                        mask == "Agricultura Perene"
                ),
                "Não observado" = (
                    cube == "Invalid-Wetlands" &
                        mask == "Não observado"
                )
            )
        )
        # reclassify!
        cube <- eval(bquote(
            sits::sits_reclassify(
                cube = cube,
                mask = reference_mask,
                rules = .(rules_expression),
                exclude_mask_na = exclude_mask_na,
                roi = roi,
                multicores = multicores,
                memsize = memsize,
                output_dir = output_dir,
                version = version
            )
        ))
    }
    .reclassify_save_rds(cube, output_dir, version)
}

#' @export
reclassify_rule38_forest_bianual_consistency <- function(files,
                                                   forest_id,
                                                   pasture_id,
                                                   excluded_class_ids,
                                                   version,
                                                   multicores,
                                                   memsize,
                                                   output_dir) {
    # Create output directory
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)
    # Define output file
    out_filename <- .reclassify_temp_filename(version)
    out_file <- fs::path(output_dir) / out_filename
    # If result already exists, return it!
    if (file.exists(out_file)) {
        return(out_file)
    }
    # The following functions define optimal parameters for parallel processing
    rast_template <- sits:::.raster_open_rast(files)
    image_size <- list(
        nrows = sits:::.raster_nrows(rast_template),
        ncols = sits:::.raster_ncols(rast_template)
    )
    # Get block size
    block <- sits:::.raster_file_blocksize(sits:::.raster_open_rast(files))
    # Check minimum memory needed to process one block
    job_block_memsize <- sits:::.jobs_block_memsize(
        block_size = sits:::.block_size(block = block, overlap = 0),
        npaths = length(files),
        nbytes = 8,
        proc_bloat = sits:::.conf("processing_bloat")
    )
    # Update multicores parameter based on size of a single block
    multicores <- sits:::.jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter based on the size of memory and number of cores
    block <- sits:::.jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = image_size,
        memsize = memsize,
        multicores = multicores
    )
    # Create chunks
    chunks <- sits:::.chunks_create(
        block = block,
        overlap = 0,
        image_size = image_size,
        image_bbox = sits:::.bbox(
            sits:::.raster_bbox(rast_template),
            default_crs = terra::crs(rast_template)
        )
    )
    # Update chunk to save extra information
    chunks[["files"]] <- rep(list(files), nrow(chunks))
    chunks[["out_filename"]] <- out_filename
    chunks[["excluded_class_ids"]] <- rep(list(excluded_class_ids), nrow(chunks))
    chunks[["forest_id"]] <- forest_id
    chunks[["pasture_id"]] <- pasture_id
    # Start workers
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)
    # Process data!
    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
        # Get chunk block
        block <- sits:::.block(chunk)
        # Get extra context defined by restoreutils
        files <- chunk[["files"]][[1]]
        out_filename <- chunk[["out_filename"]]
        excluded_class_ids <- chunk[["excluded_class_ids"]][[1]]
        forest_id <- chunk[["forest_id"]]
        pasture_id <- chunk[["pasture_id"]]
        # Define block file name / path
        block_file <- sits:::.file_block_name(
            pattern = tools::file_path_sans_ext(out_filename),
            block = block,
            output_dir = output_dir
        )
        # If block already exists, return it!
        if (file.exists(block_file)) {
            return(block_file)
        }
        # Read raster values
        values <- sits:::.raster_read_rast(files = files, block = block)
        n <- ncol(values)
        values <- restoreutils:::C_trajectory_forest_bianual_consistency(
            data_pyear = values[, 1:(n - 1), drop = FALSE],
            data_nyear = values[, 2:n, drop = FALSE],
            reference_class = forest_id,
            fallback_class = pasture_id,
            excluded_values = excluded_class_ids
        )
        # Prepare and save results as raster
        sits:::.raster_write_block(
            files = block_file,
            block = block,
            bbox = sits:::.bbox(chunk),
            values = values,
            data_type = "INT1U",
            missing_value = 255,
            crop_block = NULL
        )
        # Free memory
        gc()
        # Returned block file
        block_file
    }, progress = TRUE)
    # Merge raster blocks
    sits:::.raster_merge_blocks(
        out_files = out_file,
        base_file = files,
        block_files = block_files,
        data_type = "INT1U",
        missing_value = 255,
        multicores = multicores
    )
    # Remove block files
    unlink(block_files)
    # Return!
    return(out_file)
}

#' @export
reclassify_rule39_perene_scatter_control <- function(files,
                                                     class_a,
                                                     class_b,
                                                     class_c,
                                                     version,
                                                     year,
                                                     multicores,
                                                     memsize,
                                                     output_dir) {
    # Create output directory
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)
    # Define output file
    out_filename <- .reclassify_sits_name(version, year)
    out_file <- fs::path(output_dir) / out_filename
    # If result already exists, return it!
    if (file.exists(out_file)) {
        return(out_file)
    }
    # The following functions define optimal parameters for parallel processing
    rast_template <- sits:::.raster_open_rast(files)
    image_size <- list(
        nrows = sits:::.raster_nrows(rast_template),
        ncols = sits:::.raster_ncols(rast_template)
    )
    # Get block size
    block <- sits:::.raster_file_blocksize(sits:::.raster_open_rast(files))
    # Check minimum memory needed to process one block
    job_block_memsize <- sits:::.jobs_block_memsize(
        block_size = sits:::.block_size(block = block, overlap = 0),
        npaths = length(files),
        nbytes = 8,
        proc_bloat = sits:::.conf("processing_bloat")
    )
    # Update multicores parameter based on size of a single block
    multicores <- sits:::.jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter based on the size of memory and number of cores
    block <- sits:::.jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = image_size,
        memsize = memsize,
        multicores = multicores
    )
    # Create chunks
    chunks <- sits:::.chunks_create(
        block = block,
        overlap = 0,
        image_size = image_size,
        image_bbox = sits:::.bbox(
            sits:::.raster_bbox(rast_template),
            default_crs = terra::crs(rast_template)
        )
    )
    # Update chunk to save extra information
    chunks[["files"]] <- rep(list(files), nrow(chunks))
    chunks[["out_filename"]] <- out_filename
    chunks[["class_a"]] <- class_a
    chunks[["class_b"]] <- class_b
    chunks[["class_c"]] <- class_c
    # Start workers
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)
    # Process data!
    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
        # Get chunk block
        block <- sits:::.block(chunk)
        # Get extra context defined by restoreutils
        files <- chunk[["files"]][[1]]
        out_filename <- chunk[["out_filename"]]
        class_a <- chunk[["class_a"]]
        class_b <- chunk[["class_b"]]
        class_c <- chunk[["class_c"]]
        # Define block file name / path
        block_file <- sits:::.file_block_name(
            pattern = tools::file_path_sans_ext(out_filename),
            block = block,
            output_dir = output_dir
        )
        # If block already exists, return it!
        if (file.exists(block_file)) {
            return(block_file)
        }
        # Read raster values
        values <- sits:::.raster_read_rast(files = files, block = block)
        values <- restoreutils:::C_trajectory_triplet_behavior(
            data = values,
            class_a = class_a,
            class_b = class_b,
            class_c = class_c
        )
        # Prepare and save results as raster
        sits:::.raster_write_block(
            files = block_file,
            block = block,
            bbox = sits:::.bbox(chunk),
            values = values[, 2],
            data_type = "INT1U",
            missing_value = 255,
            crop_block = NULL
        )
        # Free memory
        gc()
        # Returned block file
        block_file
    }, progress = TRUE)
    # Merge raster blocks
    sits:::.raster_merge_blocks(
        out_files = out_file,
        base_file = files,
        block_files = block_files,
        data_type = "INT1U",
        missing_value = 255,
        multicores = multicores
    )
    # Remove block files
    unlink(block_files)
    # Return!
    return(out_file)
}
