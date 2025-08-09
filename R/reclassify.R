.reclassify_perene_filename <- function(version) {
    paste0(version, "-perene-reclass", ".tif")
}

#' @export
reclassify_rule1_secundary_vegetation <- function(cube, mask, multicores, memsize, output_dir, version) {
    sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "vegetacao_secundaria" = cube == "vegetacao_secundaria" |
                cube %in% c("Forest", "Riparian_Forest", "Mountainside_Forest") &
                mask != "Vegetação Nativa"
        ),
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )
}

#' @export
reclassify_rule2_current_deforestation <- function(cube, mask, multicores, memsize, output_dir, version, rarg_year) {
    # build args for expression
    deforestation_year <- paste0("d", rarg_year)

    # build expression
    rules_expression <- bquote(
        list(
            "Forest" = cube == "Forest" | mask == "Vegetação Nativa",
            "deforest_year" = mask == .(deforestation_year)
        )
    )

    # reclassify!
    eval(bquote(
        sits_reclassify(
            cube = cube,
            mask = mask,
            rules = .(rules_expression),
            multicores = multicores,
            memsize = memsize,
            output_dir = output_dir,
            version = version
        )
    ))
}

#' @export
reclassify_rule3_pasture_wetland <- function(cube, mask, multicores, memsize, output_dir, version, rarg_year) {
    # build args for expression
    residuals_years <- paste0("r", 2010:rarg_year)
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
    eval(bquote(
        sits_reclassify(
            cube = cube,
            mask = mask,
            rules = .(rules_expression),
            multicores = multicores,
            memsize = memsize,
            output_dir = output_dir,
            version = version
        )
    ))
}

#' @export
reclassify_rule4_silviculture <- function(cube, mask, multicores, memsize, output_dir, version) {
    sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "Silvicultura" = cube == "Silvicultura" | mask == "SILVICULTURA"
        ),
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )
}

#' @export
reclassify_rule5_silviculture_pasture <- function(cube, mask, multicores, memsize, output_dir, version) {
    sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "pasto_silvicultura" = cube == "Silvicultura" & mask != "SILVICULTURA"
        ),
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )
}

#' @export
reclassify_rule6_semiperennial <- function(cube, mask, multicores, memsize, output_dir, version) {
    sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "Agr. Semiperene" = cube == "Agr. Semiperene" | mask == "CULTURA AGRICOLA SEMIPERENE"
        ),
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )
}

#' @export
reclassify_rule7_semiperennial_pasture <- function(cube, mask, multicores, memsize, output_dir, version) {
    sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "pasto_semiperene" = cube == "Agr. Semiperene" &
                                !(mask %in% c("CULTURA AGRICOLA SEMIPERENE",
                                              "CULTURA AGRICOLA TEMPORARIA DE 1 CICLO",
                                              "CULTURA AGRICOLA TEMPORARIA DE MAIS DE 1 CICLO"))
        ),
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )
}

#' @export
reclassify_rule8_annual_agriculture <- function(cube, mask, multicores, memsize, output_dir, version) {
    sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "2ciclos" = cube == "2ciclos"  |
                        (cube == "Agr. Semiperene" & mask %in% c(
                            "CULTURA AGRICOLA TEMPORARIA DE 1 CICLO",
                            "CULTURA AGRICOLA TEMPORARIA DE MAIS DE 1 CICLO"
                        ))
        ),
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )
}

#' @export
reclassify_rule9_minning <- function(cube, mask, multicores, memsize, output_dir, version) {
    sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "mineracao" = mask == "MINERACAO"
        ),
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )
}

#' @export
reclassify_rule10_urban_area <- function(cube, mask, multicores, memsize, output_dir, version) {
    sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "area_urbanizada" = mask == "URBANIZADA"
        ),
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )
}

#' @export
reclassify_rule11_water <- function(cube, mask, multicores, memsize, output_dir, version) {
    sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "agua" = (
                    mask == "CORPO DAGUA" &
                    !cube %in% c("Wetland_ICS", "Seasonally_Flooded_ICS")
            )
        ),
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )
}

#' @export
reclassify_rule12_non_forest <- function(cube, mask, multicores, memsize, output_dir, version) {
    sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "nat_non_forest" = mask == "NATURAL NAO FLORESTAL"
        ),
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )
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
reclassify_temporal_results_to_maps <- function(files, file_reclassified, version) {
    purrr::map_chr(seq_len(length(files)), function(idx) {
        file_path <- files[[idx]]
        file_out_path <- stringr::str_replace(file_path,
                                              ".tif",
                                              .reclassify_perene_filename(version))

        message("Processing: ",
                basename(file_reclassified),
                " → ",
                basename(file_out_path))

        sf::gdal_utils(
            util = "translate",
            source = as.character(fs::path_expand(file_reclassified)),
            destination = file_out_path,
            options = sits:::.gdal_params(
                list(
                    "-b"     = as.character(idx),
                    "-of"    = "GTiff",
                    "-co"    = "TILED=YES",
                    "-co"    = "COMPRESS=LZW",
                    "-co"    = "INTERLEAVE=BAND",
                    "-co"    =  "PREDICTOR=2"
                )
            ),
            quiet = FALSE
        )

        sf::gdal_addo(file_out_path)

        file_out_path
    })
}

#' @export
reclassify_remap_pixels <- function(file,
                                    file_out,
                                    rules,
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
    rast_template <- sits:::.raster_open_rast(file)
    image_size <- list(
        nrows = sits:::.raster_nrows(rast_template),
        ncols = sits:::.raster_ncols(rast_template)
    )
    # Get block size
    block <- sits:::.raster_file_blocksize(sits:::.raster_open_rast(files))
    # Check minimum memory needed to process one block
    job_block_memsize <- sits:::.jobs_block_memsize(
        block_size = sits:::.block_size(block = block, overlap = 0),
        npaths = length(file),
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
    chunks[["file"]] <- file
    chunks[["rules"]] <- list(rules)
    # Start workers
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)
    # Process data!
    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
        # Get chunk block
        block <- sits:::.block(chunk)
        # Get extra context defined by restoreutils
        file <- chunk[["file"]]
        rules <- chunk[["rules"]][[1]]
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
        values <- sits:::.raster_read_rast(files = file, block = block)
        for (rule_idx in seq_len(nrow(rules))) {
            rule <- rules[rule_idx,]

            values <- restoreutils:::C_remap_values(
                data   = values,
                source = rule[["source"]],
                target = rule[["target"]]
            )
        }
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
