#' @title Validate inputs for sankey trajectory preparation
#'
#' @param cube  A \code{sits} classification cube tibble.
#' @param years Numeric or character vector of year labels.
#'
#' @returns Called for side effects (throws error on invalid input).
#' @noRd
.check_sankey_inputs <- function(cube, years) {
    if (!inherits(cube, "tbl_df") || nrow(cube) < 2) {
        cli::cli_abort(c(
            "Expected a sits cube tibble with at least 2 rows.",
            "i" = "Each row should represent one time step (e.g., year)."
        ))
    }

    if (length(years) != nrow(cube)) {
        cli::cli_abort(c(
            "Length of {.arg years} ({length(years)}) must match
             the number of cube rows ({nrow(cube)})."
        ))
    }
}

#' @title Validate that all cube rows share the same spatial grid
#'
#' @param cube A \code{sits} classification cube tibble.
#'
#' @returns Called for side effects (throws error if grids are misaligned).
#' @noRd
.check_aligned_grids <- function(cube) {
    base_file <- sits:::.tile_path(cube[1, ])
    base_rast <- sits:::.raster_open_rast(base_file)
    base_nrows <- sits:::.raster_nrows(base_rast)
    base_ncols <- sits:::.raster_ncols(base_rast)

    for (i in seq_len(nrow(cube))[-1]) {
        file <- sits:::.tile_path(cube[i, ])
        rast <- sits:::.raster_open_rast(file)

        if (sits:::.raster_nrows(rast) != base_nrows ||
            sits:::.raster_ncols(rast) != base_ncols) {
            cli::cli_abort(c(
                "All cube rows must share the same spatial grid.",
                "x" = "Row {i} has dimensions
                        {sits:::.raster_nrows(rast)} x {sits:::.raster_ncols(rast)}
                        (expected {base_nrows} x {base_ncols}).",
                "i" = "Use {.fn sits::sits_regularize} to align cubes."
            ))
        }
    }
}

#' @title Prepare trajectory data from multi-temporal classification cube
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Reads a classification cube and computes pixel
#' trajectory frequencies using chunk-based parallel processing. The output
#' is a trajectory frequency table suitable for Sankey diagrams, temporal
#' analysis, or transition studies.
#'
#' Each row in the result represents a unique trajectory (sequence of class
#' labels across time steps), with the \code{freq} column indicating how many
#' pixels followed that trajectory.
#'
#' @note All cube rows must share the same spatial grid (extent, resolution,
#' and CRS). The function validates grid alignment before processing.
#'
#' @param cube       A \code{sits} classification cube tibble where each row
#'                   represents one time step.
#' @param years      A numeric or character vector of year labels, one per
#'                   cube row. Used as column names in the output.
#' @param output_dir Character path for intermediate files.
#' @param multicores Integer number of cores for parallel processing
#'                   (default: 10).
#' @param memsize    Integer max memory in GB (default: 16).
#' @param roi        Optional region of interest (ROI) to filter chunks.
#'
#' @returns A tibble with one column per year and a \code{freq} column with
#' pixel counts for each unique trajectory.
#'
#' @export
sankey_prepare <- function(cube, years, output_dir, multicores = 10,
                           memsize = 16, roi = NULL) {
    # Validate inputs
    .check_sankey_inputs(cube, years)
    .check_aligned_grids(cube)

    # Setup
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)

    if (!is.null(roi)) {
        roi <- sits:::.roi_as_sf(roi)
    }

    step_names <- as.character(years)

    # Collect labels for each cube row
    cube_labels <- lapply(seq_len(nrow(cube)), function(i) {
        sits::sits_labels(cube[i, ])
    })

    # Use the first row to define the chunk grid
    base_tile <- cube[1, ]
    file <- sits:::.tile_path(base_tile)
    rast_template <- sits:::.raster_open_rast(file)

    image_size <- list(
        nrows = sits:::.raster_nrows(rast_template),
        ncols = sits:::.raster_ncols(rast_template)
    )

    # Compute optimal block size
    block <- sits:::.raster_file_blocksize(rast_template)

    job_block_memsize <- sits:::.jobs_block_memsize(
        block_size = sits:::.block_size(block = block, overlap = 0),
        npaths = nrow(cube),
        nbytes = 8,
        proc_bloat = sits:::.conf("processing_bloat")
    )

    multicores <- sits:::.jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )

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

    # Filter chunks by ROI
    if (!is.null(roi)) {
        chunks <- sits:::.chunks_filter_spatial(
            chunks = chunks,
            roi = roi
        )
    }

    # Attach extra data to each chunk
    chunks[[".cube"]] <- rep(list(cube), nrow(chunks))
    chunks[[".step_names"]] <- rep(list(step_names), nrow(chunks))
    chunks[[".cube_labels"]] <- rep(list(cube_labels), nrow(chunks))

    # Start workers
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)

    # Process chunks in parallel
    data <- sits:::.jobs_map_parallel_dfr(chunks, function(chunk) {
        block <- sits:::.block(chunk)
        cube <- chunk[[".cube"]][[1]]
        step_names <- chunk[[".step_names"]][[1]]
        cube_labels <- chunk[[".cube_labels"]][[1]]

        # Read values from each cube row for this block
        values_list <- lapply(seq_len(nrow(cube)), function(i) {
            tile <- cube[i, ]
            as.integer(as.vector(
                sits:::.tile_read_block(
                    tile = tile,
                    band = sits:::.tile_bands(tile),
                    block = block
                )
            ))
        })

        # Combine into a data.frame (pixels x time steps)
        values_df <- as.data.frame(do.call(cbind, values_list))
        colnames(values_df) <- step_names

        # Remove pixels with any NA
        values_df <- values_df[complete.cases(values_df), , drop = FALSE]

        if (nrow(values_df) == 0) {
            return(data.frame())
        }

        # Map integer codes to class labels
        for (i in seq_along(step_names)) {
            col <- step_names[[i]]
            labels <- cube_labels[[i]]
            values_df[[col]] <- labels[as.character(values_df[[col]])]
        }

        # Count unique trajectories within this chunk
        # (The idea here is: we only need to know how many times a transition
        # happens. We don't need to save the actual transition)
        tbl <- dplyr::count(
            values_df,
            dplyr::across(dplyr::everything()),
            name = "freq"
        )

        # Garbage collection
        gc()

        # Return table
        tbl
    }, progress = TRUE)

    # Aggregate trajectory counts across all chunks
    data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(step_names))) |>
        dplyr::summarise(freq = sum(.data[["freq"]]), .groups = "drop")
}

#' @title Convert trajectory data to long format for Sankey plots
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Converts the wide trajectory table from
#' \code{\link{sankey_prepare}} into a long format suitable for
#' \code{ggalluvial} Sankey plots.
#'
#' @param data      A tibble returned by \code{\link{sankey_prepare}}.
#' @param step_col  Character name for the time step column
#'                  (default: \code{"step"}).
#' @param class_col Character name for the class column
#'                  (default: \code{"class"}).
#'
#' @returns A tibble in long format with columns: \code{trajectory} (integer
#' id), \code{freq}, and the specified step/class columns as factors.
#'
#' @export
sankey_to_long <- function(data, step_col = "step", class_col = "class") {
    # Identify step columns (assume everything except 'freq')
    step_names <- setdiff(names(data), "freq")

    # Add trajectory id (each pixel is a unique trajectory)
    data[["trajectory"]] <- seq_len(nrow(data))

    # Pivot to long format
    data |>
        tidyr::pivot_longer(
            cols = dplyr::all_of(step_names),
            names_to = step_col,
            values_to = class_col
        ) |>
        dplyr::mutate(
            !!step_col := factor(.data[[step_col]], levels = step_names)
        )
}

#' @title Plot a Sankey (alluvial) diagram from trajectory data
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Creates a Sankey (alluvial) diagram using \code{ggalluvial},
#' showing class trajectories across time steps.
#'
#' @param data       A tibble in long format, as returned by
#'                   \code{\link{sankey_to_long}}.
#' @param colors     Color specification. One of:
#'                   \itemize{
#'                     \item \code{NULL} generates a random palette (default).
#'                     \item A path to a \code{.qml} file (QGIS style).
#'                     \item A tibble with \code{label} and \code{color}
#'                           columns.
#'                     \item A named character vector mapping class names
#'                           to hex colors.
#'                   }
#' @param step_col   Character name of the time step column
#'                   (default: \code{"step"}).
#' @param class_col  Character name of the class column
#'                   (default: \code{"class"}).
#' @param title      Character plot title
#'                   (default: \code{"Class trajectory by year"}).
#' @param x_label    Character x-axis label (default: \code{"Year"}).
#' @param y_label    Character y-axis label (default: \code{"Frequency"}).
#' @param legend_rows Integer number of rows in the legend (default: 3).
#' @param alpha      Numeric alluvium transparency (default: 0.7).
#'
#' @returns A \code{ggplot2} object.
#'
#' @export
sankey_plot <- function(data, colors = NULL, step_col = "step",
                        class_col = "class",
                        title = "Class trajectory by year",
                        x_label = "Year", y_label = "Frequency",
                        legend_rows = 3, alpha = 0.7) {
    # Resolve colors from any supported format
    # Get the unique classes
    classes <- unique(data[[class_col]])

    # Resolve the colors
    palette <- .qml_to_palette(colors, classes)

    # Plot the Sankey diagram
    ggplot2::ggplot(
        data = data,
        ggplot2::aes(
            x = .data[[step_col]],
            y = .data[["freq"]],
            stratum = .data[[class_col]],
            alluvium = .data[["trajectory"]],
            fill = .data[[class_col]]
        )
    ) +
        ggalluvial::geom_alluvium(alpha = alpha, decreasing = FALSE) +
        ggalluvial::geom_stratum(decreasing = FALSE) +
        ggplot2::scale_fill_manual(values = palette) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
            title = title,
            x     = x_label,
            y     = y_label,
            fill  = "Class"
        ) +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = legend_rows))
}
