#' @export
reclassify_cer_rule0_natveg <- function(cube, mask, multicores, memsize, output_dir, version, exclude_mask_na = FALSE) {

    # Labels that will not be included in mask
    not_included_labels <- c(
        "31 - Área Antropizada",
        "32 - Corpos d'Água",
        "34 - Não Observado"
    )

    # Filter labels to be included in mask
    labels_to_mask <- setdiff(
        unname(sits::sits_labels(mask)), not_included_labels
    )

    # Build rules expression: each label will be a class
    expressions <- lapply(labels_to_mask, function(label) {
        bquote(mask == .(label))
    })

    names(expressions) <- labels_to_mask

    rules_expression <- as.call(c(bquote(list), expressions))

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
reclassify_cer_rule1_veg <- function(cube, mask, multicores, memsize, output_dir, version, exclude_mask_na = FALSE) {

    # Cube labels
    cube_labels <- c(
        "Cerrado",
        "Cerradao",
        "Open-Cerrado"
    )

    # Mask labels
    mask_labels <- sits::sits_labels(mask)

    # Build rules expression: each label will be a class
    expressions <- lapply(mask_labels, function(mask_label) {
        lapply(cube_labels, function(cube_label) {
            bquote(mask == .(mask_label) & cube == .(cube_label))
        })
    })

    # Transform to an unique level list
    expressions <- unlist(expressions)

    # Create mask labels
    labels_to_mask <- sapply(mask_labels, function(mask_label) {
        glue::glue("{cube_labels}-{mask_label}")
    }, USE.NAMES = FALSE)

    # Add labels for the output list
    names(expressions) <- c(labels_to_mask)

    # Transform to call
    rules_expression <- as.call(c(bquote(list), expressions))

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
reclassify_cer_rule3_agr_anual <- function(cube, mask, multicores, memsize, output_dir, version, rarg_year, exclude_mask_na = FALSE) {

    if (rarg_year < 2018) {
        return(cube)
    }

    cube <- sits::sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "Agricultura anual" = cube == "Agricultura anual" |
                mask %in% c("CULTURA AGRICOLA TEMPORARIA DE 1 CICLO",
                            "CULTURA AGRICOLA TEMPORARIA DE MAIS DE 1 CICLO")
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
reclassify_cer_rule4_semi_perene <- function(cube, mask, multicores, memsize, output_dir, version, rarg_year, exclude_mask_na = FALSE) {

    if (rarg_year < 2018) {
        return(cube)
    }

    cube <- sits::sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "Cana" = cube == "Cana" | mask == "CULTURA AGRICOLA SEMIPERENE"
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
reclassify_cer_rule5_perene <- function(cube, mask, multicores, memsize, output_dir, version, rarg_year, exclude_mask_na = FALSE) {

    if (rarg_year < 2018) {
        return(cube)
    }

    cube <- sits::sits_reclassify(
        cube = cube,
        mask = mask,
        rules = list(
            "Agricultura Perene" = cube == "Agricultura Perene" | mask == "CULTURA AGRICOLA PERENE"
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
reclassify_cer_rule6_silviculture <- function(cube, mask, multicores, memsize, output_dir, version, rarg_year, exclude_mask_na = FALSE) {

    if (rarg_year < 2018) {
        return(cube)
    }

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
