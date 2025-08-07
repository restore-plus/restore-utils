library(sits)
library(restoremasks)

#
# General definitions
#
mask_version <- "mask-mcti-v3"
files_version <- "mask-terraclass-step13"
output_version <- "step14"

base_output_dir <- "data/derived/masks"

perene_class_id <- 2       # "Ag_perene"
replacement_class_id <- 21 # "Pasto_Perene"

memsize <- 200
multicores <- 28


#
# 1. Generate directories
#
output_dir <- create_data_dir(base_output_dir, mask_version)
output_dir <- create_data_dir(output_dir, "perene-transitions")


#
# 2. Get masks files
#
files <- get_restore_masks_files(
    mask_version = mask_version,
    files_version = files_version,
    multicores = multicores,
    memsize = memsize
)


#
# 3. Reclassify perene neighbor data
#
file_reclassified <- reclassify_neighbor_perene_data(
    files           = files,
    perene_class_id = perene_class_id,
    replacement_class_id     = replacement_class_id,
    version         = output_version,
    multicores      = multicores,
    memsize         = memsize,
    output_dir      = output_dir
)


#
# 4. Save results as classification maps
#
reclassify_perene_result_to_maps(
    files             = files,
    file_reclassified = file_reclassified,
    version           = output_version
)
