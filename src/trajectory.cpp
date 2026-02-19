#include <Rcpp.h>
#include <algorithm>

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix C_trajectory_transition_analysis(NumericMatrix data, int reference_class, int neighbor_class) {
    int npixel = data.nrow();
    int nyear = data.ncol();

    if (nyear != 10) {
        stop("Expected exactly 10 years (columns), but got " + std::to_string(nyear));
    }

    for (int i = 0; i < npixel; i++) {
        for (int j = 0; j + 2 < nyear; j += 3) {

            bool valid_neighbor = data(i, j) == neighbor_class && data(i, j + 2) == neighbor_class;
            bool valid_class = data(i, j + 1) == reference_class;

            if (valid_class && valid_neighbor) {
                data(i, j + 1) = neighbor_class;

                // 9 means the last time step
                // 6 means the last iteration for the moving window
                if (j == 6 && data(i, 9) == reference_class) {
                    data(i, nyear - 1) = neighbor_class;
                }
            }
        }
    }

    return data;
}


// [[Rcpp::export]]
NumericVector C_urban_transition(NumericMatrix data, int urban_id, int nb_id) {
    int npixel = data.nrow();
    int nyear = data.ncol();

    int current_index;
    if (nyear == 2) {
        current_index = 0;
        for (int i = 0; i < npixel; i++) {
            bool valid_neighbor = data(i, 0) == nb_id && data(i, 1) == urban_id;
            if (valid_neighbor) {
                data(i, 0) = urban_id;
            }
        }
    } else {
        current_index = 1;
        for (int i = 0; i < npixel; i++) {
            bool valid_neighbor_lhs = data(i, 0) == urban_id && data(i, 1) == nb_id;
            bool valid_neighbor_rhs = data(i, 2) == urban_id && data(i, 1) == nb_id;

            if (valid_neighbor_lhs || valid_neighbor_rhs) {
                data(i, 1) = urban_id;
            }
        }
    }

    return data(_, current_index);
}

// [[Rcpp::export]]
NumericMatrix C_trajectory_neighbor_analysis(NumericMatrix data, int reference_class, int replacement_class) {
    int npixel = data.nrow();
    int nyear = data.ncol();

    if (nyear < 3) {
        stop("Expected at least 3 years (columns), but got " + std::to_string(nyear));
    }

    for (int i = 0; i < npixel; i++) {
        // remove edges (start: j = 1; end = j - 1)
        for (int j = 1; j < nyear - 1; j++) {

            if (data(i, j) == reference_class) {
                bool is_left_diff = data(i, j - 1) != reference_class;
                bool is_right_diff = data(i, j + 1) != reference_class;

                if (is_left_diff && is_right_diff) {
                    data(i, j) = replacement_class;
                }
            }
        }
    }

    return data;
}

// [[Rcpp::export]]
NumericMatrix C_trajectory_neighbor_consistency_analysis(NumericMatrix data, int reference_class) {
    // This rule was originally implemented to:
    // > "Se tem agr anual, qualquer classe, agr anual, vira agr anual (2ciclos)"

    int npixel = data.nrow();
    int nyear = data.ncol();

    if (nyear < 3) {
        stop("Expected at least 3 years (columns), but got " + std::to_string(nyear));
    }

    for (int i = 0; i < npixel; i++) {
        // remove edges (start: j = 1; end = j - 1)
        for (int j = 1; j < nyear - 1; j++) {
            bool is_left_valid = data(i, j - 1)  == reference_class;
            bool is_right_valid = data(i, j + 1) == reference_class;

            bool is_middle_invalid = data(i, j)  != reference_class;

            if (is_left_valid && is_right_valid && is_middle_invalid) {
                data(i, j) = reference_class;
            }
        }
    }

    return data;
}

// [[Rcpp::export]]
NumericMatrix C_trajectory_neighbor_consistency_analysis_with_mask(NumericMatrix data, NumericMatrix mask, int data_class, int mask_class) {
    int npixel = data.nrow();

    if (data.nrow() != mask.nrow()) {
        stop("Data and mask must have the same dimensions");
    }

    for (int i = 0; i < npixel; i++) {
        bool is_mask_left_valid  = mask(i, 0) == mask_class;
        bool is_mask_right_valid = mask(i, 1) == mask_class;

        bool is_data_middle_invalid = data(i, 0) != data_class;

        if (is_mask_left_valid && is_mask_right_valid && is_data_middle_invalid) {
            data(i, 0) = data_class;
        }
    }

    return data;
}

// [[Rcpp::export]]
NumericMatrix C_trajectory_neighbor_majority_analysis(NumericMatrix data, int reference_class, DataFrame target_class_map) {
    // This rule was originally implemented to:
    // > "Se temos classe x, ag anual (2ciclos), classe x, o valor do meio (ag anual), vira classe x"

    int npixel = data.nrow();
    int nyear = data.ncol();

    if (nyear < 3) {
        stop("Expected at least 3 years (columns), but got " + std::to_string(nyear));
    }

    // Extract source and target columns from DataFrame
    IntegerVector source = target_class_map["source"];
    IntegerVector target = target_class_map["target"];
    List indices_list = target_class_map["indices"];

    for (int i = 0; i < npixel; i++) {
        // remove edges (start: j = 1; end = j - 1)
        for (int j = 1; j < nyear - 1; j++) {

            // Convert left and right values
            int left_value = static_cast<int>(data(i, j - 1));
            int right_value = static_cast<int>(data(i, j + 1));

            // Convert left value
            for (int k = 0; k < source.length(); k++) {
                if (source[k] == left_value) {
                    // Get the indices vector for this mapping
                    IntegerVector valid_indices = as<IntegerVector>(indices_list[k]) - 1;

                    // Check if j is in the valid indices
                    if (std::find(valid_indices.begin(), valid_indices.end(), j - 1) != valid_indices.end()) {
                        left_value = target[k];
                        break;
                    }
                }
            }

            // Convert right value
            for (int k = 0; k < source.length(); k++) {

                if (source[k] == right_value) {
                    // Get the indices vector for this mapping
                    IntegerVector valid_indices = as<IntegerVector>(indices_list[k]) - 1;

                    // Check if j is in the valid indices
                    if (std::find(valid_indices.begin(), valid_indices.end(), j + 1) != valid_indices.end()) {
                        right_value = target[k];
                        break;
                    }
                }
            }

            bool is_left_equal_to_right = left_value == right_value;
            bool is_middle_valid = data(i, j) == reference_class;

            if (is_left_equal_to_right && is_middle_valid) {
                data(i, j) = left_value;
            }
        }
    }

    return data;
}

// [[Rcpp::export]]
NumericMatrix C_trajectory_neighbor_majority_analysis_target(NumericMatrix data, int reference_class, int target_class) {
    // This rule was originally implemented to:
    // > "Se temos qualquer pastagem, ag anual (2ciclos), qualquer pastagem, o valor do meio (ag anual), vira pastagem"

    int npixel = data.nrow();
    int nyear = data.ncol();

    if (nyear < 3) {
        stop("Expected at least 3 years (columns), but got " + std::to_string(nyear));
    }

    for (int i = 0; i < npixel; i++) {
        // remove edges (start: j = 1; end = j - 1)
        for (int j = 1; j < nyear - 1; j++) {
            // Get left and right values from previous and next years
            int left_value = static_cast<int>(data(i, j - 1));
            int right_value = static_cast<int>(data(i, j + 1));

            // Define auxiliary variables
            bool is_middle_valid = data(i, j) == reference_class;
            bool is_left_equal_to_right = left_value == target_class && right_value == target_class;

            if (is_left_equal_to_right && is_middle_valid) {
                data(i, j) = left_value;
            }
        }
    }

    return data;
}

// [[Rcpp::export]]
NumericMatrix C_trajectory_cropland_transitions(NumericMatrix data, IntegerVector source_classes, IntegerVector target_classes, int pasture_id, int cropland_id) {
    // This rule was originally implemented to:
    // > "Se temos pastagem agricultura e desmatamento, ag anual (2ciclos), qualquer pastagem agricultura e desmatamento, o valor do meio (ag anual), vira pastagem"

    int npixel = data.nrow();
    int nyear = data.ncol();

    if (nyear < 3) {
        stop("Expected at least 3 years (columns), but got " + std::to_string(nyear));
    }

    for (int i = 0; i < npixel; i++) {
        // remove edges (start: j = 1; end = j - 1)
        for (int j = 1; j < nyear - 1; j++) {
            // Get left and right values from previous and next years
            int left_value = static_cast<int>(data(i, j - 1));
            int middle_value = static_cast<int>(data(i, j));
            int right_value = static_cast<int>(data(i, j + 1));

            // Are left and right values valid?
            bool is_left_valid = std::find(source_classes.begin(), source_classes.end(), left_value) != source_classes.end();
            bool is_middle_valid = middle_value == cropland_id;
            bool is_right_valid = std::find(target_classes.begin(), target_classes.end(), right_value) != target_classes.end();

            // Define auxiliary variables
            bool is_left_equal_to_right = is_left_valid && is_right_valid;

            if (is_left_equal_to_right && is_middle_valid) {
                data(i, j) = pasture_id;
            }
        }
    }

    return data;
}

// [[Rcpp::export]]
NumericMatrix C_trajectory_water_analysis(NumericMatrix data, int water_class, DataFrame target_class_map, IntegerVector excluded_values) {
    int npixel = data.nrow();

    // Extract source and target columns from DataFrame
    IntegerVector source = target_class_map["source"];
    IntegerVector target = target_class_map["target"];

    for (int i = 0; i < npixel; i++) {
        // Skip pixels whose left, middle, or right value is in the excluded list
        int left_value = static_cast<int>(data(i, 0));
        int middle_value = static_cast<int>(data(i, 1));
        int right_value = static_cast<int>(data(i, 2));

        bool is_left_excluded = std::find(excluded_values.begin(), excluded_values.end(), left_value) != excluded_values.end();
        bool is_middle_excluded = std::find(excluded_values.begin(), excluded_values.end(), middle_value) != excluded_values.end();
        bool is_right_excluded = std::find(excluded_values.begin(), excluded_values.end(), right_value) != excluded_values.end();

        if (is_left_excluded || is_middle_excluded || is_right_excluded) {
            continue;
        }

        bool is_left_water   = data(i, 0) == water_class;
        bool is_middle_water = data(i, 1) == water_class;
        bool is_right_water  = data(i, 2) == water_class;

        if (is_middle_water && (!is_left_water || !is_right_water)) {
            // Use data(i, 0) to find matching source and get target
            int source_class = static_cast<int>(data(i, 0));

            for (int j = 0; j < source.length(); j++) {
                if (source[j] == source_class) {
                    data(i, 1) = target[j];
                    break;
                }
            }
        }
    }

    return data;
}

// [[Rcpp::export]]
NumericMatrix C_trajectory_urban_analysis(NumericMatrix data, NumericMatrix mask, int urban_class_id, int forest_class_id, int forest_class_id_mask) {
    int npixel = data.nrow();
    int nyear = data.ncol();

    for (int i = 0; i < npixel; i++) {
        int forest_index = -1;
        int urban_index = -1;
        int urban_change = 0;

        // Looking for urban in the series
        for (int j = 0; j < nyear; j++) {
            if (urban_index != -1) {
                break;
            }

            if (data(i, j) == urban_class_id) {
                urban_index = j;

                break;
            }
        }

        // If urban exist, index will not be `-1`
        if (urban_index != -1) {

            // Looking for changes in the time-series
            for (int j = urban_index; j < nyear; j++) {
                if (urban_change) {
                    break;
                }

                if (data(i, j) != urban_class_id) {
                    urban_change = 1;

                    break;
                }
            }

        }

        // Check if change goes to forest
        if (urban_change) {
            // Looking for forest
            for (int j = urban_index; j < nyear; j++) {
                // If forest was already identified, skip operation
                if (forest_index != -1) {
                    break;
                }

                // If is a forest, save the index
                if (data(i, j) == forest_class_id) {
                    forest_index = j;

                    break;
                }
            }
        }

        // If we change the firest index, this means we identify forest
        // in a urban area time-series
        if (forest_index != -1) {

            // Transforming everything in forest
            for (int j = urban_index; j < nyear; j++) {
                if (mask(i, j) == forest_class_id_mask) {
                    data(i, j) = forest_class_id;
                }
            }

        } else if (urban_index != -1) {
            // Transforming everything in urban area
            for (int j = urban_index; j < nyear; j++) {
                data(i, j) = urban_class_id;
            }
        }
    }

    return data;
}

// [[Rcpp::export]]
NumericMatrix C_trajectory_vs_analysis(NumericMatrix data, int vs_class, IntegerVector pasture_class, int target_class) {
    int npixel = data.nrow();

    for (int i = 0; i < npixel; i++) {
        bool is_left   = std::find(pasture_class.begin(), pasture_class.end(), data(i, 0)) != pasture_class.end();
        bool is_middle = data(i, 1) == vs_class;
        bool is_right  = std::find(pasture_class.begin(), pasture_class.end(), data(i, 2)) != pasture_class.end();

        if (is_left && is_middle && is_right) {
            // If middle is water, change to the first class.
            data(i, 1) = target_class; // data(i, 0);
        }
    }

    return data;
}

