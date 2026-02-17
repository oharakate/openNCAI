#' Import and Process Natural Capital Account Data
#'
#' This is the primary wrapper function for importing data from the NatureScot NCAI
#' Excel template. It extracts habitat classifications, ecosystem service weights,
#' condition indicators, and yearly measurements, bundling them into a structured
#' list for subsequent index calculations.
#'
#' @param path A string representing the file path to the .xlsx data source.
#' @param year_list A vector of years to include in the account (e.g., 2000:2022).
#'   Can be numeric or character.
#' @param tir_constant A numeric constant added to the Total Indicator Relevances
#'   to avoid zero divisions. Default is 2.
#'
#' @return A named list containing 11 components:
#' \itemize{
#'   \item \code{ns_year_list}: Character vector of years.
#'   \item \code{ns_habitats_label_tree}: Nested list of broad and detailed habitat labels.
#'   \item \code{ns_es_label_tree}: Nested list of ecosystem service types and specific labels.
#'   \item \code{ns_esppu}: Data frame of Ecosystem Service Potential Per Unit scores.
#'   \item \code{ns_custom_divisor_matrix}: Matrix of divisors for ESPPU normalization.
#'   \item \code{ns_between_importance_scores}: Named list of broad ES importance weights.
#'   \item \code{ns_within_importance_scores}: Named list of weights for specific services.
#'   \item \code{ns_indicator_directory}: Data frame mapping condition indicators to services.
#'   \item \code{ns_cirms_list}: List of binary Condition Indicator Relevance Matrices.
#'   \item \code{ns_habitat_extent}: Data frame of habitat area per year.
#'   \item \code{ns_ci_score_matrix}: Data frame of yearly scores for each condition indicator.
#' }
#' @keywords internal
import_ns_data <- function(path, year_list = 2000:2022, tir_constant = 2) {
  year_list <- as.character(year_list)

  # 1. HABITAT AND ECOSYSTEM SERVICE LABEL TREES
  habitat_tree <- get_ns_habitat_tree(
    path = path,
    sheet = 3,
    bh_range = "C4:C34",
    dh_ranges = c("E4:E6", "E7", "E8:E11", "E12:E16", "E17:E20",
                  "E21:E25", "E26:E27", "E28:E29", "E30:E33", "E34")
  )
  broad_habitats <- names(habitat_tree)
  all_habitat_labels <- unlist(habitat_tree, use.names = FALSE)

  es_tree <- get_ns_es_tree(
    path = path,
    sheet = 3,
    est_range = "F1:AC1",
    es_code_ranges = c("F2:Q2", "R2:AB2", "AC2:AG2"),
    es_name_ranges = c("F3:Q3", "R3:AB3", "AC3:AG3")
  )
  service_types <- names(es_tree)
  all_service_labels <- unlist(es_tree, use.names = FALSE)

  # 2. ECOSYSTEM SERVICE POTENTIAL WEIGHTING (esppu)
  esppu <- readxl::read_xlsx(path = path,
                             sheet = 3,
                             range = "F4:AG34",
                             col_names = FALSE,
                             col_types = "numeric",
                             trim_ws = TRUE,
                             .name_repair = "minimal") %>%
    label_ncai_matrix(habitats_label_tree = habitat_tree,
                      es_label_tree = es_tree)

  # 2b. Custom divisor matrix
  habitats_to_adjust = c(rep("b1",7), rep("b2",5), rep("b3",5), "d1",
                         rep("i2",6), rep("j1",5), rep("j2",5))
  # Note that just the start of the name is enough here.
  services_to_adjust = unlist(c("mediation_of_mass_flows", "soil_formation_and_composition",
                                es_tree[["cultural"]],
                                es_tree[["cultural"]],
                                es_tree[["cultural"]],
                                "global_regional",
                                "global_regional",
                                es_tree[["cultural"]],
                                es_tree[["cultural"]],
                                es_tree[["cultural"]]))

  custom_divisor_matrix <- make_custom_divisor_matrix(
    all_habitat_labels = all_habitat_labels,
    all_es_labels = all_service_labels,
    habitats_to_adjust = habitats_to_adjust,
    services_to_adjust = services_to_adjust,
    usual_divisor = 5,
    custom_divisor = 1
  )

  # 3. IMPORTANCE WEIGHTING
  between_importance_scores <- readxl::read_xlsx(
    path = path,
    sheet = 4,
    range = "D6:D8",
    col_names = "score",
    col_types = "numeric",
    trim_ws = TRUE
  ) %>%
    dplyr::pull(.data$score) %>%
    as.list() %>%
    stats::setNames(service_types)

  within_importance_scores <- get_ns_importance_scores(
    path = path,
    sheet = 4,
    importance_ranges = list(
      provisioning = "D13:D24",
      regulation_and_maintenance = "D29:D39",
      cultural = "D44:D48"
    ),
    es_tree = es_tree
  )

  # 4. CONDITION INDICATORS
  indicator_directory <- readxl::read_xlsx(
    path = path,
    sheet = 8,
    range = "A3:R106",
    col_names = FALSE,
    col_types = NULL,
    trim_ws = TRUE,
    .name_repair = "unique"
  ) %>%
    as.data.frame() %>%
    dplyr::select(1, 4, 14, 15, 16, 18) %>%
    stats::setNames(c("raw_code", "raw_name", service_types, "used")) %>%
    dplyr::filter(.data$used == "Yes") %>%
    dplyr::mutate(
      ci_id = janitor::make_clean_names(paste(.data$raw_name, .data$raw_code, sep = "_"), case = "snake"),
      dplyr::across(dplyr::all_of(service_types), as.numeric)
    ) %>%
    dplyr::select("ci_id", dplyr::all_of(service_types))

  ci_ids <- as.character(indicator_directory$ci_id)

  cirms_list <- get_ns_cirm_list(
    path = path,
    sheet_list = 9:46,
    matrix_range = "F4:AG34",
    ci_ids = ci_ids,
    all_service_labels = all_service_labels,
    all_habitat_labels = all_habitat_labels
  )

  # Check that there is a sheet for each condition indicator marked
  # yes in the directory:
  if (length(ci_ids) != length(9:46)) {
    stop("Number of 'Yes' indicators in directory does not match sheet range 9:46.")
  }

  # 5. ENVIRONMENTAL MEASUREMENTS
  habitat_extent <- readxl::read_xlsx(
    path = path,
    sheet = 5,
    range = "E4:AA34",
    col_names = FALSE,
    col_types = "numeric",
    trim_ws = TRUE,
    .name_repair = "minimal"
  ) %>%
    as.data.frame()
  names(habitat_extent) <- year_list
  rownames(habitat_extent) <- all_habitat_labels

  ci_score_matrix <- read_the_ci_scores(path = path,
                                        sheet_list = 9:46,
                                        vector_range = "I36:I58",
                                        ci_ids = ci_ids)
  rownames(ci_score_matrix) <- year_list

  return(list(
    ns_habitat_extent = habitat_extent,
    ns_ci_score_matrix = ci_score_matrix,
    ns_habitats_label_tree = habitat_tree,
    ns_es_label_tree = es_tree,
    ns_year_list = year_list,
    ns_esppu = esppu,
    ns_custom_divisor_matrix = custom_divisor_matrix,
    ns_between_importance_scores = between_importance_scores,
    ns_within_importance_scores = within_importance_scores,
    ns_cirms_list = cirms_list,
    ns_indicator_directory = indicator_directory
  ))
}

#' Extract and Clean Habitat Classification Tree
#'
#' @param path Path to Excel source.
#' @param sheet Sheet index.
#' @param bh_range Range for broad habitats.
#' @param dh_ranges Vector of ranges for detailed habitats.
#' @keywords internal
get_ns_habitat_tree <- function(path, sheet, bh_range, dh_ranges) {
  broad_habitats <- readxl::read_xlsx(
    path = path, sheet = sheet, range = bh_range,
    col_names = "category", col_types = "text", trim_ws = TRUE
  ) %>%
    dplyr::filter(!is.na(.data$category)) %>%
    dplyr::mutate(category = janitor::make_clean_names(.data$category, case = "snake")) %>%
    dplyr::pull(.data$category)

  full_category_names <- c(broad_habitats[1], "c_inland_surface_waters",
                           broad_habitats[2:length(broad_habitats)], "k_montane")

  habitat_tree <- lapply(dh_ranges, function(rng) {
    readxl::read_xlsx(path = path, sheet = sheet, range = rng,
                      col_names = "label", col_types = "text", trim_ws = TRUE) %>%
      dplyr::filter(!is.na(.data$label)) %>%
      dplyr::mutate(label = janitor::make_clean_names(.data$label, case = "snake")) %>%
      dplyr::pull(.data$label)
  })

  names(habitat_tree) <- full_category_names
  return(habitat_tree)
}

#' Extract and Clean Ecosystem Service Tree
#'
#' @param path Path to Excel source.
#' @param sheet Sheet index.
#' @param est_range Range for ES categories.
#' @param es_code_ranges Vector of ranges for ES codes.
#' @param es_name_ranges Vector of ranges for ES names.
#' @keywords internal
get_ns_es_tree <- function(path, sheet, est_range, es_code_ranges, es_name_ranges) {
  es_categories <- readxl::read_xlsx(path = path, sheet = sheet, range = est_range,
                                     col_names = FALSE, col_types = "text", trim_ws = TRUE) %>%
    unlist(use.names = FALSE) %>%
    stats::na.omit() %>%
    janitor::make_clean_names(case = "snake")

  es_tree <- lapply(seq_along(es_code_ranges), function(i) {
    codes <- readxl::read_xlsx(path = path, sheet = sheet, range = es_code_ranges[i],
                               col_names = FALSE, col_types = "text", trim_ws = TRUE) %>%
      unlist(use.names = FALSE) %>% as.character()

    names_full <- readxl::read_xlsx(path = path, sheet = sheet, range = es_name_ranges[i],
                                    col_names = FALSE, col_types = "text", trim_ws = TRUE) %>%
      unlist(use.names = FALSE)

    combined_labels <- janitor::make_clean_names(paste(names_full, codes, sep = "_"), case = "snake")
    if (i == 1) combined_labels[1] <- "cultivated_crops_1_1"
    return(combined_labels)
  })

  names(es_tree) <- es_categories
  return(es_tree)
}

#' Extract Ecosystem Service Importance Scores
#'
#' @param path Path to Excel source.
#' @param sheet Sheet index.
#' @param importance_ranges Named list of Excel ranges for each service type.
#' @param es_tree The cleaned ES label tree.
#' @keywords internal
get_ns_importance_scores <- function(path, sheet, importance_ranges, es_tree) {
  importance_scores_list <- lapply(names(importance_ranges), function(service_type) {
    rng <- importance_ranges[[service_type]]
    scores_vec <- readxl::read_xlsx(path = path, sheet = sheet, range = rng,
                                    col_names = "score", col_types = "numeric", trim_ws = TRUE) %>%
      dplyr::pull(.data$score)
    names(scores_vec) <- es_tree[[service_type]]
    return(as.list(scores_vec))
  })
  names(importance_scores_list) <- names(importance_ranges)
  return(importance_scores_list)
}

#' Extract List of Condition Indicator Relevance Matrices (CIRMs)
#'
#' @param path Path to Excel source.
#' @param sheet_list Vector of sheet indices to iterate through.
#' @param matrix_range Excel range for the binary matrices.
#' @param ci_ids Vector of indicator IDs.
#' @param all_service_labels Flat vector of all service labels.
#' @param all_habitat_labels Flat vector of all habitat labels.
#' @keywords internal
get_ns_cirm_list <- function(path, sheet_list, matrix_range, ci_ids,
                             all_service_labels, all_habitat_labels) {

  list_of_dfs <- lapply(sheet_list, function(current_sheet) {
    data <- readxl::read_xlsx(path = path, sheet = current_sheet, range = matrix_range,
                              col_names = FALSE, col_types = "numeric", trim_ws = TRUE,
                              .name_repair = "minimal") %>%
      as.data.frame() %>%
      stats::setNames(all_service_labels)

    # Debug if label mismatch
    if (nrow(data) != length(all_habitat_labels)) {
      stop(paste("Row mismatch in CIRM! Matrix has", nrow(data),
                 "rows but labels have", length(all_habitat_labels)))
    }

    # Apply habitat labels as row names
    row.names(data) <- all_habitat_labels

    # Convert to binary 1/0
    data[] <- lapply(data, function(x) ifelse(!is.na(x) & x > 0, 1, 0))

    return(data)
  })

  names(list_of_dfs) <- ci_ids
  return(list_of_dfs)
}

#' Generate Custom Divisor Matrix
#'
#' @param all_habitat_labels Vector of habitat labels.
#' @param all_es_labels Vector of service labels.
#' @param habitats_to_adjust Vector of habitat shorthands (e.g., "b1").
#' @param services_to_adjust Vector of service keywords for partial matching.
#' @param usual_divisor Numeric default divisor (e.g., 5).
#' @param custom_divisor Numeric adjustment divisor (e.g., 1).
#' @keywords internal
make_custom_divisor_matrix <- function(all_habitat_labels, all_es_labels,
                                       habitats_to_adjust, services_to_adjust,
                                       usual_divisor, custom_divisor) {
  htst <- expand.grid(habitat = all_habitat_labels, service_type = all_es_labels,
                      stringsAsFactors = FALSE) %>%
    dplyr::mutate(divisor = usual_divisor)

  for (i in seq_along(habitats_to_adjust)) {
    h_pattern <- paste0("^", habitats_to_adjust[i])
    s_pattern <- paste0("^", services_to_adjust[i])
    matches <- grepl(h_pattern, htst$habitat) & grepl(s_pattern, htst$service_type)
    htst$divisor[matches] <- custom_divisor
  }

  htst_wide <- htst %>%
    tidyr::pivot_wider(
      names_from = "service_type", # Use strings here
      values_from = "divisor"      # Use strings here
    ) %>%
    as.data.frame()

  htst_wide <- htst_wide[, !names(htst_wide) %in% "habitat"]
  rownames(htst_wide) <- all_habitat_labels

  return(as.data.frame(htst_wide))
}

#' Read Yearly Condition Indicator Scores
#'
#' @param path Path to Excel source.
#' @param sheet_list Vector of sheet indices.
#' @param vector_range Range for the single-column score vectors.
#' @param ci_ids Vector of indicator IDs.
#' @keywords internal
read_the_ci_scores <- function(path, sheet_list, vector_range, ci_ids) {
  list_of_vectors <- list()
  for (idx in seq_along(sheet_list)) {
    actual_sheet_index <- sheet_list[idx]
    raw_score_data <- readxl::read_xlsx(path = path, sheet = actual_sheet_index,
                                        range = vector_range, col_names = FALSE,
                                        col_types = "numeric", trim_ws = TRUE,
                                        .name_repair = "minimal")
    list_of_vectors[[as.character(ci_ids[idx])]] <- as.numeric(raw_score_data[[1]])
    cat("Processed Sheet", actual_sheet_index, "(CI ID:", ci_ids[idx], ")\n")
  }
  return(as.data.frame(list_of_vectors, check.names = FALSE))
}
