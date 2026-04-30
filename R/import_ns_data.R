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
#' @return A named list containing 14 components:
#' \itemize{
#'   \item \code{ns_habitat_extent}: Data frame of habitat area per year.
#'   \item \code{ns_ci_scores}: Data frame of yearly scores for each condition indicator.
#'   \item \code{ns_habitats_label_tree}: Nested list of cleaned broad and detailed habitat labels.
#'   \item \code{ns_es_label_tree}: Nested list of cleaned ecosystem service types and specific labels.
#'   \item \code{ns_year_list}: Character vector of years included in the account.
#'   \item \code{ns_esppu_scores}: Data frame of Ecosystem Service Potential Per Unit scores.
#'   \item \code{ns_custom_divisor_matrix}: Matrix of divisors for ESPPU normalization.
#'   \item \code{ns_between_importance_scores}: Named list of broad ES importance weights.
#'   \item \code{ns_within_importance_scores}: Named list of weights for specific services.
#'   \item \code{ns_ci_relevance_matrices}: List of binary Condition Indicator Relevance Matrices.
#'   \item \code{ns_indicator_directory}: Data frame mapping condition indicators to services.
#'   \item \code{ns_dirty_habitats_label_tree}: Nested list of original habitat names for display.
#'   \item \code{ns_dirty_es_label_tree}: Nested list of original ecosystem service names for display.
#'   \item \code{ns_dirty_ci_names}: Character vector of condition indicator names in "Number Name" format.
#' }
#'
#' @importFrom janitor make_clean_names
#' @importFrom readxl read_xlsx read_excel
#' @importFrom dplyr mutate select filter across all_of arrange pull
#' @importFrom tidyr fill
#' @importFrom stringr str_squish str_starts
#' @importFrom stats setNames
#' @keywords internal
#' Import and Process Natural Capital Account Data
#'
#' @param path A string representing the file path to the .xlsx data source.
#' @param year_list A vector of years to include in the account (e.g., 2000:2022).
#' @param tir_constant A numeric constant added to the Total Indicator Relevances.
#'
#' @return A named list containing structured NCAI data objects.
#' @importFrom janitor make_clean_names
#' @importFrom readxl read_xlsx read_excel
#' @importFrom dplyr mutate select filter across all_of arrange pull
#' @importFrom tidyr fill
#' @importFrom stringr str_squish str_starts
#' @importFrom stats setNames
#' @keywords internal
import_ns_data <- function(path, year_list = 2000:2022, tir_constant = 2) {
  year_list <- as.character(year_list)

  # 1. HABITAT LABELS & TREE
  raw_habs_df <- readxl::read_excel(
    path, sheet = "ES Potential per SPU", range = "C4:E34",
    col_names = c("broad_cat", "code", "name"), col_types = "text"
  ) %>%
    tidyr::fill("broad_cat", .direction = "down") %>%
    dplyr::filter(!is.na(.data$name)) %>%
    dplyr::mutate(print_name = stringr::str_squish(.data$name))

  # Manual fix for broad categories
  raw_habs_df$broad_cat[stringr::str_starts(raw_habs_df$print_name, "C ")] <- "C. INLAND SURFACE WATERS"
  raw_habs_df$broad_cat[stringr::str_starts(raw_habs_df$print_name, "K ")] <- "K. MONTANE"

  # FLAT LABELS: Use these for matrix dimensions (Length: 31)
  all_habitat_labels <- janitor::make_clean_names(raw_habs_df$print_name)
  # print(length(all_habitat_labels))

  # TREE STRUCTURE: Build the list manually to ensure character vectors
  hab_order <- c("B. COASTAL HABITATS", "C. INLAND SURFACE WATERS", "D. MIRES, BOGS AND FENS",
                 "E. GRASSLANDS AND LANDS DOMINATED BY FORBS, MOSSES OR LICHENS",
                 "F. HEATHLAND, SCRUB AND TUNDRA", "G. WOODLAND, FOREST AND OTHER WOODED LAND",
                 "H. INLAND UNVEGETATED OR SPARSELY VEGETATED HABITATS",
                 "I. CULTIVATED AGRICULTURAL, HORTICULTURAL AND DOMESTIC HABITATS",
                 "J. CONSTRUCTED, INDUSTRIAL AND OTHER ARTIFICIAL HABITATS", "K. MONTANE")

  habitat_tree <- lapply(hab_order, function(b) {
    janitor::make_clean_names(raw_habs_df$print_name[raw_habs_df$broad_cat == b])
  }) %>% stats::setNames(janitor::make_clean_names(hab_order))

  dirty_habitats_label_tree <- lapply(hab_order, function(b) {
    raw_habs_df$print_name[raw_habs_df$broad_cat == b]
  }) %>% stats::setNames(hab_order)


  # 2. ES LABELS & TREE
  raw_es_header <- readxl::read_excel(
    path, sheet = "ES Potential per SPU", range = "F1:AG3",
    col_names = FALSE, col_types = "text"
  )

  raw_es_df <- as.data.frame(t(raw_es_header)) %>%
    dplyr::rename("es_type" = 1, "code" = 2, "name" = 3) %>%
    tidyr::fill("es_type", .direction = "down") %>%
    dplyr::mutate(
      es_type = stringr::str_squish(.data$es_type),
      print_name = stringr::str_squish(paste(ifelse(is.na(.data$code), "", .data$code), .data$name))
    )

  # FLAT LABELS: Create clean names and strip the leading 'x'
  all_service_labels <- raw_es_df %>%
    dplyr::mutate(
      # Create the concatenated string: "1.1.1 Cultivated crops"
      full_str = paste(.data$code, .data$name),
      # Clean it: results in "x1_1_1_cultivated_crops"
      clean = janitor::make_clean_names(.data$full_str),
      # Strip the leading 'x' using regex
      clean = gsub("^x", "", .data$clean),
      # Apply your specific Crops fix
      clean = ifelse(.data$clean == "1_1_1_cultivated_crops", "1_1_cultivated_crops", .data$clean)
    ) %>%
    dplyr::pull(.data$clean)

  # TREE STRUCTURE: Build using the updated flat labels
  es_order <- c("PROVISIONING", "REGULATION AND MAINTENANCE", "CULTURAL")

  # Attach the corrected labels back to the dataframe for tree building
  raw_es_df$clean_name <- all_service_labels

  es_tree <- lapply(es_order, function(e) {
    raw_es_df$clean_name[raw_es_df$es_type == e]
  }) %>% stats::setNames(janitor::make_clean_names(es_order))

  dirty_es_label_tree <- lapply(es_order, function(e) {
    raw_es_df$print_name[raw_es_df$es_type == e]
  }) %>% stats::setNames(es_order)

  service_types <- names(es_tree)

  # 3. ECOSYSTEM SERVICE POTENTIAL (esppu)
  esppu <- readxl::read_xlsx(
    path = path, sheet = 3, range = "F4:AG34",
    col_names = FALSE, col_types = "numeric", trim_ws = TRUE, .name_repair = "minimal"
  ) %>% as.data.frame()

  rownames(esppu) <- all_habitat_labels
  colnames(esppu) <- all_service_labels

  # 4. CUSTOM DIVISOR MATRIX
  habitats_to_adjust <- c(
    rep("b1", 7), rep("b2", 5), rep("b3", 5), # Coastal adjustments
    "d1",                                     # Peatland adjustment
    rep("i2", 6),                             # Urban/Garden adjustments
    rep("j1", 5), rep("j2", 5)                # Constructed land adjustments
  )

  services_to_adjust <- unlist(c(
    "mediation_of_mass_flows",
    "soil_formation_and_composition",
    es_tree[["cultural"]], # Automatically expands to all 5 cultural IDs
    es_tree[["cultural"]],
    es_tree[["cultural"]],
    "global_regional",              # Matches "2_11_global_regional_..."
    "global_regional",
    es_tree[["cultural"]],
    es_tree[["cultural"]],
    es_tree[["cultural"]]
  ))

  custom_divisor_matrix <- make_custom_divisor_matrix(
    all_habitat_labels = all_habitat_labels,
    all_es_labels = all_service_labels,
    habitats_to_adjust = habitats_to_adjust,
    services_to_adjust = services_to_adjust,
    usual_divisor = 5,
    custom_divisor = 1
  )

  # 5. IMPORTANCE WEIGHTS
  between_importance_scores <- readxl::read_xlsx(
    path = path, sheet = 4, range = "D6:D8", col_names = "score", col_types = "numeric"
  ) %>% dplyr::pull(.data$score) %>% as.list() %>% stats::setNames(service_types)

  within_importance_scores <- get_ns_importance_scores(
    path = path, sheet = 4,
    importance_ranges = list(provisioning = "D13:D24", regulation_and_maintenance = "D29:D39", cultural = "D44:D48"),
    es_tree = es_tree
  )

  # 6. CONDITION INDICATORS (Unified Logic)
  raw_ind_data <- readxl::read_excel(
    path = path, sheet = "Indicator Directory", range = "A3:R106",
    col_names = FALSE, col_types = "text"
  ) %>% as.data.frame() %>%
    dplyr::select(1, 4, 14, 15, 16, 18) %>%
    stats::setNames(c("num", "name", service_types, "used")) %>%
    dplyr::filter(.data$used == "Yes") %>%
    dplyr::mutate(
      num = stringr::str_squish(.data$num),
      name = stringr::str_squish(.data$name),
      dirty_name = paste(.data$num, .data$name),
      # Create the clean ID
      ci_id = janitor::make_clean_names(.data$dirty_name, case = "snake"),
      # STRIP THE LEADING 'x'
      ci_id = gsub("^x", "", .data$ci_id),
      dplyr::across(dplyr::all_of(service_types), as.numeric)
    )

  dirty_ci_names <- raw_ind_data$dirty_name
  ci_ids         <- raw_ind_data$ci_id # Now contains clean IDs without 'x'
  indicator_directory <- raw_ind_data %>% dplyr::select("ci_id", dplyr::all_of(service_types))


  # 7. MATRICES AND MEASUREMENTS
  ci_relevance_matrices <- get_ns_cirm_list(
    path = path, sheet_list = 9:46, matrix_range = "F4:AG34",
    ci_ids = ci_ids, all_service_labels = all_service_labels, all_habitat_labels = all_habitat_labels
  )

  habitat_extent <- readxl::read_xlsx(
    path = path, sheet = 5, range = "E4:AA34", col_names = FALSE, col_types = "numeric"
  ) %>% as.data.frame()
  names(habitat_extent) <- year_list
  rownames(habitat_extent) <- all_habitat_labels

  ci_scores <- read_the_ci_scores(
    path = path, sheet_list = 9:46, vector_range = "I36:I58", ci_ids = ci_ids
  )
  rownames(ci_scores) <- year_list

  return(list(
    ns_habitat_extent = habitat_extent,
    ns_ci_scores = ci_scores,
    ns_habitats_label_tree = habitat_tree,
    ns_es_label_tree = es_tree,
    ns_year_list = year_list,
    ns_esppu_scores = esppu,
    ns_custom_divisor_matrix = custom_divisor_matrix,
    ns_between_importance_scores = between_importance_scores,
    ns_within_importance_scores = within_importance_scores,
    ns_ci_relevance_matrices = ci_relevance_matrices,
    ns_indicator_directory = indicator_directory,
    ns_dirty_habitats_label_tree = dirty_habitats_label_tree,
    ns_dirty_es_label_tree = dirty_es_label_tree,
    ns_dirty_ci_names = dirty_ci_names
  ))
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
                                    col_names = "score", col_types = "numeric", trim_ws = TRUE,
                                    .name_repair = "minimal",
                                    progress = FALSE) %>%
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
#' Creates a matrix of normalization divisors (e.g., scoring scales) for every
#' habitat-service combination. It allows for specific adjustments where certain
#' services or habitats use a different scale (e.g., a 1-point scale instead
#' of a 5-point scale).
#'
#' @param all_habitat_labels A character vector of all cleaned habitat names.
#' @param all_es_labels A character vector of all cleaned ecosystem service names.
#' @param habitats_to_adjust A character vector of habitat shorthands or patterns
#'   (e.g., "b1") to be matched for custom divisors.
#' @param services_to_adjust A character vector of service shorthands or patterns
#'   to be matched for custom divisors.
#' @param usual_divisor Numeric. The default divisor applied to most combinations (e.g., 5).
#' @param custom_divisor Numeric. The adjustment divisor for specified matches (e.g., 1).
#'
#' @return A data frame where rows represent habitats and columns represent
#'   ecosystem services, containing the divisor values for each intersection.
#'
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_wider
#' @keywords internal
make_custom_divisor_matrix <- function(all_habitat_labels, all_es_labels,
                                       habitats_to_adjust, services_to_adjust,
                                       usual_divisor, custom_divisor) {

  # Safety check for documentation/reproducibility
  if (length(habitats_to_adjust) != length(services_to_adjust)) {
    stop("habitats_to_adjust and services_to_adjust must be the same length.")
  }

  htst <- expand.grid(habitat = all_habitat_labels, service_type = all_es_labels,
                      stringsAsFactors = FALSE) %>%
    dplyr::mutate(divisor = usual_divisor)

  for (i in seq_along(habitats_to_adjust)) {
    # We keep ^ for habitats (e.g., ^b1) because they start with the code.
    h_pattern <- paste0("^", habitats_to_adjust[i])

    # We REMOVE ^ for services because the name now follows a numeric prefix.
    # This allows "mediation_of_mass_flows" to match "2_3_mediation_of_mass_flows".
    s_pattern <- services_to_adjust[i]

    matches <- grepl(h_pattern, htst$habitat) & grepl(s_pattern, htst$service_type)
    htst$divisor[matches] <- custom_divisor
  }

  htst_wide <- htst %>%
    tidyr::pivot_wider(
      names_from = "service_type",
      values_from = "divisor"
    ) %>%
    as.data.frame()

  # Clean up and restore row order based on original habitat list
  rownames(htst_wide) <- htst_wide$habitat
  htst_wide <- htst_wide[all_habitat_labels, all_es_labels]

  return(htst_wide)
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
                                        .name_repair = "minimal",
                                        progress = FALSE)
    list_of_vectors[[as.character(ci_ids[idx])]] <- as.numeric(raw_score_data[[1]])
    # cat("Processed Sheet", actual_sheet_index, "(CI ID:", ci_ids[idx], ")\n")
  }
  return(as.data.frame(list_of_vectors, check.names = FALSE))
}
