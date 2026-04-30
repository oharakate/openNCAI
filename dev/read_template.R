#' Read and Clean NCAI Template
#'
#' @param path Path to the populated .xlsx file
#' @param habitats_label_tree The same dirty list used for template creation
#' @param es_label_tree The same dirty list used for template creation
#' @param ci_names The same dirty vector of indicator names
#' Read and Clean Template Data
#'
#' @param path Path to the populated .xlsx file
#' @param habitats_label_tree The original dirty list used for template creation
#' @param es_label_tree The original dirty list used for template creation
#' @param ci_names The original dirty vector of indicator names
read_ncai_template <- function(path,
                               habitats_label_tree,
                               es_label_tree,
                               ci_names) {

  # --- 1. SETUP & CLEANING ---
  clean_vec <- function(x) {
    x %>%
      stringr::str_trim() %>%
      stringr::str_replace_all("[[:punct:] ]+", "_") %>%
      stringr::str_to_lower() %>%
      stringr::str_replace_all("^_+|_+$", "")
  }

  clean_habitats_label_tree <- lapply(habitats_label_tree, clean_vec)
  names(clean_habitats_label_tree) <- clean_vec(names(habitats_label_tree))

  clean_es_label_tree <- lapply(es_label_tree, clean_vec)
  names(clean_es_label_tree) <- clean_vec(names(es_label_tree))

  # Cast to pure character vectors to avoid any underlying attribute issues
  all_clean_habs <- as.character(unlist(clean_habitats_label_tree, use.names = FALSE))
  all_clean_es   <- as.character(unlist(clean_es_label_tree, use.names = FALSE))
  all_clean_cis  <- as.character(clean_vec(ci_names))

  n_habs  <- length(all_clean_habs)
  n_es    <- length(all_clean_es)
  n_cis   <- length(all_clean_cis)
  n_types <- length(clean_es_label_tree)

  col_to_lab <- function(n) openxlsx::int2col(n)

  # --- 2. HABITAT EXTENT ---
  ext_headers <- readxl::read_excel(path, sheet = "Habitat Extent", n_max = 0)
  n_years     <- ncol(ext_headers) - 1
  extent_range <- sprintf("A2:%s%d", col_to_lab(1 + n_years), 1 + n_habs)

  extent_raw <- readxl::read_excel(path, sheet = "Habitat Extent", range = extent_range, col_names = FALSE)
  # "Wash" the data through a matrix to strip readxl attributes
  habitat_extent <- as.data.frame(as.matrix(extent_raw[, -1]))
  habitat_extent[] <- lapply(habitat_extent, as.numeric) # Ensure numeric

  rownames(habitat_extent) <- all_clean_habs
  colnames(habitat_extent) <- as.character(colnames(ext_headers)[-1])
  year_list <- colnames(habitat_extent)

  # --- 3. PROVISION PER UNIT ---
  esppu_range <- sprintf("A2:%s%d", col_to_lab(1 + n_es), 1 + n_habs)
  esppu_raw   <- readxl::read_excel(path, sheet = "Provision Per Unit", range = esppu_range, col_names = FALSE)

  esppu_scores <- as.data.frame(as.matrix(esppu_raw[, -1]))
  esppu_scores[] <- lapply(esppu_scores, as.numeric)

  rownames(esppu_scores) <- all_clean_habs
  colnames(esppu_scores) <- all_clean_es

  # --- 4. IMPORTANCE WEIGHTS ---
  step1_range <- sprintf("B3:B%d", 3 + n_types - 1)
  imp_between_raw <- readxl::read_excel(path, sheet = "Importance", range = step1_range, col_names = FALSE)
  between_importance <- setNames(as.list(as.numeric(imp_between_raw[[1]])), names(clean_es_label_tree))

  all_imp_col <- readxl::read_excel(path, sheet = "Importance", range = "B1:B500", col_names = FALSE)[[1]]
  header_indices <- which(all_imp_col == "Raw_Score")

  within_importance <- list()
  for (i in seq_along(header_indices)) {
    type_name <- names(clean_es_label_tree)[i]
    group_len <- length(clean_es_label_tree[[i]])
    scores <- as.numeric(all_imp_col[(header_indices[i] + 1):(header_indices[i] + group_len)])
    # Critical: Ensure names match and it's a simple list of numbers
    names(scores) <- clean_es_label_tree[[i]]
    within_importance[[type_name]] <- as.list(scores)
  }

  # --- 5. CONDITION SCORES ---
  scores_range <- sprintf("A2:%s%d", col_to_lab(1 + n_cis), 2 + length(year_list) - 1)
  scores_raw   <- readxl::read_excel(path, sheet = "Condition Indicator Scores", range = scores_range, col_names = FALSE)

  ci_scores <- as.data.frame(as.matrix(scores_raw[, -1]))
  ci_scores[] <- lapply(ci_scores, as.numeric)

  rownames(ci_scores) <- as.character(scores_raw[[1]])
  colnames(ci_scores) <- all_clean_cis

  # --- 6. INDICATOR DIRECTORY ---
  dir_range <- sprintf("A2:%s%d", col_to_lab(1 + n_types), 1 + n_cis)
  dir_raw   <- readxl::read_excel(path, sheet = "Indicator Directory", range = dir_range, col_names = FALSE)

  indicator_directory <- as.data.frame(as.matrix(dir_raw))
  # Ensure numeric ES types, character ID
  indicator_directory[, 2:ncol(indicator_directory)] <- lapply(indicator_directory[, 2:ncol(indicator_directory)], as.numeric)

  colnames(indicator_directory) <- c("ci_id", names(clean_es_label_tree))
  indicator_directory$ci_id     <- all_clean_cis

  # --- 7. RELEVANCE MATRICES ---
  ci_relevance_matrices <- list()
  rel_range <- sprintf("A2:%s%d", col_to_lab(1 + n_es), 1 + n_habs)

  for (i in seq_along(ci_names)) {
    ci_dirty  <- ci_names[i]
    sheet_tab <- trimws(substr(stringr::str_replace_all(ci_dirty, "[[:punct:]]", " "), 1, 31))

    rel_raw <- readxl::read_excel(path, sheet = sheet_tab, range = rel_range, col_names = FALSE)
    rel_df  <- as.data.frame(as.matrix(rel_raw[, -1]))
    rel_df[] <- lapply(rel_df, as.numeric)

    rownames(rel_df) <- all_clean_habs
    colnames(rel_df) <- all_clean_es
    ci_relevance_matrices[[all_clean_cis[i]]] <- rel_df
  }

  return(list(
    clean_habitats_label_tree = clean_habitats_label_tree,
    clean_es_label_tree       = clean_es_label_tree,
    year_list                 = year_list,
    habitat_extent            = habitat_extent,
    ci_scores                 = ci_scores,
    esppu_scores              = esppu_scores,
    between_importance        = between_importance,
    within_importance         = within_importance,
    indicator_directory       = indicator_directory,
    ci_relevance_matrices     = ci_relevance_matrices
  ))
}

new_objects_list <- read_ncai_template("dev/NCAI_Data_Entry_Template.xlsx",
                               ns_dirty_habitats_label_tree,
                               ns_dirty_es_label_tree,
                               ns_dirty_ci_names)

names(new_objects_list)


# Create NS custom divisor matrix (adjustments to provision per unit)
# NOTE TO NATURESCOT: You could take this opportunity to just adjust the
# provision per unit weights in that weight matrix. I.e. divide everything
# in the affected cells by 0.2 (check that!), and not have to use this divisor
# matrix any more.
new_divisor_matrix <- ns_custom_divisor_matrix
rownames(new_divisor_matrix) <- unlist(new_objects_list$clean_habitats_label_tree, use.names = FALSE)
colnames(new_divisor_matrix) <- unlist(new_objects_list$clean_es_label_tree, use.names = FALSE)


get_ncai(habitat_extent = new_objects_list$habitat_extent,
         ci_scores = new_objects_list$ci_scores,
         habitats_label_tree = new_objects_list$clean_habitats_label_tree,
         es_label_tree = new_objects_list$clean_es_label_tree,
         year_list = new_objects_list$year_list,
         year_one = new_objects_list$year_list[1],
         esppu_scores = new_objects_list$esppu_scores,
         custom_divisor_matrix = new_divisor_matrix,
         between_importance_scores = new_objects_list$between_importance,
         within_importance_scores = new_objects_list$within_importance,
         ci_relevance_matrices = new_objects_list$ci_relevance_matrices,
         indicator_directory = new_objects_list$indicator_directory,
         return = "everything")


get_ncai(habitat_extent = ns_habitat_extent,
         ci_scores = get_ncai(habitat_extent = new_objects_list$habitat_extent,
         ci_scores = new_objects_list$ci_scores,
         habitats_label_tree = new_objects_list$clean_habitats_label_tree,
         es_label_tree = new_objects_list$clean_es_label_tree,
         year_list = new_objects_list$year_list,
         year_one = new_objects_list$year_list[1],
         esppu_scores = new_objects_list$esppu_scores,
         custom_divisor_matrix = new_divisor_matrix,
         between_importance_scores = new_objects_list$between_importance,
         within_importance_scores = new_objects_list$within_importance,
         ci_relevance_matrices = new_objects_list$ci_relevance_matrices,
         indicator_directory = new_objects_list$indicator_directory,
         return = "everything"),
         habitats_label_tree = new_objects_list$clean_habitats_label_tree,
         es_label_tree = new_objects_list$clean_es_label_tree,
         year_list = new_objects_list$year_list,
         year_one = new_objects_list$year_list[1],
         esppu_scores = new_objects_list$esppu_scores,
         custom_divisor_matrix = new_divisor_matrix,
         between_importance_scores = new_objects_list$between_importance,
         within_importance_scores = new_objects_list$within_importance,
         ci_relevance_matrices = new_objects_list$ci_relevance_matrices,
         indicator_directory = new_objects_list$indicator_directory,
         return = "everything")
