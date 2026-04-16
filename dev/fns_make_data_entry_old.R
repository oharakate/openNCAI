# Note that this will make a data entry spreadsheet for the version of the
# the NCAI spreadsheet to 2022.

# Functions and code to build a data entry spreadsheet for NatureScot
# It will collect the habitats and ecosystem services metadata
# Kate O'Hara
# 25-03-2026




#### NEW #####

#' Write Standard Input Matrix to a Sheet (Direct Write)
#'
#' @param wb A workbook object
#' @param sheet_name Name of the sheet to create/write to
#' @param data_df The data frame containing matrix values (pre-sorted)
#' @param hab_labels Character vector of habitat names (Rows)
#' @param es_labels Character vector of ES names (Columns)
#' @param style_obj A list containing our createStyle objects
write_input_matrix <- function(wb, sheet_name, data_df, hab_labels, es_labels, style_obj) {

  # 1. Create the sheet
  openxlsx::addWorksheet(wb, sheet_name)

  # 2. Write Labels
  # Row 1: ES Labels across columns
  openxlsx::writeData(wb, sheet_name, t(es_labels), startCol = 2, startRow = 1, colNames = FALSE)

  # Column 1: Habitat Labels down rows
  openxlsx::writeData(wb, sheet_name, hab_labels, startCol = 1, startRow = 2, colNames = FALSE)

  # 3. Write Data Directly
  # We assume data_df rows match hab_labels and columns match es_labels
  openxlsx::writeData(wb, sheet_name, data_df,
                      startCol = 2, startRow = 2,
                      colNames = FALSE, rowNames = FALSE)

  # 4. Apply Styles
  # Vertical Headers for ES
  openxlsx::addStyle(wb, sheet_name, style = style_obj$vert,
                     rows = 1, cols = 2:(1 + length(es_labels)))

  # Grid Body
  openxlsx::addStyle(wb, sheet_name, style = style_obj$body,
                     rows = 2:(1 + length(hab_labels)),
                     cols = 2:(1 + length(es_labels)),
                     gridExpand = TRUE)

  # 5. Dimensions
  openxlsx::setColWidths(wb, sheet_name, cols = 2:(1 + length(es_labels)), width = 5)
  openxlsx::setRowHeights(wb, sheet_name, rows = 1, heights = 150)
  openxlsx::setColWidths(wb, sheet_name, cols = 1, width = 45)

  # 6. Freeze Panes (Keeps labels visible while scrolling)
  openxlsx::freezePane(wb, sheet_name, firstActiveRow = 2, firstActiveCol = 2)
}








#### EXECUTE ####
library(openxlsx)
library(readxl)
library(openNCAI)
library(tidyr)

# Path for output
template_out <- file.path("dev", "template2.xlsx")
original_spreadsheet <- file.path("data-raw", "ncai_corrected.xlsx")

# Bring in NatureScot data items

# Create the workbook object in R's memory
wb <- createWorkbook()

# Defining these once makes your code much cleaner later
header_style <- createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  fgFill = "#DCE6F1", # Light blue background
  border = "TopBottomLeftRight"
)

body_style <- createStyle(
  halign = "left",
  border = "TopBottomLeftRight"
)

# We can add sheets one by one or in a loop
addWorksheet(wb, "Provision_per_Unit")

# Save the workbook once
saveWorkbook(wb, "test_template.xlsx", overwrite = TRUE)


















##### OLD ####


#### Code used to import NatureScot data at 08-04-2026 ####
sheet_path <- file.path("data-raw", "ncai_corrected.xlsx")

# METADATA
# Read the HABITATS hierarchy metadata
raw_hab_df <- read.xlsx(
  sheet_path,
  sheet = "ES Potential per SPU",
  startRow = 3,
  check.names = FALSE
)[1:31, 2:3]
# Manual edits to include broad habitat names for all:
raw_hab_df[4,1] <- "C. INLAND SURFACE WATERS"
raw_hab_df[31,1] <- "K. MONTANE"
# View(raw_hab_df)
raw_hab_df <- raw_hab_df %>% tidyr::fill(1)
hab_label_tree <- split(raw_hab_df[[2]], raw_hab_df[[1]])
hab_label_tree <- hab_label_tree[unique(raw_hab_df[[1]])]
# hab_label_tree

# Read the ECOSYSTEM SERVICES hierarchy metadata
raw_es_header <- openxlsx::read.xlsx(
  sheet_path,
  sheet = "ES Potential per SPU",
  rows = 1:3,
  cols = 6:33,
  colNames = FALSE
)
# Transpose and apply names
es_meta_df <- as.data.frame(t(raw_es_header), stringsAsFactors = FALSE)
colnames(es_meta_df) <- c("Category", "Code", "Label")
es_meta_df <- es_meta_df %>% tidyr::fill(Category, .direction = "down")
# Concatenate codes and label of ESs
es_meta_df$Full_Label <- paste(trimws(es_meta_df$Code), trimws(es_meta_df$Label))
# Split into named list
es_label_tree <- split(es_meta_df$Full_Label, es_meta_df$Category)
# Fix the order
original_order <- unique(es_meta_df$Category)
es_label_tree <- es_label_tree[original_order]
# es_label_tree


# Get the condition indicator names and salience weights:
raw_inddir_df <- openxlsx::read.xlsx(
  sheet_path,
  sheet = "Indicator Directory",
  rows = 3:106,
  cols = 1:18,
  colNames = FALSE
)
# Only keep CIs in use
inddir_df <- raw_inddir_df[tolower(raw_inddir_df$"X17") == "yes", ]
# Keep the ids, names and weights
inddir_df <- inddir_df[, c(1, 4, 13:15)]
colnames(inddir_df) <- c("ci_id", "ci_name", names(es_label_tree))
# Clean up rownames
rownames(inddir_df) <- NULL
# View(inddir_df)



# WEIGHTS DATA
# Pick these up from the bundled NatureScot data
ls("package:openNCAI")
ppu <- ns_esppu_scores
demand_between <- ns_between_importance_scores
# demand_between <- ns_between_demand_scores
demand_within <- ns_within_importance_scores




# demand_within <- ns_within_demand_scores
#

# Also need the condition indicator time series
# The habitat extent time series
# The condition indicator relevance matrices



####
# Set up a new template to work with
template_path <- file.path("dev", "template2.xlsx")
template_out <- openxlsx::createWorkbook(template_path)


# Provision per unit weights, but I need to fix this.
openxlsx::writeData(
  wb = template_out,
  sheet = "provision_per_unit",
  x = ppu_mat,
  startCol = 2,
  startRow = 2,
  colNames = FALSE,
  rowNames = FALSE
)





# SETTING UP THE WORKBOOK
wb <- createWorkbook()
template_out = file.path("dev", "template2.xlsx")

# Function to draw input matrices:
#' Write Standard Input Matrix to a Sheet
#'
#' @param wb A workbook object
#' @param sheet_name Name of the sheet to create/write to
#' @param data_df The data frame containing matrix values
#' @param hab_labels Character vector of habitat names (Rows)
#' @param es_labels Character vector of ES names (Columns)
#' @param style_obj A list containing our createStyle objects
write_input_matrix <- function(wb, sheet_name, data_df, hab_labels, es_labels, style_obj) {

  # 1. Create the sheet
  openxlsx::addWorksheet(wb, sheet_name)

  # 2. Write Labels
  # Row 1: ES Labels (transposed to go across columns)
  openxlsx::writeData(wb, sheet_name, t(es_labels), startCol = 2, startRow = 1, colNames = FALSE)

  # Column 1: Habitat Labels (starting from row 2)
  openxlsx::writeData(wb, sheet_name, hab_labels, startCol = 1, startRow = 2, colNames = FALSE)

  # 3. Write Data
  # We use [hab_labels, es_labels] to ensure the data aligns with our headers
  # even if the input data frame is sorted differently.
  final_matrix <- data_df[hab_labels, es_labels]

  openxlsx::writeData(wb, sheet_name, final_matrix,
                      startCol = 2, startRow = 2,
                      colNames = FALSE, rowNames = FALSE)

  # 4. Apply Styles
  # Header Style (Vertical ES Labels)
  openxlsx::addStyle(wb, sheet_name, style = style_obj$vert,
                     rows = 1, cols = 2:(1 + length(es_labels)))

  # Body Style (The data grid)
  openxlsx::addStyle(wb, sheet_name, style = style_obj$body,
                     rows = 2:(1 + length(hab_labels)),
                     cols = 2:(1 + length(es_labels)),
                     gridExpand = TRUE)

  # 5. Column/Row Adjustments
  openxlsx::setColWidths(wb, sheet_name, cols = 2:(1 + length(es_labels)), width = 5)
  openxlsx::setRowHeights(wb, sheet_name, rows = 1, heights = 150)
  openxlsx::setColWidths(wb, sheet_name, cols = 1, width = 45)
}

header_style <- createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  fgFill = "#DCE6F1", # Light blue background
  border = "TopBottomLeftRight"
)

body_style <- createStyle(
  halign = "left",
  border = "TopBottomLeftRight"
)

addWorksheet(wb, "Weights - Provision Per Unit")

saveWorkbook(wb, template_out, overwrite = TRUE)





















# Run it
# build_metadata_entry(file.path("dev", "openNCAI_data_entry.xlsx"), replace = TRUE)

# get_labels_from_template gets a flat vector of the detailed habitats and
# ecotystem services.
get_labels_from_template <- function(df) {
  # Remove the NA padding we added during creation
  labels <- unlist(df, use.names = FALSE)
  return(labels[!is.na(labels)])
}

# extract_indicator_id gets the number from the end of the condition indicator
# name and uses it to name the CI relevance matrix sheets.
extract_indicator_id <- function(ci_name) {
  # Regex explained:
  # _      : matches a literal underscore
  # ([0-9]+): matches one or more digits and "captures" them
  # $      : ensures this pattern is at the very end of the string
  matches <- regmatches(ci_name, regexec("_([0-9]+)$", ci_name))

  # If a match is found, return the number; otherwise return a truncated name
  if (length(matches[[1]]) > 1) {
    return(paste0("CI_", matches[[1]][2])) # Returns e.g., "CI_2"
  } else {
    # Fallback if no trailing number exists
    return(substr(ci_name, 1, 31))
  }
}

# populate template() will populate the template with entry sheets for weights
# and relevance matrices.
populate_template <- function(input_sheet_path, output_path, use_ns_weights = TRUE, esppu = NULL, ci_relevance_matrices = NULL) {

  # 1. Load Metadata
  hab_metadata <- openxlsx::read.xlsx(input_sheet_path, sheet = "habitats_label_tree", startRow = 3)
  es_metadata <- openxlsx::read.xlsx(input_sheet_path, sheet = "es_label_tree", startRow = 3)

  # Get flat vectors (unlisted)
  all_habs <- get_labels_from_template(hab_metadata)
  all_es <- get_labels_from_template(es_metadata)

  wb <- openxlsx::createWorkbook()

  # 2. Define Styles
  vertStyle <- openxlsx::createStyle(
    textRotation = 90,
    halign = "center",
    valign = "center",
    border = "TopBottomLeftRight",
    textDecoration = "bold"
  )

  bodyStyle <- openxlsx::createStyle(
    halign = "center",
    border = "TopBottomLeftRight"
  )

  # Helper to write a matrix sheet
  # Defined inside to capture all_habs and all_es from the parent environment
  write_matrix_sheet <- function(wb, sheet_name, data_df) {
    openxlsx::addWorksheet(wb, sheet_name)

    # Write Labels (using colNames = FALSE to avoid 'x' or 'V1' headers)
    openxlsx::writeData(wb, sheet_name, all_habs, startCol = 2, startRow = 4, colNames = FALSE)
    openxlsx::writeData(wb, sheet_name, t(all_es), startCol = 3, startRow = 3, colNames = FALSE)

    # Write Data
    # Indexing with [all_habs, all_es] ensures the matrix matches the user's label order
    final_data <- data_df[all_habs, all_es]
    openxlsx::writeData(wb, sheet_name, final_data,
                        startCol = 3, startRow = 4,
                        colNames = FALSE, rowNames = FALSE)

    # Formatting headers
    openxlsx::addStyle(wb, sheet_name, style = vertStyle, rows = 3, cols = 3:(2+length(all_es)))

    # Formatting body
    openxlsx::addStyle(wb, sheet_name, style = bodyStyle,
                       rows = 4:(3+length(all_habs)),
                       cols = 3:(2+length(all_es)), gridExpand = TRUE)

    # Sizing
    openxlsx::setColWidths(wb, sheet_name, cols = 3:(2+length(all_es)), width = 4)
    openxlsx::setRowHeights(wb, sheet_name, rows = 3, heights = 150)
    openxlsx::setColWidths(wb, sheet_name, cols = 2, width = 35)
  }

  # 3. Create 'provision_per_unit' sheet
  if (use_ns_weights && !is.null(esppu)) {
    write_matrix_sheet(wb, "provision_per_unit", esppu)
  } else {
    # Create empty df if not using weights
    empty_df <- as.data.frame(matrix(0, nrow = length(all_habs), ncol = length(all_es)))
    rownames(empty_df) <- all_habs
    colnames(empty_df) <- all_es
    write_matrix_sheet(wb, "provision_per_unit", empty_df)
  }

  # 4. Create Condition Indicator sheets
  if (!is.null(ci_relevance_matrices)) {
    indicator_names <- names(ci_relevance_matrices)

    invisible(lapply(indicator_names, function(ci_full_name) {

      # Get short ID for tab (e.g. CI_2)
      short_tab_name <- extract_indicator_id(ci_full_name)

      # Write the matrix sheet
      write_matrix_sheet(wb, short_tab_name, ns_ci_relevance_matrices[[ci_full_name]])

      # Add the full name as a reference in cell A2
      openxlsx::writeData(wb, short_tab_name, paste("Indicator:", ci_full_name),
                          startCol = 1, startRow = 2)
    }))
  }

  # 5. Save
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  message("Success: Template with all matrices generated.")
}

# Run it:
populate_template(
  input_sheet_path = file.path("dev", "openNCAI_data_entry_ns.xlsx"),
  output_path = file.path("dev", "openNCAI_data_entry_2_ns.xlsx"),
  use_ns_weights = TRUE,
  esppu = ns_esppu_scores,
  ci_relevance_matrices = ns_ci_relevance_matrices
)
