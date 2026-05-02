#' Create an NCAI Data Entry Template
#'
#' Generates a highly formatted, protected Excel (.xlsx) workbook containing
#' the necessary sheets for a user to populate an openNCAI account. The template
#' includes internal logic for habitat and ecosystem service trees, automated
#' styling, and cell locking to ensure data integrity for subsequent re-import.
#' Note that the habitats and ecosystem service label trees passed here may be
#' used to subsequently read in the data with read_ncai_template().
#'
#' Optional arguments to pre-populate the template with data assume that the
#' passed-in data matches those of the label trees, year list and condition
#' indicator list as relevant. Users are recommended to create an empty
#' template first to verify the required formats before attempting to pass in
#' data.
#'
#' @param template_out A string representing the file path where the .xlsx will be saved.
#' @param habitats_label_tree A named list of character vectors representing the habitat hierarchy. Names: broad habitats, typically EUNIS level 1. Character vector items: typically EUNIS level 2 habitats.
#' @param es_label_tree A named list of character vectors representing the ecosystem service hierarchy. Names: typically SEEA ecosystem service types. Character vector items: typically CICES-type ecosystem services.
#' @param ci_names A character vector of condition indicator names.
#' @param year_list A vector of years (numeric or character) to be included in the account.
#' @param overwrite Logical. If \code{TRUE}, an existing file at \code{template_out} will
#' be overwritten without warning. Default is \code{FALSE} to prevent accidental loss
#' of manually entered data.
#' @param habitat_extent Optional data frame of existing habitat extent data.
#' Row and column order and dimensions MUST matched label trees/year list.
#' @param provision_per_unit_scores Optional data frame of existing ES Potential
#'  Per Unit scores. Row and column order MUST match lower levels of
#'  \code{habitats_label_tree} and \code{es_label_tree}.
#' @param between_importance_scores Optional list of ecosystem service type
#' importance weights. Order MUST match names of \code{es_label_tree}.
#' @param within_importance_scores Optional list of lists of weights for
#' specific services. Order MUST match the order and dimensions of the lower
#' level of \code{es_label_tree}.
#' @param ci_scores Optional data frame of existing condition indicator scores.
#' Rows and columns MUST match dimensions and order of \code{year_list} and
#' \code{ci_names}.
#' @param indicator_directory Optional data frame of existing indicator salience
#'  data. Row order and length MUST match \code{ci_names}. Column order and
#'  length MUST match \code{es_label_tree} names.
#' @param ci_relevance_matrices Optional list of existing binary relevance
#' matrices. Matrix list order and length must mach \code{ci_names}. Each
#' matrix row and column order MUST match lower levels of
#'  \code{habitats_label_tree} and \code{es_label_tree}.
#'
#' @return Generates an Excel file at the specified path and returns a message of success.
#' @export
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData addStyle createStyle mergeCells setRowHeights setColWidths freezePane protectWorksheet saveWorkbook
#' @importFrom stringr str_replace_all
#'
#' @examples
#' \dontrun{
#' # 1. Define the habitat hierarchy
#' habitat_label_tree <- list(
#'   'B. Coastal Habitats' = c(
#'     "Coastal vegetated shingle",
#'     "Coastal dunes and sandy shores"
#'   ),
#'   'E. Grasslands' = c(
#'     "Dry Grasslands",
#'     "Mesic Grasslands"
#'   )
#' )
#'
#' # 2. Define the ecosystem service hierarchy
#' ecosystem_service_label_tree <- list(
#'   'PROVISIONING' = c(
#'     "1.1 Cultivated Crops",
#'     "1.2 Reared Animals And Their Outputs"
#'   ),
#'   'CULTURAL' = c(
#'     "3.5. Existence & bequest"
#'   )
#' )
#'
#' # 3. Define indicators and temporal scope
#' condition_indicator_list <- c("National Water Quality Index", "AgriSCOR")
#' list_of_years <- 2020:2025
#'
#' # 4. Generate the template
#' # The resulting Excel file will have locked headers and editable data areas.
#' create_ncai_template(
#'   template_out = "NCAI_Entry_Template.xlsx",
#'   habitats_label_tree = habitat_label_tree,
#'   es_label_tree = ecosystem_service_label_tree,
#'   ci_names = condition_indicator_list,
#'   year_list = list_of_years
#' )
#' }
create_ncai_template <- function(template_out,
                                 habitats_label_tree,
                                 es_label_tree,
                                 ci_names,
                                 year_list,
                                 overwrite = FALSE,
                                 habitat_extent = NULL,
                                 provision_per_unit_scores = NULL,
                                 between_importance_scores = NULL,
                                 within_importance_scores = NULL,
                                 ci_scores = NULL,
                                 indicator_directory = NULL,
                                 ci_relevance_matrices = NULL) {

  # Check if file exists and ask re. overwriting.
  if (file.exists(template_out) && !overwrite) {
    stop(paste0("The file '", template_out, "' already exists. ",
                "Set overwrite = TRUE to replace it, or choose a different name."))
  }

  # Set up styles
  hab_palette <- c("#FFDEAD", "#E0FFFF", "#EEEEE0", "#CAFF70", "#FFBBFF",
                   "#B4EEB4", "#CDCDC1", "#EE9572", "#9FB6CD", "#D8BFD8")

  thick_border_style <- openxlsx::createStyle(border = "Left", borderStyle = "thick", borderColour = "black")
  header_style <- openxlsx::createStyle(textDecoration = "bold", halign = "center", valign = "bottom",
                                        fgFill = "#DCE6F1", border = "Bottom", textRotation = 90,
                                        wrapText = TRUE, fontSize = 9)
  instruction_style <- openxlsx::createStyle(fontColour = "#0070C0", textDecoration = "bold", fontSize = 10, wrapText = TRUE)
  entry_style <- openxlsx::createStyle(fgFill = "#FDE9D9", halign = "center", border = "TopBottomLeftRight")

  style_obj <- list(vert = header_style, body = openxlsx::createStyle(halign = "center", border = NULL))

  wb <- openxlsx::createWorkbook()

  # 1. Instructions
  write_instructions_sheet(wb, "Instructions")

  # 2. Habitat Extent
  write_extent_sheet(wb, "Habitat Extent", habitats_label_tree, year_list, hab_palette, habitat_extent)

  # 3. Provision Per Unit
  provision_per_unit_data <- if (!is.null(provision_per_unit_scores)) provision_per_unit_scores else prepare_template_matrix(habitats_label_tree, es_label_tree)
  write_input_matrix(wb, "Provision Per Unit", provision_per_unit_data, habitats_label_tree, es_label_tree,
                     style_obj, hab_palette, thick_border_style, instruction_style,
                     instruction = "Enter exemplary ecosystem service potential per service-providing unit - score out of 5")

  # 4. Importance Weights
  write_importance_sheet_with_data(wb, "Importance", es_label_tree, between_importance_scores,
                                   within_importance_scores, instruction_style, entry_style)

  # 5. Condition Scores
  write_condition_scores_sheet(wb, "Condition Indicator Scores", ci_names, year_list, ci_scores)

  # 6. Indicator Salience
  write_indicator_directory(wb, "Indicator Directory", ci_names, names(es_label_tree), indicator_directory)

  # 7. Relevance Matrices
  for (i in seq_along(ci_names)) {
    ci_name <- ci_names[i]
    clean_name <- stringr::str_replace_all(ci_name, "[[:punct:]]", " ")
    short_name <- trimws(substr(clean_name, 1, 31))
    ci_data <- if (!is.null(ci_relevance_matrices)) ci_relevance_matrices[[i]] else prepare_template_matrix(habitats_label_tree, es_label_tree)

    write_input_matrix(wb, short_name, ci_data, habitats_label_tree, es_label_tree,
                       style_obj, hab_palette, thick_border_style, instruction_style,
                       instruction = "Enter 1 or 0 in each cell.")
  }

  openxlsx::saveWorkbook(wb, template_out, overwrite = overwrite)
  message(paste("Workbook generated:", template_out))
}
#' Prepare a blank data frame based on Habitat and ES trees
#' @keywords internal
prepare_template_matrix <- function(hab_tree, es_tree) {
  all_habs <- unlist(hab_tree, use.names = FALSE)
  all_es <- unlist(es_tree, use.names = FALSE)
  mat <- matrix(0, nrow = length(all_habs), ncol = length(all_es))
  df <- as.data.frame(mat)
  row.names(df) <- all_habs
  names(df) <- all_es
  return(df)
}

#' Write a clean Instructions sheet at the start of the workbook
#' @keywords internal
write_instructions_sheet <- function(wb, sheet_name) {
  openxlsx::addWorksheet(wb, sheet_name)

  # Define Styles
  title_style <- openxlsx::createStyle(
    fontSize = 20, fontColour = "#2F5597", textDecoration = "bold",
    halign = "left", valign = "center"
  )

  text_style <- openxlsx::createStyle(
    fontSize = 12, halign = "left", valign = "top", wrapText = TRUE
  )

  # 1. Create a large white background
  bg_style <- openxlsx::createStyle(fgFill = "white")
  openxlsx::addStyle(wb, sheet_name, style = bg_style, rows = 1:100, cols = 1:20, gridExpand = TRUE)

  # 2. Add Header
  openxlsx::writeData(wb, sheet_name, "Instructions", startCol = 2, startRow = 2)
  openxlsx::addStyle(wb, sheet_name, style = title_style, rows = 2, cols = 2)

  # 3. Add Instruction Text
  instr_text <- paste(
    "Welcome to the NCAI Data Entry Template. Complete the sheets with your own data as follows:\n\n",
    "Habitat extent: area of each habitat (e.g. in hectares) each year.\n\n",
    "Provision Per Unit: score, typically out of 5, representing the exemplary per unit provision of ecosystem services per unit of area.\n\n",
    "Importance: define relative importance of ecosystem service types, and the specific services within them.\n\n",
    "Condition Indicator Scores: raw scores of each condition indicator in each year.\n\n",
    "Indicator Directory: salience (between 0 and 1) of condition indicators.\n\n",
    "Individual condition indicator relevance sheets: binary values (1 = relevant; 0 = not relevant).\n\n",
    "\n\n",
    "IMPORTANT - avoid changing data in locked cells. Unchanged habitat and ecosystem service labels are expected when using openNCAI::read_ncai_template to import the data to R./n/n",
    "Any changes to habitat names or ecosystem services must be reflected in the habitats_label_tree and es_label_tree passed to that function."
  )

  openxlsx::mergeCells(wb, sheet_name, cols = 2:10, rows = 4:30)
  openxlsx::writeData(wb, sheet_name, instr_text, startCol = 2, startRow = 4)
  openxlsx::addStyle(wb, sheet_name, style = text_style, rows = 4, cols = 2)
  openxlsx::setRowHeights(wb, sheet_name, rows = 4:25, heights = 25)

  # Clean up dimensions
  openxlsx::setColWidths(wb, sheet_name, cols = 1, widths = 3)
  openxlsx::setColWidths(wb, sheet_name, cols = 2, widths = 80)

  # Lock the entire sheet
  openxlsx::protectWorksheet(wb, sheet_name, protect = TRUE, lockSelectingLockedCells = FALSE)
}
#' Write standard Matrix sheets (e.g., ES Potential or CI Relevance)
#' @keywords internal
write_input_matrix <- function(wb, sheet_name, data_df, hab_tree, es_tree,
                               style_obj, hab_palette, thick_border_style,
                               instruction_style, instruction) {
  openxlsx::addWorksheet(wb, sheet_name)
  all_habs <- unlist(hab_tree, use.names = FALSE)
  all_es <- unlist(es_tree, use.names = FALSE)

  # 1. Write the Skeleton
  openxlsx::writeData(wb, sheet_name, instruction, startCol = 1, startRow = 1)
  openxlsx::addStyle(wb, sheet_name, style = instruction_style, rows = 1, cols = 1)
  openxlsx::writeData(wb, sheet_name, t(all_es), startCol = 2, startRow = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet_name, all_habs, startCol = 1, startRow = 2, colNames = FALSE)
  openxlsx::writeData(wb, sheet_name, as.matrix(data_df), startCol = 2, startRow = 2, colNames = FALSE, rowNames = FALSE)

  # 2. ES HEADER SHADING
  current_col <- 2
  es_colors <- c("#F2F2F2", "#E6E6E6")
  for (i in seq_along(names(es_tree))) {
    group_size <- length(es_tree[[i]])
    cols_to_style <- current_col:(current_col + group_size - 1)
    cat_header_style <- openxlsx::createStyle(
      textDecoration = "bold", halign = "center", valign = "bottom",
      fgFill = es_colors[(i %% 2) + 1], border = "Bottom",
      textRotation = 90, wrapText = TRUE, fontSize = 9
    )
    openxlsx::addStyle(wb, sheet_name, style = cat_header_style, rows = 1, cols = cols_to_style, gridExpand = TRUE)
    current_col <- current_col + group_size
  }

  # 3. HABITAT COLOURING
  current_row <- 2
  for (i in seq_along(names(hab_tree))) {
    group_size <- length(hab_tree[[i]])
    rows_to_style <- current_row:(current_row + group_size - 1)
    current_color <- hab_palette[((i - 1) %% length(hab_palette)) + 1]

    row_bg_style <- openxlsx::createStyle(fgFill = current_color, halign = "center", border = NULL)
    openxlsx::addStyle(wb, sheet_name,
                       style = row_bg_style, rows = rows_to_style,
                       cols = 2:(1 + length(all_es)), gridExpand = TRUE, stack = TRUE
    )

    label_col_style <- openxlsx::createStyle(fgFill = current_color, textDecoration = "bold", border = "Right", halign = "left")
    openxlsx::addStyle(wb, sheet_name, style = label_col_style, rows = rows_to_style, cols = 1)
    current_row <- current_row + group_size
  }

  # 4. ES TYPE DIVIDERS
  current_col <- 2
  for (i in seq_along(names(es_tree))) {
    group_size <- length(es_tree[[i]])
    if (i > 1) {
      openxlsx::addStyle(wb, sheet_name,
                         style = thick_border_style,
                         rows = 1:(1 + length(all_habs)), cols = current_col, stack = TRUE
      )
    }
    current_col <- current_col + group_size
  }

  # 5. UNLOCK DATA BODY & PROTECT
  unlocked_st <- openxlsx::createStyle(locked = FALSE)
  openxlsx::addStyle(wb, sheet_name, style = unlocked_st,
                     rows = 2:(1 + length(all_habs)),
                     cols = 2:(1 + length(all_es)),
                     gridExpand = TRUE, stack = TRUE)

  openxlsx::setRowHeights(wb, sheet_name, rows = 1, heights = 180)
  openxlsx::setColWidths(wb, sheet_name, cols = 1, width = 50)
  openxlsx::setColWidths(wb, sheet_name, cols = 2:(1 + length(all_es)), width = 4.5)
  openxlsx::freezePane(wb, sheet_name, firstActiveRow = 2, firstActiveCol = 2)
  openxlsx::protectWorksheet(wb, sheet_name, protect = TRUE, lockSelectingLockedCells = FALSE)
}
#' Write the specialized Importance Weights sheet
#' @keywords internal
write_importance_sheet_with_data <- function(wb, sheet_name, es_tree,
                                             between_data, within_list,
                                             instruction_style, entry_style) {
  openxlsx::addWorksheet(wb, sheet_name)
  between_types <- names(es_tree)
  unlocked_st <- openxlsx::createStyle(locked = FALSE)

  # --- STEP 1 ---
  instr1 <- "Step 1: ecosystem service type (SEEA) section."
  openxlsx::writeData(wb, sheet_name, instr1, startRow = 1)
  openxlsx::addStyle(wb, sheet_name, instruction_style, rows = 1, cols = 1)

  b_vals <- if (!is.null(between_data)) unlist(between_data) else rep(0, length(between_types))
  header_df <- data.frame(Type = between_types, Score = b_vals)
  openxlsx::writeData(wb, sheet_name, header_df, startRow = 2, colNames = TRUE)

  # Unlock scores
  openxlsx::addStyle(wb, sheet_name, entry_style, rows = 3:(3 + length(between_types) - 1), cols = 2)
  openxlsx::addStyle(wb, sheet_name, unlocked_st, rows = 3:(3 + length(between_types) - 1), cols = 2, stack = TRUE)

  # --- STEP 2 ---
  current_row <- 8
  for (i in seq_along(names(es_tree))) {
    cat_name <- names(es_tree)[i]
    services <- es_tree[[cat_name]]
    w_vals <- if (!is.null(within_list)) unlist(within_list[[i]]) else rep(0, length(services))

    instr2 <- sprintf("Step 2: %s services.", cat_name)
    openxlsx::writeData(wb, sheet_name, instr2, startRow = current_row)
    openxlsx::addStyle(wb, sheet_name, instruction_style, rows = current_row, cols = 1)

    df <- data.frame(Service = services, Raw_Score = w_vals)
    openxlsx::writeData(wb, sheet_name, df, startRow = current_row + 1, colNames = TRUE)

    # Unlock score column
    rows_to_unlock <- (current_row + 2):(current_row + 1 + length(services))
    openxlsx::addStyle(wb, sheet_name, entry_style, rows = rows_to_unlock, cols = 2)
    openxlsx::addStyle(wb, sheet_name, unlocked_st, rows = rows_to_unlock, cols = 2, stack = TRUE)

    current_row <- current_row + length(services) + 4
  }

  openxlsx::setColWidths(wb, sheet_name, cols = 1, widths = 60)
  openxlsx::setColWidths(wb, sheet_name, cols = 2, widths = 15)
  openxlsx::protectWorksheet(wb, sheet_name, protect = TRUE, lockSelectingLockedCells = FALSE)
}
#' Write Habitat Extent Time-Series Sheet
#' @keywords internal
write_extent_sheet <- function(wb, sheet_name, hab_tree, years, hab_palette, source_data = NULL) {
  openxlsx::addWorksheet(wb, sheet_name)
  all_habs <- unlist(hab_tree, use.names = FALSE)
  openxlsx::writeData(wb, sheet_name, "Habitat extent in hectares", startCol = 1, startRow = 1)
  openxlsx::writeData(wb, sheet_name, t(years), startCol = 2, startRow = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet_name, all_habs, startCol = 1, startRow = 2, colNames = FALSE)

  ext_data <- if (is.null(source_data)) matrix(0, nrow = length(all_habs), ncol = length(years)) else source_data
  openxlsx::writeData(wb, sheet_name, ext_data, startCol = 2, startRow = 2, colNames = FALSE)

  current_row <- 2
  for (i in seq_along(names(hab_tree))) {
    group_size <- length(hab_tree[[i]])
    rows_to_style <- current_row:(current_row + group_size - 1)
    current_color <- hab_palette[((i - 1) %% length(hab_palette)) + 1]

    # Style and UNLOCK body
    openxlsx::addStyle(wb, sheet_name,
                       style = openxlsx::createStyle(fgFill = current_color, halign = "center", locked = FALSE),
                       rows = rows_to_style, cols = 2:(1 + length(years)), gridExpand = TRUE
    )
    openxlsx::addStyle(wb, sheet_name,
                       style = openxlsx::createStyle(fgFill = current_color, textDecoration = "bold", border = "Right", locked = TRUE),
                       rows = rows_to_style, cols = 1
    )
    current_row <- current_row + group_size
  }
  openxlsx::setColWidths(wb, sheet_name, cols = 1, width = 50)
  openxlsx::setColWidths(wb, sheet_name, cols = 2:(1 + length(years)), width = 8)
  openxlsx::protectWorksheet(wb, sheet_name, protect = TRUE, lockSelectingLockedCells = FALSE)
}

#' Write Indicator Directory Sheet
#' @keywords internal
write_indicator_directory <- function(wb, sheet_name, ci_names, es_types, source_data = NULL) {
  openxlsx::addWorksheet(wb, sheet_name)
  unlocked_st <- openxlsx::createStyle(locked = FALSE)

  headers <- c("Condition Indicator", es_types)
  openxlsx::writeData(wb, sheet_name, t(headers), startCol = 1, startRow = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet_name, ci_names, startCol = 1, startRow = 2, colNames = FALSE)

  dir_data <- if (is.null(source_data)) matrix(0, nrow = length(ci_names), ncol = length(es_types)) else {
    if (ncol(source_data) > length(es_types)) source_data[, 2:(length(es_types) + 1)] else source_data
  }
  openxlsx::writeData(wb, sheet_name, dir_data, startCol = 2, startRow = 2, colNames = FALSE, rowNames = FALSE)

  header_style_rot <- openxlsx::createStyle(
    textDecoration = "bold", fgFill = "#DCE6F1", border = "Bottom",
    halign = "center", valign = "bottom", textRotation = 90, fontSize = 9
  )
  openxlsx::addStyle(wb, sheet_name, style = header_style_rot, rows = 1, cols = 1:(1 + length(es_types)))
  openxlsx::setRowHeights(wb, sheet_name, rows = 1, heights = 180)

  for (i in seq_along(ci_names)) {
    this_row <- i + 1
    bg <- if (i %% 2 == 0) "#F2F2F2" else "white"
    # Style and UNLOCK
    openxlsx::addStyle(wb, sheet_name, style = openxlsx::createStyle(fgFill = bg, halign = "center", locked = FALSE),
                       rows = this_row, cols = 2:(1 + length(es_types)))
    openxlsx::addStyle(wb, sheet_name, style = openxlsx::createStyle(textDecoration = "bold", border = "Right", fgFill = bg, locked = TRUE),
                       rows = this_row, cols = 1)
  }
  openxlsx::setColWidths(wb, sheet_name, cols = 1, width = 60)
  openxlsx::protectWorksheet(wb, sheet_name, protect = TRUE, lockSelectingLockedCells = FALSE)
}
#' Write Condition Scores Time-Series Sheet
#' @keywords internal
write_condition_scores_sheet <- function(wb, sheet_name, ci_names, years, source_data = NULL) {
  openxlsx::addWorksheet(wb, sheet_name)
  unlocked_st <- openxlsx::createStyle(locked = FALSE)

  openxlsx::writeData(wb, sheet_name, "Year", startCol = 1, startRow = 1)
  openxlsx::writeData(wb, sheet_name, t(ci_names), startCol = 2, startRow = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet_name, years, startCol = 1, startRow = 2, colNames = FALSE)

  score_data <- if (is.null(source_data)) matrix(0, nrow = length(years), ncol = length(ci_names)) else source_data
  openxlsx::writeData(wb, sheet_name, score_data, startCol = 2, startRow = 2, colNames = FALSE)

  header_st <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DCE6F1", border = "Bottom",
                                     textRotation = 90, fontSize = 9, halign = "center", valign = "bottom")
  openxlsx::addStyle(wb, sheet_name, style = header_st, rows = 1, cols = 1:(1 + length(ci_names)))

  for (j in seq_along(ci_names)) {
    this_col <- j + 1
    bg <- if (j %% 2 == 0) "#F2F2F2" else "white"
    # Unlock body
    openxlsx::addStyle(wb, sheet_name, style = openxlsx::createStyle(fgFill = bg, halign = "center", locked = FALSE),
                       rows = 2:(1 + length(years)), cols = this_col, gridExpand = TRUE)
  }
  openxlsx::addStyle(wb, sheet_name, style = openxlsx::createStyle(textDecoration = "bold", border = "Right", halign = "center"),
                     rows = 2:(1 + length(years)), cols = 1)

  openxlsx::setRowHeights(wb, sheet_name, rows = 1, heights = 180)
  openxlsx::setColWidths(wb, sheet_name, cols = 1, width = 10)
  openxlsx::setColWidths(wb, sheet_name, cols = 2:(1 + length(ci_names)), width = 6)
  openxlsx::freezePane(wb, sheet_name, firstActiveRow = 2, firstActiveCol = 2)
  openxlsx::protectWorksheet(wb, sheet_name, protect = TRUE, lockSelectingLockedCells = FALSE)
}
#' Read and Process a Populated NCAI Template
#'
#' Imports data from an Excel workbook created by \code{create_ncai_template}.
#' The function uses the provided label trees to re-align the data and applies
#' automated cleaning to headers to ensure compatibility with internal openNCAI
#' calculation engines. Note that the habitat and ecosystem service label trees
#' should match the labels used in the spreadsheet; typically the same label trees
#' should be used to generate and read the template.
#'
#' @param path String. The file path to the populated .xlsx file.
#' @param habitats_label_tree A named list of character vectors representing the habitat hierarchy. Names: broad habitats, typically EUNIS level 1. Character vector items: typically EUNIS level 2 habitats.
#' @param es_label_tree A named list of character vectors representing the ecosystem service hierarchy. Names: typically SEEA ecosystem service types. Character vector items: typically CICES-type ecosystem services.
#' @param ci_names The original character vector of indicator names used to create the template.
#'
#' @return A named list of data structures ready for use in \code{get_ncai}:
#' \itemize{
#'   \item \code{clean_habitats_label_tree}: Cleaned version of habitat hierarchy.
#'   \item \code{clean_es_label_tree}: Cleaned version of ES hierarchy.
#'   \item \code{habitat_extent}: Data frame of habitat areas over time.
#'   \item \code{ci_scores}: Data frame of indicator scores over time.
#'   \item \code{provision_per_unit_scores}: Data frame of ES potential per unit area.
#'   \item \code{between_importance}: List of broad ES type weights.
#'   \item \code{within_importance}: List of specific ES service weights.
#'   \item \code{indicator_directory}: Data frame mapping indicators to service types.
#'   \item \code{ci_relevance_matrices}: List of binary matrices for every indicator.
#' }
#' @export
#'
#' @importFrom readxl read_excel
#' @importFrom janitor make_clean_names
#' @importFrom stringr str_replace str_trim str_to_lower
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' # 1. Use the same trees used during create_ncai_template()
#' habitat_label_tree <- list(
#'   'B. Coastal Habitats' = c(
#'     "Coastal vegetated shingle",
#'     "Coastal dunes and sandy shores"
#'   ),
#'   'E. Grasslands' = c(
#'     "Dry Grasslands",
#'     "Mesic Grasslands"
#'   )
#' )
#'
#' ecosystem_service_label_tree <- list(
#'   'PROVISIONING' = c(
#'     "1.1 Cultivated Crops",
#'     "1.2 Reared Animals And Their Outputs"
#'   ),
#'   'CULTURAL' = c(
#'     "3.5. Existence & bequest"
#'   )
#' )
#'
#' condition_indicator_list <- c("National Water Quality Index", "AgriSCOR")
#'
#' # 2. Read the populated template
#' # Ensure the path points to a file filled out by a user
#' ncai_data <- read_ncai_template(
#'   path = "NCAI_Entry_Template_FILLED.xlsx",
#'   habitats_label_tree = habitat_label_tree,
#'   es_label_tree = ecosystem_service_label_tree,
#'   ci_names = condition_indicator_list
#' )
#'
#' # 3. Access the cleaned data structures
#' # The habitat extent data frame
#' head(ncai_data$habitat_extent)
#'
#' # The list of binary relevance matrices for each indicator
#' names(ncai_data$ci_relevance_matrices)
#' }
read_ncai_template <- function(path,
                               habitats_label_tree,
                               es_label_tree,
                               ci_names) {

  # --- 1. SETUP & CLEANING ---
  clean_vec <- function(x) {
    # 1. Clean the names
    cleaned <- janitor::make_clean_names(x)
    # 2. Strip the leading 'x' that janitor adds to numeric strings
    cleaned <- stringr::str_replace(cleaned, "^x", "")
    # 3. Apply the specific manual fix for Crops to maintain consistency
    cleaned <- ifelse(cleaned == "1_1_1_cultivated_crops", "1_1_cultivated_crops", cleaned)
    return(cleaned)
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
  provision_per_unit_range <- sprintf("A2:%s%d", col_to_lab(1 + n_es), 1 + n_habs)
  provision_per_unit_raw   <- readxl::read_excel(path, sheet = "Provision Per Unit", range = provision_per_unit_range, col_names = FALSE)

  provision_per_unit_scores <- as.data.frame(as.matrix(provision_per_unit_raw[, -1]))
  provision_per_unit_scores[] <- lapply(provision_per_unit_scores, as.numeric)

  rownames(provision_per_unit_scores) <- all_clean_habs
  colnames(provision_per_unit_scores) <- all_clean_es

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
    ci_display  <- ci_names[i]
    sheet_tab <- trimws(substr(stringr::str_replace_all(ci_display, "[[:punct:]]", " "), 1, 31))

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
    provision_per_unit_scores              = provision_per_unit_scores,
    between_importance        = between_importance,
    within_importance         = within_importance,
    indicator_directory       = indicator_directory,
    ci_relevance_matrices     = ci_relevance_matrices
  ))
}
