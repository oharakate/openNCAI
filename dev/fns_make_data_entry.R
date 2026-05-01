# library(readxl)
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(openxlsx)

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Prepare a blank data frame based on Habitat and ES trees
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

# ==============================================================================
# MASTER GENERATOR FUNCTION
# ==============================================================================
create_ncai_template <- function(template_out,
                                 habitats_label_tree,
                                 es_label_tree,
                                 ci_names,
                                 year_list,
                                 habitat_extent = NULL,
                                 esppu_scores = NULL,
                                 between_importance_scores = NULL,
                                 within_importance_scores = NULL,
                                 ci_scores = NULL,
                                 indicator_directory = NULL,
                                 ci_relevance_matrices = NULL) {

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
  esppu_data <- if (!is.null(esppu_scores)) esppu_scores else prepare_template_matrix(habitats_label_tree, es_label_tree)
  write_input_matrix(wb, "Provision Per Unit", esppu_data, habitats_label_tree, es_label_tree,
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

  openxlsx::saveWorkbook(wb, template_out, overwrite = TRUE)
  message(paste("Workbook generated:", template_out))
}

####### EXECUTE ########

create_ncai_template(template_out = "dev/NCAI_Data_Entry_Template.xlsx",
                     habitats_label_tree = ns_dirty_habitats_label_tree,
                     es_label_tree = ns_dirty_es_label_tree,
                     ci_names = ns_dirty_ci_names,
                     year_list = ns_year_list,
                     habitat_extent = ns_habitat_extent,
                     esppu_scores = ns_esppu_scores,
                     between_importance_scores = ns_between_importance_scores,
                     within_importance_scores = ns_within_importance_scores,
                     ci_scores = ns_ci_scores,
                     indicator_directory = ns_indicator_directory,
                     ci_relevance_matrices = ns_ci_relevance_matrices)


create_ncai_template(template_out = "dev/test_clean.xlsx",
                     habitats_label_tree = ns_habitats_label_tree,
                     es_label_tree = ns_es_label_tree,
                     ci_names = names(ns_ci_relevance_matrices),
                     year_list = ns_year_list)

new_hab_tree <- list(
  'B. Coastal Habitats' = c(
    "Coastal vegetated shingle",
    "Coastal dunes and sandy shores"
  ),
  'E. Grasslands' = c(
    "Dry Grasslands",
    "Mesic Grasslands"
  )
)

new_es_tree <- list(
  'PROVISIONING' = c(
    "1.1 Cultivated Crops",
    "1.2 Reared Animals And Their Outputs"
  ),
  'CULTURAL' = c(
    "3.5. Existence & bequest"
  )
)

new_dirty_ci_names <- c("National Water Quality Index", "AgriSCOR")

create_ncai_template(template_out = "dev/test_small_new.xlsx",
                     habitats_label_tree = new_hab_tree,
                     es_label_tree = new_es_tree,
                     ci_names = new_dirty_ci_names,
                     year_list = 2020:2025)
