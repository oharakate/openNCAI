library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

# --- CONFIGURATION ---
source_path <- "data-raw/ncai_corrected.xlsx"
template_out <- "dev/NCAI_Data_Entry_Template.xlsx"

# --- PALETTE & STYLES ---
# THIS WILL NEED UPDATED IF A NEW NUMBER OF
hab_palette <- c(
  "#FFDEAD", # Coastal: Navajo White/Peach
  "#E0FFFF", # Inland Waters: Light Sky Blue
  "#EEEEE0", # Mires, Bogs, Fens: Olive/Khaki Green
  "#CAFF70", # Grasslands: Bright Lime/Chartreuse
  "#FFBBFF", # Heathland: Plum/Light Purple
  "#B4EEB4", # Woodland: Pale Green
  "#CDCDC1", # Unvegetated: Ash Grey
  "#EE9572", # Cultivated:
  "#9FB6CD", # Constructed: Slate/Steel Blue
  "#D8BFD8"  # Montane: Thistle/Pale Violet
)

thick_border_style <- openxlsx::createStyle(
  border = "Left",
  borderStyle = "thick",
  borderColour = "black"
)

# Only a bottom border for the vertical headers
header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "bottom",
  fgFill = "#DCE6F1",
  border = "Bottom",
  textRotation = 90,
  wrapText = TRUE,
  fontSize = 9
)

# No borders for the main data body
body_style <- openxlsx::createStyle(
  halign = "center",
  border = NULL
)

# Instruction text style for importance weights (Blue, bold)
instruction_style <- openxlsx::createStyle(
  fontColour = "#0070C0",
  textDecoration = "bold",
  fontSize = 10,
  wrapText = TRUE
)

# Score entry style for importance weights (Peach background, centered)
entry_style <- openxlsx::createStyle(
  fgFill = "#FDE9D9",
  halign = "center",
  border = "TopBottomLeftRight"
)

# Score entry style (Peach background, centered)
entry_style <- openxlsx::createStyle(
  fgFill = "#FDE9D9",
  halign = "center",
  border = "TopBottomLeftRight"
)

style_obj <- list(
  vert = header_style,
  body = body_style
)

# --- FUNCTIONS ---

prepare_template_matrix <- function(hab_tree, es_tree) {
  all_habs <- unlist(hab_tree, use.names = FALSE)
  all_es   <- unlist(es_tree, use.names = FALSE)
  mat <- matrix(0, nrow = length(all_habs), ncol = length(all_es))
  df <- as.data.frame(mat)
  row.names(df) <- all_habs
  names(df)     <- all_es
  return(df)
}

write_input_matrix <- function(wb, sheet_name, data_df, hab_tree, es_tree,
                               style_obj,
                               instruction) {

  openxlsx::addWorksheet(wb, sheet_name)

  all_habs <- unlist(hab_tree, use.names = FALSE)
  all_es   <- unlist(es_tree, use.names = FALSE)

  # Write the Skeleton
  openxlsx::writeData(wb, sheet_name, instruction, startCol = 1, startRow = 1)
  openxlsx::addStyle(wb, sheet_name, style = instruction_style, rows = 1, cols = 1)
  openxlsx::writeData(wb, sheet_name, t(all_es), startCol = 2, startRow = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet_name, all_habs, startCol = 1, startRow = 2, colNames = FALSE)
  openxlsx::writeData(wb, sheet_name, data_df, startCol = 2, startRow = 2, colNames = FALSE, rowNames = FALSE)

  # --- ES HEADER SHADING (Row 1) ---
  current_col <- 2
  es_colors <- c("#F2F2F2", "#E6E6E6")

  for (i in seq_along(names(es_tree))) {
    group_size <- length(es_tree[[i]])
    cols_to_style <- current_col:(current_col + group_size - 1)

    cat_header_style <- openxlsx::createStyle(
      textDecoration = "bold", halign = "center", valign = "bottom",
      fgFill = es_colors[(i %% 2) + 1],
      border = "Bottom",
      textRotation = 90, wrapText = TRUE, fontSize = 9
    )

    openxlsx::addStyle(wb, sheet_name, style = cat_header_style,
                       rows = 1, cols = cols_to_style, gridExpand = TRUE)
    current_col <- current_col + group_size
  }

  # --- HABITAT COLOURING & BODY (Rows) ---
  current_row <- 2
  for (i in seq_along(names(hab_tree))) {
    group_size <- length(hab_tree[[i]])
    rows_to_style <- current_row:(current_row + group_size - 1)

    # Use modulo to recycle colors if i exceeds the number of colors in hab_palette
    # The +1 is needed because R is 1-indexed and modulo returns 0 at the end of the set
    current_color <- hab_palette[((i - 1) %% length(hab_palette)) + 1]

    # 1. Data area: background color with NO internal borders
    row_bg_style <- openxlsx::createStyle(
      fgFill = current_color,
      halign = "center",
      border = NULL
    )

    openxlsx::addStyle(wb, sheet_name, style = row_bg_style,
                       rows = rows_to_style, cols = 2:(1 + length(all_es)),
                       gridExpand = TRUE, stack = TRUE)

    # 2. Habitat labels: background color with RIGHT border only
    label_col_style <- openxlsx::createStyle(
      fgFill = current_color,
      textDecoration = "bold",
      border = "Right",
      halign = "left"
    )

    openxlsx::addStyle(wb, sheet_name, style = label_col_style,
                       rows = rows_to_style, cols = 1)

    current_row <- current_row + group_size
  }

  # --- ES TYPE DIVIDERS (Thick Lines) ---
  current_col <- 2
  for (i in seq_along(names(es_tree))) {
    group_size <- length(es_tree[[i]])
    if (i > 1) {
      openxlsx::addStyle(wb, sheet_name,
                         style = thick_border_style,
                         rows = 1:(1 + length(all_habs)),
                         cols = current_col,
                         stack = TRUE)
    }
    current_col <- current_col + group_size
  }

  # --- A1 CLEANUP ---
  openxlsx::addStyle(wb, sheet_name,
                     style = openxlsx::createStyle(fgFill = "white", wrapText = TRUE, fontSize = 9),
                     rows = 1, cols = 1, stack = TRUE)

  # Dimensions & Freeze
  openxlsx::setRowHeights(wb, sheet_name, rows = 1, heights = 180)
  openxlsx::setColWidths(wb, sheet_name, cols = 1, width = 50)
  openxlsx::setColWidths(wb, sheet_name, cols = 2:(1 + length(all_es)), width = 4.5)
  openxlsx::freezePane(wb, sheet_name, firstActiveRow = 2, firstActiveCol = 2)
}

write_importance_sheet <- function(wb, sheet_name, es_tree) {
  openxlsx::addWorksheet(wb, sheet_name)

  # --- SECTION 1: BETWEEN ES TYPES ---
  between_types <- names(es_tree)

  openxlsx::writeData(wb, sheet_name, "Step 1: ecosystem service type (SEEA) section. The most important service type is assigned a value of 20, and the other two are assigned a value (between 0 and 20) in terms of their relative importance.", startRow = 1)
  openxlsx::addStyle(wb, sheet_name, instruction_style, rows = 1, cols = 1)

  # Table Header
  header_df <- data.frame(Type = between_types, Score = 0)
  openxlsx::writeData(wb, sheet_name, header_df, startRow = 2, colNames = TRUE)

  # Style the entry cells
  openxlsx::addStyle(wb, sheet_name, entry_style, rows = 3:(3 + length(between_types)-1), cols = 2)

  # --- SECTION 2: WITHIN ES TYPES (The Groups) ---
  current_row <- 8

  for (i in seq_along(names(es_tree))) {
    cat_name <- names(es_tree)[i]
    services <- es_tree[[cat_name]]

    # Construct the instructions
    instr <- sprintf("Step 2: %s services. The most important is assigned a value of 20, and the others are assigned a value (between 0 and 20) in terms of their relative importance.",
                     cat_name)

    openxlsx::writeData(wb, sheet_name, instr, startRow = current_row)
    openxlsx::addStyle(wb, sheet_name, instruction_style, rows = current_row, cols = 1)

    # Data Table (Header and Services)
    df <- data.frame(Service = services, Raw_Score = 0)
    openxlsx::writeData(wb, sheet_name, df, startRow = current_row + 1, colNames = TRUE)

    # Entry Cells Styling (Peach)
    entry_rows <- (current_row + 2):(current_row + 1 + length(services))
    openxlsx::addStyle(wb, sheet_name, entry_style, rows = entry_rows, cols = 2)

    # Move cursor down for next group (adding gap)
    current_row <- current_row + length(services) + 4
  }

  # Formatting
  openxlsx::setColWidths(wb, sheet_name, cols = 1, widths = 60)
  openxlsx::setColWidths(wb, sheet_name, cols = 2, widths = 15)
}



#' Write Habitat Extent Time-Series Sheet
#'
#' @param wb A workbook object
#' @param sheet_name Name of the sheet
#' @param hab_tree The nested list of habitats
#' @param years Numeric vector (e.g., 2010:2024)
#' @param source_data Optional matrix or data frame of extent values
write_extent_sheet <- function(wb, sheet_name, hab_tree, years, source_data = NULL) {

  openxlsx::addWorksheet(wb, sheet_name)

  all_habs <- unlist(hab_tree, use.names = FALSE)

  # 1. Write headers and labels
  openxlsx::writeData(wb, sheet_name, "Habitat extent in hectares", startCol = 1, startRow = 1)
  openxlsx::addStyle(wb, sheet_name,
                     style = openxlsx::createStyle(fgFill = "white", wrapText = TRUE, fontSize = 10),
                     rows = 1, cols = 1, stack = FALSE)
  openxlsx::writeData(wb, sheet_name, t(years), startCol = 2, startRow = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet_name, all_habs, startCol = 1, startRow = 2, colNames = FALSE)

  # 2. Write data (source or zeros)
  if (is.null(source_data)) {
    ext_data <- matrix(0, nrow = length(all_habs), ncol = length(years))
  } else {
    ext_data <- source_data
  }
  openxlsx::writeData(wb, sheet_name, ext_data, startCol = 2, startRow = 2, colNames = FALSE)

  # 3. Apply Styling

  # Header Style (Row 1)
  year_header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    border = "Bottom",
    halign = "center",
    fgFill = "#F2F2F2"
  )
  openxlsx::addStyle(wb, sheet_name, style = year_header_style,
                     rows = 1, cols = 1:(1 + length(years)))

  # Habitat Row Coloring Loop
  current_row <- 2
  for (i in seq_along(names(hab_tree))) {
    group_size <- length(hab_tree[[i]])
    rows_to_style <- current_row:(current_row + group_size - 1)

    # Recycle colors from palette
    current_color <- hab_palette[((i - 1) %% length(hab_palette)) + 1]

    # Style for data area (Swimlanes)
    row_bg_style <- openxlsx::createStyle(
      fgFill = current_color,
      halign = "center",
      border = NULL
    )

    openxlsx::addStyle(wb, sheet_name, style = row_bg_style,
                       rows = rows_to_style,
                       cols = 2:(1 + length(years)),
                       gridExpand = TRUE, stack = TRUE)

    # Style for habitat labels (Column A)
    label_col_style <- openxlsx::createStyle(
      fgFill = current_color,
      textDecoration = "bold",
      border = "Right",
      halign = "left"
    )

    openxlsx::addStyle(wb, sheet_name, style = label_col_style,
                       rows = rows_to_style, cols = 1)

    current_row <- current_row + group_size
  }

  # 4. Final Cleanup
  # Corner A1 Style
  openxlsx::addStyle(wb, sheet_name, style = openxlsx::createStyle(fgFill = "white"),
                     rows = 1, cols = 1, stack = FALSE)

  openxlsx::setColWidths(wb, sheet_name, cols = 1, width = 50)
  openxlsx::setColWidths(wb, sheet_name, cols = 2:(1 + length(years)), width = 5)
  openxlsx::freezePane(wb, sheet_name, firstActiveRow = 2, firstActiveCol = 2)
}

#' Write Indicator Directory Sheet
#'
#' @param wb A workbook object
#' @param sheet_name Name of the sheet
#' @param ci_names Vector of indicator names (ns_dirty_ci_names)
#' @param es_types Vector of ES categories (names(ns_dirty_es_label_tree))
#' @param source_data Optional matrix or data frame of relevance scores
write_indicator_directory <- function(wb, sheet_name, ci_names, es_types, source_data = NULL) {

  openxlsx::addWorksheet(wb, sheet_name)

  # 1. Prepare the Header Row
  # Column A is "Condition Indicator", followed by the ES Types
  headers <- c("Condition Indicator", es_types)
  openxlsx::writeData(wb, sheet_name, t(headers), startCol = 1, startRow = 1, colNames = FALSE)

  # 2. Write the Indicator Names (Column A)
  openxlsx::writeData(wb, sheet_name, ci_names, startCol = 1, startRow = 2, colNames = FALSE)

  # 3. Write Data (Scores)
  if (is.null(source_data)) {
    # Default to 0 if no data provided
    dir_data <- matrix(0, nrow = length(ci_names), ncol = length(es_types))
  } else {
    dir_data <- source_data
  }
  openxlsx::writeData(wb, sheet_name, dir_data, startCol = 2, startRow = 2, colNames = FALSE)

  # 4. Styling

  # Vertical Header Style for ES Types (Cols 2 onwards)
  # Reusing your vertical logic but without the 90-degree rotation unless you prefer it
  # Here I'll keep it bold and centered to match the Matrix headers
  dir_header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    fgFill = "#DCE6F1",
    border = "Bottom"
  )
  openxlsx::addStyle(wb, sheet_name, style = dir_header_style, rows = 1, cols = 1:(1 + length(es_types)))

  # Alternating Row Styles
  grey_style <- openxlsx::createStyle(fgFill = "#F2F2F2", halign = "center")
  white_style <- openxlsx::createStyle(fgFill = "white", halign = "center")
  indicator_col_style <- openxlsx::createStyle(textDecoration = "bold", halign = "left", border = "Right")

  for (i in seq_along(ci_names)) {
    this_row <- i + 1
    this_style <- if (i %% 2 == 0) grey_style else white_style

    # Apply background to the data area (Cols 2+)
    openxlsx::addStyle(wb, sheet_name, style = this_style,
                       rows = this_row, cols = 2:(1 + length(es_types)), gridExpand = TRUE)

    # Apply bold label style to Column A (keeps it distinct)
    openxlsx::addStyle(wb, sheet_name, style = indicator_col_style, rows = this_row, cols = 1)
  }

  # 5. Dimensions
  openxlsx::setColWidths(wb, sheet_name, cols = 1, width = 60)
  openxlsx::setColWidths(wb, sheet_name, cols = 2:(1 + length(es_types)), width = 20)
  openxlsx::freezePane(wb, sheet_name, firstActiveRow = 2, firstActiveCol = 2)
}


#' Write Condition Scores Time-Series Sheet
#'
#' @param wb A workbook object
#' @param sheet_name Name of the sheet
#' @param ci_names Vector of full indicator names (ns_dirty_ci_names)
#' @param years Numeric vector (e.g., 2000:2019)
#' @param source_data Optional matrix or data frame of scores
write_condition_scores_sheet <- function(wb, sheet_name, ci_names, years, source_data = NULL) {

  openxlsx::addWorksheet(wb, sheet_name)

  # 1. Write headers and labels
  # Column A is "Year", Row 1 (B onwards) are the Indicators
  openxlsx::writeData(wb, sheet_name, "Year", startCol = 1, startRow = 1)
  openxlsx::writeData(wb, sheet_name, t(ci_names), startCol = 2, startRow = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet_name, years, startCol = 1, startRow = 2, colNames = FALSE)

  # 2. Write data (source or zeros)
  if (is.null(source_data)) {
    score_data <- matrix(0, nrow = length(years), ncol = length(ci_names))
  } else {
    score_data <- source_data
  }
  openxlsx::writeData(wb, sheet_name, score_data, startCol = 2, startRow = 2, colNames = FALSE)

  # 3. Styling

  # Rotated Header Style for Indicators (to match Matrix style)
  ci_header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    valign = "bottom",
    fgFill = "#DCE6F1",
    border = "Bottom",
    textRotation = 90,
    fontSize = 9
  )

  # Apply to the whole top row (Year + Indicators)
  openxlsx::addStyle(wb, sheet_name, style = ci_header_style,
                     rows = 1, cols = 1:(1 + length(ci_names)))

  # Alternating row colors for better tracking across many columns
  grey_row <- openxlsx::createStyle(fgFill = "#F2F2F2", halign = "center")
  white_row <- openxlsx::createStyle(fgFill = "white", halign = "center")
  year_col_style <- openxlsx::createStyle(textDecoration = "bold", border = "Right", halign = "center")

  for (i in seq_along(years)) {
    this_row <- i + 1
    this_style <- if (i %% 2 == 0) grey_row else white_row

    # Style data area
    openxlsx::addStyle(wb, sheet_name, style = this_style,
                       rows = this_row, cols = 2:(1 + length(ci_names)), gridExpand = TRUE)

    # Style Year column
    openxlsx::addStyle(wb, sheet_name, style = year_col_style, rows = this_row, cols = 1)
  }

  # 4. Dimensions & Freeze
  openxlsx::setRowHeights(wb, sheet_name, rows = 1, heights = 180) # Space for rotated text
  openxlsx::setColWidths(wb, sheet_name, cols = 1, width = 10)
  openxlsx::setColWidths(wb, sheet_name, cols = 2:(1 + length(ci_names)), width = 6)
  openxlsx::freezePane(wb, sheet_name, firstActiveRow = 2, firstActiveCol = 2)
}


# --- EXECUTION ---
wb <- openxlsx::createWorkbook()

# 1. Habitat Extent input
write_extent_sheet(wb, "Habitat Extent", ns_dirty_habitats_label_tree, ns_year_list)

# 2. Weights - ES Potential per SPU input
esppu_skeleton <- prepare_template_matrix(ns_dirty_habitats_label_tree, ns_dirty_es_label_tree)
write_input_matrix(wb, "Provision Per Unit", esppu_skeleton,
                   ns_dirty_habitats_label_tree, ns_dirty_es_label_tree, style_obj,
                   instruction = "Enter exemplary ecosystem service potential per service-providing unit - score out of 5")

# 3. Weights - Importance
# Note: This uses the function we just designed for the vertical layout
write_importance_sheet(wb, "Importance", ns_dirty_es_label_tree)

# 4. Condition Indicator
write_condition_scores_sheet(wb, "Condition Indicator Scores",
                             ns_dirty_ci_names,
                             ns_year_list)

# 5. Condition Indicator directory
write_indicator_directory(wb, "Condition Indicator Salience",
                          ns_dirty_ci_names,
                          names(ns_dirty_es_label_tree))

# 6. Condition Indicator relevance matrices
for(ci_name in ns_dirty_ci_names) {

  # Clean the name for Excel sheet compatibility
  clean_name <- stringr::str_replace_all(ci_name, "[[:punct:]]", " ")
  short_name <- trimws(substr(clean_name, 1, 31))

  # Build a 0-filled skeleton for this specific indicator
  ci_skeleton <- prepare_template_matrix(ns_dirty_habitats_label_tree, ns_dirty_es_label_tree)

  # Write the sheet using the matrix function
  write_input_matrix(wb, short_name, ci_skeleton,
                     ns_dirty_habitats_label_tree, ns_dirty_es_label_tree,
                     style_obj,
                     instruction = "Enter 1 or 0 in each cell of the matrix to denote whether this condition indicator is relevant in gauging flow of each ecosystem service from each habitat")
}

# 4. Save final workbook
openxlsx::saveWorkbook(wb, template_out, overwrite = TRUE)
