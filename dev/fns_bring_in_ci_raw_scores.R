# Script to harvest raw condition indicator scores (NatureScot)
# Kate O'Hara
# 17-12-2025

# install.packages("readxl")
library(readxl)
library(dplyr)
library(readr)

## FUNCTION read_the_ci_scores()
# Takes NatureScot's spreadsheet and extracts the vector of raw condition
# indicator scores for each CI and joins these into a matrix with numbered
# columns.
read_the_ci_scores <- function(sheet_path, # path to the spreadsheet
                               sheet_list, # list of sheets containing CI scores
                               vector_range # SINGLE-COLUMN range where scores
                                            # are; must be same in each sheet.
                               ) {

  # Get details of range - useful for testing/changing
  # limits <- as.numeric(unlist(regmatches(vector_range, gregexpr("[0-9]+", vector_range))))
  # expected_rows <- limits[2] - limits[1] + 1

  # Initialise list of score vectors
  list_of_vectors <-  list()

  # Loop through list of sheets, reading in vector of scores
  for (idx in seq_along(sheet_list)) {

    actual_sheet_index <- sheet_list[idx]

    raw_score_data <- readxl::read_excel(
      path = sheet_path,
      sheet = actual_sheet_index,
      range = vector_range,
      col_names = FALSE,
      col_types = "numeric",
      trim_ws = TRUE,
      .name_repair = "minimal" #quietens reporting on name repair
    )

    # Force the vector to be the expected length to dodge unexpected Excel
    # behaviour:
    vec <- as.vector(as.matrix(raw_score_data))

    # Add vector to list
    list_of_vectors[[paste0("ind", idx)]] <- vec

    # Confirmation message hopefully:
    cat("Processed column", idx, "(Sheet", actual_sheet_index, ")\n")
  }

  # Make list of vecs into df:
  ci_scores_df <- dplyr::bind_cols(list_of_vectors)

  return(ci_scores_df)

}

# For NatureScot:
# Run on 23-12-2025 and no need to repeat.
# Read in the CI raw scores from NatureScot sheet:
# (Note that by raw scores we mean the complete list of scores per year per CI,
# after any smoothing, extrapolation, etc. )

# Path
# ncai_sheet_path <- file.path("dev", "working_excel2.xlsx")
# Check sheet numbers:
# excel_sheets(ncai_sheet_path)
# ncai_sheet_list <- 10:47
# Set range where list of CI raw scores per year are recorded. Assumed to be
# the same in each sheet, and have no missing data.
# ncai_common_range <- "I36:I58"

# Read in:
# scot_ci_raw_scores_matrix <- read_the_ci_scores(ncai_sheet_path,
#                                   ncai_sheet_list,
#                                   ncai_common_range)

# View(stbi_matrix)
# Checked this on 19-12-2025
# manual_ci_matrix <- read_csv("dev/scot_year_ci_matrix.csv", col_names = TRUE)
# all.equal(as.data.frame(stbi_matrix), as.data.frame(manual_ci_matrix))

# Saving this automated version:
# write_csv(scot_stbi_matrix, file.path("dev", "scot_year_ci_matrix_automated.csv"))

# And moving the one we created manually to "archive".
