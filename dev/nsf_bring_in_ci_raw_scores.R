# Script to harvest raw condition indicator scores (NatureScot)
# Kate O'Hara
# 17-12-2025

# install.packages("readxl")
library(readxl)
library(dplyr)
library(readr)

ncai_sheet_path <- file.path("dev", "ncai.xlsx")

excel_sheets(ncai_sheet_path)

ncai_sheet_list <- 10:47
ncai_common_range <- "I36:I58"

read_the_ci_scores <- function(sheet_path, # path to the spreadsheet
                               sheet_list, # list of sheets containing CI scores
                               vector_range # SINGLE-COLUMN range where scores
                                            # are; must be same in each sheet.
                               ) {

  list_of_vectors <-  list()

  for (i in sheet_list) {

    raw_score_data <- readxl::read_excel(
      path = sheet_path,
      sheet = i,
      range = vector_range,
      col_names = FALSE,
      col_types = NULL,
      # na = "",
      trim_ws = TRUE,
      # skip = 0,
      # n_max = Inf,
      # guess_max = 1000,
      # progress = readxl_progress(),
      .name_repair = "minimal" #quietens reporting on name repair
    )

    list_of_vectors[[i]] <- unname(dplyr::pull(raw_score_data))
  }

  ci_scores_df <- bind_cols(list_of_vectors)

  return(ci_scores_df)

}


# Read in the CI raw scores from NatureScot sheet:
# (Note that by raw scores we mean the complete list of scores per year per CI,
# after any smoothing, extrapolation, etc. )

# Path
ncai_sheet_path <- file.path("dev", "ncai.xlsx")
# Check sheet numbers:
excel_sheets(ncai_sheet_path)
ncai_sheet_list <- 10:47
# Set range where list of CI raw scores per year are recorded. Assumed to be
# the same in each sheet, and have no missing data.
ncai_common_range <- "I36:I58"

# Read in:
stbi_matrix <- read_the_ci_scores(ncai_sheet_path,
                                  ncai_sheet_list,
                                  ncai_common_range)
