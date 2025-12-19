# Script to harvest condition indicator relevance matrices from the NCAI
# spreadsheet (NatureScot)
# Chris Littleboy & Kate O'Hara
 # 09-12-2025

# install.packages("readxl")
library(readxl)
library(dplyr)
library(readr)


# FUNCTION read_one_cirm gets the habitat/service type matrix containing the
# weights from NatureScot spreadhseet.
read_one_cirm <- function(spreadsheet_path, sheet, matrix_range) {

  readxl::read_excel(
    path = spreadsheet_path,
    sheet = sheet,
    range = matrix_range,
    col_names = FALSE,
    col_types = "numeric",
    na = "",
    trim_ws = TRUE,
    skip = 0,
    n_max = Inf,
    guess_max = 1000,
    progress = readxl_progress(),
    .name_repair = "unique"
  )
}

## FUNCTION cirms_to_csv() takes the path of the NatureScot spreadsheet, a
# list of the condition indicator sheets, and the regular range of the matrix
# holding the condition indicator matrix of weights organised by habitat/
# ecosystem service. It converts each CI's weights matrix to a matrix of binary
# indicators denoting whether the index is applicable to each habitat/ecosystem
# service combination. It saves writes these cirms (condition
# indicator relevance matrices) to a batch of regularly named csvs in a special
# folder. These will be processed in the main calculation script.
cirms_to_csv <- function(spreadsheet_path, sheet_list, matrix_range) {

  # Check there is a folder to save in:
  dir.create(file.path("dev", "cirms"), recursive = TRUE, showWarnings = FALSE)

  # Initialise counter
  counter <- 0

  # Loop through the provided list of sheets, replacing any non-zero value with
  # 1 (relies on NatureScot's practice of filling the weights in the matrix
  # and no weights being equal to zero.)
  for (i in sheet_list) {
    counter <-  counter + 1
    sheet_to_read <- i
    output_object_name <- paste0("scot_cirm", counter)
    save_path <- file.path("dev", "cirms", paste0(output_object_name, ".csv"))

    cirm_df <- read_one_cirm(spreadsheet_path, sheet_to_read, matrix_range)
    binarised <- cirm_df %>%
      mutate(across(everything(), ~ {
        case_when(
          is.na(.x) ~ 0,
          .x != 0   ~ 1,
          TRUE      ~ 0
        )
      }))

    # Write these to csv in a folder of their own.
    readr::write_csv(binarised, save_path, col_names = FALSE)

  }
}



# For NatureScot (run on 19-12-25; no need to do again)
# See the index of sheets:
# excel_sheets("dev/ncai.xlsx")

# Process the condition indicator relevance matrices and save as CSV:
# nature_scot_ss_path <- "dev/ncai.xlsx"
# sheet_list <- 10:47
# matrix_range <- "F4:AG34"

# cirms_to_csv(spreadsheet_path = nature_scot_ss_path, sheet_list, matrix_range)
