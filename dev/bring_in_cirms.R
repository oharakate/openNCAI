# Script to harvest condition indicator relevance matrices from the NCAI
# spreadsheet
# Chris Littleboy & Kate O'Hara
 # 09-12-2025

# install.packages("readxl")
library(readxl)
library(dplyr)
library(readr)


excel_sheets("dev/ncai.xlsx")

sheet_list <- 10:47

read_the_cirms <- function(sheet) {

  readxl::read_excel(
    path = "dev/ncai.xlsx",
    sheet = sheet,
    range = "F4:AG34",
    col_names = FALSE,
    col_types = NULL,
    na = "",
    trim_ws = TRUE,
    skip = 0,
    n_max = Inf,
    guess_max = 1000,
    progress = readxl_progress(),
    .name_repair = "unique"
  )

}

counter <- 0

for (i in sheet_list) {
  counter <-  counter + 1
  sheet_to_read <- i
  output_object_name <- paste0("scot_cirm", counter)
  save_path <- file.path("dev", paste0(output_object_name, ".csv"))

  cirm_df <- read_the_cirms(sheet_to_read)
  binarised <- cirm_df %>%
    mutate(across(everything(), ~ {
      case_when(
        is.na(.x) ~ 0,
        .x != 0   ~ 1,
        TRUE      ~ 0
      )
    }))

  # assign(output_object_name, binarised, envir = .GlobalEnv)

  readr::write_csv(binarised, save_path, col_names = FALSE)

}
