## FUNCTION TO IMPORT THE HABITAT EXTENT DATA FROM THE NATURE SCOT
## NCAI SPREADSHEET
## Kate O'Hara
## 20-12-2025

# THIS DOESN'T NEED TO BE A FUNCTION AND CAN BE REMOVED.


import_extent_data <- function(sheet_path, # path to the spreadsheet
                               sheet_num, # list of sheets containing CI scores
                               cell_range # range where area figures are
                                            # recorded in matrix of habitat/year
                                            # raw figures not indexed
                               ) {

  habitat_year_matrix <- readxl::read_excel(
    path = sheet_path,
    sheet = sheet_num,
    range = cell_range,
    col_names = FALSE,
    col_types = "numeric",
    trim_ws = TRUE,
    .name_repair = "minimal" #quietens reporting on name repair
  )

}

# For Scotland, run on 20-12-2025, no need to repeat:

# See the sheets index:
# excel_sheets("dev/ncai.xlsx")
#
sheet_path <- file.path("dev", "ncai.xlsx")
sheet_num <- 5
cell_range = "E4:AA34"

scot_extent_data <- import_extent_data(sheet_path = sheet_path,
                                       sheet_num = sheet_num,
                                       cell_range = cell_range)

head(scot_extent_data)

write_csv(scot_extent_data, file.path("dev", "scot_extent_data_automated.csv"),
          col_names = FALSE)


