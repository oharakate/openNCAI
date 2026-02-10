#' Import Reference Data for Replication Testing
#'
#' Imports selected sheets and ranges from the NatureScot NCAI spreadsheet that
#' contain the intermediate and final results of their calculations. This data
#' is used as a "gold standard" to ensure the `openNCAI` package replicates the
#' methodology reliably.
#'
#' @param path A string representing the file path to the NatureScot .xlsx source.
#' @param habitats_label_tree A named list of habitat labels used for matrix row naming.
#' @param es_label_tree A named list of ecosystem service labels used for matrix column naming.
#'
#' @return A nested list containing:
#' \itemize{
#'   \item \code{ref_espb}: Ecosystem Service Potential Base matrix (Sheet 6).
#'   \item \code{ref_wellbeing_base}: Well-being Base matrix (Sheet 7).
#'   \item \code{ref_tir}: Total Indicator Relevances matrix (Sheet 74).
#'   \item \code{ref_all_year_sheets}: A list of yearly natural capital asset matrices.
#'   \item \code{ref_index_breakdowns}: Data frames containing the final index and its
#'     breakdowns by service type and habitat.
#' }
#' @keywords internal
#' @export
import_ns_testing_data <- function(path,
                                   habitats_label_tree,
                                   es_label_tree,
                                   year_list) {

  # 1. Get the Ecosystem Service Potential Base (ESPB)
  # Calculated from year one habitat extent and the Ecosystem Service Potential
  # Per SPU (ESPPU) weightings.
  ref_espb <- readxl::read_xlsx(path,
                                sheet = 6,
                                range = "F4:AG34",col_names = FALSE,
                                col_types = "numeric",
                                trim_ws = TRUE,
                                .name_repair = "minimal") %>%
    label_ncai_matrix(habitats_label_tree, es_label_tree)

  # 2. Get the Well-being Base
  # Calculated from the Importance Scores and the Ecosystem Service Potential
  # Base.
  ref_wellbeing_base <- readxl::read_xlsx(path,
                                          sheet = 7,
                                          range = "F4:AG34",
                                          col_names = FALSE,
                                          col_types = "numeric",
                                          trim_ws = TRUE,
                                          .name_repair = "minimal"
  ) %>%
    label_ncai_matrix(habitats_label_tree, es_label_tree)

  # 3. Get the Total Indicator Relevances (TIR)
  ref_tir <- readxl::read_xlsx(
    path = path,
    sheet = 74,
    range = "F4:AG34",
    col_names = FALSE,
    col_types = "numeric",
    trim_ws = TRUE,
    .name_repair = "minimal"
  ) %>%
    label_ncai_matrix(habitats_label_tree, es_label_tree)

  # 4. Get the final natural capital assets yearly matrices (year sheets)
  ref_all_year_sheets <- lapply(X = 50:72,
                                FUN = read_ns_year_sheet,
                                path = path,
                                es_label_tree = es_label_tree,
                                habitats_label_tree = habitats_label_tree)

  # 5. Get the final index and the various breakdowns of it:
  # Ranges of the indices:
  index_breakdown_ranges <- c("B2:D24",
                              "B30:D52", "G30:I52", "L30:N52",
                              "B59:D81", "G59:I81", "L59:N81", "Q59:S81",
                              "V59:X81", "AA59:AC81", "AF59:AH81")

  # The breakdown labels:
  index_breakdown_labels <- c(
    "overall",
    names(es_label_tree),
    names(habitats_label_tree)[c(1:6, 8)]
  )

  # Extract:
  ref_index_breakdowns <- lapply(index_breakdown_ranges, function(rng) {
    read_the_indices(
      indices_range = rng,
      path = path,
      sheet = 73,
      year_list = year_list
    )
  }) %>%
    setNames(index_breakdown_labels)

  # Return a list of objects
  return(list(ref_espb = ref_espb,
              ref_wellbeing_base = ref_wellbeing_base,
              ref_tir = ref_tir,
              ref_all_year_sheets = ref_all_year_sheets,
              ref_index_breakdowns = ref_index_breakdowns
              ))

}
#' Read and Label a Specific Yearly Asset Sheet
#'
#' @param sheet The index or name of the Excel sheet to read.
#' @param path String file path to the .xlsx source.
#' @param es_label_tree Named list of ES labels.
#' @param habitats_label_tree Named list of habitat labels.
#'
#' @return A labeled data frame representing the asset matrix for a single year.
#' @keywords internal
read_ns_year_sheet <- function(sheet, path, es_label_tree, habitats_label_tree) {

  year_sheet <- readxl::read_xlsx(
    path = path,
    sheet = sheet,
    range = "F4:AG34",
    col_names = FALSE,
    col_types = "numeric",
    trim_ws = TRUE,
    .name_repair = "minimal" #quietens reporting on name repair
  ) %>%
    label_ncai_matrix(habitats_label_tree, es_label_tree)

  # NAs to 0 as before
  year_sheet[is.na(year_sheet)] <- 0

  # Confirm
  message("Successfully read and processed sheet: ", sheet)

  return(year_sheet)

}
#' Read Index and Breakdown Results
#'
#' Extracts the final index values (total, raw index, and smoothed index) for
#' specific breakdowns (e.g., by habitat or service type) from the results sheet.
#'
#' @param indices_range A string representing the Excel range (e.g., "B2:D24").
#' @param path String file path to the .xlsx source.
#' @param sheet The index or name of the sheet containing the index summaries (usually 73).
#'
#' @return A data frame with years as row names and columns for raw total,
#'   raw index, and smoothed index.
#' @keywords internal
read_the_indices <- function(indices_range,
                             path,
                             sheet,
                             year_list) {
  index_set <- readxl::read_xlsx(
    path = path,
    sheet = sheet,
    range = indices_range,
    col_names = FALSE,
    col_types = "numeric",
    trim_ws = TRUE,
    .name_repair = "minimal"
  ) %>%
    as.data.frame() %>%
    setNames(c("raw_total", "raw_index", "smoothed_index"))
  row.names(index_set) <- as.character(year_list)

  return(index_set)
}
