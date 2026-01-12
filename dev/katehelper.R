getsheets <- function(x, cn){
  sheet <- readxl::read_excel(
    path = "dev/ncai.xlsx",
    sheet = x,
    range = "F4:AG34",
    col_names = FALSE,
    col_types = "numeric",
    trim_ws = TRUE,
    .name_repair = "minimal" #quietens reporting on name repair
  )
  colnames(sheet) <- cn
  sheet <- data.frame(sheet)
  return(sheet)
}

build_ncai_matrix <- function(wb, ityc_list, ed, target_year, year_one) {

  # Convert to characters for safe column indexing

  target_str <- as.character(target_year)
  origin_str <- as.character(year_one)

  # Extract extent vectors from the ed matrix/df
  ed_target_vec <- ed[[target_str]]
  ed_origin_vec <- ed[[origin_str]]

  extent_index <- (ed_target_vec / ed_origin_vec) * 100
  ityc <- ityc_list[[target_year-1999]] # because 2000 is the first list element, 2000-1999 = 1
  wb_tyc <- as.matrix(ityc) * as.matrix(wb)
  # And multiply in the indexed habitat extent values for that year
  ncai_matrix <- sweep(
    x = wb_tyc,
    MARGIN = 1, # Apply to Rows
    STATS = extent_index,
    FUN = "*"
  )
  return(as.data.frame(ncai_matrix/10000))
}

scot_tycs <- lapply(X = 2000:2022,
                    raw_cis = scot_raw_ci_score_matrix,
                    FUN = build_tyc,
                    year_list = scot_year_list,
                    ciwms_list = all_ciwms_list,
                    tir = scot_tir_with2,
                    addon = 2)

scot_itcs <- lapply(X = scot_tycs, FUN = build_indexed_tyc,year_one_rtyc = scot_2000_tyc)

ncai_matrices <- lapply(X = 2000:2022, FUN = build_ncai_matrix,
                        wb = wb,
                        ityc_list = scot_itcs,
                        ed = ed,
                        year_one = 2000)

cn <- colnames(ncai_matrices[[1]])

ncai_sheets <- lapply(X = 50:72, FUN = getsheets, cn = cn)

