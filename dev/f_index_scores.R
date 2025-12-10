## FUNCTION index_scores() converts matrix of year/ci raw scores to year/ci
## indexed scores
# Requires a matrix shape year / indicator number
index_scores <- function(score_matrix, # matrix year / ci_num of raw scores
                         n_cis # number CIs to handle, from nrow(indd)
                         ) {

  scorecol_names <- paste0("stbi", 1:n_cis)
  names(score_matrix) <- scorecol_names
  working_matrix <- score_matrix

  for (i in 1:n_cis) {
    score_name <- paste0("stbi", i)
    indic_name <- paste0("ind", i)

    y1score <- score_matrix %>%
      pull(!!score_name) %>%
      .[1]

    working_matrix <- working_matrix %>%
      mutate(!!indic_name := (!!sym(score_name) / y1score) * 100)
    # I'm not really familiar with the !! := and !!sym() operators.
    # LLM recommendation!
  }
  index_matrix <- working_matrix %>%
    select(-all_of(scorecol_names))

  return(index_matrix)
}
