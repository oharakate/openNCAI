check_missing <- function(df){
  countna <- apply(X = df, FUN = function(x){sum(is.na(x))}, MARGIN = 1)
  if(sum(countna) == 0){print("Complete data supplied.")}
  if(sum(countna) != 0){
    ntoreport <- ifelse(nrow(df)<3, nrow(df), 3)
    print(paste0("openNCAI expects no missing data. The supplied data has missingness. For example, the ", ntoreport, " indicators with the highest proportion of missing data are:"))
    dfna <- data.frame(rownames(df), countna/ncol(df))
    colnames(dfna) <- c("ci", "napct")
    napctorder <- order(dfna$napct, decreasing = TRUE)
    dfna <- data.frame(dfna$ci[napctorder], dfna$napct[napctorder])
    dfna <- dfna[1:ntoreport,]
    for(i in 1:ntoreport){
      print(paste0("Condition Indicator ", dfna$ci[i], " which has ", round(dfna$napct[i]*100), "% its of values missing."))
    }
  }
}
