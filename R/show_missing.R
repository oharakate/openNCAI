show_missing <- function(df){

  r <- rownames(df)
  c <- colnames(df)
  countna <- apply(X = df, FUN = function(x){sum(is.na(x))}, MARGIN = 1)
  if(sum(countna) == 0) {print("Complete data supplied.")}
  if(sum(countna) != 0){
    dfna <- data.frame(rownames(df), countna/ncol(df))
    colnames(dfna) <- c("ci", "napct")
    napctorder <- order(dfna$napct, decreasing = TRUE)
    dfna <- data.frame(dfna$ci[napctorder], dfna$napct[napctorder])
    colnames(dfna) <- c("ci", "napct")
    levels(dfna$ci) <- rev(napctorder)

    plot <- ggplot(dfna) + geom_col(aes(y = ci, x = napct*100, fill = napct)) +
      scale_fill_viridis_c(guide = FALSE) +
      theme_classic(base_size = 18) +
      xlab("% of values which are missing.") +
      ylab("Condition Indicator")

    plot

    return(plot)
  }
}
