introduce_nas <- function(df){
  n <- nrow(df)-1
  makena <- sample(1:n)
  for(i in 1:ncol(df)){
    df[sample(1:nrow(df), size = makena[i]),i] <- NA
  }
  return(df)
}
