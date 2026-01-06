## FUNCTION imp_rtw_within()
# Gets within-service-type IMPORTANCE weights from a df of raw scores, using
# between weights output from imp_rtw_between()

# Expect the ecosystem relative service scores per service, the output of
# imp_rwt_between() and the index of the service type to process
#(index of st/betweenweights).

importance_rtw_within <- function(within_scores, between_weights, index) {

  within_weights  <- within_scores / sum(within_scores) * between_weights[index, 1]

  return(within_weights)
}
