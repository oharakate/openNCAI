## FUNCTION imp_rtw_within()
# Gets within-service-type IMPORTANCE weights from a df of raw scores, using
# between weights output from imp_rtw_between()

# Expect the ecosystem relative service scores per service, the output of
# imp_rwt_between() and the index of the service type to process
#(index of st/betweenweights).

imp_rtw_within <- function(scores, # v. raw scores per service type, within section
                           between_weights, # v. output of imp_rtw_between()
                           index # integer which service type to process
                           ) {
  # Takes index 1-3 for the appropriate section.
  # Should improve this to make that list of indices soft maybe.
  # Maybe could add a section index column to the df holding the sets of
  # within weights?
  within_weights  <- scores / sum(scores) * between_weights[index,]

  return(within_weights)

}
