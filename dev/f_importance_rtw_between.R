# # FUNCTION imp_rtw_between()
# Gets between-service-provision-type IMPORTANCE weights from a df of raw
# scores.
# Expect ecosystem service relative scores between service types, as a vector.

# Convert sections of raw ratings to actual weights:
# Guidance from excel sheet:
# Get service section weights from a vector of scores
# ("Step 1: ecosystem service section. The most important of the <list> is
# assigned a value of 20, and the other <remainder> are assigned a value
# (between 0 and 20) in terms of their relative importance.
# 1. Provisioning (1.1 thru 1.12)
# 2. Regulation and maintenance (2.1 thru 2.11)
# 3. Cultural services (3.1 through 3.5

importance_rtw_between <- function(between_scores) {
  # between_cores is a vector of between-service-type importance scores
  between_weights <- between_scores / sum(between_scores) * 100

  return(between_weights)
}
