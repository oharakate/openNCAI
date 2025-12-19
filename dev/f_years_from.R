## Function to create a list of years to be processed
# Kate O'Hara
# 19-12-2025

# This function will take a start year and a number of consecutive years to
# allow easy creation of a list of years.
# Returns a list of consecutive years.

years_from <- function(start_year, # a year, as integer numeric
                       n_years # the number of consecutive years to process
                       ) {

  as.character(start_year:(start_year + n_years - 1))

}

print(years_from(2000, 3))
