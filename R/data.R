#' Habitat Extent for Scotland
#'
#' A table containing the annual extent (area) of various habitats.
#'
#' @format A data frame:
#' \itemize{
#'   \item Rows represent broad habitats matching the habitats label tree.
#'   \item Columns represent years (e.g., 2000 to 2022).
#'   \item Values are numeric representing area in hectares.
#' }
"ns_habitat_extent"

#' Condition Indicator Score Matrix
#'
#' A matrix containing the raw condition indicator scores over the
#' years of the time series.
#'
#' @format A data frame:
#' \itemize{
#'   \item Rows represent years in the time series.
#'   \item Columns represent condition indicators (e.g., "Adult red grouse density").
#'   \item Values are numeric condition scores.
#' }
"ns_ci_scores"

#' Habitats Label Tree
#'
#' A hierarchical tree mapping broad habitat groups to specific habitat types.
#'
#' @format A named list of character vectors:
#' \itemize{
#'   \item Names represent broad habitat groups (e.g., "Coastal habitats").
#'   \item Values are character vectors of specific habitat types (e.g., "Coastal shingle").
#' }
"ns_habitats_label_tree"

#' Ecosystem Services Label Tree
#'
#' A hierarchical tree mapping ecosystem service types to specific ecosystem
#' services.
#'
#' @format A named list of character vectors:
#' \itemize{
#'   \item Names represent ecosystem service types (e.g., "Provisioning").
#'   \item Values are character vectors of specific ecosystem services (e.g., "Cultivated crops").
#' }
"ns_es_label_tree"

#' Year List
#'
#' A list of years for which data is available and over which the index will
#' be calculated.
#'
#' @format A character vector of year labels (e.g., "2000", "2001").
"ns_year_list"

#' Provision Per Unit Scores
#'
#' A matrix of scores denoting the exemplary capacity of each habitat to provide
#' each ecosystem service.
#'
#' @format A data frame:
#' \itemize{
#'   \item Rows represent habitats matching the habitats label tree.
#'   \item Columns represent ecosystem services matching the ecosystem services label tree.
#'   \item Values are numeric scores from 0 (No potential) to 5 (Maximum potential).
#' }
"ns_provision_per_unit_scores"

#' Nature Scot Custom Divisor Matrix
#'
#' A matrix containing numbers by which the Ecosystem Service Potential per
#' Unit Scores will be divided to calculate a weight.
#'
#' @format A data frame:
#' \itemize{
#'   \item Rows represent habitats matching the habitats label tree.
#'   \item Columns represent ecosystem services matching the ecosystem services label tree.
#'   \item Values are numeric divisors used for weight normalization.
#' }
"ns_custom_divisor_matrix"

#' Ecosystem Service Importance Scores (between-service-type)
#'
#' A set of scores denoting the importance of each type
#' group of ecosystem services to Scotland.
#'
#' @format A named list of scores where values range from 0 to 20,
#' representing the relative importance of each ecosystem service type. Item
#' names are ecosystem service types.
"ns_between_importance_scores"

#' Ecosystem Service Importance Scores (within-service-type)
#'
#' Sets of scores denoting the importance of each ecosystem service
#' to Scotland, within its type group.
#'
#' @format A named list of named lists of scores:
#' \itemize{
#'   \item Names represent ecosystem service types.
#'   \item Names of scores represent individual ecosystem services
#'   \item Values are numeric scores (0-20) for specific services within that group.
#' }
"ns_within_importance_scores"

#' Indicator Directory
#'
#' A table recording the salience of habitat condition scores as
#' indicators of the likely flow of services under each ecosystem service type.
#'
#' @format A data frame with the following columns:
#' \itemize{
#'   \item \code{ci_id}: Unique condition indicator identifier.
#'   \item \code{provisioning}: Salience for Provisioning services.
#'   \item \code{regulation_and_maintenance}: Salience for Regulation and Maintenance services.
#'   \item \code{cultural}: Salience for Cultural services.
#' }
"ns_indicator_directory"

#' Condition Indicator Relevance Matrix List
#'
#' A set of matrices recording whether or not each condition indicator is
#' relevant to each habitat/ecosystem service combination.
#'
#' @format A named list of data frames:
#' \itemize{
#'   \item Names are condition indicator IDs matching \code{ns_indicator_directory}.
#'   \item Each data frame has habitats as rows and ecosystem services as columns.
#'   \item Values are binary (0 or 1) indicating relevance.
#' }
"ns_ci_relevance_matrices"

#' Display name version of Habitats Label Tree
#'
#' Display name version (no string cleaning) version of \code{ns_habitats_label_tree}
#' which is used internally.
#'
#' @format A named list of character vectors:
#' \itemize{
#'   \item Names represent broad habitat groups (e.g., "Coastal habitats").
#'   \item Values are character vectors of specific habitat types (e.g., "Coastal shingle").
#' }
"ns_display_habitats_label_tree"

#' Display name version of Ecosystem Services Label Tree
#'
#' Display name version (no string cleaning) version of \code{ns_es_label_tree}
#' which is used internally.
#'
#' @format A named list of character vectors:
#' \itemize{
#'   \item Names represent ecosystem service types (e.g., "Provisioning").
#'   \item Values are character vectors of specific ecosystem services (e.g., "Cultivated crops").
#' }
"ns_display_es_label_tree"

#' Display names of Condition Indicators
#'
#' Display name (no string cleaning) list of condition indicator names which
#' is used internally.
#'
#' @format A character vector where items are the original number and name of
#' NatureScot's 38 condition indicators:
#'
"ns_display_ci_names"
