##############################################################################
## ------------------------------ eDNABridge ------------------------------ ##
## This package was designed and developed by Epi (https://www.epi.group/)  ##
## with funding provided by the New Zealand Ministry for the Environment.   ##
##                                                                          ##
## Support was provided by Wilderlab, the Hawke’s Bay Regional Council,     ##
## Department of Conservation and Earth Sciences New Zealand (NIWA)         ##
## ------------------------------------------------------------------------ ##
##############################################################################


# Overall validation against Darwin Core standards
# https://www.gbif.org/data-quality-requirements-occurrences

# Although basisOfRecord is required, this is inserted by the package as all samples will be "MaterialSample"
DWC_REQUIRED <- c("occurrenceID", "eventDate", "scientificName")
DWC_RECOMMENDED <- c(
  "countryCode", "taxonRank", "kingdom", "decimalLatitude", "decimalLongitude",
  "geodeticDatum", "coordinateUncertaintyInMeters", "individualCount", 
  "organismQuantity", "organismQuantityType"
)
DWC_PREFERRED <- c(
  "informationWithheld", "dataGeneralizations", "eventTime", "country"
)
DWC_LEVELS <- factor(
  c("required", "recommended", "preferred"),
  levels = c("required", "recommended", "preferred"),
  ordered = TRUE
)

#' Validates existence of DwC columns to a specified standard
#' 
#' Given a data frame, checks for the presence of required, recommended, or preferred terms
#' as defined by the Darwin Core standard / GBIF data quality requirements.
#' 
#' @param df A data frame to validate
#' @param level A character string specifying the Darwin Core level to validate against.
#' Must be one of "required", "recommended", or "preferred".
#' @return A list of issue tibbles if any issues are found, or NULL if no issues are found
#' @examples
#' # Example data frame missing some recommended terms
#' # Requires internet connection to fetch latest DwC terms
#' \dontrun{
#' df <- data.frame(
#'  occurrenceID = c("occ1", "occ2"),
#' eventDate = c("2020-01-01", "2020-01-02"),
#' scientificName = c("Species A", "Species B"),
#' decimalLatitude = c(34.05, 36.16)
#' )
#' # Validate against recommended level
#' issues <- val_validate_dwc_to_level(df, level = "recommended")
#' if (!is.null(issues)) {
#'   print(issues)
#' } else {
#'   print("No issues found")
#' }
#' # Validate against preferred level
#' issues_preferred <- val_validate_dwc_to_level(df, level = "preferred")
#' if (!is.null(issues_preferred)) {
#'   print(issues_preferred)
#' } else {
#'   print("No issues found")
#' }
#' }
#' 
#' 
#' @export
val_validate_dwc_to_level <- function(df, level) {
  terms_df <- tibble::tibble(term = DWC_REQUIRED, requirement = "required")
  if (level >= DWC_LEVELS[2]) {
    terms_df <- dplyr::bind_rows(
      terms_df,
      tibble::tibble(term = DWC_RECOMMENDED, requirement = "recommended")
    )
  }
  if (level >= DWC_LEVELS[3]) {
    terms_df <- dplyr::bind_rows(
      terms_df,
      tibble::tibble(term = DWC_PREFERRED, requirement = "preferred")
    )
  }
  terms_df <- terms_df |>
    dplyr::mutate(present = term %in% colnames(df))

  issues <- terms_df |>
    dplyr::filter(!present) |>
    dplyr::mutate(error_level = ifelse(requirement == "required", "error", "warning")) |>
    dplyr::mutate(issue = paste("missing", requirement, "Darwin Core term")) |>
    val_tibble_to_issues()

  if (length(issues) > 0) {
    return(issues)
  }
  return(NULL)
}

#' Validates that no unexpected columns are present
#' 
#' Validates that no additional terms are present in the data frame beyond those defined 
#' in the latest Darwin Core standard
#' 
#' @param df A data frame to validate
#' @return A list of issue tibbles if any extra terms are found, or NULL if no extra terms are found
#' @examples
#' # Example data frame with an extra term
#' # Requires internet connection to fetch latest DwC terms
#' \dontrun{
#' df <- data.frame(
#'   occurrenceID = c("occ1", "occ2"),
#'   eventDate = c("2020-01-01", "2020-01-02"),
#'   scientificName = c("Species A", "Species B"),
#'   extraTerm = c("extra1", "extra2")
#' )
#' # Validate for extra terms
#' issues <- val_validate_no_additional_terms(df)
#' if (!is.null(issues)) {
#'  print(issues)
#' } else {
#'   print("No extra terms found")
#' }
#' }
#' 
#' @export
val_validate_no_additional_terms <- function(df) {
  latest_terms <- util_get_latest_dwc_terms(recommendedOnly = FALSE)
  extra_cols <- setdiff(colnames(df), latest_terms$term_localName)
  if (length(extra_cols) > 0) {
    issues <- purrr::map(
      extra_cols,
      function(col) {
        val_create_issue(
          term = col,
          error_level = "warning",
          index = NA,
          value = NA,
          issue = "extra term not defined in Darwin Core standard"
        )
      }
    )
    return(issues)
  }
  return(NULL)
}