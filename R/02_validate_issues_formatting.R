##############################################################################
## ------------------------------ eDNABridge ------------------------------ ##
## This package was designed and developed by Epi (https://www.epi.group/)  ##
## with funding provided by the New Zealand Ministry for the Environment.   ##
##                                                                          ##
## Support was provided by Wilderlab, the Hawke’s Bay Regional Council,     ##
## Department of Conservation and Earth Sciences New Zealand (NIWA)         ##
## ------------------------------------------------------------------------ ##
##############################################################################


#' Create a tibble row for validation warnings
#' @param term The term being validated
#' @param error_level The severity level of the issue (e.g., "warning", "error")
#' @param index The index of the value in the dataset
#' @param value The invalid value
#' @param issue A description of the issue
#' @return A tibble with columns: term, index, value, issue
#' @keywords internal
#' @noRd
val_create_issue <- function(term, error_level, issue, index=NA, value=NA) {
  tibble::tibble(
    term = term,
    error_level = error_level,
    issue = issue,
    index = as.integer(index),
    value = as.character(value)
  )
}

#' Convert a tibble of issues into a list of issue tibbles
#' Ensures consistent structure for further processing, even if we remake the tibble later
#' @param issues_tibble A tibble containing validation issues
#' @return A list of tibbles, each representing a validation issue
#' @keywords internal
#' @noRd
val_tibble_to_issues <- function(issues_tibble) {
  if (is.null(issues_tibble)) {
    return(NULL)
  }
  issue_list <- purrr::pmap(
    issues_tibble,
    function(term, error_level, issue, index=NA, value=NA, ...) {
      val_create_issue(
        term = term,
        error_level = error_level,
        issue = issue,
        index = index,
        value = value
      )
    }
  )
  return(issue_list)
}

#' Convenience function to combine a list of issue tibbles into one
#' @param issue_list A list of tibbles, each representing validation issues
#' @return A single tibble combining all issues, or NULL if the list is empty
#' @keywords internal
#' @noRd
val_combine_issues <- function(issue_list) {
  if (length(issue_list) == 0) {
    return(NULL)
  }
  return(dplyr::bind_rows(issue_list))
}