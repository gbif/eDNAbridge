##############################################################################
## ------------------------------ eDNABridge ------------------------------ ##
## This package was designed and developed by Epi (https://www.epi.group/)  ##
## with funding provided by the New Zealand Ministry for the Environment.   ##
##                                                                          ##
## Support was provided by Wilderlab, the Hawke’s Bay Regional Council,     ##
## Department of Conservation and Earth Sciences New Zealand (NIWA)         ##
## ------------------------------------------------------------------------ ##
##############################################################################


#' Fetch the latest Darwin Core terms from the official GitHub repository.
#' Optionally filter to only include recommended terms.
#' 
#' @param recommendedOnly Logical; if TRUE, only recommended terms are returned.
#' @return A data frame containing the latest Darwin Core terms.
#' 
#' @keywords internal
#' @noRd
util_get_latest_dwc_terms <- function(recommendedOnly = TRUE) {
  url <- "https://raw.githubusercontent.com/tdwg/dwc/refs/heads/master/vocabulary/term_versions.csv"
  terms <- readr::read_csv(url, show_col_types = FALSE)
  if (recommendedOnly) {
    terms <- terms |> 
      dplyr::filter(status == "recommended")
  }
  return(terms)
}