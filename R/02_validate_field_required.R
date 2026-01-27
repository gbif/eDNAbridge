##############################################################################
## ------------------------------ eDNABridge ------------------------------ ##
## This package was designed and developed by Epi (https://www.epi.group/)  ##
## with funding provided by the New Zealand Ministry for the Environment.   ##
##                                                                          ##
## Support was provided by Wilderlab, the Hawke’s Bay Regional Council,     ##
## Department of Conservation and Earth Sciences New Zealand (NIWA)         ##
## ------------------------------------------------------------------------ ##
##############################################################################


#' Field class for occurrenceID
#' @keywords internal
#' @noRd
fld_field_occurrenceID <- R6::R6Class(
  "fld_field_occurrenceID",
  inherit = fld_base_field,
  public = list(
    initialize = function() {
      super$initialize(
        name = "occurrenceID",
        dwc_link = "https://dwc.tdwg.org/list/#dwc_occurrenceID",
        type_check_func = function(x) {
          is.character(x) || is.numeric(x)
        },
        default_error = "error"
      )
    }
  )
)

#' Field class for eventDate
#' @keywords internal
#' @noRd
fld_field_eventDate <- R6::R6Class(
  "fld_field_eventDate",
  inherit = fld_base_field,
  public = list(
    initialize = function() {
      super$initialize(
        name = "eventDate",
        dwc_link = "https://dwc.tdwg.org/list/#dwc_eventDate",
        type_check_func = function(x) {
          if (is.character(x)) {
            # Try to parse date strings
            parsed_date <- suppressWarnings(lubridate::as_date(x))
            return(!is.na(parsed_date))
          } else {
            return(lubridate::is.Date(x))
          }
        },
        default_error = "error"
      )
    }
  )
)
