##############################################################################
## ------------------------------ eDNABridge ------------------------------ ##
## This package was designed and developed by Epi (https://www.epi.group/)  ##
## with funding provided by the New Zealand Ministry for the Environment.   ##
##                                                                          ##
## Support was provided by Wilderlab, the Hawke’s Bay Regional Council,     ##
## Department of Conservation and Earth Sciences New Zealand (NIWA)         ##
## ------------------------------------------------------------------------ ##
##############################################################################


#' Field class for decimalLatitude
#' @keywords internal
#' @noRd
fld_field_decimalLatitude <- R6::R6Class(
  "fld_field_decimalLatitude",
  inherit = fld_base_field,
  public = list(
    initialize = function() {
      super$initialize(
        name = "eventDate",
        dwc_link = "https://dwc.tdwg.org/list/#dwc_decimalLatitude",
        type_check_func = function(x) {
          is.numeric(x)
        },
        default_error = "warning"
      )
    },
    validate_correct_latitude = function(value_list) {
      invalid_indices <- which(value_list < -90 | value_list > 90)
      if (length(invalid_indices) > 0) {
        return(private$format_issue(
          value_list,
          invalid_indices,
          "latitude must be between -90 and 90",
          error="error"
        ))
      }
      return(NULL)
    },
    validate = function(value_list) {
      validation_issues <- super$validate(value_list)
      coord_issues <- self$validate_correct_latitude(value_list)
      return(list(validation_issues, coord_issues))
    }
  )
)

#' Field class for decimalLongitude
#' @keywords internal
#' @noRd
fld_field_decimalLongitude <- R6::R6Class(
  "fld_field_decimalLongitude",
  inherit = fld_base_field,
  public = list(
    initialize = function() {
      super$initialize(
        name = "eventDate",
        dwc_link = "https://dwc.tdwg.org/list/#dwc_decimalLongitude",
        type_check_func = function(x) {
          is.numeric(x)
        },
        default_error = "warning"
      )
    },
    validate_correct_longitude = function(value_list) {
      invalid_indices <- which(value_list < -180 | value_list > 180)
      if (length(invalid_indices) > 0) {
        return(private$format_issue(
          value_list,
          invalid_indices,
          "longitude must be between -180 and 180",
          error="error"
        ))
      }
      return(NULL)
    },
    validate = function(value_list) {
      validation_issues <- super$validate(value_list)
      coord_issues <- self$validate_correct_longitude(value_list)
      return(list(validation_issues, coord_issues))
    }
  )
)
