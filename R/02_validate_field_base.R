##############################################################################
## ------------------------------ eDNABridge ------------------------------ ##
## This package was designed and developed by Epi (https://www.epi.group/)  ##
## with funding provided by the New Zealand Ministry for the Environment.   ##
##                                                                          ##
## Support was provided by Wilderlab, the Hawke’s Bay Regional Council,     ##
## Department of Conservation and Earth Sciences New Zealand (NIWA)         ##
## ------------------------------------------------------------------------ ##
##############################################################################


# To validate fields, we create a general R6 class and methods for it.
# Then have specific fields inherit from that class and add specific validation rules as needed.

#' Factory function to create field objects based on field name
#' When looping over a dataframe's columns, this function can be used to create
#' the appropriate field object for each column based on its name.
#' This allows for dynamic instantiation of field classes and their associated validation methods.
#' @param field_name A character string representing the field name
#' @return An instance of the corresponding field class
#' @keywords internal
#' @noRd
fld_factory <- function(field_name) {
  generator <- tryCatch(
    {
      get(
        paste0("fld_field_", field_name),
        envir = asNamespace(utils::packageName())
      )
    },
    error = function(e) {
      NULL
    }
  )
  if (!is.null(generator)) {
    return(generator$new())
  } else {
    return(fld_field_generic$new(name = field_name))
  }
}

#' Base class for Darwin Core fields
#' This class provides common properties and methods for all Darwin Core fields.
#' Specific field classes can inherit from this base class and implement their own validation logic.
#' @field name The name of the field
#' @field dwc_link A link to the Darwin Core term definition
#' @field type_check_func A function to check the type of field values
#' @field default_error The default error level for validation issues
#'
#' @section Methods: 
#' - validate_missing: Validate that the field does not contain missing values
#' - validate_type: Validate that the field values are of the correct type
#' - validate: Perform all validations for the field and return any issues found
#'
#' @keywords internal
#' @noRd
fld_base_field <- R6::R6Class(
  "fld_base_field",
  public = list(
    name = NULL, # Field name to show in messages
    dwc_link = NULL, # Link to Darwin Core term definition, if needed
    type_check_func = NULL, # Function to check type of field values
    default_error = "warning", # Default error level for validation issues
    initialize = function(name, dwc_link, type_check_func, default_error) {
      self$name <- name
      self$dwc_link <- dwc_link
      self$type_check_func <- type_check_func
      self$default_error <- default_error
    },
    validate_missing = function(value_list) {
      missing_indices <- which(is.na(value_list) | value_list == "")
      if (length(missing_indices) > 0) {
        return(private$format_issue(
          value_list,
          missing_indices,
          "missing value"
        ))
      }
      return(NULL)
    },
    validate_type = function(value_list) {
      invalid_indices <- which(
        !sapply(value_list, function(x) {
          if (is.null(x) || is.na(x)) {
            return(TRUE) # Skip NULL/NA values, they are handled in missing check
          }
          return(self$type_check_func(x))
        })
      )
      if (length(invalid_indices) > 0) {
        return(private$format_issue(
          value_list,
          invalid_indices,
          "invalid type"
        ))
      }
      return(NULL)
    },
    validate = function(value_list) {
      validation_issues <- list()
      missing_issues <- self$validate_missing(value_list)
      if (!is.null(missing_issues)) {
        validation_issues <- append(validation_issues, list(missing_issues))
      }
      type_issues <- self$validate_type(value_list)
      if (!is.null(type_issues)) {
        validation_issues <- append(validation_issues, list(type_issues))
      }
      if (length(validation_issues) > 0) {
        return(validation_issues)
      }
      return(NULL)
    }
  ),
  private = list(
    format_issue = function(value_list, invalid_indices, issue_description, error_level=self$default_error) {
      val_create_issue(
        term = self$name,
        error_level = error_level,
        index = invalid_indices,
        value = value_list[invalid_indices],
        issue = issue_description
      )
    }
  )
)

#' Generic field class for unknown Darwin Core fields
#' This class is used when a specific field class is not defined for a given field name
#' and provides basic validation that always passes.
#' @inheritSection fld_base_field Methods
#' @field name The name of the field (set to "unknown")
#' @field dwc_link A link to the Darwin Core term definition (set to "unknown")
#' @field type_check_func A function that always returns TRUE
#' @field default_error The default error level for validation issues (set to "warning")
#' 
#' @keywords internal
#' @noRd
fld_field_generic <- R6::R6Class(
  "fld_field_generic",
  inherit = fld_base_field,
  public = list(
    initialize = function(
      name = "unknown"
    ) {
      super$initialize(
        name = name,
        dwc_link = "unknown",
        type_check_func = function(x) {
          TRUE
        },
        default_error = "warning"
      )
    }
  )
)