##############################################################################
## ------------------------------ eDNABridge ------------------------------ ##
## This package was designed and developed by Epi (https://www.epi.group/)  ##
## with funding provided by the New Zealand Ministry for the Environment.   ##
##                                                                          ##
## Support was provided by Wilderlab, the Hawke’s Bay Regional Council,     ##
## Department of Conservation and Earth Sciences New Zealand (NIWA)         ##
## ------------------------------------------------------------------------ ##
##############################################################################


#' Validate Single Column
#' 
#' Validates a single column data frame by calling the corresponding
#' class based on the column name.
#' The column should be passed as a tibble not as a vector.
#' E.g. `df[,1]` instead of `df$col1`.
#' 
#' @param df_col A data frame containing a single column to be validated.
#' @return A list of validation results, including any issues found during the validation process.
#' @keywords internal
#' @noRd
val_validate_column <- function(df_col) {
  col_name <- names(df_col)
  if (length(col_name) != 1) {
    stop("df_col must be a single column data frame")
  }
  validator <- fld_factory(col_name)
  validation_results <- validator$validate(df_col[[1]])
  return(validation_results)
}

#' Validate All Fields in Data Frame
#' 
#' This function serves as an orchestrator to validate all fields in a given data frame.
#' It builds each validator class based on the column names of the data frame and applies the respective
#' validation methods to ensure data integrity.
#' 
#' @param df A data frame containing the data to be validated.
#' @return A list of validation results, including any issues found during the validation process.
#' @examples
#' # Example data frame
#' df <- data.frame(
#'   scientificName = c("Puma concolor", "Invalid species name", "Canis lupus"),
#'   eventDate = c("2020-01-01", "2020-11-20", "Invalid Date")
#' )
#' # Validate all fields
#' issues <- val_validate_all_fields(df)
#' if (!is.null(issues)) {
#'   print(issues)
#' } else {
#'   print("No issues found")
#' }
#' 
#' 
#' @export
val_validate_all_fields <- function(df) {
  all_issues <- list()
  for (col_name in names(df)) {
    df_col <- df[, col_name, drop = FALSE]
    validation_results <- val_validate_column(df_col)
    if (!is.null(validation_results)) {
      all_issues <- c(all_issues, validation_results)
    }
  }
  return(val_combine_issues(all_issues))
}