##############################################################################
## ------------------------------ eDNABridge ------------------------------ ##
## This package was designed and developed by Epi (https://www.epi.group/)  ##
## with funding provided by the New Zealand Ministry for the Environment.   ##
##                                                                          ##
## Support was provided by Wilderlab, the Hawke’s Bay Regional Council,     ##
## Department of Conservation and Earth Sciences New Zealand (NIWA)         ##
## ------------------------------------------------------------------------ ##
##############################################################################


#' Map custom table columns to standard names
#' 
#' A user can provide a custom table with non-standard column names,
#' Along with a mapping vector that maps standard names to custom names
#' 
#' @param table A data frame representing the custom table, or a path to a CSV file
#' @param mapping A named character vector where names are standard column names and values are custom column names
#' @return A data frame with columns renamed to standard names
#' @examples
#' # This example is not specific to Darwin Core, but demonstrates the mapping functionality
#' # Example custom table
#' custom_table <- data.frame(
#'   job_id = 1:3,
#'   sample_code = c("A", "B", "C"),
#'   value = c(10.5, 20.3, 15.8)
#' )
#' # Mapping vector
#' mapping <- c(
#' "job_identifier" = "job_id",
#' "sample_identifier" = "sample_code",
#' "measurement_value" = "value"
#' )
#' # Map the custom table to standard names
#' standard_table <- ingest_read_and_map_table(custom_table, mapping)
#' print(standard_table)
#' #   job_identifier sample_identifier measurement_value
#' # 1              1                 A              10.5
#' # 2              2                 B              20.3
#' # 3              3                 C              15.8
#' 
#' @export
ingest_read_and_map_table <- function(table, mapping = c()) {
  if (is.character(table)) {
    table <- readr::read_csv(table, show_col_types = FALSE)
  }
  if (length(mapping) == 0) {
    return(table)
  }
  return(table |>
    dplyr::rename(mapping) |>
    dplyr::select(tidyselect::all_of(names(mapping)))
  )
}