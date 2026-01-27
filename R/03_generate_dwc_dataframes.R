##############################################################################
## ------------------------------ eDNABridge ------------------------------ ##
## This package was designed and developed by Epi (https://www.epi.group/)  ##
## with funding provided by the New Zealand Ministry for the Environment.   ##
##                                                                          ##
## Support was provided by Wilderlab, the Hawke’s Bay Regional Council,     ##
## Department of Conservation and Earth Sciences New Zealand (NIWA)         ##
## ------------------------------------------------------------------------ ##
##############################################################################


#' Split dataframe by Darwin Core file
#' 
#' Converts a singular tibble into a list of Darwin Core data frames.
#' Each data frame in the list corresponds to a Darwin Core extension.
#' 
#' 
#' @param data A tibble containing the data to be converted.
#' @return A named list of tibbles, each representing a Darwin Core extension.
#' @examples
#' # Requires an internet connection to fetch latest DwC schema
#' \dontrun{
#' # Example tibble
#' df <- tibble::tibble(
#'   scientificName = c("Puma concolor", "Canis lupus"),
#'   eventDate = c("2020-01-01", "2020-11-20"),
#'   decimalLatitude = c(34.05, 36.16),
#'   DNA_sequence = c("ATCG", "GCTA"),
#'   measurementType = c("Length", "Weight"),
#'   measurementValue = c(15.5, 22.3)
#' )
#' # Convert to Darwin Core frames
#' dwc_frames <- gen_tibble_to_dwc_frames(df)
#' print(names(dwc_frames))
#' # [1] "occurrence_core" "dna_extension"   "mof_extension"
#' }
#'
#' 
#' @export
gen_tibble_to_dwc_frames <- function(data) {
  dwc_frames <- list()

  data$id <- uuid::UUIDgenerate(n = nrow(data), use.time = TRUE)

  tryCatch(
    {
      dwc_schema <- get_all_gbif_dwc_schema()
    },
    error = function(e) {
      message("Failed to fetch latest Darwin Core schema. Using internal fallback schema.")
      dwc_schema <<- gbif_dwc_schema
    }
  )

  dwc_frames$occurrence_core <- data |>
    gen_tibble_distinct(dwc_schema$occurrence_core$name)
  dwc_frames$dna_extension <- data |>
    gen_tibble_distinct(dwc_schema$dna_extension$name)
  dwc_frames$mof_extension <- data |>
    gen_tibble_distinct(dwc_schema$mof_extension$name)

  return(dwc_frames)
}

gen_tibble_distinct <- function(df, cols) {
  df <- df |>
    dplyr::select(id, dplyr::any_of(cols)) |>
    dplyr::distinct()
  if (nrow(df) == 0 || ncol(df) == 1) {
    return(NULL)
  }
  if (ncol(df) == 2 && setequal(colnames(df), c("id", "occurrenceID"))) {
    return(NULL)
  }
  return(df)
}