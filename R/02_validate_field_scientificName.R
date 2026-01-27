##############################################################################
## ------------------------------ eDNABridge ------------------------------ ##
## This package was designed and developed by Epi (https://www.epi.group/)  ##
## with funding provided by the New Zealand Ministry for the Environment.   ##
##                                                                          ##
## Support was provided by Wilderlab, the Hawke’s Bay Regional Council,     ##
## Department of Conservation and Earth Sciences New Zealand (NIWA)         ##
## ------------------------------------------------------------------------ ##
##############################################################################


# ID for GBIF Backbone Taxonomy
GBIF_DATASET_ID <- "d7dddbf4-2cf0-4f39-9b2a-bb099caae36c"

#' Convienience function to create a species request
#' @param name A scientific name to check
#' @return A httr2 request object
#' @keywords internal
#' @noRd
val_make_species_request <- function(name) {
  httr2::request("https://api.gbif.org/v1/species") |>
    httr2::req_retry(max_tries = 5, retry_on_failure = TRUE) |>
    httr2::req_throttle(capacity = 600, fill_time_s = 60) |>
    httr2::req_url_query(
      datasetKey = GBIF_DATASET_ID,
      name = name,
      limit = 1
    )
}


#' Check if a scientific name exists in the GBIF Backbone Taxonomy
#' 
#' This function queries the GBIF Species API to determine if the provided scientific name
#' exists in the GBIF Backbone Taxonomy.
#' 
#' @param name A scientific name to check
#' @return The number of matches found in the GBIF Backbone Taxonomy
#' @seealso [val_check_name_list()]
#' @examples
#' \dontrun{
#' val_check_scientific_name("Puma concolor") # should return TRUE
#' val_check_scientific_name("Invalid species name") # should return FALSE
#' }
#' 
#' 
#' @export
val_check_scientific_name <- function(name) {
  result <- val_make_species_request(name) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    magrittr::extract2("endOfRecords") # TRUE if no records found, otherwise FALSE if limit = 1 and records found
  return(!result) # return TRUE if match found, FALSE otherwise
}

#' Check multiple scientific names against the GBIF Backbone Taxonomy
#' 
#' Iterates over a list of scientific names and checks each one against the GBIF Backbone Taxonomy
#' using the GBIF Species API. Returns a data frame indicating which names were found.
#' The underlying API requests are performed in parallel for efficiency, and will show a progress bar
#' unless `quiet = TRUE`. 
#' 
#' Requests will be dynamically retried on failure, and throttled to 10 requests per second.
#' GBIF does not require an API key and has not specified strict rate limits, but this is to be respectful of their servers.
#' 
#' @param name_list A vector of scientific names to check
#' @return A data frame with scientific names and a logical column indicating if a match was found
#' @seealso [val_check_scientific_name()]
#' @examples
#' \dontrun{
#' names_to_check <- c("Puma concolor", "Invalid species name")
#' result_df <- val_check_name_list(names_to_check)
#' print(result_df)
#' # A tibble: 2 × 2
#' #  scientificName          match
#' #  <chr>                   <lgl>
#' #1 Puma concolor          TRUE
#' #2 Invalid species name   FALSE
#' }
#' 
#' 
#' @export
val_check_name_list <- function(name_list, quiet = FALSE) {
  unique_name_list <- unique(name_list)
  match_list <- vector("list", length(unique_name_list))
  names(match_list) <- unique_name_list

  results <- httr2::req_perform_parallel(
    lapply(unique_name_list, val_make_species_request),
    progress = ifelse(!quiet, "Checking names against GBIF backbone", FALSE)
  )

  for (i in seq_along(results)) {
    resp <- results[[i]]
    name <- unique_name_list[i]
    if (httr2::resp_is_error(resp)) {
      match_list[[name]] <- NA
    } else {
      body <- httr2::resp_body_json(resp)
      match_list[[name]] <- body |>
        magrittr::extract2("endOfRecords") |>
        magrittr::not() # TRUE if match found, FALSE otherwise
    }
  }

  match_list |>
    tibble::as_tibble() |> 
    tidyr::pivot_longer(
      dplyr::everything(), 
      names_to = "scientificName", 
      values_to = "match"
    )
}

#' Field class for scientificName with GBIF Backbone Taxonomy validation
#' @keywords internal
#' @noRd
fld_field_scientificName <- R6::R6Class(
  "fld_field_scientificName",
  inherit = fld_base_field,
  public = list(
    initialize = function() {
      super$initialize(
        name = "scientificName",
        dwc_link = "https://dwc.tdwg.org/list/#dwc_scientificName",
        type_check_func = function(x) {
          is.character(x)
        },
        default_error = "error"
      )
    },
    validate_name_exists = function(value_list) {
      # Check if scientific names exist in GBIF Backbone Taxonomy
      name_check_df <- val_check_name_list(unique(value_list))
      missing_names <- name_check_df |>
        dplyr::filter(!match) |>
        dplyr::pull(scientificName)
      if (length(missing_names) > 0) {
        invalid_indices <- which(value_list %in% missing_names)
        return(private$format_issue(
          value_list,
          invalid_indices,
          "name not found in GBIF Backbone Taxonomy",
          error="info"
        ))
      }
      return(NULL)
    },
    validate = function(value_list) {
      validation_issues <- super$validate(value_list)
      name_issues <- self$validate_name_exists(value_list)
      return(list(validation_issues, name_issues))
    }
  )
)