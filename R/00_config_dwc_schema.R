##############################################################################
## ------------------------------ eDNABridge ------------------------------ ##
## This package was designed and developed by Epi (https://www.epi.group/)  ##
## with funding provided by the New Zealand Ministry for the Environment.   ##
##                                                                          ##
## Support was provided by Wilderlab, the Hawke’s Bay Regional Council,     ##
## Department of Conservation and Earth Sciences New Zealand (NIWA)         ##
## ------------------------------------------------------------------------ ##
##############################################################################


URL_CORE_SCHEMA <- "https://rs.gbif.org/core/?C=M;O=D"
URL_GBIF_EXTENSION_SCHEMA <- "https://rs.gbif.org/extension/gbif/1.0/?C=M;O=D"
URL_DWC_EXTENSION_SCHEMA <- "https://rs.gbif.org/extension/dwc/?C=M;O=D"

#' Fetches the latest DWC schema from URL
#' 
#' By using the sort parameters in the URL, this function retrieves the most recent schema file.
#' 
#' @param url The URL to fetch the schema from
#' @param name The name of the file to match (e.g., "occurrence")
#' @return The URL of the latest schema file
#' @keywords internal
#' @noRd
latest_dwc_schema_from_url <- function(url, name) {
  page <- xml2::read_html(url)
  links <- rvest::html_nodes(page, "a")
  hrefs <- rvest::html_attr(links, "href")
  matched_links <- hrefs[grepl(paste0(name, ".*\\.xml$"), hrefs)]
  if (length(matched_links) == 0) {
    stop(paste("No schema files found for", name))
  }
  latest_link <- matched_links[1]
  full_url <- paste0(sub("\\?C=M;O=D$", "", url), latest_link)
  return(full_url)
}

#' Converts a DWC XML schema to data frame
#' 
#' Reads all the property nodes and extracts all attr/value pairs into a data frame
#' 
#' @param schema_url The URL of the Darwin Core XML schema
#' @return A data frame representing the schema properties
#' @keywords internal
#' @noRd
read_dwc_schema_url <- function(schema_url) {
  schema_xml <- xml2::read_xml(schema_url)
  xml2::xml_ns_strip(schema_xml)
  properties <- xml2::xml_find_all(schema_xml, ".//property")
  prop_list <- lapply(properties, function(prop) {
    attrs <- xml2::xml_attrs(prop)
    as.list(attrs)
  })
  schema_df <- dplyr::bind_rows(prop_list)
  return(schema_df)
}

#' Get latest GBIF Darwin Core schema
#' 
#' Fetches the latest versions of the occurrence core, DNA extension, and measurements or facts extension schemas 
#' Uses the GBIF schema repository URLs to get the most recent files
#' 
#' @param bind Logical indicating whether to bind all schemas into a single data frame (default: FALSE)
#' @return A list of data frames for each schema or a single combined data frame if bind is TRUE
#' @examples
#' # Get all schemas as a list of data frames
#' # Requires an internet connection
#' \dontrun{
#' schemas_list <- get_all_gbif_dwc_schema()
#' print(names(schemas_list))
#' # [1] "occurrence_core" "dna_extension"   "mof_extension" 
#' }
#' 
#' @export
get_all_gbif_dwc_schema <- function(bind = FALSE) {
  core_url <- latest_dwc_schema_from_url(URL_CORE_SCHEMA, "occurrence")
  dna_ext_url <- latest_dwc_schema_from_url(URL_GBIF_EXTENSION_SCHEMA, "dna_derived_data")
  mof_ext_url <- latest_dwc_schema_from_url(URL_DWC_EXTENSION_SCHEMA, "measurements_or_facts")

  files <- list(
    occurrence_core = read_dwc_schema_url(core_url),
    dna_extension = read_dwc_schema_url(dna_ext_url),
    mof_extension = read_dwc_schema_url(mof_ext_url)
  )

  if (bind) {
    combined_df <- dplyr::bind_rows(
      files$occurrence_core |> dplyr::mutate(source = "core"),
      files$dna_extension |> dplyr::mutate(source = "dna_extension"),
      files$mof_extension |> dplyr::mutate(source = "mof_extension")
    )
    return(combined_df)
  } else {
    return(files)
  }
}