##############################################################################
## ------------------------------ eDNABridge ------------------------------ ##
## This package was designed and developed by Epi (https://www.epi.group/)  ##
## with funding provided by the New Zealand Ministry for the Environment.   ##
##                                                                          ##
## Support was provided by Wilderlab, the Hawke’s Bay Regional Council,     ##
## Department of Conservation and Earth Sciences New Zealand (NIWA)         ##
## ------------------------------------------------------------------------ ##
##############################################################################


#' Generate Darwin Core Archive ZIP
#'
#' Generates a Darwin Core Archive ZIP file containing the provided data tables and metadata XML files.
#'
#' @param dwc_tibbles A named list of tibbles representing Darwin Core data tables.
#' @param eml_xml An XML object representing the EML metadata.
#' @param meta_xml An XML object representing the Darwin Core Archive metadata.
#' @param path The file path where the resulting ZIP archive will be saved.
#' @return The file path of the created Darwin Core Archive ZIP file.
#' @examples
#' # Requires an internet connection to fetch latest DwC schema
#' \dontrun{
#' # Example Darwin Core tibbles
#' dwc_tibbles <- list(
#'   occurrence_core = tibble::tibble(
#'     id = c("1", "2"),
#'     scientificName = c("Puma concolor", "Canis lupus"),
#'     eventDate = c("2020-01-01", "2020-11-20")
#'   ),
#'   dna_extension = tibble::tibble(
#'     id = c("1", "2"),
#'     DNA_sequence = c("ATCG", "GCTA")
#'   ),
#'   mof_extension = tibble::tibble(
#'     id = c("1", "2"),
#'     measurementType = c("Length", "Weight"),
#'     measurementValue = c(15.5, 22.3)
#'   )
#' )
#' # Example EML and metadata XML
#' eml_xml <- xml2::read_xml("<eml></eml>")
#' meta_xml <- xml2::read_xml("<archive></archive>")
#' # Generate Darwin Core Archive ZIP
#' dwc_zip_path <- gen_make_dwc_archive(
#'   dwc_tibbles = dwc_tibbles,
#'   eml_xml = eml_xml,
#'   meta_xml = meta_xml,
#'   path = "dwc_archive.zip"
#' )
#' print(dwc_zip_path)
#' }
#'
#'
#' @export
gen_make_dwc_archive <- function(
  dwc_tibbles,
  eml_xml,
  meta_xml,
  path
) {
  temp_dir <- tempdir()

  eml_path <- file.path(temp_dir, "eml.xml")
  xml2::write_xml(eml_xml, eml_path)

  meta_path <- file.path(temp_dir, "meta.xml")
  xml2::write_xml(meta_xml, meta_path)

  table_files <- c()
  for (table_name in names(dwc_tibbles)) {
    # Dont write if empty or none
    if (
      is.null(dwc_tibbles[[table_name]]) || ncol(dwc_tibbles[[table_name]]) == 0
    ) {
      next
    }
    table_path <- file.path(temp_dir, paste0(table_name, ".txt"))
    # Move id to first column
    readr::write_tsv(dwc_tibbles[[table_name]], table_path)
    table_files <- c(table_files, table_path)
  }

  archive_path <- file.path(tempdir(), "dwc_archive.zip")
  zip::zipr(
    archive_path,
    files = c(
      eml_path,
      meta_path,
      table_files
    )
  )

  file.rename(archive_path, path)
  return(path)
}
