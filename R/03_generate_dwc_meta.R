##############################################################################
## ------------------------------ eDNABridge ------------------------------ ##
## This package was designed and developed by Epi (https://www.epi.group/)  ##
## with funding provided by the New Zealand Ministry for the Environment.   ##
##                                                                          ##
## Support was provided by Wilderlab, the Hawke’s Bay Regional Council,     ##
## Department of Conservation and Earth Sciences New Zealand (NIWA)         ##
## ------------------------------------------------------------------------ ##
##############################################################################


#' Generate base Darwin Core Archive metadata XML
#' 
#' Automatically sets up an empty core node for adding fields later
#' 
#' @return Darwin Core Archive metadata XML object
#' @examples
#' meta_xml <- meta_xml_base()
#' print(xml2::as_xml_document(meta_xml))
#' @export
meta_xml_base <- function() {
  x <- xml2::xml_new_root("archive")
  xml2::xml_set_attr(x, "xmlns", "http://rs.tdwg.org/dwc/text/")
  xml2::xml_set_attr(x, "metadata", "eml.xml")
  return(x)
}

#' Add a file to Darwin Core Archive metadata XML
#' 
#' The function adds a core or extension file description to the Darwin Core Archive metadata XML.
#' It includes attributes for file encoding, field delimiters, and row types.
#' It also maps the columns of the provided data frame to Darwin Core terms based on the GBIF schema.
#' Downstream functions will make tab delimited text files, so the defaults should only be changed
#' if you are designing a custom workflow.
#' 
#' @param xml Darwin Core Archive metadata XML object
#' @param type Type of file to add (e.g., "core" or "extension")
#' @param df Data frame to describe
#' @param location Name of the data file
#' @param encoding File encoding (default: "UTF-8")
#' @param fieldsTerminatedBy Character that separates fields (default: tab)
#' @param linesTerminatedBy Character that separates lines (default: newline)
#' @param fieldsEnclosedBy Character that encloses fields (default: double quote)
#' @param ignoreHeaderLines Number of header lines to ignore (default: 1)
#' @param rowType Row type URI (default: occurrence)
#' @return Updated Darwin Core Archive metadata XML object
#' @examples
#' # Requires an internet connection to fetch latest DwC schema
#' \dontrun{
#' # Example data frame
#' df <- data.frame(
#'  id = c("uuid1", "uuid2"),
#'  scientificName = c("Puma concolor", "Canis lupus"),
#'  eventDate = c("2020-01-01", "2020-11-20")
#' )
#' # Add core file to metadata XML
#' meta_xml <- meta_xml_base() |>
#' meta_xml_add_file(
#'  type="core",
#'  df=df,
#'  location="occurrence_core.txt"
#' )
#' print(xml2::as_xml_document(meta_xml))
#' }
#' 
#' @export
meta_xml_add_file <- function(
  xml,
  type,
  df,
  location,
  encoding="UTF-8",
  fieldsTerminatedBy="\\t",
  linesTerminatedBy="\\n",
  fieldsEnclosedBy="",
  ignoreHeaderLines="1",
  rowType="http://rs.tdwg.org/dwc/terms/Occurrence"
) {
  # Raise an error if a core file already exists
  if (type == "core") {
    existing_cores <- xml2::xml_find_all(xml, ".//core")
    if (length(existing_cores) > 0) {
      stop("A core file already exists in the metadata XML.")
    }
  }

  # Add core or extension node
  node <- xml2::xml_add_child(xml, type)
  xml2::xml_set_attr(node, "encoding", encoding)
  xml2::xml_set_attr(node, "fieldsTerminatedBy", fieldsTerminatedBy)
  xml2::xml_set_attr(node, "linesTerminatedBy", linesTerminatedBy)
  xml2::xml_set_attr(node, "fieldsEnclosedBy", fieldsEnclosedBy)
  xml2::xml_set_attr(node, "ignoreHeaderLines", ignoreHeaderLines)
  xml2::xml_set_attr(node, "rowType", rowType)

  # Add filename from user input
  files_node <- xml2::xml_add_child(node, "files")
  xml2::xml_add_child(files_node, "location", location)

  # Add column index of id, which is always 0
  index_node <- xml2::xml_add_child(node, "id")
  xml2::xml_set_attr(index_node, "index", "0")

  dwc_schema <- get_all_gbif_dwc_schema(bind=TRUE)

  # Add fields based on data frame column names
  # Will fail if a column is not described in DwC schema
  for (i in seq_along(names(df))) {
    if (i == 1) {
      # Skip first column, which is always id
      next
    }
    if (!names(df)[i] %in% dwc_schema$name) {
      stop(paste0("Field '", names(df)[i], "' not found in Darwin Core schema"))
    }

    qual_name <- dwc_schema |>
      dplyr::filter(name == names(df)[i]) |>
      dplyr::slice(1) |>
      dplyr::pull(qualName)

    field_node <- xml2::xml_add_child(
      node,
      "field"
    )
    xml2::xml_set_attr(field_node, "index", as.character(i-1))
    xml2::xml_set_attr(field_node, "term", qual_name)
  }
  
  return(xml)
}

#' Write Darwin Core Archive metadata XML to file
#' 
#' Wrapper function to write the Darwin Core Archive metadata XML to a specified file path.
#' 
#' @param meta_xml Darwin Core Archive metadata XML object
#' @param path File path to write the metadata XML
#' @return The file path where the metadata XML was written
#' @examples
#' # Requires an internet connection to fetch latest DwC schema
#' \dontrun{
#' # Example data frame
#' df <- data.frame(
#'   id = c("uuid1", "uuid2"),
#'   scientificName = c("Puma concolor", "Canis lupus"),
#'   eventDate = c("2020-01-01", "2020-11-20")
#' )
#' # Create metadata XML with core file
#' meta_xml <- meta_xml_base() |>
#' meta_xml_add_file(
#'   type="core",
#'   df=df,
#'   location="occurrence_core.txt"
#' )
#' # Write metadata XML to file
#' meta_path <- meta_write_xml(meta_xml, "meta.xml")
#' print(paste("Metadata XML written to:", meta_path))
#' }
#' 
#' @export
meta_write_xml <- function(meta_xml, path) {
  xml2::write_xml(meta_xml, path)
  return(path)
}