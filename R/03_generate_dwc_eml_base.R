##############################################################################
## ------------------------------ eDNABridge ------------------------------ ##
## This package was designed and developed by Epi (https://www.epi.group/)  ##
## with funding provided by the New Zealand Ministry for the Environment.   ##
##                                                                          ##
## Support was provided by Wilderlab, the Hawke’s Bay Regional Council,     ##
## Department of Conservation and Earth Sciences New Zealand (NIWA)         ##
## ------------------------------------------------------------------------ ##
##############################################################################


#' Create a base EML object for a pipe
#'
#' eml_* functions can be used in a pipe to build up an EML object, which is
#' an xml2 object representing the EML metadata for a Darwin Core Archive. This function
#' creates the base EML object with the required namespaces and dataset node.
#'
#' When using these functions manually, please note that the order in which nodes are
#' added is important. Confer with existing EML files to ensure proper structure.
#'
#' @return EML object
#' @examples
#' eml <- eml_base()
#' print(eml)
#'
#'
#' @export
eml_base <- function() {
  eml <- xml2::xml_new_root("eml:eml")
  xml2::xml_set_attr(
    eml,
    "xmlns:eml",
    "https://eml.ecoinformatics.org/eml-2.2.0"
  )
  xml2::xml_set_attr(eml, "xmlns:dc", "http://purl.org/dc/terms/")
  xml2::xml_set_attr(
    eml,
    "xmlns:xsi",
    "http://www.w3.org/2001/XMLSchema-instance"
  )
  xml2::xml_set_attr(
    eml,
    "xsi:schemaLocation",
    "https://eml.ecoinformatics.org/eml-2.2.0 https://rs.gbif.org/schema/eml-gbif-profile/1.3/eml.xsd"
  )
  xml2::xml_set_attr(eml, "packageId", uuid::UUIDgenerate(use.time = FALSE))
  xml2::xml_set_attr(eml, "system", "uuid")
  xml2::xml_set_attr(eml, "xml:lang", "eng")
  xml2::xml_add_child(eml, "dataset")
  return(eml)
}

#' Set title in EML object
#'
#' Set the title of the dataset, i.e. for everything in a single Darwin Core Archive.
#'
#' @param eml EML object
#' @param title Title string
#' @return EML object with updated title
#' @examples
#' eml <- eml_base() |>
#'  eml_set_title("My Sample Dataset")
#' print(eml)
#'
#' @export
eml_set_title <- function(eml, title) {
  dataset <- xml2::xml_find_first(eml, ".//dataset")
  title <- xml2::xml_add_child(dataset, "title", title)
  xml2::xml_set_attr(title, "xml:lang", "eng")
  return(eml)
}

#' Set abstract in EML object
#'
#' Set the abstract of the dataset, i.e. for everything in a single Darwin Core Archive.
#'
#' @param eml EML object
#' @param abstract Abstract string
#' @return EML object with updated abstract
#' @examples
#' eml <- eml_base() |>
#' eml_set_abstract("This dataset contains sample data.")
#' print(eml)
#'
#' @export
eml_set_abstract <- function(eml, abstract) {
  dataset <- xml2::xml_find_first(eml, ".//dataset")
  abstract_node <- xml2::xml_add_child(dataset, "abstract")
  xml2::xml_add_child(abstract_node, "para", abstract)
  return(eml)
}

#' Set license in EML object
#'
#' Set the license of the dataset, choosing from supported licenses.
#'
#' @param eml EML object
#' @param license_name License name string
#' @param license_identifier License identifier string (optional)
#' @param license_url License URL string (optional)
#' @return EML object with updated license
#' @examples
#' eml <- eml_base() |>
#' eml_set_license("CC-BY-NC-4.0")
#' print(eml)
#'
#' @export
eml_set_license <- function(eml, license_identifier = "CCBYNC4.0") {
  license_identifier <- stringr::str_remove_all(license_identifier, "-") |>
    stringr::str_remove_all(" ") |>
    toupper()
  if (license_identifier == "CCBYNC4.0") {
    license_prefix <- "This work is licensed under a "
    license_name <- "Creative Commons Attribution Non Commercial 4.0 International"
    license_long_name <- "Creative Commons Attribution Non Commercial 4.0 (CC-BY-NC-4.0) License"
    license_url <- "https://spdx.org/licenses/CC-BY-NC-4.0.html"
    license_ulink <- "http://creativecommons.org/licenses/by-nc/4.0/legalcode"
  } else if (license_identifier == "CCBY4.0") {
    license_prefix <- "This work is licensed under a "
    license_name <- "Creative Commons Attribution 4.0 International"
    license_long_name <- "Creative Commons Attribution 4.0 (CC-BY-4.0) License"
    license_url <- "https://spdx.org/licenses/CC-BY-4.0.html"
    license_ulink <- "http://creativecommons.org/licenses/by/4.0/legalcode"
  } else if (
    license_identifier == "PUBLICDOMAIN" || license_identifier == "CC01.0"
  ) {
    license_prefix <- "To the extent possible under law, the publisher has waived all rights to these data and has dedicated them to the "
    license_name <- "Public Domain"
    license_long_name <- "Public Domain (CC0 1.0)"
    license_url <- "https://spdx.org/licenses/CC0-1.0.html"
    license_ulink <- "http://creativecommons.org/publicdomain/zero/1.0/legalcode"
  } else {
    stop("Unsupported license")
  }
  dataset <- xml2::xml_find_first(eml, ".//dataset")
  intelrights <- xml2::xml_add_child(dataset, "intellectualRights")
  intelrights_para <- xml2::xml_add_child(
    intelrights,
    "para",
    license_prefix
  )
  intelrights_para_ulink <- xml2::xml_add_child(
    intelrights_para,
    "ulink",
    url = license_ulink
  )
  xml2::xml_add_child(intelrights_para_ulink, "citetitle", license_long_name)

  licensed <- xml2::xml_add_child(dataset, "licensed")
  xml2::xml_add_child(licensed, "licenseName", license_name)
  xml2::xml_add_child(licensed, "url", license_url)
  xml2::xml_add_child(licensed, "identifier", license_identifier)
  return(eml)
}


#' Add a person (creator or contact) to the EML object
#' @param eml EML object
#' @param type Type of person to add ("creator" or "contact")
#' @param given_name Given name of the person
#' @param family_name Family name of the person
#' @param email_address Email address of the person (optional)
#' @return EML object with added person
#' @keywords internal
#' @noRd
eml_add_person <- function(
  eml,
  type,
  given_name,
  family_name,
  organization = NULL,
  position = NULL,
  email_address = NULL,
  parent_node = ".//dataset"
) {
  parent_node <- xml2::xml_find_first(eml, parent_node)
  if (is.na(parent_node)) {
    stop("Parent node not found in EML object.")
  }
  ptype <- xml2::xml_add_child(parent_node, type)
  name <- xml2::xml_add_child(ptype, "individualName")
  xml2::xml_add_child(name, "givenName", given_name)
  xml2::xml_add_child(name, "surName", family_name)
  if (!is.null(email_address)) {
    xml2::xml_add_child(ptype, "electronicMailAddress", email_address)
  }
  if (!is.null(organization)) {
    xml2::xml_add_child(ptype, "organizationName", organization)
  }
  if (!is.null(position)) {
    xml2::xml_add_child(ptype, "positionName", position)
  }
  return(eml)
}

#' Add multiple project persons to EML object
#'
#' Adds multiple persons to an EML object.
#'
#' @param eml EML object
#' @param type The type of people being added, "creator", "contact", or "personnel" for projects
#' @param persons A list of personnel, each created using the `person()` function
#' @param parent_node Which EML to insert under, should always be ".//dataset", unless adding project personnel, in which case it is ".//dataset/project"
#' @return EML object with added project personnels
#' @examples
#' eml <- eml_base() |>
#' eml_set_project(
#'   title = "Test Project Title"
#' ) |>
#' eml_add_persons(
#'   type = "personnel",
#'   persons = list(
#'     person(given = "Emily", family = "Davis"),
#'     person(given = "Michael", family = "Brown", email = "mb@email.com")
#'   )
#' )
#' print(eml)
#'
#' @export
eml_add_persons <- function(
  eml,
  type,
  persons = list(person()),
  parent_node = ".//dataset"
) {
  for (person in persons) {
    if (inherits(person, "person")) {
      given_name <- person$given
      family_name <- person$family
      email_address <- person$email
      organization <- NULL
      position <- NULL
    } else {
      given_name <- person[["given"]]
      family_name <- person[["family"]]
      email_address <- person[["email"]]
      organization <- person[["organization"]]
      position <- person[["position"]]
    }
    eml <- eml_add_person(
      eml,
      type = type,
      given_name = given_name,
      family_name = family_name,
      email_address = email_address,
      organization = organization,
      position = position,
      parent_node = parent_node
    )
  }
  return(eml)
}


#' Add a contact to the EML object
#'
#' Sets a contact person for the dataset. Multiple contacts can be added.
#'
#' @param eml EML object
#' @param given_name Given name of the contact
#' @param family_name Family name of the contact
#' @param email_address Email address of the contact (optional)
#' @return EML object with added contact
#' @examples
#' eml <- eml_base() |>
#' eml_add_contact(
#'   given_name = "Jane",
#'   family_name = "Smith",
#'   email_address = "jsmith@email.com"
#' )
#'
#' @export
eml_add_contact <- function(
  eml,
  given_name,
  family_name,
  email_address = NULL
) {
  eml |>
    eml_add_person(
      type = "contact",
      given_name = given_name,
      family_name = family_name,
      email_address = email_address
    )
}

#' Add a creator to the EML object
#'
#' Sets a creator person for the dataset. Multiple creators can be added.
#'
#' @param eml EML object
#' @param given_name Given name of the creator
#' @param family_name Family name of the creator
#' @param email_address Email address of the creator (optional)
#' @return EML object with added creator
#' @examples
#' eml <- eml_base() |>
#' eml_add_creator(
#'   given_name = "Alice",
#'   family_name = "Johnson"
#' )
#'
#' @export
eml_add_creator <- function(
  eml,
  given_name,
  family_name,
  email_address = NULL
) {
  eml |>
    eml_add_person(
      type = "creator",
      given_name = given_name,
      family_name = family_name,
      email_address = email_address
    )
}

#' Calculate bounding box from a list of coordinates
#'
#' Returns the max and min lat/longs for use in setting a coverage object
#' @param lats Numeric vector of latitudes
#' @param longs Numeric vector of longitudes
#' @return A list with west, east, north, and south bounding coordinates
#' @examples
#' lats <- c(34.0, 35.0, 36.5)
#' longs <- c(-120.0, -119.5, -121.0)
#' bbox <- eml_calculate_bounding_box(lats, longs)
#' print(bbox)
#'
#' @export
eml_calculate_bounding_box <- function(lats = NULL, longs = NULL) {
  if (is.null(lats) || is.null(longs)) {
    return(NULL)
  }
  west <- min(longs, na.rm = TRUE)
  east <- max(longs, na.rm = TRUE)
  north <- max(lats, na.rm = TRUE)
  south <- min(lats, na.rm = TRUE)
  return(list(
    west = west,
    east = east,
    north = north,
    south = south
  ))
}


#' Set coverage in EML object
#'
#' Sets the coverage of the dataset, including geographic and temporal coverage.
#'
#' @param eml EML object
#' @param geographic_description Geographic description string (optional)
#' @param west West bounding coordinate (optional)
#' @param east East bounding coordinate (optional)
#' @param north North bounding coordinate (optional)
#' @param south South bounding coordinate (optional)
#' @param begin_date Begin date string (optional)
#' @param end_date End date string (optional)
#' @return EML object with updated coverage
#' @seealso [eml_calculate_bounding_box()]
#' @examples
#' eml <- eml_base() |>
#' eml_set_coverage(
#'  geographic_description = "Sample Location",
#'  west = -120.0,
#'  east = -119.0,
#'  north = 35.0,
#'  south = 34.0,
#'  begin_date = "2020-01-01",
#'  end_date = "2020-12-31"
#' )
#' print(eml)
#'
#' @export
eml_set_coverage <- function(
  eml,
  geographic_description = NULL,
  west = NULL,
  east = NULL,
  north = NULL,
  south = NULL,
  begin_date = NULL,
  end_date = NULL
) {
  dataset <- xml2::xml_find_first(eml, ".//dataset")
  coverage <- xml2::xml_add_child(dataset, "coverage")

  if (!is.null(geographic_description)) {
    geographic_coverage <- xml2::xml_add_child(coverage, "geographicCoverage")
    xml2::xml_add_child(
      geographic_coverage,
      "geographicDescription",
      geographic_description
    )
  }

  if (!is.null(west) && !is.null(east) && !is.null(north) && !is.null(south)) {
    geographic_coverage <- xml2::xml_find_first(
      coverage,
      ".//geographicCoverage"
    )
    if (is.na(xml2::xml_name(geographic_coverage))) {
      geographic_coverage <- xml2::xml_add_child(coverage, "geographicCoverage")
    }
    bounding_coordinates <- xml2::xml_add_child(
      geographic_coverage,
      "boundingCoordinates"
    )
    xml2::xml_add_child(
      bounding_coordinates,
      "westBoundingCoordinate",
      as.character(west)
    )
    xml2::xml_add_child(
      bounding_coordinates,
      "eastBoundingCoordinate",
      as.character(east)
    )
    xml2::xml_add_child(
      bounding_coordinates,
      "northBoundingCoordinate",
      as.character(north)
    )
    xml2::xml_add_child(
      bounding_coordinates,
      "southBoundingCoordinate",
      as.character(south)
    )
  }

  if (!is.null(begin_date) || !is.null(end_date)) {
    temporal_coverage <- xml2::xml_add_child(coverage, "temporalCoverage")
    range_of_dates <- xml2::xml_add_child(temporal_coverage, "rangeOfDates")
    if (!is.null(begin_date)) {
      xml2::xml_add_child(range_of_dates, "beginDate") |>
        xml2::xml_add_child("calendarDate", begin_date)
    }
    if (!is.null(end_date)) {
      xml2::xml_add_child(range_of_dates, "endDate") |>
        xml2::xml_add_child("calendarDate", end_date)
    }
  }

  return(eml)
}

#' Set maintenance in EML object
#'
#' Sets the maintenance information of the dataset.
#'
#' @param eml EML object
#' @param description Maintenance description string
#' @param frequency Maintenance update frequency string
#' @return EML object with updated maintenance
#' @examples
#' eml <- eml_base() |>
#' eml_set_maintenance(
#'  description = "This metadata is maintained regularly.",
#'  frequency = "monthly"
#' )
#' print(eml)
#'
#' @export
eml_set_maintenance <- function(
  eml,
  description = "Not specified.",
  frequency = "Unknown"
) {
  frequency_options <- c(
    "continual",
    "daily",
    "weekly",
    "monthly",
    "annually",
    "asNeeded",
    "irregular",
    "notPlanned",
    "unknown"
  )
  if (!(frequency %in% frequency_options)) {
    stop(
      "Invalid maintenance update frequency. Must be one of: ",
      paste(frequency_options, collapse = ", ")
    )
  }

  dataset <- xml2::xml_find_first(eml, ".//dataset")
  maintenance <- xml2::xml_add_child(dataset, "maintenance")
  xml2::xml_add_child(maintenance, "description") |>
    xml2::xml_add_child("para", description)
  xml2::xml_add_child(
    maintenance,
    "maintenanceUpdateFrequency",
    frequency
  )
  return(eml)
}

#' Set methods in EML object
#'
#' Sets the methods information of the dataset.
#'
#' @param eml EML object
#' @param step_description Step description string
#' @param study_extent Study extent description string
#' @param sampling_description Sampling description string
#' @return EML object with updated methods
#' @examples
#' eml <- eml_base() |>
#' eml_set_methods(
#'   step_description = "This is a test methods description.",
#'   study_extent = "Study extent description.",
#'   sampling_description = "Sampling description."
#' )
#' print(eml)
#'
#' @export
eml_set_methods <- function(
  eml,
  step_description = "No methods provided.",
  study_extent = "Not specified.",
  sampling_description = "Not specified."
) {
  dataset <- xml2::xml_find_first(eml, ".//dataset")
  methods <- xml2::xml_add_child(dataset, "methods")
  xml2::xml_add_child(methods, "methodStep") |>
    xml2::xml_add_child("description") |>
    xml2::xml_add_child("para", step_description)
  xml2::xml_add_child(methods, "studyExtent") |>
    xml2::xml_add_child("description") |>
    xml2::xml_add_child("para", study_extent)
  xml2::xml_add_child(methods, "samplingDescription") |>
    xml2::xml_add_child("para", sampling_description)
  return(eml)
}

#' Set project in EML object
#'
#' Sets the project information of the dataset.
#'
#' @param eml EML object
#' @param title Project title string
#' @return EML object with updated project
#' @examples
#' eml <- eml_base() |>
#' eml_set_project(
#'  title = "Sample Project Title"
#' )
#' print(eml)
#'
#' @export
eml_set_project <- function(
  eml,
  title
) {
  dataset <- xml2::xml_find_first(eml, ".//dataset")
  project <- xml2::xml_add_child(dataset, "project")
  xml2::xml_add_child(project, "title", title)
  return(eml)
}

#' Add project personnel to EML object
#'
#' Adds personnel to the project section of the dataset. Multiple personnel can be added.
#' The project must be set before adding personnel.
#'
#' @param eml EML object
#' @param given_name Given name of the personnel
#' @param family_name Family name of the personnel
#' @param email_address Email address of the personnel (optional)
#' @return EML object with added project personnel
#' @examples
#' eml <- eml_base() |>
#' eml_set_project(
#'  title = "Test Project Title"
#' ) |>
#' eml_add_project_personnel(
#'  given_name = "Emily",
#'  family_name = "Davis",
#' )
#' print(eml)
#'
#' @export
eml_add_project_personnel <- function(
  eml,
  given_name,
  family_name,
  email_address = NULL
) {
  eml |>
    eml_add_person(
      type = "personnel",
      given_name = given_name,
      family_name = family_name,
      email_address = email_address,
      parent_node = ".//dataset/project"
    )
  return(eml)
}
