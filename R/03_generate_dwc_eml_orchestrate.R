##############################################################################
## ------------------------------ eDNABridge ------------------------------ ##
## This package was designed and developed by Epi (https://www.epi.group/)  ##
## with funding provided by the New Zealand Ministry for the Environment.   ##
##                                                                          ##
## Support was provided by Wilderlab, the Hawke’s Bay Regional Council,     ##
## Department of Conservation and Earth Sciences New Zealand (NIWA)         ##
## ------------------------------------------------------------------------ ##
##############################################################################


#' Create a minimally valid EML file for GBIF
#'
#' This function generates a basic EML (Ecological Metadata Language) file
#' required for publishing datasets to GBIF (Global Biodiversity Information Facility).
#'
#' @param title The title of the dataset.
#' @param creators A list of person objects representing the dataset creators.
#' @param abstract A brief abstract describing the dataset.
#' @param license The license under which the dataset is released (in SPDX format).
#' @param coverage_geo_description A description of the geographic coverage of the dataset.
#' @param coverage_bbox A list or data frame containing the bounding box coordinates with
#' columns: west, east, north, south.
#' @param coverage_start_date The start date of data collection (YYYY-MM-DD).
#' @param coverage_end_date The end date of data collection (YYYY-MM-DD).
#' @param maintenance_frequency The frequency of metadata maintenance (e.g., "monthly").
#' @param maintenance_description A description of how the metadata is maintained.
#' @param contacts A list of person objects representing the dataset contacts.
#' @param step_description A description of the methods used in data collection.
#' @param study_extent A description of the study extent.
#' @param sampling_description A description of the sampling methods.
#' @param project_title The title of the project associated with the dataset.
#' @param project_personnel A list of person objects representing the project personnel.
#' @return A xml2 object representing the EML metadata.
#' @examples
#' creators <- list(
#'   person("Jane", "Smith", email = "jsmith@email.com")
#' )
#' contacts <- list(
#'   person("John", "Doe", email = "jd@email.com")
#' )
#' project_personnel <- list(
#'   person("Emily", "Davis"),
#'   person("Michael", "Brown")
#' )
#' bbox <- list(west = -120.0, east = -119.0, north = 35.0, south = 34.0)
#' eml <- eml_gbif_create(
#'   title = "Sample Dataset",
#'   creators = creators,
#'   abstract = "This dataset contains sample data for demonstration purposes.",
#'   license = "CC-BY-4.0",
#'   coverage_geo_description = "Sample Location",
#'   coverage_bbox = bbox,
#'   coverage_start_date = "2020-01-01",
#'   coverage_end_date = "2020-12-31",
#'   maintenance_frequency = "monthly",
#'   maintenance_description = "Metadata is updated monthly.",
#'   contacts = contacts,
#'   step_description = "Data collection methods description.",
#'   study_extent = "Study extent description.",
#'   sampling_description = "Sampling methods description.",
#'   project_title = "Sample Project",
#'   project_personnel = project_personnel
#' )
#'
#' @export
eml_gbif_create <- function(
  title,
  creators,
  abstract,
  license,
  coverage_geo_description,
  coverage_bbox,
  coverage_start_date,
  coverage_end_date,
  maintenance_frequency,
  maintenance_description,
  contacts,
  step_description,
  study_extent,
  sampling_description,
  project_title,
  project_personnel
) {
  eml_base() |>
    eml_set_title(title) |>
    eml_add_persons(type = "creator", persons = creators) |>
    eml_set_abstract(abstract) |>
    eml_set_license(license) |>
    eml_set_coverage(
      geographic_description = coverage_geo_description,
      west = coverage_bbox$west,
      east = coverage_bbox$east,
      north = coverage_bbox$north,
      south = coverage_bbox$south,
      begin_date = coverage_start_date,
      end_date = coverage_end_date
    ) |>
    eml_set_maintenance(
      description = maintenance_description,
      frequency = maintenance_frequency
    ) |>
    eml_add_persons(type = "contact", persons = contacts) |>
    eml_set_methods(
      step_description = step_description,
      study_extent = study_extent,
      sampling_description = sampling_description
    ) |>
    eml_set_project(
      title = project_title
    ) |>
    eml_add_persons(
      type = "personnel",
      persons = project_personnel,
      parent_node = ".//dataset/project"
    )
}

#' Create a CSV template for GBIF EML metadata
#' 
#' This function generates a CSV file template that users can fill out
#' with their dataset metadata. The filled CSV can then be read and converted
#' into a valid EML file for GBIF using the `eml_gbif_template_read` function.
#' 
#' @param path The file path where the CSV template will be saved.
#' @return None. The function writes a CSV file to the specified path.
#' @seealso [eml_gbif_template_read()]
#' @examples
#' \dontrun{
#' eml_gbif_template_create("gbif_eml_template.csv")
#' }
#' 
#' 
#' @export
eml_gbif_template_create <- function(
  path
) {
  tibble::tibble(
    title = "Enter the dataset title",
    creators = "Enter creators separated by semicolons in git format with optional org/position, e.g. John Smith <js@email.com> (Org, Position); Jane Doe; ...",
    abstract = "Enter the dataset abstract",
    license = "Write the license in SPDX format, e.g. CC-BY-4.0",
    coverage_geo_description = "Enter a description of the geographic coverage",
    coverage_start_date = "Enter the start date of data collection (YYYY-MM-DD)",
    coverage_end_date = "Enter the end date of data collection (YYYY-MM-DD)",
    maintenance_frequency = "Enter the maintenance frequency, e.g. 'monthly'",
    maintenance_description = "Enter a description of how the metadata is maintained",
    contacts = "Enter contacts separated by semicolons in git format with optional org/position",
    step_description = "Enter a description of the methods used in data collection",
    study_extent = "Enter a description of the study extent",
    sampling_description = "Enter a description of the sampling methods",
    project_title = "Enter the project title",
    project_personnel = "Enter project personnel separated by semicolons in git format with optional org/position"
  ) |>
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "field",
      values_to = "example_value"
    ) |>
    dplyr::mutate(
      value = ""
    ) |>
    readr::write_csv(path)
}

#' Create EML metadata from a filled GBIF template CSV
#' 
#' This function reads a CSV file filled out using the GBIF EML template
#' and generates a valid EML metadata object.
#' 
#' @param path The file path to the filled CSV template.
#' @param bbox A list or data frame containing the bounding box coordinates with
#' columns: west, east, north, south.
#' @return A xml2 object representing the EML metadata.
#' @seealso [eml_gbif_template_create()], [eml_calculate_bounding_box()]
#' @examples
#' # bbox is usually derived from the dataset, instead of a metadata file
#' \dontrun{
#' bbox <- list(west = -120.0, east = -119.0, north = 35.0, south = 34.0)
#' eml <- eml_gbif_template_read("filled_gbif_eml_template.csv", bbox)
#' }
#' 
#' @export
eml_gbif_template_read <- function(
  path,
  bbox
) {
  input <- readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(
      parsed_value = purrr::map2(
        field,
        value,
        ~ if (.x %in% c("creators", "contacts", "project_personnel")) {
          lapply(strsplit(.y, ";")[[1]], as.person)
        } else {
          .y
        }
      )
    ) |>
    dplyr::select(field, parsed_value) |>
    tidyr::pivot_wider(
      names_from = field,
      values_from = parsed_value
    )
  eml_gbif_create(
    title = input$title[[1]],
    creators = input$creators[[1]],
    abstract = input$abstract[[1]],
    license = input$license[[1]],
    coverage_geo_description = input$coverage_geo_description[[1]],
    coverage_bbox = bbox,
    coverage_start_date = input$coverage_start_date[[1]],
    coverage_end_date = input$coverage_end_date[[1]],
    maintenance_frequency = input$maintenance_frequency[[1]],
    maintenance_description = input$maintenance_description[[1]],
    contacts = input$contacts[[1]],
    step_description = input$step_description[[1]],
    study_extent = input$study_extent[[1]],
    sampling_description = input$sampling_description[[1]],
    project_title = input$project_title[[1]],
    project_personnel = input$project_personnel[[1]]
  )
}


#' Parse a person string in git format into a structured list
#' 
#' This function takes a string representing a person in git format with an optional org/title,
#' Eg: `John Smith <jsmith@email.com> (Organization, Senior Developer)` and parses it
#' into a structured list containing given name, family name, email, and title.
#' 
#' @param input A string representing a person in git format.
#' @return A list with elements: given_name, family_name, email, title.
#' @examples
#' input <- "John Smith <jsmith@email.com> (Senior Developer)"
#' person <- eml_parse_person(input)
#' print(person)
#' 
#' @export
eml_parse_person <- function(input) {
  parts <- input |> 
    stringr::str_split("\\(", simplify = TRUE) |>
    as.vector()
  
  person <- parts[1] |> 
    stringr::str_trim() |>
    as.person()
  
  if (length(parts) == 2) {
    job_string <- parts[2] |> 
      stringr::str_remove("\\)") |>
      stringr::str_trim() |>
      stringr::str_split(",", simplify = TRUE) |>
      as.vector()
  } else {
    job_string <- NULL
  }

  if (length(job_string) == 2) {
    organization <- stringr::str_trim(job_string[1])
    position <- stringr::str_trim(job_string[2])
  } else if (length(job_string) == 1 && !is.na(job_string)) {
    organization <- NULL
    position <- stringr::str_trim(job_string[1])
  } else {
    organization <- NULL
    position <- NULL
  }

  list(
    given = person$given,
    family = person$family,
    email = person$email,
    organization = organization,
    position = position
  )
}
