# Integration tests for the package
# We don't test connection to laboratory APIs to reduce burden on their systems

test_eml_file <- function() {
  eml <- eml_base() |>
    eml_set_title("Test Title") |>
    eml_add_creator(
      given_name = "Jane",
      family_name = "Smith",
      email_address = "jasmith@email.com"
    ) |>
    eml_add_persons(
      type = "contact",
      persons = list(person("Emmy", "Davis"), person("John", "Smith"))
    ) |> 
    eml_set_abstract("This is a test abstract.") |>
    eml_set_license("CC-BY-NC-4.0") |>
    eml_set_coverage(
      geographic_description = "Test Location",
      west = -120.0,
      east = -119.0,
      north = 35.0,
      south = 34.0,
      begin_date = "2020-01-01",
      end_date = "2020-12-31"
    ) |>
    eml_set_maintenance(
      description = "This metadata is maintained regularly.",
      frequency = "monthly"
    ) |>
    eml_add_contact(
      given_name = "John",
      family_name = "Doe",
      email_address = "johndoe@email.com"
    ) |>
    eml_add_persons(
      type = "contact",
      persons = list(person("David", "Smith"), person("John", "Smith"))
    ) |>
    eml_set_methods(
      step_description = "This is a test methods description.",
      study_extent = "Study extent description.",
      sampling_description = "Sampling description."
    ) |>
    eml_set_project(
      title = "Test Project Title"
    ) |>
    eml_add_project_personnel(
      given_name = "Emily",
      family_name = "Davis",
    ) |>
    eml_add_persons(
      type = "personnel",
      persons = list(person("Jane", "Doe"))
    )
}

test_that("Integration from data ingestion to archive generation",
{
  example_files <- list(
    jobs=test_path("testdata/jobs.csv"),
    samples=test_path("testdata/samples.csv"),
    taxa=test_path("testdata/taxa.csv"),
    records=test_path("testdata/records.csv")
  )
  
  ingested_data <- wl_read_wilderlab_data(test_path("testdata/")) |>
    wl_map_wilderlab_data() |>
    wl_inject_dwc_wilderlab_constants()

  expect_true(is.data.frame(ingested_data))

  issues <- val_validate_all_fields(ingested_data)
  if (!is.null(issues)) {
    expect_equal(issues |> dplyr::filter(error_level == "error") |> nrow(), 0)
  } else {
    expect_null(issues)
  }

  dwc_tibbles <- gen_tibble_to_dwc_frames(ingested_data)
  expect_true(is.list(dwc_tibbles))

  eml_xml <- test_eml_file()
  meta_xml <- meta_xml_base() |>
    meta_xml_add_file(
      type="core",
      df=dwc_tibbles$occurrence_core,
      location="occurrence_core.txt",
    )
  
  gen_make_dwc_archive(
    dwc_tibbles = dwc_tibbles,
    eml_xml = eml_xml,
    meta_xml = meta_xml,
    path = test_path("testdata/dwc_archive.zip")
  )
  expect_true(file.exists(test_path("testdata/dwc_archive.zip")))
})