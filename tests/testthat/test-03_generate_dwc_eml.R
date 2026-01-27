test_that("Can set all EML parameters", {
  eml <- eml_base() |>
    eml_set_title("Test Title") |>
    eml_add_creator(
      given_name = "Jane",
      family_name = "Smith",
      email_address = "jasmith@email.com"
    ) |>
    eml_add_creator(
      given_name = "Alice",
      family_name = "Johnson"
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
    eml_add_contact(
      given_name = "John",
      family_name = "Doe",
      email_address = "johndoe@email.com"
    )

  temp_file <- tempfile(fileext = ".xml")
  xml2::write_xml(eml, file = temp_file)
  expect_true(EML::eml_validate(temp_file))
})

test_that("Can get bounding box from a list of lats and longs", {
  lats <- c(34.5, 35.0, 34.8, 34.9)
  longs <- c(-120.0, -119.5, -119.8, -119.9)
  bbox <- eml_calculate_bounding_box(lats, longs)
  expect_equal(bbox$west, -120.0)
  expect_equal(bbox$east, -119.5)
  expect_equal(bbox$north, 35.0)
  expect_equal(bbox$south, 34.5)
})