test_that("Generate a valid meta.xml file from test data", {
  occurrence <- readr::read_csv(
    test_path("testdata/dwc/occurence_core.csv"),
    show_col_types = FALSE
  )

  meta_xml <- meta_xml_base() |>
    meta_xml_add_file(
      type="core",
      df=occurrence,
      location="occurrence_core.txt"
    )
  
  expected_xml <- xml2::read_xml(
    test_path("testdata/dwc/expected_meta.xml")
  )

  expect_equal(
    xml2::as_list(meta_xml),
    xml2::as_list(expected_xml)
  )
})