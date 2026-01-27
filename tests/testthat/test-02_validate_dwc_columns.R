test_that("validate dwc to level - success", {
  df <- data.frame(
    occurrenceID = "1",
    basisOfRecord = "HumanObservation",
    eventDate = "2023-01-01",
    scientificName = "Pseudomonas aeruginosa"
  )
  val_validate_dwc_to_level(df, "required") |> 
    expect_equal(NULL)
})

test_that("validate dwc to level - missing required", {
  df <- data.frame(
    occurrenceID = "1",
    basisOfRecord = "HumanObservation",
    eventDate = "2023-01-01"
  )
  result <- val_validate_dwc_to_level(df, "required")
  expect_false(is.null(result))
  expect_true(is.data.frame(result[[1]]))
  expect_equal(nrow(result[[1]]), 1)
  expect_equal(result[[1]]$term, "scientificName")
})

test_that("validate no additional terms - success", {
  df <- data.frame(
    occurrenceID = "1",
    basisOfRecord = "HumanObservation",
    eventDate = "2023-01-01",
    scientificName = "Pseudomonas aeruginosa"
  )
  val_validate_no_additional_terms(df) |> 
    expect_equal(NULL)
})

test_that("validate no additional terms - extra columns", {
  df <- data.frame(
    occurrenceID = "1",
    basisOfRecord = "HumanObservation",
    eventDate = "2023-01-01",
    scientificName = "Pseudomonas aeruginosa",
    extraColumn = "not a dwc term"
  )
  result <- val_validate_no_additional_terms(df)
  expect_false(is.null(result))
  expect_true(is.data.frame(result[[1]]))
  expect_equal(nrow(result[[1]]), 1)
  expect_equal(result[[1]]$term, "extraColumn")
})