test_that("Make R6 class for occurenceID", {
  test_field <- fld_field_occurrenceID$new()
  expect_equal(test_field$name, "occurrenceID")
  expect_equal(test_field$dwc_link, "https://dwc.tdwg.org/list/#dwc_occurrenceID")
  expect_true(is.function(test_field$type_check_func))
})

test_that("Validate occurrenceID - valid values", {
  test_field <- fld_field_occurrenceID$new()
  valid_values <- c("ID123", "abc-456", "789_xyz")
  issues <- test_field$validate(valid_values)
  expect_null(issues)
})

test_that("Validate occurrenceID - missing values", {
  test_field <- fld_field_occurrenceID$new()
  values_with_missing <- c("ID123", NA, "", "abc-456")
  issues <- val_combine_issues(test_field$validate(values_with_missing))
  expect_true(is.data.frame(issues))
  expect_equal(nrow(issues), 2) # Two missing values
  expect_true(all(issues$issue == "missing value"))
  expect_true(all(issues$term == "occurrenceID"))
  expect_equal(issues$index, c(2, 3))
})

test_that("Validate occurrenceID - invalid types", {
  test_field <- fld_field_occurrenceID$new()
  values_with_invalid <- list("ID123", list(456), 123, TRUE)
  issues <- val_combine_issues(test_field$validate(values_with_invalid))
  expect_true(is.data.frame(issues))
  expect_equal(nrow(issues), 2) # Two invalid type values
  expect_true(all(issues$issue == "invalid type"))
  expect_true(all(issues$term == "occurrenceID"))
  expect_equal(issues$index, c(2, 4))
})

test_that("Construct occurrenceID field dynamically using field factory", {
  field_name <- "occurrenceID"
  test_field <- fld_factory(field_name)
  expect_equal(test_field$name, field_name)
  expect_equal(test_field$dwc_link, "https://dwc.tdwg.org/list/#dwc_occurrenceID")
  expect_true(is.function(test_field$type_check_func))
})