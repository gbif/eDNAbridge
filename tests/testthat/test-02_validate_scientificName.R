test_that("Check scientific name - exists", {
  result <- val_check_scientific_name("Pseudomonas")
  expect_equal(result, TRUE)
})

test_that("Check scientific name - does not exist", {
  result <- val_check_scientific_name("NonExistentSpecies12345")
  expect_equal(result, FALSE)
})

test_that("Check name list", {
  names <- c("Pseudomonas", "NonExistentSpecies12345", "Escherichia coli")
  result <- val_check_name_list(names, quiet = TRUE)
  
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 3)
  expect_equal(result$scientificName, unique(names))
  expect_equal(result$match, c(TRUE, FALSE, TRUE))
})
