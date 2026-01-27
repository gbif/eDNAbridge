test_that("Test setting details in a temporary .Renviron file", {
  temp_renviron <- tempfile()
  config_set_creds("TEST_KEY", "initial_value", cache = TRUE, renv_path = temp_renviron)
  expect_true(Sys.getenv("TEST_KEY") == "initial_value")
  Sys.unsetenv("TEST_KEY")
  unlink(temp_renviron)
})

test_that("Test updating existing key in .Renviron file", {
  temp_renviron <- tempfile()
  config_set_creds("TEST_KEY", "initial_value", cache = TRUE, renv_path = temp_renviron)
  config_set_creds("TEST_KEY", "updated_value", cache = TRUE, renv_path = temp_renviron)
  lines <- readLines(temp_renviron)
  expect_true(any(grepl("^TEST_KEY=updated_value$", lines)))
  expect_true(Sys.getenv("TEST_KEY") == "updated_value")
  Sys.unsetenv("TEST_KEY")
  unlink(temp_renviron)
})

test_that("Test checking credentials with missing keys", {
  temp_renviron <- tempfile()
  # Ensure the environment is clean
  Sys.unsetenv("WILDERLAB_KEY")
  Sys.unsetenv("WILDERLAB_SECRET")
  Sys.unsetenv("WILDERLAB_XAPIKEY")
  # Check creds should identify all as missing
  missing <- config_check_creds(prompt_user = FALSE)
  expect_equal(length(missing), 3)
  expect_true(all(c("WILDERLAB_KEY", "WILDERLAB_SECRET", "WILDERLAB_XAPIKEY") %in% missing))
  # Now set one and check again
  config_set_creds("WILDERLAB_KEY", "some_key", cache = FALSE)
  missing <- config_check_creds(prompt_user = FALSE)
  expect_equal(length(missing), 2)
  expect_true(all(c("WILDERLAB_SECRET", "WILDERLAB_XAPIKEY") %in% missing))
  # Clean up
  Sys.unsetenv("WILDERLAB_KEY")
  Sys.unsetenv("WILDERLAB_SECRET")
  Sys.unsetenv("WILDERLAB_XAPIKEY")
  unlink(temp_renviron)
})

test_that("config_check_creds prompts for missing keys and sets them", {
  withr::local_envvar(c(WILDERLAB_KEY = "", WILDERLAB_SECRET = "", WILDERLAB_XAPIKEY = ""))

  inputs <- c("key123", "secret456", "xapikey789", "y")  # responses for each prompt
  input_index <- 0
  mock_readline <- function(prompt = "") {
    input_index <<- input_index + 1
    inputs[[input_index]]
  }
  sink(tempfile())  # suppress output
  missing <- withr::with_options(
    list(
      cli.default_handler = function(...) NULL,  # suppress cli output during tests
      usethis.quiet = TRUE
    ),
    config_check_creds(prompt_user = TRUE, read_fn = mock_readline)
  )
  sink()
  expect_equal(missing, c("WILDERLAB_KEY", "WILDERLAB_SECRET", "WILDERLAB_XAPIKEY"))
  expect_equal(Sys.getenv("WILDERLAB_KEY"), "key123")
  expect_equal(Sys.getenv("WILDERLAB_SECRET"), "secret456")
  expect_equal(Sys.getenv("WILDERLAB_XAPIKEY"), "xapikey789")
})