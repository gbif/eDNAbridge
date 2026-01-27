test_that("wl_read_wilderlab_data reads example data correctly", {
  example_files <- list(
    jobs=test_path("testdata/jobs.csv"),
    samples=test_path("testdata/samples.csv"),
    taxa=test_path("testdata/taxa.csv"),
    records=test_path("testdata/records.csv")
  )
  
  expected_data <- purrr::map(example_files, readr::read_csv, show_col_types = FALSE)
  
  ingested_data <- wl_read_wilderlab_data(test_path("testdata/"))
  
  expect_equal(names(ingested_data), names(expected_data))
  
  for (table_name in names(expected_data)) {
    expect_equal(ingested_data[[table_name]], expected_data[[table_name]])
  }
})

test_that("wl_export_wilderlab_data exports data correctly", {
  temp_dir <- tempdir()
  
  sample_data <- list(
    jobs=tibble::tibble(job_id=1:3, job_name=c("JobA", "JobB", "JobC")),
    samples=tibble::tibble(sample_id=1:2, sample_type=c("Type1", "Type2")),
    taxa=tibble::tibble(taxon_id=1:4, taxon_name=c("Taxa1", "Taxa2", "Taxa3", "Taxa4")),
    records=tibble::tibble(record_id=1:5, record_value=c(10, 20, 30, 40, 50))
  )
  
  wl_export_wilderlab_data(sample_data, temp_dir)
  
  for (table_name in names(sample_data)) {
    exported_file <- file.path(temp_dir, paste0(table_name, ".csv"))
    expect_true(file.exists(exported_file))
    
    exported_data <- readr::read_csv(exported_file, show_col_types = FALSE)
    expect_equal(exported_data, sample_data[[table_name]])
  }
})

test_that("wl_build_wilderlab_request constructs requests correctly", {
  Sys.unsetenv("WILDERLAB_KEY")
  Sys.unsetenv("WILDERLAB_SECRET")
  Sys.unsetenv("WILDERLAB_XAPIKEY")

  config_set_wilderlab_creds(
    "test_key",
    "test_secret",
    "test_xapikey"
  )
  req <- wl_build_wilderlab_request("jobs")
  
  expect_s3_class(req, "httr2_request")
  expect_equal(req$url, "https://connect.wilderlab.co.nz/edna/?query=jobs")
  expect_equal(req$headers$`X-API-KEY`, "test_xapikey")
  aws_params <- req$policies$auth_sign$params
  expect_equal(aws_params$aws_access_key_id, "test_key")
  expect_equal(aws_params$aws_secret_access_key, "test_secret")
  expect_equal(aws_params$aws_service, "execute-api")
  expect_equal(aws_params$aws_region, "ap-southeast-2")

  Sys.unsetenv("WILDERLAB_KEY")
  Sys.unsetenv("WILDERLAB_SECRET")
  Sys.unsetenv("WILDERLAB_XAPIKEY")
})

test_that("wl_format_wilderlab_response processes responses correctly", {
  sample_data = readr::read_csv(test_path("testdata/samples.csv"), show_col_types = FALSE)

  sample_response <- httr2::response_json(
    status_code = 200,
    headers = list(`Content-Type` = "application/json"),
    body = list(message=readr::format_csv(sample_data))
  )
  
  formatted_data <- wl_format_wilderlab_response(sample_response)
  expect_equal(formatted_data, sample_data)
})