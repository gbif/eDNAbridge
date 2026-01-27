##############################################################################
## ------------------------------ eDNABridge ------------------------------ ##
## This package was designed and developed by Epi (https://www.epi.group/)  ##
## with funding provided by the New Zealand Ministry for the Environment.   ##
##                                                                          ##
## Support was provided by Wilderlab, the Hawke’s Bay Regional Council,     ##
## Department of Conservation and Earth Sciences New Zealand (NIWA)         ##
## ------------------------------------------------------------------------ ##
##############################################################################


#' Build Wilderlab API request
#' @param table The name of the table to query (jobs, samples, taxa, records)
#' @param jobID Optional JobID to filter results (only for records tables)
#' @return An httr2 request object
#' @keywords internal
#' @noRd
wl_build_wilderlab_request <- function(table, jobID = NULL) {
  config_check_creds(
    c("WILDERLAB_KEY", "WILDERLAB_SECRET", "WILDERLAB_XAPIKEY"),
    prompt_user = TRUE
  )

  req <- httr2::request("https://connect.wilderlab.co.nz/edna/") |>
    httr2::req_url_query(query = table) |>
    httr2::req_auth_aws_v4(
      aws_access_key_id = Sys.getenv("WILDERLAB_KEY"),
      aws_secret_access_key = Sys.getenv("WILDERLAB_SECRET"),
      aws_service = "execute-api",
      aws_region = "ap-southeast-2"
    ) |>
    httr2::req_headers(
      `X-API-Key` = Sys.getenv("WILDERLAB_XAPIKEY")
    ) |>
    httr2::req_user_agent("eDNABridge R package") |>
    httr2::req_throttle(capacity = 30, fill_time_s = 60) # 30 request per min max

  if (!is.null(jobID)) {
    req <- req |>
      httr2::req_url_query(JobID = jobID)
  }

  return(req)
}


#' Format Wilderlab API response
#' @param response The response object from wl_get_wilderlab_data
#' @return A data frame containing the formatted data
#' @keywords internal
#' @noRd
wl_format_wilderlab_response <- function(response) {
  return(
    response |> 
      httr2::resp_body_json(simplifyVector = TRUE) |>
      magrittr::extract2("message") |>
      readr::read_csv(show_col_types = FALSE)
  )
}

#' Get data from Wilderlab API
#' 
#' This function retrieves data from the Wilderlab API for a specified table and optional JobID.
#' It displays terminal output and formats the response.
#' Underlying functions will request Wilderlab API keys if necessary. These are not stored
#' Unless you choose to cache them in a .Renviron, which will be made in the working directory
#' 
#' @param table The name of the table to query (jobs, samples, taxa, records)
#' @param jobID Optional JobID to filter results (only for records tables)
#' @param request_only Logical; if TRUE, only the request object is returned without performing the request
#' @return A data frame containing the requested data
#' @examples
#' # Get jobs data
#' \dontrun{
#' jobs_data <- wl_get_wilderlab_data("jobs")
#' print(head(jobs_data))
#' }
#' 
#' @export
wl_get_wilderlab_data <- function(table, jobID = NULL) {
  if (table == "records" && is.null(jobID)) {
    stop("jobID must be provided when querying the records table.")
  }
  
  if(table == "records") {
    waiter <- cli::cli_status("Downloading {.val {table}} data for job {.val {jobID}} from Wilderlab ...")
  } else {
    waiter <- cli::cli_status("Downloading {.val {table}} data from Wilderlab ...")
  }
  
  res <- wl_build_wilderlab_request(table = table, jobID = jobID)  |> 
    httr2::req_perform() |>
    wl_format_wilderlab_response()
    
  cli::cli_status_clear(waiter)
  if (table == "records") {
    cli::cli_alert_success("{.val {table}} data for job {.val {jobID}} downloaded from Wilderlab")
  } else {
    cli::cli_alert_success("{.val {table}} data downloaded from Wilderlab")
  }

  return(res)
}

#' Export Wilderlab data to local folder
#' 
#' Creates jobs.csv, samples.csv, taxa.csv, and records.csv files in the specified folder.
#' Each file is formatted as it exists in the Wilderlab API.
#' 
#' @param wilderlab_data A list containing data frames for jobs, samples, taxa, and records
#' @param path The path to the folder where the CSV files will be saved
#' @return None. The function writes CSV files to the specified folder.
#' @seealso [wl_get_wilderlab_data()], [wl_get_all_wilderlab_data()]
#' @examples
#' \dontrun{
#' # Assuming wilderlab_data is a list with jobs, samples, taxa, and records data frames
#' # Which can be obtained using wl_get_all_wilderlab_data()
#' wilderlab_data <- wl_get_all_wilderlab_data()
#' wl_export_wilderlab_data(wilderlab_data, path = "path/to/save/folder")
#' }
#' 
#' @export
wl_export_wilderlab_data <- function(wilderlab_data, path) {
  readr::write_csv(wilderlab_data$jobs, file.path(path, "jobs.csv"))
  readr::write_csv(wilderlab_data$samples, file.path(path, "samples.csv"))
  readr::write_csv(wilderlab_data$taxa, file.path(path, "taxa.csv"))
  readr::write_csv(wilderlab_data$records, file.path(path, "records.csv"))
}

#' Read Wilderlab data from local folder
#' 
#' This function reads Wilderlab data from CSV files in a specified folder.
#' It expects the folder to contain jobs.csv, samples.csv, taxa.csv, and records.csv
#' 
#' @param folder_path The path to the folder containing the CSV files
#' @return A list containing data frames for jobs, samples, taxa, and records
#' @seealso [wl_export_wilderlab_data()]
#' @examples
#' \dontrun{
#' # Read Wilderlab data from a local folder
#' wilderlab_data <- wl_read_wilderlab_data("path/to/folder")
#' print(head(wilderlab_data$jobs))
#' }
#' 
#' @export
wl_read_wilderlab_data <- function(folder_path) {
  jobs <- readr::read_csv(file.path(folder_path, "jobs.csv"), show_col_types = FALSE)
  samples <- readr::read_csv(file.path(folder_path, "samples.csv"), show_col_types = FALSE)
  taxa <- readr::read_csv(file.path(folder_path, "taxa.csv"), show_col_types = FALSE)
  records <- readr::read_csv(file.path(folder_path, "records.csv"), show_col_types = FALSE)

  return(list(
    jobs = jobs,
    samples = samples,
    taxa = taxa,
    records = records
  ))
}

#' Get records data for multiple JobIDs from Wilderlab API
#' 
#' This function retrieves records data for a list of JobIDs from the Wilderlab API.
#' It performs parallel requests for efficiency and combines the results into a single data frame.
#' 
#' @param jobIDs A vector of JobIDs for which to retrieve records data
#' @return A data frame containing the combined records data for all specified JobIDs
#' @examples
#' \dontrun{
#' # Get records data for multiple JobIDs
#' samples <- wl_get_wilderlab_data("samples")
#' jobIDs <- samples |> 
#'   filter(MakeDataPublic == 1) |> 
#'   pull(JobID) |> 
#'   unique()
#' records_data <- wl_get_records_from_joblist(jobIDs)
#' print(head(records_data))
#' }
#' 
#' @export
wl_get_records_from_joblist <- function(jobIDs) {
  record_waiter <- cli::cli_status('Downloading {.val records} data for all jobs from Wilderlab ...')
  record_responses <- httr2::req_perform_parallel(
    lapply((jobIDs), function(id) {
      wl_build_wilderlab_request("records", jobID = id)
    }),
    progress = "Fetching records from Wilderlab"
  )
  cli::cli_status_clear(record_waiter)
  cli::cli_alert_success('{.val records} data for all jobs downloaded from Wilderlab')

  records_list <- lapply(record_responses, function(resp) {
    wl_format_wilderlab_response(resp)
  })

  do.call(rbind, records_list)
}

#' Get all data from Wilderlab API
#' 
#' This function retrieves all data from the Wilderlab API, including jobs, samples, taxa, and all records in the job.
#' It handles authentication and returns the data as a list of data frames.
#' *Use with caution!* Depending on the number of jobs and records, this may take a long time.
#' 
#' @param public_only Logical; if TRUE, only public data is retrieved from Wilderlab
#' @return A list containing data frames for jobs, samples, taxa, and records
#' @examples
#' \dontrun{
#' # Get all data from Wilderlab
#' all_data <- wl_get_all_wilderlab_data()
#' print(head(all_data$records))
#' }
#' @export
wl_get_all_wilderlab_data <- function(public_only = TRUE) {
  cli::cli_h1("Downloading data from Wilderlab")
  jobs <- wl_get_wilderlab_data("jobs")
  samples <- wl_get_wilderlab_data("samples")


  if(public_only) {
    jobIDs <- samples |> 
      dplyr::filter(MakeDataPublic == 1) |> 
      dplyr::pull(JobID) |> 
      unique()
    if(length(jobIDs) == 0) {
      cli::cli_alert_warning("No jobs found in Wilderlab data, are you sure you have public data?")
      stop("No public jobs found")
    }
    jobs <- jobs |>
      dplyr::filter(JobID %in% jobIDs)
    samples <- samples |>
      dplyr::filter(JobID %in% jobIDs)
    taxa <- wl_get_wilderlab_data("taxa")
    records <- wl_get_records_from_joblist(jobIDs) |>
      dplyr::filter(UID %in% samples$UID)
    cli::cli_alert_info("Filtered to {.val {nrow(jobs)}} public jobs only")
  } else {
    taxa <- wl_get_wilderlab_data("taxa")
    cli::cli_alert_info("Downloading records for all {.val {nrow(jobs)}} jobs (public and private)")
    records <- wl_get_records_from_joblist(jobs$JobID)
  }

  cli::cli_alert_success("Data downloaded from Wilderlab")

  return(list(
    jobs = jobs,
    samples = samples,
    taxa = taxa,
    records = records
  ))
}

#' Map Wilderlab data to standard column names
#' 
#' This function maps the columns of Wilderlab data frames to standard column names used in the darwin core
#' Also joins the data frames together into a single data frame using the records as the main table
#' Optionally, mappings can be left blank just to get the joined dataframe, purely for testing purposes
#' 
#' @param wilderlab_data A list containing data frames for jobs, samples, taxa, and records
#' @param mappings A named character vector where names are standard column names and values are Wilderlab column names
#' @return A data frame with standardised column names
#' @examples
#' \dontrun{
#' # Assuming wilderlab_data is a list with jobs, samples, taxa, and records data frames
#' # Which can be obtained using wl_get_all_wilderlab_data()
#' wilderlab_data <- wl_get_all_wilderlab_data()
#' mapped_data <- wl_map_wilderlab_data(wilderlab_data)
#' print(head(mapped_data))
#' }
#' @export
wl_map_wilderlab_data <- function(wilderlab_data, mappings = wl_wilderlab_dwc_mappings()) {
  lineage <- insect::get_lineage( # Travels the parent-child graph to get full taxonomy
    wilderlab_data$records$TaxID,
    wilderlab_data$taxa |> 
      dplyr::select(1:4) |> 
      dplyr::rename(
        c(
          "taxID"="TaxID", 
          "parent_taxID"="ParentTaxID", 
          "rank"="Rank", 
          "name"="Name")
      ), 
    simplify=FALSE,
    )

  joined_df <- wilderlab_data$records |>
      dplyr::left_join(
        wilderlab_data$samples,
        by = c("UID")
      ) |>
      dplyr::left_join(
        wilderlab_data$jobs,
        by = c("JobID")
      ) |>
      dplyr::mutate(
        kingdom = sapply(lineage, function(x) {x['kingdom']}),
        phylum = sapply(lineage, function(x) {x['phylum']}),
        class = sapply(lineage, function(x) {x['class']}),
        order = sapply(lineage, function(x) {x['order']}),
        family = sapply(lineage, function(x) {x['family']}),
        genus = sapply(lineage, function(x) {x['genus']}),
        species = sapply(lineage, function(x) {x['species']})
      ) 
  if(length(mappings) > 0) {
    colnames <- names(mappings)
    joined_df <- joined_df |>
      dplyr::rename(!!!mappings) |>
      dplyr::select(dplyr::any_of(colnames))
  }
  return(joined_df)
}

#' Inject Wilderlab constant values
#' 
#' This function adds constant values required for Darwin Core compliance to the joined Wilderlab data frame.
#' It sets the basisOfRecord, organismQuantityType, sampleSizeValue, and sampleSizeUnit fields.
#' 
#' @section Constants injected: 
#' - Basis of Record is set to "MaterialSample"
#' - Organism Quantity Type is set to "DNA sequence reads"
#' - Sample Size Value is set to the total sum of organismQuantity across all records
#' - Sample Size Unit is set to "DNA sequence reads"
#' 
#' @param joined_wilderlab_df A data frame resulting from wl_map_wilderlab_data
#' @return A data frame with additional constant fields for Darwin Core compliance
#' @examples
#' \dontrun{
#' # Assuming joined_wilderlab_df is a data frame obtained from wl_map_wilderlab_data()
#' wilderlab_df_with_constants <- wl_inject_dwc_wilderlab_constants(joined_wilderlab_df)
#' print(head(wilderlab_df_with_constants))
#' }
#' @export
wl_inject_dwc_wilderlab_constants <- function(joined_wilderlab_df) {
  return(
    joined_wilderlab_df |>
      dplyr::mutate(
        basisOfRecord = "MaterialSample",
        organismQuantityType = "DNA sequence reads",
        sampleSizeValue = sum(organismQuantity),
        sampleSizeUnit = "DNA sequence reads"
      )
  )
}