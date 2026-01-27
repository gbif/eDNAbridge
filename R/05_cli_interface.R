##############################################################################
## ------------------------------ eDNABridge ------------------------------ ##
## This package was designed and developed by Epi (https://www.epi.group/)  ##
## with funding provided by the New Zealand Ministry for the Environment.   ##
##                                                                          ##
## Support was provided by Wilderlab, the Hawke’s Bay Regional Council,     ##
## Department of Conservation and Earth Sciences New Zealand (NIWA)         ##
## ------------------------------------------------------------------------ ##
##############################################################################


#' R6 class to handle CLI workflow
#'
#' This class handles the storage of user selections from the CLI
#' and uses this to call various different functions in the package
#' as part of a workflow.
#'
#' @keywords internal
#' @noRd
cli_workflow_helper <- R6::R6Class(
  "cli_workflow_helper",
  public = list(
    current_page = NULL,
    status = list(
      ingestion = FALSE,
      validation = FALSE,
      generation = FALSE,
      upload = FALSE
    ),
    ingestion_func = NULL,
    validation_func = NULL,
    generation_func = NULL,
    upload_func = NULL,
    allowed_errors = c("info", "warning", "error"),
    source_data = NULL,
    issues = NULL,
    archive = NULL,
    initialize = function() {
      self$current_page <- "start"
    },
    execute_workflow = function() {
      cli::cli_h1("Starting data publication workflow")
      if (!is.null(self$ingestion_func)) {
        cli::cli_h2("Data Ingestion Beginning")
        source_data <- self$ingestion_func()
        cli::cli_alert_success("Data Ingestion Completed")
      }

      if (!is.null(self$validation_func)) {
        cli::cli_h2("Data Validation Beginning")
        issues <- self$validation_func(source_data)
        if (!is.null(issues)) {
          issues <- issues |>
            dplyr::filter(!error_level %in% self$allowed_errors)
          if (nrow(issues) > 0) {
            cli::cli_alert_danger("Validation failed with the following issues:")
            print(issues)
            self$current_page <- "validation_failed"
            return(invisible(NULL))
          }
        }
        cli::cli_alert_success(
          "Data Validation Completed with no blocking issues"
        )
      }

      if (is.null(self$generation_func)) {
        cli::cli_alert_warning("No generation function configured. Exiting workflow.")
        return(invisible(NULL))
      } else {
        cli::cli_h2("Darwin Core Archive Generation Beginning")
        archive <- self$generation_func(source_data)
        cli::cli_alert_success("Darwin Core Archive Generation Completed")
      }

      if (!is.null(self$upload_func)) {
        cli::cli_h2("Data Upload Beginning")
        self$upload_func(archive)
        cli::cli_alert_success("Data Upload Completed")
      }

      cli::cli_h1("Data publication workflow completed successfully!")
    },
    set = function(field, value) {
      self[[field]] <- value
    }
  )
)

#' eDNABridge CLI Interface
#' 
#' This function launches a command-line interface (CLI) for the eDNABridge package,
#' allowing users to interactively configure and execute a workflow for publishing
#' eDNA data to GBIF. Users can select options for data ingestion, validation,
#' Darwin Core Archive generation, and data upload to the GBIF IPT.
#' 
#' @examples
#' \dontrun{
#' eDNABridge()
#' }
#' @return Invisibly returns NULL after exiting the CLI interface.
#' 
#' @export
eDNABridge <- function() {
  cli_helper <- cli_workflow_helper$new()
  cli::cli_h1("Welcome to the eDNABridge CLI Interface")
  cli::cli_text("This interface will guide you through the process of publishing your eDNA data to GBIF.")
  cli::cli_text("Select options as prompted to complete the workflow.")
  cli::cli_rule()
  repeat {
    cli::cli_h1("Main Menu")
    cli_print_status(cli_helper)
    cli::cli_text("Available commands:")
    cli::cli_ul(c(
      "1: Configure workflow settings",
      "2: Run data publication workflow",
      "0: Exit the interface"
    ))
    cli::cli_text("\n")
    choice <- readline("Enter your choice: ")
    cli::cli_rule()
    if (choice == "1") {
      cli_helper <- cli_configure_workflow(cli_helper)
    } else if (choice == "2") {
      cli_helper$execute_workflow()
      cli::cli_h1("Exiting the eDNABridge CLI Interface")
      break
    } else if (choice == "0") {
      cli::cli_h1("Exiting the eDNABridge CLI Interface")
      break
    } else {
      cli::cli_alert_danger("Invalid choice. Please enter a number between 0 and 3.")
    }
    cli::cli_rule()
  }
}

#' @noRd
cli_print_status <- function(cli_helper) {
  cli::cli_h2("Current Workflow Configuration Status")
  cli::cli_text("Ingestion Step: {ifelse(cli_helper$status$ingestion, 'Configured', 'Not Configured')}")
  cli::cli_text("Validation Step: {ifelse(cli_helper$status$validation, 'Configured', 'Not Configured')}")
  cli::cli_text("Generation Step: {ifelse(cli_helper$status$generation, 'Configured', 'Not Configured')}")
  cli::cli_text("Upload Step: {ifelse(cli_helper$status$upload, 'Configured', 'Not Configured')}")
  cli::cli_text("\n")
}

#' @noRd
cli_configure_workflow <- function(cli_helper) {
  cli::cli_h1("Configure Workflow Settings")
  cli::cli_text("Select the functions to use for each step of the workflow.")
  cli::cli_rule()
  ingestion_options <- c(
    "1: Ingest from Wilderlab API (public only)",
    "2: Ingest from Wilderlab API (all data)",
    "3: Ingest from CSV",
    "4: Skip step",
    "0: Return to Main Menu"
  )
  validation_options <- c(
    "1: Standard Validation",
    "2: Skip step",
    "0: Return to Main Menu"
  )
  generation_options <- c(
    "1: Standard Archive Generation",
    "2: Import Existing Archive",
    "0: Return to Main Menu"
  )
  upload_options <- c(
    "1: Upload to IPT and Publish",
    "2: Upload to IPT without Publishing",
    "3: Skip step",
    "0: Return to Main Menu"
  )

  # --- Data Ingestion Options
  cli::cli_h2("Data Ingestion Options")
  cli::cli_ul(ingestion_options)
  cli::cli_text("\n")
  ingestion_choice <- readline("Select ingestion method: ")
  if (ingestion_choice == "1") {
    cli_helper$ingestion_func <- function() {
      wl_get_all_wilderlab_data() |>
      wl_map_wilderlab_data() |>
      wl_inject_dwc_wilderlab_constants()
    }
    cli_helper$status$ingestion <- TRUE
  } else if (ingestion_choice == "2") {
    cli_helper$ingestion_func <- function() {
      wl_get_all_wilderlab_data(public_only = FALSE) |>
      wl_map_wilderlab_data() |>
      wl_inject_dwc_wilderlab_constants()
    }
    cli_helper$status$ingestion <- TRUE
  } else if (ingestion_choice == "3") {
    cli::cli_alert_info("Please ensure your CSV file matches the expected format.")
    cli::cli_text("\n")
    file_path <- readline("Enter the path to the CSV file: ")
    cli_helper$ingestion_func <- function() {
      ingest_read_and_map_table(file_path, mapping = c())
    }
    cli_helper$status$ingestion <- TRUE
  } else if (ingestion_choice == "4") {
    cli_helper$ingestion_func <- NULL
    cli_helper$status$ingestion <- TRUE
  } else {
    cli::cli_alert_info("Returning to Main Menu.")
    return(cli_helper)
  }

  # --- Data Validation Options
  cli::cli_h2("Data Validation Options")
  cli::cli_ul(validation_options)
  cli::cli_text("\n")
  validation_choice <- readline("Select validation method: ")
  if (validation_choice == "1") {
    cli_helper$validation_func <- function(input) {
      col_req_issues <- val_validate_dwc_to_level(input, "required")
      col_extra_issues <- val_validate_no_additional_terms(input)
      row_issues <- val_validate_all_fields(input)
      return(
        val_combine_issues(list(col_req_issues, col_extra_issues, row_issues))
      )
    }
    cli_helper$status$validation <- TRUE
  } else if (validation_choice == "2") {
    cli_helper$validation_func <- NULL
    cli_helper$status$validation <- TRUE
  } else{
    cli::cli_alert_info("Returning to Main Menu.")
    return(cli_helper)
  }

  # --- Archive Generation Options
  cli::cli_h2("Archive Generation Options")
  cli::cli_ul(generation_options)
  cli::cli_text("\n")
  generation_choice <- readline("Select generation method: ")
  if (generation_choice == "1") {
    cli_helper$generation_func <- function(input){
      bbox <- eml_calculate_bounding_box(
        lats = input$decimalLatitude,
        longs = input$decimalLongitude
      )

      cli::cli_alert_info("Do you have an existing EML template to read?")
      cli::cli_ul(c(
        "1: No, generate a new template",
        "2: Yes, read from existing template"
      ))
      cli::cli_text("\n")
      template_choice <- readline("Select an option: ")

      if (template_choice == "1") {
        cli::cli_alert_info("To continue, please provide a path to save the generated archive template.")
        cli::cli_text("\n")
        template_path <- readline("Enter the path for the template CSV file: ")
        eml_gbif_template_create(template_path)
        cli::cli_alert_info("Please fill out the generated template with appropriate metadata.")
        cli::cli_text("\n")
        readline("Press Enter once the template is filled out and saved: ")
      } else {
        cli::cli_alert_info("To continue, please provide a path to the filled out template.")
        cli::cli_text("\n")
        template_path <- readline("Enter the path for the template CSV file: ")
      }

      eml <- eml_gbif_template_read(template_path, bbox = bbox)
      dwc_tibbles <- gen_tibble_to_dwc_frames(input)
      meta <- meta_xml_base() |>
        meta_xml_add_file(
          "core",
          dwc_tibbles$occurrence_core,
          location = "occurrence_core.txt"
        )
      archive_path <- gen_make_dwc_archive(
        dwc_tibbles,
        eml,
        meta,
        path = "dwc_archive.zip"
      )
      cli::cli_alert_info("Darwin Core Archive generated at {archive_path}")
      return(archive_path)
    }
    cli_helper$status$generation <- TRUE
  } else if (generation_choice == "2") {
    cli_helper$generation_func <- function(...) {
      cli::cli_alert_info("Please provide the path to your existing Darwin Core Archive ZIP file.")
      cli::cli_text("\n")
      archive_path <- readline("Enter the path to the ZIP file: ")
      return(archive_path)
    }
    cli_helper$status$generation <- TRUE
  } else {
    cli::cli_alert_info("Returning to Main Menu.")
    return(cli_helper)
  }

  # --- Data Upload Options
  cli::cli_h2("Data Upload Options")
  cli::cli_ul(upload_options)
  cli::cli_text("\n")
  upload_choice <- readline("Select upload method: ")
  if (upload_choice == "1") {
    cli_helper$upload_func <- function(archive_path) {
      cli::cli_alert_info("Please provide a resource shortname for your dataset on GBIF IPT. (lowercase, no spaces)")
      cli::cli_text("\n")
      dataset_shortname <- readline("Enter dataset shortname: ")
      ipt_upload_and_publish(archive_path, dataset_shortname)
    }
    cli_helper$status$upload <- TRUE
  } else if (upload_choice == "2") {
    cli_helper$upload_func <- function(archive_path) {
      cli::cli_alert_info("Please provide a resource shortname for your dataset on GBIF IPT. (lowercase, no spaces)")
      cli::cli_text("\n")
      dataset_shortname <- readline("Enter dataset shortname: ")
      ipt_login() |>
        ipt_upload_archive(
          shortname = dataset_shortname,
          archive = archive_path
        ) |>
        ipt_save_metadata(shortname = dataset_shortname)
      cli::cli_alert_info("Dataset uploaded to IPT but not published.")
    }
    cli_helper$status$upload <- TRUE
  } else if (upload_choice == "3") {
    cli_helper$upload_func <- NULL
    cli_helper$status$upload <- TRUE
  } else {
    cli::cli_alert_info("Returning to Main Menu.")
    return(cli_helper)
  }
  cli::cli_alert_success("Workflow configuration completed.")
  return(cli_helper)
}