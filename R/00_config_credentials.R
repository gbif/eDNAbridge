##############################################################################
## ------------------------------ eDNABridge ------------------------------ ##
## This package was designed and developed by Epi (https://www.epi.group/)  ##
## with funding provided by the New Zealand Ministry for the Environment.   ##
##                                                                          ##
## Support was provided by Wilderlab, the Hawke’s Bay Regional Council,     ##
## Department of Conservation and Earth Sciences New Zealand (NIWA)         ##
## ------------------------------------------------------------------------ ##
##############################################################################


#' Set credentials for services in the package
#'
#' Sets a credential in the environment and optionally cache it in .Renviron
#'
#' @param key_name The name of the environment variable to set (e.g., "WILDERLAB_KEY")
#' @param key_value The value to set for the environment variable
#' @param cache Logical indicating whether to save the key in the .Renviron file
#' @param renv_path The path to the .Renviron file (default is ".Renviron" in the working directory)
#' @return None. Sets the environment variable and optionally updates .Renviron.
#' @examples
#' # .Renviron will be created/updated in the working directory
#' \dontrun{
#' config_set_creds("WILDERLAB_KEY", "your_api_key_here", cache = TRUE)
#' }
#'
#' @export
config_set_creds <- function(
  key_name = NULL,
  key_value = NULL,
  cache = FALSE,
  renv_path = ".Renviron"
) {
  if (is.null(key_name) || is.null(key_value)) {
    stop("Both key_name and key_value must be provided")
  }
  key_name <- toupper(key_name)
  if (!grepl("^[A-Z0-9_]+$", key_name)) {
    stop(
      "key_name must contain only uppercase letters, numbers, and underscores"
    )
  }
  do.call(Sys.setenv, setNames(list(key_value), key_name))
  if (cache) {
    if (file.exists(renv_path)) {
      lines <- readLines(renv_path)
      pattern <- paste0("^", key_name, "=")
      lines <- lines[!grepl(pattern, lines)]
      lines <- c(lines, paste0(key_name, "=", key_value))
      writeLines(lines, renv_path)
    } else {
      writeLines(paste0(key_name, "=", key_value), renv_path)
    }
  }
}

#' Specifically set Wilderlab credentials
#'
#' Sets Wilderlab API credentials in the environment and optionally cache them in .Renviron
#'
#' @param wilderlab_key The Wilderlab API key
#' @param wilderlab_secret The Wilderlab API secret
#' @param wilderlab_xapikey The Wilderlab X-API-Key
#' @param cache Logical indicating whether to save the keys in the .Renviron file
#' @return None. Sets the environment variables and optionally updates .Renviron.
#' @seealso [config_set_creds()]
#' @examples
#' # .Renviron will be created/updated in the working directory
#' \dontrun{
#' config_set_wilderlab_creds(
#'   wilderlab_key = "your_key_here",
#'   wilderlab_secret = "your_secret_here",
#'   wilderlab_xapikey = "your_xapikey_here",
#'   cache = TRUE
#' )
#' }
#'
#' @export
config_set_wilderlab_creds <- function(
  wilderlab_key = NULL,
  wilderlab_secret = NULL,
  wilderlab_xapikey = NULL,
  cache = FALSE
) {
  if (!is.null(wilderlab_key)) {
    config_set_creds("WILDERLAB_KEY", wilderlab_key, cache = cache)
  }
  if (!is.null(wilderlab_secret)) {
    config_set_creds("WILDERLAB_SECRET", wilderlab_secret, cache = cache)
  }
  if (!is.null(wilderlab_xapikey)) {
    config_set_creds("WILDERLAB_XAPIKEY", wilderlab_xapikey, cache = cache)
  }
}

#' Checks that a list of credentials are set in the environment
#' 
#' If not, prompts the user to set them via CLI input, or gives an option to stop to set .Renviron manually
#' 
#' @param keys A character vector of environment variable names to check
#' @param prompt_user Logical indicating whether to prompt the user for missing keys
#' @param read_fn Function to use for reading user input (default is readline, can be mocked for tests)
#' @return A character vector of missing keys if any were missing, otherwise NULL
#' @keywords internal
#' @noRd
config_check_creds <- function(
  keys = c("WILDERLAB_KEY", "WILDERLAB_SECRET", "WILDERLAB_XAPIKEY"),
  prompt_user = TRUE,
  read_fn = readline # allows mocking for tests
) {
  missing_keys <- keys[!nzchar(Sys.getenv(keys))]
  if (length(missing_keys) > 0) {
    cli::cli_alert_warning(
      "The following credentials are missing: ",
      paste(missing_keys, collapse = ", ")
    )
    user_keys <- list()
    if (prompt_user) {
      for (key in missing_keys) {
        repeat {
          cli::cli_alert_info(paste0(
            "Please enter value for ",
            key,
            " (or type 'skip' to stop): "
          ))
          user_input <- read_fn(prompt = paste(key, ": "))
          if (tolower(user_input) == "skip") {
            stop(
              "Credential setup aborted by user. Please set the missing credentials in your .Renviron file."
            )
          } else if (nzchar(user_input)) {
            user_keys[[key]] <- user_input
            break
          } else {
            cli::cli_alert_danger("Input cannot be empty. Please try again.")
          }
        }
      }
      cli::cli_alert_info(
        "Would you like to cache these credentials in your .Renviron file for future sessions? (y/n): "
      )
      cache_input <- tolower(read_fn(prompt = "Cache in .Renviron? (y/n): "))
      cache <- cache_input == "y"
      for (key in names(user_keys)) {
        config_set_creds(key, user_keys[[key]], cache = cache)
      }
      cli::cli_alert_success("Credentials set successfully.")
    }
    return(missing_keys)
  }
}
