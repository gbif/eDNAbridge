##############################################################################
## ------------------------------ eDNABridge ------------------------------ ##
## This package was designed and developed by Epi (https://www.epi.group/)  ##
## with funding provided by the New Zealand Ministry for the Environment.   ##
##                                                                          ##
## Support was provided by Wilderlab, the Hawke’s Bay Regional Council,     ##
## Department of Conservation and Earth Sciences New Zealand (NIWA)         ##
## ------------------------------------------------------------------------ ##
##############################################################################


#' Authenticate to an IPT instance
#' 
#' This function logs into an IPT instance using credentials stored in environment variables.
#' It returns an authenticated session that can be used for subsequent requests.
#' If the required environment variables are not set, the user will be prompted to enter them.
#' 
#' @section Environment Variables:
#' The following environment variables must be set for authentication:
#' - `IPT_URL`: The base URL of the IPT instance (e.g., "https://ipt.example.com")
#' - `IPT_USERNAME`: The username for IPT login
#' - `IPT_PASSWORD`: The password for IPT login
#' 
#' @return An authenticated rvest session object
#' @seealso [ipt_get_jsessionid()], [ipt_upload_archive()], [ipt_save_metadata()], [ipt_publish_resource()]
#' @examples
#' \dontrun{
#' session <- ipt_login()
#' print(session$response$status_code)
#' }
#' @export
ipt_login <- function() {
  config_check_creds(
    keys = c("IPT_URL", "IPT_USERNAME", "IPT_PASSWORD"),
    prompt_user = TRUE
  )
  url <- Sys.getenv("IPT_URL") |>
    paste0("/login.do")
  s <- rvest::session(url)
  login_form <- rvest::html_form(s)[[2]]
  filled <- rvest::html_form_set(
    login_form,
    email = Sys.getenv("IPT_USERNAME"),
    password = Sys.getenv("IPT_PASSWORD")
  )
  rvest::session_submit(s, filled)
}

#' Extract JSESSIONID from an authenticated IPT session
#' 
#' This function retrieves the JSESSIONID cookie value from an authenticated IPT session.
#' This cookie is required for making authenticated requests to the IPT.
#' This works for both rvest sessions and httr2 requests.
#' However, the token expires when a session is closed, so holding a session open with
#' [ipt_login()] is required for httr2.
#' 
#' @param session An authenticated rvest session object
#' @return The JSESSIONID cookie value as a string
#' @seealso [ipt_login()], [ipt_upload_archive()], [ipt_save_metadata()], [ipt_publish_resource()]
#' @examples
#' \dontrun{
#' session <- ipt_login()
#' jsessionid <- ipt_get_jsessionid(session)
#' print(jsessionid)
#' }
#' 
#' @export
ipt_get_jsessionid <- function(session) {
  session$response$cookies$value[session$response$cookies$name == "JSESSIONID"]
}