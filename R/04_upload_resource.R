##############################################################################
## ------------------------------ eDNABridge ------------------------------ ##
## This package was designed and developed by Epi (https://www.epi.group/)  ##
## with funding provided by the New Zealand Ministry for the Environment.   ##
##                                                                          ##
## Support was provided by Wilderlab, the Hawke’s Bay Regional Council,     ##
## Department of Conservation and Earth Sciences New Zealand (NIWA)         ##
## ------------------------------------------------------------------------ ##
##############################################################################


#' Upload Darwin Core Archive to IPT
#'
#' Uploads a Darwin Core Archive (DwCA) to the Integrated Publishing Toolkit (IPT)
#' and creates a new resource with the specified shortname.
#'
#' @param session An optional IPT session object. If NULL, a new session will be created.
#' @param archive The file path to the Darwin Core Archive ZIP file.
#' @param shortname The shortname for the new resource in the IPT.
#' @return An updated IPT session object after the upload.
#' @seealso [ipt_login()], [ipt_get_jsessionid()], [ipt_save_metadata()], [ipt_publish_resource()]
#' @examples
#' \dontrun{
#' archive_path <- "path/to/dwca.zip"
#' shortname <- "my_resource"
#' ipt_login() |>
#' ipt_upload_archive(
#'  archive = archive_path,
#'  shortname = shortname
#' )
#' }
#'
#' @export
ipt_upload_archive <- function(
  session = NULL,
  archive,
  shortname
) {
  if (is.null(session)) {
    session <- ipt_login()
  }
  jsessionid <- session |> ipt_get_jsessionid()
  httr2::request(Sys.getenv("IPT_URL")) |>
    httr2::req_url_path_append("manage/create.do") |>
    httr2::req_body_multipart(
      create = "Create",
      shortname = shortname,
      resourceType = "occurrence",
      importDwca = "true",
      `_checkbox_importDwca` = "true",
      file = curl::form_file(archive),
    ) |>
    httr2::req_cookies_set(
      JSESSIONID = jsessionid
    ) |>
    httr2::req_perform()
  return(session)
}

#' Trigger metadata save on IPT resource
#'
#' Even if an EML file in a Darwin Core Archive contains all required metadata,
#' the IPT does not automatically recognize the data as complete. This function
#' navigates to the metadata editing page for the specified resource and triggers
#' a save action to ensure the IPT acknowledges the metadata. It sets the shortname
#' as part of the form submission, to trigger the save.
#'
#' @param session An optional IPT session object. If NULL, a new session will be created.
#' @param shortname The shortname of the resource whose metadata is to be saved.
#' @param org An optional organization name to set for the resource.
#' @return An updated IPT session object after saving the metadata.
#' @seealso [ipt_login()], [ipt_get_jsessionid()], [ipt_upload_archive()], [ipt_publish_resource()]
#' @examples
#' \dontrun{
#' shortname <- "my_resource"
#' ipt_login() |>
#' ipt_save_metadata(
#'   shortname = shortname
#' )
#' }
#'
#' @export
ipt_save_metadata <- function(
  session = NULL,
  shortname,
  org = NULL
) {
  if (is.null(session)) {
    session <- ipt_login()
  }
  session <- session |>
    rvest::session_jump_to(paste0(
      "/manage/metadata-basic.do?r=",
      shortname
    ))
  save_form <- rvest::html_form(session)[[1]]
  if (!is.null(org)) {
    org_choices <- save_form$fields$id$options
    if (!any(names(org_choices) |> stringr::str_detect(org))) {
      cli::cli_alert_warning(
        paste0(
          "Organization '",
          org,
          "' not found in IPT account. Available organizations are: ",
          paste(names(org_choices), collapse = ", ")
        )
      )
      cli::cli_alert_warning(
        "Proceeding without setting organization."
      )
    } else {
      save_form <- rvest::html_form_set(
        save_form,
        id = org_choices[names(org_choices) |> stringr::str_detect(org)][1]
      )
    }
  }
  filled <- rvest::html_form_set(
    save_form,
    eml.shortName = shortname
  )
  rvest::session_submit(session, filled)
  return(session)
}

#' Publish IPT resource
#'
#' Publishes a resource on the Integrated Publishing Toolkit (IPT) using its shortname.
#' Note that this triggers the publication process, but does not verify if the resource
#' is successfully published, and the IPT will need to process the request before it
#' appears as published.
#'
#' @param session An optional IPT session object. If NULL, a new session will be created.
#' @param shortname The shortname of the resource to be published.
#' @return An updated IPT session object after publishing the resource.
#' @seealso [ipt_login()], [ipt_get_jsessionid()], [ipt_upload_archive()], [ipt_save_metadata()]
#' @examples
#' \dontrun{
#' shortname <- "my_resource"
#' ipt_login() |>
#' ipt_publish_resource(
#'  shortname = shortname
#' )
#' # Check IPT after a few seconds to confirm publication
#' }
#'
#' @export
ipt_publish_resource <- function(
  session = NULL,
  shortname
) {
  if (is.null(session)) {
    session <- ipt_login()
  }
  jsessionid <- session |> ipt_get_jsessionid()
  summary_message <- paste0(
    "Publishing resource '",
    shortname,
    "' via {eDNABridge} on ",
    Sys.Date()
  )
  httr2::request(Sys.getenv("IPT_URL")) |>
    httr2::req_url_path_append("manage/publish.do") |>
    httr2::req_body_form(
      r = shortname,
      summary = summary_message,
      publish = "Publish"
    ) |>
    httr2::req_cookies_set(
      JSESSIONID = jsessionid
    ) |>
    httr2::req_perform()
  return(session)
}

#' Make IPT resource publicly visible
#' 
#' Sets the specified IPT resource to be publicly visible.
#' 
#' @param session An optional IPT session object. If NULL, a new session will be created.
#' @param shortname The shortname of the resource to be made public.
#' @return An updated IPT session object after making the resource public.
#' @seealso [ipt_login()], [ipt_get_jsessionid()]
#' @examples
#' \dontrun{
#' shortname <- "my_resource"
#' ipt_login() |>
#' ipt_set_visible(
#'   shortname = shortname
#' )
#' # Check IPT after a few seconds to confirm visibility
#' }
#' 
#' @export
ipt_set_visible <- function(
  session = NULL,
  shortname
) {
  if (is.null(session)) {
    session <- ipt_login()
  }
  jsessionid <- session |> ipt_get_jsessionid()
  httr2::request(Sys.getenv("IPT_URL")) |>
    httr2::req_url_path_append("manage/resource-makePublic.do") |>
    httr2::req_body_form(
      r = shortname,
      makePublicDateTime = NULL
    ) |>
    httr2::req_cookies_set(
      JSESSIONID = jsessionid
    ) |>
    httr2::req_perform()
  return(session)
}

#' Upload and publish Darwin Core Archive to IPT
#'
#' This function combines the steps of logging into the IPT, uploading a Darwin Core Archive,
#' saving the metadata, and publishing the resource. It takes the path to the archive
#' and the desired shortname as inputs, and performs all necessary actions in sequence.
#'
#' @param archive The file path to the Darwin Core Archive ZIP file.
#' @param shortname The shortname for the new resource in the IPT.
#' @return TRUE if all steps are completed successfully.
#' @seealso [ipt_login()], [ipt_upload_archive()], [ipt_save_metadata()], [ipt_publish_resource()]
#' @examples
#' \dontrun{
#' archive_path <- "path/to/dwca.zip"
#' shortname <- "my_resource"
#' ipt_upload_and_publish(
#'  archive = archive_path,
#'  shortname = shortname
#' )
#' # Check IPT after a few seconds to confirm publication
#' }
#'
#' @export
ipt_upload_and_publish <- function(
  archive,
  shortname
) {
  ipt_login() |>
    ipt_upload_archive(
      archive = archive,
      shortname = shortname
    ) |>
    ipt_save_metadata(
      shortname = shortname
    ) |>
    ipt_set_visible(
      shortname = shortname
    ) |>
    ipt_publish_resource(
      shortname = shortname
    )
  return(TRUE)
}

#' Register a resource with GBIF
#' 
#' Registers a resource on GBIF using its shortname in the IPT.
#' 
#' @param session An optional IPT session object. If NULL, a new session will be
#' created.
#' @param shortname The shortname of the resource to be registered with GBIF.
#' @return An updated IPT session object after registering the resource.
#' @seealso [ipt_login()], [ipt_get_jsessionid()]
#' 
#' @examples
#' \dontrun{
#' shortname <- "my_resource"
#' ipt_login() |>
#' ipt_register_with_gbif(
#'   shortname = shortname
#' )
#' }
#' 
#' @export
ipt_register_resource <- function(
  session = NULL,
  shortname
) {
  if (is.null(session)) {
    session <- ipt_login()
  }
  jsessionid <- session |> ipt_get_jsessionid()
  httr2::request(Sys.getenv("IPT_URL")) |>
    httr2::req_url_path_append("manage/resource-registerResource.do") |>
    httr2::req_body_form(
      r = shortname,
    ) |>
    httr2::req_cookies_set(
      JSESSIONID = jsessionid
    ) |>
    httr2::req_perform()
  return(session)
}