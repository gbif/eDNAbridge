##############################################################################
## ------------------------------ eDNABridge ------------------------------ ##
## This package was designed and developed by Epi (https://www.epi.group/)  ##
## with funding provided by the New Zealand Ministry for the Environment.   ##
##                                                                          ##
## Support was provided by Wilderlab, the Hawke’s Bay Regional Council,     ##
## Department of Conservation and Earth Sciences New Zealand (NIWA)         ##
## ------------------------------------------------------------------------ ##
##############################################################################


#' Returns the mappings of Wilderlab columns to Darwin Core standard column names
#' @return A named character vector where names are standard column names and values are Wilderlab
#' @keywords internal
#' @noRd
wl_wilderlab_dwc_mappings <- function() {
  return(
    c(
      occurrenceID = "HID",
      eventID = "UID",
      eventDate = "CollectionDate",
      scientificName = "Name",
      taxonRank = "Rank",
      taxonID = "TaxID",
      organismQuantity = "Count",
      decimalLatitude = "Latitude",
      decimalLongitude = "Longitude",
      kingdom = "kingdom",
      phylum = "phylum",
      class = "class",
      order = "order",
      family = "family",
      genus = "genus",
      species = "species"
    )
  )
}