# An offline copy of the schema is stored as a fallback
gbif_dwc_schema <- get_all_gbif_dwc_schema()
usethis::use_data(gbif_dwc_schema, overwrite = TRUE, internal = TRUE)