#' Open MARFIS Documentation and Reference Guides
#'
#' Provides access to bundled reference documents for querying and understanding
#' the MARFIS Catch & Effort data model in `TUNAVERSE`.
#'
#' **Bundled Resources:**
#' - **MARFIS Catch & Effort User's Guide (`MARFIS Catch & Effort User's Guide.pdf`):** A
#'   practical SQL/R extraction and query-design guide covering MARFIS table
#'   hierarchy and grain, standard vs. `*_ENTRD_DETS` detail storage, rules for
#'   preventing row fan-out, pre-filtering `PRO_SPC_INFO` and detail CTEs, and
#'   a reference set-and-species query template.
#' - **Science Description of MARFIS (`Science Description of MARFIS.pdf`):** An
#'   authoritative system-level reference describing MARFIS functional domains
#'   (Licencing/Entitlements, Quota, and Catch & Effort), operational trip and
#'   hail-in/hail-out workflows, Catch & Effort table structure, `FV_` column
#'   behavior and caveats, and `V_MON_DOC_DEFINTIONS` inspection rules.
#'
#' @param doc Character string selecting which guide to open: `"users_guide"`
#'   (default) for the *MARFIS Catch & Effort User's Guide* or
#'   `"science_description"` for the *Science Description of MARFIS*.
#' @return Invisibly returns the file path to the selected PDF document.
#' @export
marfis_guide <- function(doc = c("users_guide", "science_description")) {
  doc <- match.arg(doc)
  
  candidates <- switch(
    doc,
    users_guide = c(
      "MARFIS Catch & Effort User's Guide.pdf",
      "marfis_users_guide.pdf"
    ),
    science_description = c(
      "Science Description of MARFIS.pdf",
      "marfis_science_description.pdf"
    )
  )
  
  pdf_path <- ""
  for (fname in candidates) {
    p <- system.file(fname, package = "TUNAVERSE")
    if (nzchar(p) && file.exists(p)) {
      pdf_path <- p
      break
    }
  }
  
  if (!nzchar(pdf_path)) {
    stop("Document for `", doc, "` was not found in the TUNAVERSE package.")
  }
  
  if (interactive()) {
    utils::browseURL(pdf_path)
  }
  
  invisible(pdf_path)
}