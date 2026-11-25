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