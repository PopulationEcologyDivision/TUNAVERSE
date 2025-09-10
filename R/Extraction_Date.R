#' @title ExtractDate
#' @description This function provides the date the Oracle tables were last extracted
#' and the number of elapsed days. 
#' @author  Alex Hanke, \email{Alex.Hanke@@dfo-mpo.gc.ca}
#' @importFrom magrittr %>%
#' @export
ExtractDate <- function(){
  data.dir = "R:/ATLSiteShares/SABS/LargePelagics/COMMERCIAL DATA/TUNAVERSE/data/MARFISSCI.TRIPS.RData"
  TimeMade <- round(file.mtime(data.dir))
  Elapsed_Days = as.numeric(round(Sys.time()- TimeMade,1))
  message = paste("Data was last extracted on: ",TimeMade, ".\nThis was ",Elapsed_Days," days ago.", sep="")
  message(message)
}