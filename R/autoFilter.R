#' @title autoFilter
#' @description This function operates on the various object identified in \code{tblList}, and 
#' ensures that they don't contain extraneous records.  For example, if the TRIPS object within 
#' tblList is filtered to only 2013 records, other objects such as MON_DOCS and LOG_SPC_STD_INFO 
#' will be filtered to ensure that only contain records that related to the remianing trips.
#' @param tblList  this is a vector of object names necessary for the functions of TUNAVERSE to run. 
#' @import data.table
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
autoFilter <- function(tblList){
  # Convert data frames to data tables
  tblList <- lapply(tblList, as.data.table)
  LOOPAGAIN = T
  while (LOOPAGAIN){
    precnt = sum(sapply(tblList, nrow))
    tblList$SLIP_OFFLD_STD_INFO <- tblList$SLIP_OFFLD_STD_INFO[unique(tblList$MON_DOCS[, .(MON_DOC_ID)]), on = "MON_DOC_ID", nomatch = 0]
    tblList$SLIP_BUYR_STD_INFO  <- tblList$SLIP_BUYR_STD_INFO[unique(tblList$SLIP_OFFLD_STD_INFO[, .(SLIP_OFFLD_STD_INFO_ID)]), on = "SLIP_OFFLD_STD_INFO_ID", nomatch = 0]
    tblList$SLIP_SPC_STD_INFO   <- tblList$SLIP_SPC_STD_INFO[unique(tblList$SLIP_BUYR_STD_INFO[, .(SLIP_BUYR_STD_INFO_ID)]), on = "SLIP_BUYR_STD_INFO_ID", nomatch = 0]  
    
    tblList$TRIPS        <- tblList$TRIPS[unique(tblList$MON_DOCS[, .(TRIP_ID)]), on = "TRIP_ID", nomatch = 0]
    tblList$PRO_SPC_INFO <- tblList$PRO_SPC_INFO[unique(tblList$TRIPS[, .(TRIP_ID)]), on = "TRIP_ID", nomatch = 0]
    tblList$PRO_SPC_INFO <- tblList$PRO_SPC_INFO[unique(tblList$MON_DOCS[, .(MON_DOC_ID)]), on = "MON_DOC_ID", nomatch = 0]
    tblList$PRO_SPC_INFO <- tblList$PRO_SPC_INFO[unique(tblList$LOG_EFRT_STD_INFO[, .(LOG_EFRT_STD_INFO_ID)]), on = "LOG_EFRT_STD_INFO_ID", nomatch = 0] #needed?
    
    tblList$LOG_EFRT_STD_INFO <-  tblList$LOG_EFRT_STD_INFO[unique(tblList$MON_DOCS[,.(MON_DOC_ID)]), on = "MON_DOC_ID", nomatch = 0]
    tblList$LOG_SPC_STD_INFO <- tblList$LOG_SPC_STD_INFO[unique(tblList$LOG_EFRT_STD_INFO[,.(LOG_EFRT_STD_INFO_ID)]), on = "LOG_EFRT_STD_INFO_ID", nomatch = 0]    
    
    
    tblList$LOG_EFRT_ENTRD_DETS <- tblList$LOG_EFRT_ENTRD_DETS[unique(tblList$LOG_SPC_STD_INFO[,.(LOG_EFRT_STD_INFO_ID)]), on = "LOG_EFRT_STD_INFO_ID", nomatch = 0]
    tblList$MON_DOC_ENTRD_DETS <- tblList$MON_DOC_ENTRD_DETS[unique(tblList$MON_DOCS[,.(MON_DOC_ID)]), on = "MON_DOC_ID", nomatch = 0]  
    tblList$LOG_SPC_ENTRD_DETS <- tblList$LOG_SPC_ENTRD_DETS[unique(tblList$LOG_SPC_STD_INFO[,.(LOG_SPC_STD_INFO_ID)]), on = "LOG_SPC_STD_INFO_ID", nomatch = 0]
    
    postcnt = sum(sapply(tblList, nrow))
    if(postcnt==precnt) {
      LOOPAGAIN=FALSE
    } 
  }
  tblList <- lapply(tblList, as.data.frame)
  return(tblList)
}