autoFilter <- function(tblList, quiet = T){
  LOOPAGAIN = T
  while (LOOPAGAIN){
    precnt = sum(sapply(tblList, NROW))
    if (!quiet) print(precnt)
    tblList$SLIP_OFFLD_STD_INFO <- merge(tblList$SLIP_OFFLD_STD_INFO,  unique(tblList$MON_DOCS[,"MON_DOC_ID",drop=F]))
    tblList$SLIP_BUYR_STD_INFO  <- merge(tblList$SLIP_BUYR_STD_INFO, unique(tblList$SLIP_OFFLD_STD_INFO[,"SLIP_OFFLD_STD_INFO_ID",drop=F]))
    tblList$SLIP_SPC_STD_INFO   <- merge(tblList$SLIP_SPC_STD_INFO,  unique(tblList$SLIP_BUYR_STD_INFO[,"SLIP_BUYR_STD_INFO_ID",drop=F]))  
    
    tblList$TRIPS <- merge(tblList$TRIPS,  unique(tblList$MON_DOCS[,"TRIP_ID",drop=F]))
    tblList$PRO_SPC_INFO <- merge(tblList$PRO_SPC_INFO, unique(tblList$TRIPS[,"TRIP_ID",drop=F]))
    tblList$PRO_SPC_INFO <- merge(tblList$PRO_SPC_INFO, unique(tblList$MON_DOCS[,"MON_DOC_ID",drop=F]))
    tblList$PRO_SPC_INFO <- merge(tblList$PRO_SPC_INFO, unique(tblList$LOG_EFRT_STD_INFO[,"LOG_EFRT_STD_INFO_ID",drop=F])) #needed?
    
    tblList$LOG_EFRT_STD_INFO <-  merge(tblList$LOG_EFRT_STD_INFO,   unique(tblList$MON_DOCS[,"MON_DOC_ID",drop=F]))
    tblList$LOG_SPC_STD_INFO <- merge(tblList$LOG_SPC_STD_INFO,  unique(tblList$LOG_EFRT_STD_INFO[,"LOG_EFRT_STD_INFO_ID",drop=F]))
    
    tblList$LOG_EFRT_ENTRD_DETS <- merge(tblList$LOG_EFRT_ENTRD_DETS,  unique(tblList$LOG_SPC_STD_INFO[,"LOG_EFRT_STD_INFO_ID",drop=F]))
    tblList$MON_DOC_ENTRD_DETS <- merge(tblList$MON_DOC_ENTRD_DETS,  unique(tblList$MON_DOCS[,"MON_DOC_ID",drop=F]))  
    tblList$LOG_SPC_ENTRD_DETS <- merge(tblList$LOG_SPC_ENTRD_DETS, unique(tblList$LOG_SPC_STD_INFO[,"LOG_SPC_STD_INFO_ID",drop=F]))
    
    postcnt = sum(sapply(tblList, NROW))
    
    if(postcnt==precnt) {
      LOOPAGAIN=FALSE
      if (!quiet) print(paste0("FINAL:", postcnt))
    } 
  }
  return(tblList)
}
