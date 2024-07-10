#' @title enWidener
#' @description A number of data objects used by this package store their data in "long" format 
#' (i.e. various *_DETS).  This function converts these objects from long to wide while only 
#' retaining fields directly related to the large pelagics fishery.
#' @param tblList  this is a vector of object names necessary for the functions of TUNAVERSE to run. 
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom dplyr %>%
#' @export
enWidener<-function(tblList){
  #####
  tblList$LOG_EFRT_ENTRD_DETS <- tblList$LOG_EFRT_ENTRD_DETS[tblList$LOG_EFRT_ENTRD_DETS$COLUMN_DEFN_ID %in% c(479,689,559,558),c("LOG_EFRT_STD_INFO_ID","COLUMN_DEFN_ID","DATA_VALUE")]
  tblList$LOG_EFRT_ENTRD_DETS <- tblList$LOG_EFRT_ENTRD_DETS %>%
    tidyr::spread(key = COLUMN_DEFN_ID, value = DATA_VALUE)
  LE_DETS_LOOKUP <- data.frame(
    COL_DEFN_ID = c(479, 689,559,558),
    Name = c("Gear","Capture_Time","Water_Temp_UOM","Water_Temp")
  ) 
  LE_lookup <- stats::setNames(LE_DETS_LOOKUP$Name, LE_DETS_LOOKUP$COL_DEFN_ID)
  names(tblList$LOG_EFRT_ENTRD_DETS) <- as.character(names(tblList$LOG_EFRT_ENTRD_DETS))
  for (LE_col in names(tblList$LOG_EFRT_ENTRD_DETS)) {
    if (LE_col %in% names(LE_lookup)) {
      names(tblList$LOG_EFRT_ENTRD_DETS)[names(tblList$LOG_EFRT_ENTRD_DETS) == LE_col] <- LE_lookup[LE_col]
    }
  }
  tblList$LOG_EFRT_ENTRD_DETS <- suppressWarnings(tblList$LOG_EFRT_ENTRD_DETS %>%
                                                    dplyr::mutate(Gear = dplyr::if_else(!is.na(as.numeric(Gear)), NA_character_, Gear)))
  
  tblList$LOG_SPC_ENTRD_DETS <- tblList$LOG_SPC_ENTRD_DETS[tblList$LOG_SPC_ENTRD_DETS$COLUMN_DEFN_ID %in% c(484,743,807,744,808),c("LOG_SPC_STD_INFO_ID","COLUMN_DEFN_ID","DATA_VALUE")]
  tblList$LOG_SPC_ENTRD_DETS <- tblList$LOG_SPC_ENTRD_DETS %>%
    tidyr::spread(key = COLUMN_DEFN_ID, value = DATA_VALUE)
  LS_DETS_LOOKUP <- data.frame(
    COL_DEFN_ID = c(484,743,807,744,808),
    Name = c("log_BFT_Tag_Num","flank_length","flank_length_uom","dressed_length","dressed_length_uom")
  ) 
  LS_lookup <- stats::setNames(LS_DETS_LOOKUP$Name, LS_DETS_LOOKUP$COL_DEFN_ID)
  names(tblList$LOG_SPC_ENTRD_DETS) <- as.character(names(tblList$LOG_SPC_ENTRD_DETS))
  for (LS_col in names(tblList$LOG_SPC_ENTRD_DETS)) {
    if (LS_col %in% names(LS_lookup)) {
      names(tblList$LOG_SPC_ENTRD_DETS)[names(tblList$LOG_SPC_ENTRD_DETS) == LS_col] <- LS_lookup[LS_col]
    }
  }
  
  tblList$MON_DOC_ENTRD_DETS <- tblList$MON_DOC_ENTRD_DETS[tblList$MON_DOC_ENTRD_DETS$COLUMN_DEFN_ID %in% c(552,79,104,428,742,43,477,4,480,481,53),c("MON_DOC_ID","COLUMN_DEFN_ID","DATA_VALUE")]
  tblList$MON_DOC_ENTRD_DETS <- tblList$MON_DOC_ENTRD_DETS %>%
    tidyr::spread(key = COLUMN_DEFN_ID, value = DATA_VALUE)
  MD_DETS_LOOKUP <- data.frame(
    COL_DEFN_ID = c(552,79,104,428,55,742,43,477,4,480,481,53),
    Name = c("Home_Mgt_Area", "Date_Sailed", "Time_Sailed", "Trip_Number","NAFO_Unit", 
             "Total_Hours_Fished","Num_Of_Strikes","Total_Num_BFT_Caught_Trip",
             "Hook_Size","Num_Of_Lines","Gauge_Of_Mono","Bait")
  )
  MD_lookup <- stats::setNames(MD_DETS_LOOKUP$Name, MD_DETS_LOOKUP$COL_DEFN_ID)
  names(tblList$MON_DOC_ENTRD_DETS) <- as.character(names(tblList$MON_DOC_ENTRD_DETS))
  for (MD_col in names(tblList$MON_DOC_ENTRD_DETS)) {
    if (MD_col %in% names(MD_lookup)) {
      names(tblList$MON_DOC_ENTRD_DETS)[names(tblList$MON_DOC_ENTRD_DETS) == MD_col] <- MD_lookup[MD_col]
    }
  }
  return(tblList)
}

#' @title dropFields
#' @description By default, the original MARFIS objects have a number of fields that are not 
#' useful for the purposes of this package. This function removes them.
#' @param tblList  this is a vector of object names necessary for the functions of TUNAVERSE to run. 
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
dropFields<-function(tblList){
  sink("nul") 
  dfList <- lapply(tblList, function(x) {
    x <- Mar.utils::clean_dfo_fields(x)
    return(x)
  })
  sink()
  return(dfList)
}

#' @title loadIntoList
#' @description This function takes a (specified) list of object names, and pulls them out of the 
#' global environment and into a list object.
#' @param tblList this is a vector of object names necessary for the functions of TUNAVERSE to run. 
#' @param removeOriginals default is \code{F}.  Once the specified tables have been loaded into a 
#' list, this tells the function whether to retain or delete the original objects. 
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
loadIntoList <- function(tblList, removeOriginals=F) {
  # Use mget to get all these dataframes and put them in a list
  dfList <- mget(tblList, envir = .GlobalEnv)
  
  # Remove the dataframes from the global environment
  if (removeOriginals)rm(list = tblList, envir = .GlobalEnv)
  
  return(dfList)
}