#' @title prepareData
#' @description This function extracts and/or loads data from the MARFISSCI schema into a list 
#' object.  If the necessary data (i.e. .rdata files) already exists within C:/DFO-MPO/PESDData/Tunaverse
#' \code{data.dir} (and \code{force.extract} is set to FALSE), it will load those.  If 
#' \code{force.extract} is TRUE or the files don't exist, it will extract the necessry files into
#' the \code{data.dir}.
#' @param cxn A valid Oracle connection object. This parameter allows you to 
#' pass an existing connection, reducing the need to establish a new connection 
#' within the function. If provided, it takes precedence over the connection-
#' related parameters.
#' @param force.extract The default value is FALSE.  By default, existing data will be loaded.  If
#' \code{force.extract ==TRUE}, than a full extraction will take place, overwriting any existing
#' data.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
prepareData <- function(cxn=NULL,force.extract=F, extract_user = NULL, extract_computer = NULL){
  # if (is.null(data.dir))stop("Please provide a location where the extracted .rdata files can be stored and/or accessed")
  Mar.utils::get_data_tables(schema = "MARFISSCI", cxn = cxn,
                             data.dir=get_pesd_tv_dir(),
                             tables= allTables, 
                             force.extract = force.extract,
                             extract_user = extract_user, 
                             extract_computer = extract_computer)
  
  rawData <- loadIntoList(allTables,removeOriginals = T)
  rawData <- dropFields(rawData)
  # rawData <- enWidener(rawData)
  rawData$LOG_EFRT_ENTRD_DETS <- rawData$LOG_EFRT_ENTRD_DETS[rawData$LOG_EFRT_ENTRD_DETS$COLUMN_DEFN_ID %in% c(479, 689,559,558),]
  # c("Gear","Capture_Time","Water_Temp_UOM","Water_Temp")
  rawData$LOG_EFRT_ENTRD_DETS <- Mar.utils::dets_defuddler(marfName = "LOG_EFRT_ENTRD_DETS", df=rawData$LOG_EFRT_ENTRD_DETS)
  
  rawData$LOG_SPC_ENTRD_DETS <- rawData$LOG_SPC_ENTRD_DETS[rawData$LOG_SPC_ENTRD_DETS$COLUMN_DEFN_ID %in%c(484,743,807,744,808),]
  # c("log_BFT_Tag_Num","flank_length","flank_length_uom","dressed_length","dressed_length_uom")
  rawData$LOG_SPC_ENTRD_DETS <- Mar.utils::dets_defuddler(marfName = "LOG_SPC_ENTRD_DETS", df=rawData$LOG_SPC_ENTRD_DETS)
  
  rawData$MON_DOC_ENTRD_DETS <- rawData$MON_DOC_ENTRD_DETS[rawData$MON_DOC_ENTRD_DETS$COLUMN_DEFN_ID %in% c(552,79,104,428,55,742,43,477,4,480,481,53),]

  # c("Home_Mgt_Area", "Date_Sailed", "Time_Sailed", "Trip_Number","NAFO_Unit","Total_Hours_Fished","Num_Of_Strikes","Total_Num_BFT_Caught_Trip","Hook_Size","Num_Of_Lines","Gauge_Of_Mono","Bait")
  rawData$MON_DOC_ENTRD_DETS <- Mar.utils::dets_defuddler(marfName = "MON_DOC_ENTRD_DETS", df=rawData$MON_DOC_ENTRD_DETS)  
  return(rawData)
}