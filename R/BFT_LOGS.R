 #' @title BFT_LOGS
#' @description This function operates on the various object identified in \code{tblList}, and 
#' reformats them to imitate the MARFISSCI.BFT_LOGS view, but valid for all MON_DOC types.
#' @param tblList  this is a vector of object names necessary for the functions of TUNAVERSE to run. 
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom magrittr %>%
#' @export
BFT_LOGS <- function(tblList){
  filterBFT <-function(tblList){
    #simple Filtering - inclusive of all years
    tblList$MON_DOCS              <- tblList$MON_DOCS[tblList$MON_DOCS$MON_DOC_DEFN_ID %in% c(3,4,5,22,58),]  #3,4,5,22,
    tblList$LOG_SPC_STD_INFO      <- tblList$LOG_SPC_STD_INFO[tblList$LOG_SPC_STD_INFO$SSF_SPECIES_CODE %in% c(252,253,254,256),]
    tblList$SLIP_SPC_STD_INFO     <- tblList$SLIP_SPC_STD_INFO[tblList$SLIP_SPC_STD_INFO$SSF_SPECIES_CODE %in% c(252,253,254,256),]
    tblList <- autoFilter(tblList)
    return(tblList)
  }
  
  tblList <- filterBFT(tblList)
  # LOG_SPC_STD_INFO
  # (like MARFISSCI.BFT_LOG sql) limit to spp 254, and only consider data where unit of measure is 
  # 10 or 20  
  LOG_SPC_STD_INFO_corr<- tblList$LOG_SPC_STD_INFO %>%
    dplyr::filter(SSF_SPECIES_CODE == 254 & UNIT_OF_MEASURE_ID %in% c(10,20) ) %>% 
    dplyr::mutate(
      WEIGHT_LBS = dplyr::if_else(UNIT_OF_MEASURE_ID == 10, WEIGHT * 2.20462, WEIGHT), #kg to lbs
    ) %>% 
    dplyr::rename(UNIT_OF_MEASURE_ID_LS = UNIT_OF_MEASURE_ID,
                  SSF_LANDED_FORM_CODE_LS = SSF_LANDED_FORM_CODE)
  
  # SLIP_SPC_STD_INFO
  # (like MARFISSCI.BFT_LOG sql) limiting to spp 252,253,254 and 256; unit_of_measures 10 and 20, 
  # and SSF_LANDED_FORM_CODE of 1 and 3, convert weights as necessary, and then finding total of 
  # each spp by trip
  fake_data <- dplyr::tibble(
    SSF_SPECIES_CODE = c(252, 253, 254, 256),
    SLIP_BUYR_STD_INFO_ID = NA,
    TOTAL_WEIGHT_LBS = NA
  )
  SLIP_SPC_STD_INFO_wide <- tblList$SLIP_SPC_STD_INFO %>%
    dplyr::filter(UNIT_OF_MEASURE_ID %in% c(10,20) & SSF_SPECIES_CODE %in% c(252,253,254,256) & SSF_LANDED_FORM_CODE %in% c(1,3)) %>% 
    dplyr::mutate(
      WEIGHT_LBS = dplyr::if_else(UNIT_OF_MEASURE_ID == 10, WEIGHT * 2.20462, WEIGHT), #kg to lbs
      WEIGHT_LBS = dplyr::if_else(SSF_SPECIES_CODE == 254 & SSF_LANDED_FORM_CODE == 1, WEIGHT_LBS / 1.25, WEIGHT_LBS) 
      #landed_form of 1 gets divided by 1.25
    ) %>% 
    dplyr::rename(UNIT_OF_MEASURE_ID_SS = UNIT_OF_MEASURE_ID,
                  SSF_LANDED_FORM_CODE_SS = SSF_LANDED_FORM_CODE) %>%
    dplyr::group_by(SSF_SPECIES_CODE, SLIP_BUYR_STD_INFO_ID) %>%
    dplyr::summarise(TOTAL_WEIGHT_LBS = sum(WEIGHT_LBS, na.rm = TRUE), .groups = "keep") %>%
    dplyr::ungroup() %>%
    # Bind fake data
    dplyr::bind_rows(fake_data) %>%
    tidyr::pivot_wider(names_from = SSF_SPECIES_CODE, values_from = TOTAL_WEIGHT_LBS) %>%
    # Drop the row with NA for SLIP_BUYR_CODE
    dplyr::filter(!is.na(SLIP_BUYR_STD_INFO_ID)) %>%
    dplyr::rename(
      ALB_dressed_weight_lbs_trip = `252`,
      BET_dressed_weight_lbs_trip = `253`,
      BFT_dressed_weight_lbs_trip = `254`,
      YFT_dressed_weight_lbs_trip = `256`
    )
  
  # just grab the cols necessary to join SLIP_OFFLD to mon_docs
  OFFLD_TRIP_join <- tblList$SLIP_OFFLD_STD_INFO %>%
    dplyr::select(MON_DOC_ID, SLIP_OFFLD_STD_INFO_ID) %>%
    dplyr::distinct()
  
  # join together data to link trip-level spp catches with a mon_doc
  TRIP_b <- merge(tblList$SLIP_BUYR_STD_INFO[!is.na(tblList$SLIP_BUYR_STD_INFO$SLIP_OFFLD_STD_INFO_ID),c("SLIP_OFFLD_STD_INFO_ID", "SLIP_BUYR_STD_INFO_ID")], 
                  SLIP_SPC_STD_INFO_wide, by= "SLIP_BUYR_STD_INFO_ID")
  TRIP_b<- merge(TRIP_b, OFFLD_TRIP_join,all.x=T)
  TRIP_b <- TRIP_b %>%
    dplyr::select(-SLIP_OFFLD_STD_INFO_ID, -SLIP_BUYR_STD_INFO_ID) %>%
    dplyr::group_by(MON_DOC_ID) %>% #, SSF_SPECIES_SIZE_CODE, SSF_LANDED_FORM_CODE_SS 
    dplyr::summarise_all(sum, na.rm = TRUE)
  
  # grab the landing date and community for each offload.  Grab most recent date only if > 1 
  OFFLD_TRIP_dets <- tblList$SLIP_OFFLD_STD_INFO %>%
    dplyr::filter(!is.na(SLIP_OFFLD_STD_INFO_ID)) %>%
    dplyr::select(MON_DOC_ID, LANDING_DATE_TIME, COMMUNITY_CODE) %>%
    dplyr::distinct() %>%
    dplyr::group_by(MON_DOC_ID) %>%
    dplyr::filter(LANDING_DATE_TIME == max(LANDING_DATE_TIME))
  res<- list()
  LOG <- merge(tblList$MON_DOCS[,c("MON_DOC_ID", "MON_DOC_DEFN_ID", "FV_NAFO_UNIT_AREA_ID", "FV_GEAR_CODE")], 
               tblList$LOG_EFRT_STD_INFO[,c("MON_DOC_ID", "LOG_EFRT_STD_INFO_ID", "ENT_LATITUDE", "DET_LATITUDE", "ENT_LONGITUDE", "DET_LONGITUDE", "DET_NAFO_UNIT_AREA_ID", "FV_NAFO_UNIT_AREA_ID", "FV_FISHED_DATETIME","FV_GEAR_CODE")], by = "MON_DOC_ID",all.x=T)
  colnames(LOG)[colnames(LOG)=="FV_NAFO_UNIT_AREA_ID.x"] <- "MD_FV_NAFO_UNIT_AREA_ID"
  colnames(LOG)[colnames(LOG)=="FV_NAFO_UNIT_AREA_ID.y"] <- "LE_FV_NAFO_UNIT_AREA_ID"
  colnames(LOG)[colnames(LOG)=="FV_GEAR_CODE.x"] <- "GEAR_CODE_TRIP"
  colnames(LOG)[colnames(LOG)=="FV_GEAR_CODE.y"] <- "GEAR_CODE_SET"
  
  LOG <- merge(LOG, LOG_SPC_STD_INFO_corr[,c("LOG_EFRT_STD_INFO_ID","LOG_SPC_STD_INFO_ID","SSF_LANDED_FORM_CODE_LS","UNIT_OF_MEASURE_ID_LS","WEIGHT_LBS")], all.x=T)
  colnames(LOG)[colnames(LOG)=="MON_DOC_DEFN_ID"] <- "MON_DOC_DEFN_ID_LOG"
  colnames(LOG)[colnames(LOG)=="WEIGHT_LBS"] <- "BFT_LANDED_WEIGHT_LBS"
  LOG <- merge(LOG, tblList$LOG_EFRT_ENTRD_DETS, all.x=T)
  LOG <- merge(LOG, tblList$LOG_SPC_ENTRD_DETS, all.x=T)
  # newer data has these values in the gear column - let's change them to match the original format
  # also, let's settle on single values for each of lat, long, nafo and gear
  LOG <- LOG %>%
    dplyr::mutate(
      latitude = dplyr::coalesce(ENT_LATITUDE, DET_LATITUDE),
      longitude = dplyr::coalesce(ENT_LONGITUDE, DET_LONGITUDE),
      NAFO_UNIT_AREA_ID = dplyr::coalesce(dplyr::coalesce(DET_NAFO_UNIT_AREA_ID, LE_FV_NAFO_UNIT_AREA_ID), MD_FV_NAFO_UNIT_AREA_ID)
    ) %>%
    dplyr::select(-ENT_LATITUDE, -DET_LATITUDE, -ENT_LONGITUDE, -DET_LONGITUDE, -DET_NAFO_UNIT_AREA_ID, -LE_FV_NAFO_UNIT_AREA_ID, -MD_FV_NAFO_UNIT_AREA_ID)

  TRIP <- merge(unique(tblList$MON_DOCS[,c("MON_DOC_ID","MON_DOC_DEFN_ID","TRIP_ID","VR_NUMBER","FV_GEAR_CODE","FV_NAFO_UNIT_AREA_ID")]), 
                tblList$MON_DOC_ENTRD_DETS, by="MON_DOC_ID", all.x = T)

  TRIP <- merge(TRIP, 
                OFFLD_TRIP_dets, by="MON_DOC_ID",all.x = T)
  TRIP <- merge(TRIP, TRIP_b, by = "MON_DOC_ID", all.x = T)
  colnames(TRIP)[colnames(TRIP)=="MON_DOC_DEFN_ID"] <- "MON_DOC_DEFN_ID_TRIP"
  TRIP<- merge(TRIP, tblList$AREAS[,c("AREA_ID", "AREA")], by.x="FV_NAFO_UNIT_AREA_ID", by.y="AREA_ID", all.x = T)
  colnames(TRIP)[colnames(TRIP)=="AREA"] <- "nafo_unit_trip"
  LOG<- merge(LOG, tblList$AREAS[,c("AREA_ID", "AREA")], by.x="NAFO_UNIT_AREA_ID", by.y="AREA_ID", all.x = T)
  colnames(LOG)[colnames(LOG)=="AREA"] <- "nafo_unit_log"

  # MMM 2025 - I came in to fix aspects related to the oracle connection but found 
  # that LOG and TRIP needed some fields renamed for the script to work?
  LOG <- LOG %>%
    dplyr::mutate(
      log_BFT_Tag_Num = BFT_TAG_NUM_,
      Capture_Time = CAPTURE_TIME_,
      flank_length = FLANK_LENGTH_,
      flank_length_uom = FLANK_LENGTH_UOM_,
      dressed_length = DRESSED_LENGTH_,
      dressed_length_uom = DRESSED_LENGTH_UOM_,
      Water_Temp = TEMPERATURE_OF_WATER_,
        Water_Temp_UOM = TEMP_UNIT_OF_MEASURE_DEFAULT_F_)

  TRIP <- TRIP %>%
    dplyr::mutate(    
      Home_Mgt_Area = ATLANTIC_BLUEFIN_TUNA_DOCUMENT_,
      Date_Sailed = DATE_SAILED_79_,
      Time_Sailed = TIME_SAILED_104_,
      Trip_Number = TRIP_NUMBER_,
      Total_Hours_Fished = TOTAL_HOURS_FISHED_742_,
      Num_Of_Strikes = NUM_OF_STRIKES_,
      Hook_Size = HOOK_SIZE_4_,
      Num_Of_Lines = NUM_OF_LINES_,
      Gauge_Of_Mono = GAUGE_OF_MONO_,
      Bait = BAIT_TYPE_,
      Total_Num_BFT_Caught_Trip =  BLUEFIN_CAUGHT_)

  results <- merge(TRIP[,c("MON_DOC_ID", "MON_DOC_DEFN_ID_TRIP", "TRIP_ID", "VR_NUMBER", "Home_Mgt_Area", "Date_Sailed","Time_Sailed", 
                           "LANDING_DATE_TIME", "COMMUNITY_CODE", "Trip_Number", "FV_NAFO_UNIT_AREA_ID", "nafo_unit_trip", "Total_Hours_Fished", "Num_Of_Strikes", 
                           "Hook_Size", "Num_Of_Lines", "Gauge_Of_Mono", "Bait", "Total_Num_BFT_Caught_Trip", "BFT_dressed_weight_lbs_trip", 
                           "BET_dressed_weight_lbs_trip", "YFT_dressed_weight_lbs_trip", "ALB_dressed_weight_lbs_trip")], 
                   LOG[,c("MON_DOC_ID", "LOG_EFRT_STD_INFO_ID", "NAFO_UNIT_AREA_ID", "nafo_unit_log", "log_BFT_Tag_Num", "FV_FISHED_DATETIME", "Capture_Time", "latitude", "longitude", 
                          "flank_length", "flank_length_uom", "dressed_length", "dressed_length_uom", "SSF_LANDED_FORM_CODE_LS", "BFT_LANDED_WEIGHT_LBS", 
                          "Water_Temp", "Water_Temp_UOM","GEAR_CODE_TRIP","GEAR_CODE_SET")], by= "MON_DOC_ID", all.y = T)  
  return(results)
}