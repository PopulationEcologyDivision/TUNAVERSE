#' @title LARGE_PELAGICS_LOGS_legacy
#' @description This function recreates a legacy MARFIS view that is used by the
#' Large Pelagics Group at SABs. This version is more robust in that it is able to 
#' address incorrect data format issues for some fields. It is the responsibility
#' of the analyst to find the inconsistent formats and correct them.
#' @param cxn A valid Oracle connection object. This parameter allows you to 
#' pass an existing connection, reducing the need to establish a new connection 
#' within the function. If provided, it takes precedence over the connection-
#' related parameters.
#' @author  Alex Hanke, \email{Alex.Hanke@@dfo-mpo.gc.ca}
#' @export
LARGE_PELAGICS_LOGS_legacy = function(cxn=NULL) {
  thecmd <- Mar.utils::connectionCheck(cxn)
  SQL1 <- "SELECT trips.trip_id,
          nvl(logs.mon_doc_id,trips.mon_doc_id) MON_DOC_ID,                                           -- previously pounded out
          logs.log_efrt_std_info_id log_efrt_std_info_id,
          nvl(logs.vr_number,trips.vr_number) VR_NUMBER, 
          (select vessel_name from marfissci.vessels where vr_number = logs.vr_number) vessel_name,   -- Boat_name
          logs.licence_id licence_number, 					                                        -- Licence Id
          (select species_code from marfissci.licences where licence_id = logs.licence_id) target_species,    
          logs.trip_number trip_number, 			                                                    --Trip Number
          trips.fishery_code  fishery_code,                                                           -- Discard matchfishery code
          logs.home_mgt_area, 							                                            -- home management area
          logs.gang_len_fm gang_len_fm, 			                                                    -- Ganging length (fm) (999 if unknown)
          logs.hook_size hook_size,                                                                   -- Hook size (99 if unknown)
          logs.hook_make hook_make, 		                                                            -- Hook make
          logs.hook_type hook_type, 		                                                            -- Hook type
          logs.dist_hooks_fm dist_hooks_fm,                                                           -- Distance between hooks (fms, 99 if unknown)
          logs.total_hours_fished total_hours_fished,                                                 -- total hours fished
          logs.num_of_strikes num_of_strikes,                                                         -- no. of strikes
          logs.num_of_lines num_of_lines,                                                             -- number of lines fished
          logs.date_sailed,							                                                -- date sailed
          logs.time_sailed,							                                                -- time sailed
          (select area from marfissci.areas where area_id = logs.nafo_unit_area_id) nafo_unit,        -- NAFO Area
          logs.gauge_of_mono,                                                                         -- monofilament gauge
          logs.BFT_tag_num BFT_tag_number,                                                            -- fish tag number
          logs.ssf_species_code species_code,
          logs.fv_gear_code gear_code,                                                                -- gear type
          logs.bait,                                                                                  -- Bait type (Stats Branch codes)
          logs.ssf_landed_form_code landed_form_code,                                                 -- Condition Landed
          logs.catch_usage_code,
          trips.landing_date_time date_landed,                                                        -- Date Landed
          logs.fv_fished_datetime date_caught,                                                        -- Date of set
          to_number(to_char(logs.fv_fished_datetime,'YYYY')) year_caught,                             -- year fished
          to_number(to_char(logs.fv_fished_datetime,'MM')) month_caught,                              -- month fished
          to_number(to_char(logs.fv_fished_datetime,'DD')) day_caught,                                -- day fished
          to_char(logs.latitude*.01) latitude_caught,	                                                -- Latitude
          to_char(logs.longitude*.01) longitude_caught,                                               -- Longitude
          -- Problems with data in these columns
          to_char(logs.end_latitude)  end_latitude,                                                   -- Latitude
          to_char(logs.end_longitude) end_longitude,                                                  -- Longitude
          --##########################################
          logs.time_started,                                                                          -- Time at start of set
          logs.surf_temp_f surface_temp_f,                                                            -- Surface temp (?F)
          logs.bot_depth_fm bot_depth_fm,                                                             -- Depth to bottom (fm)
          logs.hook_depth_fm hook_depth_fm,                                                           -- Depth to hook (fm)
          logs.num_of_hooks num_of_hooks,                                                             -- No of hooks
          logs.flank_length,
          logs.dressed_length,
          logs.num_of_fish_landed num_landed,                                                         -- No of fish caught daily
          logs.weight_landed_pds,					                                                    -- Weight of fish caught daily (lbs dressed)
          logs.weight_landed_kgs,					                                                    -- Weight of fish caught daily (kgs dressed)
          logs.slip_weight_kgs_per_effort,
          logs.slip_weight_kgs_per_species,
          logs.weight_of_bait,                                                                        -- added this to the table
          logs.comments								                                                -- Comments
          FROM
          ( -- trips
          SELECT
          md.trip_id, md.mon_doc_id,md.vr_number,
          (SELECT min(fishery_code) from marfissci.discard_matchfishery df where md.trip_id = df.trip_id) fishery_code, 
          TO_CHAR(so.landing_date_time, 'YYYY-MM-DD HH24:MI:SS') landing_date_time		                                                                -- DATE LANDED TIME LANDED
          FROM
          marfissci.mon_docs md,	
          marfissci.slip_offld_std_info so 
          WHERE
          --########### remember to change this too!
          md.mon_doc_defn_id > 0                                                                      -- 5 = SWORDFISH/SHARK LONGLINE MONITORING
          --##### ##########
          AND md.mon_doc_id = so.mon_doc_id(+)
          GROUP BY md.trip_id,
          md.mon_doc_id,md.vr_number,
          so.landing_date_time
          ) TRIPS,
          ( --logs
          SELECT
          mon_doc_id, vr_number,log_efrt_std_info_id, SSF_SPECIES_SIZE_CODE, SSF_LANDED_FORM_CODE, licence_id,
          to_char(latitude) latitude, to_char(longitude) longitude,
          nafo_unit_area_id, fv_gear_code, 
          (select data_value from marfissci.mon_doc_entrd_dets where mon_doc_id = DETS.mon_doc_id and column_defn_id = 552) Home_Mgt_Area,	  --HOME MANAGEMENT AREA
          (select data_value from marfissci.mon_doc_entrd_dets where mon_doc_id = DETS.mon_doc_id and column_defn_id = 428) trip_number,
          (select data_value from marfissci.mon_doc_entrd_dets where mon_doc_id = DETS.mon_doc_id and column_defn_id = 47) gang_len_fm,
          (select data_value from marfissci.mon_doc_entrd_dets where mon_doc_id = DETS.mon_doc_id and column_defn_id = 4) hook_size,
          (select data_value from marfissci.mon_doc_entrd_dets where mon_doc_id = DETS.mon_doc_id and column_defn_id = 555) hook_make,
          (select data_value from marfissci.mon_doc_entrd_dets where mon_doc_id = DETS.mon_doc_id and column_defn_id = 5) hook_type,
          (select data_value from marfissci.mon_doc_entrd_dets where mon_doc_id = DETS.mon_doc_id and column_defn_id = 48) dist_hooks_fm,
          (select data_value from marfissci.mon_doc_entrd_dets where mon_doc_id = DETS.mon_doc_id and column_defn_id = 104) time_sailed,
          (select data_value from marfissci.mon_doc_entrd_dets where mon_doc_id = DETS.mon_doc_id and column_defn_id = 12) date_sailed,
          (select data_value from marfissci.mon_doc_entrd_dets where mon_doc_id = DETS.mon_doc_id and column_defn_id = 481) Gauge_Of_Mono,        -- MONOFILAMENT GUAGE
          (select data_value from marfissci.mon_doc_entrd_dets where mon_doc_id = DETS.mon_doc_id and column_defn_id = 742) Total_Hours_Fished,   -- TOTAL HOURS FISHED
          (select data_value from marfissci.mon_doc_entrd_dets where mon_doc_id = DETS.mon_doc_id and column_defn_id = 43) Num_Of_Strikes,	    --  NO. OF STRIKES
          (select data_value from marfissci.mon_doc_entrd_dets where mon_doc_id = DETS.mon_doc_id and column_defn_id = 480) Num_Of_Lines,         -- NUMBER OF LINES FISHED
          (select data_value from marfissci.mon_doc_entrd_dets where mon_doc_id = DETS.mon_doc_id and column_defn_id = 482) weight_Of_bait,	    -- WEIGHT OF BAIT
          fv_fished_datetime, time_started,surf_temp_f,bot_depth_fm,hook_depth_fm,num_of_hooks,
          num_of_bouys,comments,bait,
          -- ##### Problems with data in these columns
          end_longitude,end_latitude,
          -- ########################################
          decode(BFT_tag_num,NULL,0,BFT_tag_num) BFT_tag_num,
          ssf_species_code,
          catch_usage_code,
          flank_length,
          dressed_length,
          (decode(ssf_species_code, 254,1,num_of_fish)) num_of_fish_landed,
          weight_pds weight_landed_pds,
          weight_kgs weight_landed_kgs,
          slip_weight_kgs_per_effort,
          slip_weight_kgs_per_species
          FROM
          (  -- log detail
          SELECT
          md.mon_doc_id, md.vr_number,le.log_efrt_std_info_id, ls.SSF_SPECIES_SIZE_CODE, ls.SSF_LANDED_FORM_CODE, mdLic.licence_id,
          nvl(le.ent_latitude,le.det_latitude) latitude, nvl(le.fv_gear_code,md.fv_gear_code) fv_gear_code,
          nvl(le.ent_longitude,le.det_longitude) longitude,
          nvl( (nvl(le.DET_NAFO_UNIT_AREA_ID,le.FV_NAFO_UNIT_AREA_ID)),md.FV_NAFO_UNIT_AREA_ID) NAFO_UNIT_AREA_ID,
          le.fv_fished_datetime,
          (select data_value from marfissci.log_efrt_entrd_dets where log_efrt_std_info_id = le.log_efrt_std_info_id and column_defn_id=41) time_started,
          (select data_value from marfissci.log_efrt_entrd_dets where log_efrt_std_info_id = le.log_efrt_std_info_id and column_defn_id=39) surf_temp_f,
          (select data_value from marfissci.log_efrt_entrd_dets where log_efrt_std_info_id = le.log_efrt_std_info_id and column_defn_id=49) bot_depth_fm,
          (select data_value from marfissci.log_efrt_entrd_dets where log_efrt_std_info_id = le.log_efrt_std_info_id and column_defn_id=50 AND  log_efrt_std_info_id NOT IN (1567679, 1560738, 1557115))  hook_depth_fm,
          (select data_value from marfissci.log_efrt_entrd_dets where log_efrt_std_info_id = le.log_efrt_std_info_id and column_defn_id=51) num_of_hooks,
          (select data_value from marfissci.log_efrt_entrd_dets where log_efrt_std_info_id = le.log_efrt_std_info_id and column_defn_id=52) num_of_bouys,
          (select data_value from marfissci.log_efrt_entrd_dets where log_efrt_std_info_id = le.log_efrt_std_info_id and column_defn_id=196) comments,
          -- ##### Problems with data in these columns
          to_char((select data_value from marfissci.log_efrt_entrd_dets where log_efrt_std_info_id = le.log_efrt_std_info_id and column_defn_id = 813)) end_latitude,
          to_char((select data_value from marfissci.log_efrt_entrd_dets where log_efrt_std_info_id = le.log_efrt_std_info_id and column_defn_id = 814)) end_longitude,
          -- #########################################
          (select data_value from marfissci.log_efrt_entrd_dets where log_efrt_std_info_id = le.log_efrt_std_info_id and column_defn_id = 53) bait,
          ls.ssf_species_code,
          ls.catch_usage_code,
          round(decode(ls.unit_of_measure_id,10,ls.weight*2.2046,ls.weight)) weight_pds,
          round(decode(ls.unit_of_measure_id,20,ls.weight/2.2046,ls.weight)) weight_kgs,
          (select sum(rnd_weight_kgs) from marfissci.pro_spc_info where log_efrt_std_info_id = le.log_efrt_std_info_id and
          species_code =ls.ssf_species_code) slip_weight_kgs_per_effort,
          (select sum(rnd_weight_kgs) from marfissci.pro_spc_info where species_code =ls.ssf_species_code
          and trip_id = md.trip_id) slip_weight_kgs_per_species,
          (select data_value from marfissci.log_spc_entrd_dets where log_spc_std_info_id = ls.log_spc_std_info_id and column_defn_id=743) flank_length,
          (select data_value from marfissci.log_spc_entrd_dets where log_spc_std_info_id = ls.log_spc_std_info_id and column_defn_id=744) dressed_length,
          (select data_value from marfissci.log_spc_entrd_dets where log_spc_std_info_id = ls.log_spc_std_info_id and column_defn_id=45)  num_of_fish,
          (select data_value from marfissci.log_spc_entrd_dets where log_spc_std_info_id = ls.log_spc_std_info_id and column_defn_id=484) BFT_tag_num -- FISH TAG NUMBER
          FROM
          marfissci.log_efrt_std_info le, marfissci.log_spc_std_info ls, marfissci.mon_doc_lics mdLic,marfissci.mon_docs md
          WHERE
          -- ########### remember to change this
          md.mon_doc_defn_id > 0 	--5 = SWORDFISH/SHARK LONGLINE MONITORING
          -- ########################################
          AND md.mon_doc_id = le.mon_doc_id
          AND mdLic.mon_doc_lic_id (+) = ls.mon_doc_lic_id
          AND le.log_efrt_std_info_id = ls.log_efrt_std_info_id(+)
          ) DETS
           ) LOGS
          WHERE
          trips.mon_doc_id = logs.mon_doc_id(+)
          -- ###### Limit the years
          AND extract(year from logs.fv_fished_datetime) > 2002    -- this is everything!"
  
  results =thecmd(cxn, SQL1)
  
  return(results)
}