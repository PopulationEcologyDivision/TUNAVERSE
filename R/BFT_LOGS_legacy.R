#' @title BFT_LOGS_legacy
#' @description This function recreates a legacy MARFIS view that was used by the
#' Large Pelagics Group at SABs until 2023 when new logbooks affected the completeness
#' of the data extracted. It is recreated here for comparability purposes so that 
#' the data used in analyses prior to 2023 can be compared with data now proided by
#' the BFT_LOGS R function.
#' @param dsn default is \code{'PTRAN 64bit'},  this is a character vector with the DSN name configured on the 
#' local computer allowing communication with the MARFIS tables.
#' @param username default is \code{NULL}
#' @param password default is \code{NULL}
#' @param usepkg default is \code{'rodbc'}
#' @author  Alex Hanke, \email{Alex.Hanke@@dfo-mpo.gc.ca}
#' @importFrom dplyr %>%
#' @export
BFT_LOGS_legacy = function(dsn = "PTRAN 64bit", username=NULL,password=NULL, usepkg='rodbc') {
  channel = Mar.utils::make_oracle_cxn(usepkg = usepkg, fn.oracle.username = username, 
                                       fn.oracle.password = password, 
                                       fn.oracle.dsn = dsn)
  SQL1 <- "SELECT 
trip.mon_doc_id, 
trip.mon_doc_defn_id, 
trip.trip_id,  
logs.log_efrt_std_info_id, 
trip.vr_number,  					                                                                                             -- cfv number
trip.home_mgt_area,  				                                                                                           -- home management area
trip.date_sailed, 					                                                                                           -- date sailed
trip.time_sailed, 					                                                                                           -- time sailed
trip.landing_date_time, 				                                                                                       -- date landed
trip.community_code, 		     		                                                                                       -- port landed (prov,dist,port)
trip.trip_number, 					                                                                                           -- trip number
(select area from marfissci.areas where area_id = (nvl(logs.nafo_unit_area_id,trip.fv_nafo_unit_area_id))) nafo_unit,  -- NAFO Area
to_number(trip.total_hours_fished) total_hours_fished, 		                                                             -- total hours fished
to_number(trip.num_of_strikes) num_of_strikes, 	                                                                        -- no. of strikes
trip.fv_gear_code trip_gear_code, 				                                                                              -- trip level gear type
trip.hook_size, 					                                                                                             -- hook size
to_number(trip.num_of_lines) num_of_lines, 		                                                                         -- number of lines fished
trip.gauge_of_mono, 					                                                                                         -- monofilament gauge
trip.bait, 						                                                                                                 -- bait used
logs.log_BFT_Tag_Num BFT_tag_number, 			                                                                              -- fish tag number
logs.fv_fished_datetime date_fished, 			                                                                              -- date caught
logs.capture_time, 					                                                                                            -- time caught
logs.latitude, 					                                                                                                -- Latitude (Position of fishing)
logs.longitude, 				                                                                                                -- Longitude
logs.set_gear_code,                                                                                                     -- set level gear code
to_number(logs.flank_length)flank_length, 		                                                                          -- Length (bluefin)
logs.flank_length_uom, 				                                                                                         --  Flank Length units of measure 
to_number(logs.dressed_length) dressed_length,   	                                                                      -- Length (bluefin)
logs.dressed_length_uom,  			 	                                                                                     --  Dressed Length units of measure (bluefin)
to_number(trip.total_num_BFT_caught_trip) total_num_BFT_caught_trip, 	                                                  -- total catch/trip (num of fish)
logs.ssf_landed_form_code landed_form_code,  	                                                                          --  landed weight code  (bluefin) /1=Round (lbs), 3=Dressed (lbs)
logs.bft_landed_weight_lbs, 
trip.BFT_dressed_weight_lbs_trip, 	                                                                                    --  total weight/trip (in pounds dressed)  ## Dressed weight (lbs) Bluefin tuna (254)
trip.BET_dressed_weight_lbs_trip, 	                                                                                     --  Dressed weight (lbs) Bigeye tuna (253)
trip.YFT_dressed_weight_lbs_trip, 	                                                                                     --  Dressed weight (lbs) Yellowfin tuna (256)
trip.ALB_dressed_weight_lbs_trip, 	                                                                                     --  Dressed weight (lbs) Albacore tuna (252)
logs.water_temp_UOM, 					                                                                                          -- Water temperature code
logs.water_temp water_temp 			                                                                                        -- Water temperature
FROM  
( -- trip slip weights, all trips even 0 catch    
select 
mon_doc_id, mon_doc_defn_id, trip_id, vr_number, fv_gear_code,FV_NAFO_UNIT_AREA_ID, 
home_mgt_area, date_sailed, time_sailed, trip_number, total_hours_fished, 
num_of_strikes,total_num_BFT_caught_trip,hook_size, num_of_lines, gauge_of_mono, bait, 
to_date(to_char(landing_date_time,'YYYY-MON-DD'),'YYYY-MON-DD') landing_date_time, 
community_code, 
sum(decode(ssf_species_code || ssf_species_size_code || ssf_landed_form_code || unit_of_measure_id, 2549320, weight,2549310, weight * 2.2046, 2549120,weight/1.25,2549110, weight * 2.2046/.25,null)) BFT_dressed_weight_lbs_trip,  	--  bluefin dressed weight lbs
sum(decode(ssf_species_code || ssf_species_size_code || ssf_landed_form_code || unit_of_measure_id, 2539320, weight,2539310, weight * 2.2046,null)) BET_dressed_weight_lbs_trip,  	--  bigeye dressed weight lbs
sum(decode(ssf_species_code || ssf_species_size_code || ssf_landed_form_code || unit_of_measure_id, 2569320, weight,2569310, weight * 2.2046,null)) YFT_dressed_weight_lbs_trip,  	--  yellowfin dressed weight lbs
sum(decode(ssf_species_code || ssf_species_size_code || ssf_landed_form_code || unit_of_measure_id, 2529320, weight,2529310, weight * 2.2046,null)) ALB_dressed_weight_lbs_trip  	--  albacore dressed weight lbs
from 
(       
select 
md.mon_doc_id, md.mon_doc_defn_id, md.trip_id, md.vr_number, md.fv_gear_code,md.FV_NAFO_UNIT_AREA_ID, 
max( decode( mdDet.COLUMN_DEFN_ID,552, mdDet.DATA_VALUE, null)) Home_Mgt_Area, 	                                      --  HOME MANAGEMENT AREA
max( decode( mdDet.COLUMN_DEFN_ID,79 , mdDet.DATA_VALUE, null)) Date_Sailed, 	                                        --  DATE SAILED 
max( decode( mdDet.COLUMN_DEFN_ID,104 , mdDet.DATA_VALUE, null)) Time_Sailed, 	                                       -- TIME SAILED
max( decode( mdDet.COLUMN_DEFN_ID,428 , mdDet.DATA_VALUE, null)) Trip_Number, 	                                       --  TRIP NUMBER 
max( decode( mdDet.COLUMN_DEFN_ID,742, mdDet.DATA_VALUE, null)) Total_Hours_Fished, 	                                 -- TOTAL HOURS FISHED
max( decode( mdDet.COLUMN_DEFN_ID,43 , mdDet.DATA_VALUE, null)) Num_Of_Strikes, 	                                     --  NO. OF STRIKES
max( decode( mdDet.COLUMN_DEFN_ID,477 , mdDet.DATA_VALUE, null)) Total_Num_BFT_Caught_Trip,                             --  TOTAL CATCH/TRIP
max( decode( mdDet.COLUMN_DEFN_ID,4 , mdDet.DATA_VALUE, null)) Hook_Size, 		                                        --  HOOK SIZE
max( decode( mdDet.COLUMN_DEFN_ID,480 , mdDet.DATA_VALUE, null)) Num_Of_Lines, 	                                      --  NUMBER OF LINES FISHED
max( decode( mdDet.COLUMN_DEFN_ID,481 , mdDet.DATA_VALUE, null)) Gauge_Of_Mono, 	                                     --  MONOFILAMENT GUAGE
max( decode( mdDet.COLUMN_DEFN_ID,53 , mdDet.DATA_VALUE, null)) Bait, 		                                            --  BAIT
so.landing_date_time, 		                                                                                            -- DATE LANDED TIME LANDED
so.community_code, 		                                                                                                 -- PORT LANDED
ss.ssf_species_code, ss.ssf_species_size_code, ss.ssf_landed_form_code, ss.unit_of_measure_id,  
ss.weight 
from 
marfissci.mon_docs md, 	
marfissci.mon_doc_entrd_dets mdDet, 
marfissci.slip_offld_std_info so,  
marfissci.slip_buyr_std_info sb,  
marfissci.slip_spc_std_info ss 
where 
md.mon_doc_defn_id IN (3,22,58,4,5) 
and md.mon_doc_id = mdDet.mon_doc_id(+) 
and md.mon_doc_id = so.mon_doc_id(+) 
and so.slip_offld_std_info_id = sb.slip_offld_std_info_id(+) 
and sb.slip_buyr_std_info_id = ss.slip_buyr_std_info_id(+) 
group by 
md.mon_doc_id, md.mon_doc_defn_id, md.trip_id, md.vr_number, md.fv_gear_code,md.FV_NAFO_UNIT_AREA_ID, 
so.landing_date_time, so.community_code, 
ss.ssf_species_code, ss.ssf_species_size_code, ss.ssf_landed_form_code, ss.unit_of_measure_id,  
ss.weight        
)         
group by 
mon_doc_id, mon_doc_defn_id, trip_id, vr_number, fv_gear_code,FV_NAFO_UNIT_AREA_ID, 
Home_Mgt_Area, Date_Sailed, Time_Sailed, Trip_Number, Total_Hours_Fished, 
Num_Of_Strikes,Total_Num_BFT_Caught_Trip,Hook_Size, Num_Of_Lines, Gauge_Of_Mono, Bait, 
to_char(landing_date_time,'YYYY-MON-DD'), community_code       
) trip, 
( --  log  
select 
md.mon_doc_id, md.mon_doc_defn_id, le.log_efrt_std_info_id, 
nvl(le.ent_latitude, le.det_latitude) latitude, 
nvl(le.ent_longitude, le.det_longitude) longitude, 
nvl( (nvl(le.DET_NAFO_UNIT_AREA_ID,le.FV_NAFO_UNIT_AREA_ID)),md.FV_NAFO_UNIT_AREA_ID) NAFO_UNIT_AREA_ID, 
le.fv_fished_datetime, 		                                                                                                   --  DATE CAUGHT
ls.ssf_landed_form_code, 
le.fv_gear_code set_gear_code,  
decode( ls.unit_of_measure_id, 20, ls.weight, 10, ls.weight * 2.2046, null) bft_landed_weight_lbs,                                                      -- bluefin  weightlbs
(select data_value from marfissci.log_efrt_entrd_dets where log_efrt_std_info_id = le.log_efrt_std_info_id and column_defn_id=689) Capture_Time, 		 --  TIME CAUGHT
(select data_value from marfissci.log_efrt_entrd_dets where log_efrt_std_info_id = le.log_efrt_std_info_id and column_defn_id=559)  Water_Temp_UOM, 	 --  WATER TEMPERATURE CODE
(select data_value from marfissci.log_efrt_entrd_dets where log_efrt_std_info_id = le.log_efrt_std_info_id and column_defn_id=558) Water_Temp, 	 	      --  WATER TEMPERATURE
(select data_value from marfissci.log_spc_entrd_dets where log_spc_std_info_id = ls.log_spc_std_info_id and column_defn_id=484) log_BFT_Tag_Num, 	    	--  FISH TAG NUMBER
(select data_value from marfissci.log_spc_entrd_dets where log_spc_std_info_id = ls.log_spc_std_info_id and column_defn_id=743)         flank_length, 		--  FLANK LENGTH
(select data_value from marfissci.log_spc_entrd_dets where log_spc_std_info_id = ls.log_spc_std_info_id and column_defn_id=807)         flank_length_uom, 		--  FLANK LENGTH UOM
(select data_value from marfissci.log_spc_entrd_dets where log_spc_std_info_id = ls.log_spc_std_info_id and column_defn_id=744)         dressed_length, 		--  DRESSED LENGTH
(select data_value from marfissci.log_spc_entrd_dets where log_spc_std_info_id = ls.log_spc_std_info_id and column_defn_id=808)         dressed_length_uom	 	--  DRESSED LENGTH uom
from 
marfissci.mon_docs md, 	
marfissci.log_efrt_std_info le,  
marfissci.log_spc_std_info ls 
where 
md.mon_doc_defn_id IN (3,22,58,4,5) AND 
md.mon_doc_id = le.mon_doc_id AND 
ls.ssf_species_code = 254 AND 
le.log_efrt_std_info_id = ls.log_efrt_std_info_id(+) 
) logs 
WHERE 
trip.mon_doc_id = logs.mon_doc_id(+)"
  
  results = channel$thecmd(channel$channel, SQL1)
  

  return(results)
}