#' @title LARGE_PELAGICS_SLIPS_legacy
#' @description This function recreates a legacy MARFIS view that is used by the
#' Large Pelagics Group at SABs. This version is more inclusive in that it includes 
#' all monitoring documents the report catching typical large pelagic species.
#' "SWORDFISH"                 "TUNA, ALBACORE"           
#' "TUNA, BIGEYE"              "TUNA, BLUEFIN"            
#' "TUNA, YELLOWFIN"           "MARLIN BLUE"              
#' "MARLIN WHITE"              "MAHI MAHI (DOLPHIN)"      
#' "ESCOLAR"                   "PELAGIC, UNSPECIFIED"     
#' "SHARK, PORBEAGLE/MACKEREL" "SHARK, BLUE"              
#' "SHARK, MAKO"               "CRAB, PORCUPINE"  
#'  The extracted data has most field names in common with the extracted data
#'  returned by the LARGE_PELAGICS_LOGS_legacy function.
#' The units of weight are consistently kgs and represent the trip total estimated 
#' round weight by species etc.
#' @param cxn A valid Oracle connection object. This parameter allows you to 
#' pass an existing connection, reducing the need to establish a new connection 
#' within the function. If provided, it takes precedence over the connection-
#' related parameters.
#' @author  Alex Hanke, \email{Alex.Hanke@@dfo-mpo.gc.ca}
#' @export
LARGE_PELAGICS_LOGS_legacy = function(cxn=NULL) {
  thecmd <- Mar.utils::connectionCheck(cxn)
  SQL2 = "SELECT
    slips.mon_doc_id,
    slips.trip_id,
    slips.vr_number,
    v.vessel_name,
    CAST(NULL AS NUMBER)              AS log_efrt_std_info_id,
    slips.licence_id                  AS licence_number,
    l.species_code                    AS target_species,     
    TO_NUMBER(REGEXP_SUBSTR(slips.trip_number, '^[0-9]+$')) AS trip_number,
    slips.date_sailed,
    -- slips.port_sailed,
    -- slips.port_landed,
    slips.nafo_unit,
    slips.BFT_tag_num                 AS BFT_tag_number,
    slips.ssf_species_code            AS species_code,
    -- slips.ssf_species_size_code       AS species_size_code, -- Added column
    CAST(10 AS NUMBER)                AS catch_usage_code,
    slips.ssf_landed_form_code        AS landed_form_code,  -- Added column
    ssf.desc_eng                      AS comments,
    -- fac.form_factor,                                        -- Added conversion factor
    slips.fv_gear_code                AS gear_code,
    slips.landing_date_time           AS date_landed,
    TO_CHAR(slips.landing_date_time, 'YYYY') AS year_caught,
    slips.num_of_fish_offld           AS num_landed,
    -- slips.weight_offld_kgs,
    ROUND(slips.weight_offld_kgs * NVL(fac.form_factor, 1), 2) AS rnd_weight_kg -- Calculated round weight
FROM (
    SELECT
        mon_doc_id,
        trip_id,
        vr_number,
        ssf_species_size_code,
        ssf_landed_form_code,
        licence_id, 
        trip_number,
        date_sailed,
        port_sailed,
        port_landed,
        nafo_unit,
        fv_gear_code,
        landing_date_time,
        NVL(BFT_tag_num, '0') AS BFT_tag_num,
        ssf_species_code,
        CASE 
            WHEN ssf_species_code = 254 THEN 1 
            ELSE TO_NUMBER(REGEXP_SUBSTR(num_of_fish, '^[0-9]+$')) 
        END AS num_of_fish_offld,
        weight_kgs AS weight_offld_kgs
    FROM (
        SELECT
            md.mon_doc_id,
            md.trip_id,
            md.vr_number,
            ss.ssf_species_size_code,
            ss.ssf_landed_form_code,
            mdlic.licence_id,
            md.fv_gear_code,
            (SELECT data_value FROM MARFISSCI.mon_doc_entrd_dets WHERE mon_doc_id = md.mon_doc_id AND column_defn_id = 428 AND ROWNUM = 1) AS trip_number,
            (SELECT data_value FROM MARFISSCI.mon_doc_entrd_dets WHERE mon_doc_id = md.mon_doc_id AND column_defn_id = 12 AND ROWNUM = 1) AS date_sailed,
            (SELECT data_value FROM MARFISSCI.mon_doc_entrd_dets WHERE mon_doc_id = md.mon_doc_id AND column_defn_id = 615 AND ROWNUM = 1) AS port_sailed,
            (SELECT data_value FROM MARFISSCI.mon_doc_entrd_dets WHERE mon_doc_id = md.mon_doc_id AND column_defn_id = 668 AND ROWNUM = 1) AS port_landed,
            (SELECT data_value FROM MARFISSCI.mon_doc_entrd_dets WHERE mon_doc_id = md.mon_doc_id AND column_defn_id = 14 AND ROWNUM = 1) AS nafo_unit,
            so.landing_date_time, 
            ss.slip_spc_std_info_id,
            CASE ss.unit_of_measure_id
                WHEN 10 THEN ss.weight           -- Kilograms
                WHEN 20 THEN ss.weight / 2.2046  -- Pounds to Kgs
                WHEN 30 THEN ss.weight * 1000    -- Metric Tonnes to Kgs
                WHEN 40 THEN ss.weight * 15.876  -- Bags to Kgs
                ELSE ss.weight                   -- Default fallback
            END AS weight_kgs,
            ss.ssf_species_code,
            (SELECT data_value FROM MARFISSCI.slip_spc_entrd_dets WHERE slip_spc_std_info_id = ss.slip_spc_std_info_id AND column_defn_id = 45 AND ROWNUM = 1) AS num_of_fish,
            (SELECT data_value FROM MARFISSCI.slip_spc_entrd_dets WHERE slip_spc_std_info_id = ss.slip_spc_std_info_id AND column_defn_id = 484 AND ROWNUM = 1) AS BFT_tag_num
        FROM MARFISSCI.slip_spc_std_info ss
        JOIN MARFISSCI.mon_doc_lics mdlic ON mdlic.mon_doc_lic_id = ss.mon_doc_lic_id
        JOIN MARFISSCI.mon_docs md ON md.mon_doc_id = mdlic.mon_doc_id
        LEFT JOIN MARFISSCI.slip_buyr_std_info sb ON sb.slip_buyr_std_info_id = ss.slip_buyr_std_info_id
        LEFT JOIN MARFISSCI.slip_offld_std_info so ON so.slip_offld_std_info_id = sb.slip_offld_std_info_id
        WHERE md.mon_doc_defn_id > 0
          AND ss.ssf_species_code IN (262, 253, 254, 251, 256, 252, 261, 263, 299, 369, 372, 375, 270, 778, 830, 29, 710, 730)
    )
) slips
LEFT JOIN MARFISSCI.vessels v ON v.vr_number = slips.vr_number
LEFT JOIN MARFISSCI.licences l ON l.licence_id = slips.licence_id
LEFT JOIN MARFISSCI.specie_size_forms ssf ON ssf.species_code = slips.ssf_species_code 
    AND ssf.species_size_code = slips.ssf_species_size_code 
    AND ssf.landed_form_code = slips.ssf_landed_form_code
LEFT JOIN MARFISSCI.ssf_factors fac ON fac.ssf_species_code = slips.ssf_species_code
    AND fac.ssf_species_size_code = slips.ssf_species_size_code
    AND fac.ssf_landed_form_code = slips.ssf_landed_form_code"

MARFISLIPS = thecmd(cxn, SQL2)
save(MARFISLIPS, file="R:/ATLSiteShares/SABS/LargePelagics/COMMERCIAL DATA/TUNAVERSE/data/Large_Pelagics_Slips.Rdata")
return(results)

}
