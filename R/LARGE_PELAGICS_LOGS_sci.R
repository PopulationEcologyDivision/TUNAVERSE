#' @title LARGE_PELAGICS_LOGS_sci
#' @description Extracts MARFISSCI large pelagics and pelagic/groundfish fishing data
#' at the set-and-species level of resolution (`LOG_EFRT_STD_INFO_ID` and species
#' per `TRIP_ID`), returning round/reported weights, effort, spatial, timing,
#' gear/rigging, catch usage, landed form, target species, tag IDs, logbook
#' document definition ID, coalesced detail fields across the specified
#' `COLUMN_DEFN_ID`s, and multi-source fields suffixed by data origin.
#' @param cxn A valid Oracle connection object. This parameter allows you to
#' pass an existing connection, reducing the need to establish a new connection
#' within the function. If provided, it takes precedence over the connection-
#' related parameters.
#' @param min_year Optional integer minimum fishing year filter applied to
#' `COALESCE(LOG_EFRT_STD_INFO.FV_FISHED_DATETIME, SLIP_OFFLD_STD_INFO.LANDING_DATE_TIME)`.
#' Defaults to `NULL` (no year filter; use `min_year = 2003` to match legacy
#' `extract(year from fv_fished_datetime) > 2002`).
#' @param mondoc_type Character string specifying which monitoring document
#' definitions to extract. `"ALL"` (default) includes all document definitions
#' (`MON_DOC_DEFN_ID > 0`); `"TASK1_fleets"` restricts extraction to
#' `task1_doc_defns` (`1, 2, 3, 4, 5, 6, 9, 10, 14, 19, 28, 29, 33, 58`).
#' @param task1_doc_defns Integer vector of `MON_DOC_DEFN_ID` values used when
#' `mondoc_type = "TASK1_fleets"`. Defaults to
#' `c(1L, 2L, 3L, 4L, 5L, 6L, 9L, 10L, 14L, 19L, 28L, 29L, 33L, 58L)`.
#' @param save_rdata Logical; if `TRUE` (default), saves `LPL` to
#' `R:/ATLSiteShares/SABS/LargePelagics/COMMERCIAL DATA/TUNAVERSE/data/Large_Pelagics_logs_sci_<mondoc_type>_<min_year>.Rdata`
#' (e.g., `Large_Pelagics_logs_sci_ALL_all_years.Rdata` when `min_year = NULL`,
#' or `Large_Pelagics_logs_sci_TASK1_fleets_from_2003.Rdata` when
#' `min_year = 2003`).
#' @return A `data.frame` of set-and-species fishing records from `MARFISSCI`.
#' @author Alex Hanke, \email{Alex.Hanke@@dfo-mpo.gc.ca}
#' @export
LARGE_PELAGICS_LOGS_sci <- function(cxn = NULL,
                                    min_year = NULL,
                                    mondoc_type = c("ALL", "TASK1_fleets"),
                                    task1_doc_defns = c(
                                      1L, 2L, 3L, 4L, 5L, 6L, 9L,
                                      10L, 14L, 19L, 28L, 29L, 33L, 58L
                                    ),
                                    save_rdata = TRUE) {
  thecmd <- Mar.utils::connectionCheck(cxn)
  mondoc_type <- match.arg(mondoc_type)
  
  doc_defn_filter <- "WHERE md.mon_doc_defn_id > 0"
  if (identical(mondoc_type, "TASK1_fleets")) {
    if (is.null(task1_doc_defns) || length(task1_doc_defns) == 0) {
      stop("`task1_doc_defns` must be non-empty when `mondoc_type = \"TASK1_fleets\"`.")
    }
    defns_sql <- paste(unique(as.integer(task1_doc_defns)), collapse = ", ")
    doc_defn_filter <- paste0("WHERE md.mon_doc_defn_id IN (", defns_sql, ")")
  }
  
  year_filter <- ""
  if (!is.null(min_year)) {
    year_filter <- paste0(
      " AND EXTRACT(YEAR FROM COALESCE(le.fv_fished_datetime, sld.date_landed_slip)) >= ",
      as.integer(min_year)
    )
  }
  
  all_det_ids <- c(
    767, 768, 775, 786, 804, 805, 666, 682, 685, 689, 694, 698, 699, 703, 704,
    706, 707, 708, 730, 732, 738, 742, 741, 743, 744, 745, 758, 606, 608, 57,
    41, 527, 21, 106, 12, 79, 59, 46, 45, 39, 3, 54, 552, 553, 557, 558, 559,
    196, 188, 167, 43, 143, 169, 51, 47, 48, 50, 110, 52, 53, 136, 1, 34, 28,
    177, 178, 477, 425, 427, 428, 430, 433, 479, 480, 481, 482, 484, 488, 93,
    159, 124, 9, 158, 104, 111, 2, 18, 192, 193, 459, 55, 14, 13, 49, 5, 107,
    37, 838, 807, 813, 808, 814, 817, 834, 835, 946, 947, 948, 949, 950, 951,
    952
  )
  ids_sql <- paste(unique(as.integer(all_det_ids)), collapse = ", ")
  
  sql_query <- paste0("
    WITH slip_landing_dates AS (
      SELECT
        mon_doc_id,
        MAX(landing_date_time) AS date_landed_slip
      FROM marfissci.slip_offld_std_info
      GROUP BY mon_doc_id
    ),
    qualifying_sets AS (
      SELECT DISTINCT
        md.mon_doc_id,
        md.trip_id,
        le.log_efrt_std_info_id
      FROM marfissci.mon_docs md
      JOIN marfissci.log_efrt_std_info le
        ON md.mon_doc_id = le.mon_doc_id
      LEFT JOIN slip_landing_dates sld
        ON md.mon_doc_id = sld.mon_doc_id
      ", doc_defn_filter,
                      year_filter,
                      "
    ),
    qualifying_mon_docs AS (
      SELECT DISTINCT mon_doc_id
      FROM qualifying_sets
    ),
    qualifying_trips AS (
      SELECT DISTINCT trip_id
      FROM qualifying_sets
      WHERE trip_id IS NOT NULL
    ),
    qualifying_spc AS (
      SELECT DISTINCT ls.log_spc_std_info_id
      FROM qualifying_sets qs
      JOIN marfissci.log_spc_std_info ls
        ON qs.log_efrt_std_info_id = ls.log_efrt_std_info_id
    ),
    vessels_lookup AS (
      SELECT
        vr_number,
        MAX(vessel_name) AS vessel_name_vsl
      FROM marfissci.vessels
      WHERE vr_number IS NOT NULL
      GROUP BY vr_number
    ),
    hail_in_lookup AS (
      SELECT
        hic.trip_id,
        MAX(hic.conf_number) AS hail_in_conf_coal
      FROM marfissci.hail_in_calls hic
      JOIN qualifying_trips qt
        ON hic.trip_id = qt.trip_id
      WHERE hic.conf_number IS NOT NULL
      GROUP BY hic.trip_id
    ),
    md_det_vals AS (
      SELECT
        mded.mon_doc_id,
        MAX(CASE WHEN column_defn_id = 767 THEN data_value END) AS port_sailed_767_det,
        MAX(CASE WHEN column_defn_id = 768 THEN data_value END) AS gear_type_768_det,
        MAX(CASE WHEN column_defn_id = 775 THEN data_value END) AS gear_type_775_det,
        MAX(CASE WHEN column_defn_id = 786 THEN data_value END) AS hours_fished_786_det,
        MAX(CASE WHEN column_defn_id = 804 THEN data_value END) AS spc_sought_804_det,
        MAX(CASE WHEN column_defn_id = 805 THEN data_value END) AS gear_desc_805_det,
        MAX(CASE WHEN column_defn_id = 666 THEN data_value END) AS hail_out_conf_666_det,
        MAX(CASE WHEN column_defn_id = 682 THEN data_value END) AS end_time_682_det,
        MAX(CASE WHEN column_defn_id = 685 THEN data_value END) AS start_time_685_det,
        MAX(CASE WHEN column_defn_id = 689 THEN data_value END) AS capture_time_689_md_det,
        MAX(CASE WHEN column_defn_id = 694 THEN data_value END) AS depth_694_det,
        MAX(CASE WHEN column_defn_id = 698 THEN data_value END) AS nafo_unit_area_698_det,
        MAX(CASE WHEN column_defn_id = 699 THEN data_value END) AS num_strikes_699_det,
        MAX(CASE WHEN column_defn_id = 703 THEN data_value END) AS soak_days_703_det,
        MAX(CASE WHEN column_defn_id = 704 THEN data_value END) AS uom_704_det,
        MAX(CASE WHEN column_defn_id = 706 THEN data_value END) AS depth_fm_706_det,
        MAX(CASE WHEN column_defn_id = 707 THEN data_value END) AS soak_time_hrs_707_det,
        MAX(CASE WHEN column_defn_id = 708 THEN data_value END) AS port_departed_708_det,
        MAX(CASE WHEN column_defn_id = 730 THEN data_value END) AS gear_type_730_det,
        MAX(CASE WHEN column_defn_id = 732 THEN data_value END) AS harp_and_ll_732_det,
        MAX(CASE WHEN column_defn_id = 738 THEN data_value END) AS hours_fished_738_det,
        MAX(CASE WHEN column_defn_id = 742 THEN data_value END) AS total_hours_fished_742_det,
        MAX(CASE WHEN column_defn_id = 741 THEN data_value END) AS observer_trip_code_741_det,
        MAX(CASE WHEN column_defn_id = 743 THEN data_value END) AS flank_length_743_md_det,
        MAX(CASE WHEN column_defn_id = 744 THEN data_value END) AS dressed_length_744_md_det,
        MAX(CASE WHEN column_defn_id = 745 THEN data_value END) AS search_start_time_745_det,
        MAX(CASE WHEN column_defn_id = 758 THEN data_value END) AS depth_uom_fm_758_det,
        MAX(CASE WHEN column_defn_id = 606 THEN data_value END) AS gillnet_type_606_det,
        MAX(CASE WHEN column_defn_id = 608 THEN data_value END) AS soak_time_min_608_det,
        MAX(CASE WHEN column_defn_id = 57 THEN data_value END) AS depth_57_det,
        MAX(CASE WHEN column_defn_id = 41 THEN data_value END) AS time_started_41_md_det,
        MAX(CASE WHEN column_defn_id = 527 THEN data_value END) AS date_fished_527_det,
        MAX(CASE WHEN column_defn_id = 21 THEN data_value END) AS obs_onboard_21_det,
        MAX(CASE WHEN column_defn_id = 106 THEN data_value END) AS nafo_div_106_det,
        MAX(CASE WHEN column_defn_id = 12 THEN data_value END) AS date_sailed_12_det,
        MAX(CASE WHEN column_defn_id = 79 THEN data_value END) AS date_sailed_79_det,
        MAX(CASE WHEN column_defn_id = 59 THEN data_value END) AS spc_sought_59_det,
        MAX(CASE WHEN column_defn_id = 46 THEN data_value END) AS targ_specie_46_det,
        MAX(CASE WHEN column_defn_id = 45 THEN data_value END) AS num_fish_45_md_det,
        MAX(CASE WHEN column_defn_id = 39 THEN data_value END) AS surf_temp_f_39_md_det,
        MAX(CASE WHEN column_defn_id = 3 THEN data_value END) AS hooks_per_tub_3_det,
        MAX(CASE WHEN column_defn_id = 54 THEN data_value END) AS date_hauled_54_det,
        MAX(CASE WHEN column_defn_id = 552 THEN data_value END) AS home_mgt_area_552_det,
        MAX(CASE WHEN column_defn_id = 553 THEN data_value END) AS wharf_sailed_553_det,
        MAX(CASE WHEN column_defn_id = 557 THEN data_value END) AS date_capture_557_det,
        MAX(CASE WHEN column_defn_id = 558 THEN data_value END) AS water_temp_558_md_det,
        MAX(CASE WHEN column_defn_id = 559 THEN data_value END) AS water_temp_uom_559_md_det,
        MAX(
          REPLACE(
            TRIM(REGEXP_REPLACE(CASE WHEN column_defn_id = 196 THEN data_value END, '[^[:print:]]', '')),
            '\\\\', '_'
          )
        ) AS comments_196_md_det,
        MAX(CASE WHEN column_defn_id = 188 THEN data_value END) AS time_sailed_188_det,
        MAX(CASE WHEN column_defn_id = 167 THEN data_value END) AS start_time_167_det,
        MAX(CASE WHEN column_defn_id = 43 THEN data_value END) AS num_strikes_43_det,
        MAX(CASE WHEN column_defn_id = 143 THEN data_value END) AS time_hauled_143_det,
        MAX(CASE WHEN column_defn_id = 169 THEN data_value END) AS end_time_169_det,
        MAX(CASE WHEN column_defn_id = 51 THEN data_value END) AS hook_num_51_md_det,
        MAX(CASE WHEN column_defn_id = 47 THEN data_value END) AS gang_len_fm_47_det,
        MAX(CASE WHEN column_defn_id = 48 THEN data_value END) AS dist_hooks_fm_48_det,
        MAX(CASE WHEN column_defn_id = 50 THEN data_value END) AS hook_depth_fm_50_md_det,
        MAX(CASE WHEN column_defn_id = 110 THEN data_value END) AS depth_fm_110_det,
        MAX(CASE WHEN column_defn_id = 52 THEN data_value END) AS num_buoys_52_det,
        MAX(CASE WHEN column_defn_id = 53 THEN data_value END) AS bait_53_md_det,
        MAX(CASE WHEN column_defn_id = 136 THEN data_value END) AS gear_size_136_det,
        MAX(CASE WHEN column_defn_id = 1 THEN data_value END) AS ll_gear_1_det,
        MAX(CASE WHEN column_defn_id = 34 THEN data_value END) AS harpoon_only_34_det,
        MAX(CASE WHEN column_defn_id = 28 THEN data_value END) AS other_gear_mobile_28_det,
        MAX(CASE WHEN column_defn_id = 177 THEN data_value END) AS hours_fished_177_det,
        MAX(CASE WHEN column_defn_id = 178 THEN data_value END) AS min_fished_178_det,
        MAX(CASE WHEN column_defn_id = 477 THEN data_value END) AS bluefin_caught_477_det,
        MAX(CASE WHEN column_defn_id = 425 THEN data_value END) AS bait_425_det,
        MAX(CASE WHEN column_defn_id = 427 THEN data_value END) AS port_sailed_427_det,
        MAX(CASE WHEN column_defn_id = 428 THEN data_value END) AS trip_number_428_det,
        MAX(CASE WHEN column_defn_id = 430 THEN data_value END) AS gear_type_430_det,
        MAX(CASE WHEN column_defn_id = 433 THEN data_value END) AS uom_433_det,
        MAX(CASE WHEN column_defn_id = 479 THEN data_value END) AS gear_used_479_det,
        MAX(CASE WHEN column_defn_id = 480 THEN data_value END) AS num_of_lines_480_det,
        MAX(CASE WHEN column_defn_id = 481 THEN data_value END) AS gauge_of_mono_481_det,
        MAX(CASE WHEN column_defn_id = 482 THEN data_value END) AS weight_of_bait_482_det,
        MAX(CASE WHEN column_defn_id = 484 THEN data_value END) AS bft_tag_484_md_det,
        MAX(CASE WHEN column_defn_id = 488 THEN data_value END) AS weight_uom_488_det,
        MAX(CASE WHEN column_defn_id = 93 THEN data_value END) AS set_start_time_93_det,
        MAX(CASE WHEN column_defn_id = 159 THEN data_value END) AS uom_default_fm_159_det,
        MAX(CASE WHEN column_defn_id = 124 THEN data_value END) AS c_or_f_124_det,
        MAX(CASE WHEN column_defn_id = 9 THEN data_value END) AS dpth_of_nets_9_det,
        MAX(CASE WHEN column_defn_id = 158 THEN data_value END) AS depth_158_det,
        MAX(CASE WHEN column_defn_id = 104 THEN data_value END) AS time_sailed_104_det,
        MAX(CASE WHEN column_defn_id = 111 THEN data_value END) AS soak_days_111_det,
        MAX(CASE WHEN column_defn_id = 2 THEN data_value END) AS num_tubs_2_det,
        MAX(CASE WHEN column_defn_id = 18 THEN data_value END) AS date_set_18_det,
        MAX(CASE WHEN column_defn_id = 192 THEN data_value END) AS bottom_type_192_det,
        MAX(CASE WHEN column_defn_id = 193 THEN data_value END) AS depth_193_det,
        MAX(CASE WHEN column_defn_id = 459 THEN data_value END) AS uom_459_det,
        MAX(CASE WHEN column_defn_id = 55 THEN data_value END) AS nafo_unit_55_det,
        MAX(CASE WHEN column_defn_id = 14 THEN data_value END) AS nafo_unit_14_det,
        MAX(CASE WHEN column_defn_id = 13 THEN data_value END) AS port_sailed_code_13_det,
        MAX(CASE WHEN column_defn_id = 49 THEN data_value END) AS bot_depth_fm_49_md_det,
        MAX(CASE WHEN column_defn_id = 5 THEN data_value END) AS hook_type_5_det,
        MAX(CASE WHEN column_defn_id = 107 THEN data_value END) AS date_hauled_107_det,
        MAX(CASE WHEN column_defn_id = 37 THEN data_value END) AS date_fished_37_det,
        MAX(CASE WHEN column_defn_id = 838 THEN data_value END) AS sara_log_838_det,
        MAX(CASE WHEN column_defn_id = 807 THEN data_value END) AS flank_len_uom_807_md_det,
        MAX(CASE WHEN column_defn_id = 813 THEN data_value END) AS end_latitude_813_md_det,
        MAX(CASE WHEN column_defn_id = 808 THEN data_value END) AS dressed_len_uom_808_md_det,
        MAX(CASE WHEN column_defn_id = 814 THEN data_value END) AS end_longitude_814_md_det,
        MAX(CASE WHEN column_defn_id = 817 THEN data_value END) AS gear_desc_817_det,
        MAX(CASE WHEN column_defn_id = 834 THEN data_value END) AS observer_name_834_det,
        MAX(CASE WHEN column_defn_id = 835 THEN data_value END) AS observer_id_835_det,
        MAX(CASE WHEN column_defn_id = 946 THEN data_value END) AS strategy_946_det,
        MAX(CASE WHEN column_defn_id = 947 THEN data_value END) AS bait_brought_947_det,
        MAX(CASE WHEN column_defn_id = 948 THEN data_value END) AS bait_wt_brought_948_det,
        MAX(CASE WHEN column_defn_id = 949 THEN data_value END) AS bait_uom_brought_949_det,
        MAX(CASE WHEN column_defn_id = 950 THEN data_value END) AS bait_caught_950_det,
        MAX(CASE WHEN column_defn_id = 951 THEN data_value END) AS bait_wt_caught_951_det,
        MAX(CASE WHEN column_defn_id = 952 THEN data_value END) AS bait_uom_caught_952_det
      FROM marfissci.mon_doc_entrd_dets mded
      JOIN qualifying_mon_docs qmd
        ON mded.mon_doc_id = qmd.mon_doc_id
      WHERE column_defn_id IN (", ids_sql, ")
      GROUP BY mded.mon_doc_id
    ),
    efrt_det_vals AS (
      SELECT
        leed.log_efrt_std_info_id,
        MAX(CASE WHEN column_defn_id = 767 THEN data_value END) AS port_sailed_767_ed_det,
        MAX(CASE WHEN column_defn_id = 768 THEN data_value END) AS gear_type_768_ed_det,
        MAX(CASE WHEN column_defn_id = 775 THEN data_value END) AS gear_type_775_ed_det,
        MAX(CASE WHEN column_defn_id = 786 THEN data_value END) AS hours_fished_786_ed_det,
        MAX(CASE WHEN column_defn_id = 804 THEN data_value END) AS spc_sought_804_ed_det,
        MAX(CASE WHEN column_defn_id = 805 THEN data_value END) AS gear_desc_805_ed_det,
        MAX(CASE WHEN column_defn_id = 666 THEN data_value END) AS hail_out_conf_666_ed_det,
        MAX(CASE WHEN column_defn_id = 682 THEN data_value END) AS end_time_682_ed_det,
        MAX(CASE WHEN column_defn_id = 685 THEN data_value END) AS start_time_685_ed_det,
        MAX(CASE WHEN column_defn_id = 689 THEN data_value END) AS capture_time_689_det,
        MAX(CASE WHEN column_defn_id = 694 THEN data_value END) AS depth_694_ed_det,
        MAX(CASE WHEN column_defn_id = 698 THEN data_value END) AS nafo_unit_area_698_ed_det,
        MAX(CASE WHEN column_defn_id = 699 THEN data_value END) AS num_strikes_699_ed_det,
        MAX(CASE WHEN column_defn_id = 703 THEN data_value END) AS soak_days_703_ed_det,
        MAX(CASE WHEN column_defn_id = 704 THEN data_value END) AS uom_704_ed_det,
        MAX(CASE WHEN column_defn_id = 706 THEN data_value END) AS depth_fm_706_ed_det,
        MAX(CASE WHEN column_defn_id = 707 THEN data_value END) AS soak_time_hrs_707_ed_det,
        MAX(CASE WHEN column_defn_id = 708 THEN data_value END) AS port_departed_708_ed_det,
        MAX(CASE WHEN column_defn_id = 730 THEN data_value END) AS gear_type_730_ed_det,
        MAX(CASE WHEN column_defn_id = 732 THEN data_value END) AS harp_and_ll_732_ed_det,
        MAX(CASE WHEN column_defn_id = 738 THEN data_value END) AS hours_fished_738_ed_det,
        MAX(CASE WHEN column_defn_id = 742 THEN data_value END) AS total_hours_fished_742_ed_det,
        MAX(CASE WHEN column_defn_id = 741 THEN data_value END) AS observer_trip_code_741_ed_det,
        MAX(CASE WHEN column_defn_id = 743 THEN data_value END) AS flank_length_743_ed_det,
        MAX(CASE WHEN column_defn_id = 744 THEN data_value END) AS dressed_length_744_ed_det,
        MAX(CASE WHEN column_defn_id = 745 THEN data_value END) AS search_start_time_745_ed_det,
        MAX(CASE WHEN column_defn_id = 758 THEN data_value END) AS depth_uom_fm_758_ed_det,
        MAX(CASE WHEN column_defn_id = 606 THEN data_value END) AS gillnet_type_606_ed_det,
        MAX(CASE WHEN column_defn_id = 608 THEN data_value END) AS soak_time_min_608_ed_det,
        MAX(CASE WHEN column_defn_id = 57 THEN data_value END) AS depth_57_ed_det,
        MAX(CASE WHEN column_defn_id = 41 THEN data_value END) AS time_started_41_det,
        MAX(CASE WHEN column_defn_id = 527 THEN data_value END) AS date_fished_527_ed_det,
        MAX(CASE WHEN column_defn_id = 21 THEN data_value END) AS obs_onboard_21_ed_det,
        MAX(CASE WHEN column_defn_id = 106 THEN data_value END) AS nafo_div_106_ed_det,
        MAX(CASE WHEN column_defn_id = 12 THEN data_value END) AS date_sailed_12_ed_det,
        MAX(CASE WHEN column_defn_id = 79 THEN data_value END) AS date_sailed_79_ed_det,
        MAX(CASE WHEN column_defn_id = 59 THEN data_value END) AS spc_sought_59_ed_det,
        MAX(CASE WHEN column_defn_id = 46 THEN data_value END) AS targ_specie_46_ed_det,
        MAX(CASE WHEN column_defn_id = 45 THEN data_value END) AS num_fish_45_ed_det,
        MAX(CASE WHEN column_defn_id = 39 THEN data_value END) AS surface_temp_f_39_det,
        MAX(CASE WHEN column_defn_id = 3 THEN data_value END) AS hooks_per_tub_3_ed_det,
        MAX(CASE WHEN column_defn_id = 54 THEN data_value END) AS date_hauled_54_ed_det,
        MAX(CASE WHEN column_defn_id = 553 THEN data_value END) AS wharf_sailed_553_ed_det,
        MAX(CASE WHEN column_defn_id = 557 THEN data_value END) AS date_capture_557_ed_det,
        MAX(CASE WHEN column_defn_id = 558 THEN data_value END) AS water_temp_558_det,
        MAX(CASE WHEN column_defn_id = 559 THEN data_value END) AS water_temp_uom_559_det,
        MAX(
          REPLACE(
            TRIM(REGEXP_REPLACE(CASE WHEN column_defn_id = 196 THEN data_value END, '[^[:print:]]', '')),
            '\\\\', '_'
          )
        ) AS comments_196_det,
        MAX(CASE WHEN column_defn_id = 188 THEN data_value END) AS time_sailed_188_ed_det,
        MAX(CASE WHEN column_defn_id = 167 THEN data_value END) AS start_time_167_ed_det,
        MAX(CASE WHEN column_defn_id = 43 THEN data_value END) AS num_strikes_43_ed_det,
        MAX(CASE WHEN column_defn_id = 143 THEN data_value END) AS time_hauled_143_ed_det,
        MAX(CASE WHEN column_defn_id = 169 THEN data_value END) AS end_time_169_ed_det,
        MAX(CASE WHEN column_defn_id = 51 THEN data_value END) AS hook_num_51_det,
        MAX(CASE WHEN column_defn_id = 47 THEN data_value END) AS gang_len_fm_47_ed_det,
        MAX(CASE WHEN column_defn_id = 48 THEN data_value END) AS dist_hooks_fm_48_ed_det,
        MAX(CASE WHEN column_defn_id = 50 THEN data_value END) AS hook_depth_fm_50_det,
        MAX(CASE WHEN column_defn_id = 110 THEN data_value END) AS depth_fm_110_ed_det,
        MAX(CASE WHEN column_defn_id = 52 THEN data_value END) AS num_buoys_52_ed_det,
        MAX(CASE WHEN column_defn_id = 53 THEN data_value END) AS bait_53_det,
        MAX(CASE WHEN column_defn_id = 136 THEN data_value END) AS gear_size_136_ed_det,
        MAX(CASE WHEN column_defn_id = 1 THEN data_value END) AS ll_gear_1_ed_det,
        MAX(CASE WHEN column_defn_id = 34 THEN data_value END) AS harpoon_only_34_ed_det,
        MAX(CASE WHEN column_defn_id = 28 THEN data_value END) AS other_gear_mobile_28_ed_det,
        MAX(CASE WHEN column_defn_id = 177 THEN data_value END) AS hours_fished_177_ed_det,
        MAX(CASE WHEN column_defn_id = 178 THEN data_value END) AS min_fished_178_ed_det,
        MAX(CASE WHEN column_defn_id = 477 THEN data_value END) AS bluefin_caught_477_ed_det,
        MAX(CASE WHEN column_defn_id = 425 THEN data_value END) AS bait_425_ed_det,
        MAX(CASE WHEN column_defn_id = 427 THEN data_value END) AS port_sailed_427_ed_det,
        MAX(CASE WHEN column_defn_id = 428 THEN data_value END) AS trip_number_428_ed_det,
        MAX(CASE WHEN column_defn_id = 430 THEN data_value END) AS gear_type_430_ed_det,
        MAX(CASE WHEN column_defn_id = 433 THEN data_value END) AS uom_433_ed_det,
        MAX(CASE WHEN column_defn_id = 479 THEN data_value END) AS gear_used_479_ed_det,
        MAX(CASE WHEN column_defn_id = 480 THEN data_value END) AS num_of_lines_480_ed_det,
        MAX(CASE WHEN column_defn_id = 482 THEN data_value END) AS weight_of_bait_482_ed_det,
        MAX(CASE WHEN column_defn_id = 484 THEN data_value END) AS bft_tag_484_ed_det,
        MAX(CASE WHEN column_defn_id = 488 THEN data_value END) AS weight_uom_488_ed_det,
        MAX(CASE WHEN column_defn_id = 93 THEN data_value END) AS set_start_time_93_ed_det,
        MAX(CASE WHEN column_defn_id = 159 THEN data_value END) AS uom_default_fm_159_ed_det,
        MAX(CASE WHEN column_defn_id = 124 THEN data_value END) AS c_or_f_124_ed_det,
        MAX(CASE WHEN column_defn_id = 9 THEN data_value END) AS dpth_of_nets_9_ed_det,
        MAX(CASE WHEN column_defn_id = 158 THEN data_value END) AS depth_158_ed_det,
        MAX(CASE WHEN column_defn_id = 104 THEN data_value END) AS time_sailed_104_ed_det,
        MAX(CASE WHEN column_defn_id = 111 THEN data_value END) AS soak_days_111_ed_det,
        MAX(CASE WHEN column_defn_id = 2 THEN data_value END) AS num_tubs_2_ed_det,
        MAX(CASE WHEN column_defn_id = 18 THEN data_value END) AS date_set_18_ed_det,
        MAX(CASE WHEN column_defn_id = 192 THEN data_value END) AS bottom_type_192_ed_det,
        MAX(CASE WHEN column_defn_id = 193 THEN data_value END) AS depth_193_ed_det,
        MAX(CASE WHEN column_defn_id = 459 THEN data_value END) AS uom_459_ed_det,
        MAX(CASE WHEN column_defn_id = 55 THEN data_value END) AS nafo_unit_55_ed_det,
        MAX(CASE WHEN column_defn_id = 14 THEN data_value END) AS nafo_unit_14_ed_det,
        MAX(CASE WHEN column_defn_id = 13 THEN data_value END) AS port_sailed_code_13_ed_det,
        MAX(CASE WHEN column_defn_id = 49 THEN data_value END) AS bot_depth_fm_49_det,
        MAX(CASE WHEN column_defn_id = 5 THEN data_value END) AS hook_type_5_ed_det,
        MAX(CASE WHEN column_defn_id = 107 THEN data_value END) AS date_hauled_107_ed_det,
        MAX(CASE WHEN column_defn_id = 37 THEN data_value END) AS date_fished_37_ed_det,
        MAX(CASE WHEN column_defn_id = 838 THEN data_value END) AS sara_log_838_ed_det,
        MAX(CASE WHEN column_defn_id = 807 THEN data_value END) AS flank_len_uom_807_ed_det,
        MAX(CASE WHEN column_defn_id = 813 THEN data_value END) AS end_latitude_813_det,
        MAX(CASE WHEN column_defn_id = 808 THEN data_value END) AS dressed_len_uom_808_ed_det,
        MAX(CASE WHEN column_defn_id = 814 THEN data_value END) AS end_longitude_814_det,
        MAX(CASE WHEN column_defn_id = 817 THEN data_value END) AS gear_desc_817_ed_det,
        MAX(CASE WHEN column_defn_id = 834 THEN data_value END) AS observer_name_834_ed_det,
        MAX(CASE WHEN column_defn_id = 835 THEN data_value END) AS observer_id_835_ed_det,
        MAX(CASE WHEN column_defn_id = 946 THEN data_value END) AS strategy_946_ed_det,
        MAX(CASE WHEN column_defn_id = 947 THEN data_value END) AS bait_brought_947_ed_det,
        MAX(CASE WHEN column_defn_id = 948 THEN data_value END) AS bait_wt_brought_948_ed_det,
        MAX(CASE WHEN column_defn_id = 949 THEN data_value END) AS bait_uom_brought_949_ed_det,
        MAX(CASE WHEN column_defn_id = 950 THEN data_value END) AS bait_caught_950_ed_det,
        MAX(CASE WHEN column_defn_id = 951 THEN data_value END) AS bait_wt_caught_951_ed_det,
        MAX(CASE WHEN column_defn_id = 952 THEN data_value END) AS bait_uom_caught_952_ed_det
      FROM marfissci.log_efrt_entrd_dets leed
      JOIN qualifying_sets qs
        ON leed.log_efrt_std_info_id = qs.log_efrt_std_info_id
      WHERE column_defn_id IN (", ids_sql, ")
      GROUP BY leed.log_efrt_std_info_id
    ),
    spc_det_vals AS (
      SELECT
        lsed.log_spc_std_info_id,
        MAX(CASE WHEN column_defn_id = 45 THEN data_value END) AS num_caught_45_det,
        MAX(CASE WHEN column_defn_id = 484 THEN data_value END) AS bft_tag_484_det,
        MAX(CASE WHEN column_defn_id = 743 THEN data_value END) AS flank_length_743_det,
        MAX(CASE WHEN column_defn_id = 807 THEN data_value END) AS flank_length_uom_807_det,
        MAX(CASE WHEN column_defn_id = 744 THEN data_value END) AS dressed_length_744_det,
        MAX(CASE WHEN column_defn_id = 808 THEN data_value END) AS dressed_length_uom_808_det,
        MAX(CASE WHEN column_defn_id = 689 THEN data_value END) AS capture_time_689_sd_det,
        MAX(CASE WHEN column_defn_id = 557 THEN data_value END) AS date_capture_557_sd_det,
        MAX(CASE WHEN column_defn_id = 488 THEN data_value END) AS weight_uom_488_sd_det
      FROM marfissci.log_spc_entrd_dets lsed
      JOIN qualifying_spc qsp
        ON lsed.log_spc_std_info_id = qsp.log_spc_std_info_id
      WHERE column_defn_id IN (", ids_sql, ")
      GROUP BY lsed.log_spc_std_info_id
    ),
    pro_set_species AS (
      SELECT
        psi.log_efrt_std_info_id,
        psi.species_code,
        MAX(psi.species_size_code) AS species_size_code_pro,
        MAX(psi.landed_form_code) AS landed_form_code_pro,
        SUM(psi.rnd_weight_kgs) AS round_weight_kg_pro,
        SUM(psi.rpt_weight_kgs) AS reported_weight_kg_pro,
        MAX(psi.spc_tag_id) AS spc_tag_id_pro,
        MAX(psi.gear_code) AS gear_pro,
        MAX(psi.nafo_unit_area_id) AS nafo_area_id_pro,
        MAX(psi.latitude) AS latitude_pro,
        MAX(psi.longitude) AS longitude_pro
      FROM marfissci.pro_spc_info psi
      JOIN (SELECT DISTINCT log_efrt_std_info_id FROM qualifying_sets) qs
        ON psi.log_efrt_std_info_id = qs.log_efrt_std_info_id
      WHERE psi.log_efrt_std_info_id IS NOT NULL
      GROUP BY psi.log_efrt_std_info_id, psi.species_code
    ),
    pro_trip_species AS (
      SELECT
        psi.trip_id,
        psi.species_code,
        SUM(psi.rnd_weight_kgs) AS weight_kgs_trip_pro
      FROM marfissci.pro_spc_info psi
      JOIN qualifying_trips qt
        ON psi.trip_id = qt.trip_id
      WHERE psi.trip_id IS NOT NULL
      GROUP BY psi.trip_id, psi.species_code
    ),
    ssf_pro_lookup AS (
      SELECT
        species_code,
        species_size_code,
        landed_form_code,
        MAX(desc_eng) AS desc_eng_pro,
        MAX(form_factor) AS form_factor_pro
      FROM marfissci.specie_size_forms
      GROUP BY species_code, species_size_code, landed_form_code
    )
    SELECT
      -- Identifiers & vessel/licence/trip context
      md.trip_id,
      md.mon_doc_id,
      md.mon_doc_defn_id,
      COALESCE(mdd.trip_number_428_det, ed.trip_number_428_ed_det) AS trip_number_coal,
      le.log_efrt_std_info_id AS set_id,
      ls.log_spc_std_info_id AS log_spc_id,
      md.vr_number AS vr_number_md,
      md.vr_number AS vr_number,
      vl.vessel_name_vsl,
      mdlic.licence_id AS licence_number_log,
      mdd.home_mgt_area_552_det,
      COALESCE(mdd.observer_trip_code_741_det, ed.observer_trip_code_741_ed_det) AS observer_trip_code_coal,
      COALESCE(mdd.obs_onboard_21_det, ed.obs_onboard_21_ed_det) AS obs_onboard_coal,
      COALESCE(mdd.observer_name_834_det, ed.observer_name_834_ed_det) AS observer_name_coal,
      COALESCE(mdd.observer_id_835_det, ed.observer_id_835_ed_det) AS observer_id_coal,
      COALESCE(mdd.hail_out_conf_666_det, ed.hail_out_conf_666_ed_det) AS hail_out_conf_coal,
      hil.hail_in_conf_coal,
      COALESCE(mdd.sara_log_838_det, ed.sara_log_838_ed_det) AS sara_log_coal,

      -- Ports & sailing/fishing/hauling/capture dates & times
      COALESCE(
        mdd.port_sailed_767_det, ed.port_sailed_767_ed_det,
        mdd.port_sailed_427_det, ed.port_sailed_427_ed_det,
        mdd.port_departed_708_det, ed.port_departed_708_ed_det,
        mdd.port_sailed_code_13_det, ed.port_sailed_code_13_ed_det
      ) AS port_sailed_coal,
      COALESCE(mdd.wharf_sailed_553_det, ed.wharf_sailed_553_ed_det) AS wharf_sailed_coal,
      COALESCE(mdd.date_sailed_12_det, mdd.date_sailed_79_det, ed.date_sailed_12_ed_det, ed.date_sailed_79_ed_det) AS date_sailed_coal,
      COALESCE(mdd.time_sailed_104_det, mdd.time_sailed_188_det, ed.time_sailed_104_ed_det, ed.time_sailed_188_ed_det) AS time_sailed_coal,
      sld.date_landed_slip AS date_landed,
      le.fv_fished_datetime AS date_fished_log,
      le.fv_fished_datetime AS date_caught_log,
      TRUNC(le.fv_fished_datetime) AS date_caught_calc,
      COALESCE(mdd.date_fished_527_det, ed.date_fished_527_ed_det, mdd.date_fished_37_det, ed.date_fished_37_ed_det) AS date_fished_det_coal,
      COALESCE(mdd.date_set_18_det, ed.date_set_18_ed_det) AS date_set_coal,
      COALESCE(
        ed.time_started_41_det, mdd.time_started_41_md_det,
        mdd.start_time_167_det, ed.start_time_167_ed_det,
        mdd.start_time_685_det, ed.start_time_685_ed_det,
        mdd.set_start_time_93_det, ed.set_start_time_93_ed_det,
        mdd.search_start_time_745_det, ed.search_start_time_745_ed_det
      ) AS time_started_coal,
      COALESCE(mdd.end_time_169_det, ed.end_time_169_ed_det, mdd.end_time_682_det, ed.end_time_682_ed_det) AS end_time_coal,
      COALESCE(mdd.date_hauled_54_det, ed.date_hauled_54_ed_det, mdd.date_hauled_107_det, ed.date_hauled_107_ed_det) AS date_hauled_coal,
      COALESCE(mdd.time_hauled_143_det, ed.time_hauled_143_ed_det) AS time_hauled_coal,
      COALESCE(sdv.date_capture_557_sd_det, mdd.date_capture_557_det, ed.date_capture_557_ed_det) AS date_capture_coal,
      COALESCE(ed.capture_time_689_det, mdd.capture_time_689_md_det, sdv.capture_time_689_sd_det) AS capture_time_coal,

      -- Gear, rigging, strategy & bait
      le.fv_gear_code AS gear_log,
      md.fv_gear_code AS gear_md,
      pro.gear_pro,
      COALESCE(
        mdd.gear_type_730_det, ed.gear_type_730_ed_det,
        mdd.gear_type_768_det, ed.gear_type_768_ed_det,
        mdd.gear_type_775_det, ed.gear_type_775_ed_det,
        mdd.gear_type_430_det, ed.gear_type_430_ed_det,
        mdd.gear_used_479_det, ed.gear_used_479_ed_det
      ) AS gear_type_coal,
      COALESCE(
        mdd.gear_desc_805_det, ed.gear_desc_805_ed_det,
        mdd.gear_desc_817_det, ed.gear_desc_817_ed_det
      ) AS gear_desc_coal,
      COALESCE(mdd.harp_and_ll_732_det, ed.harp_and_ll_732_ed_det) AS harp_and_ll_coal,
      COALESCE(mdd.harpoon_only_34_det, ed.harpoon_only_34_ed_det) AS harpoon_only_coal,
      COALESCE(mdd.other_gear_mobile_28_det, ed.other_gear_mobile_28_ed_det) AS other_gear_mobile_coal,
      COALESCE(mdd.ll_gear_1_det, ed.ll_gear_1_ed_det) AS ll_gear_coal,
      COALESCE(mdd.gillnet_type_606_det, ed.gillnet_type_606_ed_det) AS gillnet_type_coal,
      COALESCE(mdd.hook_type_5_det, ed.hook_type_5_ed_det) AS hook_type_coal,
      COALESCE(mdd.bottom_type_192_det, ed.bottom_type_192_ed_det) AS bottom_type_coal,
      COALESCE(mdd.strategy_946_det, ed.strategy_946_ed_det) AS strategy_coal,
      COALESCE(mdd.gang_len_fm_47_det, ed.gang_len_fm_47_ed_det) AS gang_len_fm_coal,
      COALESCE(mdd.dist_hooks_fm_48_det, ed.dist_hooks_fm_48_ed_det) AS dist_hooks_fm_coal,
      COALESCE(mdd.num_of_lines_480_det, ed.num_of_lines_480_ed_det) AS num_of_lines_coal,
      COALESCE(mdd.num_tubs_2_det, ed.num_tubs_2_ed_det) AS num_tubs_coal,
      COALESCE(mdd.hooks_per_tub_3_det, ed.hooks_per_tub_3_ed_det) AS hooks_per_tub_coal,
      COALESCE(mdd.num_buoys_52_det, ed.num_buoys_52_ed_det) AS num_buoys_coal,
      COALESCE(mdd.gear_size_136_det, ed.gear_size_136_ed_det) AS gear_size_coal,
      mdd.gauge_of_mono_481_det,
      COALESCE(
        ed.bait_53_det, mdd.bait_53_md_det,
        mdd.bait_425_det, ed.bait_425_ed_det,
        mdd.bait_brought_947_det, ed.bait_brought_947_ed_det,
        mdd.bait_caught_950_det, ed.bait_caught_950_ed_det
      ) AS bait_coal,
      COALESCE(
        mdd.weight_of_bait_482_det, ed.weight_of_bait_482_ed_det,
        mdd.bait_wt_brought_948_det, ed.bait_wt_brought_948_ed_det,
        mdd.bait_wt_caught_951_det, ed.bait_wt_caught_951_ed_det
      ) AS weight_of_bait_coal,
      COALESCE(
        mdd.bait_uom_brought_949_det, ed.bait_uom_brought_949_ed_det,
        mdd.bait_uom_caught_952_det, ed.bait_uom_caught_952_ed_det,
        mdd.weight_uom_488_det, ed.weight_uom_488_ed_det, sdv.weight_uom_488_sd_det,
        mdd.uom_433_det, ed.uom_433_ed_det,
        mdd.uom_459_det, ed.uom_459_ed_det,
        mdd.uom_704_det, ed.uom_704_ed_det
      ) AS uom_coal,

      -- Effort metrics & soak times
      le.fv_duration_in_hours AS hours_fished_log,
      COALESCE(
        mdd.total_hours_fished_742_det, ed.total_hours_fished_742_ed_det,
        mdd.hours_fished_786_det, ed.hours_fished_786_ed_det,
        mdd.hours_fished_738_det, ed.hours_fished_738_ed_det,
        mdd.hours_fished_177_det, ed.hours_fished_177_ed_det
      ) AS hours_fished_det_coal,
      COALESCE(mdd.min_fished_178_det, ed.min_fished_178_ed_det) AS min_fished_coal,
      COALESCE(mdd.soak_days_703_det, ed.soak_days_703_ed_det, mdd.soak_days_111_det, ed.soak_days_111_ed_det) AS soak_days_coal,
      COALESCE(mdd.soak_time_hrs_707_det, ed.soak_time_hrs_707_ed_det) AS soak_time_hrs_coal,
      COALESCE(mdd.soak_time_min_608_det, ed.soak_time_min_608_ed_det) AS soak_time_min_coal,
      COALESCE(ed.hook_num_51_det, mdd.hook_num_51_md_det) AS hook_num_coal,
      COALESCE(mdd.num_strikes_43_det, ed.num_strikes_43_ed_det, mdd.num_strikes_699_det, ed.num_strikes_699_ed_det) AS num_strikes_coal,
      COALESCE(mdd.bluefin_caught_477_det, ed.bluefin_caught_477_ed_det) AS bluefin_caught_coal,

      -- NAFO area from multiple origins
      le.det_nafo_unit_area_id AS nafo_area_id_log_det,
      le.fv_nafo_unit_area_id AS nafo_area_id_log_fv,
      md.fv_nafo_unit_area_id AS nafo_area_id_md,
      pro.nafo_area_id_pro,
      nafo_det.area AS nafo_area_log_det,
      nafo_fv.area AS nafo_area_log_fv,
      nafo_md.area AS nafo_area_md,
      nafo_pro.area AS nafo_area_pro,
      COALESCE(
        mdd.nafo_unit_area_698_det, ed.nafo_unit_area_698_ed_det,
        mdd.nafo_div_106_det, ed.nafo_div_106_ed_det,
        mdd.nafo_unit_55_det, ed.nafo_unit_55_ed_det,
        mdd.nafo_unit_14_det, ed.nafo_unit_14_ed_det
      ) AS nafo_det_coal,

      -- Species caught, target species, catch usage & landed form
      COALESCE(ls.ssf_species_code, pro.species_code) AS species_caught,
      lic.species_code AS target_species_lic,
      COALESCE(
        mdd.targ_specie_46_det, ed.targ_specie_46_ed_det,
        mdd.spc_sought_804_det, ed.spc_sought_804_ed_det,
        mdd.spc_sought_59_det, ed.spc_sought_59_ed_det
      ) AS target_species_det_coal,
      ls.catch_usage_code,
      ls.ssf_landed_form_code AS landed_form_code_log,
      pro.landed_form_code_pro,
      ssf_pro.desc_eng_pro,
      ssf_pro.form_factor_pro,

      -- Counts & weights
      COALESCE(sdv.num_caught_45_det, mdd.num_fish_45_md_det, ed.num_fish_45_ed_det) AS num_caught_coal,
      DECODE(ls.ssf_species_code, 254, 1, sdv.num_caught_45_det) AS num_landed_calc,
      CASE ls.unit_of_measure_id
        WHEN 10 THEN ls.weight
        WHEN 20 THEN ls.weight / 2.2046
        WHEN 30 THEN ls.weight * 1000
        WHEN 40 THEN ls.weight * 15.876
        ELSE ls.weight
      END AS reported_weight_kg_log,
      pro.reported_weight_kg_pro,
      ROUND(
        CASE ls.unit_of_measure_id
          WHEN 10 THEN ls.weight
          WHEN 20 THEN ls.weight / 2.2046
          WHEN 30 THEN ls.weight * 1000
          WHEN 40 THEN ls.weight * 15.876
          ELSE ls.weight
        END * NVL(fac.form_factor, 1),
        3
      ) AS round_weight_kg_calc,
      pro.round_weight_kg_pro,
      pts.weight_kgs_trip_pro,

      -- Tag IDs & biological lengths
      pro.spc_tag_id_pro,
      COALESCE(sdv.bft_tag_484_det, mdd.bft_tag_484_md_det, ed.bft_tag_484_ed_det) AS bft_tag_coal,
      COALESCE(sdv.flank_length_743_det, mdd.flank_length_743_md_det, ed.flank_length_743_ed_det) AS flank_length_coal,
      COALESCE(sdv.flank_length_uom_807_det, mdd.flank_len_uom_807_md_det, ed.flank_len_uom_807_ed_det) AS flank_length_uom_coal,
      COALESCE(sdv.dressed_length_744_det, mdd.dressed_length_744_md_det, ed.dressed_length_744_ed_det) AS dressed_length_coal,
      COALESCE(sdv.dressed_length_uom_808_det, mdd.dressed_len_uom_808_md_det, ed.dressed_len_uom_808_ed_det) AS dressed_length_uom_coal,

      -- Spatial coordinates, depths & temperatures
      le.ent_latitude AS latitude_ent,
      le.det_latitude AS latitude_det,
      pro.latitude_pro,
      ROUND(NVL(le.ent_latitude, le.det_latitude) * 0.01, 6) AS latitude_caught_calc,
      le.ent_longitude AS longitude_ent,
      le.det_longitude AS longitude_det,
      pro.longitude_pro,
      ROUND(NVL(le.ent_longitude, le.det_longitude) * 0.01, 6) AS longitude_caught_calc,
      COALESCE(ed.end_latitude_813_det, mdd.end_latitude_813_md_det) AS end_latitude_coal,
      COALESCE(ed.end_longitude_814_det, mdd.end_longitude_814_md_det) AS end_longitude_coal,
      COALESCE(
        ed.bot_depth_fm_49_det, mdd.bot_depth_fm_49_md_det,
        mdd.depth_fm_706_det, ed.depth_fm_706_ed_det,
        mdd.depth_fm_110_det, ed.depth_fm_110_ed_det,
        mdd.depth_694_det, ed.depth_694_ed_det,
        mdd.depth_57_det, ed.depth_57_ed_det,
        mdd.depth_158_det, ed.depth_158_ed_det,
        mdd.depth_193_det, ed.depth_193_ed_det,
        mdd.dpth_of_nets_9_det, ed.dpth_of_nets_9_ed_det
      ) AS depth_coal,
      COALESCE(mdd.depth_uom_fm_758_det, ed.depth_uom_fm_758_ed_det, mdd.uom_default_fm_159_det, ed.uom_default_fm_159_ed_det) AS depth_uom_coal,
      COALESCE(ed.hook_depth_fm_50_det, mdd.hook_depth_fm_50_md_det) AS hook_depth_fm_coal,
      COALESCE(ed.surface_temp_f_39_det, mdd.surf_temp_f_39_md_det) AS surface_temp_f_coal,
      COALESCE(ed.water_temp_558_det, mdd.water_temp_558_md_det) AS water_temp_coal,
      COALESCE(ed.water_temp_uom_559_det, mdd.water_temp_uom_559_md_det, mdd.c_or_f_124_det, ed.c_or_f_124_ed_det) AS water_temp_uom_coal,

      -- Comments
      COALESCE(ed.comments_196_det, mdd.comments_196_md_det) AS comments_196_coal

    FROM marfissci.mon_docs md
    JOIN marfissci.log_efrt_std_info le
      ON md.mon_doc_id = le.mon_doc_id
    LEFT JOIN marfissci.log_spc_std_info ls
      ON le.log_efrt_std_info_id = ls.log_efrt_std_info_id
    LEFT JOIN pro_set_species pro
      ON le.log_efrt_std_info_id = pro.log_efrt_std_info_id
     AND ls.ssf_species_code = pro.species_code
    LEFT JOIN pro_trip_species pts
      ON md.trip_id = pts.trip_id
     AND COALESCE(ls.ssf_species_code, pro.species_code) = pts.species_code
    LEFT JOIN ssf_pro_lookup ssf_pro
      ON pro.species_code = ssf_pro.species_code
     AND pro.species_size_code_pro = ssf_pro.species_size_code
     AND pro.landed_form_code_pro = ssf_pro.landed_form_code
    LEFT JOIN marfissci.mon_doc_lics mdlic
      ON ls.mon_doc_lic_id = mdlic.mon_doc_lic_id
    LEFT JOIN marfissci.licences lic
      ON mdlic.licence_id = lic.licence_id
    LEFT JOIN vessels_lookup vl
      ON md.vr_number = vl.vr_number
    LEFT JOIN hail_in_lookup hil
      ON md.trip_id = hil.trip_id
    LEFT JOIN slip_landing_dates sld
      ON md.mon_doc_id = sld.mon_doc_id
    LEFT JOIN md_det_vals mdd
      ON md.mon_doc_id = mdd.mon_doc_id
    LEFT JOIN efrt_det_vals ed
      ON le.log_efrt_std_info_id = ed.log_efrt_std_info_id
    LEFT JOIN spc_det_vals sdv
      ON ls.log_spc_std_info_id = sdv.log_spc_std_info_id
    LEFT JOIN marfissci.ssf_factors fac
      ON ls.ssf_species_code = fac.ssf_species_code
     AND ls.ssf_species_size_code = fac.ssf_species_size_code
     AND ls.ssf_landed_form_code = fac.ssf_landed_form_code
    LEFT JOIN marfissci.nafo_unit_areas nafo_det
      ON le.det_nafo_unit_area_id = nafo_det.area_id
    LEFT JOIN marfissci.nafo_unit_areas nafo_fv
      ON le.fv_nafo_unit_area_id = nafo_fv.area_id
    LEFT JOIN marfissci.nafo_unit_areas nafo_md
      ON md.fv_nafo_unit_area_id = nafo_md.area_id
    LEFT JOIN marfissci.nafo_unit_areas nafo_pro
      ON pro.nafo_area_id_pro = nafo_pro.area_id
    ", doc_defn_filter,
                      year_filter,
                      "
    ORDER BY
      md.trip_id,
      le.log_efrt_std_info_id,
      species_caught,
      ls.log_spc_std_info_id
  ")
  
  LPL <- thecmd(cxn, sql_query)
  
  if (isTRUE(save_rdata)) {
    year_tag <- if (is.null(min_year)) "all_years" else paste0("from_", as.integer(min_year))
    scope_tag <- if (identical(mondoc_type, "ALL")) "ALL" else "TASK1_fleets"
    out_file <- file.path(
      "R:/ATLSiteShares/SABS/LargePelagics/COMMERCIAL DATA/TUNAVERSE/data",
      paste0("Large_Pelagics_logs_sci_", scope_tag, "_", year_tag, ".Rdata")
    )
    save(LPL, file = out_file)
  }
  
  return(LPL)
}