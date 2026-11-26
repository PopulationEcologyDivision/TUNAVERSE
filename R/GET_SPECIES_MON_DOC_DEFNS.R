#' @title get_species_mon_doc_defns
#' @description Queries `MARFISSCI` to identify which monitoring document
#' definitions (`MON_DOC_DEFN_ID`) record catches of a specified vector of
#' species codes (`SSF_SPECIES_CODE`), returning each matching document
#' definition's short title, a sorted comma-separated list of observed target
#' species codes, and the associated variable detail `COLUMN_DEFN_ID`s captured
#' across document header (`MON_DOC_ENTRD_DETS`), effort (`LOG_EFRT_ENTRD_DETS`),
#' and species (`LOG_SPC_ENTRD_DETS`) detail tables.
#'
#' By default, species associations are restricted strictly to log-level catch
#' entries (`LOG_SPC_STD_INFO.SSF_SPECIES_CODE`) linked to each monitoring
#' document's effort sets (`LOG_EFRT_STD_INFO`), avoiding cross-contamination
#' from unrelated monitoring documents or sets sharing the same `TRIP_ID`.
#' Set `include_pro_set = TRUE` to optionally include set-matched prorated catch
#' records (`PRO_SPC_INFO` joined on `LOG_EFRT_STD_INFO_ID`).
#'
#' @param cxn A valid Oracle connection object (e.g., from `RODBC::odbcConnect()`
#' or `ROracle::dbConnect()`). Passed to `Mar.utils::connectionCheck()`.
#' @param species_codes Integer or numeric vector of `MARFISSCI` species codes
#' (`SSF_SPECIES_CODE`) used to filter qualifying monitoring documents.
#' Defaults to common large pelagic and bycatch species codes
#' (`c(262, 253, 254, 251, 256, 252, 261, 263, 299, 369, 372, 375, 270, 29, 778, 830, 730, 710, 399, 189, 225)`).
#' @param min_year Optional integer minimum fishing year filter applied to
#' `COALESCE(LOG_EFRT_STD_INFO.FV_FISHED_DATETIME, SLIP_OFFLD_STD_INFO.LANDING_DATE_TIME)`.
#' Defaults to `NULL` (no year restriction).
#' @param include_pro_set Logical; if `FALSE` (default), matches species strictly
#' from `LOG_SPC_STD_INFO.SSF_SPECIES_CODE`. If `TRUE`, also unions set-level
#' matches from `PRO_SPC_INFO.SPECIES_CODE` joined on `LOG_EFRT_STD_INFO_ID`.
#' @return A `data.frame` with columns `MON_DOC_DEFN_ID`, `SHORT_DOC_TITLE`,
#' `SPECIES` (comma-separated species codes), and `COLUMN_DEFN_IDS`
#' (comma-separated `COLUMN_DEFN_ID` values), ordered by `MON_DOC_DEFN_ID`.
#' @examples
#' \dontrun{
#' library(data.table)
#' channel <- RODBC::odbcConnect(
#'   dsn = Sys.getenv("ORACLE_SID"),
#'   uid = Sys.getenv("ORACLE_USER"),
#'   pwd = Sys.getenv("ORACLE_PASS")
#' )
#' MD_sp = data.table(get_species_mon_doc_defns(cxn=channel, min_year = 2003))
#' }
#' @author Alex Hanke, \email{Alex.Hanke@@dfo-mpo.gc.ca}
#' @seealso [LARGE_PELAGICS_LOGS_sci()], [marfis_guide()]
#' @export
get_species_mon_doc_defns <- function(cxn = NULL,
                                      species_codes = c(
                                        262, 253, 254, 251, 256, 252, 261, 263,
                                        299, 369, 372, 375, 270, 29, 778, 830,
                                        730, 710, 399, 189, 225
                                      ),
                                      min_year = NULL,
                                      include_pro_set = FALSE) {
  thecmd <- Mar.utils::connectionCheck(cxn)
  sp_vec <- unique(as.integer(species_codes))
  sp_sql <- paste(sp_vec, collapse = ", ")
  
  year_filter <- ""
  if (!is.null(min_year)) {
    year_filter <- paste0(
      " AND EXTRACT(YEAR FROM COALESCE(le.fv_fished_datetime, sld.date_landed_slip)) >= ",
      as.integer(min_year)
    )
  }
  
  pro_union_sql <- ""
  if (isTRUE(include_pro_set)) {
    pro_union_sql <- paste0("
      UNION
      SELECT DISTINCT
        md.mon_doc_id,
        md.mon_doc_defn_id,
        pro.species_code AS species_code
      FROM marfissci.mon_docs md
      JOIN marfissci.log_efrt_std_info le
        ON md.mon_doc_id = le.mon_doc_id
      JOIN marfissci.pro_spc_info pro
        ON le.log_efrt_std_info_id = pro.log_efrt_std_info_id
      LEFT JOIN slip_landing_dates sld
        ON md.mon_doc_id = sld.mon_doc_id
      WHERE md.mon_doc_defn_id > 0
        AND pro.species_code IN (", sp_sql, ")",
                            year_filter
    )
  }
  
  sql_query <- paste0("
    WITH slip_landing_dates AS (
      SELECT
        mon_doc_id,
        MAX(landing_date_time) AS date_landed_slip
      FROM marfissci.slip_offld_std_info
      GROUP BY mon_doc_id
    ),
    qualifying_docs AS (
      SELECT DISTINCT
        md.mon_doc_id,
        md.mon_doc_defn_id,
        ls.ssf_species_code AS species_code
      FROM marfissci.mon_docs md
      JOIN marfissci.log_efrt_std_info le
        ON md.mon_doc_id = le.mon_doc_id
      JOIN marfissci.log_spc_std_info ls
        ON le.log_efrt_std_info_id = ls.log_efrt_std_info_id
      LEFT JOIN slip_landing_dates sld
        ON md.mon_doc_id = sld.mon_doc_id
      WHERE md.mon_doc_defn_id > 0
        AND ls.ssf_species_code IN (", sp_sql, ")",
                      year_filter,
                      pro_union_sql,
                      "
    ),
    species_by_defn AS (
      SELECT
        mon_doc_defn_id,
        LISTAGG(TO_CHAR(species_code), ', ') WITHIN GROUP (ORDER BY species_code) AS species
      FROM (
        SELECT DISTINCT mon_doc_defn_id, species_code
        FROM qualifying_docs
        WHERE species_code IS NOT NULL
      )
      GROUP BY mon_doc_defn_id
    ),
    defn_col_pairs AS (
      SELECT DISTINCT
        qd.mon_doc_defn_id,
        mded.column_defn_id
      FROM qualifying_docs qd
      JOIN marfissci.mon_doc_entrd_dets mded
        ON qd.mon_doc_id = mded.mon_doc_id
      WHERE mded.column_defn_id IS NOT NULL
      UNION
      SELECT DISTINCT
        qd.mon_doc_defn_id,
        leed.column_defn_id
      FROM qualifying_docs qd
      JOIN marfissci.log_efrt_std_info le
        ON qd.mon_doc_id = le.mon_doc_id
      JOIN marfissci.log_efrt_entrd_dets leed
        ON le.log_efrt_std_info_id = leed.log_efrt_std_info_id
      WHERE leed.column_defn_id IS NOT NULL
      UNION
      SELECT DISTINCT
        qd.mon_doc_defn_id,
        lsed.column_defn_id
      FROM qualifying_docs qd
      JOIN marfissci.log_efrt_std_info le
        ON qd.mon_doc_id = le.mon_doc_id
      JOIN marfissci.log_spc_std_info ls
        ON le.log_efrt_std_info_id = ls.log_efrt_std_info_id
      JOIN marfissci.log_spc_entrd_dets lsed
        ON ls.log_spc_std_info_id = lsed.log_spc_std_info_id
      WHERE lsed.column_defn_id IS NOT NULL
    ),
    cols_by_defn AS (
      SELECT
        mon_doc_defn_id,
        LISTAGG(TO_CHAR(column_defn_id), ', ') WITHIN GROUP (ORDER BY column_defn_id) AS column_defn_ids
      FROM defn_col_pairs
      GROUP BY mon_doc_defn_id
    )
    SELECT
      mdd.mon_doc_defn_id,
      mdd.short_doc_title,
      sbd.species,
      cbd.column_defn_ids
    FROM species_by_defn sbd
    JOIN marfissci.mon_doc_defns mdd
      ON sbd.mon_doc_defn_id = mdd.mon_doc_defn_id
    LEFT JOIN cols_by_defn cbd
      ON mdd.mon_doc_defn_id = cbd.mon_doc_defn_id
    ORDER BY mdd.mon_doc_defn_id
  ")
  
  res <- thecmd(cxn, sql_query)
  return(res)
}