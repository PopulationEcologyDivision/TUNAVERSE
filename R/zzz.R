.onAttach <- function(libname, pkgname) {
  #onAttach is for interactive sessions
  Mar.utils::updateCheck(gitPkg = 'PopulationEcologyDivision/TUNAVERSE')
  localVer = utils::packageDescription('TUNAVERSE')$Version
  packageStartupMessage(paste0("Version: ", localVer))
}

.onLoad <- function(libname, pkgname){
  # base_dir <- file.path("C:", "DFO-MPO")
  # pesd_tv_dir <- file.path(base_dir, "PESDData","Tunaverse")
  # if (!dir.exists(pesd_tv_dir)) dir.create(pesd_tv_dir, recursive = T)
}
get_pesd_tv_dir <- function() {
  #file.path("C:", "DFO-MPO", "PESDData","Tunaverse")
  file.path("R:/ATLSiteShares/SABS/LargePelagics/COMMERCIAL DATA/TUNAVERSE/data/")
}