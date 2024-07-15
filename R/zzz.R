.onAttach <- function(libname, pkgname) {
  #onAttach is for interactive sessions
  Mar.utils::updateCheck(gitPkg = 'PopulationEcologyDivision/TUNAVERSE')
  localVer = utils::packageDescription('TUNAVERSE')$Version
  packageStartupMessage(paste0("Version: ", localVer))
}