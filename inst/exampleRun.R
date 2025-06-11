cxn <- ROracle::dbConnect(DBI::dbDriver("Oracle"), oracle.username, oracle.password, "PTRAN")
BFT_Data <- prepareData(cxn, force.extract  = F,extract_user = "")
devtools::load_all()
source("C:/git/Maritimes/Mar.utils/R/get_data_tables.R")
source("C:/git/Maritimes/Mar.utils/R/zzz.R")

BFT_Data <- prepareData(cxn, force.extract  = F,extract_user = "", ex)

BFT_LOGS <- BFT_LOGS(BFT_Data)
library(dplyr)
gearByYear(BFT_LOGS) %>% View()
