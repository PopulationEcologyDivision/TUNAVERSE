devtools::install_github('PopulationEcologyDivision/TUNAVERSE')
library(TUNAVERSE)
  BFT_Data <- prepareData(username = "<yourusername>", password = "<yourpassword>", 
                        data.dir = "C:/Users/McMahonM/OneDrive - DFO-MPO/data/wrangled", 
                        usepkg = 'roracle',force.extract  = F)
BFT_LOGS <- BFT_LOGS(BFT_Data)
library(dplyr)
gearByYear(BFT_LOGS) %>% View()
