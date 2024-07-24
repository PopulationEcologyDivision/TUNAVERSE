devtools::install_github('PopulationEcologyDivision/TUNAVERSE')
library(TUNAVERSE)
BFT_Data <- prepareData(username = "<yourusername>", password = "<yourpassword>", 
                        dsn="<yourdsn>", data.dir = "C:/Users/McMahonM/OneDrive - DFO-MPO/data/wrangled", 
                        usepkg = 'roracle',force.extract  = F)
#use something like below if data already extracted 
BFT_Data <- prepareData(data.dir = "C:/Users/McMahonM/OneDrive - DFO-MPO/data/wrangled")

BFT_LOGS <- BFT_LOGS(BFT_Data)
library(dplyr)
gearByYear(BFT_LOGS) %>% View()
