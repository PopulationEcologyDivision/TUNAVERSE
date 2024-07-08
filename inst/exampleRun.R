BFT_Data <- prepareData(username = "<yourusername>", password = "<yourpassword>", 
                        data.dir = "C:/Users/McMahonM/OneDrive - DFO-MPO/data/wrangled", 
                        usepkg = 'roracle', update = F)
BFT_LOGS <- BFT_LOGS(BFT_Data)
gearByYear(BFT_LOGS) %>% View()
