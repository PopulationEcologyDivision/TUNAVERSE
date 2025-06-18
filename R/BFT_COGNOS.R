#' @title BFT_COGNOS
#' @description This function loads the most recent extracted COGNOS data and
#' optionally returns a QAQC'd data object. At the moment there is no slick way to link and
#' extract reports from this southern Gulf of St. Lawrence fishery database. 
#' @param QAQC Logical. If true (the default), the COGNOS report is QAQC'd. If false,
#' you get the original data with all its warts.
#' @author  Alex Hanke, \email{Alex.Hanke@@dfo-mpo.gc.ca}
#' @importFrom magrittr %>%
#' @export
BFT_COGNOS = function(QAQC=TRUE) {
  require(stringr)
  require(data.table)
  require(readit)
  require(lubridate)
  require(plyr)
  require(dplyr)
  
  data_path="R:/ATLSiteShares/SABS/LargePelagics/COMMERCIAL DATA/TUNAVERSE/data/"
  
  if(QAQC==FALSE){
    # Return the loaded data
    # Read data found on logbook version 1 (2002 until 20XX)
    GULF1 = data.table(readit(.data=paste0(data_path,"DW - Tuna Details - GSSSB381.xlsx")))
    setnames(GULF1, gsub(" ", "_", names(GULF1)))
    attr(GULF1,"comment") = "GULF1"
    
    # Read data found on logbook version 2 (2022 to present)
    GULF2 = data.table(readit(.data=paste0(data_path,"DW - Tuna V2022 Details - GSSSB385.xlsx")))
    setnames(GULF2, gsub(" ", "_", names(GULF2)))
    GULF2 = GULF2[YEAR>2021&MON_DOC_DEFN_ID==124,]
    attr(GULF2,"comment") = "GULF2"
    
    GHIST = data.table(readit(.data=paste0(data_path,"Historical_Gulf_Feb 19 2016.xlsx" )))
    attr(GHIST,"comment") = "GHIST"
    
    rawDATA = list(rawGULF1=GULF1, rawGULF2=GULF2, rawGHIST=GHIST)
    attr(rawDATA,"comments") =
      "Raw historical, logbook 1 and logbook 2 BFT fishery data."
    
    return(rawDATA)
    
  }else{
    # Read data found on logbook version 1 (2002 until 20XX)
    GULF1 = data.table(readit(.data=paste0(data_path,"DW - Tuna Details - GSSSB381.xlsx")))
    setnames(GULF1, gsub(" ", "_", names(GULF1)))
    attr(GULF1,"comment") = "GULF1"
    
    # Read data found on logbook version 2 (2022 to present)
    GULF2 = data.table(readit(.data=paste0(data_path,"DW - Tuna V2022 Details - GSSSB385.xlsx")))
    setnames(GULF2, gsub(" ", "_", names(GULF2)))
    GULF2 = GULF2[YEAR>2021&MON_DOC_DEFN_ID==124,]
    attr(GULF2,"comment") = "GULF2"
    
    GHIST = data.table(readit(.data=paste0(data_path,"Historical_Gulf_Feb 19 2016.xlsx" )))
    attr(GHIST,"comment") = "GHIST"
    
    ComCodes = data.table(readit(.data =paste0(data_path,"Reference_Community_Ports_updated_20151216.xlsx"), sheet="for R", skip = 0))
    ComCodes[,PortName:=gsub("\\*","",COMMUNITY_NAME)]
    ComCodes[, PROVINCE_LANDED:= as.numeric(str_sub(COMMUNITY_CODE,1,1))]
    ComCodes[, DISTRICT_LANDED:= as.numeric(str_sub(COMMUNITY_CODE,2,3))]
    ComCodes[, PORT_LANDED:= as.numeric(str_sub(COMMUNITY_CODE,4,5))]
    
    ###########################################################################
    #
    # CURRENT GULF DATA QAQC
    #
    ###########################################################################
    prepareGULF = function(data) {
    GULF = data
    # Year
    GULF[,Year:=year(parse_date_time(DATE_SAILED,"Ymd HMS",truncated = 3))]
    GULF[is.na(Year), Year:=year(parse_date_time(LANDED_DATE_CN,"Ymd HMS",truncated = 3))] 
    GULF[is.na(Year), Year:=year(parse_date_time(LANDED_DATE_DMC,"Ymd HMS",truncated = 3))]
    
    # Gear
    if(attr(data,"comment") == "GULF1"){
    GULF[GEAR_CODE_MDD%in%c("58","CANNE ET MOULINET (CHUMMING)","CHUMMING","ROD AND REEL (CHUMMING)","RRC","ROD N REEL","RR","ROD","1","11"), GEAR_CODE_MDD:= "58"]
    GULF[GEAR_CODE_MDD%in%c("ANG","A","ANGLING","0","10","25","7"),GEAR_CODE_MDD:= "60" ]
    GULF[GEAR_CODE_MDD%in%c("LIGNE TENDUE","TENDED LINE"),GEAR_CODE_MDD:= "54" ]
    GULF[GEAR_CODE_MDD%in%c("ELECTRIC HARPOON"), GEAR_CODE_MDD:= "85"]
    # 95% Harpoon records have hook type and size info. Looks like transposition error
    GULF[GEAR_CODE_MDD%in%c("85"), GEAR_CODE_MDD:= "58"]
    
    GULF[GEAR_LOGD%in%c("ROD AND REEL (CHUMMING","ELECTRIC HARPOON","58","CANNE ET MOULINET (CHUMMING)","CHUMMING"), GEAR_LOGD:= "ROD AND REEL (CHUMMING)"]
    GULF[GEAR_LOGD%in%c("60"),GEAR_LOGD:= "ANGLING"]
    GULF[GEAR_LOGD%in%c("LIGNE TENDUE"),GEAR_LOGD:= "TENDED LINE"]
    
    GULF[is.na(GEAR_CODE_MDD), GEAR_CODE_MDD:= GEAR_LOGD]
    GULF[GEAR_CODE_MDD=="ANGLING",GEAR_CODE_MDD:= "60"]
    GULF[GEAR_CODE_MDD=="ROD AND REEL (CHUMMING)",GEAR_CODE_MDD:= "58"]
    GULF[is.na(GEAR_CODE_MDD), GEAR_CODE_MDD:= "99"]
    GULF[LICENCE_ID_MD=="027231"&GEAR_CODE_MDD=="H10"&Year==2022,GEAR_CODE_MDD:= "58"]
    GULF[, Gear := GEAR_CODE_MDD]
    }else{
    GULF[STRATEGY%in%c("RRC","CH","CHUMMING","C","AMORÇAGE (CHUMMING)"), 
         STRATEGY:="CHUM"]
    GULF[STRATEGY%in%c("SQUID DRIFT", "SQUID, DR", "D","DÉRIVE","RRD","DR",
         "DRIFTING","DD","C/D","C&D","D/C"), STRATEGY:="DRIFT"]
    GULF[STRATEGY%in%c("T","TL","TR","RRT","TROWLING","TROWL","TROLL",
         "TROLLING","TRAWL","TROLLING/SQUID BAIT",
         "SQUID TROLLING","DOWN","DOWN BAIT","DOWN ROD",
         "DOWN ROD/SQUIDS","SQUID","SQUIDS","SQUID RODS"), 
         STRATEGY:="TROLL"]
    GULF[STRATEGY%in%c("R","R&R","RR","ANGLE","LIVE BAIT"), 
         STRATEGY:="ROD AND REEL"]
    GULF[STRATEGY%in%c("DOWN ROD -KITE","DRIFT KITE","TRACKIE & KITE",
         "KITE  & TRACKIE","KITE & TRACKIE",
         "TRACADIE","TRACKIE"), STRATEGY:="KITE"]
    GULF[STRATEGY%in%c("CATCH","CATCH ONE","UNKNOWN","A","B","P",
         "SIT AND WAIT","SPAG"), STRATEGY:="UKN"]
    GULF[STRATEGY%in%c("A/C"), STRATEGY:="ANCHOR"]
    GULF[STRATEGY%in%c("DRIFT TROLL","TROLL/DRIFT","TROLL DRIFT"), 
         STRATEGY:="DRIFT"]
    GULF[, Gear := ifelse(STRATEGY=="TROLL","RRtroll","RRchum")]
    GULF[is.na(Gear), Gear:=GEAR_LOGD]
    GULF[is.na(Gear), Gear:=99]
    }
    
    # NAFO
    if(attr(data,"comment") == "GULF1"){
    GULF[, NAFO := NAFO_MDD]
    #GULF[is.na(NAFO), NAFO := NAFO_CN]
    GULF[is.na(NAFO), NAFO := NAFO_DMC]
    GULF[is.na(NAFO), NAFO := NAFO_MD]
    GULF[is.na(NAFO), NAFO := NAFO_MDD]
    #GULF[is.na(NAFO), NAFO := NAFO_LOG_1]
    #GULF[is.na(NAFO), NAFO := NAFO_LOG_2]
    GULF[grep("/",NAFO), NAFO := "4RST"]
    GULF[grep("TT",NAFO), NAFO := "4TG"]
    GULF[grep("T2",NAFO), NAFO := "4T"]
    GULF[grep("7G",NAFO), NAFO := "4TG"]
    GULF[grep("T5",NAFO), NAFO := "4T"]
    GULF[grep("4T6",NAFO), NAFO := "4TG"]
    GULF[grep("TC",NAFO), NAFO := "4TG"]
    GULF[grep("4G",NAFO), NAFO := "4TG"]
    GULF[grep("45TG",NAFO), NAFO := "4TG"]
    GULF[grep("4TU",NAFO), NAFO := "4T"]
    GULF[grep("TG",NAFO), NAFO := "4TG"]
    GULF[grep("4TRST",NAFO), NAFO := "4RST"]
    GULF[grep("4JG",NAFO), NAFO := "4TG"]
    GULF[grep("3TL",NAFO), NAFO := "4TL"]
    GULF[grep("2",NAFO), NAFO := "4TG"]
    GULF[grep("6",NAFO), NAFO := "4TL"]
    GULF[NAFO%in%c("1",".5","9","10","2HE","0.5",""), NAFO := NA]
    GULF[is.na(NAFO), NAFO := "UKN"]
    }else{
      GULF[, NAFO := NAFO_DMC]
      GULF[is.na(NAFO), NAFO := NAFO_LOGD]
      GULF[is.na(NAFO), NAFO := "UKN"]
    }
    
    # HMA
    GULF[,HMA := HOME_MGT_AREA]
    GULF[HMA%in%c("NE-G"), HMA := "GNS"]
    GULF[grep("GNSW",HMA), HMA := "GNS"]
    GULF[HMA%in%c("4TG","","4T","4NS"), HMA := NA]
    GULF[HMA%in%c("PE","P","PEIE"), HMA:="PEI"]
    GULF[HMA%in%c("2","1"), HMA:="PEI"]
    GULF[HMA=="PPEI", HMA:="PEI"]
    GULF[HMA=="NB-G", HMA:="GNB"]
    GULF[HMA%in%c("NB","GMB","3"), HMA:="GNB"]
    GULF[HMA=="QC", HMA:="PQ"]
    GULF[HMA%in%c("NL","NF"), HMA:="NFLD"]
    GULF[is.na(HMA), HMA:= "UKN"]
    
    # Port
    GULF[,Port:= as.character(WHARF_COMM_CODE_SAILED_CN)]; GULF[is.na(Port),.N]
    GULF[is.na(Port),Port:= as.character(PORT_SAILED_CN)]; GULF[Port=="",.N]
    GULF[is.na(Port),Port := PORT_SAILED]; GULF[is.na(Port),.N] 
    GULF[is.na(Port), Port:=as.character(WHARF_COMM_CODE_LANDED)]; GULF[is.na(Port),.N]
    GULF[,Port:=gsub(pattern = "^ +", replacement = "",Port)] # remove leading blank
    
    # Numbers
    GULF[str_detect(Port,"^395044$"),Port:="39504"]
    GULF[str_detect(Port,"^388203$"),Port:="38820"]
    GULF[str_detect(Port,"^338824$"),Port:="38824"]
    GULF[str_detect(Port,"^3882$"),Port:="38820"]
    GULF[str_detect(Port,"^38/820$"),Port:="38820"]
    GULF[str_detect(Port,"^3913$"),Port:="39213"]
    GULF[str_detect(Port,"^387520$"),Port:="38720"]
    GULF[str_detect(Port,"^339212$"),Port:="39212"]
    GULF[str_detect(Port,"^339230$"),Port:="39230"]
    GULF[str_detect(Port,"^339606$"),Port:="39606"]
    GULF[str_detect(Port,"^339214$"),Port:="39214"]
    GULF[str_detect(Port,"^339504$"),Port:="39504"]
    GULF[str_detect(Port,"^2108$"),Port:="ARISAIG"]
    GULF[str_detect(Port,"^1200$"),Port:="38821"]
    GULF[str_detect(Port,"^1121$"),Port:="11321"]
    GULF[str_detect(Port,"^103087$"),Port:="10308"]
    GULF[str_detect(Port,"^0600$"),Port:="39504"]
    GULF[str_detect(Port,"^0630$"),Port:="28701"]
    GULF[str_detect(Port,"^0000$|^00000$|^0$|^2$"),Port:="00000"]
    GULF[str_detect(Port,"^12$"),Port:="NORTH RUSTICO"]
    
    # A
    GULF[grep("ANT H|ANTIGONISH H|ANITGONISH HARBOUR",Port),Port:="ANTIGONISH HARBOUR"]
    GULF[grep("ARFISAIG|ARISAG|ARSAG|ARISAIG|ARSAIG|ASASAIG|ARASAIG",Port),Port:="ARISAIG"]
    GULF[grep("AULDS",Port),Port:="AULD'S COVE"]
    # B
    GULF[grep("BALLAENTYNES|BALLYNETYNES|BALLANATYNES|BALLANT|BALLINT|BALLEN|BALENT|BALLNET|BALLAT|BALANT|VALENTINES|BALLEBNTYNES COVE",Port),Port:="BALLANTYNE'S COVE"]
    GULF[grep("BAL;LENT|BAL;LANT",Port),Port:="BALLANTYNE'S COVE"]
    GULF[grep("BLLANT|BLLENT|BLLENY",Port),Port:="BALLANTYNE'S COVE"]
    GULF[grep("BASLLANT",Port),Port:="BALLANTYNE'S COVE"]
    GULF[grep("BVALLENT",Port),Port:="BALLANTYNE'S COVE"]
    GULF[grep("BLAAENT",Port),Port:="BALLANTYNE'S COVE"]
    GULF[grep("BAY ST LAWRENCE",Port),Port:="BAY ST. LAWRENCE"]
    GULF[grep("BEACH",Port),Port:="BEACH POINT"]
    GULF[grep("PIG COVE",Port),Port:="BIG COVE"]
    GULF[grep("BAXTERS COVE",Port),Port:="BAXTER'S COVE"]
    # C
    GULF[grep("CAPE TOURMENTINE",Port),Port:="CAPE TORMENTINE"]
    GULF[grep("CAPR GEORGE|GAPE GEORGE",Port),Port:="CAPE GEORGE"]
    GULF[grep("CAPE GE",Port),Port:="CAPE GEORGE"]
    GULF[grep("CARIBOU|CARRIBOU",Port),Port:="CARIBOU HARBOUR"]
    GULF[grep("CHANCE HARBOUT",Port),Port:="CHANCE HARBOUR"]
    GULF[grep("CRIBBANS|CRIBBON|CIBBONS|CRIBBINS|CROBBONS",Port),Port:="CRIBBENS POINT"]
    GULF[grep("CRIBBIONS|CRRBBONS POINT",Port),Port:="CRIBBENS POINT"]
    GULF[grep("CRIBBEN|CRIBBERS",Port),Port:="CRIBBENS POINT"]
    GULF[grep("BRIBBONS",Port),Port:="CRIBBENS POINT"]
    GULF[grep("CRIBENS",Port),Port:="CRIBBENS POINT"] 
    GULF[grep("CRIBONS",Port),Port:="CRIBBENS POINT"]
    GULF[grep("COLE MINERS|COAL MINER|COAL MINES|COALMINES",Port),Port:="COAL MINES"]
    # F
    GULF[grep("FINLAYS|FINLEY|FINDLEYS|FINELY POINT",Port),Port:="FINLAY POINT"]
    # G
    GULF[grep("(GRANDE-ILE)",Port),Port:="GRANDE-ILE"]
    GULF[grep("GRAHAMS POND",Port),Port:="GRAHAM'S POND"]
    GULF[grep("GRAND ENTRY|GRANDE-ENTREE|GRANDE ENTREE|GRANS ENTRE IM|GRAND ENTRE",Port),Port:="GRANDE-ENTREE"]
    GULF[grep("GASCON",Port),Port:="GASCONS"]
    GULF[grep("GROSSE I",Port),Port:="GROSSE-ILE"]
    #GULF[grep("GOV",Port),Port:="GOVERNMENT WHARF"]
    # H
    GULF[grep("HARVE BOUCHER|HARVER BOUCHER",Port),Port:="HAVRE BOUCHER"]
    # I
    GULF[grep("ENTRY ISLAND|ILE D'ENTREE",Port),Port:="ILE D'ENTREE"]
    GULF[grep("INVERNESS|INVRNESS|INERNESS",Port),Port:="INVERNESS"]
    # J
    GULF[grep("LITTLE JDUQIUE HARBOUR|LITTLE JUDIOQUE HARBOUR|LITTLE JUDOQUE HARBOUR|LITTLE JUDIQUE HAROUR",Port),Port:="LITTLE JUDIQUE HARBOUR"]
    # L
    GULF[grep("LISMORE|LISEMORE",Port),Port:="LISMORE"]
    GULF[grep("LIVINGSTON",Port),Port:="LIVINGSTONE'S COVE"]
    GULF[grep("LOGAN",Port),Port:="LOGAN'S POINT"]
    GULF[grep("JUDIQUE HARBOUR|JUDIQUE HBR|LITTTLE JUDIQUE HARBOUR",Port),Port:="LITTLE JUDIQUE HARBOUR"]
    GULF[grep("LITTTLE JUDIQUE POND|LITTLE JUDIQUE POND",Port),Port:="LITTLE JUDIQUE PONDS"]
    # M
    GULF[grep("MABO COAL MINE|MABO MINES|MABOU COAL MINE|MABOU MINES|COAL MINES",Port),Port:="MABOU MINES"]
    GULF[grep("MABOU HARBOUR`|MABOUR HARBOUR",Port),Port:="MABOU HARBOUR"]
    GULF[grep("^MABOO$",Port),Port:="MABOU"]
    GULF[grep("MARRY'SVILLE|MARY VILLE|MARYVALE|MARYVALLE",Port),Port:="MARYVILLE"]
    GULF[grep("MONTAGUE|MONTEGUE",Port),Port:="MONTAGUE"]
    GULF[grep("MORREL",Port),Port:="MORELL"]
    GULF[grep("MURHYS|MURPHY|MUPHYS|MUPRHY|MURPHU",Port),Port:="MURPHY'S POND"] 
    GULF[grep("MURRY HARBOUR",Port),Port:="MURRAY HARBOUR"]
    GULF[grep("MARGARE",Port), Port:="MARGAREE"]
    GULF[grep("MARGAREE HBR|MARGARE HARBOUR",Port), Port:="MARGAREE HARBOUR"]
    GULF[grep("MACHONS POINT",Port), Port:="MACHON POINT"]
    # N
    GULF[grep("NAUFAG|NAUFAGE|NAUFRAG|NEW FRAUGE|NEW FRAG PEI|NUFRAGE",Port),Port:="NAUFRAGE"]
    GULF[grep("^.8820$|NORTHLAKE|NORTH LKAE|NORTH LAKE|NORHT LAKE|NORT LAKE|NORTH LAK|NORTH  LAKE|NORTH LLAKE|NORTHLKAE",Port),Port:="NORTH LAKE"]
    GULF[grep("NORTH RUSTIO",Port), Port:="NORTH RUSTICO"]
    # P
    GULF[grep("PETIT-SHIPPAGANQ|PETIT-SHIPPAGAN",Port),Port:="PETIT SHIPPAGAN"] 
    GULF[grep("PICTOU IS",Port),Port:="PICTOU ISLAND EAST"]
    GULF[grep("PORT  HOOD|PORT OOD|PORTHOOD|PORT HOOD ISLAND",Port),Port:="PORT HOOD"]
    #R
    GULF[grep("REDHEAD|^RE$",Port),Port:="RED HEAD"]
    # S
    GULF[grep("SEA COW POND",Port),Port:="SEACOW POND"]
    GULF[, Port:=gsub("SKINNERS","SKINNER'S",Port)]
    GULF[grep("STE-THRâSE-DE-GASP",Port),Port:="STE-THERESE-DE-GASPE"]
    GULF[grep("SUREY",Port),Port:="SOURIS"]
    GULF[grep("GASCONS",Port),Port:="SHIPPAGAN"]
    GULF[grep("SINCLAIRS ISLAND",Port),Port:="SINCLAIR ISLAND"]
    # T
    GULF[grep("TIGNISH RIN|TIGNIS R",Port),Port:="TIGNISH RUN"]
    GULF[grep("^TIDNISH$",Port),Port:="TIGNISH"]
    # U
    GULF[grep("UN|CAP-ARPALIRTUQ",Port),Port:="UNKNOWN"]
    GULF[Port=="",Port:="UNKNOWN"]
    
    # Check list (GULF[order(Port),.(unique(Port))])$V1
    
    GULF[, Port2:= as.numeric(Port)]
    GULF[, Port3:= ComCodes[match(Port2,ComCodes[,COMMUNITY_CODE]),COMMUNITY_NAME] ] # names
    GULF[is.na(Port3), Port3:= Port]
    GULF[, Port2:= ComCodes[match(Port3,ComCodes[,COMMUNITY_NAME]),COMMUNITY_CODE] ] # codes
    GULF[is.na(Port2),Port2:= "99999"]
    GULF[is.na(Port3),Port3:= "UNKNOWN"]
    
    # Jday 
    GULF[, Jday :=  yday(parse_date_time(DATE_SAILED,"Ymd HMS",truncated = 3))]
    GULF[is.na(Jday), Jday :=  yday(parse_date_time(LANDED_DATE_DMC,"Ymd HMS",truncated = 3))]
    GULF[is.na(Jday), Jday :=  yday(parse_date_time(LANDED_DATE_CN,"Ymd HMS",truncated = 3))]
    
    # Effort
    if(attr(data,"comment") == "GULF1"){
    GULF[, Effort:=as.numeric(TOTAL_HOURS_FISHED)]
    GULF[is.na(Effort)&as.numeric(ymd(LANDING_DATE)-(ymd(DATE_SAILED)),"hours")>0,
         Effort := 24-as.numeric(hm(TIME_SAILED),"hours")+
           as.numeric(hm(LANDED_TIME_DMC),"hours")]
    GULF[is.na(Effort),Effort := 
           as.numeric(hm(LANDED_TIME_DMC)-hm(TIME_SAILED),"hours")]
    GULF[is.na(Effort), Effort :=
           as.numeric(hm("20:00")-(hm(TIME_SAILED)),"hours")]
    GULF[is.na(Effort)&as.numeric(hm("05:00")-hm(LANDED_TIME_DMC),"hours")>0, Effort :=
           4+as.numeric(hm(LANDED_TIME_DMC),"hours")]
    GULF[is.na(Effort), Effort :=
           as.numeric(hm(LANDED_TIME_DMC)-(hm("05:00")),"hours")]
    GULF[is.na(Effort)|Effort<0, Effort := 8]
    }else{
      GULF[as.numeric(hm(END_TIME)-(hm(START_TIME)),"hours")<0,
           Effort := 24-as.numeric(hm(START_TIME), "hours")+
             as.numeric(hm(END_TIME),"hours")]
      GULF[is.na(Effort),Effort := 
             as.numeric(hm(END_TIME)-(hm(START_TIME)),"hours")]
      GULF[is.na(Effort)&as.numeric(ymd(LANDING_DATE)-(ymd(DATE_SAILED)),"hours")>0,
           Effort := 24-as.numeric(hm(TIME_SAILED), "hours") +
             as.numeric(hm(LANDED_TIME_DMC),"hours")]
      GULF[is.na(Effort),Effort := 
             as.numeric(hm(LANDED_TIME_DMC)-hm(TIME_SAILED),"hours")]
      GULF[is.na(Effort), Effort :=
             as.numeric(hm("20:00")-(hm(START_TIME)),"hours")]
      GULF[is.na(Effort), Effort :=
             as.numeric(hm("20:00")-(hm(TIME_SAILED)),"hours")]
      GULF[is.na(Effort)|Effort<0, Effort := 8] # 4 records 
    }
    
    # drop rows where Jday is NA
    GULF = GULF[!is.na(Jday)]
    
    # Fix Tag number
    GULF[, TAG_ID:= TAG_NO]
    GULF[is.na(TAG_ID), TAG_ID:=TAG_NO_DMC]
    GULF[is.na(TAG_ID), TAG_ID:=TAG_NO_CN]
    GULF[,Count := ifelse(!is.na(TAG_ID)|RND_WEIGHT_KGS_SLIP>0|
                          EST_WEIGHT_KGS_CN>0 ,1,0)]
    GULF[is.na(Count),Count := 0]
    
    # Fix Strikes
    if(attr(data,"comment") == "GULF1"){
    GULF[,Strikes := as.numeric(NO_OF_STRIKES)]
    GULF[Strikes>=20, Strikes:=round(Strikes/10)]
    }else{
    GULF[,Strikes := ifelse(is.na(NO_OF_STRIKES),0,
          as.numeric(NO_OF_STRIKES))]
    }
    
    # More Housekeeping
    if(attr(data,"comment") == "GULF1"){
    GULF[, GEAR := factor(dplyr::recode(Gear, '81'="HARP",
                                 '85'="HARP",
                                 '41'="GILL",
                                 '42'="GILL",
                                 '99'="UKN",
                                 '54'="TL",
                                 '58'="RRchum",
                                 '60'="RRtroll",
                                 '61'="TRAP",
                                 '12'="TRAWL",
                                 '63'="WEIR"))] 
    }else{  
      GULF[, GEAR := factor(dplyr::recode(Gear,
                        'CANNE ET MOULINET (CHUMMING)'="RRchum",
                        'INCONNU'="UKN",
                        '99'="UKN",
                        'LIGNE TENDUE'="TL",
                        'UKN'="UKN",
                        'TENDED LINE'="TL",
                        'ROD AND REEL (CHUMMING)'="RRchum",
                        'ANGLING'="RRtroll"))]  
      }
    GULF[, DOYs := yday(parse_date_time(DATE_SAILED,"Ymd HMS",truncated = 3))]
    GULF[, DOYl := yday(parse_date_time(LANDING_DATE,"Ymd HMS",truncated = 3))]
    GULF[, Month := month(parse_date_time(DATE_SAILED,"Ymd HMS",truncated = 3))]
    
    # a trip is:
    GULF[,TRIP := paste(Year,VRN,DOYs,DOYl,sep="_")]
    
    # # Data for analysis
    # temp = GULF[grep("4T",NAFO),.(Count=sum(!is.na(TAG_ID),na.rm=T),Effort=sum(Effort,na.rm=T)), by = Year]
    # temp[,CPUE := Count/Effort]
    # 
    # temp2 = GULF[grep("4X",NAFO,invert=T), .(BFT_c = 1*!is.na(TAG_ID), Effort = as.numeric(Effort), Year = Year, Gear = as.numeric(Gear), NAFO = NAFO, Port = as.numeric(Port2), HMA = HMA, Jday=Jday)]
    # temp2 = temp2[apply(is.na(temp2),1,sum)==0,]
    return(GULF)
    }
    
    GULF1 = prepareGULF(GULF1)
    GULF2 = prepareGULF(GULF2)
    ###########################################################################
    #
    # HISTORICAL GULF DATA QAQC
    #
    ###########################################################################
    # Year
    GHIST[YEAR_SAILED == 98, YEAR_SAILED := 1998]
    GHIST[,YEAR_LANDED:= YEAR_SAILED]
    GHIST[,Year:=YEAR_SAILED]
    
    # Fix 2002 dates
    GHIST[YEAR==2002, MONTH_SAILED:= month(parse_date_time(DAY_SAILED,"dmy",truncated = 3))]
    GHIST[YEAR==2002, MONTH_LANDED:= month(parse_date_time(DAY_LANDED,"dmy",truncated = 3))]
    GHIST[YEAR==2002, YEAR_LANDED:= year(parse_date_time(DAY_LANDED,"dmy",truncated = 3))]
    GHIST[YEAR==2002, DAY_LANDED:= day(parse_date_time(DAY_LANDED,"dmy",truncated = 3))]
    GHIST[YEAR==2002, DAY_SAILED:= day(parse_date_time(DAY_SAILED,"dmy",truncated = 3))]
    
    
    # Complete province, district and port landed fields
    GHIST[is.na(PROVINCE_LANDED), PROVINCE_LANDED:= as.numeric(str_sub(COMMUNITY_LANDED,1,1))]
    GHIST[is.na(DISTRICT_LANDED), DISTRICT_LANDED:= as.numeric(str_sub(COMMUNITY_LANDED,2,3))]
    GHIST[is.na(PORT_LANDED), PORT_LANDED:= as.numeric(str_sub(COMMUNITY_LANDED,4,5))]
    
    # DROP records where province landed is NS and District ranges from 14 to 44 and where the NAFO area is 4VWX5YZ
    GHIST[, NAFO:= toupper(NAFO)]
    GHIST = GHIST[!(PROVINCE_LANDED%in%c(0,1,9)&DISTRICT_LANDED%in%c(0,14:44,99)&str_sub(NAFO,1,2)%in%c("5Z","5Y","4X","4V","4W","UK","WD","99","NA"))]
    GHIST = GHIST[!str_sub(NAFO,1,2)%in%c("5Z")]
    GHIST = GHIST[!(str_sub(NAFO,1,2)%in%c("3L","3N","3O","3P"))] # These fish caught on Grand Banks
    
    # NAFO
    GHIST[NAFO%in%c("GNS","GBS","TG"), NAFO:= "4T"]
    GHIST[NAFO%in%c("4YG","44","4"), NAFO:= "4TG"]
    GHIST[NAFO%in%c("4XS","4T1","41"), NAFO:= "4TL"]
    # Not sure I should have reassigned 4X,4V, 4VN, 4W, 4WD to Gulf. The ports landed are in the Gulf
    # The hours fished are a bit high and could be due to improper location of decimal
    GHIST[DISTRICT_LANDED%in%c(88,87,86,11,3,13,12)&NAFO%in%c("_","4VN","4V","4W","4WD","4X"), NAFO:= "4TG"]
    GHIST[DISTRICT_LANDED%in%c(96)&NAFO%in%c("_","4VN","4V","4W","4WD","4X"), NAFO:= "4TJ"]
    GHIST[DISTRICT_LANDED%in%c(92)&NAFO%in%c("_","4VN","4V","4W","4WD","4X"), NAFO:= "4TL"]
    GHIST[DISTRICT_LANDED%in%c(0)&NAFO%in%c("_"), NAFO:= "4T"]
    
    GHIST = GHIST[!str_sub(NAFO,1,2)%in%c("4V","4W","4X","NA")]
    
    # Gear
    GHIST[GEAR%in%c(18,81,85), Gear2:= "ELECTRIC HARPOON"]
    GHIST[GEAR%in%c(22), Gear2:= "ROD AND REEL (CHUMMING)"]
    GHIST[GEAR%in%c(99), Gear2:= "TROLL/LL"]
    GHIST[GEAR%in%c(37,54,36), Gear2:= "TENDED LINE"]
    GHIST[GEAR%in%c(60,61), Gear2:= "ANGLING"]
    
    # Jday (assume trips are a day long when filling in gaps)
    GHIST[is.na(DAY_SAILED), DAY_SAILED := DAY_LANDED]
    GHIST[is.na(MONTH_SAILED), MONTH_SAILED := MONTH_LANDED]
    GHIST[MONTH_SAILED==9&DAY_SAILED==31,DAY_SAILED:=30]
    GHIST[, Jday :=  yday(parse_date_time(paste(YEAR_SAILED,MONTH_SAILED,DAY_SAILED,sep="-"),"Ymd HMS",truncated = 3))]
    
    # BFT caught
    GHIST[LANDED_WEIGHT_CODE==3, LANDED_WEIGHT :=  6.19709 + (LANDED_WEIGHT*1.23034)] # ICCAT DWT to RWT conversion
    GHIST[is.na(ROUND_WEIGHT), ROUND_WEIGHT:=0]
    GHIST[is.na(LANDED_WEIGHT), LANDED_WEIGHT:=0]
    GHIST[LANDED_WEIGHT>0, ROUND_WEIGHT:= LANDED_WEIGHT]
    GHIST[is.na(TOTAL_TRIP_WEIGHT), TOTAL_TRIP_WEIGHT:=0]
    GHIST[TOTAL_TRIP_WEIGHT>2000,TOTAL_TRIP_WEIGHT:=TOTAL_TRIP_WEIGHT-10000]
    # GHIST[TOTAL_TRIP_WEIGHT==0, TOTAL_TRIP_WEIGHT:=ROUND_WEIGHT]
    # GHIST[ROUND_WEIGHT==0,ROUND_WEIGHT:=TOTAL_TRIP_WEIGHT]
    # GHIST[,plot(TOTAL_TRIP_WEIGHT,ROUND_WEIGHT)]
    # GHIST[,plot(TOTAL_TRIP_WEIGHT,TOTAL_BFT_CAUGHT)]
    # GHIST[,plot(ROUND_WEIGHT,TOTAL_BFT_CAUGHT)]
    
    GHIST[as.numeric(TAG_NO)==0, TAG_NO := NA]
    GHIST[,Count := ifelse(!is.na(TAG_NO)|ROUND_WEIGHT>0,1,0)]
    GHIST[is.na(Count),Count := 0]
    
    # Strikes: add these to what was caught for additional perspective on abundance
    GHIST[is.na(NO_STRIKES),NO_STRIKES := 0]
      
    # Effort (assume trips are only a single day when filling gaps)
    GHIST[is.na(TIME_SAILED)|TIME_SAILED==0,TIME_SAILED:=700]
    GHIST[is.na(TIME_LANDED)|TIME_LANDED==0,TIME_LANDED:=2000]
    GHIST[,TIME_SAILED := str_pad(as.character(TIME_SAILED),4,"left",pad=0)]
    GHIST[,TIME_LANDED := str_pad(as.character(TIME_LANDED),4,"left",pad=0)]
    GHIST[,TIME_SAILED := paste(substr(TIME_SAILED,1,2),substr(TIME_SAILED,3,4),sep=":")]
    GHIST[,TIME_LANDED := paste(substr(TIME_LANDED,1,2),substr(TIME_LANDED,3,4),sep=":")]
    GHIST[is.na(DAY_LANDED), DAY_LANDED := DAY_SAILED]
    GHIST[is.na(MONTH_LANDED), MONTH_LANDED := MONTH_SAILED]
    GHIST[DAY_LANDED==0, DAY_LANDED := DAY_SAILED]
    GHIST[MONTH_LANDED==0, MONTH_LANDED := MONTH_SAILED]
    GHIST[MONTH_LANDED==9&DAY_LANDED==31,DAY_LANDED:=30]
    GHIST[TIME_LANDED=="27:00", TIME_LANDED:="20:00"]
    
    # add in the port names
    GHIST = merge(GHIST, ComCodes[,.(PROVINCE_LANDED,DISTRICT_LANDED,
            PORT_LANDED,PORT=PortName)], by= c("PROVINCE_LANDED",
            "DISTRICT_LANDED","PORT_LANDED"), all.x=T)
    
    GHIST[, HMA := HMA1]
    GHIST[is.na(HMA), HMA := HMA2]
    GHIST[, HMA := factor(dplyr::recode(HMA, '1'="SF",
                                 '2'="PEI",
                                 '3'="PQ",
                                 '4'="NL",
                                 '5'="GNS",
                                 '6'="GNB",
                                 '7'="SMB"))] 
    
    GHIST[, Gear2 := recode(Gear2, 'ANGLING'="RRtroll",
                            'ELECTRIC HARPOON'="HARP",
                            'TROLL/LL'="RRtroll",
                            'TENDED LINE'="TL",
                            'ROD AND REEL (CHUMMING)'="RRchum"
    )] 
    GHIST[, DOYs := yday(parse_date_time(paste(YEAR_SAILED,MONTH_SAILED,DAY_SAILED,sep="-"),"Ymd HMS",truncated = 3))]
    GHIST[, DOYl := yday(parse_date_time(paste(YEAR_LANDED,MONTH_LANDED,DAY_LANDED,sep="-"),"Ymd HMS",truncated = 3))]
    GHIST[, Month := month(parse_date_time(paste(YEAR_SAILED,MONTH_SAILED,DAY_SAILED,sep="-"),"Ymd HMS",truncated = 3))]
    GHIST[,RWT := ROUND_WEIGHT]
    GHIST[is.na(RWT), RWT := mean(c(WEIGHT1,WEIGHT2),na.rm=T)]
    GHIST[is.na(HMA), HMA := "UKN"]
    
    # fix effort
    GHIST[HOURS_FISHED==0, HOURS_FISHED := NA]
    GHIST[, Effort := HOURS_FISHED]
    GHIST[Year<1997, Effort := HOURS_FISHED/10]
    GHIST[is.na(Effort),Effort := as.numeric(
      parse_date_time(paste(YEAR_LANDED,MONTH_LANDED,DAY_LANDED,TIME_LANDED,sep="-"),"Ymd HMS",truncated = 3) -
        parse_date_time(paste(YEAR_SAILED,MONTH_SAILED,DAY_SAILED,TIME_SAILED,sep="-"),"Ymd HMS",truncated = 3),"hours")]
    GHIST[is.na(Effort), Effort:=as.numeric(hm(TIME_LANDED)-hm(TIME_SAILED),"hours")]
    GHIST[Effort>20, Effort := Effort/10]
    
    # a trip is:
    GHIST[,TRIP := paste(Year,CFV,DOYs,DOYl,sep="_")]
    
    # Assign district based on CFV history
    # apply to situations where district is 99 first
    # consider using when district is 0 or NA and there is no Port info
    a1 = factor(unique(GHIST[DISTRICT_LANDED%in%c(0,99),CFV]))
    a2 = factor(unique(GHIST[is.na(DISTRICT_LANDED),CFV]))
    b1 = GHIST[!(DISTRICT_LANDED%in%c(0,99))&CFV%in%a1, .(Count=.N), by=.(DISTRICT_LANDED,CFV,Year)]
    b2 = GHIST[!(is.na(DISTRICT_LANDED))&CFV%in%a2, .(Count=.N), by=.(DISTRICT_LANDED,CFV,Year)]
    b=rbind(b1,b2)
    c = b[,.(DISTRICT_LANDED2=DISTRICT_LANDED[which.max(Count)]), by=.(Year,CFV)]
    GHIST = merge(GHIST,c, by=c("Year","CFV"),all.x=T)
    GHIST[DISTRICT_LANDED%in%c(0,99)|is.na(DISTRICT_LANDED),DISTRICT_LANDED:=DISTRICT_LANDED2]
    attr(GHIST,"comment") = "GHIST"
    ###########################################################################
    #
    # CURRENT GULF DATA AGGREGATION TO JDAY AND PORT LEVEL
    #
    ###########################################################################
    getmode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    
    # aggregate to level of trip and then to Port-Day
    prepareGULFagg = function(data) {
      GULF = data
      GULFb = GULF[!NAFO%in%c("4W","4WD","4WG","4X","4XM","4XN","4R") & 
                     HMA!="UKN" & Month%in%7:11 ,
                   .(Year = Year[1],
                     Jday = Jday[1],
                     Port3 = Port3[1],
                     NAFO = NAFO[1],
                     HMA = HMA[1],
                     GEAR = getmode(GEAR),
                     Month = Month[1],
                     Effort = Effort[1],
                     Count = sum(Count),
                     Strikes = sum(Strikes)),
                     by = .(TRIP )]
      
      GULFb = GULFb[, .(Effort = sum(Effort),
                   Count = sum(Count),
                   Strikes = sum(Strikes)),
                 by = .(Year, Jday, Port3, NAFO, HMA, GEAR, 
                        Month )]
    
 
    GULFb[NAFO%in%c("4TL","4TM","4TN","4TK","4TI"), Region := "WEST"]
    GULFb[NAFO%in%c("4TG","4TF","4TH","4TJ","4RST"), Region := "EAST"]
    GULFb[NAFO%in%c("4T","UKN","4TR","4TS","4TD"), Region := "UKN"]
    GULFb[, Region := factor(Region)]
    
    GULFb[Port3%in%c("GASPERAUX","CARIBOU/FERRY WHARF","MURRAY RIVER",
                     "PORT HOOD","PORT HOOD ISLAND","TRACADIE",
                     "WEST TRACADIE","NORTH LAKE","MURPHY'S POND",
                     "RUSTICO","NORTH RUSTICO","SOURIS","NINE MILE CREEK",
                     "PICTOU","NORTH EAST MABOU","MORELL",
                     "RED HEAD","NAUFRAGE","BALLANTYNE'S COVE",
                     "ANTIGONISH","ANTIGONISH HARBOUR","CRIBBENS POINT",
                     "EAST TRACADIE","CAPE GEORGE","MARGAREE HARBOUR",
                     "SAVAGE HARBOUR","MONTAGUE","LITTLE JUDIQUE HARBOUR",
                     "MARGAREE HARBOUR","MABOU","MABOU HARBOUR",
                     "MABOU MINES","INVERNESS","HAVRE BOUCHER",
                     "MURRAY HARBOUR","LAUNCHING","FINLAY POINT",
                     "FORTUNE","GRAHAM'S POND","MALIGANT COVE",
                     "ANNANDALE","BEACH POINT","BARRIOS BEACH",
                     "LIVINGSTONE'S COVE","MACHON POINT","GRAND TRACADIE",
                     "LONG POINT","AULD'S COVE","MINK RIVER","CHETICAMP",
                     "MALPEQUE","CAPE TORMENTINE","BAXTER'S COVE",
                     "CREIGNISH","ARISAIG","TONEY RIVER","MONK'S HEAD",
                     "LONG CREEK","BAYFIELD","MARYVILLE","LINWOOD",
                     "PORT BAN","PLEASANT BAY","BORDEN","SKINNER'S COVE",
                     "GRAND ETANG","BRULE","LISMORE","GEORGETOWN",
                     "CARDIGAN BAY","BAY ST. LAWRENCE","SKINNER'S POND",
                     "LITTLE JUDIQUE PONDS","CHARLOTTETOWN","MARGAREE",
                     "CARIBOU HARBOUR","SUMMERSIDE","FRENCH RIVER",
                     "ABBOTT'S HARBOUR","LOGAN'S POINT","FLAT RIVER",
                     "CHANCE HARBOUR"), Region := "EAST"]
    GULFb[Port3%in%c("TIGNISH RUN","TIGNISH", "SEACOW POND",
                     "JUDE'S POINT","POINT SAPIN","RICHIBUCTO",
                     "ALBERTON","HARDY'S CHANNEL","SHIPPAGAN",
                     "STE-THÉRÈSE-DE-GASPÉ","MISCOU HARBOUR","MISCOU",
                     "NORTHPORT","CARAQUET","MIMINEGASH","MILLIGAN'S WHARF",
                     "ESCUMINAC","PETIT SHIPPAGAN","ESCUMINAC POINT",
                     "LAMEQUE","ST. EDOUARD","ROSEVILLE",
                     "HARDWICKE"), Region :="WEST"]
    
    GULFb = GULFb[Region!="UKN"]
    GULFb[, Region := factor(Region)]
    
    # Combine gear types
    GULFb[GEAR=="TL", GEARb := "RRchum"]
    GULFb[GEAR=="TrollLL", GEARb := "RRtroll"]
    GULFb[is.na(GEARb), GEARb := GEAR]
    GULFb[, GEARb := factor(GEARb)]
    
    # Time
    GULFb[, YEAR := factor(Year)]
    GULFb[, MONTH := factor(Month)]
    
    # Effort
    GULFb = GULFb[Effort>0]
    }
    
    GULF1b = prepareGULFagg(GULF1)
    GULF2b = prepareGULFagg(GULF2)
    ###########################################################################
    #
    # HISTORICAL GULF DATA AGGREGATION TO JDAY AND PORT LEVEL
    #
    ###########################################################################
    
    # aggregate to level of trip and then to Port-Day
    GULFc = GHIST[!NAFO%in%c("4W","4WD","4WG","4X","4XM","4XN","4R","4S","4RS") & 
                    HMA!="UKN" & Month%in%7:11 &
                    !DISTRICT_LANDED%in%c(1,4,6,7,8,9,14:40) ,
                 .(Year = Year[1],
                   Jday = Jday[1],
                   PORT = PORT[1],
                   NAFO = NAFO[1],
                   HMA = HMA[1],
                   Gear2 = getmode(Gear2),
                   District = DISTRICT_LANDED[1],
                   Month = Month[1],
                   Effort = Effort[1],
                   Count = sum(Count),
                   Strikes = sum(NO_STRIKES)),
                 by = .(TRIP )]
    
    GULFc = GULFc[, .(Effort = sum(Effort),
                      Count = sum(Count),
                      Strikes = sum(Strikes)),
                  by = .(Year, Jday, PORT, NAFO, HMA, Gear2, 
                         District, Month )]
    

    GULFc[NAFO%in%c("4TL","4TM","4TN","4TK","4TI"), Region := "WEST"]
    GULFc[NAFO%in%c("4TG","4TF","4TH","4TJ","4RST"), Region := "EAST"]
    GULFc[NAFO%in%c("4T","UKN","4TR","4TS","4TD"), Region := "UKN"]
    GULFc[, Region := factor(Region)]
    
    GULFc[PORT%in%c("GASPERAUX","CARIBOU/FERRY WHARF","MURRAY RIVER",
                    "PORT HOOD","PORT HOOD ISLAND","TRACADIE",
                    "WEST TRACADIE","NORTH LAKE","MURPHY'S POND",
                    "RUSTICO","NORTH RUSTICO","SOURIS","NINE MILE CREEK",
                    "PICTOU","NORTH EAST MABOU","MORELL",
                    "RED HEAD","NAUFRAGE","BALLANTYNE'S COVE",
                    "ANTIGONISH","ANTIGONISH HARBOUR","CRIBBENS POINT",
                    "EAST TRACADIE","CAPE GEORGE","MARGAREE HARBOUR",
                    "SAVAGE HARBOUR","MONTAGUE","LITTLE JUDIQUE HARBOUR",
                    "MARGAREE HARBOUR","MABOU","MABOU HARBOUR",
                    "MABOU MINES","INVERNESS","HAVRE BOUCHER",
                    "MURRAY HARBOUR","LAUNCHING","FINLAY POINT",
                    "FORTUNE","GRAHAM'S POND","MALIGANT COVE",
                    "ANNANDALE","BEACH POINT","BARRIOS BEACH",
                    "LIVINGSTONE'S COVE","MACHON POINT","GRAND TRACADIE",
                    "LONG POINT","AULD'S COVE","MINK RIVER","CHETICAMP",
                    "MALPEQUE","CAPE TORMENTINE","BAXTER'S COVE",
                    "CREIGNISH","ARISAIG","TONEY RIVER","MONK'S HEAD",
                    "LONG CREEK","BAYFIELD","MARYVILLE","LINWOOD",
                    "PORT BAN","PLEASANT BAY","BORDEN","SKINNER'S COVE",
                    "GRAND ETANG","BRULE","LISMORE","GEORGETOWN",
                    "CARDIGAN BAY","BAY ST. LAWRENCE"), Region := "EAST"]
    GULFc[PORT%in%c("TIGNISH RUN","TIGNISH", "SEACOW POND",
                    "JUDE'S POINT","POINT SAPIN","RICHIBUCTO",
                    "ALBERTON","HARDY'S CHANNEL"), Region :="WEST"]

    # Use District to assign trip to a region
    GULFc[Region == "UKN" & 
            District%in%c(2,3,10:13,93,95,96,88,87,86,85,83,80,45,46,77,78),
          Region := "EAST"]
    GULFc[Region == "UKN" & 
            District%in%c(63:76,82,92),
          Region := "WEST"]
    
    # Combine gear types
    GULFc[, GEARb := Gear2]
    GULFc[Gear2=="TL", GEARb := "RRchum"]
    GULFc[Gear2=="HARP", GEARb := NA]
    GULFc[is.na(GEARb), GEARb := "UKN"]
    GULFc[, GEARb := factor(GEARb)]
    
    GULFc = GULFc[!GEARb=="UKN"]
    
    # Time
    GULFc[, YEAR := factor(Year)]
    GULFc[, MONTH := factor(Month)]
    
    # Effort
    GULFc = GULFc[Effort>0&Effort<50]
    
    ###########################################################################
    #
    # COMBINE HISTORICAL ANS CURRENT GULF DATA AGGREGATED TO JDAY AND PORT 
    #
    ###########################################################################
   
    DATA1 = GULFc[HMA%in%c("GNS","PEI","PQ","GNB","SF"),
                  .(YEAR,Effort,Strikes,Count,HMA,Region,Jday,GEARb,Gear=Gear2)]
    DATA2 = GULF1b[,.(YEAR,Effort,Strikes,Count,HMA,Region,Jday,GEARb,Gear=GEAR)]
    DATA3 = GULF2b[,.(YEAR,Effort,Strikes,Count,HMA,Region,Jday,GEARb,Gear=GEAR)]
    
    DATA = rbind(DATA1,DATA2,DATA3)
    DATA = DATA[Region!="UKN" & !HMA%in%c("SF","NFLD") & GEARb!="UKN"]
    DATA[, HMA := factor(HMA)]
    DATA[, YEAR := factor(YEAR)]
    DATA[GEARb=="RRchum", GEARb := "RR"]
    DATA[, GEARb := factor(GEARb)]
    DATA = DATA[Jday>195&Jday<310]
    DATA[,FG := factor(paste(HMA,GEARb,sep="_"))] 
    DATA[,FGR := factor(paste(HMA,GEARb,Region,sep="_"))]
    DATA[,FR := factor(paste(HMA,Region,sep="_"))] 
    DATA[,FRY := factor(paste(HMA,Region,YEAR,sep="_"))] 
    DATA[,dum := 1]
    DATA[,Effort_w := length(Effort),cut(Effort,breaks=20)]
    DATA[,Effort_f := factor(cut(Effort,breaks=20))]
    DATA[,Week := cut(Jday, breaks=seq(195,314,7),labels=F)]
    DATA[,FGW := factor(paste(HMA,GEARb,Week,sep="_"))] 
    DATA[,YFGW := factor(paste(YEAR,HMA,GEARb,Week,sep="_"))] 
    DATA[,YF := factor(paste(YEAR,HMA,sep="_"))] 
    DATA[,YFG := factor(paste(YEAR,HMA,GEARb,sep="_"))] 
    DATA[,Quarter := factor(quarter(strptime(Jday, "%j")))]
    DATA[,CPUE := Count/Effort]
    sGSL =  DATA[,.(dum,NAFO=Region,HMA,
                    Year=as.numeric(as.character(YEAR)),
                    YEAR,Jday,GEAR=GEARb,Count,Effort)]
    DATA[,Year := as.numeric(as.character(YEAR))]
    
    # reduce DATA based on grouping variable size
    DATA[,FGW_Size:= length(Count),FGW]
    #DATA[,table(FGW_Size)]
    DATA2 = DATA[FGW_Size>10,]
    
    attr(DATA,"comments") = "Port-Day level combined data"
    
    # Return the loaded data
    allDATA = list(GHIST=GHIST, GULF1=GULF1, GULF2=GULF2, DATA=DATA)
    attr(allDATA,"comments") =
    "QAQC data for historical, logbook 1, logbook 2 and port-day level combined data."
    
    return(allDATA)
  }

}


