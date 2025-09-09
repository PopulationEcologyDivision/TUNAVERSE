require(ggplot2)

Log1 = DATA[[2]]
Log2 = DATA[[3]]

ggplot(Log1[!is.na(TAG_NO_DMC)&LANDED_FORM_CODE_DMC==1,],
       aes(x=RND_WEIGHT_KGS_SLIP,
           y=RPT_WEIGHT_KGS_SLIP)) + 
  geom_point()  + xlim(0,750) + ylim(0,750)+ facet_wrap(~YEAR)

ggplot(Log2[!is.na(TAG_NO_DMC),],
       aes(x=RND_WEIGHT_KGS_SLIP,
           y=RPT_WEIGHT_KGS_SLIP)) + 
  geom_point() + facet_wrap(~LANDED_FORM_CODE_DMC)

ggplot(Log1[Year>2010&as.numeric(FLANK_LENGTH_CM)<500,],
       aes(x=RND_WEIGHT_KGS_SLIP,
           y=as.numeric(OFFLOAD_WEIGHT_KGS_DMC),
           col= FLANK_LENGTH_UOM_CN)) + 
  geom_point() + facet_wrap(~LANDED_FORM_CODE_DMC)

ggplot(Log2[Year>2010&as.numeric(FLANK_LENGTH_CM)<500,],
       aes(x=RND_WEIGHT_KGS_SLIP,
           y=as.numeric(OFFLOAD_WEIGHT_KGS_DMC),
           col= FLANK_LENGTH_UOM_CN)) + 
  geom_point() + facet_wrap(~LANDED_FORM_CODE_DMC)

ggplot(Log1[as.numeric(FLANK_LENGTH_CN)<500,],
       aes(x=as.numeric(DRESSED_LENGTH_CN),
           y=as.numeric(FLANK_LENGTH_CN)) )+ 
  geom_point() 

ggplot(Log2[Year>2010,],
       aes(y=RND_WEIGHT_KGS_SLIP, x=as.character(Year))) + 
  geom_boxplot() +
  geom_boxplot(data=Log1[Year>2013,], 
       aes(x=as.character(Year), y=RND_WEIGHT_KGS_SLIP))+
  ylim(0,1000) + geom_hline(yintercept = 240, col="red")+
  geom_hline(yintercept = 370, col="green")

ggplot(Log1[LANDED_FORM_CODE_DMC==1&LANDED_FORM_CODE_SLIP==1],
       aes(OFFLOAD_WEIGHT_KGS_DMC,RND_WEIGHT_KGS_SLIP)) +
  geom_point() + geom_abline(slope=1,col="red")+
labs(caption="Form code of DMC and Slip are 1, round")

ggplot(Log1[LANDED_FORM_CODE_DMC==3&LANDED_FORM_CODE_SLIP==3],
       aes(OFFLOAD_WEIGHT_KGS_DMC,RND_WEIGHT_KGS_SLIP)) +
  geom_point() + geom_abline(slope=1,col="red")+ 
  labs(caption="Form code of DMC is 3 and Slip is 3, dressed")

ggplot(Log1[LANDED_FORM_CODE_DMC==1&LANDED_FORM_CODE_SLIP==3],
       aes(OFFLOAD_WEIGHT_KGS_DMC,RND_WEIGHT_KGS_SLIP)) +
  geom_point() + geom_abline(slope=1,col="red")+ 
  labs(caption="Form code of DMC is 1 and Slip is 3") +
  ylim(0,800)

ggplot(Log1[LANDED_FORM_CODE_DMC==1&LANDED_FORM_CODE_SLIP==3],
       aes(OFFLOAD_WEIGHT_KGS_DMC,RPT_WEIGHT_KGS_SLIP)) +
  geom_point() + geom_abline(slope=1,col="red")+ 
  labs(caption="Form code of DMC is 1 and Slip is 3") +
  ylim(0,800)
