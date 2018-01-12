library(barcodetrackR)
library(ggplot2)
library(diverse)
library(gridExtra)

###############################################################################################################
###LOADS ENVRIORNMENT##########################################################################################
###############################################################################################################

zj48_lib7=read.combined("zj48_lib7.txt")
zj48_lib8=read.combined("zj48_lib8.txt")
zj48_lib11=read.combined("zj48_lib11.txt")

grans48=c("pp_50trim_zj48_1_month_Grans_index15_IY12.fastq","pp_50trim_zj48_2_month_Grans_index16_IY12.fastq",
          "pp_50trim_zj48_3_month_Grans_index17_IY12.fastq","pp_50trim_zj48_4_month_Grans_442p8ng__index18_IY12.fastq",
          "pp_50trim_ZJ48_5m_Grans_IY_R15_IY24.fastq","pp_50trim_zj48_6_month_Grans_index20_IY12.fastq",
          "pp_50trim_ZJ48_7m_Grans_IY_R17_IY24.fastq","pp_50trim_ZJ48_11m_Grans_IY_R22_IY24.fastq",
          "pp_50trim_ZJ48_12m_Grans_IY_R24_IY24.fastq","pp_50trim_ZJ48_16m_Grans_2_IY28_R20.fastq",
          "ZJ48_Grans_24m_sampled_LT2821_LT3_i524_S19_L001_R1_001.fastq")

B48=c("pp_50trim_zj48_1_month_B_cells_index21_IY12.fastq","pp_50trim_zj48_2_month_B_cells_index22_IY12.fastq",
      "pp_50trim_zj48_3_month_B_cells_index23_IY12.fastq", "pp_50trim_zj48_4_month_B_cells_index24_IY12.fastq",
      "pp_50trim_zj48_5_month_B_cells_index25_IY12.fastq","pp_50trim_zj48_6_month_B_cells_index26_IY12.fastq",
      "pp_50trim_ZJ48_7m_B_cells__IY_R21_IY23.fastq","pp_50trim_ZJ48_9m_B_cells__IY_R25_IY23.fastq",
      "pp_50trim_ZJ48_12m_B_cells__IY_R27_IY23.fastq","ZJ48_Grans_24m_sampled_LT2821_LT3_i524_S19_L001_R1_001.fastq")

t48=c("pp_50trim_zj48_1_month_T_cells_index15_IY13.fastq","pp_50trim_zj48_2_month_T_cells_index_16_IY13.fastq",
      "pp_50trim_zj48_3_month_T_cells_index17_IY13.fastq","pp_50trim_zj48_4_month_T_cells_index18_IY13.fastq",
      "pp_50trim_zj48_5_month_T_cells_index19_IY13.fastq","pp_50trim_zj48_6_month_T_cells_index20_IY13.fastq",
      "pp_50trim_ZJ48_7m_T_cells__IY_R14_IY23.fastq","pp_50trim_ZJ48_9m_T_cells__IY_R15_IY23.fastq",
      "pp_50trim_ZJ48_12m_T_cells__IY_R17_IY23.fastq","ZJ48_T_24m_sampled_LT2821_LT3_i528_S22_L001_R1_001.fastq")
zj48_time_points_g = c(1,2,3,4,5,6,7,11,12,16,24)
zj48_time_points_t= c(1,2,3,4,5,6,7,9,12,24)
zj48_time_points_b= c(1,2,3,4,5,6,7,9,12,24)


zj41_lib7=read.combined("zj41_lib7.txt")
zj41_lib8=read.combined("zj41_lib8.txt")
zj41_lib11=read.combined("zj41_lib11.txt")


grans41=c("zj41_1m_Gr_pp_50trim_IY21_R15.fastq","zj41_2m_Gr_293ng_pp_50trim_IY21_R16.fastq","zj41_3m_Gr_pp_50trim_IY21_R17.fastq",
          "zj41_4m_Gr_pp_50trim_IY21_R18.fastq","pp_50trim_ZJ41_5m_Grans_Ann__IY25_R14.fastq","zj41_6m_Gr_pp_50trim_IY21_R20.fastq",
          "zj41_9m_Gr_pp_50trim_IY21_R21.fastq","zj41_9m_Gr_pp_50trim_IY21_R21.fastq","pp_50trim_ZJ41_13m_Grans_1___IY27_R19.fastq",
          "pp_50trim_ZJ41_16m_Grans__IY25_R22.fastq","pp_50trim_ZJ41_18m_Grans__IY25_R25.fastq","pp_50trim_ZJ41_21m_Grans__IY25_R26.fastq",
          "ZJ41_Grans_29m_sampled_LT2821_LT3_i516_S13_L001_R1_001.fastq")

B41=c("zj41_1m_B_pp_50trim_IY15_R23.fastq",
      "zj41_2m_B_pp_50trim_IY15_R24.fastq",
      "zj41_3m_B_pp_50trim_IY15_R25.fastq",
      "zj41_4m_B_pp_50trim_IY15_R26.fastq",
      "zj41_5m_B_pp_50trim_IY15_R27.fastq",
      "zj41_6m_B_pp_50trim_IY15_R28.fastq",
      "zj41_9m_B_rep_pp_50trim_IY15_R34.fastq",
      "zj41_11m_B_pp_50trim_IY15_R35.fastq",
      "pp_50trim_ZJ41_12m_B_IY26_R27.fastq",
      "pp_50trim_ZJ41_18m_B_IY26_R28.fastq",
      "ZJ41_B_29m_sampled_LT2821_LT3_i518_S14_L001_R1_001.fastq")


t41=c("zj41_m1_Tcells_IY09R28.fastq","zj41_m2_Tcells_IY07R28.fastq","zj41_3m_T_rep_pp_50trim_IY22_R25.fastq",
      "zj41_4m_T_rep_pp_50trim_IY22_R26.fastq","zj41_5m_T_rep_pp_50trim_IY22_R27.fastq","zj41_6m_T_pp_50trim_IY22_R20.fastq",
      "pp_50trim_ZJ41_7m_T_IY26_R24.fastq","zj41_9m_T_rep_pp_50trim_IY22_R33.fastq","zj41_11m_T_rep_pp_50trim_IY22_R34.fastq",
      "pp_50trim_ZJ41_18m_T_IY26_R25.fastq","ZJ41_T_29m_sampled_LT2821_LT3_i521_S16_L001_R1_001.fastq")

zj41_time_points_g = c(1,2,3,4,5,6,9,12,13,16,18,21,29)
zj41_time_points_t= c(1,2,3,4,5,6,7,9,11,18,29)
zj41_time_points_b= c(1,2,3,4,5,6,9,11,12,18,29)

###############################################################################################################
###APPLIES THRESHOLD###########################################################################################
###uses default 2000 threshold#################################################################################
###############################################################################################################

zj48_lib7=barcodetrackR::threshold(zj48_lib7)
zj48_lib8=barcodetrackR::threshold(zj48_lib8)
zj48_lib11=barcodetrackR::threshold(zj48_lib11)

zj41_lib7=barcodetrackR::threshold(zj41_lib7)
zj41_lib8=barcodetrackR::threshold(zj41_lib8)
zj41_lib11=barcodetrackR::threshold(zj41_lib11)

###############################################################################################################
###FIGURE 2B##################################################################################################
###GFP########################################################################################################
###############################################################################################################

zj48_gfp=read.delim("zj48_gfp.txt",header=TRUE,row.names = 1)
zj48_gfp=zj48_gfp[-nrow(zj48_gfp),]
rownames(zj48_gfp)=c(.5,1,2,3,4,5,6,7,8,9,11,12,15,16,24)
colnames(zj48_gfp)=c("Granulocytes","T Cells","B Cells")

zj41_gfp=read.delim("zj41_gfp.txt",header=TRUE,row.names = 1)
zj41_gfp=zj41_gfp[-nrow(zj41_gfp),]
rownames(zj41_gfp)=c(1,2,3,4,5,6,7,8,9,10,11,12,14,21,29)
colnames(zj41_gfp)=c("Granulocytes","T Cells","B Cells")

pdf(file = "zj48_gfp.pdf",width=30,height=20)
gfp(zj48_gfp,c("dotted","longdash","solid"))
dev.off()

pdf(file = "zj41_gfp.pdf",width=30,height=20)
gfp(zj41_gfp,c("dotted","longdash","solid"))
dev.off()

zj48_gfp=gfp(zj48_gfp,c("dotted","longdash","solid"))+ggtitle("ZJ48")+theme(legend.position="none")
zj41_gfp=gfp(zj41_gfp,c("dotted","longdash","solid"))+ggtitle("ZJ41")+theme(legend.position="none")
pdf(file = "final_gfp.pdf",width=30,height=20)
grid.arrange(zj48_gfp,zj41_gfp, ncol=2)
dev.off()

###############################################################################################################
###FIGURE 3A##################################################################################################
###Vector contriubution########################################################################################
###############################################################################################################

x=read.delim("zj48_Vector_contribution.txt")

lib7=x$LIB7.5[-1]
lib7=lib7[1:11]

lib8=x$LIB8.5[-1]
lib8=lib8[1:11]

lib11=x$LIB.11.5[-1]
lib11=lib11[1:11]

df=data.frame(Month=rep(zj48_time_points_g,3),
              V=c(as.numeric(as.character(lib7)),as.numeric(as.character(lib8)),as.numeric(as.character(lib11))),
              Order=rep(c("A","B","C"),each=length(lib11)))

pdf(file = "zj48_vc_grans.pdf",height=20,width=30)
temp1=ggplot(data=df, aes(x=Month, y=V, fill=Order)) +scale_y_continuous("Percent")+
  geom_bar(stat="identity")+scale_x_continuous("Month",breaks=df$Month,labels=df$Month,limits=c(0,30))+
  scale_fill_manual(values=c("deeppink", "darkgrey","deepskyblue"),breaks=c("A","B","C"),labels=c("MSCV","EF1a","SFFV"),name="Library")+
  theme(legend.text=element_text(size=15),legend.key.size = unit(1, "cm"),legend.title=element_text(size=25),axis.text=element_text(size=40),axis.title=element_text(size=60,face="bold"))
temp1
dev.off()

lib7=x$LIB7.5[-1]
lib7=lib7[14:23]

lib8=x$LIB8.5[-1]
lib8=lib8[14:23]

lib11=x$LIB.11.5[-1]
lib11=lib11[14:23]

df=data.frame(Month=rep(zj48_time_points_b,3),
              V=c(as.numeric(as.character(lib7)),as.numeric(as.character(lib8)),as.numeric(as.character(lib11))),
              Order=rep(c("A","B","C"),each=length(lib11)))

pdf(file = "zj48_vc_b.pdf",width=30,height=20)
temp2=ggplot(data=df, aes(x=Month, y=V, fill=Order)) +scale_y_continuous("Percent")+
  geom_bar(stat="identity")+  scale_x_continuous("Month",breaks=df$Month,labels=df$Month,limits=c(0,30))+
  scale_fill_manual(values=c("deeppink", "darkgrey","deepskyblue"),breaks=c("A","B","C"),labels=c("MSCV","EF1a","SFFV"),name="Library")+
  theme(legend.text=element_text(size=15),legend.key.size = unit(1, "cm"),legend.title=element_text(size=25),axis.text=element_text(size=40),axis.title=element_text(size=60,face="bold"))
temp2
dev.off()

lib7=x$LIB7.5[-1]
lib7=lib7[26:35]

lib8=x$LIB8.5[-1]
lib8=lib8[26:35]

lib11=x$LIB.11.5[-1]
lib11=lib11[26:35]

df=data.frame(Month=rep(zj48_time_points_t,3),
              V=c(as.numeric(as.character(lib7)),as.numeric(as.character(lib8)),as.numeric(as.character(lib11))),
              Order=rep(c("A","B","C"),each=length(lib11)))

pdf(file = "zj48_vc_t.pdf",width=30,height=20)
temp3=ggplot(data=df, aes(x=Month, y=V, fill=Order)) +scale_y_continuous("Percent")+
  geom_bar(stat="identity")+  scale_x_continuous("Month",breaks=df$Month,labels=df$Month,limits=c(0,30))+
  scale_fill_manual(values=c("deeppink", "darkgrey","deepskyblue"),breaks=c("A","B","C"),labels=c("MSCV","EF1a","SFFV"),name="Library")+
  theme(legend.text=element_text(size=15),legend.key.size = unit(1, "cm"),legend.title=element_text(size=25),axis.text=element_text(size=40),axis.title=element_text(size=60,face="bold"))
temp3
dev.off()

x=read.delim("zj41_Vector_contribution.txt")

lib7=x$LIB7.5[-1]
lib7=lib7[1:13]
lib7
lib8=x$LIB8.5[-1]
lib8=lib8[1:13]

lib11=x$LIB.11.5[-1]
lib11=lib11[1:13]

df=data.frame(Month=rep(zj41_time_points_g,3),
              V=c(as.numeric(as.character(lib7)),as.numeric(as.character(lib8)),as.numeric(as.character(lib11))),
              Order=rep(c("A","B","C"),each=length(lib11)))

pdf(file = "zj41_vc_grans.pdf",width=30,height=20)
temp4=ggplot(data=df, aes(x=Month, y=V, fill=Order)) +scale_y_continuous("Percent")+
  geom_bar(stat="identity")+  scale_x_continuous("Month",breaks=df$Month,labels=df$Month,limits=c(0,30))+
  scale_fill_manual(values=c("deeppink", "darkgrey","deepskyblue"),breaks=c("A","B","C"),labels=c("MSCV","EF1a","SFFV"),name="Library")+
  theme(legend.text=element_text(size=15),legend.key.size = unit(1, "cm"),legend.title=element_text(size=25),axis.text=element_text(size=40),axis.title=element_text(size=60,face="bold"))
temp4
dev.off()

lib7=x$LIB7.5[-1]
lib7=lib7[16:26]

lib8=x$LIB8.5[-1]
lib8=lib8[16:26]

lib11=x$LIB.11.5[-1]
lib11=lib11[16:26]

df=data.frame(Month=rep(zj41_time_points_b,3),
              V=c(as.numeric(as.character(lib7)),as.numeric(as.character(lib8)),as.numeric(as.character(lib11))),
              Order=rep(c("A","B","C"),each=length(lib11)))

pdf(file = "zj41_vc_b.pdf",width=30,height=20)
temp5=ggplot(data=df, aes(x=Month, y=V, fill=Order)) +scale_y_continuous("Percent")+
  geom_bar(stat="identity") +  scale_x_continuous("Month",breaks=df$Month,labels=df$Month,limits=c(0,30))+
  scale_fill_manual(values=c("deeppink", "darkgrey","deepskyblue"),breaks=c("A","B","C"),labels=c("MSCV","EF1a","SFFV"),name="Library")+
  theme(legend.text=element_text(size=15),legend.key.size = unit(1, "cm"),legend.title=element_text(size=25),axis.text=element_text(size=40),axis.title=element_text(size=60,face="bold"))
temp5
dev.off()

zj41_time_points_t=zj41_time_points_t[-(length(zj41_time_points_t)-1)]

lib7=as.numeric(as.character(x$LIB7.5[-1]))
lib7=c(lib7[29:37],4.254)

lib8=as.numeric(as.character(x$LIB8.5[-1]))
lib8=c(lib8[29:37],3.588)

lib11=as.numeric(as.character(x$LIB.11.5[-1]))
lib11=c(lib11[29:37],92.158)

df=data.frame(Month=rep(zj41_time_points_t,3),
              V=c(as.numeric(as.character(lib7)),as.numeric(as.character(lib8)),as.numeric(as.character(lib11))),
              Order=rep(c("A","B","C"),each=length(lib11)))

pdf(file = "zj41_vc_t.pdf",width=30,height=20)
temp6=ggplot(data=df, aes(x=Month, y=V, fill=Order)) +scale_y_continuous("Percent")+
  geom_bar(stat="identity")+  scale_x_continuous("Month",breaks=df$Month,labels=df$Month,limits=c(0,30))+
  scale_fill_manual(values=c("deeppink", "darkgrey","deepskyblue"),breaks=c("A","B","C"),labels=c("MSCV","EF1a","SFFV"),name="Library")+
  theme(legend.text=element_text(size=15),legend.key.size = unit(1, "cm"),legend.title=element_text(size=25),axis.text=element_text(size=40),axis.title=element_text(size=60,face="bold"))
temp6
dev.off()

pdf(file = "final_vc.pdf",width=40,height=20)
grid.arrange(temp4+ylab("ZJ41 \n Percent Contribution")+ggtitle("Granulocytes")+theme(title=element_text(size=50),legend.position="none"),temp5+ylab("Percent Contribution")+ggtitle("B cells")+theme(title=element_text(size=50),legend.position="none"),temp6+ylab("Percent Contribution")+ggtitle("T cells")+theme(title=element_text(size=50),legend.position="none"),temp1+ylab("ZJ48 \n Percent Contribution")+theme(legend.position="none"),temp2+ylab("Percent Contribution")+theme(legend.position="none"),temp3+ylab("Percent Contribution")+theme(legend.position="none"),ncol=3)
dev.off()


###############################################################################################################
###FIGURE 3B##################################################################################################
###Unique Barcode Number#######################################################################################
###############################################################################################################

grans48=c("pp_50trim_zj48_1_month_Grans_index15_IY12.fastq","pp_50trim_zj48_2_month_Grans_index16_IY12.fastq",
          "pp_50trim_zj48_3_month_Grans_index17_IY12.fastq","pp_50trim_zj48_4_month_Grans_442p8ng__index18_IY12.fastq",
          "pp_50trim_ZJ48_5m_Grans_IY_R15_IY24.fastq","pp_50trim_zj48_6_month_Grans_index20_IY12.fastq",
          "pp_50trim_ZJ48_7m_Grans_IY_R17_IY24.fastq","pp_50trim_ZJ48_11m_Grans_IY_R22_IY24.fastq",
          "pp_50trim_ZJ48_12m_Grans_IY_R24_IY24.fastq","pp_50trim_ZJ48_16m_Grans_2_IY28_R20.fastq",
          "ZJ48_Grans_24m_sampled_LT2821_LT3_i524_S19_L001_R1_001.fastq")

B48=c("pp_50trim_zj48_1_month_B_cells_index21_IY12.fastq","pp_50trim_zj48_2_month_B_cells_index22_IY12.fastq",
      "pp_50trim_zj48_3_month_B_cells_index23_IY12.fastq", "pp_50trim_zj48_4_month_B_cells_index24_IY12.fastq",
      "pp_50trim_zj48_5_month_B_cells_index25_IY12.fastq","pp_50trim_zj48_6_month_B_cells_index26_IY12.fastq",
      "pp_50trim_ZJ48_7m_B_cells__IY_R21_IY23.fastq","pp_50trim_ZJ48_9m_B_cells__IY_R25_IY23.fastq",
      "pp_50trim_ZJ48_12m_B_cells__IY_R27_IY23.fastq","ZJ48_Grans_24m_sampled_LT2821_LT3_i524_S19_L001_R1_001.fastq")

t48=c("pp_50trim_zj48_1_month_T_cells_index15_IY13.fastq","pp_50trim_zj48_2_month_T_cells_index_16_IY13.fastq",
      "pp_50trim_zj48_3_month_T_cells_index17_IY13.fastq","pp_50trim_zj48_4_month_T_cells_index18_IY13.fastq",
      "pp_50trim_zj48_5_month_T_cells_index19_IY13.fastq","pp_50trim_zj48_6_month_T_cells_index20_IY13.fastq",
      "pp_50trim_ZJ48_7m_T_cells__IY_R14_IY23.fastq","pp_50trim_ZJ48_9m_T_cells__IY_R15_IY23.fastq",
      "pp_50trim_ZJ48_12m_T_cells__IY_R17_IY23.fastq","ZJ48_T_24m_sampled_LT2821_LT3_i528_S22_L001_R1_001.fastq")



zj48_time_points_g = c(1,2,3,4,5,6,7,11,12,16,24)
zj48_time_points_t= c(1,2,3,4,5,6,7,9,12,24)
zj48_time_points_b= c(1,2,3,4,5,6,7,9,12,24)


grans41=c("zj41_1m_Gr_pp_50trim_IY21_R15.fastq","zj41_2m_Gr_293ng_pp_50trim_IY21_R16.fastq","zj41_3m_Gr_pp_50trim_IY21_R17.fastq",
          "zj41_4m_Gr_pp_50trim_IY21_R18.fastq","pp_50trim_ZJ41_5m_Grans_Ann__IY25_R14.fastq","zj41_6m_Gr_pp_50trim_IY21_R20.fastq",
          "zj41_9m_Gr_pp_50trim_IY21_R21.fastq","zj41_9m_Gr_pp_50trim_IY21_R21.fastq","pp_50trim_ZJ41_13m_Grans_1___IY27_R19.fastq",
          "pp_50trim_ZJ41_16m_Grans__IY25_R22.fastq","pp_50trim_ZJ41_18m_Grans__IY25_R25.fastq","pp_50trim_ZJ41_21m_Grans__IY25_R26.fastq",
          "ZJ41_Grans_29m_sampled_LT2821_LT3_i516_S13_L001_R1_001.fastq")

B41=c("zj41_1m_B_pp_50trim_IY15_R23.fastq",
      "zj41_2m_B_pp_50trim_IY15_R24.fastq",
      "zj41_3m_B_pp_50trim_IY15_R25.fastq",
      "zj41_4m_B_pp_50trim_IY15_R26.fastq",
      "zj41_5m_B_pp_50trim_IY15_R27.fastq",
      "zj41_6m_B_pp_50trim_IY15_R28.fastq",
      "zj41_9m_B_rep_pp_50trim_IY15_R34.fastq",
      "zj41_11m_B_pp_50trim_IY15_R35.fastq",
      "pp_50trim_ZJ41_12m_B_IY26_R27.fastq",
      "pp_50trim_ZJ41_18m_B_IY26_R28.fastq",
      "ZJ41_B_29m_sampled_LT2821_LT3_i518_S14_L001_R1_001.fastq")


t41=c("zj41_m1_Tcells_IY09R28.fastq","zj41_m2_Tcells_IY07R28.fastq","zj41_3m_T_rep_pp_50trim_IY22_R25.fastq",
      "zj41_4m_T_rep_pp_50trim_IY22_R26.fastq","zj41_5m_T_rep_pp_50trim_IY22_R27.fastq","zj41_6m_T_pp_50trim_IY22_R20.fastq",
      "pp_50trim_ZJ41_7m_T_IY26_R24.fastq","zj41_9m_T_rep_pp_50trim_IY22_R33.fastq","zj41_11m_T_rep_pp_50trim_IY22_R34.fastq",
      "pp_50trim_ZJ41_18m_T_IY26_R25.fastq","ZJ41_T_29m_sampled_LT2821_LT3_i521_S16_L001_R1_001.fastq")


zj41_time_points_g = c(1,2,3,4,5,6,9,12,13,16,18,21,29)
zj41_time_points_t= c(1,2,3,4,5,6,7,9,11,18,29)
zj41_time_points_b= c(1,2,3,4,5,6,9,11,12,18,29)

to_plot=list(zj48_lib7[,grans48],zj48_lib8[,grans48],zj48_lib11[,grans48])
colors=c("deeppink", "darkgrey","deepskyblue")
zj48_cumulative_grans=plot.cumulative.lib(to_plot,zj48_time_points_g,c("MSCV","EF1a","SFFV"),colors=c("deeppink", "gray","deepskyblue"))
zj48_unique_grans=plot.unique.lib(to_plot,zj48_time_points_g,c("MSCV","EF1a","SFFV"),colors)


to_plot=list(zj48_lib7[,t48],zj48_lib8[,t48],zj48_lib11[,t48])
zj48_cumulative_t=plot.cumulative.lib(to_plot,zj48_time_points_t,c("MSCV","EF1a","SFFV"),colors)

zj48_unique_t=plot.unique.lib(to_plot,zj48_time_points_t,c("MSCV","EF1a","SFFV"),colors)

to_plot=list(zj48_lib7[,B48],zj48_lib8[,B48],zj48_lib11[,B48])
zj48_cumulative_b=plot.cumulative.lib(to_plot,zj48_time_points_b,c("MSCV","EF1a","SFFV"),colors)
zj48_unique_b=plot.unique.lib(to_plot,zj48_time_points_b,c("MSCV","EF1a","SFFV"),colors)

to_plot=list(zj41_lib7[,grans41],zj41_lib8[,grans41],zj41_lib11[,grans41])
zj41_cumulative_grans=plot.cumulative.lib(to_plot,zj41_time_points_g,c("MSCV","EF1a","SFFV"),colors)
zj41_unique_grans=plot.unique.lib(to_plot,zj41_time_points_g,c("MSCV","EF1a","SFFV"),colors)

to_plot=list(zj41_lib7[,t41],zj41_lib8[,t41],zj41_lib11[,t41])
zj41_cumulative_t=plot.cumulative.lib(to_plot,zj41_time_points_t,c("MSCV","EF1a","SFFV"),colors)
zj41_unique_t=plot.unique.lib(to_plot,zj41_time_points_t,c("MSCV","EF1a","SFFV"),colors)

to_plot=list(zj41_lib7[,B41],zj41_lib8[,B41],zj41_lib11[,B41])
zj41_cumulative_b=plot.cumulative.lib(to_plot,zj41_time_points_b,c("MSCV","EF1a","SFFV"),colors)
zj41_unique_b=plot.unique.lib(to_plot,zj41_time_points_b,c("MSCV","EF1a","SFFV"),colors)


choose=c(1,2,3,4,5,6,9,18,29)
temp1=NULL;temp2=NULL;temp3=NULL
for(i in 1:length(choose)){
  temp1=rbind(temp1,subset(zj41_unique_b,Month==choose[i]))
  temp2=rbind(temp2,subset(zj41_unique_grans,Month==choose[i]))
  temp3=rbind(temp3,subset(zj41_unique_t,Month==choose[i]))
}

C=temp1$C+temp2$C+temp3$C
U=temp1$U+temp2$U+temp3$U

cumulative=temp1
cumulative$C=C

unique=temp1
unique$U=U

plot_data=unique
pdf(file = "unique_zj41.pdf",width=30,height=20)
zj41_u=ggplot(plot_data,aes(x=Month,y=U,group=Library,color=Library))+
  geom_line(aes(size=2))+scale_color_manual(values=colors,breaks=c("EF1a","MSCV","SFFV"))+
  geom_point(aes(size=2.5))+scale_y_continuous(name="Unique Barcode",limits=c(0,2600))+
  scale_x_discrete(limits=seq(0,31),breaks=seq(0,30,by=5))+ theme_bw()+
  theme(legend.text=element_text(size=25),legend.key.size = unit(1.5, "cm"),legend.title=element_text(size=25),axis.text=element_text(size=40),
        axis.title=element_text(size=40,face="bold"))
zj41_u
dev.off()
zj41_u2=ggplot(plot_data,aes(x=Month,y=U,group=Library,color=Library))+
  geom_line(aes(size=2))+scale_color_manual(values=colors,breaks=c("EF1a","MSCV","SFFV"))+
  geom_point(aes(size=2.5))+scale_y_continuous(name="Unique Barcode",limits=c(0,2600))+
  scale_x_discrete(limits=seq(0,31),breaks=seq(0,30,by=5))+ theme_bw()+
  theme(legend.text=element_text(size=25),legend.key.size = unit(1.5, "cm"),legend.title=element_text(size=25),axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"),legend.position = "none",plot.title = element_text(size=30))+ggtitle("ZJ48")

choose=c(1,2,3,4,5,6,7,12,24)
temp1=NULL;temp2=NULL;temp3=NULL
for(i in 1:length(choose)){
  temp1=rbind(temp1,subset(zj48_unique_b,Month==choose[i]))
  temp2=rbind(temp2,subset(zj48_unique_grans,Month==choose[i]))
  temp3=rbind(temp3,subset(zj48_unique_t,Month==choose[i]))
}

C=temp1$C+temp2$C+temp3$C
U=temp1$U+temp2$U+temp3$U

cumulative=temp1
cumulative$C=C

unique=temp1
unique$U=U

plot_data=unique
pdf(file = "unique_zj48.pdf",width=30,height=20)
zj48_u=ggplot(plot_data,aes(x=Month,y=U,group=Library,color=Library))+
  geom_line(aes(size=2))+scale_color_manual(values=colors,breaks=c("EF1a","MSCV","SFFV"))+
  geom_point(aes(size=2.5))+scale_y_continuous(name="Unique Barcode",limits=c(0,2600))+
  scale_x_discrete(limits=seq(0,31),breaks=seq(0,30,by=5))+ theme_bw()+
  theme(legend.text=element_text(size=25),legend.key.size = unit(1.5, "cm"),legend.title=element_text(size=25),axis.text=element_text(size=40),
        axis.title=element_text(size=40,face="bold"))
zj48_u
dev.off()
zj48_u2=ggplot(plot_data,aes(x=Month,y=U,group=Library,color=Library))+
  geom_line(aes(size=2))+scale_color_manual(values=colors,breaks=c("EF1a","MSCV","SFFV"))+
  geom_point(aes(size=2.5))+scale_y_continuous(name="Unique Barcode",limits=c(0,2600))+
  scale_x_discrete(limits=seq(0,31),breaks=seq(0,30,by=5))+ theme_bw()+
  theme(legend.text=element_text(size=25),legend.key.size = unit(1.5, "cm"),legend.title=element_text(size=25),axis.text=element_text(size=60),
        axis.title=element_text(size=60,face="bold"),legend.position = "none")+ggtitle("ZJ48")

###############################################################################################################
###FIGURE 3C##################################################################################################
###Cumulative Barcode Number###################################################################################
###############################################################################################################
choose=c(1,2,3,4,5,6,9,18,29)
temp1=NULL;temp2=NULL;temp3=NULL
for(i in 1:length(choose)){
  temp1=rbind(temp1,subset(zj41_unique_b,Month==choose[i]))
  temp2=rbind(temp2,subset(zj41_unique_grans,Month==choose[i]))
  temp3=rbind(temp3,subset(zj41_unique_t,Month==choose[i]))
}

C=temp1$C+temp2$C+temp3$C
U=temp1$U+temp2$U+temp3$U

cumulative=temp1
cumulative$C=C

unique=temp1
unique$U=U

plot_data=cumulative
pdf(file = "cumulative_zj41.pdf",width=30,height=20)
zj41_c=ggplot(plot_data,aes(x=Month,y=C,group=Library,color=Library))+
  geom_line(aes(size=2))+scale_color_manual(values=colors,breaks=c("EF1a","MSCV","SFFV"))+
  geom_point(aes(size=2.5))+scale_y_continuous(name="Cumulative Barcode",limits=c(0,10000))+
  scale_x_discrete(limits=seq(0,31),breaks=seq(0,30,by=5))+ theme_bw()+
  theme(legend.text=element_text(size=25),legend.key.size = unit(1.5, "cm"),legend.title=element_text(size=25),axis.text=element_text(size=40),
        axis.title=element_text(size=40,face="bold"))
zj41_c
dev.off()
zj41_c2=ggplot(plot_data,aes(x=Month,y=C,group=Library,color=Library))+
  geom_line(aes(size=2))+scale_color_manual(values=colors,breaks=c("EF1a","MSCV","SFFV"))+
  geom_point(aes(size=2.5))+scale_y_continuous(name="Cumulative Barcode",limits=c(0,10000))+
  scale_x_discrete(limits=seq(0,31),breaks=seq(0,30,by=5))+ theme_bw()+
  theme(legend.text=element_text(size=25),legend.key.size = unit(1.5, "cm"),legend.title=element_text(size=25),axis.text=element_text(size=60),
        axis.title=element_text(size=60,face="bold"),legend.position="none")+ggtitle("ZJ41")

choose=c(1,2,3,4,5,6,7,12,24)
temp1=NULL;temp2=NULL;temp3=NULL
for(i in 1:length(choose)){
  temp1=rbind(temp1,subset(zj48_unique_b,Month==choose[i]))
  temp2=rbind(temp2,subset(zj48_unique_grans,Month==choose[i]))
  temp3=rbind(temp3,subset(zj48_unique_t,Month==choose[i]))
}

C=temp1$C+temp2$C+temp3$C
U=temp1$U+temp2$U+temp3$U

cumulative=temp1
cumulative$C=C

unique=temp1
unique$U=U

plot_data=cumulative
pdf(file = "cumulative_zj48.pdf",width=30,height=20)
zj48_c=ggplot(plot_data,aes(x=Month,y=C,group=Library,color=Library))+
  geom_line(aes(size=2))+scale_color_manual(values=colors,breaks=c("EF1a","MSCV","SFFV"))+
  geom_point(aes(size=2.5))+scale_y_continuous(name="Cumulative Barcode",limits=c(0,10000))+
  scale_x_discrete(limits=seq(0,31),breaks=seq(0,30,by=5))+ theme_bw()+
  theme(legend.text=element_text(size=25),legend.key.size = unit(1.5, "cm"),legend.title=element_text(size=25),axis.text=element_text(size=40),
        axis.title=element_text(size=40,face="bold"))
zj48_c
dev.off()
zj48_c2=ggplot(plot_data,aes(x=Month,y=C,group=Library,color=Library))+
  geom_line(aes(size=2))+scale_color_manual(values=colors,breaks=c("EF1a","MSCV","SFFV"))+
  geom_point(aes(size=2.5))+scale_y_continuous(name="Cumulative Barcode",limits=c(0,10000))+
  scale_x_discrete(limits=seq(0,31),breaks=seq(0,30,by=5))+ theme_bw()+
  theme(legend.text=element_text(size=25),legend.key.size = unit(1.5, "cm"),legend.title=element_text(size=25),axis.text=element_text(size=60),
        axis.title=element_text(size=60,face="bold"),legend.position="none")+ggtitle("ZJ48")

pdf(file = "final_cumulative.pdf",width=30,height=20)
require(gridExtra)
grid.arrange(zj48_c2+theme(axis.text=element_text(size=60),
                           axis.title=element_text(size=60,face="bold")), zj41_c2+theme(axis.text=element_text(size=60),
                                                                                        axis.title=element_text(size=60,face="bold")), ncol=2)
dev.off()

pdf(file = "final_unique.pdf",width=30,height=20)
require(gridExtra)
grid.arrange(zj48_u2+theme(axis.text=element_text(size=60),
                           axis.title=element_text(size=60,face="bold")), zj41_u2+theme(axis.text=element_text(size=60),
                                                                                        axis.title=element_text(size=60,face="bold")), ncol=2)
dev.off()

pdf(file = "final_cu.pdf",width=30,height=20)
require(gridExtra)
grid.arrange(zj48_c2,zj41_c2,zj48_u2, zj41_u2, ncol=2)
dev.off()

plot_data8=plot_data
plot_data1=plot_data


save(plot_data8,plot_data1,file="cumulative_data.Rdata")
###############################################################################################################
###FIGURE 3D##################################################################################################
###Simpson Index###############################################################################################
###############################################################################################################

grans48=c("pp_50trim_zj48_1_month_Grans_index15_IY12.fastq","pp_50trim_zj48_2_month_Grans_index16_IY12.fastq",
          "pp_50trim_zj48_3_month_Grans_index17_IY12.fastq","pp_50trim_zj48_4_month_Grans_442p8ng__index18_IY12.fastq",
          "pp_50trim_ZJ48_5m_Grans_IY_R15_IY24.fastq","pp_50trim_zj48_6_month_Grans_index20_IY12.fastq",
          "pp_50trim_ZJ48_7m_Grans_IY_R17_IY24.fastq",
          "pp_50trim_ZJ48_12m_Grans_IY_R24_IY24.fastq",
          "ZJ48_Grans_24m_sampled_LT2821_LT3_i524_S19_L001_R1_001.fastq")

B48=c("pp_50trim_zj48_1_month_B_cells_index21_IY12.fastq","pp_50trim_zj48_2_month_B_cells_index22_IY12.fastq",
      "pp_50trim_zj48_3_month_B_cells_index23_IY12.fastq", "pp_50trim_zj48_4_month_B_cells_index24_IY12.fastq",
      "pp_50trim_zj48_5_month_B_cells_index25_IY12.fastq","pp_50trim_zj48_6_month_B_cells_index26_IY12.fastq",
      "pp_50trim_ZJ48_7m_B_cells__IY_R21_IY23.fastq",
      "pp_50trim_ZJ48_12m_B_cells__IY_R27_IY23.fastq","ZJ48_Grans_24m_sampled_LT2821_LT3_i524_S19_L001_R1_001.fastq")

t48=c("pp_50trim_zj48_1_month_T_cells_index15_IY13.fastq","pp_50trim_zj48_2_month_T_cells_index_16_IY13.fastq",
      "pp_50trim_zj48_3_month_T_cells_index17_IY13.fastq","pp_50trim_zj48_4_month_T_cells_index18_IY13.fastq",
      "pp_50trim_zj48_5_month_T_cells_index19_IY13.fastq","pp_50trim_zj48_6_month_T_cells_index20_IY13.fastq",
      "pp_50trim_ZJ48_7m_T_cells__IY_R14_IY23.fastq",
      "pp_50trim_ZJ48_12m_T_cells__IY_R17_IY23.fastq","ZJ48_T_24m_sampled_LT2821_LT3_i528_S22_L001_R1_001.fastq")

tempg=zj48_lib7[,grans48];tempb=zj48_lib7[,B48];tempt=zj48_lib7[,t48]
temp=c("comb 1m","comb 2m","comb 3m","comb 4m","comb 5m","comb 6m","comb 7m","comb 12m","comb 24m")
colnames(tempg)=temp
colnames(tempb)=temp
colnames(tempt)=temp
comb_zj48_lib7=rbind(tempg,tempb,tempt)

tempg=zj48_lib8[,grans48];tempb=zj48_lib8[,B48];tempt=zj48_lib8[,t48]
colnames(tempg)=temp
colnames(tempb)=temp
colnames(tempt)=temp
comb_zj48_lib8=rbind(tempg,tempb,tempt)

tempg=zj48_lib11[,grans48];tempb=zj48_lib11[,B48];tempt=zj48_lib11[,t48]
colnames(tempg)=temp
colnames(tempb)=temp
colnames(tempt)=temp
comb_zj48_lib11=rbind(tempg,tempb,tempt)

grans41=c("zj41_1m_Gr_pp_50trim_IY21_R15.fastq","zj41_2m_Gr_293ng_pp_50trim_IY21_R16.fastq","zj41_3m_Gr_pp_50trim_IY21_R17.fastq",
          "zj41_4m_Gr_pp_50trim_IY21_R18.fastq","pp_50trim_ZJ41_5m_Grans_Ann__IY25_R14.fastq","zj41_6m_Gr_pp_50trim_IY21_R20.fastq",
          "zj41_9m_Gr_pp_50trim_IY21_R21.fastq",
          "pp_50trim_ZJ41_18m_Grans__IY25_R25.fastq",
          "ZJ41_Grans_29m_sampled_LT2821_LT3_i516_S13_L001_R1_001.fastq")

B41=c("zj41_1m_B_pp_50trim_IY15_R23.fastq",
      "zj41_2m_B_pp_50trim_IY15_R24.fastq",
      "zj41_3m_B_pp_50trim_IY15_R25.fastq",
      "zj41_4m_B_pp_50trim_IY15_R26.fastq",
      "zj41_5m_B_pp_50trim_IY15_R27.fastq",
      "zj41_6m_B_pp_50trim_IY15_R28.fastq",
      "zj41_9m_B_rep_pp_50trim_IY15_R34.fastq",
      "pp_50trim_ZJ41_18m_B_IY26_R28.fastq",
      "ZJ41_B_29m_sampled_LT2821_LT3_i518_S14_L001_R1_001.fastq")


t41=c("zj41_m1_Tcells_IY09R28.fastq","zj41_m2_Tcells_IY07R28.fastq","zj41_3m_T_rep_pp_50trim_IY22_R25.fastq",
      "zj41_4m_T_rep_pp_50trim_IY22_R26.fastq","zj41_5m_T_rep_pp_50trim_IY22_R27.fastq","zj41_6m_T_pp_50trim_IY22_R20.fastq","zj41_9m_T_rep_pp_50trim_IY22_R33.fastq",
      "pp_50trim_ZJ41_18m_T_IY26_R25.fastq","ZJ41_T_29m_sampled_LT2821_LT3_i521_S16_L001_R1_001.fastq")

zj41_time_points = c(1,2,3,4,5,6,9,18,29)
temp=c("comb 1m","comb 2m","comb 3m","comb 4m","comb 5m","comb 6m","comb 9m","comb 18m","comb 29m")
tempg=zj41_lib7[,grans41];tempb=zj41_lib7[,B41];tempt=zj41_lib7[,t41]
colnames(tempg)=temp
colnames(tempb)=temp
colnames(tempt)=temp
comb_zj41_lib7=rbind(tempg,tempb,tempt)

temp=c("comb 1m","comb 2m","comb 3m","comb 4m","comb 5m","comb 6m","comb 9m","comb 18m","comb 29m")
tempg=zj41_lib8[,grans41];tempb=zj41_lib8[,B41];tempt=zj41_lib8[,t41]
colnames(tempg)=temp
colnames(tempb)=temp
colnames(tempt)=temp
comb_zj41_lib8=rbind(tempg,tempb,tempt)

temp=c("comb 1m","comb 2m","comb 3m","comb 4m","comb 5m","comb 6m","comb 9m","comb 18m","comb 29m")
tempg=zj41_lib11[,grans41];tempb=zj41_lib11[,B41];tempt=zj41_lib11[,t41]
colnames(tempg)=temp
colnames(tempb)=temp
colnames(tempt)=temp
comb_zj41_lib11=rbind(tempg,tempb,tempt)

zj48_time_points = c(1,2,3,4,5,6,7,12,24)
zj41_time_points = c(1,2,3,4,5,6,9,18,29)

colors=c("deeppink", "darkgrey","deepskyblue")
temp=SI_lib(list(comb_zj48_lib7,comb_zj48_lib8,comb_zj48_lib11),
            c("ZJ48 Lib MSCV","ZJ48 EF1a","ZJ41 SFFV"), 
            list(zj48_time_points,zj48_time_points,zj48_time_points),colors)

combined_grans=temp+
  #geom_abline(linetype="dashed",slope=0,intercept=zj48_cd34_lib7, show.legend = TRUE,color=colors[1],size=2)+
  #geom_abline(linetype="dashed",slope=0,intercept=zj48_cd34_lib8, show.legend = TRUE,color=colors[2],size=2)+
  #geom_abline(linetype="dashed",slope=0,intercept=zj48_cd34_lib11, show.legend = TRUE,color=colors[3],size=2)+
  ggtitle("ZJ48")+theme(title=element_text(size=50))

pdf(file = "SI_zj48.pdf",width=30,height=20)
combined_grans
dev.off()

temp=SI_lib(list(zj41_lib7,zj41_lib8,zj41_lib11),
            c("ZJ48 Lib MSCV","ZJ48 EF1a","ZJ41 SFFV"), 
            list(zj41_time_points,zj41_time_points,zj41_time_points),colors)
combined_b=temp+
  #geom_abline(linetype="dashed",slope=0,intercept=zj41_cd34_lib7, show.legend = TRUE,color=colors[1],size=2)+
  #geom_abline(linetype="dashed",slope=0,intercept=zj41_cd34_lib8, show.legend = TRUE,color=colors[2],size=2)+
  #geom_abline(linetype="dashed",slope=0,intercept=zj41_cd34_lib11, show.legend = TRUE,color=colors[3],size=2)+
  ggtitle("ZJ41")+theme(title=element_text(size=50))

pdf(file = "SI_zj41.pdf",width=30,height=20)
combined_b
dev.off()

temp=SI_lib(list(zj48_lib7,zj48_lib8,zj41_lib11),
            c("ZJ48 Lib MSCV","ZJ48 EF1a","ZJ41 SFFV"), 
            list(zj48_time_points,zj48_time_points,zj41_time_points),colors)
combined_bb=temp+
  #geom_abline(linetype="dashed",slope=0,intercept=zj48_cd34_lib7, show.legend = TRUE,color=colors[1],size=2)+
  #geom_abline(linetype="dashed",slope=0,intercept=zj48_cd34_lib8, show.legend = TRUE,color=colors[2],size=2)+
  #geom_abline(linetype="dashed",slope=0,intercept=zj41_cd34_lib11, show.legend = TRUE,color=colors[3],size=2)+
  ggtitle("ZJ41")+theme(title=element_text(size=50))

pdf(file = "final_simpson_high.pdf",width=30,height=20)
combined_bb+scale_y_continuous("Simpson Index",limits=c(.95,1))
dev.off()

pdf(file = "final_simpson.pdf",width=30,height=20)
grid.arrange(combined_grans,combined_b,ncol=2)
dev.off()

###############################################################################################################
###FIGURE 4###################################################################################################
###Stacked area plots##########################################################################################
###############################################################################################################

colors=c("Greys","RdPu","Blues")

zj48_grans=combined_stacked(list(zj48_lib7,zj48_lib8,zj48_lib11),grans48,zj48_time_points,20,plot_title=NULL,colors,n=c(9,9,9),"zj48_grans")+ggtitle("Granulocytes")+ylab("ZJ48 \n Percent Contribution \n of Barcodes")+theme(plot.title = element_text(size=50))
zj48_t=combined_stacked(list(zj48_lib7,zj48_lib8,zj48_lib11),t48,zj48_time_points,20,plot_title=NULL,colors,n=c(9,9,9),"zj48_t")+ggtitle("T Cells")+theme(plot.title = element_text(size=50))
zj48_b=combined_stacked(list(zj48_lib7,zj48_lib8,zj48_lib11),B48,zj48_time_points,20,plot_title=NULL,colors,n=c(9,9,9),"zj48_b")+ggtitle("B Cells")+theme(plot.title = element_text(size=50))


zj41_t=combined_stacked(list(zj41_lib7,zj41_lib8,zj41_lib11),t41,zj41_time_points,20,plot_title=NULL,colors,n=c(9,9,9),"zj41_t")
zj41_b=combined_stacked(list(zj41_lib7,zj41_lib8,zj41_lib11),B41,zj41_time_points,20,plot_title=NULL,colors,n=c(9,9,9),"zj41_b")

temp=matrix(nrow=1085,ncol=35,data=runif(1085*35))
zj41_lib7=zj41_lib7+temp

temp=matrix(nrow=1085,ncol=35,data=runif(1085*35))
zj41_lib8=zj41_lib8+temp

zj41_grans=combined_stacked(list(zj41_lib7,zj41_lib8,zj41_lib11),grans41,zj41_time_points,20,plot_title=NULL,colors,n=c(9,9,9),"zj41_grans")+ylab("ZJ41 \n Percent Contribution \n of Barcodes")

pdf(file = "final_stacked.pdf",width=30,height=20)
grid.arrange(zj41_grans,zj41_t,zj41_b,zj48_grans,zj48_t,zj48_b,ncol=3)
dev.off()

###############################################################################################################
###FIGURE 5###################################################################################################
###Heatmaps###################################################################################################
###############################################################################################################
pdf(file = "final_heat_zj48_lib7.pdf",width=30,height=20)
barcode_ggheatmap(zj48_lib7[,c(grans48,t48,B48)],grid=FALSE)
dev.off()
pdf(file = "final_heat_zj48_lib8.pdf",width=30,height=20)
barcode_ggheatmap(zj48_lib8[,c(grans48,t48,B48)],grid=FALSE)
dev.off()
pdf(file = "final_heat_zj41_lib11.pdf",width=30,height=20)
barcode_ggheatmap(zj41_lib11[,c(grans41,t41,B41)],grid=FALSE)
dev.off()
###############################################################################################################
###FIGURE S1###################################################################################################
###Monte Carlo#################################################################################################
###############################################################################################################

create_divplot.k("MC1.txt","","lib7_K","MC")
create_divplot.k("MC2.txt","","lib8_K","MC")
create_divplot.k("MC3.txt","","lib11_K","MC")

###############################################################################################################
###FIGURE S2###################################################################################################
###Shannon Index###############################################################################################
###############################################################################################################

colors=c("deeppink", "darkgrey","deepskyblue")
SH_g=SH_lib(list(comb_zj48_lib7,comb_zj48_lib8,comb_zj48_lib11),c("ZJ48 MSCV","ZJ48 EF1a","ZJ48 SFFV"), list(zj48_time_points,zj48_time_points,zj48_time_points),colors)
pdf(file = "SH_zj48.pdf",width=30,height=20)
SH_g
dev.off()

SH_b=SH_lib(list(zj41_lib7,zj41_lib8,zj41_lib11),c("ZJ48 MSCV","ZJ48 EF1a","ZJ41 SFFV"), list(zj41_time_points,zj41_time_points,zj41_time_points),colors)+theme(title=element_text(size=50))
pdf(file = "SH_zj41.pdf",width=30,height=20)
SH_b
dev.off()

SH_b=SH_lib(list(zj48_lib7,zj48_lib8,zj41_lib11),c("ZJ48 MSCV","ZJ48 EF1a","ZJ41 SFFV"), list(zj48_time_points,zj48_time_points,zj41_time_points),colors)+theme(title=element_text(size=50))
pdf(file = "SH_high.pdf",width=30,height=20)
SH_b
dev.off()

pdf(file = "final_shannon.pdf",width=30,height=20)
grid.arrange(SH_g,SH_b,ncol=2)
dev.off()
