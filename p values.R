# install.packages("devtools")
# install.packages("ggplot2")
# install.packages("diverse")
# install.packages("gridExtra")
# require(devtools) 
# devtools::install_github("d93espinoza/barcodetrackR")
require(barcodetrackR)
require(ggplot2)
require(diverse)
require(gridExtra)

source("functions.R")

threshold=2000
#sets barcode count threshold

zj48_lib7=read.combined("ZJ48_combined_20180605_lib7.txt")
zj48_lib8=read.combined("ZJ48_combined_20180605_lib8.txt")
zj48_lib11=read.combined("ZJ48_combined_20180605_lib11.txt")
zj41_lib7=read.combined("ZJ41_combined_20180605_lib7.txt")
zj41_lib8=read.combined("ZJ41_combined_20180605_lib8.txt")
zj41_lib11=read.combined("ZJ41_combined_20180605_lib11.txt")

grans48=c("pp_50trim_zj48_1_month_Grans_index15_IY12.fastq","pp_50trim_zj48_2_month_Grans_index16_IY12.fastq",
          "pp_50trim_zj48_3_month_Grans_index17_IY12.fastq","pp_50trim_zj48_4_month_Grans_442p8ng__index18_IY12.fastq",
          "pp_50trim_ZJ48_5m_Grans_IY_R15_IY24.fastq","pp_50trim_zj48_6_month_Grans_index20_IY12.fastq",
          "pp_50trim_ZJ48_7m_Grans_IY_R17_IY24.fastq",
          "pp_50trim_ZJ48_12m_Grans_IY_R24_IY24.fastq",
          "ZJ48_Grans_24m_sampled_LT2821_LT3_i524_S19_L001_R1_001.fastq",
          "ZJ48_33m____Grans___IY33_sampled_IY33_i527_S93_L008_R1_001.fastq")

B48=c("pp_50trim_zj48_1_month_B_cells_index21_IY12.fastq","pp_50trim_zj48_2_month_B_cells_index22_IY12.fastq",
      "pp_50trim_zj48_3_month_B_cells_index23_IY12.fastq", "pp_50trim_zj48_4_month_B_cells_index24_IY12.fastq",
      "pp_50trim_zj48_5_month_B_cells_index25_IY12.fastq","pp_50trim_zj48_6_month_B_cells_index26_IY12.fastq",
      "pp_50trim_ZJ48_7m_B_cells__IY_R21_IY23.fastq",
      "pp_50trim_ZJ48_12m_B_cells__IY_R27_IY23.fastq","ZJ48_B_24m_sampled_LT2821_LT3_i526_S20_L001_R1_001.fastq",
      "ZJ48_33m_2018_5_2_B__rep1_IY34_sampled_IY34_i528_S23_L005_R1_001.fastq")

t48=c("pp_50trim_zj48_1_month_T_cells_index15_IY13.fastq","pp_50trim_zj48_2_month_T_cells_index_16_IY13.fastq",
      "pp_50trim_zj48_3_month_T_cells_index17_IY13.fastq","pp_50trim_zj48_4_month_T_cells_index18_IY13.fastq",
      "pp_50trim_zj48_5_month_T_cells_index19_IY13.fastq","pp_50trim_zj48_6_month_T_cells_index20_IY13.fastq",
      "pp_50trim_ZJ48_7m_T_cells__IY_R14_IY23.fastq",
      "pp_50trim_ZJ48_12m_T_cells__IY_R17_IY23.fastq","ZJ48_T_24m_sampled_LT2821_LT3_i528_S22_L001_R1_001.fastq",
      "ZJ48_33m____T___IY33_sampled_IY33_i522_S89_L008_R1_001.fastq")

length(t48)

zj48_time_points_g = c(1,2,3,4,5,6,7,12,24,33)
zj48_time_points_t= c(1,2,3,4,5,6,7,12,24,33)
zj48_time_points_b= c(1,2,3,4,5,6,7,12,24,33)


grans41=c("zj41_1m_Gr_pp_50trim_IY21_R15.fastq",
          "zj41_2m_Gr_293ng_pp_50trim_IY21_R16.fastq",
          "zj41_3m_Gr_pp_50trim_IY21_R17.fastq",
          "zj41_4m_Gr_pp_50trim_IY21_R18.fastq",
          "pp_50trim_ZJ41_5m_Grans_Ann__IY25_R14.fastq",
          "zj41_6m_Gr_pp_50trim_IY21_R20.fastq",
          "zj41_9m_Gr_pp_50trim_IY21_R21.fastq",
          "pp_50trim_ZJ41_18m_Grans__IY25_R25.fastq",
          "ZJ41_Grans_29m_sampled_LT2821_LT3_i516_S13_L001_R1_001.fastq",
          "ZJ41_38m_2018_5_16_Grans___IY34_sampled_IY34_i507_S7_L005_R1_001.fastq")

B41=c("zj41_1m_B_pp_50trim_IY15_R23.fastq",
      "zj41_2m_B_pp_50trim_IY15_R24.fastq",
      "zj41_3m_B_pp_50trim_IY15_R25.fastq",
      "zj41_4m_B_pp_50trim_IY15_R26.fastq",
      "zj41_5m_B_pp_50trim_IY15_R27.fastq",
      "zj41_6m_B_pp_50trim_IY15_R28.fastq",
      "zj41_9m_B_rep_pp_50trim_IY15_R34.fastq",
      "pp_50trim_ZJ41_18m_B_IY26_R28.fastq",
      "ZJ41_B_29m_sampled_LT2821_LT3_i518_S14_L001_R1_001.fastq",
      "ZJ41_38m_2018_5_16_B___IY34_sampled_IY34_i522_S18_L005_R1_001.fastq")


t41=c("zj41_m1_Tcells_IY09R28.fastq","zj41_m2_Tcells_IY07R28.fastq","zj41_3m_T_rep_pp_50trim_IY22_R25.fastq",
      "zj41_4m_T_rep_pp_50trim_IY22_R26.fastq","zj41_5m_T_rep_pp_50trim_IY22_R27.fastq","zj41_6m_T_pp_50trim_IY22_R20.fastq",
      "zj41_9m_T_rep_pp_50trim_IY22_R33.fastq",
      "pp_50trim_ZJ41_18m_T_IY26_R25.fastq","ZJ41_T_29m_sampled_LT2821_LT3_i521_S16_L001_R1_001.fastq",
      "ZJ41_38m_2018_5_16_T___IY34_sampled_IY34_i520_S16_L005_R1_001.fastq")


zj41_time_points_g = c(1,2,3,4,5,6,9,18,29,38)
zj41_time_points_t= c(1,2,3,4,5,6,9,18,29,38)
zj41_time_points_b= c(1,2,3,4,5,6,9,18,29,38)


zj48_lib7=zj48_lib7[,c(grans48,B48,t48)]
zj48_lib7=barcodetrackR::threshold(zj48_lib7,threshold/4000000)
zj48_lib8=zj48_lib8[,c(grans48,B48,t48)]
zj48_lib8=barcodetrackR::threshold(zj48_lib8,threshold/4000000)
zj48_lib11=zj48_lib11[,c(grans48,B48,t48)]
zj48_lib11=barcodetrackR::threshold(zj48_lib11,threshold/4000000)

zj41_lib7=zj41_lib7[,c(grans41,B41,t41)]
zj41_lib7=barcodetrackR::threshold(zj41_lib7,threshold/4000000)
zj41_lib8=zj41_lib8[,c(grans41,B41,t41)]
zj41_lib8=barcodetrackR::threshold(zj41_lib8,threshold/4000000)
zj41_lib11=zj41_lib11[,c(grans41,B41,t41)]
zj41_lib11=barcodetrackR::threshold(zj41_lib11,threshold/4000000)

par(mfrow=c(2,3))
rn=c(apply(zj48_lib7,2,sum)[1:10],
     apply(zj48_lib8,2,sum)[1:10],
     apply(zj48_lib11,2,sum)[1:10])
unique=c(barcodecount(zj48_lib7[,1:10],count="unique",months=colnames(zj48_lib7)[1:10]),
         barcodecount(zj48_lib8[,1:10],count="unique",months=colnames(zj48_lib7)[1:10]),
         barcodecount(zj48_lib11[1:10],count="unique",months=colnames(zj48_lib7)[1:10]))
df=NULL
df$unique=log10(unique)
df$rn=log10(rn)
df=as.data.frame(df)

plot(df,col=rep(c("deeppink","lightgrey","deepskyblue"),each=10),pch=19,xlab="Log(unique barcodes)",ylab="Log(total reads)")
cor(df,method="pearson")

rn=c(apply(zj48_lib7,2,sum)[11:20],
     apply(zj48_lib8,2,sum)[11:20],
     apply(zj48_lib11,2,sum)[11:20])
unique=c(barcodecount(zj48_lib7[,11:20],count="unique",months=colnames(zj48_lib7)[11:20]),
         barcodecount(zj48_lib8[,11:20],count="unique",months=colnames(zj48_lib7)[11:20]),
         barcodecount(zj48_lib11[,11:20],count="unique",months=colnames(zj48_lib7)[11:20]))
df=NULL
df$unique=log10(unique)
df$rn=log10(rn)
df=as.data.frame(df)

plot(df,col=rep(c("deeppink","lightgrey","deepskyblue"),each=10),pch=19,xlab="Log(unique barcodes)",ylab="Log(total reads)")
cor(df,method="pearson")

rn=c(apply(zj48_lib7,2,sum)[21:30],
     apply(zj48_lib8,2,sum)[21:30],
     apply(zj48_lib11,2,sum)[21:30])
unique=c(barcodecount(zj48_lib7[,21:30],count="unique",months=colnames(zj48_lib7)[21:30]),
         barcodecount(zj48_lib8[,21:30],count="unique",months=colnames(zj48_lib7)[21:30]),
         barcodecount(zj48_lib11[,21:30],count="unique",months=colnames(zj48_lib7)[21:30]))
df=NULL
df$unique=log10(unique)
df$rn=log10(rn)
df=as.data.frame(df)

plot(df,col=rep(c("deeppink","lightgrey","deepskyblue"),each=10),pch=19,xlab="Log(unique barcodes)",ylab="Log(total reads)")
cor(df,method="pearson")


###zj41

rn=c(apply(zj41_lib7,2,sum)[1:10],
     apply(zj41_lib8,2,sum)[1:10],
     apply(zj41_lib11,2,sum)[1:10])
unique=c(barcodecount(zj41_lib7[,1:10],count="unique",months=colnames(zj41_lib7)[1:10]),
         barcodecount(zj41_lib8[,1:10],count="unique",months=colnames(zj41_lib7)[1:10]),
         barcodecount(zj41_lib11[1:10],count="unique",months=colnames(zj41_lib7)[1:10]))
df=NULL
df$unique=log10(unique)
df$rn=log10(rn)
df=as.data.frame(df)

plot(df,col=rep(c("deeppink","lightgrey","deepskyblue"),each=10),pch=19,xlab="Log(unique barcodes)",ylab="Log(total reads)")
cor(df,method="pearson")

rn=c(apply(zj41_lib7,2,sum)[11:20],
     apply(zj41_lib8,2,sum)[11:20],
     apply(zj41_lib11,2,sum)[11:20])
unique=c(barcodecount(zj41_lib7[,11:20],count="unique",months=colnames(zj41_lib7)[11:20]),
         barcodecount(zj41_lib8[,11:20],count="unique",months=colnames(zj41_lib8)[11:20]),
         barcodecount(zj41_lib11[,11:20],count="unique",months=colnames(zj41_lib11)[11:20]))
df=NULL
df$unique=log10(unique)
df$rn=log10(rn)
df=as.data.frame(df)

plot(df,col=rep(c("deeppink","lightgrey","deepskyblue"),each=10),pch=19,xlab="Log(unique barcodes)",ylab="Log(total reads)")
cor(df,method="pearson")

rn=c(apply(zj41_lib7,2,sum)[21:30],
     apply(zj41_lib8,2,sum)[21:30],
     apply(zj41_lib11,2,sum)[21:30])
unique=c(barcodecount(zj41_lib7[,21:30],count="unique",months=colnames(zj41_lib7)[21:30]),
         barcodecount(zj41_lib8[,21:30],count="unique",months=colnames(zj41_lib8)[21:30]),
         barcodecount(zj41_lib11[,21:30],count="unique",months=colnames(zj41_lib11)[21:30]))
df=NULL
df$unique=log10(unique)
df$rn=log10(rn)
df=as.data.frame(df)

plot(df,col=rep(c("deeppink","lightgrey","deepskyblue"),each=10),pch=19,xlab="Log(unique barcodes)",ylab="Log(total reads)")
cor(df,method="pearson")

