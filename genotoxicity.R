library(barcodetrackR)
library(ggplot2)
library(diverse)

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
