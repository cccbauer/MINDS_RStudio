# Create graphs for the MINDS Behavioral Paper

#install.packages("ggplot2")
library(reshape)
library(ggplot2)
df <- read.csv("MINDS_RS_032017.csv")
names(df)
attach(df)
names(df)

df$treatment <- as.factor(df$treatment)


Go_plot<-ggplot(df,aes(y =delta_go , x =dmn_fpn_delta,colour=treatment,shape=treatment)) +
  geom_point() + 
  geom_smooth(method="lm", fill=NA,aes(group = treatment))+
  ggtitle("Go vs DMN conn")+
  xlab("Δ DMN connectivity Z-score")+
  ylab("Δ Go Z-score")
Go_plot

NoGo_plot<-ggplot(df,aes(y =z_delta_nogo , x =dmn_fpn_delta,colour=treatment,shape=treatment)) +
  geom_point() + geom_smooth(method="lm", fill=NA)+
  ggtitle("NoGo vs DMN conn")+
  xlab("Δ DMN connectivity Z-score")+
  ylab("Δ NoGo Z-score")
NoGo_plot


PSS_plot<-ggplot(df,aes(y =zs_PSS_delta , x =dmn_fpn_delta,colour=treatment,shape=treatment)) +
  geom_point() + geom_smooth(method="lm", fill=NA)+
  ggtitle("PSS vs DMN conn")+
  xlab("Δ DMN connectivity Z-score")+
  ylab("Δ PSS Z-score")
PSS_plot

TOMW_plot<-ggplot(df,aes(y =delta_tomw , x =dmn_fpn_delta,colour=treatment,shape=treatment)) +
  geom_point() + geom_smooth(method="lm", fill=NA)+
  ggtitle("TOMW vs DMN conn")+
  xlab("Δ DMN connectivity Z-score")+
  ylab("Δ TOMW Z-score")
TOMW_plot


Go_pcc_correl<-ggplot(df,aes(y =delta_go , x =dmn_pcc_gocor_delta,colour=treatment,shape=treatment)) +
  geom_point() + 
  geom_smooth(method="lm", fill=NA,aes(group = treatment))+
  ggtitle("Go vs DMN/PCC conn")+
  xlab("Δ DMN/PCC connectivity Z-score")+
  ylab("Δ Go Z-score")
Go_pcc_correl


NoGo_pcc_correl<-ggplot(df,aes(y =z_delta_nogo , x =dmn_pcc_gocor_delta,colour=treatment,shape=treatment)) +
  geom_point() + 
  geom_smooth(method="lm", fill=NA,aes(group = treatment))+
  ggtitle("NoGo vs DMN/PCC conn")+
  xlab("Δ DMN/PCC connectivity Z-score")+
  ylab("Δ NoGo Z-score")
NoGo_pcc_correl

boxplot(bilat_amy_pcc_acc_pre,bilat_amy_pcc_acc_post)
Amy_PSS<-ggplot(df,aes(y =zs_PSS_delta , x =bilat_amy_pcc_acc_delta,colour=treatment,shape=treatment)) +
  geom_point() + 
  geom_smooth(method="lm", fill=NA,aes(group = treatment))+
  ggtitle("Amy vs PSS")+
  xlab("Δ bilat Amy/PCC Z-score")+
  ylab("Δ PSS Z-score")
Amy_PSS

boxplot(bilat_amy_pcc_acc_pre,bilat_amy_pcc_acc_post)
ACC_PSS<-ggplot(df,aes(y =zs_PSS_delta , x =cms_acc_f_tx_pss_correl_delta,colour=treatment,shape=treatment)) +
  geom_point() + 
  geom_smooth(method="lm", fill=NA,aes(group = treatment))+
  ggtitle("CMS/ACC vs PSS")+
  xlab("Δ bilat CMS/ACC Z-score")+
  ylab("Δ PSS Z-score")
ACC_PSS




boxplot(bilat_amy_pcc_f_tx_pss_correl_pre,bilat_amy_pcc_f_tx_pss_correl_post)
Amy_PSS<-ggplot(df,aes(y =zs_PSS_delta , x =bilat_amy_pcc_f_tx_pss_correl_delta,colour=treatment,shape=treatment)) +
  geom_point() + 
  geom_smooth(method="lm", fill=NA,aes(group = treatment))+
  ggtitle("Amy vs PSS")+
  xlab("Δ bilat Amy/PCC Z-score")+
  ylab("Δ PSS Z-score")
Amy_PSS


boxplot( r_amy_pcc_pre_f_tx_pss_corr, r_amy_pcc_post_f_tx_pss_corr)
rAmyPCC_PSS_box<-ggplot(df,aes(y =r_amy_pcc_delta_f_tx_pss_corr , x = treatment,colour=treatment,shape=treatment))+
  geom_boxplot()
rAmyPCC_PSS_box



rAmyPCC_PSS<-ggplot(df,aes(y =zs_PSS_delta , x = r_amy_pcc_delta_f_tx_pss_corr,colour=treatment,shape=treatment)) +
  geom_point() + 
  geom_smooth(method="lm", fill=NA,aes(group = treatment))+
  ggtitle("rAmy/PCC conn vs PSS")+
  xlab("Δ rAmy/PCC connectivity Z-score")+
  ylab("Δ PSS Z-score")
rAmyPCC_PSS

cn_r_amy_pcc_delta=subset(df, treatment == "control")$r_amy_pcc_delta_f_tx_pss_corr
tx_r_amy_pcc_delta=subset(df, treatment == "treatment")$r_amy_pcc_delta_f_tx_pss_corr
cn_pss_delta=subset(df, treatment == "control")$zs_PSS_delta
tx_pss_delta=subset(df, treatment == "treatment")$zs_PSS_delta

cor.test(cn_r_amy_pcc_delta,cn_pss_delta)
cor.test(tx_r_amy_pcc_delta,tx_pss_delta)





boxplot(cms_acc_f_tx_pss_correl_pre,cms_acc_f_tx_pss_correl_post)



ACC_PSS_all<-ggplot(df,aes(y =zs_PSS_delta , x =cms_acc_f_tx_pss_correl_delta)) +
  geom_point() + 
  geom_smooth(method="lm")+
  ggtitle("CMS/ACC vs PSS")+
  xlab("Δ bilat CMS/ACC Z-score")+
  ylab("Δ PSS Z-score")
ACC_PSS_all


rAMY_PSS<-ggplot(df,aes(y =zs_PSS_delta , x =r_amy_lDLPFC_delta,colour=treatment,shape=treatment)) +
  geom_point() + 
  geom_smooth(method="lm", fill=NA,aes(group = treatment))+
  ggtitle("rAMY lDLPFC vs PSS")+
  xlab("Δ bilat CMS/rAMY Z-score")+
  ylab("Δ PSS Z-score")
rAMY_PSS


boxplot(r_amy_lDLPFC_pre,r_amy_lDLPFC_post)


rAMY_PSS_all<-ggplot(df,aes(y =zs_PSS_delta , x =r_amy_lDLPFC_delta)) +
  geom_point() + 
  geom_smooth(method="lm")+
  ggtitle("rAMY vs PSS")+
  xlab("Δ bilat CMS/rAMY Z-score")+
  ylab("Δ PSS Z-score")
rAMY_PSS_all

