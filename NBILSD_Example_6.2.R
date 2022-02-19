#Example 6.2# NBILSD
###############################################################
#Lower Level#
row_L<-c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))
column_L<-c(rep(1:4,4))
treatment_L<-c(1,2,3,4,2,1,4,3,3,4,1,2,4,3,2,1)
y_L<-c(15,20,22,17,15,16,15,19,18,14,10,17,13,15,19,10)
LSD_L<-data.frame(row_L,column_L,treatment_L,y_L)
LSD_L<-LSD_L[-c(1),]
LSD_L<-LSD_L[-c(7),]
LSD_L<-LSD_L[-c(8),]
LSD_L<-LSD_L[-c(12),]
LSD_L
LSD_L$row_L <- as.factor(LSD_L$row_L)
LSD_L$column_L <- as.factor(LSD_L$column_L)
LSD_L$treatment_adj._L <- as.factor(LSD_L$treatment_L)
LSD.aov_L <- aov(y_L~row_L+column_L+treatment_adj._L,data=LSD_L)
#############################################################
#Upper Level#
row_U<-c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))
column_U<-c(rep(1:4,4))
treatment_U<-c(1,2,3,4,2,1,4,3,3,4,1,2,4,3,2,1)
y_U<-c(20,25,27,22,20,21,19,24,24,19,15,22,17,20,24,16)
LSD_U<-data.frame(row_U,column_U,treatment_U,y_U)
LSD_U<-LSD_U[-c(1),]
LSD_U<-LSD_U[-c(7),]
LSD_U<-LSD_U[-c(8),]
LSD_U<-LSD_U[-c(12),]
LSD_U
LSD_U$row_U <- as.factor(LSD_U$row_U)
LSD_U$column_U <- as.factor(LSD_U$column_U)
LSD_U$treatment_adj._U <- as.factor(LSD_U$treatment_U)
LSD.aov_U <- aov(y_U~row_U+column_U+treatment_adj._U,data=LSD_U)
#############################################################
output1=anova(LSD.aov_L)
output2=anova(LSD.aov_U)
NBILSD <- list(output1,output2) 
NBILSD 
