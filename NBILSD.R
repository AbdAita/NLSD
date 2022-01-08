row_L<-c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))
column_L<-c(rep(1:4,4))
treatment_L<-c(3,4,2,1,1,2,4,3,4,3,1,2,2,1,3,4)
y_L<-c(235,236,218,268,251,241,227,229,234,273,274,226,195,270,230,225)
LSD_L<-data.frame(row_L,column_L,treatment_L,y_L)
LSD_L<-LSD_L[-c(1),]
LSD_L<-LSD_L[-c(5),]
LSD_L<-LSD_L[-c(9),]
LSD_L<-LSD_L[-c(13),]
LSD_L
LSD_L$row_L <- as.factor(LSD_L$row_L)
LSD_L$column_L <- as.factor(LSD_L$column_L)
LSD_L$treatment_adj._L <- as.factor(LSD_L$treatment_L)
LSD.aov_L <- aov(y_L~row_L+column_L+treatment_adj._L,data=LSD_L)
#############################################################
row_U<-c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))
column_U<-c(rep(1:4,4))
treatment_U<-c(3,4,2,1,1,2,4,3,4,3,1,2,2,1,3,4)
y_U<-c(235,237,219,268,252,241,228,230,234,274,275,227,195,271,231,226)
LSD_U<-data.frame(row_U,column_U,treatment_U,y_U)
LSD_U<-LSD_U[-c(1),]
LSD_U<-LSD_U[-c(5),]
LSD_U<-LSD_U[-c(9),]
LSD_U<-LSD_U[-c(13),]
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
