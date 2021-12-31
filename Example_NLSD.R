obs<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
row<-c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))
column<-c(rep(1:4,4))
treatment<-c(3,4,2,1,1,2,4,3,4,3,1,2,2,1,3,4)
y1<-c(235,236,218,268,251,241,227,229,234,273,274,226,195,270,230,225)
y2<-c(235,237,219,268,252,241,228,230,234,274,275,227,195,271,231,226)
NLSD<-data.frame(obs,row,column,treatment,y1,y2)
NLSD$row <- as.factor(NLSD$row)
NLSD$column <- as.factor(NLSD$column)
NLSD$treatment <- as.factor(NLSD$treatment)
NLSD
N_LSD(obs,row,column,treatment,y1,y2)


