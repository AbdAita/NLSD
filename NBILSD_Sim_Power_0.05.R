m <- 10000
mu0<- 0
mu1<- 1
mu2<- 2
mu3<- 3
mu4<- 4
mu5<- 5
mu6<- 6
mu8<- 8
sigma <- 1
M <- length (mu0)
F.stat1<-c()
F.stat2<-c()
pvalue.05_1<-c()
pvalue.05_2<-c()
for(j in 1:M){
  for (i in 1:m){
    row<-c(rep(1:4,4))
    column<-c(4,1,3,2,3,2,4,1,1,4,2,3,2,3,1,4)
    treatment<-c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))
    y2<-c(rnorm(4,mu1,sigma),rnorm(4,mu1,sigma),rnorm(4,mu3,sigma),rnorm(4,mu3,sigma))
    W=length(y2)
    Q=runif(W, min=0.2, max=1.5)
    y1<-c()
    for(k in 1:W){
      y1[k]<-y2[k]-Q[k]
    }
    row <- as.factor(row)
    column <- as.factor(column)
    treatment <- as.factor(treatment)
    ########################################################
    LSD1<-data.frame(row,column,treatment,y1)
    LSD1<-LSD1[-c(1),]
    LSD1<-LSD1[-c(5),]
    LSD1<-LSD1[-c(9),]
    LSD1<-LSD1[-c(13),]
    LSD1
    LSD.aov1 <- aov(y1~row+column+treatment,data=LSD1)
    F.stat1[i]<-anova(LSD.aov1)[[4]][3]
    #############################################################
    LSD2<-data.frame(row,column,treatment,y2)
    LSD2<-LSD2[-c(1),]
    LSD2<-LSD2[-c(5),]
    LSD2<-LSD2[-c(9),]
    LSD2<-LSD2[-c(13),]
    LSD2
    LSD.aov2 <- aov(y2~row+column+treatment,data=LSD2)
    F.stat2[i]<-anova(LSD.aov2)[[4]][3]
  }
  F.05 <- qf(.95,3,2)
  reject.05_1 <- ifelse(F.stat1 >= F.05,1,0)
  pvalue.05_1[j] <- mean(reject.05_1)
  
  reject.05_2 <- ifelse(F.stat2 >= F.05,1,0)
  pvalue.05_2[j] <- mean(reject.05_2)
}
power1<-pvalue.05_1
power2<-pvalue.05_2

Mean_power<- matrix(c(power1,power2),ncol=2) # make data
Mean_power
