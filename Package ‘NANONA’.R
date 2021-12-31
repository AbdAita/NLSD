NANOVA1 = function(obs,treatment,y1,y2) 
{
  one1=matrix(1,length(y1),1)
  x1=as.matrix(table(obs,treatment))
  b=ncol(x1)
  ########################################
  ######################################## 
  y1...=t(one1)%*%y1
  y1i..=t(x1)%*%y1
  k=colSums(x1)
  N=sum(k)
  SStotal1=t(y1)%*%y1-y1...^2/(N)
  SStreatment1=sum(y1i..^2/k)-y1...^2/(N)
  SSE1=SStotal1-SStreatment1
  ######################
  one2=matrix(1,length(y2),1)
  x1=as.matrix(table(obs,treatment))
  b=ncol(x1)
  ########################################
  ######################################## 
  y2...=t(one2)%*%y2
  y2i..=t(x1)%*%y2
  k=colSums(x1)
  N=sum(k)
  SStotal2=t(y2)%*%y2-y2...^2/(N)
  SStreatment2=sum(y2i..^2/k)-y2...^2/(N)
  SSE2=SStotal2-SStreatment2
  ########################################
  meantreatment1=SStreatment1/(b-1)
  meantreatment2=SStreatment2/(b-1)
  meanE1=SSE1/(N-b)
  meanE2=SSE2/(N-b)
  ####################################### 
  Ftreatment1=meantreatment1/meanE1
  Ftreatment2=meantreatment2/meanE2
  #######################################
  p.value.treatment1=pf(Ftreatment1, df1=(b-1), df2=(N-b), ncp=0, lower.tail = FALSE)
  p.value.treatment2=pf(Ftreatment2, df1=(b-1), df2=(N-b), ncp=0, lower.tail = FALSE)
  ####################################### 
  df=c((b-1),(N-b),(N-1))
  ss=c(SStreatment1,SSE1,SStotal1)
  ms=c(meantreatment1,meanE1,NA)
  F=c(Ftreatment1,NA,NA)
  p.value=c(p.value.treatment1,NA,NA)
  anova.table=cbind(df,ss,ms,F,p.value)
  rownames(anova.table)=c("Treatment_L","Error_L","Total_L")
  colnames(anova.table)=c("Df","Sum Sq","Mean Sq", "F-value","P-value")
  return(anova.table)
} 
######################################################################################################
NANOVA2 = function(obs,treatment,y1,y2) 
{
  one1=matrix(1,length(y1),1)
  x1=as.matrix(table(obs,treatment))
  b=ncol(x1)
  ########################################
  ######################################## 
  y1...=t(one1)%*%y1
  y1i..=t(x1)%*%y1
  k=colSums(x1)
  N=sum(k)
  SStotal1=t(y1)%*%y1-y1...^2/(N)
  SStreatment1=sum(y1i..^2/k)-y1...^2/(N)
  SSE1=SStotal1-SStreatment1
  ######################
  one2=matrix(1,length(y2),1)
  x1=as.matrix(table(obs,treatment))
  b=ncol(x1)
  ########################################
  ######################################## 
  y2...=t(one2)%*%y2
  y2i..=t(x1)%*%y2
  k=colSums(x1)
  N=sum(k)
  SStotal2=t(y2)%*%y2-y2...^2/(N)
  SStreatment2=sum(y2i..^2/k)-y2...^2/(N)
  SSE2=SStotal2-SStreatment2
  ########################################
  meantreatment1=SStreatment1/(b-1)
  meantreatment2=SStreatment2/(b-1)
  meanE1=SSE1/(N-b)
  meanE2=SSE2/(N-b)
  ####################################### 
  Ftreatment1=meantreatment1/meanE1
  Ftreatment2=meantreatment2/meanE2
  #######################################
  p.value.treatment1=pf(Ftreatment1, df1=(b-1), df2=(N-b), ncp=0, lower.tail = FALSE)
  p.value.treatment2=pf(Ftreatment2, df1=(b-1), df2=(N-b), ncp=0, lower.tail = FALSE)
  ####################################### 
  df=c((b-1),(N-b),(N-1))
  ss=c(SStreatment2,SSE2,SStotal2)
  ms=c(meantreatment2,meanE2,NA)
  F=c(Ftreatment2,NA,NA)
  p.value=c(p.value.treatment2,NA,NA)
  anova.table=cbind(df,ss,ms,F,p.value)
  rownames(anova.table)=c("Treatment_U","Error_U","Total_U")
  colnames(anova.table)=c("Df","Sum Sq","Mean Sq", "F-value","P-value")
  return(anova.table)
} 
##########################################
NANOVA = function(obs,treatment,y1,y2)
{
  output1=NANOVA1(obs,treatment,y1,y2) 
  output2=NANOVA2(obs,treatment,y1,y2)
  newlist <- list(output1,output2) 
  return(newlist)
}