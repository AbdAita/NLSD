N_LSD1 = function(obs,row,column,treatment,y1,y2)
{
  one1=matrix(1,length(y1),1)
  x1=as.matrix(table(obs,row))
  x2=as.matrix(table(obs,column))
  x3=as.matrix(table(obs,treatment))
  b=ncol(x1)
  ########################################
  ######################################## 
  y1...=t(one1)%*%y1
  y1i..=t(x1)%*%y1
  y1.j.=t(x2)%*%y1
  y1..k=t(x3)%*%y1
  k=colSums(x1)
  N=sum(k)
  SStotal1=t(y1)%*%y1-y1...^2/(N)
  SSrow1=sum(y1i..^2/k)-y1...^2/(N)
  SScolumn1=sum(y1.j.^2/k)-y1...^2/(N)
  SStreatment1=sum(y1..k^2/k)-y1...^2/(N)
  SSE1=SStotal1-SSrow1-SScolumn1-SStreatment1
  ######################
  one2=matrix(1,length(y2),1)
  x1=as.matrix(table(obs,row))
  x2=as.matrix(table(obs,column))
  x3=as.matrix(table(obs,treatment))
  b=ncol(x1)
  ########################################
  ######################################## 
  y2...=t(one2)%*%y2
  y2i..=t(x1)%*%y2
  y2.j.=t(x2)%*%y2
  y2..k=t(x3)%*%y2
  k=colSums(x1)
  N=sum(k)
  SStotal2=t(y2)%*%y2-y2...^2/(N)
  SSrow2=sum(y2i..^2/k)-y2...^2/(N)
  SScolumn2=sum(y2.j.^2/k)-y2...^2/(N)
  SStreatment2=sum(y2..k^2/k)-y2...^2/(N)
  SSE2=SStotal2-SSrow2-SScolumn2-SStreatment2
  ########################################
  meanrow1=SSrow1/(b-1)
  meanrow2=SSrow2/(b-1)
  meancolumn1=SScolumn1/(b-1)
  meancolumn2=SScolumn2/(b-1)
  meantreatment1=SStreatment1/(b-1)
  meantreatment2=SStreatment2/(b-1)
  meanE1=SSE1/((b-1)*(b-2))
  meanE2=SSE2/((b-1)*(b-2))
  ####################################### 
  Frow1=meanrow1/meanE1
  Frow2=meanrow2/meanE2
  Fcolumn1=meancolumn1/meanE1
  Fcolumn2=meancolumn2/meanE2
  Ftreatment1=meantreatment1/meanE1
  Ftreatment2=meantreatment2/meanE2
  #######################################
  p.value.row1=pf(Frow1, df1=(b-1), df2=((b-1)*(b-2)), ncp=0, lower.tail = FALSE)
  p.value.row2=pf(Frow2, df1=(b-1), df2=((b-1)*(b-2)), ncp=0, lower.tail = FALSE)
  p.value.column1=pf(Fcolumn1, df1=(b-1), df2=((b-1)*(b-2)), ncp=0, lower.tail = FALSE)
  p.value.column2=pf(Fcolumn2, df1=(b-1), df2=((b-1)*(b-2)), ncp=0, lower.tail = FALSE)
  p.value.treatment1=pf(Ftreatment1, df1=(b-1), df2=((b-1)*(b-2)), ncp=0, lower.tail = FALSE)
  p.value.treatment2=pf(Ftreatment2, df1=(b-1), df2=((b-1)*(b-2)), ncp=0, lower.tail = FALSE)
  ####################################### 
  df=c((b-1),(b-1),(b-1),((b-1)*(b-2)),(N-1))
  ss=c(SSrow1,SScolumn1,SStreatment1,SSE1,SStotal1)
  ms=c(meanrow1,meancolumn1,meantreatment1,meanE1,NA)
  F=c(Frow1,Fcolumn1,Ftreatment1,NA,NA)
  p.value=c(p.value.row1,p.value.column1,p.value.treatment1,NA,NA)
  anova.table=cbind(df,ss,ms,F,p.value)
  rownames(anova.table)=c("Row_L","Column_L","Treatment_L","Error_L","Total_L")
  colnames(anova.table)=c("Df","Sum Sq","Mean Sq", "F-value","P-value")
  return(anova.table)
} 
######################################################################################################
N_LSD2 = function(obs,row,column,treatment,y1,y2)
{
  one1=matrix(1,length(y1),1)
  x1=as.matrix(table(obs,row))
  x2=as.matrix(table(obs,column))
  x3=as.matrix(table(obs,treatment))
  b=ncol(x1)
  ########################################
  ######################################## 
  y1...=t(one1)%*%y1
  y1i..=t(x1)%*%y1
  y1.j.=t(x2)%*%y1
  y1..k=t(x3)%*%y1
  k=colSums(x1)
  N=sum(k)
  SStotal1=t(y1)%*%y1-y1...^2/(N)
  SSrow1=sum(y1i..^2/k)-y1...^2/(N)
  SScolumn1=sum(y1.j.^2/k)-y1...^2/(N)
  SStreatment1=sum(y1..k^2/k)-y1...^2/(N)
  SSE1=SStotal1-SSrow1-SScolumn1-SStreatment1
  ######################
  one2=matrix(1,length(y2),1)
  x1=as.matrix(table(obs,row))
  x2=as.matrix(table(obs,column))
  x3=as.matrix(table(obs,treatment))
  b=ncol(x1)
  ########################################
  ######################################## 
  y2...=t(one2)%*%y2
  y2i..=t(x1)%*%y2
  y2.j.=t(x2)%*%y2
  y2..k=t(x3)%*%y2
  k=colSums(x1)
  N=sum(k)
  SStotal2=t(y2)%*%y2-y2...^2/(N)
  SSrow2=sum(y2i..^2/k)-y2...^2/(N)
  SScolumn2=sum(y2.j.^2/k)-y2...^2/(N)
  SStreatment2=sum(y2..k^2/k)-y2...^2/(N)
  SSE2=SStotal2-SSrow2-SScolumn2-SStreatment2
  ########################################
  meanrow1=SSrow1/(b-1)
  meanrow2=SSrow2/(b-1)
  meancolumn1=SScolumn1/(b-1)
  meancolumn2=SScolumn2/(b-1)
  meantreatment1=SStreatment1/(b-1)
  meantreatment2=SStreatment2/(b-1)
  meanE1=SSE1/((b-1)*(b-2))
  meanE2=SSE2/((b-1)*(b-2))
  ####################################### 
  Frow1=meanrow1/meanE1
  Frow2=meanrow2/meanE2
  Fcolumn1=meancolumn1/meanE1
  Fcolumn2=meancolumn2/meanE2
  Ftreatment1=meantreatment1/meanE1
  Ftreatment2=meantreatment2/meanE2
  #######################################
  p.value.row1=pf(Frow1, df1=(b-1), df2=((b-1)*(b-2)), ncp=0, lower.tail = FALSE)
  p.value.row2=pf(Frow2, df1=(b-1), df2=((b-1)*(b-2)), ncp=0, lower.tail = FALSE)
  p.value.column1=pf(Fcolumn1, df1=(b-1), df2=((b-1)*(b-2)), ncp=0, lower.tail = FALSE)
  p.value.column2=pf(Fcolumn2, df1=(b-1), df2=((b-1)*(b-2)), ncp=0, lower.tail = FALSE)
  p.value.treatment1=pf(Ftreatment1, df1=(b-1), df2=((b-1)*(b-2)), ncp=0, lower.tail = FALSE)
  p.value.treatment2=pf(Ftreatment2, df1=(b-1), df2=((b-1)*(b-2)), ncp=0, lower.tail = FALSE)
  ####################################### 
  df=c((b-1),(b-1),(b-1),((b-1)*(b-2)),(N-1))
  ss=c(SSrow2,SScolumn2,SStreatment2,SSE2,SStotal2)
  ms=c(meanrow2,meancolumn2,meantreatment2,meanE2,NA)
  F=c(Frow2,Fcolumn2,Ftreatment2,NA,NA)
  p.value=c(p.value.row2,p.value.column2,p.value.treatment2,NA,NA)
  anova.table=cbind(df,ss,ms,F,p.value)
  rownames(anova.table)=c("Row_U","Column_U","Treatment_U","Error_U","Total_U")
  colnames(anova.table)=c("Df","Sum Sq","Mean Sq", "F-value","P-value")
  return(anova.table)
} 
##########################################
N_LSD = function(obs,row,column,treatment,y1,y2)
{
  output1=N_LSD1(obs,row,column,treatment,y1,y2) 
  output2=N_LSD2(obs,row,column,treatment,y1,y2)
  newlist <- list(output1,output2) 
  return(newlist)
}