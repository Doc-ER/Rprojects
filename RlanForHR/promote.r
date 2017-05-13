
#GetThePromote函数返回一个dataframe,
# file 为一个dataframe，且不能有空项。第一行含有“副高5”，“副高6”，“副高7”，中级8，中级9，中级10，初级11，初级12，初级13字样。
#groups 指明那些职称想用来计算的基数，如副高，中级，初级。
# grade 具体到职称的级数 如 副高7， 计算其有几个名额可以用来晋升。
# weight 对应grade 分配到的计算系数。
GetThePromote<-function(file = file, groups=groups, grade=grade, weight = weight){
  
  resultMatrix = NULL
  
  for (i in groups){
   groupFile<- file[, grep(i, colnames(file))]#将需要涉及到计算的组的数据提取出来
   newName1<- paste(colnames(groupFile)[1] ,"晋升数", sep="")
   newName2<- paste(colnames(groupFile)[2] ,"晋升数", sep="")
   
   indexWeightForGroup = grep(i, grade)#将组别如副高，中级，初级所对应的具体等级如副高５还是６等对应的系数序号提取。
   resultGroup<-NULL
   for (j in 1:nrow(groupFile)){
    
     groupFileEveryRow<- groupFile[j,]
     parsedNum<- parseNumber(groupFileEveryRow,weight[indexWeightForGroup] )
     resultGroup<- rbind(resultGroup, parsedNum )
   }
   colnames(resultGroup)<-c(newName1, newName2)
   
   resultMatrix<- cbind(resultMatrix, resultGroup )
  }
  
  return (as.matrix(cbind(file, resultMatrix)))
  
}



#parseNumber这个函数对一组数字进行计算。
parseNumber<-function(numbers, efficient){
  prenum1 = sum(numbers)*efficient[1] - numbers[1]
  prenum2 = sum(numbers)*efficient[2] - numbers[2]
  if (prenum1 <=0 & prenum2<=0){
    #如果算下来都是负数，则晋升数都为０
    return(c(0,0))
  }else if ((prenum1 >0 & prenum1<1) & (prenum2 >0 & prenum2<1)){
    #如果算下来两个都是大于０的小数,二者相加
    #如果大于等于１则第一位晋升１，第二位０
    if((prenum1+prenum2) >=1){
      return(c(1, 0))
    }else{
      return(c(0,0))
    }
    
  }else if((prenum1 >0 & prenum1<1) & (prenum2 >=1)){
    #如果第一位是小数，第二位是大于１的数
    intPrenum2 = trunc(prenum2)
    digitPrenum2<- prenum2 - intPrenum2
    if(prenum1+digitPrenum2 <1){
      return(c(0, intPrenum2))
    }else if (prenum1+digitPrenum2 >= 1){
      return(c(1, intPrenum2))
    }
  }else if(prenum1 >=1 & (prenum2 <1 & prenum2 > 0)){
    #如果第一位是大于１的，第二位是小数
    intPrenum1<- trunc(prenum1)
    digitPrenum1<- prenum1 - intPrenum1
    if(digitPrenum1+prenum2 >=1){
      return(c(intPrenum1+1, 0))
    }else if (digitPrenum1+prenum2 < 1){
      return(c(intPrenum1, 0))
    }
  }else if(prenum1 >=1 & prenum2 >=1){
    #两者都大于等于１
    intPrenum1<- trunc(prenum1)
    intPrenum2 = trunc(prenum2)
    digitPrenum1<- prenum1 - intPrenum1
    digitPrenum2<- prenum2 - intPrenum2
    if(digitPrenum1+digitPrenum2>=1){
      return(c(intPrenum1+1, intPrenum2))
    }else {
      return(c(intPrenum1, intPrenum2))
    }
  }else if(prenum1 >=1 & prenum2 <=0){
    #第一位大于等于１，第二位是个负数或０
    return(c(trunc(prenum1), 0))
  }else if((prenum1 >=0 & prenum1 <1) & prenum2 <=0){
　　#第一位是个小数或０，第二位是个负数或０
    return(c(0,0))
  }else if((prenum2 >=0 & prenum2 <1) & prenum1 <=0 ){
　　#第一位是负数或０，　第二位是个小数
    return(c(0,0))
  }else if(prenum1 == 1 & prenum2 <=0){
　　#第一位等于１，第二位是个负数或０
    return(c(1,0))
    
  }else if(prenum2 == 1 & prenum1 <=0){
　　#第一位是个负数或０，第二位等于１
    return(c(0,1))
  }else if(prenum1 <=0 & prenum2 >=1  ){
　　#第一位是个负数或０，　第二位大于等于１
    return(c(0,trunc(prenum2)))
  }else{
    return(c(NA,NA))
  }
  

}


#################################################################################################
thedata <- read.csv("test.csv", header=T)
ResultFile<- GetThePromote(file = thedata, groups=c("副高","中级","初级"), 
                        grade= c("副高5","副高6","中级8","中级9","初级11", "初级12"),
                        weight= c(0.2, 0.4, 0.3,0.4,0.5,0.5 ))

write.csv(ResultFile, file = "ResultFinal.csv")
