library(tidyverse)
library(fs)

file_path = fs::dir_ls('specdata')

# FUNCTION 1


pollutantmean = function(directory, pollutant, id){
  csum = 0
  row_count = 0
  for (i in id){
    file_path = fs::dir_ls(directory)
    data = read.csv(file_path[i])
    for (j in seq_len(nrow(data))){
      if (!is.na(data[j,pollutant])){
        row_count = row_count + 1 
      }
    }
    val = sum(data[,pollutant],na.rm = T)
    sum = sum + val 
  }
  mean_val = sum / row_count
  return(mean_val)
}

pollutantmean('specdata','sulfate',1:10)
pollutantmean('specdata','sulfate',2)
pollutantmean('specdata','nitrate',1:332)


# FUNCTION 2

complete = function(directory,id){
  df1 = data.frame(ID = numeric(0),nobs=numeric(0))
  df1
  for(i in id){
    file_path = fs::dir_ls(directory)
    data = read.csv(file_path[i])
    cc = complete.cases(data[,'sulfate'],data[,'nitrate'])
    r = nrow(data[cc,])
    df2 = data.frame(ID=i,nobs=r)
    df1 = rbind(df1,df2)
  }
  return(df1)
}

complete('specdata',30:25)
complete(c(6,10,20,34,100,200,310))


# FUNCTION 3
corr = function(directory,thresh){
  d_1 = c()
  for (i in 1:332){
    file_path = fs::dir_ls(directory)
    data = read.csv(file_path[i])
    cc = complete.cases(data[,'sulfate'],data[,'nitrate'])
    r = nrow(data[cc,])
    if (r > thresh){
      c_1 = cor(data[cc,'sulfate'],data[cc,'nitrate'])
      d_1 = append(c_1,d_1)
      
    }
    
  }
  return(d_1)
}


corr_val = corr('specdata',150)
head(corr_val)
summary(corr_val)

