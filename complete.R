complete <- function(directory, id = 1:332) {
   ## 'directory' is a character vector of length 1 indicating
   ## the location of the CSV files
  
   ## 'id' is an integer vector indicating the monitor ID numbers
   ## to be used
    
   ## Return a data frame of the form:
   ## id nobs
   ## 1 117
   ## 2 1041
   ## ...
   ## where 'id' is the monitor ID number and 'nobs' is the
   ## number of complete cases
  
     vec_length<-length(id)
     file_vec<-vector(length=as.integer(vec_length))
     nobs<-vector(length=as.integer(vec_length))
  
     for(i in 1:vec_length)
     {
         file_vec[i] <- if(id[i]<10){
         paste(directory,"/00",as.character(id[i]),".csv",sep="")
         }else if(id[i]>=10 && id[i]<100){
            paste(directory,"/0",as.character(id[i]),".csv",sep="")
         }else if (id[i]>=100 && id[i] <333){
            paste(directory,"/",as.character(id[i]),".csv",sep="")
         }else{
            "NA"
         }
        
         data<-read.csv(file_vec[i])
         good<-complete.cases(data)
         data<-data[good, ]
         nobs[i]<-nrow(data)
         }
   x<-data.frame(cbind(id,nobs))
   return (x)
   }