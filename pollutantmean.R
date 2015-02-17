pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
	
	vec_length<-length(id)	
	file_vec<-vector(length=as.integer(vec_length))
	len_vec<-vector(length=as.integer(vec_length))
	sum_vec<--vector(length=as.integer(vec_length))

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
		x<-data[,pollutant]
		good<-complete.cases(x)
		x<-x[good]
		len_vec[i]<-length(x)
		sum_vec[i]<-sum(x)
	}
	sum<-sum(sum_vec)
	len<-sum(len_vec)
	mean<-sum/len
	
	return (round(mean,digit=3))
}

