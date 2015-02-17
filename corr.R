corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

	
		id<-c(1:332)
		vec_length<-length(id)	
		file_vec<-vector(length=as.integer(vec_length))
		nobs<-vector(length=as.integer(vec_length))
		cor_c<-vector(length=as.integer(vec_length))

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
			if(nobs[i]>threshold){
				cor_c[i]<-cor(data["sulfate"],data["nitrate"])		
			}
			else{
				cor_c[i]<-NA
			}
		}
		good_cor<-complete.cases(cor_c)
		if(length(cor_c[good_cor])>0){
			cor_c<-cor_c[good_cor,drop=FALSE]
			return(cor_c)
		}else{	
			x1<-c()
			class(x1)<-"numeric"
			return(x1)
		}

}

