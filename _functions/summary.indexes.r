# Summary of indexes

# Author : Gwénaël Leday
# Date   : October 2009


summary.indexes <- function(index, boxplots=F){

	# Function to apply
	f <- function(x,y){
		x[1,] <- apply(y,2,function(x){mean(x,na.rm=TRUE)})
		x[2,] <- apply(y,2,function(x){sd(x,na.rm=TRUE)})
		x[3:7,] <- apply(y,2,function(x){quantile(x,na.rm=TRUE)})
		return(x)
	}

	# Initialization
	summary <- matrix(NA,7,ncol(index),
                          dimnames=list(c("Mean", "sd", "Min", "1st Quartile",
                                          "Median", "3rd Quartile", "Max"),
                                        colnames(index)))

	# Calculation and output
	output <- NULL
	output$summary   <- f(summary,  index)

	# Boxplots (Optional)
	if(boxplots){
		output$accuracy  <- list(index[,1])
		output$precision <- list(index[,2])
		output$recall    <- list(index[,3])
		output$fscore    <- list(index[,4])
		output$kappa     <- list(index[,5])
	}
	
	return(output)
}