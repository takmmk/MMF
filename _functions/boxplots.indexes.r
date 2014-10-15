# Boxplots of indexes

# Author  :   GwénaE Leday
# Updated :   May 2009

boxplots.indexes <- function(index = NULL,
                             names = NULL,
                             colors= NULL,
                             file=""){

	# Input information
	nb.models <- length(index)
	names.arg <- names(formals())

	# Concatenate measures for boxplot
	acc <- pre <- rec <- fsc <- kap <- NULL
	for(i in 1:nb.models){
		acc <- c(acc, get(names.arg[1])[[i]]$accuracy)
		pre <- c(pre, get(names.arg[1])[[i]]$precision)
		rec <- c(rec, get(names.arg[1])[[i]]$recall)
		fsc <- c(fsc, get(names.arg[1])[[i]]$fscore)
		kap <- c(kap, get(names.arg[1])[[i]]$kappa)
	}

	names(acc) <- names(pre) <- names(rec) <- names(fsc) <- names(kap) <- names

	# Plot the summary of each measure
	par(mar = c(5, 4, 2, 2) + 0.1)
	boxplot(pre, ylim=c(0,1))
	savePlot(paste(file,"boxplot_precision.png",sep=""),type="png")
	#savePlot(paste(file,"boxplot_precision.pdf",sep=""),type="pdf")
	par(mar = c(5, 4, 2, 2) + 0.1)
	boxplot(rec, ylim=c(0,1))
	savePlot(paste(file,"boxplot_recall.png",sep=""),type="png")
	#savePlot(paste(file,"boxplot_recall.pdf",sep=""),type="pdf")
	par(mar = c(5, 4, 2, 2) + 0.1)
	boxplot(fsc, ylim=c(0,1))
	savePlot(paste(file,"boxplot_fscore.png",sep=""),type="png")
	#savePlot(paste(file,"boxplot_fscore.pdf",sep=""),type="pdf")
	par(mar = c(5, 4, 2, 2) + 0.1)
	boxplot(kap, ylim=c(0,1))
	savePlot(paste(file,"boxplot_kappa.png",sep=""),type="png")
	#savePlot(paste(file,"boxplot_kappa.pdf",sep=""),type="pdf")
	par(mar = c(5, 4, 2, 2) + 0.1)
	boxplot(acc, ylim=c(0,1))
	savePlot(paste(file,"boxplot_accuracy.png",sep=""),type="png")
	#savePlot(paste(file,"boxplot_accuracy.pdf",sep=""),type="pdf")
	boxplot(acc, main="Accuracy", ylim=c(0,1))
}
