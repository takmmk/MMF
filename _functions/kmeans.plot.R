# Plotting relevant absence clusters, closest representative absence locations from kmeans clustering

# Author  :   Tak Ikeda
# Updated :   January 2010


kmeans.plot <- function(sp.name, thresh=0, save.fig, file.out, prev=50) {

	# Start to measure process time
	ptm1 <- proc.time()

	# Load data
	setwd(paste("C:\\MMA on HD\\",species.name,"\\_dist_data_OCSVM",sep=""))
	load(paste(sp.name,"_kmeans_undersampling_object_thresh_",thresh,"_prev_",prev,".RData",sep=""))
	species_abs <- read.table(paste(sp.name,"_data_absences_thresh_",thresh,".txt",sep=""),header=T)
	prevalence <- length(data.abs.kmeans$size)
	n_points_cluster <- data.abs.kmeans$size 
	which_cluster <- data.abs.kmeans$cluster
	n_absences_prob0 <- length(data.abs.kmeans$cluster)
	centroid_mat <- data.abs.kmeans$centers 

cat("Data loaded \n")

	# Cluster colors plotted, k=prevalence
	win.graph(width=70,height=40)
	plot(border.no.nz$xcoord, border.no.nz$ycoord, cex=0.2, xlab="Longitude", ylab="Latitude")
	points(species_abs$xcoord,species_abs$ycoord,col=colors()[1+which_cluster],pch=16,cex=0.2)
	savePlot(paste(sp.name,"_cluster_colours_for_relevant_absences_thresh_",thresh,"_prev_",prev,".png",sep=""), type="png")

cat("Clusters plotted \n")

	# Where are the centroids closest to 
	# calculate eucl_dist based on the variables

	min_eucl_dist <- eucl_dist <- NULL; temp <- Inf; which.m <- 1
	out <- matrix(NA,prevalence,2)
	colnames(out) <- list("min_dist","index") # index is the point of species_abs that is closest

cat("Calculating minimum distances... \n")

	for (k in 1:prevalence) {
		species_abs_k <- species_abs[which_cluster==k,]
		for (m in 1:n_points_cluster[k]) {
			eucl_dist <- sqrt(sum((centroid_mat[k,]-species_abs_k[m,-(1:4)])^2))
			min_eucl_dist <- min(eucl_dist,temp)
			if (eucl_dist < temp) which.m <- as.integer(rownames(species_abs_k[m,]))
			temp <- min_eucl_dist
		}
	
		out[k,] <- c(min_eucl_dist,which.m )
cat("k =",k,"\n")
		min_eucl_dist <- eucl_dist <- NULL; temp <- Inf; which.m <- 1
	}

	if (file.out) write.table(out,file=paste(sp.name,"_centroid_closest_data_thresh_",thresh,"_prev_",prev,".txt",sep=""),row.names=F)

cat("Minimum distance done \n")


	# Plot closest data points to centroids on map
	plot(border.no.nz$xcoord, border.no.nz$ycoord, cex=0.2, xlab="Longitude", ylab="Latitude")
	points(species_abs$xcoord[out[,2]],species_abs$ycoord[out[,2]],cex=0.5,pch=16,col=3)
	if (save.fig) savePlot(paste(sp.name,"_closest_points_to_centroids_thresh_",thresh,"_prev_",prev,".png",sep=""), type="png")

	# Plot presences and points closest to representative absences
	plot(border.no.nz$xcoord, border.no.nz$ycoord, cex=0.2, xlab="Longitude", ylab="Latitude")
	points(species$xcoord,species$ycoord,cex=0.5,pch=16,col=2)
	points(species_abs$xcoord[out[,2]],species_abs$ycoord[out[,2]],cex=0.5,pch=16,col=3)
	legend(-150,-40,col=c(2,3),legend=c("Presence","Absence"),pch=16)
	if (save.fig) savePlot(paste(sp.name,"_closest_points_to_centroids_with_pres_thresh_",thresh,"_prev_",prev,".png",sep=""), type="png")

	# calculate total process time
	ptm2 <- proc.time()
	ptm <- (ptm2 - ptm1)[3]

	# Print process time
	cat("\nTime:\n")
		print(ptm)

}
