			###############################################
			#   /-------------------------------------\   #
			#   |     MODELLING SELECTED INVASIVE     |   #
			#   |	       SPECIES DISTRIBUTIONS:	  |   #
			#   |	      a comparative approach	  |   #
			#   \-------------------------------------/   #
			###############################################
 # _____                 _                            _                             
 #|  __ \               | |                     /\   | |                            
 #| |__) |__ _ _ __   __| | ___  _ __ ___      /  \  | |__  ___  ___ _ __   ___ ___ 
 #|  _  // _` | '_ \ / _` |/ _ \| '_ ` _ \    / /\ \ | '_ \/ __|/ _ | '_ \ / __/ _ \
 #| | \ | (_| | | | | (_| | (_) | | | | | |  / ____ \| |_) \__ |  __| | | | (_|  __/
 #|_________,_|_| |_|\__,_|\___/|_| ______| /_/    \_|_.__/|___/\___|_| |_|\___\___|
 #|__   __|                        |_   _|                                          
 #   | |_   _ _ __   ___   ______    | |                                            
 #   | | | | | '_ \ / _ \ |______|   | |                                            
 #   | | |_| | |_) |  __/           _| |_                                           
 #   |_|\__, | .__/ \___|          |_____|                                          
 #       __/ | |                                                                    
 #      |___/|_|                                                                    

# Author: Tak Ikeda, all paths are edited to suit Senait's preference, 
#Changes from the original script this script uses externally provided random absence points, it doesn't run the OCSVM and K-means clustering to get an environmentally
#discriminated abseces.
# Date: 21 Dec 2011

# Use R version 2.8.1
# and packages associated to it

# # # # # #  IMPORTANT  # # # # # # # # 
# ----------------------------------- #
# Save "MMA" folder in C drive! # 
# ----------------------------------- #
# # # # # #  IMPORTANT  # # # # # # # # 



# Name species, universal throughout folder names and output
species.name <- "aa11"  # Aedes albopicta method 1 replicate 1

# Create directories for output
setwd("C:\\MMA")
dir.create(species.name)
setwd(species.name)
dir.create("_condor") 
dir.create("_dist_data")
dir.create("_dist_data_OCSVM")
dir.create("_OCSVM_results")
dir.create("_results_compar.models")
dir.create("_prediction_maps")
setwd("_prediction_maps")
dir.create("_predict_NZ")
dir.create("_predict_world")

# Copy contents of "C:\MMA\_database\_condor" into the "C:\\MMA\\_condor"
# Put presence data into "C:\MMA\species.name\_dist_data" called "species.name_data_pres.txt"

#------------------------------#
#  Load packages and functions #
#------------------------------#

require(sp)
require(kernlab)
require(MASS)
require(class)
require(e1071)
require(randomForest)
require(varSelRF)
require(ROCR)
require(rpart)
require(party)
require(nnet)
require(grDevices)
require(klaR)
names(pdfFonts())

func.dir <- "C:\\MMA\\_functions"

sourceDir <- function(path, trace = TRUE, ...) {
    for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
       if(trace) cat(nm," ")           
       source(file.path(path, nm), ...)
       if(trace) cat("\n")
    }
}
sourceDir(func.dir)


#-----------------#
#  Read in Data  presence data  #
#-----------------#

# Coordinates must be between latitude -180 to 180, longitude -180 to 180
wd <- paste("C:\\MMA\\",species.name,"\\_dist_data",sep="")
setwd(wd)
all_data <- read.table(paste(species.name,"_data_pres.txt",sep=""),header=T)
colnames(all_data) <- c("xcoord","ycoord")


#----------------------#
#  Remove Duplicates   #
#----------------------#

# Remove points within a radius of 0.17 degrees (=10min)
coordinates(all_data) <- c("xcoord", "ycoord")
rd <- remove.duplicates(all_data, zero=0.17, remove.second=T)
rd2 <- rd@coords  # Have again the dataframe # dim(rd2) 

# output no_replicate_data
rd2.coord <- cbind(as.numeric(rd2[,1]),as.numeric(rd2[,2]))
colnames(rd2.coord) <- list("xcoord","ycoord")
write.table(rd2.coord,file=paste(species.name,"_data_no_replicates.txt",sep=""),row.names=F)

#------------------------#
#  Matching of WC data   #
#------------------------#

load("C:\\MMA\\_database\\worldclim.Rdata")
world.orig <- worldclim # make back up of original

f <- function(x) sum(is.na(x))==0
bool2 <- apply(worldclim[,c(59,60)],1,f)
border <- worldclim[!bool2,]
world <- worldclim[,c(2,3)]

load("C:\\MMA\\_database\\worldclim_temp_prec_alt.Rdata")
get.worldclim_mdc_alt(species.name,wd)
#rm(alt,tmin,tmax,prec,bio)
dev.off()

# -------- #### ---------- #
# -------MAIN PART --------#

# ------------------------------ #
# Make borders and NZ-less world #
# ------------------------------ #

# Make border of world
# Remove NZ from training set
bool.nz <- worldclim$xcoord>=160 & worldclim$xcoord<=180 & 
		worldclim$ycoord>=(-55) & worldclim$ycoord<=(-30)
worldclim <- worldclim[!bool.nz,]

# Make border (with no NZ)
f <- function(x) sum(is.na(x))==0
bool3 <- apply(worldclim[,c(59,60)],1,f)
border.no.nz <- worldclim[!bool3,]


# ------------------------ #
# Read in parameter spaces #
# ------------------------ #

#setwd("C:\\MMA\\_database\\_spaces")
#space1 <- read.table("OneSVM_space_parameter_1.txt", header=T)
#space2 <- read.table("OneSVM_space_parameter_2.txt", header=T)
#space3 <- read.table("OneSVM_space_parameter_3.txt", header=T)
#space4 <- read.table("OneSVM_space_parameter_4.txt", header=T)
#space5 <- read.table("OneSVM_space_parameter_5.txt", header=T)
#space6 <- read.table("OneSVM_space_parameter_6.txt", header=T)
#space7 <- read.table("OneSVM_space_parameter_7.txt", header=T)
#space8 <- read.table("OneSVM_space_parameter_8.txt", header=T)
#space9 <- read.table("OneSVM_space_parameter_9.txt", header=T)
#space10 <- read.table("OneSVM_space_parameter_10.txt", header=T)


# ------------------- #
# Choice of variables #
# ------------------- #

# selection from bioclim:
chosen.v <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,22)+39 #Using all the variables
# chosen.v <- c(1,3,5,6,9,14,15,19)+39 # Try this if allocation error occurs
# Chosen: "cbio01" "cbio03" "cbio05" "cbio06" "cbio09" "cbio14" "cbio15" "cbio19"
# no mean diurnal range (cbio02) because global
# no mean temp warm/cold quarter (cbio10,11) because max/min of warm/cold month
# no prec wet/dry quarter (cbio16,17) because prec wet/dry month
# no min dist

# remove locations with NA as value
bool <- apply(worldclim[,chosen.v],1,f) 
worldclim2 <- worldclim[bool,chosen.v]  
x <- worldclim[bool,c(2,3)] # all coordinates


# ------ #
# OCSVM #
# ------ #

#setwd(paste("C:\\MMA\\",species.name,"\\_dist_data",sep=""))
#species <- read.table(paste(species.name,"_worldclim.txt",sep=""), header=T)

# output OCSVM files 
#setwd(paste("C:\\MMA\\",species.name,"\\_OCSVM_results",sep=""))

#memory.limit(3000)
#cv.k <- 10
#if (nrow(species)<100 & nrow(species)>=10) cv.k <- 5
#if (nrow(species)<10) {
#	cv.k <- NA
#	write.table("dataset less than 10 rows","error.txt",row.names=F,col.names=F)
#}

#test <- OneSVM.param.cross.clim(data1.x=species[,chosen.v],
#						data0.x=worldclim2, 
#						nb.models=100, k=cv.k, 
#						prior=rbind(space1,space2,space3,space4,space5,
#						space6,space7,space8,space9,space10),
#						file=T, save.out=T)
#
#
# ------------------------------ #
# Read in and join OCSVM results 
# ------------------------------ #

#setwd(paste("C:\\MMA\\",species.name,"\\_OCSVM_results",sep=""))
#load("one_class_svm_out.Rdata")
#OneSVM.param.cross.clim.read(out$models.in, path=NULL, file=T)

#---------------- #
# OCSVMk function #
#---------------- #

#ocsvmk(species.name,color.choice="venette",save.fig=T,file.out=T,do.kmeans=T, thresh= 0,preva=50)

# ---------------------------------------------------------- #
# Plot representative/relevant absences from kmeans clusters #
#----------------------------------------------------------- #

#kmeans.plot(species.name, thresh= 0, save.fig=T, file.out=T,preva=50)
#------------------------------------#
# Insert external random points      |
#------------------------------------#
#
setwd('C:\\MMA\\db\\PAG')
#1 add a column pres on both the presence and random absence points, for presence use species.name_worldclim.txt file in _dist_data folder and absenc the external random points
#2 merge external random absence points with presence points, use the col names from the presence points
#3 delete all columns that are not availavle on the file outputed after Chosen.v function was run (line184 abv)
#4 save the fils under C:\\MMA\\db\\PAG\\, with a file name species.name_data_pres_abs_threshold_0_prev_50.csv
#5. run the following 3 lines, the table needed by the varSel.species function below should run smoothly after this.
transaa11= read.csv('C:\\MMA\\db\\PAG\\aa11_data_pres_abs_threshold_0_prev_50.csv', header =T)
setwd(paste("C:\\MMA\\",species.name,"\\_dist_data_OCSVM",sep=''))
write.table(transaa11,file=paste(species.name,"_data_pres_abs_threshold_0_prev_50.txt",sep=""),sep = " ",row.names=F)

#------------------------------------#
# Variable Selection and output data |
#------------------------------------#
# Random Forest
setwd(paste("C:\\MMA\\",species.name,"\\_dist_data_OCSVM",sep=""))
vs <- varSel.species(species.name,thresh=0, preva=50) 

write.table(vs$rf_initial, paste(species.name,"_varSel_rf_initial_thresh0_prev_50.txt",sep=""),row.names=F); write.table(vs$step_initial, paste(species.name,"_varSel_step_initial_thresh0_prev_50.txt",sep=""),row.names=F); c(length(vs$rf_initial), length(vs$step_initial))

## Which Variables were selected?
vs_best <- read.table(paste(species.name,"_varSel_rf_initial_thresh0_prev_50.txt",sep=""),header=T)
dat <- read.table(paste(species.name,"_data_pres_abs_threshold_0_prev_50.txt",sep=""),header=T)
dat_best <- dat[,c(as.character(c(vs_best)$x),"pres")]
write.table(dat_best, paste(species.name,"_data_pres_abs_best.txt",sep=""),row.names=F)

# ------ #
# Condor #
# ------ #

#setwd(paste("C:\\MMA\\",species.name,"\\_dist_data_OCSVM",sep=""))
#bootstrap.condor(species.name,200)
#setwd(paste("C:\\MMA\\",species.name,"\\_condor",sep=""))
#write.table(dat_best,"data.txt",row.names=F)

# Prepare directory for condor
# cd /d "C:\MMA on HD\my data\_condor"
# python pack.py data.txt boot
# python pack.py nnet.r
# condor_submit nnet.sub

# -------------------------------------- #
# Undersample for training
# -------------------------------------- #
setwd('C:\\MMA\\db\\PAG')
undersamp = read.csv('C:\\MMA\\db\\PAG\\aa1_data_pres_abs_best_1.csv', header =T)
setwd(paste("C:\\MMA\\",species.name,"\\_dist_data_OCSVM",sep=''))
write.table(undersamp,file=paste(species.name,"_data_pres_abs_best_1.txt",sep=""),sep = " ",row.names=F)


# ------------ #
# Import data
# ------------ #

	setwd(paste("C:\\MMA\\",species.name,"\\_dist_data_OCSVM",sep=""))
	data <- read.table(paste(species.name,"_data_pres_abs_best_1.txt",sep=""), header=TRUE)
	data <- as.data.frame(data[sample(nrow(data)),])
	data.x <- scale(data[,1:(ncol(data)-1)], center=TRUE, scale=TRUE)
	data.x <- as.data.frame(data.x)
	data.y <- data[,ncol(data)]
	# Prevalence
	prevalence <- sum(data.y)

#-------------------#
#  PARAMETRIZATION  |
#-------------------#

	# Set output directory
	setwd(paste("C:\\MMA\\",species.name,"\\_results_compar.models",sep=""))
	cv.k <- 10
	if (prevalence < 50) cv.k <- 5
	knrep <- 200/cv.k


param.knn(x=data.x, y=data.y, knn.rep=100, resampling="boot", nrep=200)
cat("done knn boot \n")
param.knn(x=data.x, y=data.y, knn.rep=knrep, resampling="cv", kfold=cv.k, nrep=knrep)
cat("done knn cv \n")

# Parametrization of the Support Vector Machine by bootstrapping/CV
param.svm(x=data.x, y=data.y, kernL="rbfdot", resampling="boot", nrep = 200, thresh=0)
cat("done param.svm, resample = boot, threshold =",0, "\n")
param.svm(x=data.x, y=data.y, kernL="rbfdot", resampling="cv", kfold=cv.k, nrep=knrep, thresh=0)
cat("done param.svm, resample = cv, threshold =",0, "\n")

# Parametrization of the Neural NETworks by cv
param.nnet(x=data.x , y=data.y , nnet.rep =knrep, resampling = "cv", nrep=knrep)
cat("done nnet cv \n")

# ----------------------------------- #
# Reading in nnet results from condor #
# ----------------------------------- #

#####Work Directory and read in data
#setwd(paste("C:\\MMA\\",species.name,"\\_condor",sep=""))
#dat <- read.table("data.txt",header=T)

##### Parameters of NNET
#s <- 2:5
#m <- c(500,750,1000,1250,1500)
#d <- c(0.01,0.001,0.0001)

#setwd(paste("C:\\MMA\\",species.name,"\\_condor\\nnet",sep=""))

#results <- param.nnet.results(getwd(), data=dat)


#----------------------#
#  Parameter settings  |
#----------------------#

# boot:
b.k1 <- 4
b.s1 <- 10
b.s2 <- 0.5
b.n1 <- 5
b.n2 <- 500
b.n3 <- 0.01

# cv:
c.k1 <- 5
c.s1 <- 100
c.s2 <- 0.9
c.n1 <- 5
c.n2 <- 500
c.n3 <- 0.01

# ------------------------------------------------------------ #
# Import data- again to include the full set of pres-abs data
# ------------------------------------------------------------ #

	setwd(paste("C:\\MMA\\",species.name,"\\_dist_data_OCSVM",sep=""))
	data <- read.table(paste(species.name,"_data_pres_abs_best.txt",sep=""), header=TRUE)
	data <- as.data.frame(data[sample(nrow(data)),])
	data.x <- scale(data[,1:(ncol(data)-1)], center=TRUE, scale=TRUE)
	data.x <- as.data.frame(data.x)
	data.y <- data[,ncol(data)]
	# Prevalence
	prevalence <- sum(data.y)

#-------------------------#
#  Fix Weights for nnet   |
#-------------------------#

# given data.x, data.y, nnet.par
# Set output directory

setwd(paste("C:\\MMA\\",species.name,"\\_results_compar.models",sep=""))

nnet.Wts.boot <- best.nnet.weights(species.name,nreps=1000,
					data.x,data.y,
					nnet.par=c(b.n1,b.n2,b.n3),
					save.out=T)

nnet.Wts.cv <- best.nnet.weights(species.name,nreps=1000,
					data.x,data.y,
					nnet.par=c(c.n1,c.n2,c.n3),
					save.out=T)

#--------------#
#  COMPARISON  |
#--------------#

## Comparison of the performance of supervised models using different measures

setwd(paste("C:\\MMA\\",species.name,"\\_results_compar.models",sep=""))

# BOOTSTRAP
result.compar.b <- compar.models(x=data.x,
                               y=data.y, 
					 methods=c("LDA","QDA","LOG","NB","CART","CTREE","KNN","SVM","NNET"),
                               knn.par = b.k1, 
                               svm.par = c("rbfdot",b.s1,b.s2),                                
                               nnet.par = c(b.n1,b.n2,b.n3),
                               Wts = nnet.Wts.boot,
                               resampling="boot",
                               nrep=200,
                               train.frac=0.75,
                               k=10,
                               file = T, 
                               plots=T, 
                               plots.all=T, 
                               boxplots=T, 
                               bagging=NULL,
                               boosting=NULL)

# CROSS-VALIDATION
result.compar.cv <- compar.models(x=data.x,
                               y=data.y, 
					 methods=c("LDA","QDA","LOG","NB","CART","CTREE","KNN","SVM","NNET"),
                               knn.par = c.k1, 
                               svm.par = c("rbfdot",c.s1,c.s2),                                
                               nnet.par = c(c.n1,c.n2,c.n3),
                               Wts = nnet.Wts.cv,
                               resampling="cv",
                               nrep=5,
                               train.frac=0.75,
                               k=cv.k,
                               file = T, 
                               plots=T, 
                               plots.all=T, 
                               boxplots=T, 
                               bagging=NULL,
                               boosting=NULL)




# --------------------------------------------------- #
# Script for best model selection based on multiple criteria #
# --------------------------------------------------- #
source("C:\\MMA\\_functions\\best.model.criteria.r")

# --------------------------------------------------- #
# Best models & ranked criteria tables saved into files
# --------------------------------------------------- #

best.model.criteria(species.name,"boot")
best.model.criteria(species.name,"cv")


#####################
### PREDICT World ###
#####################
 
setwd(paste("C:\\MMA\\",species.name,"\\_dist_data_OCSVM",sep=""))

#------#
# DATA |
#------#

# Import data again
f <- function(x) sum(is.na(x))==0
bool <- apply(world.orig[,colnames(data)[1:(ncol(data)-1)]],1,f)
worldclim2 <- world.orig[bool,colnames(data)[1:(ncol(data)-1)]]
x <- world.orig[bool,c(2,3)]

f <- function(x) sum(is.na(x))==0
bool2 <- apply(world.orig[,c(59,60)],1,f)
border <- world.orig[!bool2,]
x <- world.orig[bool,c(2,3)]


#-------- Train and test with best model -------#

#----------------------#
#  Parameter settings  |
#----------------------#


# boot:
b.k1 <- 4
b.s1 <- 10
b.s2 <- 0.5
b.n1 <- 5
b.n2 <- 500
b.n3 <- 0.01

# cv:
c.k1 <- 5
c.s1 <- 100
c.s2 <- 0.9
c.n1 <- 5
c.n2 <- 500
c.n3 <- 0.01



setwd(paste("C:\\MMA\\",species.name,"\\_results_compar.models",sep=""))
nnet.Wts.boot <- read.table(paste(species.name,"_best_weights_",b.n1,"_",b.n2,"_",b.n3,".txt",sep=""),header=T)
nnet.Wts.cv <- nnet.Wts.boot
nnet.Wts.boot <- c(nnet.Wts.boot$x)
nnet.Wts.cv <- c(nnet.Wts.cv$x)
# --------------------------------------------------------------------- #
# Plotting and saving output according to name of method and resampling
# --------------------------------------------------------------------- #
###################################################################LDA-WORLD#########################################################################################


method.name <- "LDA" # Capital letters!
resampling <- "boot"


if (method.name == "KNN") {
	if (resampling == "boot") p1 <- b.k1  
	if (resampling == "cv")   p1 <- c.k1 
	p2 <- 0; p3 <- 0; p.lab <- paste(p1,sep="_")
}
if (method.name == "SVM") {
	if (resampling == "boot") {p1 <- b.s1; p2 <- b.s2}
	if (resampling == "cv")   {p1 <- c.s1; p2 <- c.s2}
	p3 <- 0; p.lab <- paste(p1,p2,sep="_")
}
if (method.name == "NNET") {
	if (resampling == "boot") {p1 <- b.n1; p2 <- b.n2; p3 <- b.n3; nnet.Wts <- nnet.Wts.boot}
	if (resampling == "cv")   {p1 <- c.n1; p2 <- c.n2; p3 <- c.n3; nnet.Wts <- nnet.Wts.cv}
	p.lab <- paste(p1,p2,p3,sep="_")
}
if (method.name != "KNN" & method.name != "SVM" & method.name != "NNET") {
	p1 <- p2 <- p3 <- 0
	p.lab <- ""
}


train.and.test.model.predict <- train.and.test.model(train=data,
                                 method=method.name,
                                 test=worldclim2,
                                 test2=NULL,
                                 test3=NULL,
                                 resp.var.name="pres", 
                                 knn.par = p1, 
                                 svm.par = c("rbfdot",p1,p2), 
                                 nnet.par = c(p1,p2,p3),
                                 Wts = nnet.Wts,
                                 nbtree=10000,
                                 priors=NULL,
                                 prob=T)

setwd(paste("C:\\MMA\\",species.name,"\\_prediction_maps\\_predict_world",sep=""))
save(train.and.test.model.predict,file=paste("World_predictions_",species.name,"_",method.name,"_",p.lab,".Rdata",sep=""))

# Results
res <- cbind(train.and.test.model.predict$pred.test,train.and.test.model.predict$prob.test)
colnames(res) <- c("Prediction","Probability")

plot.split(species.name,method.name,save.fig=F)
savePlot(paste("World_prediction_",species.name,"_",method.name,"_",p.lab,".png",sep=""),type="png")

# ------------------------ #
# Make database for ArcGIS
# ------------------------ #

# Save things greater than threshold
thresh <- 0.1
dat.prob <- cbind(x[res[,2]>thresh,1],x[res[,2]>thresh,2],res[res[,2]>thresh,2])
colnames(dat.prob) <- c("xcoord", "ycoord","Prob")
write.table(dat.prob, paste("World_",species.name,"_",method.name,"_",p.lab,"_threshold_",thresh,"_data_prob.txt",sep=""), row.names=F)

dat.vote <- cbind(x[res[,1]>0,1],x[res[,1]>0,2],res[res[,1]>0,1])
colnames(dat.vote) <- c("xcoord", "ycoord","Vote")
write.table(dat.vote, paste("World_",species.name,"_",method.name,"_",p.lab,"_data_vote1.txt",sep=""), row.names=F)

# Repeat this part as many times as you wish, by changing the method.name (capital letters), 
# resampling method and/or parameters. Each time the output will be saved with the appropriate name.

# --------------------------------------------------------------------- #
# --------------------------------------------------------------------- #

###################################################################QDA-WORLD#########################################################################################


method.name <- "QDA" # Capital letters!
resampling <- "boot"


if (method.name == "KNN") {
	if (resampling == "boot") p1 <- b.k1  
	if (resampling == "cv")   p1 <- c.k1 
	p2 <- 0; p3 <- 0; p.lab <- paste(p1,sep="_")
}
if (method.name == "SVM") {
	if (resampling == "boot") {p1 <- b.s1; p2 <- b.s2}
	if (resampling == "cv")   {p1 <- c.s1; p2 <- c.s2}
	p3 <- 0; p.lab <- paste(p1,p2,sep="_")
}
if (method.name == "NNET") {
	if (resampling == "boot") {p1 <- b.n1; p2 <- b.n2; p3 <- b.n3; nnet.Wts <- nnet.Wts.boot}
	if (resampling == "cv")   {p1 <- c.n1; p2 <- c.n2; p3 <- c.n3; nnet.Wts <- nnet.Wts.cv}
	p.lab <- paste(p1,p2,p3,sep="_")
}
if (method.name != "KNN" & method.name != "SVM" & method.name != "NNET") {
	p1 <- p2 <- p3 <- 0
	p.lab <- ""
}


train.and.test.model.predict <- train.and.test.model(train=data,
                                 method=method.name,
                                 test=worldclim2,
                                 test2=NULL,
                                 test3=NULL,
                                 resp.var.name="pres", 
                                 knn.par = p1, 
                                 svm.par = c("rbfdot",p1,p2), 
                                 nnet.par = c(p1,p2,p3),
                                 Wts = nnet.Wts,
                                 nbtree=10000,
                                 priors=NULL,
                                 prob=T)

setwd(paste("C:\\MMA\\",species.name,"\\_prediction_maps\\_predict_world",sep=""))
save(train.and.test.model.predict,file=paste("World_predictions_",species.name,"_",method.name,"_",p.lab,".Rdata",sep=""))

# Results
res <- cbind(train.and.test.model.predict$pred.test,train.and.test.model.predict$prob.test)
colnames(res) <- c("Prediction","Probability")

plot.split(species.name,method.name,save.fig=F)
savePlot(paste("World_prediction_",species.name,"_",method.name,"_",p.lab,".png",sep=""),type="png")

# ------------------------ #
# Make database for ArcGIS
# ------------------------ #

# Save things greater than threshold
thresh <- 0.1
dat.prob <- cbind(x[res[,2]>thresh,1],x[res[,2]>thresh,2],res[res[,2]>thresh,2])
colnames(dat.prob) <- c("xcoord", "ycoord","Prob")
write.table(dat.prob, paste("World_",species.name,"_",method.name,"_",p.lab,"_threshold_",thresh,"_data_prob.txt",sep=""), row.names=F)

dat.vote <- cbind(x[res[,1]>0,1],x[res[,1]>0,2],res[res[,1]>0,1])
colnames(dat.vote) <- c("xcoord", "ycoord","Vote")
write.table(dat.vote, paste("World_",species.name,"_",method.name,"_",p.lab,"_data_vote1.txt",sep=""), row.names=F)

# Repeat this part as many times as you wish, by changing the method.name (capital letters), 
# resampling method and/or parameters. Each time the output will be saved with the appropriate name.

# --------------------------------------------------------------------- #
# --------------------------------------------------------------------- #

###################################################################LOG-WORLD#########################################################################################


method.name <- "LOG" # Capital letters!
resampling <- "boot"


if (method.name == "KNN") {
	if (resampling == "boot") p1 <- b.k1  
	if (resampling == "cv")   p1 <- c.k1 
	p2 <- 0; p3 <- 0; p.lab <- paste(p1,sep="_")
}
if (method.name == "SVM") {
	if (resampling == "boot") {p1 <- b.s1; p2 <- b.s2}
	if (resampling == "cv")   {p1 <- c.s1; p2 <- c.s2}
	p3 <- 0; p.lab <- paste(p1,p2,sep="_")
}
if (method.name == "NNET") {
	if (resampling == "boot") {p1 <- b.n1; p2 <- b.n2; p3 <- b.n3; nnet.Wts <- nnet.Wts.boot}
	if (resampling == "cv")   {p1 <- c.n1; p2 <- c.n2; p3 <- c.n3; nnet.Wts <- nnet.Wts.cv}
	p.lab <- paste(p1,p2,p3,sep="_")
}
if (method.name != "KNN" & method.name != "SVM" & method.name != "NNET") {
	p1 <- p2 <- p3 <- 0
	p.lab <- ""
}


train.and.test.model.predict <- train.and.test.model(train=data,
                                 method=method.name,
                                 test=worldclim2,
                                 test2=NULL,
                                 test3=NULL,
                                 resp.var.name="pres", 
                                 knn.par = p1, 
                                 svm.par = c("rbfdot",p1,p2), 
                                 nnet.par = c(p1,p2,p3),
                                 Wts = nnet.Wts,
                                 nbtree=10000,
                                 priors=NULL,
                                 prob=T)

setwd(paste("C:\\MMA\\",species.name,"\\_prediction_maps\\_predict_world",sep=""))
save(train.and.test.model.predict,file=paste("World_predictions_",species.name,"_",method.name,"_",p.lab,".Rdata",sep=""))

# Results
res <- cbind(train.and.test.model.predict$pred.test,train.and.test.model.predict$prob.test)
colnames(res) <- c("Prediction","Probability")

plot.split(species.name,method.name,save.fig=F)
savePlot(paste("World_prediction_",species.name,"_",method.name,"_",p.lab,".png",sep=""),type="png")

# ------------------------ #
# Make database for ArcGIS
# ------------------------ #

# Save things greater than threshold
thresh <- 0.1
dat.prob <- cbind(x[res[,2]>thresh,1],x[res[,2]>thresh,2],res[res[,2]>thresh,2])
colnames(dat.prob) <- c("xcoord", "ycoord","Prob")
write.table(dat.prob, paste("World_",species.name,"_",method.name,"_",p.lab,"_threshold_",thresh,"_data_prob.txt",sep=""), row.names=F)

dat.vote <- cbind(x[res[,1]>0,1],x[res[,1]>0,2],res[res[,1]>0,1])
colnames(dat.vote) <- c("xcoord", "ycoord","Vote")
write.table(dat.vote, paste("World_",species.name,"_",method.name,"_",p.lab,"_data_vote1.txt",sep=""), row.names=F)

# Repeat this part as many times as you wish, by changing the method.name (capital letters), 
# resampling method and/or parameters. Each time the output will be saved with the appropriate name.

# --------------------------------------------------------------------- #
# --------------------------------------------------------------------- #

###################################################################NB-WORLD#########################################################################################


method.name <- "NB" # Capital letters!
resampling <- "boot"


if (method.name == "KNN") {
	if (resampling == "boot") p1 <- b.k1  
	if (resampling == "cv")   p1 <- c.k1 
	p2 <- 0; p3 <- 0; p.lab <- paste(p1,sep="_")
}
if (method.name == "SVM") {
	if (resampling == "boot") {p1 <- b.s1; p2 <- b.s2}
	if (resampling == "cv")   {p1 <- c.s1; p2 <- c.s2}
	p3 <- 0; p.lab <- paste(p1,p2,sep="_")
}
if (method.name == "NNET") {
	if (resampling == "boot") {p1 <- b.n1; p2 <- b.n2; p3 <- b.n3; nnet.Wts <- nnet.Wts.boot}
	if (resampling == "cv")   {p1 <- c.n1; p2 <- c.n2; p3 <- c.n3; nnet.Wts <- nnet.Wts.cv}
	p.lab <- paste(p1,p2,p3,sep="_")
}
if (method.name != "KNN" & method.name != "SVM" & method.name != "NNET") {
	p1 <- p2 <- p3 <- 0
	p.lab <- ""
}


train.and.test.model.predict <- train.and.test.model(train=data,
                                 method=method.name,
                                 test=worldclim2,
                                 test2=NULL,
                                 test3=NULL,
                                 resp.var.name="pres", 
                                 knn.par = p1, 
                                 svm.par = c("rbfdot",p1,p2), 
                                 nnet.par = c(p1,p2,p3),
                                 Wts = nnet.Wts,
                                 nbtree=10000,
                                 priors=NULL,
                                 prob=T)

setwd(paste("C:\\MMA\\",species.name,"\\_prediction_maps\\_predict_world",sep=""))
save(train.and.test.model.predict,file=paste("World_predictions_",species.name,"_",method.name,"_",p.lab,".Rdata",sep=""))

# Results
res <- cbind(train.and.test.model.predict$pred.test,train.and.test.model.predict$prob.test)
colnames(res) <- c("Prediction","Probability")

plot.split(species.name,method.name,save.fig=F)
savePlot(paste("World_prediction_",species.name,"_",method.name,"_",p.lab,".png",sep=""),type="png")

# ------------------------ #
# Make database for ArcGIS
# ------------------------ #

# Save things greater than threshold
thresh <- 0.1
dat.prob <- cbind(x[res[,2]>thresh,1],x[res[,2]>thresh,2],res[res[,2]>thresh,2])
colnames(dat.prob) <- c("xcoord", "ycoord","Prob")
write.table(dat.prob, paste("World_",species.name,"_",method.name,"_",p.lab,"_threshold_",thresh,"_data_prob.txt",sep=""), row.names=F)

dat.vote <- cbind(x[res[,1]>0,1],x[res[,1]>0,2],res[res[,1]>0,1])
colnames(dat.vote) <- c("xcoord", "ycoord","Vote")
write.table(dat.vote, paste("World_",species.name,"_",method.name,"_",p.lab,"_data_vote1.txt",sep=""), row.names=F)

# Repeat this part as many times as you wish, by changing the method.name (capital letters), 
# resampling method and/or parameters. Each time the output will be saved with the appropriate name.

# --------------------------------------------------------------------- #
# --------------------------------------------------------------------- #

###################################################################CART-WORLD#########################################################################################

method.name <- "CART" # Capital letters!
resampling <- "boot"


if (method.name == "KNN") {
	if (resampling == "boot") p1 <- b.k1  
	if (resampling == "cv")   p1 <- c.k1 
	p2 <- 0; p3 <- 0; p.lab <- paste(p1,sep="_")
}
if (method.name == "SVM") {
	if (resampling == "boot") {p1 <- b.s1; p2 <- b.s2}
	if (resampling == "cv")   {p1 <- c.s1; p2 <- c.s2}
	p3 <- 0; p.lab <- paste(p1,p2,sep="_")
}
if (method.name == "NNET") {
	if (resampling == "boot") {p1 <- b.n1; p2 <- b.n2; p3 <- b.n3; nnet.Wts <- nnet.Wts.boot}
	if (resampling == "cv")   {p1 <- c.n1; p2 <- c.n2; p3 <- c.n3; nnet.Wts <- nnet.Wts.cv}
	p.lab <- paste(p1,p2,p3,sep="_")
}
if (method.name != "KNN" & method.name != "SVM" & method.name != "NNET") {
	p1 <- p2 <- p3 <- 0
	p.lab <- ""
}


train.and.test.model.predict <- train.and.test.model(train=data,
                                 method=method.name,
                                 test=worldclim2,
                                 test2=NULL,
                                 test3=NULL,
                                 resp.var.name="pres", 
                                 knn.par = p1, 
                                 svm.par = c("rbfdot",p1,p2), 
                                 nnet.par = c(p1,p2,p3),
                                 Wts = nnet.Wts,
                                 nbtree=10000,
                                 priors=NULL,
                                 prob=T)

setwd(paste("C:\\MMA\\",species.name,"\\_prediction_maps\\_predict_world",sep=""))
save(train.and.test.model.predict,file=paste("World_predictions_",species.name,"_",method.name,"_",p.lab,".Rdata",sep=""))

# Results
res <- cbind(train.and.test.model.predict$pred.test,train.and.test.model.predict$prob.test)
colnames(res) <- c("Prediction","Probability")

plot.split(species.name,method.name,save.fig=F)
savePlot(paste("World_prediction_",species.name,"_",method.name,"_",p.lab,".png",sep=""),type="png")

# ------------------------ #
# Make database for ArcGIS
# ------------------------ #

# Save things greater than threshold
thresh <- 0.1
dat.prob <- cbind(x[res[,2]>thresh,1],x[res[,2]>thresh,2],res[res[,2]>thresh,2])
colnames(dat.prob) <- c("xcoord", "ycoord","Prob")
write.table(dat.prob, paste("World_",species.name,"_",method.name,"_",p.lab,"_threshold_",thresh,"_data_prob.txt",sep=""), row.names=F)

dat.vote <- cbind(x[res[,1]>0,1],x[res[,1]>0,2],res[res[,1]>0,1])
colnames(dat.vote) <- c("xcoord", "ycoord","Vote")
write.table(dat.vote, paste("World_",species.name,"_",method.name,"_",p.lab,"_data_vote1.txt",sep=""), row.names=F)

# Repeat this part as many times as you wish, by changing the method.name (capital letters), 
# resampling method and/or parameters. Each time the output will be saved with the appropriate name.

# --------------------------------------------------------------------- #
# --------------------------------------------------------------------- #

###################################################################CTREE-WORLD#########################################################################################


method.name <- "CTREE" # Capital letters!
resampling <- "boot"


if (method.name == "KNN") {
	if (resampling == "boot") p1 <- b.k1  
	if (resampling == "cv")   p1 <- c.k1 
	p2 <- 0; p3 <- 0; p.lab <- paste(p1,sep="_")
}
if (method.name == "SVM") {
	if (resampling == "boot") {p1 <- b.s1; p2 <- b.s2}
	if (resampling == "cv")   {p1 <- c.s1; p2 <- c.s2}
	p3 <- 0; p.lab <- paste(p1,p2,sep="_")
}
if (method.name == "NNET") {
	if (resampling == "boot") {p1 <- b.n1; p2 <- b.n2; p3 <- b.n3; nnet.Wts <- nnet.Wts.boot}
	if (resampling == "cv")   {p1 <- c.n1; p2 <- c.n2; p3 <- c.n3; nnet.Wts <- nnet.Wts.cv}
	p.lab <- paste(p1,p2,p3,sep="_")
}
if (method.name != "KNN" & method.name != "SVM" & method.name != "NNET") {
	p1 <- p2 <- p3 <- 0
	p.lab <- ""
}


train.and.test.model.predict <- train.and.test.model(train=data,
                                 method=method.name,
                                 test=worldclim2,
                                 test2=NULL,
                                 test3=NULL,
                                 resp.var.name="pres", 
                                 knn.par = p1, 
                                 svm.par = c("rbfdot",p1,p2), 
                                 nnet.par = c(p1,p2,p3),
                                 Wts = nnet.Wts,
                                 nbtree=10000,
                                 priors=NULL,
                                 prob=T)

setwd(paste("C:\\MMA\\",species.name,"\\_prediction_maps\\_predict_world",sep=""))
save(train.and.test.model.predict,file=paste("World_predictions_",species.name,"_",method.name,"_",p.lab,".Rdata",sep=""))

# Results
res <- cbind(train.and.test.model.predict$pred.test,train.and.test.model.predict$prob.test)
colnames(res) <- c("Prediction","Probability")

plot.split(species.name,method.name,save.fig=F)
savePlot(paste("World_prediction_",species.name,"_",method.name,"_",p.lab,".png",sep=""),type="png")

# ------------------------ #
# Make database for ArcGIS
# ------------------------ #

# Save things greater than threshold
thresh <- 0.1
dat.prob <- cbind(x[res[,2]>thresh,1],x[res[,2]>thresh,2],res[res[,2]>thresh,2])
colnames(dat.prob) <- c("xcoord", "ycoord","Prob")
write.table(dat.prob, paste("World_",species.name,"_",method.name,"_",p.lab,"_threshold_",thresh,"_data_prob.txt",sep=""), row.names=F)

dat.vote <- cbind(x[res[,1]>0,1],x[res[,1]>0,2],res[res[,1]>0,1])
colnames(dat.vote) <- c("xcoord", "ycoord","Vote")
write.table(dat.vote, paste("World_",species.name,"_",method.name,"_",p.lab,"_data_vote1.txt",sep=""), row.names=F)

# Repeat this part as many times as you wish, by changing the method.name (capital letters), 
# resampling method and/or parameters. Each time the output will be saved with the appropriate name.

# --------------------------------------------------------------------- #
# --------------------------------------------------------------------- #

###################################################################KNN-WORLD#########################################################################################

method.name <- "KNN" # Capital letters!
resampling <- "boot"


if (method.name == "KNN") {
	if (resampling == "boot") p1 <- b.k1  
	if (resampling == "cv")   p1 <- c.k1 
	p2 <- 0; p3 <- 0; p.lab <- paste(p1,sep="_")
}
if (method.name == "SVM") {
	if (resampling == "boot") {p1 <- b.s1; p2 <- b.s2}
	if (resampling == "cv")   {p1 <- c.s1; p2 <- c.s2}
	p3 <- 0; p.lab <- paste(p1,p2,sep="_")
}
if (method.name == "NNET") {
	if (resampling == "boot") {p1 <- b.n1; p2 <- b.n2; p3 <- b.n3; nnet.Wts <- nnet.Wts.boot}
	if (resampling == "cv")   {p1 <- c.n1; p2 <- c.n2; p3 <- c.n3; nnet.Wts <- nnet.Wts.cv}
	p.lab <- paste(p1,p2,p3,sep="_")
}
if (method.name != "KNN" & method.name != "SVM" & method.name != "NNET") {
	p1 <- p2 <- p3 <- 0
	p.lab <- ""
}


train.and.test.model.predict <- train.and.test.model(train=data,
                                 method=method.name,
                                 test=worldclim2,
                                 test2=NULL,
                                 test3=NULL,
                                 resp.var.name="pres", 
                                 knn.par = p1, 
                                 svm.par = c("rbfdot",p1,p2), 
                                 nnet.par = c(p1,p2,p3),
                                 Wts = nnet.Wts,
                                 nbtree=10000,
                                 priors=NULL,
                                 prob=T)

setwd(paste("C:\\MMA\\",species.name,"\\_prediction_maps\\_predict_world",sep=""))
save(train.and.test.model.predict,file=paste("World_predictions_",species.name,"_",method.name,"_",p.lab,".Rdata",sep=""))

# Results
res <- cbind(train.and.test.model.predict$pred.test,train.and.test.model.predict$prob.test)
colnames(res) <- c("Prediction","Probability")

plot.split(species.name,method.name,save.fig=F)
savePlot(paste("World_prediction_",species.name,"_",method.name,"_",p.lab,".png",sep=""),type="png")

# ------------------------ #
# Make database for ArcGIS
# ------------------------ #

# Save things greater than threshold
thresh <- 0.1
dat.prob <- cbind(x[res[,2]>thresh,1],x[res[,2]>thresh,2],res[res[,2]>thresh,2])
colnames(dat.prob) <- c("xcoord", "ycoord","Prob")
write.table(dat.prob, paste("World_",species.name,"_",method.name,"_",p.lab,"_threshold_",thresh,"_data_prob.txt",sep=""), row.names=F)

dat.vote <- cbind(x[res[,1]>0,1],x[res[,1]>0,2],res[res[,1]>0,1])
colnames(dat.vote) <- c("xcoord", "ycoord","Vote")
write.table(dat.vote, paste("World_",species.name,"_",method.name,"_",p.lab,"_data_vote1.txt",sep=""), row.names=F)

# Repeat this part as many times as you wish, by changing the method.name (capital letters), 
# resampling method and/or parameters. Each time the output will be saved with the appropriate name.

# --------------------------------------------------------------------- #
# --------------------------------------------------------------------- #

###################################################################SVM-WORLD#########################################################################################

method.name <- "SVM" # Capital letters!
resampling <- "boot"


if (method.name == "KNN") {
	if (resampling == "boot") p1 <- b.k1  
	if (resampling == "cv")   p1 <- c.k1 
	p2 <- 0; p3 <- 0; p.lab <- paste(p1,sep="_")
}
if (method.name == "SVM") {
	if (resampling == "boot") {p1 <- b.s1; p2 <- b.s2}
	if (resampling == "cv")   {p1 <- c.s1; p2 <- c.s2}
	p3 <- 0; p.lab <- paste(p1,p2,sep="_")
}
if (method.name == "NNET") {
	if (resampling == "boot") {p1 <- b.n1; p2 <- b.n2; p3 <- b.n3; nnet.Wts <- nnet.Wts.boot}
	if (resampling == "cv")   {p1 <- c.n1; p2 <- c.n2; p3 <- c.n3; nnet.Wts <- nnet.Wts.cv}
	p.lab <- paste(p1,p2,p3,sep="_")
}
if (method.name != "KNN" & method.name != "SVM" & method.name != "NNET") {
	p1 <- p2 <- p3 <- 0
	p.lab <- ""
}


train.and.test.model.predict <- train.and.test.model(train=data,
                                 method=method.name,
                                 test=worldclim2,
                                 test2=NULL,
                                 test3=NULL,
                                 resp.var.name="pres", 
                                 knn.par = p1, 
                                 svm.par = c("rbfdot",p1,p2), 
                                 nnet.par = c(p1,p2,p3),
                                 Wts = nnet.Wts,
                                 nbtree=10000,
                                 priors=NULL,
                                 prob=T)

setwd(paste("C:\\MMA\\",species.name,"\\_prediction_maps\\_predict_world",sep=""))
save(train.and.test.model.predict,file=paste("World_predictions_",species.name,"_",method.name,"_",p.lab,".Rdata",sep=""))

# Results
res <- cbind(train.and.test.model.predict$pred.test,train.and.test.model.predict$prob.test)
colnames(res) <- c("Prediction","Probability")

plot.split(species.name,method.name,save.fig=F)
savePlot(paste("World_prediction_",species.name,"_",method.name,"_",p.lab,".png",sep=""),type="png")

# ------------------------ #
# Make database for ArcGIS
# ------------------------ #

# Save things greater than threshold
thresh <- 0.1
dat.prob <- cbind(x[res[,2]>thresh,1],x[res[,2]>thresh,2],res[res[,2]>thresh,2])
colnames(dat.prob) <- c("xcoord", "ycoord","Prob")
write.table(dat.prob, paste("World_",species.name,"_",method.name,"_",p.lab,"_threshold_",thresh,"_data_prob.txt",sep=""), row.names=F)

dat.vote <- cbind(x[res[,1]>0,1],x[res[,1]>0,2],res[res[,1]>0,1])
colnames(dat.vote) <- c("xcoord", "ycoord","Vote")
write.table(dat.vote, paste("World_",species.name,"_",method.name,"_",p.lab,"_data_vote1.txt",sep=""), row.names=F)

# Repeat this part as many times as you wish, by changing the method.name (capital letters), 
# resampling method and/or parameters. Each time the output will be saved with the appropriate name.

# --------------------------------------------------------------------- #
# --------------------------------------------------------------------- #

###################################################################NNET-WORLD#########################################################################################

method.name <- "NNET" # Capital letters!
resampling <- "boot"


if (method.name == "KNN") {
	if (resampling == "boot") p1 <- b.k1  
	if (resampling == "cv")   p1 <- c.k1 
	p2 <- 0; p3 <- 0; p.lab <- paste(p1,sep="_")
}
if (method.name == "SVM") {
	if (resampling == "boot") {p1 <- b.s1; p2 <- b.s2}
	if (resampling == "cv")   {p1 <- c.s1; p2 <- c.s2}
	p3 <- 0; p.lab <- paste(p1,p2,sep="_")
}
if (method.name == "NNET") {
	if (resampling == "boot") {p1 <- b.n1; p2 <- b.n2; p3 <- b.n3; nnet.Wts <- nnet.Wts.boot}
	if (resampling == "cv")   {p1 <- c.n1; p2 <- c.n2; p3 <- c.n3; nnet.Wts <- nnet.Wts.cv}
	p.lab <- paste(p1,p2,p3,sep="_")
}
if (method.name != "KNN" & method.name != "SVM" & method.name != "NNET") {
	p1 <- p2 <- p3 <- 0
	p.lab <- ""
}


train.and.test.model.predict <- train.and.test.model(train=data,
                                 method=method.name,
                                 test=worldclim2,
                                 test2=NULL,
                                 test3=NULL,
                                 resp.var.name="pres", 
                                 knn.par = p1, 
                                 svm.par = c("rbfdot",p1,p2), 
                                 nnet.par = c(p1,p2,p3),
                                 Wts = nnet.Wts,
                                 nbtree=10000,
                                 priors=NULL,
                                 prob=T)

setwd(paste("C:\\MMA\\",species.name,"\\_prediction_maps\\_predict_world",sep=""))
save(train.and.test.model.predict,file=paste("World_predictions_",species.name,"_",method.name,"_",p.lab,".Rdata",sep=""))

# Results
res <- cbind(train.and.test.model.predict$pred.test,train.and.test.model.predict$prob.test)
colnames(res) <- c("Prediction","Probability")

plot.split(species.name,method.name,save.fig=F)
savePlot(paste("World_prediction_",species.name,"_",method.name,"_",p.lab,".png",sep=""),type="png")

# ------------------------ #
# Make database for ArcGIS
# ------------------------ #

# Save things greater than threshold
thresh <- 0.1
dat.prob <- cbind(x[res[,2]>thresh,1],x[res[,2]>thresh,2],res[res[,2]>thresh,2])
colnames(dat.prob) <- c("xcoord", "ycoord","Prob")
write.table(dat.prob, paste("World_",species.name,"_",method.name,"_",p.lab,"_threshold_",thresh,"_data_prob.txt",sep=""), row.names=F)

dat.vote <- cbind(x[res[,1]>0,1],x[res[,1]>0,2],res[res[,1]>0,1])
colnames(dat.vote) <- c("xcoord", "ycoord","Vote")
write.table(dat.vote, paste("World_",species.name,"_",method.name,"_",p.lab,"_data_vote1.txt",sep=""), row.names=F)

# Repeat this part as many times as you wish, by changing the method.name (capital letters), 
# resampling method and/or parameters. Each time the output will be saved with the appropriate name.

# --------------------------------------------------------------------- #
# --------------------------------------------------------------------- #
#--------------------------------------------------------#
#  Save workspace to redo world prediction if necessary  |
#--------------------------------------------------------#
#save workspace
save.image(paste("C:\\MMA\\",species.name,"\\aa11_world.RData",sep=""))
#save history
savehistory(file = (paste("C:\\MMA\\",species.name,"\\aa11_world.Rhistory",sep="")))

##################
### PREDICT NZ ###
##################
#----------------------#
#  Parameter settings  |
#----------------------#

# boot:
b.k1 <- 4
b.s1 <- 10
b.s2 <- 0.5
b.n1 <- 5
b.n2 <- 500
b.n3 <- 0.01

# cv:
c.k1 <- 5
c.s1 <- 100
c.s2 <- 0.9
c.n1 <- 5
c.n2 <- 500
c.n3 <- 0.01




#############################################################NZ-LDA###################################################################################################

# ------------------ #
# Load climate data
# ------------------ #

load("C:\\MMA\\_database\\bioclim_nz_alt_mdc.Rdata")
worldclim <- bioclim_nz_alt_mdc
x.nz <- worldclim[,c("xcoord","ycoord")] # all of NZ as a base, dim(x.nz)

# --------------------- #
# Filter worldclim data 
# --------------------- #

f <- function(x) sum(x>=55537)==0 # distinguish missing values
bool <- apply(worldclim[,colnames(data)[1:(ncol(data)-1)]],1,f) # columns as in data, remove missing
worldclim2 <- worldclim[bool,colnames(data)[1:(ncol(data)-1)]] # and those values for testing (just variables)
x <- worldclim[bool,c(1,2)]; nrow(x)==nrow(worldclim2) # just coordinates with no missing data


setwd(paste("C:\\MMA\\",species.name,"\\_results_compar.models",sep=""))
nnet.Wts.boot <- read.table(paste(species.name,"_best_weights_",b.n1,"_",b.n2,"_",b.n3,".txt",sep=""),header=T)
nnet.Wts.cv <- nnet.Wts.boot

method.name <- "LDA"
resampling <- "boot"


if (method.name == "KNN") {
	if (resampling == "boot") p1 <- b.k1  
	if (resampling == "cv")   p1 <- c.k1 
	p2 <- 0; p3 <- 0; p.lab <- paste(p1,sep="_")
}
if (method.name == "SVM") {
	if (resampling == "boot") {p1 <- b.s1; p2 <- b.s2}
	if (resampling == "cv")   {p1 <- c.s1; p2 <- c.s2}
	p3 <- 0; p.lab <- paste(p1,p2,sep="_")
}
if (method.name == "NNET") {
	if (resampling == "boot") {p1 <- b.n1; p2 <- b.n2; p3 <- b.n3; nnet.Wts <- nnet.Wts.boot}
	if (resampling == "cv")   {p1 <- c.n1; p2 <- c.n2; p3 <- c.n3; nnet.Wts <- nnet.Wts.cv}
	p.lab <- paste(p1,p2,p3,sep="_")
}
if (method.name != "KNN" & method.name != "SVM" & method.name != "NNET") {
	p1 <- p2 <- p3 <- 0
	p.lab <- ""
}




train.and.test.model.predict <- train.and.test.model(train=data,
                                 method=method.name,
                                 test=worldclim2,
                                 test2=NULL,
                                 test3=NULL,
                                 resp.var.name="pres", 
                                 knn.par = p1, 
                                 svm.par = c("rbfdot",p1,p2), 
                                 nnet.par = c(p1,p2,p3),
                                 Wts = nnet.Wts,
                                 nbtree=10000,
                                 priors=NULL,
                                 prob=T)


setwd(paste("C:\\MMA\\",species.name,"\\_prediction_maps\\_predict_NZ",sep=""))
save(train.and.test.model.predict,file=paste("NZ_prediction_",species.name,"_",method.name,"_",p.lab,".Rdata",sep=""))

# Load data
res <- cbind(train.and.test.model.predict$pred.test,train.and.test.model.predict$prob.test)
colnames(res) <- c("Prediction","Probability")

# ------- #
# Plot NZ #
# ------- #

plot.split.nz(species.name,method.name,color.choice="venette",save.fig=F)
savePlot(paste("NZ_prediction_",species.name,"_",method.name,"_",p.lab,".png",sep=""), type="png")

# Export lon, lat, prediction, probability>lower.bound

# ------------------------ #
# Make database for ArcGIS
# ------------------------ #

# Save things greater than threshold
thresh <- 0.1
dat.prob <- cbind(x[res[,2]>thresh,1],x[res[,2]>thresh,2],res[res[,2]>thresh,2])
colnames(dat.prob) <- c("xcoord", "ycoord","Prob")
write.table(dat.prob, paste("NZ_",species.name,"_",method.name,"_",p.lab,"_threshold_",thresh,"_data_prob.txt",sep=""), row.names=F)

dat.vote <- cbind(x[res[,1]>0,1],x[res[,1]>0,2],res[res[,1]>0,1])
colnames(dat.vote) <- c("xcoord", "ycoord","Vote")
write.table(dat.vote, paste("NZ_",species.name,"_",method.name,"_",p.lab,"_data_vote1.txt",sep=""), row.names=F)

#############################################################NZ-QDA###################################################################################################

# ------------------ #
# Load climate data
# ------------------ #

load("C:\\MMA\\_database\\bioclim_nz_alt_mdc.Rdata")
worldclim <- bioclim_nz_alt_mdc

x.nz <- worldclim[,c("xcoord","ycoord")] # all of NZ as a base, dim(x.nz)

# --------------------- #
# Filter worldclim data 
# --------------------- #

f <- function(x) sum(x>=55537)==0 # distinguish missing values
bool <- apply(worldclim[,colnames(data)[1:(ncol(data)-1)]],1,f) # columns as in data, remove missing
worldclim2 <- worldclim[bool,colnames(data)[1:(ncol(data)-1)]] # and those values for testing (just variables)
x <- worldclim[bool,c(1,2)]; nrow(x)==nrow(worldclim2) # just coordinates with no missing data

setwd(paste("C:\\MMA\\",species.name,"\\_results_compar.models",sep=""))
nnet.Wts.boot <- read.table(paste(species.name,"_best_weights_",b.n1,"_",b.n2,"_",b.n3,".txt",sep=""),header=T)
nnet.Wts.cv <- nnet.Wts.boot

method.name <- "QDA"
resampling <- "boot"


if (method.name == "KNN") {
	if (resampling == "boot") p1 <- b.k1  
	if (resampling == "cv")   p1 <- c.k1 
	p2 <- 0; p3 <- 0; p.lab <- paste(p1,sep="_")
}
if (method.name == "SVM") {
	if (resampling == "boot") {p1 <- b.s1; p2 <- b.s2}
	if (resampling == "cv")   {p1 <- c.s1; p2 <- c.s2}
	p3 <- 0; p.lab <- paste(p1,p2,sep="_")
}
if (method.name == "NNET") {
	if (resampling == "boot") {p1 <- b.n1; p2 <- b.n2; p3 <- b.n3; nnet.Wts <- nnet.Wts.boot}
	if (resampling == "cv")   {p1 <- c.n1; p2 <- c.n2; p3 <- c.n3; nnet.Wts <- nnet.Wts.cv}
	p.lab <- paste(p1,p2,p3,sep="_")
}
if (method.name != "KNN" & method.name != "SVM" & method.name != "NNET") {
	p1 <- p2 <- p3 <- 0
	p.lab <- ""
}




train.and.test.model.predict <- train.and.test.model(train=data,
                                 method=method.name,
                                 test=worldclim2,
                                 test2=NULL,
                                 test3=NULL,
                                 resp.var.name="pres", 
                                 knn.par = p1, 
                                 svm.par = c("rbfdot",p1,p2), 
                                 nnet.par = c(p1,p2,p3),
                                 Wts = nnet.Wts,
                                 nbtree=10000,
                                 priors=NULL,
                                 prob=T)


setwd(paste("C:\\MMA\\",species.name,"\\_prediction_maps\\_predict_NZ",sep=""))
save(train.and.test.model.predict,file=paste("NZ_prediction_",species.name,"_",method.name,"_",p.lab,".Rdata",sep=""))

# Load data
res <- cbind(train.and.test.model.predict$pred.test,train.and.test.model.predict$prob.test)
colnames(res) <- c("Prediction","Probability")

# ------- #
# Plot NZ #
# ------- #

plot.split.nz(species.name,method.name,color.choice="venette",save.fig=F)
savePlot(paste("NZ_prediction_",species.name,"_",method.name,"_",p.lab,".png",sep=""), type="png")

# Export lon, lat, prediction, probability>lower.bound

# ------------------------ #
# Make database for ArcGIS
# ------------------------ #

# Save things greater than threshold
thresh <- 0.1
dat.prob <- cbind(x[res[,2]>thresh,1],x[res[,2]>thresh,2],res[res[,2]>thresh,2])
colnames(dat.prob) <- c("xcoord", "ycoord","Prob")
write.table(dat.prob, paste("NZ_",species.name,"_",method.name,"_",p.lab,"_threshold_",thresh,"_data_prob.txt",sep=""), row.names=F)

dat.vote <- cbind(x[res[,1]>0,1],x[res[,1]>0,2],res[res[,1]>0,1])
colnames(dat.vote) <- c("xcoord", "ycoord","Vote")
write.table(dat.vote, paste("NZ_",species.name,"_",method.name,"_",p.lab,"_data_vote1.txt",sep=""), row.names=F)

#############################################################NZ-LOG###################################################################################################

# ------------------ #
# Load climate data
# ------------------ #

load("C:\\MMA\\_database\\bioclim_nz_alt_mdc.Rdata")
worldclim <- bioclim_nz_alt_mdc
x.nz <- worldclim[,c("xcoord","ycoord")] # all of NZ as a base, dim(x.nz)

# --------------------- #
# Filter worldclim data 
# --------------------- #

f <- function(x) sum(x>=55537)==0 # distinguish missing values
bool <- apply(worldclim[,colnames(data)[1:(ncol(data)-1)]],1,f) # columns as in data, remove missing
worldclim2 <- worldclim[bool,colnames(data)[1:(ncol(data)-1)]] # and those values for testing (just variables)
x <- worldclim[bool,c(1,2)]; nrow(x)==nrow(worldclim2) # just coordinates with no missing data

setwd(paste("C:\\MMA\\",species.name,"\\_results_compar.models",sep=""))
nnet.Wts.boot <- read.table(paste(species.name,"_best_weights_",b.n1,"_",b.n2,"_",b.n3,".txt",sep=""),header=T)
nnet.Wts.cv <- nnet.Wts.boot

method.name <- "LOG"
resampling <- "boot"


if (method.name == "KNN") {
	if (resampling == "boot") p1 <- b.k1  
	if (resampling == "cv")   p1 <- c.k1 
	p2 <- 0; p3 <- 0; p.lab <- paste(p1,sep="_")
}
if (method.name == "SVM") {
	if (resampling == "boot") {p1 <- b.s1; p2 <- b.s2}
	if (resampling == "cv")   {p1 <- c.s1; p2 <- c.s2}
	p3 <- 0; p.lab <- paste(p1,p2,sep="_")
}
if (method.name == "NNET") {
	if (resampling == "boot") {p1 <- b.n1; p2 <- b.n2; p3 <- b.n3; nnet.Wts <- nnet.Wts.boot}
	if (resampling == "cv")   {p1 <- c.n1; p2 <- c.n2; p3 <- c.n3; nnet.Wts <- nnet.Wts.cv}
	p.lab <- paste(p1,p2,p3,sep="_")
}
if (method.name != "KNN" & method.name != "SVM" & method.name != "NNET") {
	p1 <- p2 <- p3 <- 0
	p.lab <- ""
}




train.and.test.model.predict <- train.and.test.model(train=data,
                                 method=method.name,
                                 test=worldclim2,
                                 test2=NULL,
                                 test3=NULL,
                                 resp.var.name="pres", 
                                 knn.par = p1, 
                                 svm.par = c("rbfdot",p1,p2), 
                                 nnet.par = c(p1,p2,p3),
                                 Wts = nnet.Wts,
                                 nbtree=10000,
                                 priors=NULL,
                                 prob=T)


setwd(paste("C:\\MMA\\",species.name,"\\_prediction_maps\\_predict_NZ",sep=""))
save(train.and.test.model.predict,file=paste("NZ_prediction_",species.name,"_",method.name,"_",p.lab,".Rdata",sep=""))

# Load data
res <- cbind(train.and.test.model.predict$pred.test,train.and.test.model.predict$prob.test)
colnames(res) <- c("Prediction","Probability")

# ------- #
# Plot NZ #
# ------- #

plot.split.nz(species.name,method.name,color.choice="venette",save.fig=F)
savePlot(paste("NZ_prediction_",species.name,"_",method.name,"_",p.lab,".png",sep=""), type="png")

# Export lon, lat, prediction, probability>lower.bound

# ------------------------ #
# Make database for ArcGIS
# ------------------------ #

# Save things greater than threshold
thresh <- 0.1
dat.prob <- cbind(x[res[,2]>thresh,1],x[res[,2]>thresh,2],res[res[,2]>thresh,2])
colnames(dat.prob) <- c("xcoord", "ycoord","Prob")
write.table(dat.prob, paste("NZ_",species.name,"_",method.name,"_",p.lab,"_threshold_",thresh,"_data_prob.txt",sep=""), row.names=F)

dat.vote <- cbind(x[res[,1]>0,1],x[res[,1]>0,2],res[res[,1]>0,1])
colnames(dat.vote) <- c("xcoord", "ycoord","Vote")
write.table(dat.vote, paste("NZ_",species.name,"_",method.name,"_",p.lab,"_data_vote1.txt",sep=""), row.names=F)

#############################################################NZ-NB###################################################################################################

# ------------------ #
# Load climate data
# ------------------ #

load("C:\\MMA\\_database\\bioclim_nz_alt_mdc.Rdata")
worldclim <- bioclim_nz_alt_mdc
x.nz <- worldclim[,c("xcoord","ycoord")] # all of NZ as a base, dim(x.nz)

# --------------------- #
# Filter worldclim data 
# --------------------- #

f <- function(x) sum(x>=55537)==0 # distinguish missing values
bool <- apply(worldclim[,colnames(data)[1:(ncol(data)-1)]],1,f) # columns as in data, remove missing
worldclim2 <- worldclim[bool,colnames(data)[1:(ncol(data)-1)]] # and those values for testing (just variables)
x <- worldclim[bool,c(1,2)]; nrow(x)==nrow(worldclim2) # just coordinates with no missing data

setwd(paste("C:\\MMA\\",species.name,"\\_results_compar.models",sep=""))
nnet.Wts.boot <- read.table(paste(species.name,"_best_weights_",b.n1,"_",b.n2,"_",b.n3,".txt",sep=""),header=T)
nnet.Wts.cv <- nnet.Wts.boot

method.name <- "NB"
resampling <- "boot"


if (method.name == "KNN") {
	if (resampling == "boot") p1 <- b.k1  
	if (resampling == "cv")   p1 <- c.k1 
	p2 <- 0; p3 <- 0; p.lab <- paste(p1,sep="_")
}
if (method.name == "SVM") {
	if (resampling == "boot") {p1 <- b.s1; p2 <- b.s2}
	if (resampling == "cv")   {p1 <- c.s1; p2 <- c.s2}
	p3 <- 0; p.lab <- paste(p1,p2,sep="_")
}
if (method.name == "NNET") {
	if (resampling == "boot") {p1 <- b.n1; p2 <- b.n2; p3 <- b.n3; nnet.Wts <- nnet.Wts.boot}
	if (resampling == "cv")   {p1 <- c.n1; p2 <- c.n2; p3 <- c.n3; nnet.Wts <- nnet.Wts.cv}
	p.lab <- paste(p1,p2,p3,sep="_")
}
if (method.name != "KNN" & method.name != "SVM" & method.name != "NNET") {
	p1 <- p2 <- p3 <- 0
	p.lab <- ""
}




train.and.test.model.predict <- train.and.test.model(train=data,
                                 method=method.name,
                                 test=worldclim2,
                                 test2=NULL,
                                 test3=NULL,
                                 resp.var.name="pres", 
                                 knn.par = p1, 
                                 svm.par = c("rbfdot",p1,p2), 
                                 nnet.par = c(p1,p2,p3),
                                 Wts = nnet.Wts,
                                 nbtree=10000,
                                 priors=NULL,
                                 prob=T)


setwd(paste("C:\\MMA\\",species.name,"\\_prediction_maps\\_predict_NZ",sep=""))
save(train.and.test.model.predict,file=paste("NZ_prediction_",species.name,"_",method.name,"_",p.lab,".Rdata",sep=""))

# Load data
res <- cbind(train.and.test.model.predict$pred.test,train.and.test.model.predict$prob.test)
colnames(res) <- c("Prediction","Probability")

# ------- #
# Plot NZ #
# ------- #

plot.split.nz(species.name,method.name,color.choice="venette",save.fig=F)
savePlot(paste("NZ_prediction_",species.name,"_",method.name,"_",p.lab,".png",sep=""), type="png")

# Export lon, lat, prediction, probability>lower.bound

# ------------------------ #
# Make database for ArcGIS
# ------------------------ #

# Save things greater than threshold
thresh <- 0.1
dat.prob <- cbind(x[res[,2]>thresh,1],x[res[,2]>thresh,2],res[res[,2]>thresh,2])
colnames(dat.prob) <- c("xcoord", "ycoord","Prob")
write.table(dat.prob, paste("NZ_",species.name,"_",method.name,"_",p.lab,"_threshold_",thresh,"_data_prob.txt",sep=""), row.names=F)

dat.vote <- cbind(x[res[,1]>0,1],x[res[,1]>0,2],res[res[,1]>0,1])
colnames(dat.vote) <- c("xcoord", "ycoord","Vote")
write.table(dat.vote, paste("NZ_",species.name,"_",method.name,"_",p.lab,"_data_vote1.txt",sep=""), row.names=F)

#############################################################NZ-CART###################################################################################################

# ------------------ #
# Load climate data
# ------------------ #

load("C:\\MMA\\_database\\bioclim_nz.Rdata")
worldclim <- bioclim_nz
x.nz <- worldclim[,c("xcoord","ycoord")] # all of NZ as a base, dim(x.nz)

# --------------------- #
# Filter worldclim data 
# --------------------- #

f <- function(x) sum(x>=55537)==0 # distinguish missing values
bool <- apply(worldclim[,colnames(data)[1:(ncol(data)-1)]],1,f) # columns as in data, remove missing
worldclim2 <- worldclim[bool,colnames(data)[1:(ncol(data)-1)]] # and those values for testing (just variables)
x <- worldclim[bool,c(1,2)]; nrow(x)==nrow(worldclim2) # just coordinates with no missing data

#----------------------#
#  Parameter settings  |
#----------------------#

setwd(paste("C:\\MMA\\",species.name,"\\_results_compar.models",sep=""))
nnet.Wts.boot <- read.table(paste(species.name,"_best_weights_",b.n1,"_",b.n2,"_",b.n3,".txt",sep=""),header=T)
nnet.Wts.cv <- nnet.Wts.boot

method.name <- "CART"
resampling <- "boot"


if (method.name == "KNN") {
	if (resampling == "boot") p1 <- b.k1  
	if (resampling == "cv")   p1 <- c.k1 
	p2 <- 0; p3 <- 0; p.lab <- paste(p1,sep="_")
}
if (method.name == "SVM") {
	if (resampling == "boot") {p1 <- b.s1; p2 <- b.s2}
	if (resampling == "cv")   {p1 <- c.s1; p2 <- c.s2}
	p3 <- 0; p.lab <- paste(p1,p2,sep="_")
}
if (method.name == "NNET") {
	if (resampling == "boot") {p1 <- b.n1; p2 <- b.n2; p3 <- b.n3; nnet.Wts <- nnet.Wts.boot}
	if (resampling == "cv")   {p1 <- c.n1; p2 <- c.n2; p3 <- c.n3; nnet.Wts <- nnet.Wts.cv}
	p.lab <- paste(p1,p2,p3,sep="_")
}
if (method.name != "KNN" & method.name != "SVM" & method.name != "NNET") {
	p1 <- p2 <- p3 <- 0
	p.lab <- ""
}




train.and.test.model.predict <- train.and.test.model(train=data,
                                 method=method.name,
                                 test=worldclim2,
                                 test2=NULL,
                                 test3=NULL,
                                 resp.var.name="pres", 
                                 knn.par = p1, 
                                 svm.par = c("rbfdot",p1,p2), 
                                 nnet.par = c(p1,p2,p3),
                                 Wts = nnet.Wts,
                                 nbtree=10000,
                                 priors=NULL,
                                 prob=T)


setwd(paste("C:\\MMA\\",species.name,"\\_prediction_maps\\_predict_NZ",sep=""))
save(train.and.test.model.predict,file=paste("NZ_prediction_",species.name,"_",method.name,"_",p.lab,".Rdata",sep=""))

# Load data
res <- cbind(train.and.test.model.predict$pred.test,train.and.test.model.predict$prob.test)
colnames(res) <- c("Prediction","Probability")

# ------- #
# Plot NZ #
# ------- #

plot.split.nz(species.name,method.name,color.choice="venette",save.fig=F)
savePlot(paste("NZ_prediction_",species.name,"_",method.name,"_",p.lab,".png",sep=""), type="png")

# Export lon, lat, prediction, probability>lower.bound

# ------------------------ #
# Make database for ArcGIS
# ------------------------ #

# Save things greater than threshold
thresh <- 0.1
dat.prob <- cbind(x[res[,2]>thresh,1],x[res[,2]>thresh,2],res[res[,2]>thresh,2])
colnames(dat.prob) <- c("xcoord", "ycoord","Prob")
write.table(dat.prob, paste("NZ_",species.name,"_",method.name,"_",p.lab,"_threshold_",thresh,"_data_prob.txt",sep=""), row.names=F)

dat.vote <- cbind(x[res[,1]>0,1],x[res[,1]>0,2],res[res[,1]>0,1])
colnames(dat.vote) <- c("xcoord", "ycoord","Vote")
write.table(dat.vote, paste("NZ_",species.name,"_",method.name,"_",p.lab,"_data_vote1.txt",sep=""), row.names=F)

#############################################################NZ-CTREE###################################################################################################

# ------------------ #
# Load climate data
# ------------------ #

load("C:\\MMA\\_database\\bioclim_nz_alt_mdc.Rdata")
worldclim <- bioclim_nz_alt_mdc
x.nz <- worldclim[,c("xcoord","ycoord")] # all of NZ as a base, dim(x.nz)

# --------------------- #
# Filter worldclim data 
# --------------------- #

f <- function(x) sum(x>=55537)==0 # distinguish missing values
bool <- apply(worldclim[,colnames(data)[1:(ncol(data)-1)]],1,f) # columns as in data, remove missing
worldclim2 <- worldclim[bool,colnames(data)[1:(ncol(data)-1)]] # and those values for testing (just variables)
x <- worldclim[bool,c(1,2)]; nrow(x)==nrow(worldclim2) # just coordinates with no missing data

setwd(paste("C:\\MMA\\",species.name,"\\_results_compar.models",sep=""))
nnet.Wts.boot <- read.table(paste(species.name,"_best_weights_",b.n1,"_",b.n2,"_",b.n3,".txt",sep=""),header=T)
nnet.Wts.cv <- nnet.Wts.boot

method.name <- "CTREE"
resampling <- "boot"


if (method.name == "KNN") {
	if (resampling == "boot") p1 <- b.k1  
	if (resampling == "cv")   p1 <- c.k1 
	p2 <- 0; p3 <- 0; p.lab <- paste(p1,sep="_")
}
if (method.name == "SVM") {
	if (resampling == "boot") {p1 <- b.s1; p2 <- b.s2}
	if (resampling == "cv")   {p1 <- c.s1; p2 <- c.s2}
	p3 <- 0; p.lab <- paste(p1,p2,sep="_")
}
if (method.name == "NNET") {
	if (resampling == "boot") {p1 <- b.n1; p2 <- b.n2; p3 <- b.n3; nnet.Wts <- nnet.Wts.boot}
	if (resampling == "cv")   {p1 <- c.n1; p2 <- c.n2; p3 <- c.n3; nnet.Wts <- nnet.Wts.cv}
	p.lab <- paste(p1,p2,p3,sep="_")
}
if (method.name != "KNN" & method.name != "SVM" & method.name != "NNET") {
	p1 <- p2 <- p3 <- 0
	p.lab <- ""
}




train.and.test.model.predict <- train.and.test.model(train=data,
                                 method=method.name,
                                 test=worldclim2,
                                 test2=NULL,
                                 test3=NULL,
                                 resp.var.name="pres", 
                                 knn.par = p1, 
                                 svm.par = c("rbfdot",p1,p2), 
                                 nnet.par = c(p1,p2,p3),
                                 Wts = nnet.Wts,
                                 nbtree=10000,
                                 priors=NULL,
                                 prob=T)


setwd(paste("C:\\MMA\\",species.name,"\\_prediction_maps\\_predict_NZ",sep=""))
save(train.and.test.model.predict,file=paste("NZ_prediction_",species.name,"_",method.name,"_",p.lab,".Rdata",sep=""))

# Load data
res <- cbind(train.and.test.model.predict$pred.test,train.and.test.model.predict$prob.test)
colnames(res) <- c("Prediction","Probability")

# ------- #
# Plot NZ #
# ------- #

plot.split.nz(species.name,method.name,color.choice="venette",save.fig=F)
savePlot(paste("NZ_prediction_",species.name,"_",method.name,"_",p.lab,".png",sep=""), type="png")

# Export lon, lat, prediction, probability>lower.bound

# ------------------------ #
# Make database for ArcGIS
# ------------------------ #

# Save things greater than threshold
thresh <- 0.1
dat.prob <- cbind(x[res[,2]>thresh,1],x[res[,2]>thresh,2],res[res[,2]>thresh,2])
colnames(dat.prob) <- c("xcoord", "ycoord","Prob")
write.table(dat.prob, paste("NZ_",species.name,"_",method.name,"_",p.lab,"_threshold_",thresh,"_data_prob.txt",sep=""), row.names=F)

dat.vote <- cbind(x[res[,1]>0,1],x[res[,1]>0,2],res[res[,1]>0,1])
colnames(dat.vote) <- c("xcoord", "ycoord","Vote")
write.table(dat.vote, paste("NZ_",species.name,"_",method.name,"_",p.lab,"_data_vote1.txt",sep=""), row.names=F)

#############################################################NZ-KNN###################################################################################################

# ------------------ #
# Load climate data
# ------------------ #

load("C:\\MMA\\_database\\bioclim_nz_alt_mdc.Rdata")
worldclim <- bioclim_nz_alt_mdc
x.nz <- worldclim[,c("xcoord","ycoord")] # all of NZ as a base, dim(x.nz)

# --------------------- #
# Filter worldclim data 
# --------------------- #

f <- function(x) sum(x>=55537)==0 # distinguish missing values
bool <- apply(worldclim[,colnames(data)[1:(ncol(data)-1)]],1,f) # columns as in data, remove missing
worldclim2 <- worldclim[bool,colnames(data)[1:(ncol(data)-1)]] # and those values for testing (just variables)
x <- worldclim[bool,c(1,2)]; nrow(x)==nrow(worldclim2) # just coordinates with no missing data

setwd(paste("C:\\MMA\\",species.name,"\\_results_compar.models",sep=""))
nnet.Wts.boot <- read.table(paste(species.name,"_best_weights_",b.n1,"_",b.n2,"_",b.n3,".txt",sep=""),header=T)
nnet.Wts.cv <- nnet.Wts.boot

method.name <- "KNN"
resampling <- "boot"


if (method.name == "KNN") {
	if (resampling == "boot") p1 <- b.k1  
	if (resampling == "cv")   p1 <- c.k1 
	p2 <- 0; p3 <- 0; p.lab <- paste(p1,sep="_")
}
if (method.name == "SVM") {
	if (resampling == "boot") {p1 <- b.s1; p2 <- b.s2}
	if (resampling == "cv")   {p1 <- c.s1; p2 <- c.s2}
	p3 <- 0; p.lab <- paste(p1,p2,sep="_")
}
if (method.name == "NNET") {
	if (resampling == "boot") {p1 <- b.n1; p2 <- b.n2; p3 <- b.n3; nnet.Wts <- nnet.Wts.boot}
	if (resampling == "cv")   {p1 <- c.n1; p2 <- c.n2; p3 <- c.n3; nnet.Wts <- nnet.Wts.cv}
	p.lab <- paste(p1,p2,p3,sep="_")
}
if (method.name != "KNN" & method.name != "SVM" & method.name != "NNET") {
	p1 <- p2 <- p3 <- 0
	p.lab <- ""
}




train.and.test.model.predict <- train.and.test.model(train=data,
                                 method=method.name,
                                 test=worldclim2,
                                 test2=NULL,
                                 test3=NULL,
                                 resp.var.name="pres", 
                                 knn.par = p1, 
                                 svm.par = c("rbfdot",p1,p2), 
                                 nnet.par = c(p1,p2,p3),
                                 Wts = nnet.Wts,
                                 nbtree=10000,
                                 priors=NULL,
                                 prob=T)


setwd(paste("C:\\MMA\\",species.name,"\\_prediction_maps\\_predict_NZ",sep=""))
save(train.and.test.model.predict,file=paste("NZ_prediction_",species.name,"_",method.name,"_",p.lab,".Rdata",sep=""))

# Load data
res <- cbind(train.and.test.model.predict$pred.test,train.and.test.model.predict$prob.test)
colnames(res) <- c("Prediction","Probability")

# ------- #
# Plot NZ #
# ------- #

plot.split.nz(species.name,method.name,color.choice="venette",save.fig=F)
savePlot(paste("NZ_prediction_",species.name,"_",method.name,"_",p.lab,".png",sep=""), type="png")

# Export lon, lat, prediction, probability>lower.bound

# ------------------------ #
# Make database for ArcGIS
# ------------------------ #

# Save things greater than threshold
thresh <- 0.1
dat.prob <- cbind(x[res[,2]>thresh,1],x[res[,2]>thresh,2],res[res[,2]>thresh,2])
colnames(dat.prob) <- c("xcoord", "ycoord","Prob")
write.table(dat.prob, paste("NZ_",species.name,"_",method.name,"_",p.lab,"_threshold_",thresh,"_data_prob.txt",sep=""), row.names=F)

dat.vote <- cbind(x[res[,1]>0,1],x[res[,1]>0,2],res[res[,1]>0,1])
colnames(dat.vote) <- c("xcoord", "ycoord","Vote")
write.table(dat.vote, paste("NZ_",species.name,"_",method.name,"_",p.lab,"_data_vote1.txt",sep=""), row.names=F)


#############################################################NZ-SVM###################################################################################################

# ------------------ #
# Load climate data
# ------------------ #

load("C:\\MMA\\_database\\bioclim_nz_alt_mdc.Rdata")
worldclim <- bioclim_nz_alt_mdc
x.nz <- worldclim[,c("xcoord","ycoord")] # all of NZ as a base, dim(x.nz)

# --------------------- #
# Filter worldclim data 
# --------------------- #

f <- function(x) sum(x>=55537)==0 # distinguish missing values
bool <- apply(worldclim[,colnames(data)[1:(ncol(data)-1)]],1,f) # columns as in data, remove missing
worldclim2 <- worldclim[bool,colnames(data)[1:(ncol(data)-1)]] # and those values for testing (just variables)
x <- worldclim[bool,c(1,2)]; nrow(x)==nrow(worldclim2) # just coordinates with no missing data

setwd(paste("C:\\MMA\\",species.name,"\\_results_compar.models",sep=""))
nnet.Wts.boot <- read.table(paste(species.name,"_best_weights_",b.n1,"_",b.n2,"_",b.n3,".txt",sep=""),header=T)
nnet.Wts.cv <- nnet.Wts.boot

method.name <- "SVM"
resampling <- "boot"


if (method.name == "KNN") {
	if (resampling == "boot") p1 <- b.k1  
	if (resampling == "cv")   p1 <- c.k1 
	p2 <- 0; p3 <- 0; p.lab <- paste(p1,sep="_")
}
if (method.name == "SVM") {
	if (resampling == "boot") {p1 <- b.s1; p2 <- b.s2}
	if (resampling == "cv")   {p1 <- c.s1; p2 <- c.s2}
	p3 <- 0; p.lab <- paste(p1,p2,sep="_")
}
if (method.name == "NNET") {
	if (resampling == "boot") {p1 <- b.n1; p2 <- b.n2; p3 <- b.n3; nnet.Wts <- nnet.Wts.boot}
	if (resampling == "cv")   {p1 <- c.n1; p2 <- c.n2; p3 <- c.n3; nnet.Wts <- nnet.Wts.cv}
	p.lab <- paste(p1,p2,p3,sep="_")
}
if (method.name != "KNN" & method.name != "SVM" & method.name != "NNET") {
	p1 <- p2 <- p3 <- 0
	p.lab <- ""
}




train.and.test.model.predict <- train.and.test.model(train=data,
                                 method=method.name,
                                 test=worldclim2,
                                 test2=NULL,
                                 test3=NULL,
                                 resp.var.name="pres", 
                                 knn.par = p1, 
                                 svm.par = c("rbfdot",p1,p2), 
                                 nnet.par = c(p1,p2,p3),
                                 Wts = nnet.Wts,
                                 nbtree=10000,
                                 priors=NULL,
                                 prob=T)


setwd(paste("C:\\MMA\\",species.name,"\\_prediction_maps\\_predict_NZ",sep=""))
save(train.and.test.model.predict,file=paste("NZ_prediction_",species.name,"_",method.name,"_",p.lab,".Rdata",sep=""))

# Load data
res <- cbind(train.and.test.model.predict$pred.test,train.and.test.model.predict$prob.test)
colnames(res) <- c("Prediction","Probability")

# ------- #
# Plot NZ #
# ------- #

plot.split.nz(species.name,method.name,color.choice="venette",save.fig=F)
savePlot(paste("NZ_prediction_",species.name,"_",method.name,"_",p.lab,".png",sep=""), type="png")

# Export lon, lat, prediction, probability>lower.bound

# ------------------------ #
# Make database for ArcGIS
# ------------------------ #

# Save things greater than threshold
thresh <- 0.1
dat.prob <- cbind(x[res[,2]>thresh,1],x[res[,2]>thresh,2],res[res[,2]>thresh,2])
colnames(dat.prob) <- c("xcoord", "ycoord","Prob")
write.table(dat.prob, paste("NZ_",species.name,"_",method.name,"_",p.lab,"_threshold_",thresh,"_data_prob.txt",sep=""), row.names=F)

dat.vote <- cbind(x[res[,1]>0,1],x[res[,1]>0,2],res[res[,1]>0,1])
colnames(dat.vote) <- c("xcoord", "ycoord","Vote")
write.table(dat.vote, paste("NZ_",species.name,"_",method.name,"_",p.lab,"_data_vote1.txt",sep=""), row.names=F)

#############################################################NZ-NNET###################################################################################################

# ------------------ #
# Load climate data
# ------------------ #

load("C:\\MMA\\_database\\bioclim_nz_alt_mdc.Rdata")
worldclim <- bioclim_nz_alt_mdc
x.nz <- worldclim[,c("xcoord","ycoord")] # all of NZ as a base, dim(x.nz)

# --------------------- #
# Filter worldclim data 
# --------------------- #

f <- function(x) sum(x>=55537)==0 # distinguish missing values
bool <- apply(worldclim[,colnames(data)[1:(ncol(data)-1)]],1,f) # columns as in data, remove missing
worldclim2 <- worldclim[bool,colnames(data)[1:(ncol(data)-1)]] # and those values for testing (just variables)
x <- worldclim[bool,c(1,2)]; nrow(x)==nrow(worldclim2) # just coordinates with no missing data

setwd(paste("C:\\MMA\\",species.name,"\\_results_compar.models",sep=""))
nnet.Wts.boot <- read.table(paste(species.name,"_best_weights_",b.n1,"_",b.n2,"_",b.n3,".txt",sep=""),header=T)
nnet.Wts.cv <- nnet.Wts.boot

method.name <- "NNET"
resampling <- "boot"


if (method.name == "KNN") {
	if (resampling == "boot") p1 <- b.k1  
	if (resampling == "cv")   p1 <- c.k1 
	p2 <- 0; p3 <- 0; p.lab <- paste(p1,sep="_")
}
if (method.name == "SVM") {
	if (resampling == "boot") {p1 <- b.s1; p2 <- b.s2}
	if (resampling == "cv")   {p1 <- c.s1; p2 <- c.s2}
	p3 <- 0; p.lab <- paste(p1,p2,sep="_")
}
if (method.name == "NNET") {
	if (resampling == "boot") {p1 <- b.n1; p2 <- b.n2; p3 <- b.n3; nnet.Wts <- nnet.Wts.boot}
	if (resampling == "cv")   {p1 <- c.n1; p2 <- c.n2; p3 <- c.n3; nnet.Wts <- nnet.Wts.cv}
	p.lab <- paste(p1,p2,p3,sep="_")
}
if (method.name != "KNN" & method.name != "SVM" & method.name != "NNET") {
	p1 <- p2 <- p3 <- 0
	p.lab <- ""
}




train.and.test.model.predict <- train.and.test.model(train=data,
                                 method=method.name,
                                 test=worldclim2,
                                 test2=NULL,
                                 test3=NULL,
                                 resp.var.name="pres", 
                                 knn.par = p1, 
                                 svm.par = c("rbfdot",p1,p2), 
                                 nnet.par = c(p1,p2,p3),
                                 Wts = nnet.Wts,
                                 nbtree=10000,
                                 priors=NULL,
                                 prob=T)


setwd(paste("C:\\MMA\\",species.name,"\\_prediction_maps\\_predict_NZ",sep=""))
save(train.and.test.model.predict,file=paste("NZ_prediction_",species.name,"_",method.name,"_",p.lab,".Rdata",sep=""))

# Load data
res <- cbind(train.and.test.model.predict$pred.test,train.and.test.model.predict$prob.test)
colnames(res) <- c("Prediction","Probability")

# ------- #
# Plot NZ #
# ------- #

plot.split.nz(species.name,method.name,color.choice="venette",save.fig=F)
savePlot(paste("NZ_prediction_",species.name,"_",method.name,"_",p.lab,".png",sep=""), type="png")

# Export lon, lat, prediction, probability>lower.bound

# ------------------------ #
# Make database for ArcGIS
# ------------------------ #

# Save things greater than threshold
thresh <- 0.1
dat.prob <- cbind(x[res[,2]>thresh,1],x[res[,2]>thresh,2],res[res[,2]>thresh,2])
colnames(dat.prob) <- c("xcoord", "ycoord","Prob")
write.table(dat.prob, paste("NZ_",species.name,"_",method.name,"_",p.lab,"_threshold_",thresh,"_data_prob.txt",sep=""), row.names=F)

dat.vote <- cbind(x[res[,1]>0,1],x[res[,1]>0,2],res[res[,1]>0,1])
colnames(dat.vote) <- c("xcoord", "ycoord","Vote")
write.table(dat.vote, paste("NZ_",species.name,"_",method.name,"_",p.lab,"_data_vote1.txt",sep=""), row.names=F)


#######################################################################################################################################################################
#save workspace
save.image(paste("C:\\MMA\\",species.name,"\\aa_1_1.RData",sep=""))
#save history
savehistory(file = (paste("C:\\MMA\\",species.name,"\\aa_1_1.Rhistory",sep="")))
