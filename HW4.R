#CLEAR VARIABLES
rm(list=ls())

#LOAD DATA
rip <- read.csv("top5_dataset.csv")
#CLEAN DATA OF NULL VALUES
rip <- rip[-c(which(rip$Woody_Height_m < 0)), ]
rip <- rip[-c(which(rip$Woody_DBH_cm < 5)), ]
#ADD HEIGHT AS CM
rip$htcm <- rip$Woody_Height_m * 100  #CONVERT TO CM

#riparian data loaded as rip 
rip$projplot <- as.factor(paste(rip$ProjCode,rip$Plot.Name)) 

#use tapply() to cycle through each project plot and generate stats 
#where ‘htcm’ is height in cm 
ripsum <- data.frame(cbind(tapply(rip$htcm,rip$projplot,mean),tapply(rip$htcm,rip$projplot,sd),tapply(rip$htcm,rip$projplot,length))) 

#add column names #(height mean, height standard deviation, number of plots) 
colnames(ripsum) <- c("htcmmn","htcmsd","plot.n") 

#add a projplot column (from row names) to ripsum 
ripsum$projplot <- as.factor(rownames(ripsum))

#subset for plots with more than one measurement 
ripsum <- ripsum[ripsum$plot.n > 1,] 
#create proj column by stripping out the first 5 characters 
ripsum$proj <- as.factor(substr(ripsum$projplot,1,5))

#create list of project sites 
projlevels <- levels(ripsum$proj) 

#compare a ‘for’ loop of summary 
#for (p in 1:length(projlevels)) print(summary(ripsum[ripsum$proj == projlevels[p],])) 
#with a summary using lapply() (known as list apply) 
#print(lapply(projlevels, function(x) summary(ripsum[ripsum$proj == x,])))

#note that there are several ways of doing this task, but 
#lapply() uses a list and a function to execute list items 
#>> lapply(list, function(x) myfunction(x)) 
#here we subset using which() where we select for x in list 
#and then subset again randomly using sample() 
#because these row subsets are from the vectorized data.frame 
#it looks like this: data[which(),][sample(),] where [rows,cols] 

nsamples <- 6 
ripres <- lapply(projlevels, function(x) ripsum[which(ripsum$proj == x),][sample(nrow(ripsum[which(ripsum$proj == x),]),nsamples),]) 

# combine samples by row using rbind() 
# and by calling ripres lapply function from do.call() 

ripsample <- do.call(rbind,ripres) 
summary(ripsample$proj)

#calculate CV using with(data,calc)
ripsample$cv <- with(ripsample,htcmsd / htcmmn)

#ANOVA
rip.proj.cv.aov = aov(cv~proj,data=ripsample)
print(summary(rip.proj.cv.aov))

#print(summary.lm(rip.proj.cv.aov))

rip.aov.hsd <- TukeyHSD(rip.proj.cv.aov)
#print(rip.aov.hsd)

#