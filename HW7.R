#CLEAR VARIABLES
rm(list=ls())

site='11335000'
sensor='00060'
start='1907-10-01'
end='2015-09-30'

data <- read.table(paste("http://waterdata.usgs.gov/nwis/dv?cb_",sensor,"=on&format=rdb&site_no=", site,"&referred_module=sw&period=&begin_date=",start,"&end_date=", end, sep=""),
                   header=T, sep="", quote="'", skip=0, colClasses = c("factor", "factor", "factor", "factor", "factor"))
print('DONE')

#REMOVE JUNK DATA FROM FIRST ROW
data = droplevels(data[-1,])
#RENAME LAST TWO COLUMNS
colnames(data)[4:5]= c("discharge_cfs","dat_qvalue")

data$datetime = as.POSIXct(data$datetime, "America/Los_Angeles")
data$discharge_cfs = as.double(as.character(data$discharge_cfs))

data$discharge_afd = data$discharge_cfs*1.98347
#NOTE POSSIBLE OUTLIER - 122,200 AF/Day?

#WATER YEAR
data$year = as.numeric(substr(data$datetime,1,4))
data$month = as.numeric(substr(data$datetime,6,7))
data$day = as.numeric(substr(data$datetime,9,11))
data$wy = ifelse(data$month > 9, data$year +1, data$year)
data$wym = ifelse(data$month > 9, data$month - 9, data$month + 3)

#REMOVE LEAP YEAR
data <- data[!(data$month ==2 & data$day == 29),]

#TOTAL DISCHARGE BY MONTH
data.mo.afd.sum = ts(as.vector(tapply(data$discharge_afd,list(data$wym,data$wy),sum)),frequency=12)
data.mo.afd.mean = ts(as.vector(tapply(data$discharge_afd,list(data$wym,data$wy),mean)),frequency=12)
data.mo.afd.max = ts(as.vector(tapply(data$discharge_afd,list(data$wym,data$wy),max)),frequency=12)
data.mo.cfs.mean = ts(as.vector(tapply(data$discharge_cfs,list(data$wym,data$wy),mean)),frequency=12)
data.mo.cfs.max = ts(as.vector(tapply(data$discharge_cfs,list(data$wym,data$wy),max)),frequency=12)

data.yr.afd.sum = ts(as.vector(tapply(data$discharge_afd,list(data$wy),sum)),frequency=1)
data.yr.afd.mean = ts(as.vector(tapply(data$discharge_afd,list(data$wy),mean)),frequency=1)

#DRIEST YEAR
index_min = which.min(data.yr.afd.sum)
plot(data.mo.afd.mean[(index_min*12+1):(index_min*12+12)], type="o")
#WETTEST YEAR
index_max = which.max(data.yr.afd.sum)
plot(data.mo.afd.mean[(index_max*12+1):(index_max*12+12)], type="o")
#HIGHEST DAILY AVERAGE DISCHARGE
index_discharge = which.max(data.yr.afd.mean)
plot(data.mo.afd.mean[(index_discharge*12+1):(index_discharge*12+12)], type="o")

plot(stl(data.mo.afd.mean,s.window="periodic"))
plot(acf(data.mo.afd.mean))

#FLOODS
plot(data$discharge_cfs)
threshold = 800
abline(h=threshold, col="red")

data$flood <- data$discharge_cfs >= threshold
data.yr.flood.sum <- ts(as.vector(tapply(data$flood, list(data$wy),sum)), start=1907)
print(sum(data.yr.flood.sum >= 100)/length(data.yr.flood.sum))