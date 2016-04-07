get.dailyUSGS <- function(site,sensor,start,end){
  # Function to pull USGS INFO

  ### site - is number
  ### sensor - is number INCLUDING LEADING ZEROS!
  ### format start and end as "YYYY/MM/DD"
  
  #http://waterdata.usgs.gov/nwis/dv?cb_00060=on&format=rdb&site_no=11335000&referred_module=sw&period=&begin_date=1907-10-01&end_date=2015-09-30
  
  data <- read.table(paste("http://waterdata.usgs.gov/nwis/dv?cb_",sensor,"=on&format=rdb&site_no=", site,"&referred_module=sw&period=&begin_date=",start,"&end_date=", end, sep=""),
                     header=T, sep="", quote="'", skip=0, colClasses = c("factor", "factor", "factor", "factor", "factor"))
  print('DONE')
}