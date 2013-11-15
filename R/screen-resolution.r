
parse_screen_resolution <- function(resoString) {
	resoList <- strsplit(resoString, 'x')
	df <- ldply(resoList, 
		function(x) data.frame(as.integer(x[1]), as.integer(x[2])))
	names(df) <- c('x', 'y')
	df

}

twitter_bootstrap_factors <- function(df) {
	cut(df$x, c(-1,0,480,768,980, 1200, 9999), labels=c("Unknown", "Mobile", "Phones2tablets", "Portrait tablet", "Default", "Large"))

}

orientation <- function(df) {
	fact <- as.factor(df$x >= df$y)
	levels(fact) <- c("Portrait", "Landscape")
	fact

}

prepare_reso_data <- function(data) {
	dfReso <- parse_screen_resolution(data$screenResolution)
	transform(data, 
		x = dfReso$x,
		y = dfReso$y,
		resoClass = twitter_bootstrap_factors(dfReso),
		orientation = orientation(dfReso)
		)	
}


ctrByResolutionData <- function(cfg) {
	getData(cfg, "ga:adsenseCTR", "ga:date,ga:screenResolution")
}

# metrics = "ga:adsenseRevenue", 
# dimensions = "ga:date,ga:deviceCategory", 
metricByOrientation <- function(data, rolldays, metricColName, metricColTitle, fn) {  
	colIdx <- names(data) %in% c(metricColName)
  	data$theMetric <- data[,colIdx]

  data <- ddply(
    data, ~ date + orientation,
    function(x) c(theMetric = fn(x$theMetric))
  )


  data_all <- ddply(
    data, .(date), 
    function(x) c(theMetric = fn(x$theMetric)))

           
   adsense2ts <- function() {
     landscape <- data[data$orientation=="Landscape",]
     portrait <- data[data$orientation=="Portrait",]
     
     
     ts_ctr = zoo(cbind(data_all$theMetric, 
                         landscape$theMetric,
                         portrait$theMetric
                         ),
                   as.Date(data_all$date))
     colnames(ts_ctr) <- c( "All", "Landscape", "Portrait")
     ts_ctr
   }
   
	ts <- adsense2ts()
	plotRollingTsMetric(ts, rolldays, metricColTitle)
  
}

metricByResolution <- function(data, rolldays, metricColName, metricColTitle, fn) { 	
	colIdx <- names(data) %in% c(metricColName)
  	data$theMetric <- data[,colIdx]

	data_all <- ddply(
		data, .(date),
	    function(x) c(theMetric = fn(x$theMetric))
	)	
	data <- ddply(
		data, ~ date + resoClass,
		function(x) c(theMetric = fn(x$theMetric))
	)
           
   adsense2ts <- function() {
     mobile <- data[data$resoClass=="Mobile",]
     mobiletablet <- data[data$resoClass=="Phones2tablets",]
     tablet <- data[data$resoClass=="Portrait tablet",]
     default <- data[data$resoClass=="Default",]
     large <- data[data$resoClass=="Large",]
     
     ts_ctr = zoo(cbind(data_all$theMetric, 
                         mobile$theMetric,
                         mobiletablet$theMetric,
                         tablet$theMetric,
                         default$theMetric,
                         large$theMetric),
                   as.Date(data_all$date))
     colnames(ts_ctr) <- c("All", "Mobile", "Phones2tablets", "Portrait tablet", "Default", "Large")
     ts_ctr
   }
   
  ts <- adsense2ts()
  plotRollingTsMetric(ts, rolldays, metricColTitle)
  
}

plotRollingTsMetric <- function(ts, rolldays, metricColTitle) {
	date_start <- min(index(ts))
  	date_end <- max(index(ts))
 
 	lab_main <- paste(metricColTitle  ," per day, ", rolldays, "day running average" )
 	(p <- autoplot(rollapply(ts, rolldays, mean, align="right"), facet=NULL, main=lab_main) ) + 
	xlab('') +
   	coord_cartesian(xlim = c(date_start, date_end), ylim = c(0, max(ts, na.rm=TRUE))) +
   	##scale_x_date(breaks="1 month",  labels = date_format("%b")) +
   	theme(legend.text=element_text(size=14)) +
   	theme(legend.title=element_text(size=14))
 
}
