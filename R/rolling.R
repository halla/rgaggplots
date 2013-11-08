library('ggplot2')
library('zoo')
library('xts')
library('scales')
library('sqldf')


revenueByDevice <- function(data, rolldays) {
  start.date <- min(data$date)  
  end.date <- max(data$date)
 
  date_start <- as.Date(start.date, "%Y-%m-%d")
  date_end <- as.Date(end.date, "%Y-%m-%d")
  
  data_all <- sqldf('select date, sum(adsenseRevenue) as adsenseRevenue from data group by date')           
           
   adsense2ts <- function() {
     desktop <- data[data$deviceCategory=="desktop",]
     mobiili <- data[data$deviceCategory=="mobile",]
     tablet <- data[data$deviceCategory=="tablet",]  
     
     ts_revenue = zoo(cbind(data_all$adsenseRevenue, 
                         desktop$adsenseRevenue,
                         tablet$adsenseRevenue, 
                         mobiili$adsenseRevenue),
                   as.Date(desktop$date))
     colnames(ts_revenue) <- c( "All", "Desktop", "Tablet", "Mobile")
     ts_revenue
   }
   
   plotRevenueByDevice <- function(ts, rolldays) {
     
     lab_main <- paste("AdSense revenue $ per day, ", rolldays, "day running average" )
     (p <- autoplot(rollapply(ts, rolldays, mean, align="right"), facet=NULL, main=lab_main) ) + 
       xlab('') +
       coord_cartesian(xlim = c(date_start, date_end), ylim = c(0, max(ts, na.rm=TRUE))) +
       scale_x_date(breaks="1 month",  labels = date_format("%b")) +
       theme(legend.text=element_text(size=14)) +
       theme(legend.title=element_text(size=14))
     
   }
  ts <- adsense2ts()
  plotRevenueByDevice(ts, rolldays)
  
}