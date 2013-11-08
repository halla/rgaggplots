library(ggplot2)
library(scales)
library(plyr)

dayHourPlotData <- function(x) UseMethod("dayHourPlotData")
dayHourPlotData.rgaConfig <- function (cfg) {
  getData(cfg, "ga:visits,ga:adsenseRevenue,ga:pageviews", "ga:date,ga:dayOfWeek,ga:hour,ga:deviceCategory")
}

# dimensions = "ga:date,ga:dayOfWeek,ga:hour,ga:deviceCategory", 
#dayHourPlot <- function(x) UseMethod("dayHourPlot")
dayHourPlot <- function(data, metricColName, metricColTitle) {  
  colIdx <- names(data) %in% c(metricColName)
  data$theMetric <- data[,colIdx]
  
  data <- ddply(
    data, ~ hour + dayOfWeek + deviceCategory,
    summarise, theMetric = mean(theMetric)
  )

  data$dayOfWeek <- factor(
    data$dayOfWeek,
    levels = c(0:6),
    labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
    ordered = TRUE
  )
  
  qplot(
    x = hour,
    y = theMetric,
    ylab = metricColTitle,
    data = data,
    facets = ~ dayOfWeek,
    fill = deviceCategory,
    geom = "area",
    position = "stack",
    group = deviceCategory
  )

}

