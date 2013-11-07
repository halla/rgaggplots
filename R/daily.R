library(scales)
library(plyr)


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