getData <- function(cfg, metrics, dimensions) UseMethod("getData")
rgaConfig <- function(rga, ids, date_start, date_end) {
	
	s <- structure(list(
		ids = ids, 
		rga = rga, 
		date_start=date_start, 
		date_end = date_end
		), class = "rgaConfig")
	s
}


getData.rgaConfig <- function (cfg, metrics, dimensions) {

	cfg$rga$getData(
	    cfg$ids, 
	    cfg$date_start,
	    cfg$date_end, 
	    metrics = metrics, 
	    dimensions = dimensions, 
	    sort = "", 
	    filters = "", 
	    segment = "", 
	    start = 1, 
	    max = 10000,
	    batch = TRUE
	  )  

}
  
