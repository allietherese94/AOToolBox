require(shiny)
require(rjson)
require(rhandsontable)
require(markdown)
source("prepare_behaviorsJson.R")
source("prepare_foodsDyadicScanSolo.R")
source("jsonOutputConversion.R")
source("prepare_animalsJSON.R")
source("prepare_layoutInfoJSON.R")
includesFOOD <- function(dat)
{
	temp <- dat=="_FOOD"
	temp[is.na(temp)] <- FALSE
	if(sum(temp)>0) return(TRUE) else return(FALSE)
}
