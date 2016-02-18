install.load <- function(package.name)
{
if (!require(package.name, character.only=T)) install.packages(package.name)
library(package.name, character.only=T)
}
install.load("rjson")
install.load("shiny")
install.load("rhandsontable")
install.load("markdown")
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
