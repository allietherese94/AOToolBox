readLayoutJson <- function(dat)
{
	varToTable <- function(dat1)
	{
	varsrownb <- max(unlist(lapply(dat1, function(v) length(v$data))))
	res <- matrix(nrow=varsrownb, ncol=0)
	for (i in 1:length(dat1))
	{
		temp <- data.frame(c(dat1[[i]]$data,rep("", varsrownb-length(dat1[[i]]$data))))
		names(temp) <- dat1[[i]]$name
		res <- cbind(res, temp)
	}
	res
	}
	
	dayvars <- varToTable(dat$day_variables)
	focalvars <- varToTable(dat$focal_variables)
	scanvars <- varToTable(dat$scan_variables)
	pincodes <- data.frame(users=unlist(dat$pin_codes), pin_codes=names(dat$pin_codes))
	
	return(list( pincodes,dayvars, focalvars, scanvars, list(version=dat$version, default_focal_duration=dat$time_variables$default_focal_duration, default_scan_interval=dat$time_variables$default_scan_interval, focal_starts_with_scan=dat$time_variables$focal_starts_with_scan, previous_scan_settings_saved=dat$save_previous_scan_settings, background_tapped=dat$background_tap$tapped, background_untapped=dat$background_tap$untapped, scene_width_meters=dat$scene_width_meters, scene_initial_zoom_scale=dat$scene_initial_zoom_scale, map_scene_width_meters=dat$map_scene_width_meters, map_scene_initial_zoom_scale=dat$map_scene_initial_zoom_scale, physical_contact_threshold_meters=dat$physical_contact_threshold, scan_alert=dat$scan_alert, default_scan_compass_setting=dat$scan_compass, default_map_mode_setting=dat$map_mode)))
}

createLayoutJSON <- function(temp)
{

short <- function(x){
as.character(unlist(x))
}
res <- list()
res$version <- short(temp[[5]]$version)
res$day_variables <- list()	
for (i in 1:ncol(temp[[2]]))
{
	res$day_variables[[i]] <- list()
	res$day_variables[[i]]$name <- names(temp[[2]])[i]
	res$day_variables[[i]]$data <- as.character(na.omit(temp[[2]][,i]))
	res$day_variables[[i]]$data <- res$day_variables[[i]]$data[res$day_variables[[i]]$data!="" & res$day_variables[[i]]$data!="NA"]
}
res$focal_variables <- list()	
for (i in 1:ncol(temp[[3]]))
{
	res$focal_variables[[i]] <- list()
	res$focal_variables[[i]]$name <- names(temp[[3]])[i]
	res$focal_variables[[i]]$data <- as.character(na.omit(temp[[3]][,i]))
	res$focal_variables[[i]]$data <- res$focal_variables[[i]]$data[res$focal_variables[[i]]$data!="" & res$focal_variables[[i]]$data!="NA"]

}
res$scan_variables <- list()	
for (i in 1:ncol(temp[[4]]))
{
	res$scan_variables[[i]] <- list()
	res$scan_variables[[i]]$name <- names(temp[[4]])[i]
	res$scan_variables[[i]]$data <- as.character(na.omit(temp[[4]][,i]))
	res$scan_variables[[i]]$data <- res$scan_variables[[i]]$data[res$scan_variables[[i]]$data!="" & res$scan_variables[[i]]$data!="NA"]

}
res$pin_codes <- list()
for (i in 1:nrow(temp[[1]])) res$pin_codes[[i]] <- as.character(temp[[1]][i,1])
names(res$pin_codes) <- temp[[1]][,2]

res$background_tap <- list()
res$background_tap$tapped <- short(temp[[5]]$background_tapped)
res$background_tap$untapped <- short(temp[[5]]$background_untapped)
res$time_variables <- list()
res$time_variables$default_focal_duration <- as.numeric(short(temp[[5]]$default_focal_duration))
res$time_variables$default_scan_interval <- as.numeric(short(temp[[5]]$default_scan_interval))
res$time_variables$focal_starts_with_scan <- as.logical(short(temp[[5]]$focal_starts_with_scan))
res$save_previous_scan_settings <- as.logical(short(temp[[5]]$previous_scan_settings_saved))
res$scene_initial_zoom_scale <- as.numeric(short(temp[[5]]$scene_initial_zoom_scale))
res$map_scene_initial_zoom_scale <- as.numeric(short(temp[[5]]$map_scene_initial_zoom_scale))
res$scene_width_meters <- as.numeric(short(temp[[5]]$scene_width_meters))
res$map_scene_width_meters <- as.numeric(short(temp[[5]]$map_scene_width_meters))
res$physical_contact_threshold <- as.numeric(short(temp[[5]]$physical_contact_threshold_meters))
res$scan_alert <- as.logical(short(temp[[5]]$scan_alert))
res$scan_compass <- as.logical(short(temp[[5]]$default_scan_compass_setting))
res$map_mode <- as.logical(short(temp[[5]]$default_map_mode_setting))
return(toJSON(res))
}


tableToList <- function(tt)
{
	if(ncol(tt)==2)
	{
		temp1 <- data.frame(as.character(unlist(tt[-1,-1])))
		names(temp1) <- as.character(tt[1,2])
		return(temp1)
	} else if (ncol(tt)>2){
		temp1 <- tt[-1,][,-1]
		temp2 <- character(0)
		for (i in 2:ncol(tt)) temp2 <- c(temp2, as.character(tt[1,i]))
		names(temp1) <- temp2
		return(temp1)
	} else {
		return(list())
		}
}



