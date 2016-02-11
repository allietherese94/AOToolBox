
jsonOutputConversion <- function(json.output.file, behaviors.json, layout_info.json)
{
####path to behaviors.json
behav <- fromJSON(paste(behaviors.json, collapse=""))
####path to layout_info.json
layout <- fromJSON(paste(layout_info.json, collapse=""))
####path to sample output data file
dat <- fromJSON(paste(json.output.file, collapse=""))


#jsonList <- behav$solo
getListHeaders <- function(jsonList){
temp <- names(unlist(jsonList))
temp2 <- unlist(strsplit(temp, split="name"))
unique(unlist(strsplit(temp2, split="[.]")))
}

#################get the behavior list here:
behaviorHeaders <- getListHeaders(behav$dyadic)
#################get the scan activity list here:
scanHeaders <- getListHeaders(behav$scan)
#################get the self-directed behavior list here:
selfHeaders <- getListHeaders(behav$solo)

#################get the layout variables
scanVars <- unlist(lapply(layout$scan_variables, function(v) v$name))
focalVars <- unlist(lapply(layout$focal_variables, function(v) v$name))
dayVars <- unlist(lapply(layout$day_variables, function(v) v$name))


#################list_focals
focalsTable <- matrix(nrow=0, ncol=15+length(dayVars)+length(focalVars))
colnames(focalsTable) <- c(
	"session_start_timeStamp", 
	"session_end_timeStamp",  
	"device_ID",
	"layout_info_json_version",
	"behaviors_json_version",
	"pin_code_name",
	"gps_on",
	"compass_on",
	"map_mode_on",
	"group_ID",
	dayVars, 
	"focal_start_timeStamp", 
	"focal_end_timeStamp", 
	"focal_set_duration", 
	"focal_set_scan_interval",
	focalVars,
	"focal_individual_ID"
	)
	
NAcheck <- function(x){
	ifelse (is.null(x), NA,x)
}

for (i in 1:length(dat$data$sessions)){
	session <- dat$data$sessions[[i]]
	if(length(session$focal)>0){
	for (j in 1:length(session$focal)){
		focal <- session$focal[[j]]
		dayVarsTemp <- unlist(session$details)
		dayVarsTemp <- dayVarsTemp [-grep(names(dayVarsTemp), pattern="Group ID")]
		focalVarsTemp <- unlist(focal$details)
		focalsTable <- rbind(focalsTable, as.character(c(
		NAcheck(session$arrival_time),
		NAcheck(session$departure_time),
		NAcheck(session$device_ID),
		NAcheck(session$layout_info_JSON_file_ID),
		NAcheck(session$behaviors_JSON_file_ID),
		NAcheck(session$pin_name),
		NAcheck(session$gps_on),
		NAcheck(session$map_mode_on),
		NAcheck(session$compass_on),
		NAcheck(session$details$'Group ID'),
		dayVarsTemp,
		NAcheck(focal$start_time),
		NAcheck(focal$end_time),
		NAcheck(focal$duration),
		NAcheck(focal$scan_interval),
		focalVarsTemp,
		NAcheck(focal$animal_id))))
		}
	}
}

#################
behaviorsTable <- matrix(nrow=0, ncol=22+length(dayVars)+length(focalVars)+length(behaviorHeaders)+length(selfHeaders))
colnames(behaviorsTable) <- c(
	"session_start_timeStamp", 
	"session_end_timeStamp",  
	"device_ID",
	"layout_info_json_version",
	"behaviors_json_version",
	"pin_code_name",
	"gps_on",
	"compass_on",
	"map_mode_on",
	"group_ID",
	dayVars, 
	"focal_start_timeStamp", 
	"focal_end_timeStamp", 
	"focal_set_duration", 
	"focal_set_scan_interval",
	focalVars,
	"focal_individual_ID", 
	"behavior_timeStamp", 
	"actor", 
	"subject",
	 behaviorHeaders, selfHeaders,
	"latitude", 
	"longitude",
	"gps_horizontal_precision",
	"altitude"
	)

for (i in 1:length(dat$data$sessions)){
	session <- dat$data$sessions[[i]]
	if(length(session$focal)>0){
	for (j in 1:length(session$focal)){
		focal <- session$focal[[j]]
		if(length(focal$behaviors)>0){
			for (k in 1:length(focal$behaviors)){
				#print(paste(i,j,k))
				behavior <- focal$behaviors[[k]]
				dayVarsTemp <- unlist(session$details)
				dayVarsTemp <- dayVarsTemp [-grep(names(dayVarsTemp), pattern="Group ID")]
				focalVarsTemp <- unlist(focal$details)
				behaviorDetailsTemp <- behavior$details
				behaviorDetailsTemp2 <- character(length(c(behaviorHeaders, selfHeaders)))
				behaviorDetailsTemp2[match(names(unlist(behaviorDetailsTemp)), c(behaviorHeaders, selfHeaders))] <- unlist(behaviorDetailsTemp)
				behaviorsTable <- rbind(behaviorsTable, as.character(c(
				NAcheck(session$arrival_time),
				NAcheck(session$departure_time),
				NAcheck(session$device_ID),
				NAcheck(session$layout_info_JSON_file_ID),
				NAcheck(session$behaviors_JSON_file_ID),
				NAcheck(session$pin_name),
				NAcheck(session$gps_on),
				NAcheck(session$map_mode_on),
				NAcheck(session$compass_on),
				NAcheck(session$details$'Group ID'),
				dayVarsTemp,
				NAcheck(focal$start_time),
				NAcheck(focal$end_time),
				NAcheck(focal$duration),
				NAcheck(focal$scan_interval),
				focalVarsTemp,
				NAcheck(focal$animal_id),
				NAcheck(behavior$timestamp),
				NAcheck(behavior$actor),
				NAcheck(behavior$subject),
				behaviorDetailsTemp2,
				NAcheck(behavior$lat),
				NAcheck(behavior$lon),
				NAcheck(behavior$gpsPrecision),
				NAcheck(behavior$alt)
				)))
				}
			}
		}
	}
}

##############################
scansTable <- matrix(nrow=0, ncol=25+length(dayVars)+length(focalVars)+length(scanHeaders)+length(scanVars))
colnames(scansTable) <- c(
	"session_start_timeStamp", 
	"session_end_timeStamp",  
	"device_ID",
	"layout_info_json_version",
	"behaviors_json_version",
	"pin_code_name",
	"gps_on",
	"compass_on",
	"map_mode_on",
	"group_ID",
	dayVars, 
	"focal_start_timeStamp", 
	"focal_end_timeStamp", 
	"focal_set_duration", 
	"focal_set_scan_interval",
	focalVars,
	"focal_individual_ID",
	"timeStamp",
	scanVars,
	"scanned_individual_ID",
	scanHeaders,
	"x_position",
	"y_position",
	"physical_contact_threshold",
	"latitude", 
	"longitude",
	"gps_horizontal_precision",
	"altitude",
	"compass_bearing"
)

for (i in 1:length(dat$data$sessions)){
	session <- dat$data$sessions[[i]]
	if(length(session$focal)>0){
	for (j in 1:length(session$focal)){
		focal <- session$focal[[j]]
		if(length(focal$scans)>0){
			for (k in 1:length(focal$scans)){
				scan <- focal$scans[[k]]
				for(m in 1:length(focal$scans[[k]]$observations))
					{
					observation <- focal$scans[[k]]$observations[[m]]
					dayVarsTemp <- unlist(session$details)
					dayVarsTemp <- dayVarsTemp [-grep(names(dayVarsTemp), pattern="Group ID")]
					focalVarsTemp <- unlist(focal$details)
					scanDetailsTemp <- unlist(scan$details)
					observationDetailsTemp <- observation$details
					observationDetailsTemp2 <- character(length(c(scanHeaders)))
					observationDetailsTemp2[match(names(unlist(observationDetailsTemp)), scanHeaders)] <- unlist(observationDetailsTemp)
					scansTable <- rbind(scansTable, as.character(c(
					NAcheck(session$arrival_time),
					NAcheck(session$departure_time),
					NAcheck(session$device_ID),
					NAcheck(session$layout_info_JSON_file_ID),
					NAcheck(session$behaviors_JSON_file_ID),
					NAcheck(session$pin_name),
					NAcheck(session$gps_on),
					NAcheck(session$map_mode_on),
					NAcheck(session$compass_on),
					NAcheck(session$details$'Group ID'),
					dayVarsTemp,
					NAcheck(focal$start_time),
					NAcheck(focal$end_time),
					NAcheck(focal$duration),
					NAcheck(focal$scan_interval),
					focalVarsTemp,
					NAcheck(focal$animal_id),
					NAcheck(scan$timestamp),
					scanDetailsTemp,
					NAcheck(observation$actor),
					observationDetailsTemp2,
					NAcheck(observation$x_delta),
					NAcheck(observation$y_delta),
					layout$physical_contact_threshold,
					NAcheck(scan$lat),
					NAcheck(scan$lon),
					NAcheck(scan$gpsPrecision),
					NAcheck(scan$alt),
					NAcheck(scan$compassBearing))))
					}
				}
			}
		}
	}
}

##out_of_viewData #background tap time and date, background tap action, background tap latitude, background tap longitude,
backgroundTapsTable <- matrix(nrow=0, ncol=21+length(dayVars)+length(focalVars))
colnames(backgroundTapsTable) <- c(
	"session_start_timeStamp", 
	"session_end_timeStamp",  
	"device_ID",
	"layout_info_json_version",
	"behaviors_json_version",
	"pin_code_name",
	"gps_on",
	"compass_on",
	"map_mode_on",
	"group_ID",
	dayVars, 
	"focal_start_timeStamp", 
	"focal_end_timeStamp", 
	"focal_set_duration", 
	"focal_set_scan_interval",
	focalVars,
	"focal_individual_ID",
	"timeStamp",
	"description",
	"latitude", 
	"longitude",
	"gps_horizontal_precision",
	"altitude"
	)

for (i in 1:length(dat$data$sessions)){
	session <- dat$data$sessions[[i]]
	if(length(session$focal)>0){
		for (j in 1:length(session$focal)){
			focal <- session$focal[[j]]
			if (length(focal$backgroundTaps)>0){
				for (k in 1:length(focal$backgroundTaps)){
					backgroundTap <- focal$backgroundTaps[[k]]
					dayVarsTemp <- unlist(session$details)
					dayVarsTemp <- dayVarsTemp [-grep(names(dayVarsTemp), pattern="Group ID")]
					focalVarsTemp <- unlist(focal$details)
					backgroundTapsTable <- rbind(backgroundTapsTable, as.character(c(
					NAcheck(session$arrival_time),
					NAcheck(session$departure_time),
					NAcheck(session$device_ID),
					NAcheck(session$layout_info_JSON_file_ID),
					NAcheck(session$behaviors_JSON_file_ID),
					NAcheck(session$pin_name),
					NAcheck(session$gps_on),
					NAcheck(session$map_mode_on),
					NAcheck(session$compass_on),
					NAcheck(session$details$'Group ID'),
					dayVarsTemp,
					NAcheck(focal$start_time),
					NAcheck(focal$end_time),
					NAcheck(focal$duration),
					NAcheck(focal$scan_interval),
					focalVarsTemp,
					NAcheck(focal$animal_id),
					NAcheck(backgroundTap$timestamp),
					NAcheck(backgroundTap$text),
					NAcheck(backgroundTap$lat),
					NAcheck(backgroundTap$lon),
					NAcheck(backgroundTap$gpsPrecision),
					NAcheck(backgroundTap$alt)
					)))
				}
			}
		}
	}
}


#######commentsData
commentsTable <- matrix(nrow=0, ncol=21+length(dayVars)+length(focalVars))
colnames(commentsTable) <- c(
	"session_start_timeStamp", 
	"session_end_timeStamp",  
	"device_ID",
	"layout_info_json_version",
	"behaviors_json_version",
	"pin_code_name",
	"gps_on",
	"compass_on",
	"map_mode_on",
	"group_ID",
	dayVars, 
	"focal_start_timeStamp", 
	"focal_end_timeStamp", 
	"focal_set_duration", 
	"focal_set_scan_interval",
	focalVars,
	"focal_individual_ID",
	"comment_timeStamp",
	"comment_text",
	"latitude", 
	"longitude",
	"gps_horizontal_precision",
	"altitude"
	)

for (i in 1:length(dat$data$sessions)){
	session <- dat$data$sessions[[i]]
	if(length(session$focal)>0){
		for (j in 1:length(session$focal)){
			focal <- session$focal[[j]]
			if (length(focal$text)>0){
				for (k in 1:length(focal$text)){
					text <- focal$text[[k]]
					dayVarsTemp <- unlist(session$details)
					dayVarsTemp <- dayVarsTemp [-grep(names(dayVarsTemp), pattern="Group ID")]
					focalVarsTemp <- unlist(focal$details)
					commentsTable <- rbind(commentsTable, as.character(c(
					NAcheck(session$arrival_time),
					NAcheck(session$departure_time),
					NAcheck(session$device_ID),
					NAcheck(session$layout_info_JSON_file_ID),
					NAcheck(session$behaviors_JSON_file_ID),
					NAcheck(session$pin_name),
					NAcheck(session$gps_on),
					NAcheck(session$map_mode_on),
					NAcheck(session$compass_on),
					NAcheck(session$details$'Group ID'),
					dayVarsTemp,
					NAcheck(focal$start_time),
					NAcheck(focal$end_time),
					NAcheck(focal$duration),
					NAcheck(focal$scan_interval),
					focalVarsTemp,
					NAcheck(focal$animal_id),
					NAcheck(text$timestamp),
					NAcheck(text$text),
					NAcheck(text$lat),
					NAcheck(text$lon),
					NAcheck(text$gpsPrecision),
					NAcheck(text$alt)
					)))
				}
			}
		}
	}
}
return(list(focalsTable= focalsTable, behaviorsTable= behaviorsTable, scansTable=scansTable, backgroundTapsTable=backgroundTapsTable, commentsTable=commentsTable))
}

