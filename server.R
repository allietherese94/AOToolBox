# Define server logic for random distribution application
shinyServer(function(input, output, session) {

###########################################
################1st tab
dyadicInput <- reactive({
   	dyadic <- input$dyadic
    if (is.null(dyadic))
      return(NULL)
    read.csv(dyadic$datapath)
  })
scanInput <- reactive({
   	scan <- input$scan
    if (is.null(scan))
      return(NULL)
    read.csv(scan$datapath)
  })
soloInput <- reactive({
   	solo <- input$solo
    if (is.null(solo))
      return(NULL)
    read.csv(solo$datapath)
  })	
foodInput <- reactive({
   	food <- input$foods
    if (is.null(food))
      return(NULL)
    read.csv(food$datapath)
  })
  
textInput <- reactive({
	version <- input$version
	return(version)
})

observeEvent(input$link_to_foods, {
  newvalue <- "Foods"
  updateTabsetPanel(session,"panels2",newvalue)
})

observeEvent(input$link_to_dyad, {
  newvalue <- "Dyadic"
  updateTabsetPanel(session,"panels2",newvalue)
})

observeEvent(input$link_to_scan, {
  newvalue <- "Scan"
  updateTabsetPanel(session,"panels2",newvalue)
})

observeEvent(input$link_to_solo, {
  newvalue <- "Solo"
  updateTabsetPanel(session,"panels2",newvalue)
})

observeEvent(input$link_to_animals, {
  newvalue <- "Create list of study animals"
  updateTabsetPanel(session,"panels",newvalue)
})

observeEvent(input$link_to_grp, {
  newvalue <- "Group composition"
  updateTabsetPanel(session,"panels3",newvalue)
})
  
dataOutput1 <- eventReactive(input$run, {
    if(is.null(dyadicInput()) | is.null(scanInput()) | is.null(soloInput()) | textInput()=="vX.X"){
			return(NULL)
			} else 	if (is.null(foodInput())) {
			return(prepareBehaviorsJson(dyadicInput(), scanInput(), soloInput(),textInput()))
			} else {
			allBehaviorsTables <- dyadicScanSolo(dyadicInput(), scanInput(), soloInput(), foodInput())
			return(prepareBehaviorsJson(allBehaviorsTables$dyadic.all, allBehaviorsTables$scan.all, allBehaviorsTables$solo.all, textInput()))
			}
})

output$text1 <- renderText({
	if(is.null(dataOutput1())){return(NULL)}
		return("DONE !")	
		})


output$downloadBehaviorsJson <- downloadHandler(
    filename = function() { 
		 paste('behaviors.json') 
	 },
    content = function(file) {
    	writeLines(dataOutput1(), con=file)
    }
  )

###########################################
################2nd tab
compoInput <- reactive({
   	compo <- input$compo
    if (is.null(compo))
      return(NULL) else {
    return(prepareGroupCompo(read.csv(compo$datapath)))
    }
})

output$text2 <- renderText({
	if(is.null(compoInput())) return(NULL) else return("DONE")
})

output$downloadAnimalsJson <- downloadHandler(
    filename = function() {
		 paste('animals.json') 
	 },
    content = function(file) {
    	writeLines(compoInput(), con=file)
    }
)

###########################################
################3rd tab

values <-  reactiveValues()

dataOutput2 <- eventReactive(input$template, {
  if(input$run3=="upl" & is.null(dataOutput3())){
    return(NULL)
  } else if (input$run3=="upl" & !is.null(dataOutput3())){
    return(dataOutput3())
  } else {
    return(readLayoutJson(fromJSON(file="layout_info_default.json")))
  }
})

dataOutput3 <- reactive({
  if(is.null(input$layout)){
    return(NULL)
  } else {
    return(readLayoutJson(fromJSON(paste(readLines(input$layout$datapath, warn=F), collapse=""))))
  }
})


dataPinLayout = reactive({
    if(is.null(dataOutput2())) return(NULL)
    if(is.null(input$layoutPin)){
    	temp = dataOutput2()
    	MAT <- temp[[1]]
    } else {
    	MAT=hot_to_r(input$layoutPin)
    }
     values[["pinLayout"]] = MAT
     return(MAT)
  })

dataOptionsLayout = reactive({  
    if(is.null(dataOutput2())) return(NULL)
    if(is.null(input$layoutOptions)){
    	temp = dataOutput2()
    	MAT <- as.matrix(data.frame(settings=names(temp[[5]]), values=unlist(temp[[5]]), stringsAsFactors=F))[-1,]
    } else {
    	MAT=hot_to_r(input$layoutOptions)
    }
     values[["optionsLayout"]] = MAT
     return(MAT)
  })
  
dataDaysLayout = reactive({
    if(is.null(dataOutput2())) return(NULL)
    if(is.null(input$layoutDays)){
    	temp = dataOutput2()
    	mat1 <- as.data.frame(rbind(names(temp[[2]]), as.matrix(temp[[2]])))
       	MAT <- data.frame(variable.index=c("variable name:", 1:(nrow(mat1)-1)), mat1)
    } else {
    	MAT=data.frame(hot_to_r(input$layoutDays))
    	MAT[,1] <- c("variable name:", 1:(nrow(MAT)-1))
    	
    }
     values[["daysLayout"]] = MAT
     return(MAT)
  })
  
dataFocalLayout = reactive({  
    if(is.null(dataOutput2())) return(NULL)
    if(is.null(input$layoutFocal)){
    	temp = dataOutput2()
    	mat1 <- as.data.frame(rbind(names(temp[[3]]), as.matrix(temp[[3]])))
       	MAT <- data.frame(variable.index=c("variable name:", 1:(nrow(mat1)-1)), mat1)
    } else {
    	MAT=hot_to_r(input$layoutFocal)
    	MAT[,1] <- c("variable name:", 1:(nrow(MAT)-1))
    }
     values[["focalLayout"]] = MAT
     return(MAT)
  })

dataScanLayout = reactive({  
    if(is.null(dataOutput2())) return(NULL)
    if(is.null(input$layoutScan)){
    	temp = dataOutput2()
    	mat1 <- as.matrix(rbind(names(temp[[4]]), as.matrix(temp[[4]])), dimnames=NULL)
       	MAT <- data.frame(variable.index=c("variable name:", 1:(nrow(mat1)-1)), mat1)
      	#names(MAT) <- c(" ",LETTERS[1:(ncol(MAT)-1)])
    } else {
    	MAT=hot_to_r(input$layoutScan)
    	MAT[,1] <- c("variable name:", 1:(nrow(MAT)-1))
    }
     values[["scanLayout"]] = MAT
     return(MAT)
  })

output$layoutOptions <- renderRHandsontable({
    MAT = dataOptionsLayout()
       if (!is.null(MAT)) {
        return(rhandsontable(MAT, usesTypes=F, rowHeaders=1:nrow(MAT)) %>%
      hot_table(highlightCol = FALSE, highlightRow = TRUE,
            allowRowEdit = FALSE,
            columnSorting = FALSE,exportToCsv = TRUE) %>%
            hot_col(col="settings", readOnly=TRUE)
    )
    }
  })

output$layoutPin <- renderRHandsontable({
    MAT = dataPinLayout()
       if (!is.null(MAT)) {
      return(rhandsontable(MAT, useTypes=F, rowHeaders=1:nrow(MAT)) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE,
            columnSorting = FALSE, exportToCsv = TRUE) %>%
            hot_context_menu(allowRowEdit=TRUE, allowColEdit=FALSE)
       )
    }
  })
  
output$layoutDays <- renderRHandsontable({
    MAT = dataDaysLayout()
       if (!is.null(MAT)) {
      return(rhandsontable(MAT, useTypes=F,  rowHeaders=NULL, colHeaders=c(" ",LETTERS[1:(ncol(MAT)-1)])) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE,
            columnSorting = FALSE, exportToCsv = TRUE) %>%
            hot_context_menu(allowRowEdit=TRUE, allowColEdit=TRUE)
       )
    }
  })
  
output$layoutFocal <- renderRHandsontable({
    MAT = dataFocalLayout()
       if (!is.null(MAT)) {
      return(rhandsontable(MAT, useTypes=F, rowHeaders=NULL, colHeaders=c(" ",LETTERS[1:(ncol(MAT)-1)])) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE,
            columnSorting = FALSE, exportToCsv = TRUE) %>%
            hot_context_menu(allowRowEdit=TRUE, allowColEdit=TRUE)
       )
    }
  })

output$layoutScan <- renderRHandsontable({
    MAT = dataScanLayout()
       if (!is.null(MAT)) {
      return(rhandsontable(MAT, useTypes=F, rowHeaders=NULL, colHeaders=c(" ",LETTERS[1:(ncol(MAT)-1)])) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE,
            columnSorting = FALSE, exportToCsv = TRUE) %>%
            hot_context_menu(allowRowEdit=TRUE, allowColEdit=TRUE)     
       )
    }
  })

versionTextInput <- reactive({
	version <- input$versionLayout
	return(version)
})
  

output$TextPin <- renderText({
	if (!(is.null(dataOutput2()) & is.null(dataOutput3()))){
		"Enter the pin codes corresponding to the different users. Use right-click to #add/delete rows."
	}
})




output$TextOptions <- renderText({
	if (!is.null(dataOutput2())){
		"Options: fill in the second column of the following table, making sure you respect data types (eg, booleans must be TRUE or FALSE, number must not be characters)."
	}
})

#output$TextOptions <- renderText({
#	dataOptionsLayout()[3,2]
#	values[["optionsLayout"]][5,2]
#})

output$TextDays <- renderText({
	if (!is.null(dataOutput2())){
		"Here, list the variables you want to collect at the begining of each session. Use right-click to add/delete rows and columns."
	}
})

output$TextFocal <- renderText({
	if (!is.null(dataOutput2())){
		"Here, list the variables you want to record at the begining of each Focal. Use right-click to add/delete rows and columns."
	}
})

output$TextScan <- renderText({
	if (!is.null(dataOutput2())){
		"Here, list the global variables you want to record during each scan. Do not include individual-specific variables (they should already be included in behaviors.json)."
	}
})

output$downloadLayoutJson <- downloadHandler(
    filename = function() { 
    	if(versionTextInput()!="vX.X"){
		 paste('layout_info.json') 
		 } else {
		 	'error.txt'
		 }
	 },
    content = function(file) {
    if(versionTextInput()!="vX.X"){
    	temp <- list()
    	temp[[1]] <- values[["pinLayout"]]    
    temp[[2]] <- tableToList(values[["daysLayout"]])
    	temp[[3]] <- tableToList(values[["focalLayout"]])
    temp[[4]] <- tableToList(values[["scanLayout"]])
	
	optionnames <- values[["optionsLayout"]][,1]
	optionvalues<- as.character(values[["optionsLayout"]][,2])
	
	temp[[5]] <- data.frame(values=c(versionTextInput(), optionvalues))
	
	temp[[5]] <- as.data.frame(t(as.matrix(temp[[5]])))
	names(temp[[5]]) <- c("version", optionnames)
	
    writeLines(createLayoutJSON(temp), con=file)
    #write.csv(temp[[5]], file, row.names=FALSE)
} else writeLines("Empty file! Make sure you've entered the version of your layout!", con=file)

}
)
###########################################
################4th tab
json.output.file.input <- reactive({
   if (is.null(input$json.output.file))
      return(NULL)
    readLines(input$json.output.file$datapath, warn=F)
  })
  behaviors.json.input <- reactive({
    if (is.null(input$behaviors.json))
      return(NULL)
    readLines(input$behaviors.json$datapath, warn=F)
  })
  layout_info.json.input <- reactive({
    if (is.null(input$layout_info.json))
      return(NULL)
    readLines(input$layout_info.json$datapath, warn=F)
  })

	dataOutput <- reactive({
		if(is.null(json.output.file.input()) | is.null(behaviors.json.input()) | is.null(layout_info.json.input())) {return(NULL)} else 
		jsonOutputConversion(json.output.file.input(), behaviors.json.input(), layout_info.json.input())
		})
		
	output$list_focals.csv <- renderText({
	if(is.null(dataOutput())) return(NULL)
		"list_focals.csv"
		})
    output$list_behaviors.csv <- renderText({
    	if(is.null(dataOutput())) return(NULL)
    "list_behaviors.csv"
    	})
    	output$list_scans.csv <- renderText({
    	if(is.null(dataOutput())) return(NULL)
    "list_scans.csv"
    	})
    output$list_background_taps.csv <- renderText({
    	if(is.null(dataOutput())) return(NULL)
    "list_background_taps.csv"
    	})
    output$list_texts.csv <- renderText({
    	if(is.null(dataOutput())) return(NULL)
    "list_texts.csv"
    	})	
		
	output$table1 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$focalsTable
		}, include.rownames=F)
	
	output$table2 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$behaviorsTable
		}, include.rownames=F)

	output$table3 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$scansTable
		}, include.rownames=F)

	output$table4 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$backgroundTapsTable
		}, include.rownames=F)
		
	output$table5 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$commentsTable
		}, include.rownames=F)
	
	output$downloadFocalsTable <- downloadHandler(
    filename = function() { 
		 paste('list_focals.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$focalsTable, file, row.names=F)
    }
  )
	output$downloadBehaviorsTable <- downloadHandler(
    filename = function() { 
		 paste('list_behaviors.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$behaviorsTable, file, row.names=F)
    }
  )
	output$downloadScansTable <- downloadHandler(
    filename = function() { 
		 paste('list_scans.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$scansTable, file, row.names=F)
    }
  )
  	output$downloadBackgroundTapsTable <- downloadHandler(
    filename = function() { 
		 paste('list_background_taps.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$backgroundTapsTable, file, row.names=F)
    }
  )
   	output$downloadCommentsTapsTable <- downloadHandler(
    filename = function() { 
		 paste('list_texts.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$commentsTable, file, row.names=F)
    }
  )
  
})
