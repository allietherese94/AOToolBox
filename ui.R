shinyUI(fluidPage(

  ##Application title
  br(),
  includeMarkdown("rmarkdown/WelcomeAO.Rmd"),
	br(),
  br(),
  br(),
	tabsetPanel(
	id = "panels",
	  tabPanel("What is Animal Observer?", includeMarkdown("rmarkdown/AnimalObserver.Rmd")),
    tabPanel("Create focal/scan data structure file",
    br(),
    sidebarLayout(
  
    # Sidebar with a slider input
    sidebarPanel(
    fileInput('dyadic', '1. Upload social behavior (focal) csv file',
                accept=c('text/csv', 
								 'text/comma-separated-values,text/plain', 
								 '.csv')),
	fileInput('scan', '2. Upload activity (scan) csv file',
                accept=c('text/csv',
								 'text/comma-separated-values,text/plain', 
								 '.csv')),
	fileInput('solo', '3. Upload self-directed/health csv file',
                accept=c('text/csv',
								 'text/comma-separated-values,text/plain', 
								 '.csv')),
	fileInput('foods', '4. Upload optional list of food items (csv file)',
                accept=c('text/csv',
								 'text/comma-separated-values,text/plain', 
								 '.csv')),
	textInput("version", label = '5. Enter protocol version:', value = "vX.X"),
	hr(),
    actionButton("run", label = "Run", icon=icon("play")),
	downloadButton('downloadBehaviorsJson', 'Download behaviors.json')
    ),
    mainPanel(
      textOutput("text1"),
      tabsetPanel(id="panels2",
                  
                  tabPanel("Instructions", includeMarkdown("rmarkdown/Create_structure.Rmd")),
                  tabPanel("Dyadic"),
                  tabPanel("Scan"),
                  tabPanel("Solo"),
                  tabPanel("Foods"),
                  tabPanel("View protocol")
    )
    ))),
    tabPanel("Create list of study animals",
    br(),
    sidebarLayout(

    # Sidebar with a slider input
    sidebarPanel(
    helpText("Upload list of animals for each study group"),
    fileInput('compo', 'Upload group composition csv file',
                accept=c('text/csv', 
								 'text/comma-separated-values,text/plain', 
								 '.csv')),
	hr(),
	downloadButton('downloadAnimalsJson', 'Download animals.json')
    ),
    mainPanel(
      tabsetPanel(id="panels3",
      tabPanel("Instructions", includeMarkdown("rmarkdown/Create_animals_list.Rmd")),
      tabPanel("Group composition")),
      textOutput("text2")
    )
    )
    ),
	################################################
	################################################
	tabPanel("Create global variables file",
	br(),
	
    sidebarLayout(
    # Sidebar with a slider input
    sidebarPanel(
    #helpText("Upload file"),
    
      fileInput('layout', '1. Upload your layout_info.json file (if you do not have one yet, skip this step and select "template" below)'),
      radioButtons("run3", "2. Choose whether you want to edit the uploaded file or use the default template",
                   c("uploaded file" = "upl",
                     "template" = "tpl")),
      
      actionButton("template", label = "Start editing",  icon=icon("play")),
      br(),br(),
    textInput("versionLayout", label = '3. Enter protocol version:', value = "vX.X"),
	hr(),
	downloadButton('downloadLayoutJson', 'Download layout_info.json')
    ),
    mainPanel(
    textOutput("TextPin"),
    rHandsontableOutput("layoutPin"),
	br(),    
	br(), 
    textOutput("TextOptions"),
    rHandsontableOutput("layoutOptions"),
	br(),    
	br(),    
	textOutput("TextDays"),
    rHandsontableOutput("layoutDays"),
	br(),    
	br(),    
    textOutput("TextFocal"),
    rHandsontableOutput("layoutFocal"),
    br(),    
	br(),    
    textOutput("TextScan"),
    rHandsontableOutput("layoutScan"),
    br(),    
	br(),
	br(),    
	br(),
	br(),    
	br(),
	br(),    
	br(),
	br(),    
	br(),
	br(),    
	br()
    )
    )
    ),
	################################################
	################################################
	  tabPanel("Additional customizations", includeMarkdown("rmarkdown/Customizations.Rmd")),
    tabPanel("Convert collected data to csv",
    br(),
   
    sidebarLayout(

    # Sidebar with a slider input
    sidebarPanel(
    #helpText("Upload collected data"),
    fileInput('json.output.file', '1. Upload "Username_Date_Time.json"'),
	fileInput('behaviors.json', '2. Upload "behaviors.json"'),
    fileInput('layout_info.json', '3. Upload "layout_info.json"'),           
      downloadButton('downloadFocalsTable', 'Download Focal table'),
      downloadButton('downloadBehaviorsTable', 'Download Behavior table'),
	  downloadButton('downloadScansTable', 'Download Scan table'),
	  downloadButton('downloadBackgroundTapsTable', 'Download BackgroundTap table'),
	  downloadButton('downloadCommentsTapsTable', 'Download Comment table')
    ),
    mainPanel(
      tabsetPanel(id="panels3",
          tabPanel("Instructions", includeMarkdown("rmarkdown/Convert_datatocsv.Rmd"))),
		textOutput("list_focals.csv"),
		tableOutput("table1"),
		br(),
		textOutput("list_behaviors.csv"),
		tableOutput("table2"),
		br(),
		textOutput("list_scans.csv"),
		tableOutput("table3"),
		br(),
		textOutput("list_background_taps.csv"),
		tableOutput("table4"),
		br(),
		textOutput("list_texts.csv"),
		tableOutput("table5"),
		br()
)
)
)
)
)
)