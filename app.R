#Load needed packages and R scripts
library(shiny)
library(ggplot2)
library(hrbrthemes)
library(plotly)
library(readr)
library(shinydashboard)
library(data.table)
library(dplyr)
library(SplinesUtils)
library(shinycssloaders)
source("R scripts/Absolute Max Function.R")
source("R scripts/Absolute Max Function 2.0.R")
source("R scripts/Local Max Function.R")

#Set up shiny dashboard
#Create header
header <- dashboardHeader(title = "Solar Cell Research")

#Create sidebar
sidebar <- dashboardSidebar(
    
    #Create menu
    sidebarMenu(
        
        #Create Welcome tab
        menuItem("Welcome", tabName = "introduction"),
        hr(),
        
        #File Upload button
        fileInput("file", 
                  h3("File Upload", align = "center"), 
                  accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
        
        #Radio buttons
        radioButtons("sep", h3("Separator"), 
                     choices = c(Tab = "\t", Comma = ","), selected = "\t"),
        hr(),
        
        #Create select box for file format options
        selectInput("select", label = h3("Plot Download Format"), 
                    choices = list("SVG" = 1, "JPEG" = 2, "PNG" = 3), 
                    selected = 1),
        hr(),
        
        
        #Create remaining menu items 
        menuItem("Dataset Heatmaps", tabName = "datasetDash"),
        menuItem("Relative/Wavelength Intensity Plots", tabName = "relativeDash"),
        menuItem("Dataset Tables", tabName = "tables"),
        menuItem("Dataset Histograms", tabName = "datasethistograms"),
        menuItem("Export AllSpectra", tabName = "smoothSpline"),
        menuItem("About", tabName = "about")
    )
)

#Create body
body <- dashboardBody(
    
    #Create tabs with content inside them
    tabItems(
        
        #Content for "Welcome" tab
        tabItem(tabName = "introduction",
                
                fluidPage(
                    
                    h1("Welcome!"),
                    h2("Click on the any of the tabs for information you need to know!"),
                    
                    #Organize information
                    tabBox(width = 10,
                           
                           #Notes for uploading the file
                           tabPanel("File Format for Upload",
                                    
                                    #Make note to user for what the app is expecting as an input
                                    p("The program assumes the CSV file as the following format:"),
                                    p("1. The first row is the wavelengths."),
                                    p("2. The first two columns are the x and y coordinates."),
                                    p("3. Nothing in the first two boxes."),
                                    p("4. The rest are the intensities."),
                                    strong("Example:"),
                                    br(),
                                    
                                    #Image demonstrating format of CSV file
                                    img(src = "Example Data.PNG", height = 200, width = 350),
                                    hr(),
                                    
                                    #Make note of program being able to accept TSV files
                                    p(strong("Update:"), "The program now accepts files that are in TSV format (tab-
                                      separated values). It assumes the same structure."),
                                    p("You can now choose what the separator is before uploading a file! Underneath
                                      the file upload widget, just choose either tab or comma.")
                                    
                           ),
                           
                           #Note regarding downloading plots.
                           tabPanel("Downloading Plots/Tables",
                                    
                                    p("You are able to download the plots/tables displayed!"),
                                    p("The plots are downloaded in SVG format, while the tables are downloaded in
                                      CSV format."),
                                    p("For Table 2 to appear, you", strong("must"), 
                                    "input two different wavelengths first."),
                                    hr(), 
                                    
                                    #Note what calculation is being done for relative intensity
                                    p(strong("Note:"), "For the relative intensity, it is the ratio of",
                                      HTML(paste0("&lambda;",tags$sub(1))) ,"intensity to",
                                      HTML(paste0("&lambda;",tags$sub(2))) ,"intensity"),
                                    
                                    p(strong("Update:"), "The plots can now be downloaded in JPEG and PNG format.
                                      Under the \"Plot Download Format\" section, click on the box and 
                                      select the format you want!")),
                           
                           #Note recent features
                           tabPanel("More Features",
                                    
                                    #Interactive Peak Wavelength Histogram
                                    h3("Peak/Local Peak Wavelength Interactive Histograms"),
                                    p("The distribution of the peak wavelengths can be now viewed in the tab labeled
                                      'Dataset Histograms'. Make sure to upload the file first!"),
                                    p(strong("Update:"), "The Peak Wavelength Interactive Histogram now consists of
                                      the Local Peak Wavelength Histogram. You can select to display one 
                                      histogram or both."),
                                    p("There is also a separate slider for the Local Peak Wavelength Histogram and
                                      is labeled accordingly. When both histograms are present, available information
                                      is based on where you choose to click at."),
                                    p("When one histogram is displayed, only information for that histogram will be
                                      displayed. Give it a try!"),
                                    p("Class width for each histogram is underneath the slider.
                                      The units are in nanometers."),
                                    img(src = "Peak and Local Peak Hist.PNG", height = 520, width = 900),
                                    hr(),
                                    
                                    #Smoothing Spline Interpolation Output
                                    h3("Smoothing Spline View"),
                                    p("Now for any coordinate, the scatterplot of the normal intensities with the
                                      smoothing spline line graphed can be viewed!"),
                                    p("Just simply enter the row number, and the program will generate the plot."),
                                    p("Where is the row number? To view it, click on the tile of interest.
                                      Then the first number to the left is it."),
                                    p("Here is an example:"),
                                    img(src = "Example Row Number.png", height = 200, width = 350),
                                    p("The row number is 1, which would then be inputted in the box labeled 'Smooth
                                      Spline Interpolation Display Input'. Now press 'Go!' and click on the 'Export
                                      AllSpectra' tab to see the graph!"),
                                    hr(),
                                    
                                    #Interactive Smooth Spline
                                    h3("Interactive Smoothing Spline"),
                                    p("The smoothing spline plot is now interactive! You can click and drag over a
                                      portion of the plot, and the program will display what is inside the region with
                                      the window margins specified."),
                                    p("In other words, you can zoom into the plot with the 
                                      result presented underneath it."),
                                    p("Additionally, the region specified can be moved around by clicking and
                                      holding the specified region and placing it over a different area."),
                                    p("The local extrema for the smoothing spline plot are now displayed. When the
                                      selected region covers any of these points, the program displays
                                      their values."),
                                    p("Here is an example:"),
                                    img(src = "Zoom Spline.PNG", height = 520, width = 870),
                                    hr(),
                                    
                                    h3("Absolute/Local Max Interactive Histograms"),
                                    p("These histograms display the distribution of the absolute max values and
                                      local max values."),
                                    p("This interactive plot shares the same features as the other one: one or both
                                      histograms can be seen, each histogram has its own slider, class width (which is
                                      unitless) is displayed for each one, and the information underneath is presented
                                      based on which histogram(s) is selected."),
                                    img(src = "Absolute and Local Max Hist.PNG", height = 520, width = 900),
                                    hr(),
                           
                                    #Histogram main and axes title
                                    h3("Histogram Plot Controls"),
                                    p("The controls of the histograms have expanded causing the plot settings to be
                                      divided into three sections: Bin, Title and Axis. For \“Bin Settings\”, the
                                      number of bins for the axis can be controlled through the slider, and any
                                      histogram can be shown/hidden by checking/unchecking the box next to the
                                      sliders. \“Title Settings\” enables you to rename the main and axis titles.
                                      Finally, \“Axis Settings\” controls the location of the tick marks on the
                                      horizontal axis also the distance between each one. There is also a range slider
                                      that controls the horizontal axis’ limits."),
                                    p("In order to see the new names and axis on the plot, you must first press
                                      \“Apply Title/Axis Settings\”. The sliders and checkboxes apply the settings to
                                      the plot immediately, so those do not require you to press the button to be
                                      shown on the plot. You can also show/hide the plot controls by simply checking/
                                      unchecking the box labeled \“Display Plot Controls\”."),
                                    p("Above those lies an alternative method for inputting the number of bins. This
                                      one, however, controls both plots based on the color. There is one for the blue
                                      histograms (Peak Wavelength and Absolute Max Histograms) and one for the green
                                      histograms (Local Peak Wavelength and Local Max Histograms). Type in the number
                                      then press \“Enter\”."),
                                    p(strong("Note:"), "You need to have the plot controls shown for the change to take
                                      place."),
                                    img(src = "Numeric Input Number of Bins.PNG", width = 700),
                                    img(src = "Plot Controls.PNG", height = 450, width = 900),
                                    hr(),
                                    
                                    #Title option for heatmaps
                                    h3("Dataset Heatmaps: Titles Now Optional"),
                                    p("The title for both dataset heatmaps is optional when downloaded."),
                                    p("Simply leave the box unchecked if the title should be omitted. Otherwise,
                                      go ahead and check it out!"),
                                    img(src = "HeatMap Title Box.PNG"),
                                    hr(),
                                    
                                    h3("Dataset Smooth Splines"),
                                    p("This plot contains the smoothing spline for each coordinate in a dataset and
                                      can be generated by pressing \“Show All Smooth Splines\” after uploading the
                                      dataset. The row number associated with a line is displayed by simply bringing
                                      the cursor near the line, and the program shows the result underneath the plot.
                                      If the cursor is not close enough to a line, a message will be shown asking you
                                      to bring the cursor closer to it."),
                                    img(src = "Multiple Smooth Splines.png", height = 520, width = 950)
                                    )
                                
                    )   
                    
                )
        ),
        
        #Content for "Dataset Heatmaps"
        tabItem(tabName = "datasetDash",
                
                #Download button for Normalized Absolute Max Intensity Heat Map
                downloadButton("downloadgraph", "Download Normal Max Intensity Plot"),
                checkboxInput("checkTitle", "Include Title In Download", value = FALSE),
                
                #Output heat map for Normalized Absolute Max Intensity and Information
                plotOutput("normMaxIntHeatMap", click = "click") %>%
                    withSpinner(getOption("spinner.type", 8)),
                verbatimTextOutput("info1"),
                
                #Add border
                hr(),
                
                #Make download button for Peak Wavelength Heat Map
                downloadButton("downloadgraph1", "Download Peak Wavelength Plot"),
                checkboxInput("checkTitle1", "Include Title In Download", value = FALSE),
                
                #Output heat map for Peak Wavelength and Information
                plotOutput("peakWaveHeatMap", click = "click") %>%
                    withSpinner(getOption("spinner.type", 8)),
                verbatimTextOutput("info2")),
        
        #Content for Relative/Wavelength Intensity Plots
        tabItem(tabName = "relativeDash",
                
                fluidRow(
                    column(4,
                           
                           #Make two boxes that accept numeric entries for two wavelength values
                           h3("Wavelength Input", align = "left"),
                           p('Input two distinct wavelengths', align = "left"),
                           
                           numericInput('wavelength1', 
                                        HTML(paste("Enter", paste0("&lambda;",tags$sub(1)))),
                                        value = NULL, min = 0),
                           
                           numericInput('wavelength2',
                                        HTML(paste("Enter", paste0("&lambda;",tags$sub(2)))),
                                        value = NULL, min = 0),
                           
                           #Create Action button for user
                           actionButton("go","Go!")
                           
                           )
                    
                ),
                #Make fluid row for interactive relative intensity map 
                fluidRow(
                    
                    h3(strong(em("Note:")), "Did you change the inputs for one or both wavelengths,
                    but the plot(s) look(s) the same/did not change? Make sure you press \"Go!\"
                    after updating any wavelength values!", align = "center"),
                    
                    
                    
                    column(12,
                           
                           #Make download button for Relative Intensity Heat Map
                           downloadButton("downloadgraph2", "Download Relative Intensity Plot"),
                           
                           #Heat Map plot of Relative Intensity given two values
                           plotOutput("RelIntHeatMap", click = "click1") %>%
                               withSpinner(getOption("spinner.type", 8)),
                           verbatimTextOutput("info3")
                           
                        ),
                    
                    
                    #Make fluid row for interactive intensities for both inputted wavelengths
                    fluidRow(
                        
                        column(6,
                               
                               #Make download button for heat map of inputted wavelength 1 
                               downloadButton("downloadgraph3", "Download Wavelength 1 Intensity Plot"),
                               
                               #Heat map plot for intensities of wavelength 1
                               plotOutput("Wave1IntenHeatMap", click = "click2") %>%
                                   withSpinner(getOption("spinner.type", 8)),
                               verbatimTextOutput("info4"),
                        ),
                        
                        column(6,
                               
                               #Make download button for heat map of inputted wavelength 1 
                               downloadButton("downloadgraph4", "Download Wavelength 2 Intensity Plot"),
                               
                               #Heat map plot for intensities of wavelength 1
                               plotOutput("Wave2IntenHeatMap", click = "click3") %>%
                                   withSpinner(getOption("spinner.type", 8)),
                               verbatimTextOutput("info5")
                               
                        )
                    ),
                )
        ),
        
        #Content for "Dataset Tables" tab
        tabItem(tabName = "tables",
                
                #Make fluid row for both tables
                fluidRow(
                    
                    #Place table recording the values (include relative intensity)
                    column(6,
                           
                           #Display title
                           h2("Table 1"),
                           
                           #Download button for first table (X, Y, Peak wave., Int, Norm. Int.)
                           downloadButton("downloadtable", "Download Table 1"),
                           
                           #Output table regarding the given data set
                           tableOutput('table')
                           
                           ),
                    
                    #Table consisting columns for the output values for given wavelength values
                    column(6,
                           
                           #Display title
                           h2("Table 2"),
                           
                           #Download button for second table (Wavelength 1, Wavelength 2, Relative Intensity)
                           downloadButton("downloadtable1", "Download Table 2"),
                           
                           #Output table regarding inputted wavelengths
                           tableOutput('table1')
                           
                           ),
                )
        ),
        
        #Content for "Dataset Histograms" tab
        tabItem(tabName = "datasethistograms",
                
                fluidRow(
                    
                    column(3,
                           
                           #Create numeric input for number of bins for Peak Wavelength/Absolute Max Histograms
                           numericInput("numberOfBins", "Numeric Input: Number of Bins (Blue Histograms)",
                                        value = 2, min = 1),
                           
                           ),
                    
                    column(1,
                           
                           #Enter button
                           actionButton("go2", "Enter")
                           
                           ),
                    
                    column(3,
                           
                           #Create numeric input for number of bins for Local Peak Wavelength/Local Max Histograms
                           numericInput("numberOfBins1", "Numeric Input: Number of Bins 
                                        (Green Histograms)", value = 2, min = 1),
                           
                           ),
                    
                    column(1,
                           
                           #Enter button
                           actionButton("go3", "Enter")
                           
                    )),
                
                checkboxInput("showPltControl", "Display Plot Controls", value = TRUE),
                conditionalPanel(condition = "input.showPltControl == '1'",
                                 
                                 h3("Bin Settings"),
                                 
                                 fluidRow(
                                     
                                     column(3,
                                            
                                            #Create slider for Peak Wavelength Histogram
                                            uiOutput("slider"),
                                            textOutput("classwidth")
                                            
                                     ),
                                     
                                     column(1,
                                            
                                            #Create a checkbox for Peak wavelength Histogram
                                            checkboxInput("peakcheck", label = "Display Histogram", value = TRUE)
                                            
                                     ),
                                     
                                     column(3,
                                            
                                            #Create slider for Local Peak Wavelength Histogram
                                            uiOutput("slider1"),
                                            textOutput("classwidth1")
                                            
                                     ),
                                     
                                     column(1,
                                            
                                            #Create a checkbox for Local Peak wavelength histogram
                                            checkboxInput("localpeakcheck", label = "Display Histogram", value = TRUE)
                                            
                                     )),
                                 
                                 hr(),
                                 
                                 h3("Title Settings"),
                                 
                                 #Create row for text input
                                 fluidRow(
                                     
                                     column(3,
                                            
                                            #Text input: Histogram Title
                                            textInput("textInp", "Histogram Title", "Peak/Local Peak Wavelength Histogram")
                                            
                                     ),
                                     
                                     column(3,
                                            
                                            #Text input: Horizontal Axis Title
                                            textInput("textInp1", "Horizontal Axis Title", "Wavelength")
                                            
                                     ),
                                     
                                     column(3,
                                            
                                            #Text input: Vertical axis Title
                                            textInput("textInp2", "Vertical Axis Title", "Frequency")
                                            
                                     ),
                                     
                                     column(1,
                                            
                                            #Action button: Apply changes
                                            actionButton('applyChange', "Apply Title/Axis Settings")
                                            
                                     )),
                                 
                                 hr(),
                                 
                                 h3("Axis Settings"),
                                 
                                 fluidRow(
                                     
                                     column(1,
                                            
                                            uiOutput("lower")
                                    
                                     ),
                                    
                                     column(1,
                                     
                                            uiOutput("upper")
                                                   
                                     ),
                                     
                                     column(1,
                                            
                                            uiOutput("increment")
                                            
                                     ),
                                     
                                     
                                     column(3,
                                            
                                            #Create slider for the horizontal axis range of the Peak/Local Peak Histogram
                                            uiOutput("sliderx")
                                            
                                     )
                                     
                                     )
                                 
                                 
                                 ),
                
                #Download button for histogram
                downloadButton("downloadHist","Download Histogram Plot"),
                
                #Output Peak Wavelength Histogram and Information
                plotOutput("peakhist", click = "click4") %>%
                    withSpinner(getOption("spinner.type", 8)),
                verbatimTextOutput("info6"),
                hr(),
                
                checkboxInput("showPltControl1", "Display Plot Controls", value = TRUE),
                conditionalPanel(condition = "input.showPltControl1 == '1'",
                                 
                                 h3("Bin Settings"),
                                 
                                 fluidRow(
                                     
                                     column(3,
                                            
                                            #Create slider for Absolute Max Histogram
                                            uiOutput("slider2"),
                                            textOutput("classwidth2")
                                            
                                     ),
                                     
                                     column(1,
                                            
                                            #Create a checkbox for Peak wavelength Histogram)
                                            checkboxInput("absmaxcheck", label = "Display Histogram", value = TRUE)
                                            
                                     ),
                                     
                                     column(3,
                                            
                                            #Create slider for Local Max Histogram
                                            uiOutput("slider3"),
                                            textOutput("classwidth3")
                                            
                                     ),
                                     
                                     column(1,
                                            
                                            #Create a checkbox for Peak wavelength Histogram)
                                            checkboxInput("localmaxcheck", label = "Display Histogram", value = TRUE)
                                            
                                     )),
                                 
                                 hr(),
                                 
                                 h3("Title Settings"),
                                 
                                 #Create row for text input
                                 fluidRow(
                                     
                                     column(3,
                                            
                                            #Text input: Histogram Title
                                            textInput("textInp3", "Histogram Title", "Absolute/Local Max Values Histogram")
                                            
                                     ),
                                     
                                     column(3,
                                            
                                            #Text input: Horizontal Axis Title
                                            textInput("textInp4", "Horizontal Axis Title", "Value")
                                            
                                     ),
                                     
                                     column(3,
                                            
                                            #Text input: Vertical axis Title
                                            textInput("textInp5", "Vertical Axis Title", "Frequency")
                                            
                                     ),
                                     
                                     column(1,
                                            
                                            #Action button: Apply changes
                                            actionButton('applyChange1', "Apply Title/Axis Settings")
                                            
                                     )),
                                 
                                 hr(),
                                 
                                 h3("Axis Settings"),
                                 
                                 fluidRow(
                                     
                                     column(1,
                                            
                                            uiOutput("lower1")
                                            
                                     ),
                                     
                                     column(1,
                                            
                                            uiOutput("upper1")
                                            
                                     ),
                                     
                                     column(1,
                                            
                                            uiOutput("increment1")
                                            
                                     ),
                                     
                                     column(3,
                                            
                                            #Create slider for the horizontal axis range of the Absolute/Local Max Histogram
                                            uiOutput("sliderx1")
                                            
                                     ))
                                 
                                 ),
                
                #Download button for Absolute/local max histogram
                downloadButton("downloadHist1","Download Histogram Plot"),
                
                #Create histogram for absolute/local max histograms and information
                plotOutput("abslochist", click = "click5") %>%
                    withSpinner(getOption("spinner.type", 8)),
                verbatimTextOutput("info7")
                
                
        ),
        
        #Content for "Export Allspectra" tab
        tabItem(tabName = "smoothSpline",
                actionButton("go4", "Show All Smooth Splines"),
                
                #Render Plot
                plotOutput("datasetSplines", hover = hoverOpts('SmoothHover', delay = 200)) %>%
                    withSpinner(getOption("spinner.type", 8)),
                uiOutput("tooltip"),
                
                fluidRow(
                    
                    column(4,
                            
                           #Make numeric entry for row number to view smoothing spline interpolation graph with scatterplot
                           h4("Smooth Spline Interpolation Display Input", align = "left"),
                           numericInput('rowIndex', "Enter row number" , value = NULL, min = 0),
                           
                           #Create action button for it
                           actionButton("go1", "Go!"),
                           hr()
                    
                        )),
                
                
                #Download button for smoothing spline plot
                downloadButton("downloadSpectrum", "Download Spectrum Plot"),
                
                #Output smoothing spline plot
                plotOutput("smoothLine", 
                           brush = brushOpts("smoothPlot_brush", resetOnNew = TRUE)) %>%
                    withSpinner(getOption("spinner.type", 8)),
                
                #Output zoomed smoothing spline plot
                plotOutput("zoomsmooth"),
                verbatimTextOutput("info8")
                
        ),
        
        #Content for "About" tab
        tabItem(tabName = "about",
                h1("About the Application"),
                h3("It imports a cathodoluminescence dataset, which is obtained from images of a
                   photovoltaic cell. It can visualize the characterization of the luminescence of
                   solar cells, distribution of the wavelengths associated with the luminescense,
                   the relationship between both luminescence and the wavelengths and more."), 
                h3("The application can also export the plots/tables."),
                h3("It is a product of the collaboration between the Mathematics Department and
                   the Engineering Techonology Department at Texas A&M University-Central Texas. It
                   is meant to enhance the Engineering Technology Department's research about solar
                   cells through statistics."),
                h3("The application is still updated from time to time.")
        )
    ))


# Define UI for application that outputs heat maps and tables
ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body)


# Define server logic required to output heat maps and tables
server <- function(input, output, session) {
    
    #Increase file size input
    options(shiny.maxRequestSize = 30*1024^2)
    
    
    ###PREPARATION###
    
    #Read uploaded file
    first_df <- reactive({
        
        req(input$file)
        
        #Use the uploaded file and selected separator to build tibble
        read_delim(input$file$datapath, delim = input$sep, col_names = FALSE)

    })
    
    #Use abs_max_func (Absolute Max Function.R) for data frame
    dataset <- eventReactive(input$file$datapath,{
        
        #Compute dataframe
        abs_max_func(first_df())
        
    })
    
    #Get list of all local max values and their corresponding wavelengths for every row
    local_max_list <- eventReactive(input$file$datapath, {
        
        #Determine list of consisting of wavelengths and their corresponding intensities in their respective row
        local_max_func(first_df())
        
    })
    
    #Window margins for zoomsmooth plot
    window_margins <- reactiveValues(x = NULL, y = NULL)
    
    
    ###OBJECTS FOR HISTOGRAMS###
    
    #Create local peak wavelength vector
    localPeakWave <- reactive({
        
        unlist(local_max_list()[1:(length(local_max_list())/2)], use.names = FALSE)
        
    })
    
    #Calculate break points based on lowest & highest peak wavelength values along with number of bins 
    binsPeakWave <- reactive({
        
        seq(min(dataset()[,3]),max(dataset()[,3]), length.out = input$inslider + 1)
    
    })
    
    #Calculate break points based on lowest & highest local peak wavelength values along with number of bins
    binsLocWave <- reactive({
        
        seq(min(localPeakWave()),max(localPeakWave()), length.out = input$inslider1 + 1)
    
    })
    
    #Create local max vector 
    localMax <- reactive({
        
        unlist(local_max_list()[(length(local_max_list())/2 + 1):length(local_max_list())], use.names = FALSE)
    
    })
    
    #Calculate break points
    binsAbsMax <- reactive({
        
        seq(min(dataset()[,5]),max(dataset()[,5]), length.out = input$inslider2 + 1)
    
    })
    
    #Calculate break points
    binsLocMax <- reactive({
        
        seq(min(localMax()),max(localMax()), length.out = input$inslider3 + 1)
        
    })
    
    
    ###ACTION BUTTONS###
    
    #Create table using abs_max_func_2 (Absolute Max Function 2.0.R) function
    bigger_data <- eventReactive(input$go, {
        
        #Combine dataset() with dataframe created by abs_max_func_2 (Absolute Max Function 2.0.R) by column
        cbind(dataset(),abs_max_func_2(first_df(),input$wavelength1,input$wavelength2))
        
    })
    
    #Extract wavelengths from uploaded dataset
    wavelengths <- eventReactive(input$file$datapath, {
        
        #Wavelengths is the first row, first two entries are empty
        unlist(first_df()[1,-c(1,2)], use.names = FALSE)
        
    })
    
    #Get corresponding intensities for inputted row 
    y_values <- eventReactive(input$go1, {
        
        #Find row and omit first two entries (these are the coordinates for the rows)
        regular_y_val <- unlist(first_df()[input$rowIndex + 1, -c(1,2)], use.names = FALSE)
        
        #Normalize them by dividing by the largest intensity value
        regular_y_val/max(regular_y_val)
        
    })
    
    #Create smooth spline function
    splinesmoothfit <- eventReactive(input$go1, {
        
        smooth.spline(x = wavelengths(), y = y_values())
        
    }) 
    
    #Determine absolute max coordinates from inputted row
    abs_max_coordinates <- eventReactive(input$go1, {
        
        #Predict values for peak wavelength
        predicted <- predict(splinesmoothfit(), x = dataset()[input$rowIndex,3])
        
        c(predicted$x, predicted$y)
        
    })
    
    #Get coordinates
    coordinates <- eventReactive(input$go1,{
        
        #Find row and only keep first two entries
        unlist(first_df()[input$rowIndex + 1, c(1,2)], use.names = FALSE)
        
    })
    
    #Create local max matrix
    local_max_points <- eventReactive(input$go1, {
        
        #Extract local max points for given row
        p <- local_max_list()[c(input$rowIndex, input$rowIndex + length(local_max_list())/2)]
        
        #Create matrix by first unlisting p and then convert to a matrix where the first row has wavelengths and second row has local max values
        matrix(unlist(p, use.names = FALSE), nrow = 2, byrow = TRUE)
        
    })
    
    
    ###SLIDER INPUT###
    
    #Slider for horizontal range of Peak/Local Peak Wavelength Histogram
    output$sliderx <- renderUI({
        
        #Calculate min and max values for each histogram. Then use the floor function on the minimums and ceiling function on maximums for integers
        min_both <- floor(min(c(dataset()[,3], localPeakWave())))
        max_both <- ceiling(max(c(dataset()[,3], localPeakWave())))
        min_peak <- floor(min(dataset()[,3]))
        max_peak <- ceiling(max(dataset()[,3]))
        min_local <- floor(min(localPeakWave()))
        max_local <- ceiling(max(localPeakWave()))
        
        #If user checks both boxes, then display both histograms
        if (input$peakcheck & input$localpeakcheck) {
            
            #Create slider based on both histograms
            sliderInput("insliderx", "Horizontal Axis Range", min = min_both, max = max_both, value = c(min_both, max_both), step = (max_both - min_both)/(max(length(dataset()[,3]), length(localPeakWave()))-1))
            
        } else if (input$peakcheck) {
            
            #Create slider based on the peak wavelength histogram 
            sliderInput("insliderx", "Horizontal Axis Range", min = min_peak, max = max_peak, value = c(min_peak, max_peak), step = (max_peak - min_peak)/(length(dataset()[,3])-1))
            
        } else if (input$localpeakcheck) {
            
            #Create slider based on the local peak wavelength histogram
            sliderInput("insliderx", "Horizontal Axis Range", min = min_local, max = max_local, value = c(min_local, max_local), step = (max_local - min_local)/(length(localPeakWave())-1))
            
        }
        
    })
    
    #Slider for Peak Wavelength Histogram 
    output$slider <- renderUI({
        
        input$go2
        
        #Indicate number of bins
        #Lowest number is 1; highest number is how many coordinates there are in the uploaded dataset
        #Default value is 2
        sliderInput("inslider", "Peak Wavelength (Blue)", min = 1, max = length(dataset()[,3]), value = isolate(input$numberOfBins), step = 1)
        
    })
    
    #Slider for Local Peak Wavelength Histogram
    output$slider1 <- renderUI({
        
        input$go3
        
        #Indicate number of bins
        #Lowest number is 1; highest number is how many coordinates there are in the uploaded dataset
        #Default value is 2
        sliderInput("inslider1", "Local Peak Wavelength (Green)", min = 1, max = length(unlist(local_max_list()[1:(length(local_max_list())/2)], use.names = FALSE)), value = isolate(input$numberOfBins1), step = 1)
        
    })
    
    #Slider for horizontal range of Absolute/Local Max Histogram
    output$sliderx1 <- renderUI({
        
        #Calculate min and max values for each histogram. Then use the floor function on the minimums and ceiling function on maximums for integers
        min_both <- floor(min(c(dataset()[,5], localMax())))
        max_both <- ceiling(max(c(dataset()[,5],localMax())))
        min_abs <- floor(min(dataset()[,5]))
        max_abs <- ceiling(max(dataset()[,5]))
        min_local <- floor(min(localMax()))
        max_local <- ceiling(max(localMax()))
        
        #If user checks both boxes, then display both histograms
        if (input$absmaxcheck & input$localmaxcheck) {
            
            #Create slider based on both histograms
            sliderInput("insliderx1", "Horizontal Axis Range", min = min_both, max = max_both, value = c(min_both,max_both), step = (max_both - min_both)/(max(length(dataset()[,5]), length(localMax()))-1))
            
        } else if (input$absmaxcheck) {
            
            #Create slider based on the absolute max histogram 
            sliderInput("insliderx1", "Horizontal Axis Range", min = min_abs, max = max_abs, value = c(min_abs, max_abs), step = (max_abs - min_abs)/(length(dataset()[,5])-1))
            
        } else if (input$localmaxcheck) {
            
            #Create slider based on the local peak wavelength histogram
            sliderInput("insliderx1", "Horizontal Axis Range", min = min_local, max = max_local, value = c(min_local, max_local), step = (max_local - min_local)/(length(localMax())-1))
            
        }
        
    })
    
    #Slider for Absolute Max Histogram
    output$slider2 <- renderUI({
        
        input$go2
        
        #Indicate number of bins
        #Lowest number is 1; highest number is how many elements are in each vector
        #Default value is 2
        sliderInput("inslider2", "Absolute Max (Blue)", min = 1, max = length(dataset()[,5]), value = isolate(input$numberOfBins), step = 1)
        
    })
    
    #Slider for Local Max Histogram
    output$slider3 <- renderUI({
        
        input$go3
        
        #Indicate number of bins
        #Lowest number is 1; highest number is how many elements are in each vector
        #Default value is 2
        sliderInput("inslider3", "Local Max (Green)", min = 1, max = length(unlist(local_max_list()[(length(local_max_list())/2+1):length(local_max_list())], use.names = FALSE)), value = isolate(input$numberOfBins1), step = 1)
        
    })
    
    #Numeric input for minimum value for horizontal axis
    output$lower <- renderUI({
        
        minimum <- floor(min(c(dataset()[,3], localPeakWave())))
        
        numericInput("lowerBound", "Minimum", value = minimum)
        
    })
    
    output$upper <- renderUI({
        
        maximum <- ceiling(max(c(dataset()[,3], localPeakWave())))
        
        numericInput("upperBound", "Maximum", value = maximum)
        
    })
    
    output$increment <- renderUI({
        
        defIncrement <- 5
        
        numericInput("inc", "Increment", value = defIncrement)
        
    })
    
    output$lower1 <- renderUI({
        
        minimum <- floor(min(c(dataset()[,5], localMax())))
        
        numericInput("lowerBound1", "Minimum", value = minimum)
        
    })
    
    output$upper1 <- renderUI({
        
        maximum <- ceiling(max(c(dataset()[,5],localMax())))
        
        numericInput("upperBound1", "Maximum", value = maximum)
        
    })
    
    output$increment1 <- renderUI({
        
        defIncrement <- 0.1
        
        numericInput("inc1", "Increment", value = defIncrement)
        
    })
    
    ###REACTIVE DATA FRAME VALUES FOR HEAT MAP PLOTS/DOWNLOAD BUTTONS###
    
    df_absInt <- reactive({
        
        test <- dataset()
        colnames(test) <- c("X","Y","Wavelength","Intensity", "Normal_Intensity")
        test
        
    })
    
    df_peakWave <- reactive({
        
        test <- dataset()
        colnames(test) <- c("X","Y","Wavelength","Intensity", "Normal Intensity")
        test
        
    })
    
    df_relInt <- reactive({
        
        test <- bigger_data()
        colnames(test) <- c("X","Y","Wavelength","Intensity", "Normal Intensity","Wavelength 1 Intensity","Wavelength 2 Intensity", "Normal Wave 1 Intensity", "Normal Wave 2 Intensity" , "Relative_Intensity")
        test
        
    })
    
    df_wave1 <- reactive({
        
        test <- bigger_data()
        colnames(test) <- c("X","Y","Wavelength","Intensity", "Normal Intensity","Wavelength 1 Intensity","Wavelength 2 Intensity", "Normal_Wave_1_Intensity", "Normal Wave 2 Intensity" , "Relative Intensity")
        test
        
    })
    
    df_wave2 <- reactive({
        
        test <- bigger_data()
        colnames(test) <- c("X","Y","Wavelength","Intensity", "Normal Intensity","Wavelength 1 Intensity","Wavelength 2 Intensity", "Normal Wave 1 Intensity", "Normal_Wave_2_Intensity" , "Relative Intensity")
        test
        
    })
    
    
    ###FUNCTIONS: HEAT MAPS### 
    
    #Normal Intensity Heat Map
    normMaxIntPlot <- function() {
        
        #Make ggplot2 static plot
        ggplot(df_absInt(), aes(X, Y, fill= Normal_Intensity)) + 
            geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Normal Intensity") + scale_fill_gradient(low = "black", high = "red")
        
    }
    
    #Peak Wavelength Heat Map
    peakWavePlot <- function() {
        
        #Make ggplot2 static plot
        ggplot(df_peakWave(), aes(X, Y, fill= Wavelength)) + 
            geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Wavelength") + scale_fill_gradient(low = "black", high = "red")
        
    }
    
    #Relative Intensity Heat Map
    relIntPlot <- function() {
        
        #Make ggplot2 static plot
        ggplot(df_relInt(), aes(X, Y, fill= Relative_Intensity)) + 
            geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Rel. Intensity") + ggtitle(paste("Relative Intensity", paste0("(",input$wavelength1, "/", input$wavelength2,")"),"Plot"))+ scale_fill_gradient(low = "black", high = "red")
        
    }
    
    #Wavelength 1 Normal Intensity Heat Map 
    wave1IntPlot <- function() {
        
        #Make ggplot2 static plot
        ggplot(df_wave1(), aes(X, Y, fill= Normal_Wave_1_Intensity)) + 
            geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Normal Intensity") + ggtitle(paste("Wavelength 1",paste0("(",input$wavelength1,")"),"Intensity Plot")) + scale_fill_gradient(low = "black", high = "red")
        
    }
    
    #Wavelength 2 Normal Intensity Heat Map
    wave2IntPlot <- function() {
        
        #Make ggplot2 static plot
        ggplot(df_wave2(), aes(X, Y, fill= Normal_Wave_2_Intensity)) + 
            geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Normal Intensity") + ggtitle(paste("Wavelength 2",paste0("(",input$wavelength2,")"),"Intensity Plot")) + scale_fill_gradient(low = "black", high = "red")
        
    }
    
    
    ###HEAT MAP PLOTS###
    
    #Normalized Absolute Max Intensity Heat Map
    output$normMaxIntHeatMap <- renderPlot({
        
        #Generate graph
        normMaxIntPlot() + ggtitle("Normal Max Intensity Plot") 
        
    })
    
    #Peak Wavelength Heat Map 
    output$peakWaveHeatMap <- renderPlot({
        
        #Generate graph with title
        peakWavePlot() + ggtitle("Peak Wavelength Plot")
        
    })
    
    #Relative Intensity Heat Map
    output$RelIntHeatMap <- renderPlot({
        
        #Generate graph with title
        relIntPlot()
        
    })
    
    #Intensities for wavelength 1
    output$Wave1IntenHeatMap <- renderPlot({
        
        #Generate graph
        wave1IntPlot()
        
    })
    
    #Intensities for wavelength 2
    output$Wave2IntenHeatMap <- renderPlot({
        
        #Generate graph
        wave2IntPlot()
        
    })
    
    
    ###FUNCTIONS: HISTOGRAMS###
    
    #Peak/Local Peak Wavelength Histogram
    peakLocWavePlot <- function() {
        
        #If user checks both boxes, then display both histograms
        if (input$peakcheck & input$localpeakcheck) {
            
            #Display both histograms
            hist(dataset()[,3], breaks = binsPeakWave(), xaxt = "n", main = NULL, xlab = NULL, ylab = NULL, col = "blue", border = "white", xlim = c(input$insliderx[1], input$insliderx[2]))
            hist(localPeakWave(), breaks = binsLocWave(), xaxt = "n", col = rgb(0,1,0,0.5), border = "white", add = TRUE)
            
        } else if (input$peakcheck) {
            
            #If user checks box for peak wavelength histogram only, then display it
            hist(dataset()[,3], breaks = binsPeakWave(), xaxt = "n", main = NULL, xlab = NULL, ylab = NULL, col = "blue", border = "white", xlim = c(input$insliderx[1], input$insliderx[2]))
            
        } else if (input$localpeakcheck) {
            
            #If user checks box for local peak wavelength histogram only, then display it
            hist(localPeakWave(), breaks = binsLocWave(), xaxt = "n", main = NULL, xlab = NULL, ylab = NULL, col = rgb(0,1,0,0.5), border = "white", xlim = c(input$insliderx[1], input$insliderx[2]))
            
        }
        
        
    }
    
    #Absolute and Local Max Histogram
    absLocMaxPlot <- function() {
        
        #If user checks both boxes, then display both histograms
        if (input$absmaxcheck & input$localmaxcheck) {
            
            hist(dataset()[,5], breaks = binsAbsMax(), xaxt = "n", main = NULL, xlab = NULL, ylab = NULL, col = "blue", border = "white", xlim = c(input$insliderx1[1], input$insliderx1[2]))
            hist(localMax(), breaks = binsLocMax(), xaxt = "n", col = rgb(0,1,0,0.5), border = "white", add = TRUE)
            
        } else if (input$absmaxcheck) {
            
            #If user checks box for absolute max histogram only, then display it
            hist(dataset()[,5], breaks = binsAbsMax(), xaxt = "n", main = NULL, xlab = NULL, ylab = NULL, col = "blue", border = "white", xlim = c(input$insliderx1[1], input$insliderx1[2]))
            
        } else if (input$localmaxcheck) {
            
            #If user checks box for local max histogram only, then display it
            hist(localMax(), breaks = binsLocMax(), xaxt = "n", main = NULL, xlab = NULL, ylab = NULL, col = rgb(0,1,0,0.5), border = "white", xlim = c(input$insliderx1[1], input$insliderx1[2]))
            
        }
        
    }
    
    
    ###DATASET HISTOGRAMS###
    
    #Make histogram representing the distribution of the peak wavelengths 
    output$peakhist <- renderPlot({
        
        #Generate histogram
        peakLocWavePlot()
        
        
        #When action button is pressed, apply changes
        input$applyChange
        isolate({
            
            title(main = input$textInp, xlab = input$textInp1, ylab = input$textInp2)
            axis(1, at = seq(input$lowerBound, input$upperBound, by = input$inc))
            
            })
        
    })
    
    #Make histogram representing the distribution of the absolute/local max values
    output$abslochist <- renderPlot({
        
        #Generate Graph
        absLocMaxPlot()
        
        #When action button is pressed, Apply changes
        input$applyChange1
        isolate({
            
            title(main = input$textInp3, xlab = input$textInp4, ylab = input$textInp5)
            axis(1, at = seq(input$lowerBound1, input$upperBound1, by = input$inc1))
        
            })
        
    })
    
    
    ###REACTIVE VALUES REQUIRED###
    longDf <- eventReactive(input$go4, {
        
        #Convert to matrix
        data <- as.matrix(first_df())
        
        #Get rid of 1st row and 1st 2 columns
        data <- data[-1,-(1:2)]
        
        #Calculate number of columns
        numberOfColumns <- ncol(data)
        numberOfRows <- nrow(data)
        
        #Normalize intensity values for each row
        for (i in 1:numberOfRows) {
            
            data[i,] <- data[i,]/max(data[i,])
            
        }
        
        #Create matrix with 3 columns (wavelength, row number, predicted intensity)
        splineMatrix <- matrix(rep(0, numberOfRows*numberOfColumns*3), ncol = 3)
        colnames(splineMatrix) <- c("x","row_number","y")
        
        #Repeat row index values as many number of columns there are
        splineMatrix[,2] <- rep(1:numberOfRows, each = numberOfColumns)
        
        #Generate sequence from 1 to number of columns
        colSequence <- 1:numberOfColumns
        
        #Make for loop
        #The number of rows will be the iterations needed to retrieve coordinates based on smooth spline
        for (j in 1:numberOfRows){
            
            #Subset the ith row and convert to a vector by unlisting along with
            #getting rid of the names associated with each element.
            jth_row <- unlist(data[j,], use.names = FALSE)
            
            #Make smooth spline with wavelengths on the horizontal axis and the corresponding intensities (normalized)
            #on the vertical axis
            splinesmoothfit <- smooth.spline(x = wavelengths(), y = jth_row)
            
            #Calculate row indexes to replace by shifting and using value of j
            rowIndexes <- colSequence + (j-1) * numberOfColumns
            
            #Update spline_matrix
            splineMatrix[rowIndexes,1] <- splinesmoothfit$x
            splineMatrix[rowIndexes,3] <- splinesmoothfit$y
            
        }
        
        splineDf <- as.data.frame(splineMatrix)
        splineDf
        
    })
    
    ###FUNCTIONS: SMOOTH SPLINE###
    
    #Smooth Spline for all rows
    allSmooth <- function() {
        
        #Generate vector consisting of values 1 to number of rows
        rowIndexes <- unique(longDf()$row_number) 
        
        plot(longDf()$x, main = "Dataset Smooth Spline Lines", xlab = "Wavelength", ylab = "Normal Intensity", type = "n", xlim = c(min(longDf()$x),max(longDf()$x)), ylim = c(0,1))
        
        for (k in rowIndexes) {
            
            lines(x = longDf()$x[longDf()$row_number == k], y = longDf()$y[longDf()$row_number == k],col = "red", lwd = 2)
            
        }
        
    }
    
    #Smooth Spline for specific row
    smoothSplPlot <- function() {
        
        #Plot points for given row on graph
        plot(wavelengths(), y_values(), xlab = "Wavelength", ylab = "Normal Intensity", main = paste("Normal Intensity vs. Wavelength \n X =", coordinates()[1],", Y = ", coordinates()[2]))
        
        #Plot smooth spline line, absolute max point, and local max points
        lines(splinesmoothfit(), col = "red", lwd = 2)
        points(abs_max_coordinates()[1], abs_max_coordinates()[2], pch = 20, col = "blue", cex = 3)
        points(local_max_points()[1,],local_max_points()[2,], pch = 20, col = "green", cex = 3)
        legend("topright", c("Smoothing Spline","Absolute Max", "Local Max"),  lwd = c(2,NA,NA), col = c("red", "blue","green"), pch = c(NA,20,20), pt.cex = c(NA,3,3))
        
    }
    
    
    ###SMOOTH SPLINE INTERPOLATION GRAPHS###

    #Tool tip
    output$tooltip <- renderUI({
        
        req(input$SmoothHover)
        
        verbatimTextOutput("selectedRowNum")
        
    })
    
    #Value of row
    output$selectedRowNum <- renderPrint({
        
        hover <- input$SmoothHover
        
        rowNum <- unique(nearPoints(longDf(), hover, xvar = 'x', yvar = 'y', threshold = 7)$row_number)[1]
        
        if (is.na(rowNum)) {
            print("Bring cursor closer to a line.")
        } else {
            paste("Row Number:", rowNum)
        }
        
    })
    
    #Create smoothing spline lines
    output$datasetSplines <- renderPlot({
        
        allSmooth()
        
    })
    
    #Create smoothing spline line and plot it
    output$smoothLine <- renderPlot({
        
        #Generate graph
        smoothSplPlot()
        
    })
    
    #Create zoomed-in smoothing spline line
    output$zoomsmooth <- renderPlot({
        
        #Plot smooth spline line on graph
        plot(wavelengths(), y_values(), xlab = "Wavelength", ylab = "Normal Intensity", xlim = window_margins$x, ylim = window_margins$y)
        
        #Plot smooth spline line, absolute max point, and local max points
        lines(splinesmoothfit(), col = "red", lwd = 2)
        points(abs_max_coordinates()[1], abs_max_coordinates()[2], pch = 20, col = "blue", cex = 3)
        points(local_max_points()[1,],local_max_points()[2,], pch = 20, col = "green", cex = 3)
        legend("topright", c("Smoothing Spline","Absolute Max", "Local Max"),  lwd = c(2,NA,NA), col = c("red", "blue","green"), pch = c(NA,20,20), pt.cex = c(NA,3,3))
        
    })
    
    #Display effects of brush
    observe({
        
        brush <- input$smoothPlot_brush
        
        if(!is.null(brush)) {
            window_margins$x <- c(brush$xmin, brush$xmax)
            window_margins$y <- c(brush$ymin, brush$ymax)
        } else {
            window_margins$x <- NULL
            window_margins$y <- NULL
        }
        
    })

    
    ###REACTIVE VALUES FOR DOWNLOAD BUTTONS###
    
    #Table 1
    df_csv_t1 <- reactive({
        
        test <- dataset()
        colnames(test) <- c("X","Y","Wavelength","Intensity", "Normal Intensity")
        test
        
    })
    
    #Table 2
    df_csv_t2 <- reactive({
        
        test <- bigger_data()[,-(1:5)]
        colnames(test) <- c(paste("Wavelength 1 Intensity",input$wavelength1), paste("Wavelength 2 Intensity",input$wavelength2), "Wavelength 1 Normal Intensity", "Wavelength 2 Normal Intensity", "Relative Intensity")
        test
        
    })
    
    
    ###DOWNLOAD BUTTONS###
    
    #Download button for heat map of normalized max intensity
    output$downloadgraph <- downloadHandler(
        
        #Allow user to make file name
        filename = function() {
            
            #SVG is selected
            if (input$select == 1) {
                
                #Name file
                paste0("Normal Max Intensity Plot", ".svg")
                
            #JPEG is selected
            } else if (input$select == 2) {
                
                #Name file
                paste0("Normal Max Intensity Plot", ".jpeg")
                
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Name file
                paste0("Normal Max Intensity Plot", ".png")
                
            }
            
        },
        
        #Content for file
        content = function(file){
            
            #SVG is selected
            if (input$select == 1) {
                
                #Make file SVG
                svg(file)
                
            #JPEG is selected
            } else if (input$select == 2) {
                
                #Make file JPEG
                jpeg(file, quality = 100)
                
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Make file PNG
                png(file)
                
            }
            
            #Show graph
            if(!input$checkTitle) {print(normMaxIntPlot())}
            else {print(normMaxIntPlot() + ggtitle("Normal Max Intensity Plot"))}
            
            dev.off()
            
        }
        
    )
    
    #Download button for Peak Wavelength Heat Map Plot
    output$downloadgraph1 <- downloadHandler(
        
        #Allow user to make file name
        filename = function(){
            
            #SVG is selected
            if (input$select == 1) {
                
                #Name file
                paste0("Peak Wavelength Plot", ".svg")
                
            #JPEG is selected
            } else if (input$select == 2) {
                
                #Name file
                paste0("Peak Wavelength Plot", ".jpeg")
                
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Name file
                paste0("Peak Wavelength Plot", ".png")
                
            }
            
        },
        
        #Content for file
        content = function(file){
            
            #SVG is selected
            if (input$select == 1) {
                
                #Make file SVG
                svg(file)
                
            #JPEG is selected
            } else if (input$select == 2) {
                
                #Make file JPEG
                jpeg(file, quality = 100)
                
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Make file PNG
                png(file)
                
            }
            
            if(!input$checkTitle1) {print(peakWavePlot())}
            else {print(peakWavePlot() + ggtitle("Peak Wavelength Plot"))}
            dev.off()    
        }
        
    )
    
    #Download button for Relative Intensity Heat Map Plot
    output$downloadgraph2 <- downloadHandler(
        
        #Allow user to make file name
        filename = function(){
            
            #SVG is selected
            if (input$select == 1) {
                
                #Name file
                paste0("Relative Intensity Plot", ".svg")
                
            #JPEG is selected
            } else if (input$select == 2) {
                
                #Name file
                paste0("Relative Intensity Plot", ".jpeg")
                
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Name file
                paste0("Relative Intensity Plot", ".png")
                
            }
            
        },
        
        #Content for file
        content = function(file){
           
            #SVG is selected
            if (input$select == 1) {
                
                #Make file SVG
                svg(file)
                
            #JPEG is selected
            } else if (input$select == 2) {
                
                #Make file JPEG
                jpeg(file, quality = 100)
                
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Make file PNG
                png(file)
                
            }
            
            print(relIntPlot())
            
            dev.off()    
        }
        
    )
    
    #Download button for Wavelength 1 Intensity Heat Map
    output$downloadgraph3 <- downloadHandler(
        
        #Allow user to make file name
        filename = function(){
            
            #SVG is selected
            if (input$select == 1) {
                
                #Name file
                paste0("Wavelength 1 Intensity Plot", ".svg")
                
            #JPEG is selected
            } else if (input$select == 2) {
                
                #Name file
                paste0("Wavelength 1 Intensity Plot", ".jpeg")
                
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Name file
                paste0("Wavelength 1 Intensity Plot", ".png")
                
            }
            
        },
        
        #Content for file
        content = function(file){
            
            #SVG is selected
            if (input$select == 1) {
                
                #Make file SVG
                svg(file)
                
            #JPEG is selected
            } else if (input$select == 2) {
                
                #Make file JPEG
                jpeg(file, quality = 100)
                
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Make file PNG
                png(file)
                
            }
            
            print(wave1IntPlot())
            
            dev.off()    
        }
        
    )
    
    #Download button for Wavelength 2 Intensity Heat Map
    output$downloadgraph4 <- downloadHandler(
        
        #Allow user to make file name
        filename = function(){
            
            #SVG is selected
            if (input$select == 1) {
                
                #Name file
                paste0("Wavelength 2 Intensity Plot", ".svg")
                
            #JPEG is selected
            } else if (input$select == 2) {
                
                #Name file
                paste0("Wavelength 2 Intensity Plot", ".jpeg")
                
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Name file
                paste0("Wavelength 2 Intensity Plot", ".png")
                
            }
            
        },
        
        #Content for file
        content = function(file){
            
            #SVG is selected
            if (input$select == 1) {
                
                #Make file SVG
                svg(file)
                
            #JPEG is selected
            } else if (input$select == 2) {
                
                #Make file JPEG
                jpeg(file, quality = 100)
                
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Make file PNG
                png(file)
                
            }
            
            print(wave2IntPlot())
            
            dev.off()    
        }
        
    )
    
    #Download button for CSV of Table 1
    output$downloadtable <- downloadHandler(
        
        #Allow user to make file name
        filename = function() {
            
            #Name of file
            paste0("Table 1", ".csv")
            
        },
        
        #Content for file
        content = function(file) {
            
            #Create CSV
            write.csv(df_csv_t1(), file, row.names = FALSE, sep = ',')
            
        }
        
    )
    
    #Download button for CSV of Table 2
    output$downloadtable1 <- downloadHandler(
        
        #Allow user to make file name
        filename = function() {
            
            #Name of file
            paste0("Table 2", ".csv")
            
        },
        
        #Content for file
        content = function(file) {
            
            #Create CSV File
            write.csv(df_csv_t2(), file, row.names = FALSE, sep = ',')
            
        }
        
    )
    
    #Download button for Peak/Local Peak Wavelength histogram
    output$downloadHist <- downloadHandler(
        
        #Allow user to make file name
        filename = function() {
            
            #SVG is selected
            if (input$select == 1) {
                
                #Name file
                paste0("Peak Wavelength Histogram", ".svg")
                
            #JPEG is selected
            } else if (input$select == 2) {
                
                #Name file
                paste0("Peak Wavelength Histogram", ".jpeg")
                
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Name file
                paste0("Peak Wavelength Histogram", ".png")
                
            }
            
        },
        
        #Content for file
        content = function(file){
            
            #SVG is selected
            if (input$select == 1) {
                
                #Make file SVG
                svg(file)
                
            #JPEG is selected
            } else if (input$select == 2) {
                
                #Make file JPEG
                jpeg(file, quality = 100)
                
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Make file PNG
                png(file)
                
            }
            
            peakLocWavePlot()
            title(main = input$textInp, xlab = input$textInp1, ylab = input$textInp2)
            axis(1, at = seq(input$lowerBound, input$upperBound, by = input$inc))
            dev.off()    
        }
        
    )
    
    #Download button for absolute/local max histogram
    output$downloadHist1 <- downloadHandler(
        
        #Allow user to make a file name
        filename = function() {
            
            #SVG is selected
            if (input$select == 1) {
                
                #Name file
                paste0("Absolute and Local Max Histogram", ".svg")
                
            #JPEG is selected
            } else if (input$select == 2) {
                
                #Name file
                paste0("Absolute and Local Max Histogram", ".jpeg")
                
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Name file
                paste0("Absolute and Local Max Histogram", ".png")
                
            }
            
        },
        
        #Content for file
        content = function(file) {
            
            #SVG is selected
            if (input$select == 1) {
                
                #Make file SVG
                svg(file)
                
            #JPEG is selected
            } else if (input$select == 2) {
                
                #Make file JPEG
                jpeg(file, quality = 100)
                
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Make file PNG
                png(file)
                
            }
            
            absLocMaxPlot()
            title(main = input$textInp3, xlab = input$textInp4, ylab = input$textInp5)
            axis(1, at = seq(input$lowerBound1, input$upperBound1, by = input$inc1))
            dev.off()   
        }
        
    )
    
    #Download button for current spectrum plot
    output$downloadSpectrum <- downloadHandler(
        
        #Create file name
        filename = function() {
            
            #SVG is selected
            if (input$select == 1) {
                
                #Name file
                paste0("Spectrum Plot", ".svg")
                
            #JPEG is selected
            } else if (input$select == 2) {
                
                #Name file
                paste0("Spectrum Plot", ".jpeg")
                
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Name file
                paste0("Spectrum Plot", ".png")
                
            }
            
        },
        
        #Content for file
        content = function(file) {
            
            #SVG is selected
            if (input$select == 1) {
                
                #Make file SVG
                svg(file)
                
            #JPEG is selected
            } else if (input$select == 2) {
                
                #Make file JPEG
                jpeg(file, quality = 100)
                
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Make file PNG
                png(file)
                
            }
            
            print(smoothSplPlot())
            
            dev.off()    
        }
        
    )
    
    
    ###REACTIVE VALUES FOR INFO###
    
    df_info12 <- reactive({
        
        test <- dataset()
        colnames(test) <- c("X","Y","Wavelength","Intensity", "Normal Intensity")
        test
        
    })
    
    df_info345 <- reactive({
        
        test <- bigger_data()
        colnames(test) <- c("X","Y","Wavelength","Intensity","Normal Intensity","Wavelength 1 Intensity", "Wavelength 2 Intensity","Normal Wavelength 1 Intensity", "Normal Wavelength 2 Intensity", "Relative Intensity")
        test
        
    })
    
    
    ###OUTPUT INFORMATION###
    
    #Output information for interactive Heat map (normal max int.)
    output$info1 <- renderPrint({
        
        #Display row information
        nearPoints(df_info12()[,-c(3,4)], input$click, threshold = 15, maxpoints = 1)
        
    })
    
    #Output information for interactive heat map (peak wavelength)
    output$info2 <- renderPrint({
        
        #Display row information
        nearPoints(df_info12()[,-c(4,5)], input$click, threshold = 15, maxpoints = 1)
        
    })
    
    #Output information for interactive heat map (Relative Intensity)
    output$info3 <- renderPrint({
        
        #Display row information
        nearPoints(df_info345()[,-c(3,4,5)], input$click1, threshold = 15, maxpoints = 1)
        
    })
    
    #Output Information for interactive heat map (Wavelength 1)
    output$info4 <- renderPrint({
        
        #Display row information
        nearPoints(df_info345()[,-c(3:5,7:9)], input$click2, threshold = 15, maxpoints = 1)
        
    })
    
    #Output Information for interactive heat map (Wavelength 2)
    output$info5 <- renderPrint({
        
        #Display row information
        nearPoints(df_info345()[,-c(3:6,8:9)], input$click3, threshold = 15, maxpoints = 1)
        
    })
    
    #Output Information for interactive peak/local peak wavelength histogram
    output$info6 <- renderPrint({
        
        #If no bin is clicked on
        if(is.null(input$click4$x)) return()
        
        #Create reactive value for current peak wavelength histogram
        z <- hist(dataset()[,3], breaks = binsPeakWave())
        
        #Create reactive value for current local peak wavelength histogram
        y <- hist(localPeakWave(), breaks = binsLocWave())
        
        #Determine indices of limits where mouse is between (peak wavelength histogram)
        lower_lim_ind_peak <- tail(which(z$breaks <= input$click4$x),1)
        higher_lim_ind_peak <- head(which(z$breaks >= input$click4$x),1)
        
        ##Determine indices of limits where mouse is between (local peak wavelength histogram)
        lower_lim_ind_local <- tail(which(y$breaks <= input$click4$x),1)
        higher_lim_ind_local <- head(which(y$breaks >= input$click4$x),1)
        
        #Find the limits for each histogram
        lower_lim_peak <- z$breaks[lower_lim_ind_peak]
        higher_lim_peak <- z$breaks[higher_lim_ind_peak]
        lower_lim_local <- y$breaks[lower_lim_ind_local]
        higher_lim_local <- y$breaks[higher_lim_ind_local]
        
        #Determine the count
        peak_count <- z$counts[lower_lim_ind_peak]
        local_count <- y$counts[lower_lim_ind_local]
        
        #Create vectors for each histogram consisting of lower and upper limits, midpoint, and frequency
        peak_info <- c(lower_lim_peak, higher_lim_peak, z$mids[lower_lim_peak < z$mids & z$mids < higher_lim_peak], peak_count)
        local_info <- c(lower_lim_local, higher_lim_local, y$mids[lower_lim_local < y$mids & y$mids < higher_lim_local], local_count)
        
        #Create dataframe
        df <- cbind(peak_info, local_info)
        
        #If both checkboxes are marked
        if (input$peakcheck & input$localpeakcheck) {
            
            #If the user clicks within the range of either one or both histograms
            if(input$click4$x < min(c(binsPeakWave(),binsLocWave())) | input$click4$x > max(c(binsPeakWave(),binsLocWave()))) {
                
                #If user clicks out of the range for both histograms
                print("Click on a bin that is within the range of either histogram.")
                
                
            } else if (input$click4$x < min(binsPeakWave()) | input$click4$x > max(binsPeakWave())) {
                
                #If the user clicks on a green bin that is out of the range of the blue histogram
                df[,1] <- NA * integer(nrow(df))
                colnames(df) <- c("Peak Wavelength", "Local Peak Wavelength")
                rownames(df) <- c("Lower Bin Limit", "Higher Bin Limit", "Midpoint", "Frequency")
                df
                
            } else if (input$click4$x < min(binsLocWave()) | input$click4$x > max(binsLocWave())) {
                
                #If the user clicks on a blue bin that is out of the range of the green histogram
                df[,2] <- NA * integer(nrow(df))
                colnames(df) <- c("Peak Wavelength", "Local Peak Wavelength")
                rownames(df) <- c("Lower Bin Limit", "Higher Bin Limit", "Midpoint", "Frequency")
                df
                
            } else {
                
                colnames(df) <- c("Peak Wavelength", "Local Peak Wavelength")
                rownames(df) <- c("Lower Bin Limit", "Higher Bin Limit", "Midpoint", "Frequency")
                df
                
            }
            
        } else if (input$peakcheck) {
            
            #If user only checks box for peak wavelength histogram
            #If click is outside of range
            if (input$click4$x < min(binsPeakWave()) | input$click4$x > max(binsPeakWave())) {
                
                print("Click on a bin within the range of the histogram")
                
            } else {
                
                #Subset the first column
                df <- df[,1]
                names(df) <- c("Lower Bin Limit", "Higher Bin Limit", "Midpoint", "Frequency")
                df
                
            }
            
        } else if (input$localpeakcheck) {
            
            #If user only checks box for local peak wavelength histogram
            #If click is outside of range
            if (input$click4$x < min(binsLocWave()) | input$click4$x > max(binsLocWave())) {
                
                print("Click on a bin within the range of the histogram")
                
            } else {
                
                #Subset the first column
                df <- df[,2]
                names(df) <- c("Lower Bin Limit", "Higher Bin Limit", "Midpoint", "Frequency")
                df
                
            }
            
            
        }
        
    })
    
    #Output Information for interactive absolute/local max values histogram
    output$info7 <- renderPrint({
        
        #If no bin is clicked on
        if(is.null(input$click5$x)) return()
        
        #Create reactive value for current absolute max histogram
        z <- hist(dataset()[,5], breaks = binsAbsMax())
        
        #Create reactive value for current local max histogram
        y <- hist(localMax(), breaks = binsLocMax())
        
        #Determine indices of limits where mouse is between (peak wavelength histogram)
        lower_lim_ind_abs <- tail(which(z$breaks <= input$click5$x),1)
        higher_lim_ind_abs <- head(which(z$breaks >= input$click5$x),1)
        
        ##Determine indices of limits where mouse is between (local peak wavelength histogram)
        lower_lim_ind_local <- tail(which(y$breaks <= input$click5$x),1)
        higher_lim_ind_local <- head(which(y$breaks >= input$click5$x),1)
        
        #Find the limits for each histogram
        lower_lim_abs <- z$breaks[lower_lim_ind_abs]
        higher_lim_abs <- z$breaks[higher_lim_ind_abs]
        lower_lim_local <- y$breaks[lower_lim_ind_local]
        higher_lim_local <- y$breaks[higher_lim_ind_local]
        
        #Determine the count for each bin that was clicked on using lower limit index
        abs_count <- z$counts[lower_lim_ind_abs]
        local_count <- y$counts[lower_lim_ind_local]
        
        #Create vectors for each histogram consisting of lower and upper limits, midpoint, and frequency
        abs_info <- c(lower_lim_abs, higher_lim_abs, z$mids[lower_lim_abs < z$mids & z$mids < higher_lim_abs], abs_count)
        local_info <- c(lower_lim_local, higher_lim_local, y$mids[lower_lim_local < y$mids & y$mids < higher_lim_local], local_count)
        
        #Create dataframe
        df <- cbind(abs_info, local_info)
        
        #If both checkboxes are marked
        if (input$absmaxcheck & input$localmaxcheck) {
            
            #If the user clicks within the range of either one or both histograms
            if(input$click5$x < min(c(binsAbsMax(),binsLocMax())) | input$click5$x > max(c(binsAbsMax(),binsLocMax()))) {
                
                #If user clicks out of the range for both histograms
                print("Click on a bin that is within the range of either histogram.")
                
                
            } else if (input$click5$x < min(binsAbsMax()) | input$click5$x > max(binsAbsMax())) {
                
                #If the user clicks on a green bin that is out of the range of the blue histogram
                df[,1] <- NA * integer(nrow(df))
                colnames(df) <- c("Absolute Max", "Local Max")
                rownames(df) <- c("Lower Bin Limit", "Higher Bin Limit", "Midpoint", "Frequency")
                df
                
            } else if (input$click5$x < min(binsLocMax()) | input$click5$x > max(binsLocMax())) {
                
                #If the user clicks on a blue bin that is out of the range of the green histogram
                df[,2] <- NA * integer(nrow(df))
                colnames(df) <- c("Absolute Max", "Local Max")
                rownames(df) <- c("Lower Bin Limit", "Higher Bin Limit", "Midpoint", "Frequency")
                df
                
            } else {
                
                colnames(df) <- c("Absolute Max", "Local Max")
                rownames(df) <- c("Lower Bin Limit", "Higher Bin Limit", "Midpoint", "Frequency")
                df
                
            }
            
        } else if (input$absmaxcheck) {
            
            #If user only checks box for absolute max histogram
            #If click is outside of range
            if (input$click5$x < min(binsAbsMax()) | input$click5$x > max(binsAbsMax())) {
                
                print("Click on a bin within the range of the histogram")
                
            } else {
                
                #Subset the first column
                df <- df[,1]
                names(df) <- c("Lower Bin Limit", "Higher Bin Limit", "Midpoint", "Frequency")
                df
                
            }
            
        } else if (input$localmaxcheck) {
            
            #If user only checks box for local max histogram
            #If click is outside of range
            if (input$click5$x < min(binsLocMax()) | input$click5$x > max(binsLocMax())) {
                
                print("Click on a bin within the range of the histogram")
                
            } else {
                
                #Subset the first column
                df <- df[,2]
                names(df) <- c("Lower Bin Limit", "Higher Bin Limit", "Midpoint", "Frequency")
                df
                
            }
            
        }
        
    })
    
    #Output information for smoothing spline: local max through brush
    output$info8 <- renderPrint({
        
        #Determine x and y values of local max
        Wavelength <- local_max_points()[1,]
        Normal_Intensity <- local_max_points()[2,]
        
        #Print local max/min points
        brushedPoints(data.frame(Wavelength, Normal_Intensity), input$smoothPlot_brush, xvar = "Wavelength", yvar = "Normal_Intensity")
        
    })
    
    
    ###OUTPUT TEXT###
    
    #Display class width for Peak Wavelength Histogram
    output$classwidth <- renderText({
        
        #Display Class Width
        paste("Bin width is", (max(binsPeakWave()) - min(binsPeakWave()))/(length(binsPeakWave())-1), "nm")
        
    })
    
    #Display class width for Local Peak Wavelength Histogram
    output$classwidth1 <- renderText({
        
        #Display Class Width
        paste("Bin width is", (max(binsLocWave()) - min(binsLocWave()))/(length(binsLocWave())-1), "nm")
        
    })
    
    #Display class width for Absolute Max Histogram
    output$classwidth2 <- renderText({
        
        #Display Class Width
        paste("Bin width is", (max(binsAbsMax()) - min(binsAbsMax()))/(length(binsAbsMax())-1))
        
    })
    
    #Display class width for Local Max Histogram
    output$classwidth3 <- renderText({
        
        #Display Class Width
        paste("Bin width is", (max(binsLocMax()) - min(binsLocMax()))/(length(binsLocMax())-1))
        
    })
    
    
    ###REACTIVE VALUES FOR DATA TABLES###
    
    df_t1 <- reactive({
        test <- dataset()
        colnames(test) <- c("X","Y",paste0("&lambda;",tags$sub("peak")),"Intensity", "Normal Intensity")
        test
    })
    
    df_t2 <- reactive({
        
        test <- bigger_data()[,-(1:5)]
        colnames(test) <- c(paste(paste0("&lambda;",tags$sub(1)),"Intensity"), paste(paste0("&lambda;",tags$sub(2)),"Intensity"), paste("Normal", paste0("&lambda;",tags$sub(1)),"Intensity"), paste("Normal", paste0("&lambda;",tags$sub(2)),"Intensity"), "Relative Intensity")
        test
        
    })
    
    
    ###DATA TABLES###
    
    #Table 1 Output
    output$table <- renderTable({
        
        #Print df_t1
        df_t1()
        
    }, sanitize.text.function = function(x) x)
    
    
    #Table 2 Output
    output$table1 <- renderTable({
        
        df_t2()
        
    }, sanitize.text.function = function(x) x)
    
}


# Run the application 
shinyApp(ui = ui, server = server)
