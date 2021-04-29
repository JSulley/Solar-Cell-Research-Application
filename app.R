# Load needed packages and R scripts
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
library(shinyWidgets)
source("R scripts/Absolute Max Function.R")
source("R scripts/Relative Intensity Function.R")
source("R scripts/Local Max Function.R")
source("R scripts/SS Model List Function.R")

# Set up shiny dashboard
# Create header
header <- dashboardHeader(title = "Solar Cell Research")

# Create sidebar
sidebar <- dashboardSidebar(
    
    # Create menu
    sidebarMenu(
        
        # Create Welcome tab
        menuItem("Welcome", tabName = "introduction"),
        hr(),
        
        # File Upload button
        fileInput("file", 
                  h3("File Upload", align = "center"), 
                  accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
        
        # Radio buttons
        awesomeRadio("sep", h3("Separator"), 
                     choices = c(Tab = "\t", Comma = ","), selected = "\t", checkbox = TRUE),
        hr(),
        
        # Create select box for file format options
        selectInput("select", label = h3("Plot Download Format"), 
                    choices = list("SVG" = 1, "JPEG" = 2, "PNG" = 3), 
                    selected = 1),
        hr(),
        
        
        # Create remaining menu items 
        menuItem("Dataset Heatmaps", tabName = "datasetDash"),
        menuItem("Relative/Wavelength Intensity Plots", tabName = "relativeDash"),
        menuItem("Dataset Tables", tabName = "tables"),
        menuItem("Dataset Histograms", tabName = "datasethistograms"),
        menuItem("Export AllSpectra", tabName = "smoothSpline"),
        menuItem("About", tabName = "about")
    )
)

# Create body
body <- dashboardBody(
    
    # Create tabs with content inside them
    tabItems(
        
        # Content for "Welcome" tab
        tabItem(tabName = "introduction",
                
                fluidPage(
                    
                    h1("Welcome!"),
                    h2("Click on the any of the tabs for information you need to know!"),
                    
                    # Organize information
                    tabBox(width = 10,
                           
                           # Notes for uploading the file
                           tabPanel("File Format for Upload",
                                    
                                    # Make note to user for what the app is expecting as an input
                                    p("The program assumes the CSV file as the following format:"),
                                    tags$ul(
                                        tags$li("Nothing in the first two boxes."),
                                        tags$li("The first row is the wavelengths."),
                                        tags$li("The first two columns are the x and y coordinates."),
                                        tags$li("The rest are the intensities."),
                                        ),
                                    strong("Example:"),
                                    br(),
                                    
                                    # Image demonstrating format of CSV file
                                    img(src = "Example Data.PNG", height = 200, width = 350),
                                    br(),
                                    br(),
                                    
                                    # Make note of program being able to accept TSV files
                                    p("The program also accepts files that are in TSV format (tab-separated
                                    values). It assumes the same structure as it would for CSV files.
                                    In order to ensure that the program reads the dataset correctly, you need to
                                    specify the separator (tab or comma) before uploading it. This can be done
                                      underneath the file upload widget.")
                                    
                           ),
                           
                           # Note regarding downloading plots.
                           tabPanel("Downloading Plots/Tables",
                                    
                                    p("You are able to download the plots/tables displayed! The plots can be
                                      downloaded in SVG, JPEG and PNG format, while the tables are downloaded only in
                                      CSV format. To specify format for the plots, go to \"Plot Download Format\" and
                                      select the format you want."),
                                    p("For Table 2 in \"Dataset Tables\" to appear and be available for download,
                                      you", strong("must"), "input two different wavelengths first."),
                                    
                                    # Note what calculation is being done for relative intensity
                                    p(strong("Note:"), "For the relative intensity, it is the ratio of",
                                      HTML(paste0("&lambda;",tags$sub(1))) ,"intensity to",
                                      HTML(paste0("&lambda;",tags$sub(2))) ,"intensity")),
                           
                           # Note recent features
                           tabPanel("More Features",
                                    
                                    # Interactive Peak Wavelength Histogram
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
                                    
                                    # Smoothing Spline Interpolation Output
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
                                    
                                    # Interactive Smooth Spline
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
                           
                                    # Histogram main and axes title
                                    h3("Histogram Plot Controls"),
                                    p("The controls of the histograms have expanded causing the plot settings to be
                                      divided into three sections: Bin, Title and Axis. For \"Bin Settings\", the
                                      number of bins for the axis can be controlled through the slider, and any
                                      histogram can be shown/hidden by checking/unchecking the box next to the
                                      sliders. \"Title Settings\" enables you to rename the main and axis titles.
                                      Finally, \"Axis Settings\" controls the location of the tick marks on the
                                      horizontal axis also the distance between each one. There is also a range slider
                                      that controls the horizontal axis' limits."),
                                    p("In order to see the new names and axis on the plot, you must first press
                                      \"Apply Title/Axis Settings\". The sliders and checkboxes apply the settings to
                                      the plot immediately, so those do not require you to press the button to be
                                      shown on the plot. You can also show/hide the plot controls by simply checking/
                                      unchecking the box labeled \"Display Plot Controls\"."),
                                    p("Above those lies an alternative method for inputting the number of bins. This
                                      one, however, controls both plots based on the color. There is one for the blue
                                      histograms (Peak Wavelength and Absolute Max Histograms) and one for the green
                                      histograms (Local Peak Wavelength and Local Max Histograms). Type in the number
                                      then press \"Enter\"."),
                                    p(strong("Note:"), "You need to have the plot controls shown for the change to take
                                      place."),
                                    img(src = "Numeric Input Number of Bins.PNG", width = 700),
                                    img(src = "Plot Controls.PNG", height = 450, width = 900),
                                    hr(),
                                    
                                    # Title option for heatmaps
                                    h3("Dataset Heatmaps: Titles Now Optional"),
                                    p("The title for both dataset heatmaps is optional when downloaded."),
                                    p("Simply leave the box unchecked if the title should be omitted. Otherwise,
                                      go ahead and check it out!"),
                                    img(src = "HeatMap Title Box.PNG"),
                                    hr(),
                                    
                                    h3("Dataset Smooth Splines"),
                                    p("This plot contains the smoothing spline for each coordinate in a dataset and
                                      can be generated by pressing \"Show All Smooth Splines\" after uploading the
                                      dataset. The row number associated with a line is displayed by simply bringing
                                      the cursor near the line, and the program shows the result underneath the plot.
                                      If the cursor is not close enough to a line, a message will be shown asking you
                                      to bring the cursor closer to it."),
                                    img(src = "Multiple Smooth Splines.png", height = 520, width = 950)
                                    )
                                
                    )   
                    
                )
        ),
        
        # Content for "Dataset Heatmaps"
        tabItem(tabName = "datasetDash",
                
                # Download button for Normalized Absolute Max Intensity Heat Map
                downloadBttn("downloadgraph", "Download Normal Max Intensity Plot", color = "primary", size = "sm"),
                checkboxInput("checkTitle", "Include Title In Download", value = FALSE),
                
                # Output heat map for Normalized Absolute Max Intensity and Information
                plotOutput("normMaxIntHeatMap", click = "click") %>%
                    withSpinner(getOption("spinner.type", 8)),
                verbatimTextOutput("info1"),
                
                # Add border
                hr(),
                
                # Make download button for Peak Wavelength Heat Map
                downloadBttn("downloadgraph1", "Download Peak Wavelength Plot", color = "primary", size = "sm"),
                checkboxInput("checkTitle1", "Include Title In Download", value = FALSE),
                
                # Output heat map for Peak Wavelength and Information
                plotOutput("peakWaveHeatMap", click = "click") %>%
                    withSpinner(getOption("spinner.type", 8)),
                verbatimTextOutput("info2"),
                
                fluidRow(column(3,
                                
                                uiOutput("scaleLow")
                                
                                ),
                         
                         column(3,
                                
                                uiOutput("scaleUp")
                                
                                ),
                         
                         column(1,
                                
                                actionBttn("applyLimits", "Enter", color = "primary")
                                
                                ),
                         
                         column(1,
                                
                                actionBttn("reset", "Reset", color = "primary")
                                
                                ))),
        
        # Content for Relative/Wavelength Intensity Plots
        tabItem(tabName = "relativeDash",
                
                fluidRow(
                    column(4,
                           
                           # Make two boxes that accept numeric entries for two wavelength values
                           h3("Wavelength Input", align = "left"),
                           p('Input two distinct wavelengths', align = "left"),
                           
                           numericInput('wavelength1', 
                                        HTML(paste("Enter", paste0("&lambda;",tags$sub(1)))),
                                        value = NULL, min = 0),
                           
                           numericInput('wavelength2',
                                        HTML(paste("Enter", paste0("&lambda;",tags$sub(2)))),
                                        value = NULL, min = 0),
                           
                           # Create Action button for user
                           actionBttn("go","Go!", color = "primary")
                           
                           )
                    
                ),
                # Make fluid row for interactive relative intensity map 
                fluidRow(
                    
                    h3(strong(em("Note:")), "Did you change the inputs for one or both wavelengths,
                    but the plot(s) look(s) the same/did not change? Make sure you press \"Go!\"
                    after updating any wavelength values!", align = "center"),
                    
                    
                    
                    column(12,
                           
                           # Make download button for Relative Intensity Heat Map
                           downloadBttn("downloadgraph2", "Download Relative Intensity Plot", color = "primary", size = "sm"),
                           
                           # Heat Map plot of Relative Intensity given two values
                           plotOutput("RelIntHeatMap", click = "click1") %>%
                               withSpinner(getOption("spinner.type", 8)),
                           verbatimTextOutput("info3")
                           
                        ),
                    
                    
                    # Make fluid row for interactive intensities for both inputted wavelengths
                    fluidRow(
                        
                        column(6,
                               
                               # Make download button for heat map of inputted wavelength 1 
                               downloadBttn("downloadgraph3", "Download Wavelength 1 Intensity Plot", color = "primary", size = "sm"),
                               
                               # Heat map plot for intensities of wavelength 1
                               plotOutput("Wave1IntenHeatMap", click = "click2") %>%
                                   withSpinner(getOption("spinner.type", 8)),
                               verbatimTextOutput("info4"),
                        ),
                        
                        column(6,
                               
                               # Make download button for heat map of inputted wavelength 2 
                               downloadBttn("downloadgraph4", "Download Wavelength 2 Intensity Plot", color = "primary", size = "sm"),
                               
                               # Heat map plot for intensities of wavelength 1
                               plotOutput("Wave2IntenHeatMap", click = "click3") %>%
                                   withSpinner(getOption("spinner.type", 8)),
                               verbatimTextOutput("info5")
                               
                        )
                    ),
                )
        ),
        
        # Content for "Dataset Tables" tab
        tabItem(tabName = "tables",
                
                # Make fluid row for both tables
                fluidRow(
                    
                    # Place table recording the values (include relative intensity)
                    column(6,
                           
                           # Display title
                           h2("Table 1"),
                           
                           # Download button for first table (X, Y, Peak wave., Int, Norm. Int.)
                           downloadBttn("downloadtable", "Download Table 1", color = "primary"),
                           
                           # Output table regarding the given data set
                           tableOutput('table')
                           
                           ),
                    
                    # Table consisting columns for the output values for given wavelength values
                    column(6,
                           
                           # Display title
                           h2("Table 2"),
                           
                           # Download button for second table (Wavelength 1, Wavelength 2, Relative Intensity)
                           downloadBttn("downloadtable1", "Download Table 2", color = "primary"),
                           
                           # Output table regarding inputted wavelengths
                           tableOutput('table1')
                           
                           ),
                )
        ),
        
        # Content for "Dataset Histograms" tab
        tabItem(tabName = "datasethistograms",
                
                fluidRow(
                    
                    column(3,
                           
                           # Create numeric input for number of bins for Peak Wavelength/Absolute Max Histograms
                           numericInput("numberOfBins", "Numeric Input: Number of Bins (Blue Histograms)",
                                        value = 2, min = 1),
                           
                           ),
                    
                    column(1,
                           
                           # Enter button
                           actionBttn("go2", "Enter", color = "primary")
                           
                           ),
                    
                    column(3,
                           
                           # Create numeric input for number of bins for Local Peak Wavelength/Local Max Histograms
                           numericInput("numberOfBins1", "Numeric Input: Number of Bins 
                                        (Green Histograms)", value = 2, min = 1),
                           
                           ),
                    
                    column(1,
                           
                           # Enter button
                           actionBttn("go3", "Enter", color = "primary")
                           
                    )),
                
                checkboxInput("showPltControl", "Display Plot Controls", value = TRUE),
                conditionalPanel(condition = "input.showPltControl == '1'",
                                 
                                 h3("Bin Settings"),
                                 
                                 fluidRow(
                                     
                                     column(3,
                                            
                                            # Create slider for Peak Wavelength Histogram
                                            uiOutput("slider"),
                                            textOutput("classwidth")
                                            
                                     ),
                                     
                                     column(1,
                                            
                                            # Create a checkbox for Peak wavelength Histogram
                                            checkboxInput("peakcheck", label = "Display Histogram", value = TRUE)
                                            
                                     ),
                                     
                                     column(3,
                                            
                                            # Create slider for Local Peak Wavelength Histogram
                                            uiOutput("slider1"),
                                            textOutput("classwidth1")
                                            
                                     ),
                                     
                                     column(1,
                                            
                                            # Create a checkbox for Local Peak wavelength histogram
                                            checkboxInput("localpeakcheck", label = "Display Histogram", value = TRUE)
                                            
                                     )),
                                 
                                 hr(),
                                 
                                 h3("Title Settings"),
                                 
                                 # Create row for text input
                                 fluidRow(
                                     
                                     column(3,
                                            
                                            # Text input: Histogram Title
                                            textInput("textInp", "Histogram Title", "Peak/Local Peak Wavelength Histogram")
                                            
                                     ),
                                     
                                     column(3,
                                            
                                            # Text input: Horizontal Axis Title
                                            textInput("textInp1", "Horizontal Axis Title", "Wavelength")
                                            
                                     ),
                                     
                                     column(3,
                                            
                                            # Text input: Vertical axis Title
                                            textInput("textInp2", "Vertical Axis Title", "Frequency")
                                            
                                     ),
                                     
                                     column(3,
                                            
                                            # Action button: Apply changes
                                            actionBttn('applyChange', "Apply Title/Axis Settings", color = "primary")
                                            
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
                                     
                                     column(1,
                                            
                                            actionBttn("reset1", "Reset", color = "primary")
                                            
                                    ))),
                
                # Download button for histogram
                downloadBttn("downloadHist","Download Histogram Plot", color = "primary"),
                
                # Output Peak Wavelength Histogram and Information
                plotOutput("peakhist", click = "click4") %>%
                    withSpinner(getOption("spinner.type", 8)),
                verbatimTextOutput("info6"),
                hr(),
                
                checkboxInput("showPltControl1", "Display Plot Controls", value = TRUE),
                conditionalPanel(condition = "input.showPltControl1 == '1'",
                                 
                                 h3("Bin Settings"),
                                 
                                 fluidRow(
                                     
                                     column(3,
                                            
                                            # Create slider for Absolute Max Histogram
                                            uiOutput("slider2"),
                                            textOutput("classwidth2")
                                            
                                     ),
                                     
                                     column(1,
                                            
                                            # Create a checkbox for Peak wavelength Histogram)
                                            checkboxInput("absmaxcheck", label = "Display Histogram", value = TRUE)
                                            
                                     ),
                                     
                                     column(3,
                                            
                                            # Create slider for Local Max Histogram
                                            uiOutput("slider3"),
                                            textOutput("classwidth3")
                                            
                                     ),
                                     
                                     column(1,
                                            
                                            # Create a checkbox for Peak wavelength Histogram)
                                            checkboxInput("localmaxcheck", label = "Display Histogram", value = TRUE)
                                            
                                     )),
                                 
                                 hr(),
                                 
                                 h3("Title Settings"),
                                 
                                 # Create row for text input
                                 fluidRow(
                                     
                                     column(3,
                                            
                                            # Text input: Histogram Title
                                            textInput("textInp3", "Histogram Title", "Absolute/Local Max Values Histogram")
                                            
                                     ),
                                     
                                     column(3,
                                            
                                            # Text input: Horizontal Axis Title
                                            textInput("textInp4", "Horizontal Axis Title", "Value")
                                            
                                     ),
                                     
                                     column(3,
                                            
                                            # Text input: Vertical axis Title
                                            textInput("textInp5", "Vertical Axis Title", "Frequency")
                                            
                                     ),
                                     
                                     column(3,
                                            
                                            # Action button: Apply changes
                                            actionBttn('applyChange1', "Apply Title/Axis Settings", color = "primary")
                                            
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
                                     
                                     column(1,
                                            
                                            actionBttn("reset2", "Reset", color = "primary")
                                            
                                     ))
                                 
                                 ),
                
                # Download button for Absolute/local max histogram
                downloadBttn("downloadHist1","Download Histogram Plot", color = "primary"),
                
                # Create histogram for absolute/local max histograms and information
                plotOutput("abslochist", click = "click5") %>%
                    withSpinner(getOption("spinner.type", 8)),
                verbatimTextOutput("info7")
                
                
        ),
        
        # Content for "Export Allspectra" tab
        tabItem(tabName = "smoothSpline",
                
                # Switch between Path 1 & 2 Normalization of
                # smoothing spline model.
                radioGroupButtons("pathChoice", label = "Normalization Method", choices = c("Path 1", "Path 2"), status = "primary"),
                
                plotOutput("datasetSplinesPath", hover = hoverOpts('SmoothHover', delay = 200)) %>%
                    withSpinner(getOption("spinner.type", 8)),
                uiOutput("tooltip"),
                
                fluidRow(
                    
                    column(4,
                            
                           # Make numeric entry for row number to view smoothing spline interpolation graph with scatterplot
                           h4("Smooth Spline Interpolation Display Input", align = "left"),
                           numericInput('rowIndex', "Enter row number" , value = NULL, min = 0),
                           
                           # Create action button for it
                           actionBttn("go1", "Go!", color = "primary"),
                           hr()
                    
                        )),
                
                fluidRow(
                    
                    column(4,
                           
                           # Download button for smoothing spline plot
                           downloadBttn("downloadSpectrum", "Download Spectrum Plot", color = "primary")),
                           
                    column(3,       
                           # Switch between Path 1 & 2 Normalization of
                           # smoothing spline model.
                           radioGroupButtons("pathChoice1", label = "Normalization Method", choices = c("Path 1", "Path 2"), status = "primary")
                           
                           )),
                
                # Output smoothing spline plot
                plotOutput("smoothLine", 
                           brush = brushOpts("smoothPlot_brush", resetOnNew = TRUE)) %>%
                    withSpinner(getOption("spinner.type", 8)),
                
                # Output zoomed smoothing spline plot
                plotOutput("zoomsmooth"),
                verbatimTextOutput("info8")
                
        ),
        
        # Content for "About" tab
        tabItem(tabName = "about",
                h1("About the Application"),
                h3("It imports a cathodoluminescence dataset, which is obtained from images of a
                   photovoltaic cell. Using the dataset, the program can visualize the
                   characterization of the luminescence of solar cells, distribution of the
                   wavelengths associated with the luminescence, the relationship between both
                   luminescence and the wavelengths and more. The plots/tables generated are
                   capable of being downloaded.", 
                   br(), 
                   br(),
                   "This application is a product of the collaboration between both the Mathematics
                   Department and the Engineering Technology Department at 
                   Texas A&M University-Central Texas. The purpose of it is to enhance the
                   Engineering Technology Department's research about solar cells through
                   statistics. It is updated from time to time with new features and
                   improvements."),
        )
    ))


# Define UI
ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body)


# Define server logic
server <- function(input, output, session) {
    
    # Increase file size input
    options(shiny.maxRequestSize = 30*1024^2)
    
    
    ###PREPARATION###
    
    # Read uploaded file
    first_df <- reactive({
        
        req(input$file)
        
        # Use the uploaded file and selected separator to build tibble
        df <- read_delim(input$file$datapath, delim = input$sep, col_names = FALSE)
        
        as.matrix(df)

    })
    
    ###DECOMPOSE DATASET###
    
    # Subset wavelengths
    dataset_wavelengths <- eventReactive(input$file$datapath, {
        
        # Wavelengths is the first row, first two entries are empty
        unlist(first_df()[1,-(1:2)], use.names = FALSE)
        
    })
    
    # Subset coordinates
    dataset_coordinates <- eventReactive(input$file$datapath, {
        
        first_df()[-1, 1:2]
        
    })
    
    # Subset intensities 
    dataset_intensities <- eventReactive(input$file$datapath, {
        
        first_df()[-1,-(1:2)]
        
    })

    ###SMOOTHING SPLINE MODEL LIST###
    
    smoothing_spline_list <- eventReactive(input$file$datapath, {
        
        ss_model_list_function(dataset_wavelengths(), dataset_intensities())
        
    })
    
    
    # Use absolute_max_function (Absolute Max Function.R)
    dataset <- eventReactive(input$file$datapath, {
        
        absolute_max_function(dataset_wavelengths(), dataset_coordinates(), smoothing_spline_list())
        
    })
    
    # Path 1 Normalization: Divide every row's intensities by the greatest absolute max intensity value 
    normal_ss_list1 <- reactive({
        
        ss_list <- smoothing_spline_list()
        list_length <- length(ss_list)
        abs_max_intensity_vector <- unlist(dataset()[,4], use.names = FALSE)
        absolute_max_of_dataset <- max(abs_max_intensity_vector)
        
        for (k in 1:list_length) {
            ss_list[[k]]$y <- ss_list[[k]]$y/absolute_max_of_dataset
        }
        
        ss_list
        
    })
    
    # Path 2 Normalization: Divide each row's intensities by its respective absolute max intensity value
    normal_ss_list2 <- reactive({
        
        ss_list <- smoothing_spline_list()
        
        list_length <- length(ss_list)
        abs_max_intensity_vector <- unlist(dataset()[,4], use.names = FALSE)
        
        for (k in 1:list_length) {
            ss_list[[k]]$y <- ss_list[[k]]$y/abs_max_intensity_vector[k]
        }
        
        ss_list
        
    })
    
    selected_path_list <- reactive({
        
        if (input$pathChoice == "Path 1") {
            
            normal_ss_list1()
            
        } else if (input$pathChoice == "Path 2") {
            
            normal_ss_list2()
            
        }
        
    }) 
    
    # Get list of all local max values and their corresponding wavelengths for every row
    local_max_list <- eventReactive(input$file$datapath, {
        
        # Determine list of consisting of wavelengths and their corresponding intensities in their respective row
        local_max_function(dataset_wavelengths(), smoothing_spline_list())
        
    })
    
    # Window margins for zoomsmooth plot
    window_margins <- reactiveValues(x = NULL, y = NULL)
    
    
    ###OBJECTS FOR HISTOGRAMS###
    
    # Create local peak wavelength vector
    localPeakWave <- reactive({
        
        unlist(local_max_list()[1:(length(local_max_list())/2)], use.names = FALSE)
        
    })
    
    # Calculate break points based on lowest & highest peak wavelength values along with number of bins 
    binsPeakWave <- reactive({
        
        seq(min(dataset()[,3]),max(dataset()[,3]), length.out = input$inslider + 1)
    
    })
    
    # Calculate break points based on lowest & highest local peak wavelength values along with number of bins
    binsLocWave <- reactive({
        
        seq(min(localPeakWave()),max(localPeakWave()), length.out = input$inslider1 + 1)
    
    })
    
    # Create local max vector 
    localMax <- reactive({
        
        # Subset local_max_list
        p <- local_max_list()[(length(local_max_list())/2 + 1):length(local_max_list())]
        
        absolute_max_intensity <- max(unlist(dataset()[,4], use.names = FALSE))
        
        # Coerce p to vector
        p <- unlist(p, use.names = FALSE)
        
        # Normalize local max values (Path 1)
        p <- p/absolute_max_intensity
        
        p
    
    })
    
    # Calculate break points
    binsAbsMax <- reactive({
        
        seq(min(dataset()[,5]),max(dataset()[,5]), length.out = input$inslider2 + 1)
    
    })
    
    # Calculate break points
    binsLocMax <- reactive({
        
        seq(min(localMax()),max(localMax()), length.out = input$inslider3 + 1)
        
    })
    
    
    ###ACTION BUTTONS###
    
    # Create table using relative_intensity_function (Relative Intensity Function.R) function
    bigger_data <- eventReactive(input$go, {
        
        if (is.na(input$wavelength1) || is.na(input$wavelength2)) {
            
            showModal(modalDialog(
                title = "Error",
                "Make sure each box has a value before pressing \"Go!\".",
                easyClose = TRUE
            ))
            
        } else {
            
            # Combine dataset() with dataframe created by relative_intensity_function (Relative Intensity Function.R) by column
            cbind(dataset(), relative_intensity_function(input$wavelength1,input$wavelength2, smoothing_spline_list()))
            
        }
        
        
    })
    
    
    # Get corresponding intensities for inputted row 
    y_values1 <- eventReactive(input$go1, {
        
        if (is.na(input$rowIndex)) {
            
            showModal(modalDialog(
                title = "Error",
                "Enter a row number before pressing \"Go!\".",
                easyClose = TRUE
            ))
            
        } else {
            
            # Find row and omit first two entries (these are the coordinates for the rows)
            regular_y_val <- unlist(first_df()[input$rowIndex + 1, -c(1,2)], use.names = FALSE)
            
            # Normalize them by dividing by the largest intensity value
            regular_y_val/max(dataset()[,4])
            
        }
        
        
    })
    
    # Get corresponding intensities for inputted row 
    y_values2 <- eventReactive(input$go1, {
        
        
        # Find row and omit first two entries (these are the coordinates for the rows)
        regular_y_val <- unlist(first_df()[input$rowIndex + 1, -c(1,2)], use.names = FALSE)
        
        # Normalize them by dividing by the inputted row's absolute max intensity value
        row_abs_max_intensity <- dataset()[input$rowIndex,4]
        regular_y_val/row_abs_max_intensity
        
    })
    
    # Create smooth spline function
    # Path 1 smoothing spline
    splinesmoothfit1 <- eventReactive(input$go1, {
        
        normal_ss_list1()[[input$rowIndex]]
        
    }) 
    
    # Path 2 smoothing spline
    splinesmoothfit2 <- eventReactive(input$go1, {
        
        normal_ss_list2()[[input$rowIndex]]
        
    }) 
    
    # Determine absolute max coordinates from inputted row (Path 1)
    abs_max_coordinates1 <- eventReactive(input$go1, {
        
        c(dataset()[input$rowIndex,3], dataset()[input$rowIndex,5])
        
    })
    
    # Determine absolute max coordinates from inputted row (Path 2)
    abs_max_coordinates2 <- eventReactive(input$go1, {
        
        # Predict values for peak wavelength on Path 2 smoothing spline
        row_peak_wavelength <- dataset()[input$rowIndex,3]
        predicted <- predict(splinesmoothfit2(), x = row_peak_wavelength)
        row_abs_max_intensity <- dataset()[input$rowIndex, 4]
        normal_y_value <- predicted$y/row_abs_max_intensity
        
        c(row_peak_wavelength, normal_y_value)
        
    })
    
    # Get coordinates
    coordinates <- eventReactive(input$go1,{
        
        # Find row and only keep first two entries
        unlist(dataset_coordinates()[input$rowIndex,], use.names = FALSE)
        
    })
    
    # Create local max matrix for Path 1
    local_max_points1 <- eventReactive(input$go1, {
        
        # Extract local max points for given row
        p <- local_max_list()[c(input$rowIndex, input$rowIndex + length(local_max_list())/2)]
        
        # Create matrix by first unlisting p and then convert to a matrix where the first row has wavelengths and second row has local max values
        local_max_matrix <- matrix(unlist(p, use.names = FALSE), nrow = 2, byrow = TRUE)
        absolute_max_intensity <- max(unlist(dataset()[,4], use.names = FALSE))
        local_max_matrix[2,] <- local_max_matrix[2,]/absolute_max_intensity
        
        local_max_matrix
        
    })
    
    # Create local max matrix for Path 2
    local_max_points2 <- eventReactive(input$go1, {
        
        # Extract local max points for given row
        p <- local_max_list()[c(input$rowIndex, input$rowIndex + length(local_max_list())/2)]
        
        # Create matrix by first unlisting p and then convert to a matrix where the first row has wavelengths and second row has local max values
        local_max_matrix <- matrix(unlist(p, use.names = FALSE), nrow = 2, byrow = TRUE)
        row_abs_max_intensity <- dataset()[input$rowIndex, 4]
        local_max_matrix[2,] <- local_max_matrix[2,]/row_abs_max_intensity
        
        local_max_matrix
        
    })
    
    
    ###SLIDER INPUT###
    
    # Slider for Peak Wavelength Histogram 
    output$slider <- renderUI({
        
        input$go2
        
        # Indicate number of bins
        # Lowest number is 1; highest number is how many coordinates there are in the uploaded dataset
        # Default value is 2
        sliderInput("inslider", "Peak Wavelength (Blue)", min = 1, max = length(dataset()[,3]), value = isolate(input$numberOfBins), step = 1)
        
    })
    
    # Slider for Local Peak Wavelength Histogram
    output$slider1 <- renderUI({
        
        input$go3
        
        # Indicate number of bins
        # Lowest number is 1; highest number is how many coordinates there are in the uploaded dataset
        # Default value is 2
        sliderInput("inslider1", "Local Peak Wavelength (Green)", min = 1, max = length(unlist(local_max_list()[1:(length(local_max_list())/2)], use.names = FALSE)), value = isolate(input$numberOfBins1), step = 1)
        
    })
    
    # Slider for horizontal range of Absolute/Local Max Histogram
    output$sliderx1 <- renderUI({
        
        # Calculate min and max values for each histogram. Then use the floor function on the minimums and ceiling function on maximums for integers
        min_both <- floor(min(c(dataset()[,5], localMax())))
        max_both <- ceiling(max(c(dataset()[,5],localMax())))
        min_abs <- floor(min(dataset()[,5]))
        max_abs <- ceiling(max(dataset()[,5]))
        min_local <- floor(min(localMax()))
        max_local <- ceiling(max(localMax()))
        
        # If user checks both boxes, then display both histograms
        if (input$absmaxcheck & input$localmaxcheck) {
            
            # Create slider based on both histograms
            sliderInput("insliderx1", "Horizontal Axis Range", min = min_both, max = max_both, value = c(min_both,max_both), step = (max_both - min_both)/(max(length(dataset()[,5]), length(localMax()))-1))
            
        } else if (input$absmaxcheck) {
            
            # Create slider based on the absolute max histogram 
            sliderInput("insliderx1", "Horizontal Axis Range", min = min_abs, max = max_abs, value = c(min_abs, max_abs), step = (max_abs - min_abs)/(length(dataset()[,5])-1))
            
        } else if (input$localmaxcheck) {
            
            # Create slider based on the local peak wavelength histogram
            sliderInput("insliderx1", "Horizontal Axis Range", min = min_local, max = max_local, value = c(min_local, max_local), step = (max_local - min_local)/(length(localMax())-1))
            
        }
        
    })
    
    # Slider for Absolute Max Histogram
    output$slider2 <- renderUI({
        
        input$go2
        
        # Indicate number of bins
        # Lowest number is 1; highest number is how many elements are in each vector
        # Default value is 2
        sliderInput("inslider2", "Absolute Max (Blue)", min = 1, max = length(dataset()[,5]), value = isolate(input$numberOfBins), step = 1)
        
    })
    
    # Slider for Local Max Histogram
    output$slider3 <- renderUI({
        
        input$go3
        
        # Indicate number of bins
        # Lowest number is 1; highest number is how many elements are in each vector
        # Default value is 2
        sliderInput("inslider3", "Local Max (Green)", min = 1, max = length(unlist(local_max_list()[(length(local_max_list())/2+1):length(local_max_list())], use.names = FALSE)), value = isolate(input$numberOfBins1), step = 1)
        
    })
    
    # Numeric input for minimum value for horizontal axis
    output$lower <- renderUI({
        
        input$reset1
        
        minimum <- floor(min(c(dataset()[,3], localPeakWave())))
        
        numericInput("lowerBound", "Minimum", value = minimum)
        
    })
    
    output$upper <- renderUI({
        
        input$reset1
        
        maximum <- ceiling(max(c(dataset()[,3], localPeakWave())))
        
        numericInput("upperBound", "Maximum", value = maximum)
        
    })
    
    output$increment <- renderUI({
        
        input$reset1
        
        defIncrement <- 5
        
        numericInput("inc", "Increment", value = defIncrement)
        
    })
    
    output$lower1 <- renderUI({
        
        input$reset2
        
        minimum <- floor(min(c(dataset()[,5], localMax())))
        
        numericInput("lowerBound1", "Minimum", value = minimum)
        
    })
    
    output$upper1 <- renderUI({
        
        input$reset2
        
        maximum <- ceiling(max(c(dataset()[,5],localMax())))
        
        numericInput("upperBound1", "Maximum", value = maximum)
        
    })
    
    output$increment1 <- renderUI({
        
        input$reset2
        
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
    
    # Normal Intensity Heat Map
    normMaxIntPlot <- function() {
        
        # Make ggplot2 static plot
        ggplot(df_absInt(), aes(X, Y, fill= Normal_Intensity)) + 
            geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Normal Intensity") + scale_fill_gradient(low = "black", high = "red")
        
    }
    
    # Reactive values for gradient limits
    gradient_limits <- reactiveValues(lower_limit =  NULL, upper_limit = NULL)
    
    # Control gradient limits for Peak Wavelength Heat Map
    observeEvent(input$applyLimits, {
        
        gradient_limits$lower_limit <- input$scaleLowerVal
        gradient_limits$upper_limit <- input$scaleUpperVal
        
    })
    
    observeEvent(input$reset, {
        
        gradient_limits$lower_limit <- original_gradient_limits()[1]
        gradient_limits$upper_limit <- original_gradient_limits()[2]
        
    })
    
    observeEvent(input$file$datapath, {
        
        gradient_limits$lower_limit <- original_gradient_limits()[1]
        gradient_limits$upper_limit <- original_gradient_limits()[2]
        
    })
    
    # Reset graph to original heatmap
    original_gradient_limits <- reactive({
        
        # Round down the minimum wavelength value
        # Round up the maximum wavelength value
        minimum <- floor(min(df_peakWave()$Wavelength))
        maximum <- ceiling(max(df_peakWave()$Wavelength))
        
        c(minimum, maximum)
        
    })
    
    # Peak Wavelength Heat Map
    peakWavePlot <- function() {

        # Convert gradient_limit list objects to vectors
        lower_gradient_limit <- unlist(gradient_limits$lower_limit, use.names = FALSE)
        upper_gradient_limit <- unlist(gradient_limits$upper_limit, use.names = FALSE)
        
        # Make ggplot2 static plot
        ggplot(df_peakWave(), aes(X, Y, fill = Wavelength)) + geom_tile() + theme_ipsum() + 
            xlab("X") + ylab("Y") + labs(fill = "Wavelength") + 
            scale_fill_gradient(low = "#800020", high = "red", limits = c(lower_gradient_limit, upper_gradient_limit), 
                                na.value = fifelse(df_peakWave()$Wavelength < lower_gradient_limit, "black", "red"))
        
    }
    
    # Relative Intensity Heat Map
    relIntPlot <- function() {
        
        if (is.na(input$wavelength1) || is.na(input$wavelength2)) {return()}
        
        # Make ggplot2 static plot
        p <- ggplot(df_relInt(), aes(X, Y, fill= Relative_Intensity)) + 
            geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Rel. Intensity") + scale_fill_gradient(low = "black", high = "red")
        
        p
        
        input$go
        isolate({
            
            p + ggtitle(paste("Relative Intensity", paste0("(",input$wavelength1, "/", input$wavelength2,")"), "Plot"))
            
        }) 
        
    }
    
    # Wavelength 1 Normal Intensity Heat Map 
    wave1IntPlot <- function() {
        
        if (is.na(input$wavelength1) || is.na(input$wavelength2)) {return()}
        
        # Make ggplot2 static plot
        p <- ggplot(df_wave1(), aes(X, Y, fill= Normal_Wave_1_Intensity)) + 
            geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Normal Intensity") + scale_fill_gradient(low = "black", high = "red")
        
        p
        
        input$go
        isolate({
            
            p + ggtitle(paste("Wavelength 1",paste0("(",input$wavelength1,")"),"Intensity Plot")) 
            
        }) 
    }
    
    # Wavelength 2 Normal Intensity Heat Map
    wave2IntPlot <- function() {
        
        if (is.na(input$wavelength1) || is.na(input$wavelength2)) {return()}
        
        # Make ggplot2 static plot
        p <- ggplot(df_wave2(), aes(X, Y, fill= Normal_Wave_2_Intensity)) + 
            geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Normal Intensity") + scale_fill_gradient(low = "black", high = "red")
        
        p
        
        input$go
        isolate({
            
            p + ggtitle(paste("Wavelength 2",paste0("(",input$wavelength2,")"),"Intensity Plot"))  
            
        }) 
        
    }
    
    
    ###HEAT MAP PLOTS###
    
    # Normalized Absolute Max Intensity Heat Map
    output$normMaxIntHeatMap <- renderPlot({
        
        # Generate graph
        normMaxIntPlot() + ggtitle("Normal Max Intensity Plot") 
        
        
        
    })
    
    # Limits of the gradient
    output$scaleLow <- renderUI({
        
        # Put numeric inputs back to original values when user presses Reset
        input$reset 
        
        default_value <- original_gradient_limits()[1]
        
        numericInput("scaleLowerVal", "Enter Lower Limit Of Color Gradient", value = default_value)
        
        
    })
    
    output$scaleUp <- renderUI({
        
        # Put numeric inputs back to original values when user presses Reset
        input$reset
        
        default_value <- original_gradient_limits()[2]
        
        numericInput("scaleUpperVal", "Enter Upper Limit Of Color Gradient", value = default_value)
        
        
    })
    
    
    
    # Peak Wavelength Heat Map 
    output$peakWaveHeatMap <- renderPlot({
        
        peakWavePlot() + ggtitle("Peak Wavelength Plot")
        
    })
    
    # Relative Intensity Heat Map
    output$RelIntHeatMap <- renderPlot({
        
        # Generate graph with title
        relIntPlot()
        
    })
    
    # Intensities for wavelength 1
    output$Wave1IntenHeatMap <- renderPlot({
        
        # Generate graph
        wave1IntPlot()
        
    })
    
    # Intensities for wavelength 2
    output$Wave2IntenHeatMap <- renderPlot({
        
        # Generate graph
        wave2IntPlot()
        
    })
    
    
    ###FUNCTIONS: HISTOGRAMS###
    
    # Reactive values for Peak/Local Peak Wavelength Histogram
    histogram_titles_and_values <- reactiveValues(lower_limit = NULL, upper_limit = NULL, increment = NULL, 
                                                  main_title = NULL, xlabel = NULL, ylabel = NULL)
    
    observeEvent(input$applyChange, {
        
        histogram_titles_and_values$x_lower_limit <- input$lowerBound
        histogram_titles_and_values$x_upper_limit <- input$upperBound
        histogram_titles_and_values$increment <- input$inc
        histogram_titles_and_values$main_title <- input$textInp
        histogram_titles_and_values$xlabel <- input$textInp1
        histogram_titles_and_values$ylabel <- input$textInp2
        
    })
    
    observeEvent(input$file$datapath, {
        
        histogram_titles_and_values$x_lower_limit <- original_x_axis_values()[1]
        histogram_titles_and_values$x_upper_limit <- original_x_axis_values()[2]
        histogram_titles_and_values$increment <- original_x_axis_values()[3]
        histogram_titles_and_values$main_title <- input$textInp
        histogram_titles_and_values$xlabel <- input$textInp1
        histogram_titles_and_values$ylabel <- input$textInp2
        
    })
    
    observeEvent(input$reset1, {
        
        histogram_titles_and_values$x_lower_limit <- original_x_axis_values()[1]
        histogram_titles_and_values$x_upper_limit <- original_x_axis_values()[2]
        histogram_titles_and_values$increment <- original_x_axis_values()[3]
        
    })
    
    original_x_axis_values <- reactive({
        
        minimum <- floor(min(c(dataset()[,3], localPeakWave())))
        maximum <- ceiling(max(c(dataset()[,3], localPeakWave())))
        increment <- 5
        
        c(minimum, maximum, increment)
        
    })
    
    # Peak/Local Peak Wavelength Histogram
    peakLocWavePlot <- function() {
        
        # If user checks both boxes, then display both histograms
        if (input$peakcheck & input$localpeakcheck) {
            
            # Display both histograms
            hist(dataset()[,3], breaks = binsPeakWave(), xaxt = "n", main = NULL, xlab = NULL, ylab = NULL, col = "blue", border = "white", xlim = c(histogram_titles_and_values$x_lower_limit, histogram_titles_and_values$x_upper_limit))
            hist(localPeakWave(), breaks = binsLocWave(), xaxt = "n", col = rgb(0,1,0,0.5), border = "white", add = TRUE)
            
        } else if (input$peakcheck) {
            
            # If user checks box for peak wavelength histogram only, then display it
            hist(dataset()[,3], breaks = binsPeakWave(), xaxt = "n", main = NULL, xlab = NULL, ylab = NULL, col = "blue", border = "white", xlim = c(histogram_titles_and_values$x_lower_limit, histogram_titles_and_values$x_upper_limit))
            
        } else if (input$localpeakcheck) {
            
            # If user checks box for local peak wavelength histogram only, then display it
            hist(localPeakWave(), breaks = binsLocWave(), xaxt = "n", main = NULL, xlab = NULL, ylab = NULL, col = rgb(0,1,0,0.5), border = "white", xlim = c(histogram_titles_and_values$x_lower_limit, histogram_titles_and_values$x_upper_limit))
            
        }
        
        
    }
    
    # Reactive values for Absolute and Local Max Histogram
    histogram_titles_and_values1 <- reactiveValues(lower_limit = NULL, upper_limit = NULL, increment = NULL, 
                                                  main_title = NULL, xlabel = NULL, ylabel = NULL)
    
    observeEvent(input$applyChange1, {
        
        histogram_titles_and_values1$x_lower_limit <- input$lowerBound1
        histogram_titles_and_values1$x_upper_limit <- input$upperBound1
        histogram_titles_and_values1$increment <- input$inc1
        histogram_titles_and_values1$main_title <- input$textInp3
        histogram_titles_and_values1$xlabel <- input$textInp4
        histogram_titles_and_values1$ylabel <- input$textInp5
        
    })
    
    observeEvent(input$file$datapath, {
        
        histogram_titles_and_values1$x_lower_limit <- original_x_axis_values1()[1]
        histogram_titles_and_values1$x_upper_limit <- original_x_axis_values1()[2]
        histogram_titles_and_values1$increment <- original_x_axis_values1()[3]
        histogram_titles_and_values1$main_title <- input$textInp3
        histogram_titles_and_values1$xlabel <- input$textInp4
        histogram_titles_and_values1$ylabel <- input$textInp5
        
    })
    
    observeEvent(input$reset2, {
        
        histogram_titles_and_values1$x_lower_limit <- original_x_axis_values1()[1]
        histogram_titles_and_values1$x_upper_limit <- original_x_axis_values1()[2]
        histogram_titles_and_values1$increment <- original_x_axis_values1()[3]
        
    })
    
    original_x_axis_values1 <- reactive({
        
        minimum <- floor(min(c(dataset()[,5], localMax())))
        maximum <- ceiling(max(c(dataset()[,5],localMax())))
        increment <- 0.1
        
        c(minimum, maximum, increment)
        
    })
    
    # Absolute and Local Max Histogram
    absLocMaxPlot <- function() {
        
        # If user checks both boxes, then display both histograms
        if (input$absmaxcheck & input$localmaxcheck) {
            
            hist(dataset()[,5], breaks = binsAbsMax(), xaxt = "n", main = NULL, xlab = NULL, ylab = NULL, col = "blue", border = "white", xlim = c(histogram_titles_and_values1$x_lower_limit, histogram_titles_and_values1$x_upper_limit))
            hist(localMax(), breaks = binsLocMax(), xaxt = "n", col = rgb(0,1,0,0.5), border = "white", add = TRUE)
            
        } else if (input$absmaxcheck) {
            
            # If user checks box for absolute max histogram only, then display it
            hist(dataset()[,5], breaks = binsAbsMax(), xaxt = "n", main = NULL, xlab = NULL, ylab = NULL, col = "blue", border = "white", xlim = c(histogram_titles_and_values1$x_lower_limit, histogram_titles_and_values1$x_upper_limit))
            
        } else if (input$localmaxcheck) {
            
            # If user checks box for local max histogram only, then display it
            hist(localMax(), breaks = binsLocMax(), xaxt = "n", main = NULL, xlab = NULL, ylab = NULL, col = rgb(0,1,0,0.5), border = "white", xlim = c(histogram_titles_and_values1$x_lower_limit, histogram_titles_and_values1$x_upper_limit))
            
        }
        
    }
    
    
    ###DATASET HISTOGRAMS###
    
    # Make histogram representing the distribution of the peak wavelengths 
    output$peakhist <- renderPlot({
        
        # Generate histogram
        peakLocWavePlot()
        title(main = histogram_titles_and_values$main_title, xlab = histogram_titles_and_values$xlabel, ylab = histogram_titles_and_values$ylabel)
        axis(1, at = seq(histogram_titles_and_values$x_lower_limit, histogram_titles_and_values$x_upper_limit, by = histogram_titles_and_values$increment))
        
        
    })
    
    # Make histogram representing the distribution of the absolute/local max values
    output$abslochist <- renderPlot({
        
        # Generate Graph
        absLocMaxPlot()
        title(main = histogram_titles_and_values1$main_title, xlab = histogram_titles_and_values1$xlabel, ylab = histogram_titles_and_values1$ylabel)
        axis(1, at = seq(histogram_titles_and_values1$x_lower_limit, histogram_titles_and_values1$x_upper_limit, by = histogram_titles_and_values1$increment))
        
    })
    
    
    ###REACTIVE VALUES REQUIRED###
    longDf <- reactive({
        
        # Calculate number of columns
        numberOfColumns <- ncol(dataset_intensities())
        numberOfRows <- nrow(dataset_intensities())
        
        # Create matrix with 3 columns (wavelength, row number, predicted intensity)
        splineMatrix <- matrix(rep(0, numberOfRows*numberOfColumns*3), ncol = 3)
        colnames(splineMatrix) <- c("x","row_number","y")
        
        # Repeat row index values as many number of columns there are
        splineMatrix[,2] <- rep(1:numberOfRows, each = numberOfColumns)
        
        # Generate sequence from 1 to number of columns
        colSequence <- 1:numberOfColumns
        
        # Make for loop
        # The number of rows will be the iterations needed to retrieve coordinates based on smooth spline
        for (j in 1:numberOfRows){
            
            # Calculate row indexes to replace by shifting and using value of j
            rowIndexes <- colSequence + (j-1) * numberOfColumns
            
            # Update spline_matrix
            splineMatrix[rowIndexes,1] <- selected_path_list()[[j]]$x
            splineMatrix[rowIndexes,3] <- selected_path_list()[[j]]$y
            
        }
        
        splineDf <- as.data.frame(splineMatrix)
        splineDf
        
    })
    
    
    
    ###FUNCTIONS: SMOOTH SPLINE###
    
    # Smooth Spline for all rows
    allSmooth <- function() {
        
        # Generate vector consisting of values 1 to number of rows
        list_length <- length(selected_path_list()) 
        
        plot(dataset_wavelengths(), main = "Dataset Smooth Spline Lines", xlab = "Wavelength", ylab = "Normal Intensity", type = "n", xlim = c(min(dataset_wavelengths()),max(dataset_wavelengths())), ylim = c(0,1))
        
        for (k in 1:list_length) {
            
            lines(selected_path_list()[[k]], col = "red", lwd = 2)
        
            
        }
        
    }
    
    # Path 1 Normalization Smooth Spline
    smoothSplPlot1 <- function() {
        
        # Plot points for given row on graph
        plot(dataset_wavelengths(), y_values1(), xlab = "Wavelength", ylab = "Normal Intensity", main = paste("Normal Intensity vs. Wavelength \n X =", coordinates()[1],", Y = ", coordinates()[2]))
        
        # Plot smooth spline line, absolute max point, and local max points
        lines(splinesmoothfit1(), col = "red", lwd = 2)
        points(abs_max_coordinates1()[1], abs_max_coordinates1()[2], pch = 20, col = "blue", cex = 3)
        points(local_max_points1()[1,],local_max_points1()[2,], pch = 20, col = "green", cex = 3)
        legend("topright", c("Smoothing Spline","Absolute Max", "Local Max"),  lwd = c(2,NA,NA), col = c("red", "blue","green"), pch = c(NA,20,20), pt.cex = c(NA,3,3)) 
        
    }
    
    # Path 2 Normalization Smooth Spline
    smoothSplPlot2 <- function() {
            
        # Plot points for given row on graph
        plot(dataset_wavelengths(), y_values2(), xlab = "Wavelength", ylab = "Normal Intensity", main = paste("Normal Intensity vs. Wavelength \n X =", coordinates()[1],", Y = ", coordinates()[2]))
        
        # Plot smooth spline line, absolute max point, and local max points
        lines(splinesmoothfit2(), col = "red", lwd = 2)
        points(abs_max_coordinates2()[1], abs_max_coordinates2()[2], pch = 20, col = "blue", cex = 3)
        points(local_max_points2()[1,],local_max_points2()[2,], pch = 20, col = "green", cex = 3)
        legend("topright", c("Smoothing Spline","Absolute Max", "Local Max"),  lwd = c(2,NA,NA), col = c("red", "blue","green"), pch = c(NA,20,20), pt.cex = c(NA,3,3))
        
    }
    
    ###SMOOTH SPLINE INTERPOLATION GRAPHS###

    # Tool tip
    output$tooltip <- renderUI({
        
        req(input$SmoothHover)
        
        verbatimTextOutput("selectedRowNum")
        
    })
    
    # Value of row
    output$selectedRowNum <- renderPrint({
        
        hover <- input$SmoothHover
        
        rowNum <- unique(nearPoints(longDf(), hover, xvar = 'x', yvar = 'y', threshold = 7)$row_number)[1]
        
        if (is.na(rowNum)) {
            print("Bring cursor closer to a line.")
        } else {
            paste("Row Number:", rowNum)
        }
        
    })
    
    # Create smoothing spline lines
    output$datasetSplinesPath <- renderPlot({
        
        allSmooth()
        
    })
    
    
    observeEvent(input$go1, {
        
        if(is.na(input$rowIndex)) {
            
            showModal(modalDialog(
                title = "Error",
                "Make sure to enter a row number before pressing \"Go!\".",
                easyClose = TRUE
            ))
            
        }
        
    })
    
    # Create smoothing spline line and plot it
    output$smoothLine <- renderPlot({
        
        if(is.na(input$rowIndex)) {return()}
        
        if(input$pathChoice1 == "Path 1") {
            smoothSplPlot1()
        } else if (input$pathChoice1 == "Path 2") {
            smoothSplPlot2()
        }
        
    })
    
    # Create zoomed-in smoothing spline line
    output$zoomsmooth <- renderPlot({
        
        if(is.na(input$rowIndex)) {return()}
        
        if (input$pathChoice1 == "Path 1") {
            
            # Plot smooth spline line on graph
            plot(dataset_wavelengths(), y_values1(), xlab = "Wavelength", ylab = "Normal Intensity", xlim = window_margins$x, ylim = window_margins$y)
            
            # Plot smooth spline line, absolute max point, and local max points
            lines(splinesmoothfit1(), col = "red", lwd = 2)
            points(abs_max_coordinates1()[1], abs_max_coordinates1()[2], pch = 20, col = "blue", cex = 3)
            points(local_max_points1()[1,],local_max_points1()[2,], pch = 20, col = "green", cex = 3)
            legend("topright", c("Smoothing Spline","Absolute Max", "Local Max"),  lwd = c(2,NA,NA), col = c("red", "blue","green"), pch = c(NA,20,20), pt.cex = c(NA,3,3))
            
        } else if (input$pathChoice1 == "Path 2") {
            
            # Plot smooth spline line on graph
            plot(dataset_wavelengths(), y_values2(), xlab = "Wavelength", ylab = "Normal Intensity", xlim = window_margins$x, ylim = window_margins$y)
            
            # Plot smooth spline line, absolute max point, and local max points
            lines(splinesmoothfit2(), col = "red", lwd = 2)
            points(abs_max_coordinates2()[1], abs_max_coordinates2()[2], pch = 20, col = "blue", cex = 3)
            points(local_max_points2()[1,],local_max_points2()[2,], pch = 20, col = "green", cex = 3)
            legend("topright", c("Smoothing Spline","Absolute Max", "Local Max"),  lwd = c(2,NA,NA), col = c("red", "blue","green"), pch = c(NA,20,20), pt.cex = c(NA,3,3))
            
        }
        
    })
    
    # Display effects of brush
    observe({
        
        brush <- input$smoothPlot_brush
        
        if (!is.null(brush)) {
            window_margins$x <- c(brush$xmin, brush$xmax)
            window_margins$y <- c(brush$ymin, brush$ymax)
        } else {
            window_margins$x <- NULL
            window_margins$y <- NULL
        }
        
    })

    
    ###REACTIVE VALUES FOR DOWNLOAD BUTTONS###
    
    # Table 1
    df_csv_t1 <- reactive({
        
        test <- dataset()
        colnames(test) <- c("X","Y","Wavelength","Intensity", "Normal Intensity")
        test
        
    })
    
    # Table 2
    df_csv_t2 <- reactive({
        
        test <- bigger_data()[,-(1:5)]
        colnames(test) <- c(paste("Wavelength 1 Intensity",input$wavelength1), paste("Wavelength 2 Intensity",input$wavelength2), "Wavelength 1 Normal Intensity", "Wavelength 2 Normal Intensity", "Relative Intensity")
        test
        
    })
    
    
    ###DOWNLOAD BUTTONS###
    
    # Download button for heat map of normalized max intensity
    output$downloadgraph <- downloadHandler(
        
        # Allow user to make file name
        filename = function() {
            
            # SVG is selected
            if (input$select == 1) {
                
                # Name file
                paste0("Normal Max Intensity Plot", ".svg")
                
            # JPEG is selected
            } else if (input$select == 2) {
                
                # Name file
                paste0("Normal Max Intensity Plot", ".jpeg")
                
            # PNG is selected   
            } else if (input$select == 3) {
                
                # Name file
                paste0("Normal Max Intensity Plot", ".png")
                
            }
            
        },
        
        # Content for file
        content = function(file){
            
            # SVG is selected
            if (input$select == 1) {
                
                # Make file SVG
                svg(file)
                
            # JPEG is selected
            } else if (input$select == 2) {
                
                # Make file JPEG
                jpeg(file, quality = 100)
                
            # PNG is selected   
            } else if (input$select == 3) {
                
                # Make file PNG
                png(file)
                
            }
            
            # Show graph
            if(!input$checkTitle) {print(normMaxIntPlot())}
            else {print(normMaxIntPlot() + ggtitle("Normal Max Intensity Plot"))}
            
            dev.off()
            
        }
        
    )
    
    # Download button for Peak Wavelength Heat Map Plot
    output$downloadgraph1 <- downloadHandler(
        
        # Allow user to make file name
        filename = function(){
            
            # SVG is selected
            if (input$select == 1) {
                
                # Name file
                paste0("Peak Wavelength Plot", ".svg")
                
            # JPEG is selected
            } else if (input$select == 2) {
                
                # Name file
                paste0("Peak Wavelength Plot", ".jpeg")
                
            # PNG is selected   
            } else if (input$select == 3) {
                
                # Name file
                paste0("Peak Wavelength Plot", ".png")
                
            }
            
        },
        
        # Content for file
        content = function(file){
            
            # SVG is selected
            if (input$select == 1) {
                
                # Make file SVG
                svg(file)
                
            # JPEG is selected
            } else if (input$select == 2) {
                
                # Make file JPEG
                jpeg(file, quality = 100)
                
            # PNG is selected   
            } else if (input$select == 3) {
                
                # Make file PNG
                png(file)
                
            }
            
            if(!input$checkTitle1) {
                
                print(peakWavePlot())
                
            } else {
                
                print(peakWavePlot() + ggtitle("Peak Wavelength Plot"))}
            
            dev.off()    
        }
        
    )
    
    # Download button for Relative Intensity Heat Map Plot
    output$downloadgraph2 <- downloadHandler(
        
        # Allow user to make file name
        filename = function(){
            
            # SVG is selected
            if (input$select == 1) {
                
                # Name file
                paste0("Relative Intensity Plot", ".svg")
                
            # JPEG is selected
            } else if (input$select == 2) {
                
                # Name file
                paste0("Relative Intensity Plot", ".jpeg")
                
            # PNG is selected   
            } else if (input$select == 3) {
                
                # Name file
                paste0("Relative Intensity Plot", ".png")
                
            }
            
        },
        
        # Content for file
        content = function(file){
           
            # SVG is selected
            if (input$select == 1) {
                
                # Make file SVG
                svg(file)
                
            # JPEG is selected
            } else if (input$select == 2) {
                
                # Make file JPEG
                jpeg(file, quality = 100)
                
            # PNG is selected   
            } else if (input$select == 3) {
                
                # Make file PNG
                png(file)
                
            }
            
            print(relIntPlot())
            
            dev.off()    
        }
        
    )
    
    # Download button for Wavelength 1 Intensity Heat Map
    output$downloadgraph3 <- downloadHandler(
        
        # Allow user to make file name
        filename = function(){
            
            # SVG is selected
            if (input$select == 1) {
                
                # Name file
                paste0("Wavelength 1 Intensity Plot", ".svg")
                
            # JPEG is selected
            } else if (input$select == 2) {
                
                # Name file
                paste0("Wavelength 1 Intensity Plot", ".jpeg")
                
            # PNG is selected   
            } else if (input$select == 3) {
                
                # Name file
                paste0("Wavelength 1 Intensity Plot", ".png")
                
            }
            
        },
        
        # Content for file
        content = function(file){
            
            # SVG is selected
            if (input$select == 1) {
                
                # Make file SVG
                svg(file)
                
            # JPEG is selected
            } else if (input$select == 2) {
                
                # Make file JPEG
                jpeg(file, quality = 100)
                
            # PNG is selected   
            } else if (input$select == 3) {
                
                # Make file PNG
                png(file)
                
            }
            
            print(wave1IntPlot())
            
            dev.off()    
        }
        
    )
    
    # Download button for Wavelength 2 Intensity Heat Map
    output$downloadgraph4 <- downloadHandler(
        
        # Allow user to make file name
        filename = function(){
            
            # SVG is selected
            if (input$select == 1) {
                
                # Name file
                paste0("Wavelength 2 Intensity Plot", ".svg")
                
            # JPEG is selected
            } else if (input$select == 2) {
                
                # Name file
                paste0("Wavelength 2 Intensity Plot", ".jpeg")
                
            # PNG is selected   
            } else if (input$select == 3) {
                
                # Name file
                paste0("Wavelength 2 Intensity Plot", ".png")
                
            }
            
        },
        
        # Content for file
        content = function(file){
            
            # SVG is selected
            if (input$select == 1) {
                
                # Make file SVG
                svg(file)
                
            # JPEG is selected
            } else if (input$select == 2) {
                
                # Make file JPEG
                jpeg(file, quality = 100)
                
            # PNG is selected   
            } else if (input$select == 3) {
                
                # Make file PNG
                png(file)
                
            }
            
            print(wave2IntPlot())
            
            dev.off()    
        }
        
    )
    
    # Download button for CSV of Table 1
    output$downloadtable <- downloadHandler(
        
        # Allow user to make file name
        filename = function() {
            
            # Name of file
            paste0("Table 1", ".csv")
            
        },
        
        # Content for file
        content = function(file) {
            
            # Create CSV
            write.csv(df_csv_t1(), file, row.names = FALSE, sep = ',')
            
        }
        
    )
    
    # Download button for CSV of Table 2
    output$downloadtable1 <- downloadHandler(
        
        # Allow user to make file name
        filename = function() {
            
            # Name of file
            paste0("Table 2", ".csv")
            
        },
        
        # Content for file
        content = function(file) {
            
            # Create CSV File
            write.csv(df_csv_t2(), file, row.names = FALSE, sep = ',')
            
        }
        
    )
    
    # Download button for Peak/Local Peak Wavelength histogram
    output$downloadHist <- downloadHandler(
        
        # Allow user to make file name
        filename = function() {
            
            # SVG is selected
            if (input$select == 1) {
                
                # Name file
                paste0("Peak Wavelength Histogram", ".svg")
                
            # JPEG is selected
            } else if (input$select == 2) {
                
                # Name file
                paste0("Peak Wavelength Histogram", ".jpeg")
                
            # PNG is selected   
            } else if (input$select == 3) {
                
                # Name file
                paste0("Peak Wavelength Histogram", ".png")
                
            }
            
        },
        
        # Content for file
        content = function(file){
            
            # SVG is selected
            if (input$select == 1) {
                
                # Make file SVG
                svg(file)
                
            # JPEG is selected
            } else if (input$select == 2) {
                
                # Make file JPEG
                jpeg(file, quality = 100)
                
            # PNG is selected   
            } else if (input$select == 3) {
                
                # Make file PNG
                png(file)
                
            }
            
            peakLocWavePlot()
            title(main = histogram_titles_and_values$main_title, xlab = histogram_titles_and_values$xlabel, ylab = histogram_titles_and_values$ylabel)
            axis(1, at = seq(histogram_titles_and_values$x_lower_limit, histogram_titles_and_values$x_upper_limit, by = histogram_titles_and_values$increment))
            dev.off()    
        }
        
    )
    
    # Download button for absolute/local max histogram
    output$downloadHist1 <- downloadHandler(
        
        # Allow user to make a file name
        filename = function() {
            
            # SVG is selected
            if (input$select == 1) {
                
                # Name file
                paste0("Absolute and Local Max Histogram", ".svg")
                
            # JPEG is selected
            } else if (input$select == 2) {
                
                # Name file
                paste0("Absolute and Local Max Histogram", ".jpeg")
                
            # PNG is selected   
            } else if (input$select == 3) {
                
                # Name file
                paste0("Absolute and Local Max Histogram", ".png")
                
            }
            
        },
        
        # Content for file
        content = function(file) {
            
            # SVG is selected
            if (input$select == 1) {
                
                # Make file SVG
                svg(file)
                
            # JPEG is selected
            } else if (input$select == 2) {
                
                # Make file JPEG
                jpeg(file, quality = 100)
                
            # PNG is selected   
            } else if (input$select == 3) {
                
                # Make file PNG
                png(file)
                
            }
            
            absLocMaxPlot()
            title(main = histogram_titles_and_values1$main_title, xlab = histogram_titles_and_values1$xlabel, ylab = histogram_titles_and_values1$ylabel)
            axis(1, at = seq(histogram_titles_and_values1$x_lower_limit, histogram_titles_and_values1$x_upper_limit, by = histogram_titles_and_values1$increment))
            dev.off()   
        }
        
    )
    
    # Download button for current spectrum plot
    output$downloadSpectrum <- downloadHandler(
        
        # Create file name
        filename = function() {
            
            # SVG is selected
            if (input$select == 1) {
                
                # Name file
                paste0("Spectrum Plot", ".svg")
                
            # JPEG is selected
            } else if (input$select == 2) {
                
                # Name file
                paste0("Spectrum Plot", ".jpeg")
                
            # PNG is selected   
            } else if (input$select == 3) {
                
                # Name file
                paste0("Spectrum Plot", ".png")
                
            }
            
        },
        
        # Content for file
        content = function(file) {
            
            # SVG is selected
            if (input$select == 1) {
                
                # Make file SVG
                svg(file)
                
            # JPEG is selected
            } else if (input$select == 2) {
                
                # Make file JPEG
                jpeg(file, quality = 100)
                
            # PNG is selected   
            } else if (input$select == 3) {
                
                # Make file PNG
                png(file)
                
            }
            
            if(input$pathChoice1 == "Path 1") {
                smoothSplPlot1()
            } else if (input$pathChoice1 == "Path 2") {
                smoothSplPlot2()
            }
            
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
    
    # Output information for interactive Heat map (normal max int.)
    output$info1 <- renderPrint({
        
        dataframe <- nearPoints(df_info12()[,-c(3,4)], input$click, threshold = 15, maxpoints = 1)
        number_of_rows <- nrow(dataframe)
        
        if (number_of_rows == 0) {
            
            print("Click on or near the center of a tile.")
            
        } else {
            
            # Display row information
            dataframe
            
        }
        
    })
    
    # Output information for interactive heat map (peak wavelength)
    output$info2 <- renderPrint({
        
        dataframe <- nearPoints(df_info12()[,-c(4,5)], input$click, threshold = 15, maxpoints = 1)
        number_of_rows <- nrow(dataframe)
        
        if (number_of_rows == 0) {
            
            print("Click on or near the center of a tile.")
            
        } else {
            
            # Display row information
            dataframe
            
        }
        
    })
    
    # Output information for interactive heat map (Relative Intensity)
    output$info3 <- renderPrint({
        
        if (is.na(input$wavelength1) || is.na(input$wavelength2)) {return(print("Input wavelengths to generate plots."))}
        
        dataframe <- nearPoints(df_info345()[,-c(3,4,5)], input$click1, threshold = 15, maxpoints = 1)
        number_of_rows <- nrow(dataframe)
        
        if (number_of_rows == 0) {
                
            print("Click on or near the center of a tile.")
                
        } else {
                
            # Display row information
            dataframe
                
        }
        
    })
    
    # Output Information for interactive heat map (Wavelength 1)
    output$info4 <- renderPrint({
        
        if (is.na(input$wavelength1) || is.na(input$wavelength2)) {return(print("Input wavelengths to generate plots."))}
        
        dataframe <- nearPoints(df_info345()[,-c(3:5,7:9)], input$click2, threshold = 15, maxpoints = 1)
        number_of_rows <- nrow(dataframe)
        
        if (number_of_rows == 0) {
            
            print("Click on or near the center of a tile.")
            
        } else {
            
            # Display row information
            dataframe
            
        }
        
        
    })
    
    # Output Information for interactive heat map (Wavelength 2)
    output$info5 <- renderPrint({
        
        if (is.na(input$wavelength1) || is.na(input$wavelength2)) {return(print("Input wavelengths to generate plots."))}
        
        dataframe <- nearPoints(df_info345()[,-c(3:6,8:9)], input$click3, threshold = 15, maxpoints = 1)
        number_of_rows <- nrow(dataframe)
        
        if (number_of_rows == 0) {
            
            print("Click on or near the center of a tile.")
            
        } else {
            
            # Display row information
            dataframe
            
        }
        
    })
    
    # Output Information for interactive peak/local peak wavelength histogram
    output$info6 <- renderPrint({
        
        # If no bin is clicked on
        if (is.null(input$click4$x)) {
            
            return(print("Click on any bin."))
            
        }
        
        # Create reactive value for current peak wavelength histogram
        z <- hist(dataset()[,3], breaks = binsPeakWave())
        
        # Create reactive value for current local peak wavelength histogram
        y <- hist(localPeakWave(), breaks = binsLocWave())
        
        # Determine indices of limits where mouse is between (peak wavelength histogram)
        lower_lim_ind_peak <- tail(which(z$breaks <= input$click4$x),1)
        higher_lim_ind_peak <- head(which(z$breaks >= input$click4$x),1)
        
        # Determine indices of limits where mouse is between (local peak wavelength histogram)
        lower_lim_ind_local <- tail(which(y$breaks <= input$click4$x),1)
        higher_lim_ind_local <- head(which(y$breaks >= input$click4$x),1)
        
        # Find the limits for each histogram
        lower_lim_peak <- z$breaks[lower_lim_ind_peak]
        higher_lim_peak <- z$breaks[higher_lim_ind_peak]
        lower_lim_local <- y$breaks[lower_lim_ind_local]
        higher_lim_local <- y$breaks[higher_lim_ind_local]
        
        # Determine the count
        peak_count <- z$counts[lower_lim_ind_peak]
        local_count <- y$counts[lower_lim_ind_local]
        
        # Create vectors for each histogram consisting of lower and upper limits, midpoint, and frequency
        peak_info <- c(lower_lim_peak, higher_lim_peak, z$mids[lower_lim_peak < z$mids & z$mids < higher_lim_peak], peak_count)
        local_info <- c(lower_lim_local, higher_lim_local, y$mids[lower_lim_local < y$mids & y$mids < higher_lim_local], local_count)
        
        # Create dataframe
        df <- cbind(peak_info, local_info)
        
        # If both checkboxes are marked
        if (input$peakcheck & input$localpeakcheck) {
            
            # If the user clicks within the range of either one or both histograms
            if(input$click4$x < min(c(binsPeakWave(),binsLocWave())) | input$click4$x > max(c(binsPeakWave(),binsLocWave()))) {
                
                # If user clicks out of the range for both histograms
                print("Click on a bin that is within the range of either histogram.")
                
                
            } else if (input$click4$x < min(binsPeakWave()) | input$click4$x > max(binsPeakWave())) {
                
                # If the user clicks on a green bin that is out of the range of the blue histogram
                df[,1] <- NA * integer(nrow(df))
                colnames(df) <- c("Peak Wavelength", "Local Peak Wavelength")
                rownames(df) <- c("Lower Bin Limit", "Higher Bin Limit", "Midpoint", "Frequency")
                df
                
            } else if (input$click4$x < min(binsLocWave()) | input$click4$x > max(binsLocWave())) {
                
                # If the user clicks on a blue bin that is out of the range of the green histogram
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
            
            # If user only checks box for peak wavelength histogram
            # If click is outside of range
            if (input$click4$x < min(binsPeakWave()) | input$click4$x > max(binsPeakWave())) {
                
                print("Click on a bin within the range of the histogram")
                
            } else {
                
                # Subset the first column
                df <- df[,1]
                names(df) <- c("Lower Bin Limit", "Higher Bin Limit", "Midpoint", "Frequency")
                df
                
            }
            
        } else if (input$localpeakcheck) {
            
            # If user only checks box for local peak wavelength histogram
            # If click is outside of range
            if (input$click4$x < min(binsLocWave()) | input$click4$x > max(binsLocWave())) {
                
                print("Click on a bin within the range of the histogram")
                
            } else {
                
                # Subset the first column
                df <- df[,2]
                names(df) <- c("Lower Bin Limit", "Higher Bin Limit", "Midpoint", "Frequency")
                df
                
            }
            
            
        }
        
    })
    
    # Output Information for interactive absolute/local max values histogram
    output$info7 <- renderPrint({
        
        # If no bin is clicked on
        if (is.null(input$click5$x)) {
            
           return(print("Click on any bin."))
            
        }
        
        # Create reactive value for current absolute max histogram
        z <- hist(dataset()[,5], breaks = binsAbsMax())
        
        # Create reactive value for current local max histogram
        y <- hist(localMax(), breaks = binsLocMax())
        
        # Determine indices of limits where mouse is between (peak wavelength histogram)
        lower_lim_ind_abs <- tail(which(z$breaks <= input$click5$x),1)
        higher_lim_ind_abs <- head(which(z$breaks >= input$click5$x),1)
        
        # Determine indices of limits where mouse is between (local peak wavelength histogram)
        lower_lim_ind_local <- tail(which(y$breaks <= input$click5$x),1)
        higher_lim_ind_local <- head(which(y$breaks >= input$click5$x),1)
        
        # Find the limits for each histogram
        lower_lim_abs <- z$breaks[lower_lim_ind_abs]
        higher_lim_abs <- z$breaks[higher_lim_ind_abs]
        lower_lim_local <- y$breaks[lower_lim_ind_local]
        higher_lim_local <- y$breaks[higher_lim_ind_local]
        
        # Determine the count for each bin that was clicked on using lower limit index
        abs_count <- z$counts[lower_lim_ind_abs]
        local_count <- y$counts[lower_lim_ind_local]
        
        # Create vectors for each histogram consisting of lower and upper limits, midpoint, and frequency
        abs_info <- c(lower_lim_abs, higher_lim_abs, z$mids[lower_lim_abs < z$mids & z$mids < higher_lim_abs], abs_count)
        local_info <- c(lower_lim_local, higher_lim_local, y$mids[lower_lim_local < y$mids & y$mids < higher_lim_local], local_count)
        
        # Create dataframe
        df <- cbind(abs_info, local_info)
        
        # If both checkboxes are marked
        if (input$absmaxcheck & input$localmaxcheck) {
            
            # If the user clicks within the range of either one or both histograms
            if(input$click5$x < min(c(binsAbsMax(),binsLocMax())) | input$click5$x > max(c(binsAbsMax(),binsLocMax()))) {
                
                # If user clicks out of the range for both histograms
                print("Click on a bin that is within the range of either histogram.")
                
                
            } else if (input$click5$x < min(binsAbsMax()) | input$click5$x > max(binsAbsMax())) {
                
                # If the user clicks on a green bin that is out of the range of the blue histogram
                df[,1] <- NA * integer(nrow(df))
                colnames(df) <- c("Absolute Max", "Local Max")
                rownames(df) <- c("Lower Bin Limit", "Higher Bin Limit", "Midpoint", "Frequency")
                df
                
            } else if (input$click5$x < min(binsLocMax()) | input$click5$x > max(binsLocMax())) {
                
                # If the user clicks on a blue bin that is out of the range of the green histogram
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
            
            # If user only checks box for absolute max histogram
            # If click is outside of range
            if (input$click5$x < min(binsAbsMax()) | input$click5$x > max(binsAbsMax())) {
                
                print("Click on a bin within the range of the histogram")
                
            } else {
                
                # Subset the first column
                df <- df[,1]
                names(df) <- c("Lower Bin Limit", "Higher Bin Limit", "Midpoint", "Frequency")
                df
                
            }
            
        } else if (input$localmaxcheck) {
            
            # If user only checks box for local max histogram
            # If click is outside of range
            if (input$click5$x < min(binsLocMax()) | input$click5$x > max(binsLocMax())) {
                
                print("Click on a bin within the range of the histogram")
                
            } else {
                
                # Subset the first column
                df <- df[,2]
                names(df) <- c("Lower Bin Limit", "Higher Bin Limit", "Midpoint", "Frequency")
                df
                
            }
            
        }
        
    })
    
    # Output information for smoothing spline: local max through brush
    output$info8 <- renderPrint({
        
        if (is.na(input$rowIndex)) {return(print("Enter a row number to generate plots."))}
        
        req(input$pathChoice1)
        
        if (input$pathChoice1 == "Path 1") {
            
            # Determine x and y values of local max
            Wavelength <- local_max_points1()[1,]
            Normal_Intensity <- local_max_points1()[2,]
            
            dataframe <- brushedPoints(data.frame(Wavelength, Normal_Intensity), input$smoothPlot_brush,
                                       xvar = "Wavelength", yvar = "Normal_Intensity")
            
            number_of_rows <- nrow(dataframe)
            
            if (number_of_rows == 0) {
                
                print("Brush over one or more green points to display local max coordinates.")
                
            } else  {
                
                # Print local max/min points
                dataframe
                
            }
            
        } else if (input$pathChoice1 == "Path 2") {
            
            # Determine x and y values of local max
            Wavelength <- local_max_points2()[1,]
            Normal_Intensity <- local_max_points2()[2,]
            
            dataframe <- brushedPoints(data.frame(Wavelength, Normal_Intensity), input$smoothPlot_brush,
                                       xvar = "Wavelength", yvar = "Normal_Intensity")
            
            number_of_rows <- nrow(dataframe)
            
            if (number_of_rows == 0) {
                
                print("Brush over one or more green points to display local max coordinates.")
                
            } else {
                
                # Print local max/min points
                dataframe
                
            }
        }
            
        
        
    })
    
    
    ###OUTPUT TEXT###
    
    # Display class width for Peak Wavelength Histogram
    output$classwidth <- renderText({
        
        # Display Class Width
        paste("Bin width is", (max(binsPeakWave()) - min(binsPeakWave()))/(length(binsPeakWave())-1), "nm")
        
    })
    
    # Display class width for Local Peak Wavelength Histogram
    output$classwidth1 <- renderText({
        
        # Display Class Width
        paste("Bin width is",
              (max(binsLocWave()) - min(binsLocWave()))/(length(binsLocWave())-1), "nm")
        
    })
    
    # Display class width for Absolute Max Histogram
    output$classwidth2 <- renderText({
        
        # Display Class Width
        paste("Bin width is",
              (max(binsAbsMax()) - min(binsAbsMax()))/(length(binsAbsMax())-1))
        
    })
    
    # Display class width for Local Max Histogram
    output$classwidth3 <- renderText({
        
        # Display Class Width
        paste("Bin width is", 
              (max(binsLocMax()) - min(binsLocMax()))/(length(binsLocMax())-1))
        
    })
    
    
    ###REACTIVE VALUES FOR DATA TABLES###
    
    df_t1 <- reactive({
        test <- dataset()
        colnames(test) <- c("X", 
                            "Y",
                            paste0("&lambda;",tags$sub("peak")),
                            "Intensity",
                            "Normal Intensity")
        test
    })
    
    df_t2 <- reactive({
        
        test <- bigger_data()[,-(1:5)]
        colnames(test) <- c(paste(paste0("&lambda;", tags$sub(1)), "Intensity"), 
                            paste(paste0("&lambda;", tags$sub(2)), "Intensity"),
                            paste("Normal", paste0("&lambda;", tags$sub(1)), "Intensity"),
                            paste("Normal", paste0("&lambda;", tags$sub(2)), "Intensity"),
                            "Relative Intensity")
        test
        
    })
    
    
    ###DATA TABLES###
    
    # Table 1 Output
    output$table <- renderTable({
        
        # Print df_t1
        df_t1()
        
    }, sanitize.text.function = function(x) x)
    
    
    # Table 2 Output
    output$table1 <- renderTable({
        
        df_t2()
        
    }, sanitize.text.function = function(x) x)
    
}


# Run the application 
shinyApp(ui = ui, server = server)
