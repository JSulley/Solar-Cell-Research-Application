#Load all needed packages and r scripts
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
        fileInput("file", h3("File Upload", align = "center"), accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
        hr(),
        
        #Create select box for file format options
        selectInput("select", label = h3("Plot Download Format"), choices = list("SVG" = 1, "JPEG" = 2, "PNG" = 3), selected = 1),
        hr(),
        
        #Dynamic portion
        #Make two boxes that accept numeric entries for two wavelength values
        h3("Wavelength Input", align = "center"),
        p('Input two distinct wavelengths', align = "center"),
        numericInput('wavelength1', HTML(paste("Enter", paste0("&lambda;",tags$sub(1)))), value = NULL, min = 0),
        numericInput('wavelength2', HTML(paste("Enter", paste0("&lambda;",tags$sub(2)))), value = NULL, min = 0),
        
        #Create Action button for user
        actionButton("go","Go!"),
        hr(),
        
        #Make numeric entry for row number to view smoothing spline interpolation graph with scatterplot
        h4("Smooth Spline Interpolation", align = "center"),
        h4("Display Input", align = "center"),
        numericInput('rowIndex', "Enter row number" , value = NULL, min = 0),
        
        #Create action button for it
        actionButton("go1", "Go!"),
        hr(),
        
        
        #Create remaining menu items 
        menuItem("Dataset Plots", tabName = "datasetDash"),
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
                    #Greet the user
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
                                    p(strong("Update:"), "The program now accepts files that are in TSV format (tab-separated values). It assumes the same structure.")
                           ),
                           
                           #Note regarding downloading plots.
                           tabPanel("Downloading Plots/Tables",
                                    p("You are able to download the plots/tables displayed!"),
                                    p("The plots are downloaded in SVG format, while the tables are downloaded in CSV format."),
                                    p("For Table 2 to appear, you", strong("must"), "input two different wavelengths first."),
                                    hr(), 
                                    
                                    #Note what calculation is being done for relative intensity
                                    p(strong("Note:"), "For the relative intensity, it is the ratio of", HTML(paste0("&lambda;",tags$sub(1))) ,"intensity to",
                                      HTML(paste0("&lambda;",tags$sub(2))) ,"intensity"),
                                    p(strong("Update:"), "The plots can now be downloaded in JPEG and PNG format. Under the \"Plot Download Format\" section, click on the box and select the format you want!")),
                           
                           #Note recent features
                           tabPanel("More Features",
                                    
                                    #Interactive Peak Wavelength Histogram
                                    h3("Peak/Local Peak Wavelength Interactive Histograms"),
                                    p("The distribution of the peak wavelengths can be now viewed in the tab labeled 'Dataset Histograms'. Make sure to upload the file first!"),
                                    p(strong("Update:"), "The Peak Wavelength Interactive Histogram now consists of the Local Peak Wavelength Histogram . You can select to display one histogram or both."),
                                    p("There is also a separate slider for the Local Peak Wavelength Histogram and is labeled accordingly. When both histograms are present, available information is based on where you choose to click at."),
                                    p("When one histogram is displayed, only information for that histogram will be displayed. Give it a try!"),
                                    p("Class width for each histogram is underneath the slider. The units are in nanometers."),
                                    img(src = "Peak and Local Peak Hist.PNG", height = 520, width = 900),
                                    hr(),
                                    
                                    #Smoothing Spline Interpolation Output
                                    h3("Smoothing Spline View"),
                                    p("Now for any coordinate, the scatterplot of the normal intensities with the smoothing spline line graphed can be viewed!"),
                                    p("Just simply enter the row number, and the program will generate the plot."),
                                    p("Where is the row number? To view it, click on the tile of interest. Then the first number to the left is it."),
                                    p("Here is an example:"),
                                    img(src = "Example Row Number.png", height = 200, width = 350),
                                    p("The row number is 1, which would then be inputted in the box labeled 'Smooth Spline Interpolation Display Input'. Now, press 'Go!' and click on the 'Export AllSpectra' tab to see the graph!"),
                                    hr(),
                           
                                    #Interactive Smooth Spline
                                    h3("Interactive Smoothing Spline"),
                                    p("The smoothing spline plot is now interactive! You can click and drag over a portion of the plot, and the program will display what is inside the region with the window margins specified."),
                                    p("In other words, you can zoom into the plot with the result presented underneath it."),
                                    p("Additionally, the region specified can be moved around by clicking and holding the specified region and placing it over a different area."),
                                    p("The local extrema for the smoothing spline plot are now displayed. When the selected region covers any of these points, the program displays their values."),
                                    p("Here is an example:"),
                                    img(src = "Zoom Spline.PNG", height = 520, width = 870),
                                    hr(),
                                    
                                    h3("Absolute/Local Max Interactive Histograms"),
                                    p("These histograms display the distribution of the absolute max values and local max values."),
                                    p("This interactive plot shares the same features as the other one: one or both histograms can be seen, each histogram has its own slider, class width (which is unitless) is displayed for each one, and the information underneath is presented based on which histogram(s) is selected."),
                                    img(src = "Absolute and Local Max Hist.PNG", height = 520, width = 900))
                    )   
                    
                )
        ),
        
        #Content for "Dataset Plots"
        tabItem(tabName = "datasetDash",
                
                #Download button for Normalized Absolute Max Intensity Heat Map
                downloadButton("downloadgraph", "Download Normal Max Intensity Plot"),
                
                #Output heat map for Normalized Absolute Max Intensity and Information
                plotOutput("normMaxIntHeatMap", click = "click") %>% withSpinner(getOption("spinner.type", 8)),
                verbatimTextOutput("info1"),
                
                #Add border
                hr(),
                
                #Make download button for Peak Wavelength Heat Map
                downloadButton("downloadgraph1", "Download Peak Wavelength Plot"),
                
                #Output heat map for Peak Wavelength and Information
                plotOutput("peakWaveHeatMap", click = "click") %>% withSpinner(getOption("spinner.type", 8)),
                verbatimTextOutput("info2")),
        
        #Content for Relative/Wavelength Intensity Plots
        tabItem(tabName = "relativeDash",
                
                #Make fluid row for interactive relative intensity map 
                fluidRow(
                    
                    h3(strong(em("Note:")), "Did you change the inputs for one or both wavelengths, but the plot(s) look(s) the same/did not change?
                   Make sure you pressed Go! after updating any wavelength values!", align = "center"),
                    
                    column(12,
                           
                           #Make download button for Relative Intensity Heat Map
                           downloadButton("downloadgraph2", "Download Relative Intensity Plot"),
                           
                           #Heat Map plot of Relative Intensity given two values
                           plotOutput("RelIntHeatMap", click = "click1") %>% withSpinner(getOption("spinner.type", 8)),
                           verbatimTextOutput("info3")
                           
                    ),
                    
                    
                    #Make fluid row for interactive intensities for both inputted wavelengths
                    fluidRow(
                        
                        column(6,
                               
                               #Make download button for heat map of inputted wavelength 1 
                               downloadButton("downloadgraph3", "Download Wavelength 1 Intensity Plot"),
                               
                               #Heat map plot for intensities of wavelength 1
                               plotOutput("Wave1IntenHeatMap", click = "click2") %>% withSpinner(getOption("spinner.type", 8)),
                               verbatimTextOutput("info4"),
                        ),
                        
                        column(6,
                               
                               #Make download button for heat map of inputted wavelength 1 
                               downloadButton("downloadgraph4", "Download Wavelength 2 Intensity Plot"),
                               
                               #Heat map plot for intensities of wavelength 1
                               plotOutput("Wave2IntenHeatMap", click = "click3") %>% withSpinner(getOption("spinner.type", 8)),
                               verbatimTextOutput("info5"))
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
                           tableOutput('table')),
                    
                    #Table consisting columns for the output values for given wavelength values
                    column(6,
                           
                           #Display title
                           h2("Table 2"),
                           
                           #Download button for second table (Wavelength 1, Wavelength 2, Relative Intensity)
                           downloadButton("downloadtable1", "Download Table 2"),
                           
                           #Output table regarding inputted wavelengths
                           tableOutput('table1')),
                )
        ),
        
        #Content for "Dataset Histograms" tab
        tabItem(tabName = "datasethistograms",
                
                fluidRow(
                    column(3,
                           
                           #Create slider for Peak Wavelength Histogram
                           uiOutput("slider"),
                           textOutput("classwidth")),
                    
                    column(1,
                           
                           #Create a checkbox for Peak wavelength Histogram
                           checkboxInput("peakcheck", label = "Display Histogram?", value = TRUE)),
                    
                    column(3,
                           
                           #Create slider for Local Peak Wavelength Histogram
                           uiOutput("slider1"),
                           textOutput("classwidth1"),
                           
                           ),
                    
                    column(1,
                           
                           #Create a checkbox for Local Peak wavelength histogram
                           checkboxInput("localpeakcheck", label = "Display Histogram?", value = TRUE)
                )),
                
                hr(),
                
                #Download button for histogram
                downloadButton("downloadHist","Download Histogram Plot"),
                
                #Output Peak Wavelength Histogram and Information
                plotOutput("peakhist", click = "click4") %>% withSpinner(getOption("spinner.type", 8)),
                verbatimTextOutput("info6"),
                hr(),
                
                fluidRow(
                    column(3,
                           
                           #Create slider for Absolute Max Histogram
                           uiOutput("slider2"),
                           textOutput("classwidth2")),
                    
                    column(1,
                           
                           #Create a checkbox for Peak wavelength Histogram)
                           checkboxInput("absmaxcheck", label = "Display Histogram?", value = TRUE)),
                    
                    column(3,
                           
                           #Create slider for Local Max Histogram
                           uiOutput("slider3"),
                           textOutput("classwidth3")),
                    
                    column(1,
                           
                           #Create a checkbox for Peak wavelength Histogram)
                           checkboxInput("localmaxcheck", label = "Display Histogram?", value = TRUE)),
        
                ),
                
                hr(),
                
                #Download button for Absolute/local max histogram
                downloadButton("downloadHist1","Download Histogram Plot"),
                
                #Create histogram for absolute/local max histograms and information
                plotOutput("abslochist", click = "click5") %>% withSpinner(getOption("spinner.type", 8)),
                verbatimTextOutput("info7")
                
                
        ),
        
        #Content for "Export Allspectra" tab
        tabItem(tabName = "smoothSpline",
                
                #Download button for smoothing spline plot
                downloadButton("downloadSpectrum", "Download Spectrum Plot"),
                
                #Output smoothing spline plot
                plotOutput("smoothLine", brush = brushOpts("smoothPlot_brush",resetOnNew = TRUE)),
                
                #Output zoomed smoothing spline plot
                plotOutput("zoomsmooth"),
                verbatimTextOutput("info8")
                
        ),
        
        #Content for "About" tab
        tabItem(tabName = "about",
                
                #Write about application
                h1("About the Application"),
                
                h3("It imports a cathodoluminescence dataset, which is obtained from images of a photovoltaic cell. It can visualize the characterization of the luminescence of solar cells, distribution of the wavelengths associated with the luminescense, the relationship between both luminescence and the wavelengths and more."), 
                h3("The application can also export the plots/tables."),
                h3("It is a product of the collaboration between the Mathematics Department and the Engineering Techonology Department at Texas A&M University-Central Texas. It is meant to enhance the Engineering Technology Department's research about solar cells through statistics."),
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
        
        #Test for a text file
        if (grepl("\\.txt", input$file$datapath)) {read_tsv(input$file$datapath, col_names = FALSE)}
        else {read_csv(input$file$datapath, col_names = FALSE)}
        
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
    
    
    ###ACTION BUTTONS###
    
    #Create table using abs_max_func_2 (Absolute Max Function 2.0.R) function
    bigger_data <- eventReactive(input$go, {
        
        #Combine dataset() with dataframe created by abs_max_func_2 (Absolute Max Function 2.0.R) by column
        cbind(dataset(),abs_max_func_2(first_df(),input$wavelength1,input$wavelength2))
        
    })
    
    #Extract wavelengths from uploaded dataset
    wavelengths <- eventReactive(input$go1, {
        
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
    
    #Slider for Peak Wavelength Histogram 
    output$slider <- renderUI({
        
        #Indicate number of bins
        #Lowest number is 1; highest number is how many coordinates there are in the uploaded dataset
        #Default value is 2
        sliderInput("inslider", "Peak Wavelength (Blue)", min = 1, max = length(dataset()[,3]), value = 2, step = 1)
        
    })
    
    #Slider for Local Peak Wavelength Histogram
    output$slider1 <- renderUI({
        
        #Indicate number of bins
        #Lowest number is 1; highest number is how many coordinates there are in the uploaded dataset
        #Default value is 2
        sliderInput("inslider1", "Local Peak Wavelength (Green)", min = 1, max = length(unlist(local_max_list()[1:(length(local_max_list())/2)], use.names = FALSE)), value = 2, step = 1)
        
    })
    
    #Slider for Absolute Max Histogram
    output$slider2 <- renderUI({
        
        #Indicate number of bins
        #Lowest number is 1; highest number is how many elements are in each vector
        #Default value is 2
        sliderInput("inslider2", "Absolute Max (Blue)", min = 1, max = length(dataset()[,5]), value = 2, step = 1)
        
    })
    
    #Slider for Local Max Histogram
    output$slider3 <- renderUI({
        
        #Indicate number of bins
        #Lowest number is 1; highest number is how many elements are in each vector
        #Default value is 2
        sliderInput("inslider3", "Local Max (Green)", min = 1, max = length(unlist(local_max_list()[(length(local_max_list())/2+1):length(local_max_list())], use.names = FALSE)), value = 2, step = 1)
        
    })
    
    
    ###REACTIVE VALUES FOR HEAT MAP PLOTS/DOWNLOAD BUTTONS###
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
    
    
    ###HEAT MAP PLOTS###
    
    #Normalized Absolute Max Intensity Heat Map
    output$normMaxIntHeatMap <- renderPlot({

        #Make ggplot2 static plot using dataset_2()
        ggplot(df_absInt(), aes(X, Y, fill= Normal_Intensity)) + 
            geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Normal Intensity")+ggtitle("Normal Max Intensity Plot")+ scale_fill_gradient(low = "black", high = "red")
        
        
    })
    
    #Peak Wavelength Heat Map 
    output$peakWaveHeatMap <- renderPlot({
        
        #Make ggplot2 static plot
        ggplot(df_peakWave(), aes(X, Y, fill= Wavelength)) + 
            geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Wavelength")+ggtitle("Peak Wavelength Plot")+ scale_fill_gradient(low = "black", high = "red")
   
     })
    
    #Relative Intensity Heat Map
    output$RelIntHeatMap <- renderPlot({
        
        #Make ggplot2 static plot
        ggplot(df_relInt(), aes(X, Y, fill= Relative_Intensity)) + 
            geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Rel. Intensity") + ggtitle(paste("Relative Intensity", paste0("(",input$wavelength1, "/", input$wavelength2,")"),"Plot"))+ scale_fill_gradient(low = "black", high = "red")
        
    })
    
    #Intensities for wavelength 1
    output$Wave1IntenHeatMap <- renderPlot({
        
        #Make ggplot2 static plot
        ggplot(df_wave1(), aes(X, Y, fill= Normal_Wave_1_Intensity)) + 
            geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Normal Intensity") + ggtitle(paste("Wavelength 1",paste0("(",input$wavelength1,")"),"Intensity Plot")) + scale_fill_gradient(low = "black", high = "red")
    
    })
    
    #Intensities for wavelength 2
    output$Wave2IntenHeatMap <- renderPlot({
        
        #Make ggplot2 static plot
        ggplot(df_wave2(), aes(X, Y, fill= Normal_Wave_2_Intensity)) + 
            geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Normal Intensity") + ggtitle(paste("Wavelength 2",paste0("(",input$wavelength2,")"),"Intensity Plot")) + scale_fill_gradient(low = "black", high = "red")
        
    })
    
    
    ###PEAK WAVELENGTH HISTOGRAM###
    
    #Make histogram representing the distribution of the peak wavelengths 
    output$peakhist <- renderPlot({
        
        #Assign local peak wavelength vector
        w <- unlist(local_max_list()[1:(length(local_max_list())/2)], use.names = FALSE)
        
        #Calculate break points based on lowest & highest peak wavelength values along with number of bins 
        bins <- seq(min(dataset()[,3]),max(dataset()[,3]), length.out = input$inslider + 1)
        
        #Calculate break points based on lowest & highest local peak wavelength values along with number of bins
        bins1 <- seq(min(w),max(w), length.out = input$inslider1 + 1)
        
        #If user checks both boxes, then display both histograms
        if (input$peakcheck & input$localpeakcheck) {
            
            
            #Display both histograms
            hist(dataset()[,3], breaks = bins, main = "Histogram of Peak/Local Peak Wavelengths", xlab = "Wavelength", col = "blue", border = "white", xlim = c(min(c(dataset()[,3],w)) - 10, max(c(dataset()[,3],w)) + 10))
            hist(w, breaks = bins1, col = rgb(0,1,0,0.5), border = "white", add = TRUE)
            
        
        } else if (input$peakcheck) {
            
            #If user checks box for peak wavelength histogram only, then display it
            hist(dataset()[,3], breaks = bins, main = "Histogram of Peak Wavelengths", xlab = "Wavelength", col = "blue", border = "white", xlim = c(min(dataset()[,3]) - 10, max(dataset()[,3]) + 10))
            
        } else if (input$localpeakcheck) {
            
            #If user checks box for local peak wavelength histogram only, then display it
            hist(w, breaks = bins1, main = "Histogram of Local Peak Wavelengths", xlab = "Wavelength", col = rgb(0,1,0,0.5), border = "white", xlim = c(min(w) - 10, max(w) + 10))
            
        }
    })
    
    #Make histogram representing the distribution of the absolute/local max values
    output$abslochist <- renderPlot({
        
        #Assign local max value vector
        w <- unlist(local_max_list()[(length(local_max_list())/2+1): length(local_max_list())], use.names = FALSE)
        
        #Calculate break points based on lowest & highest absolute max values along with number of bins
        bins <- seq(min(dataset()[,5]), max(dataset()[,5]), length.out = input$inslider2 + 1)
        
        #Calculate break points based on lowest & highest local max values along with number of bins
        bins1 <- seq(min(w), max(w), length.out = input$inslider3 + 1)
        
        #If user checks both boxes, then display both histograms
        if (input$absmaxcheck & input$localmaxcheck) {
            
            hist(dataset()[,5], breaks = bins, main = "Histogram of Absolute/Local Max Values", xlab = "Value", col = "blue", border = "white", xlim = c(min(c(dataset()[,5],w)) - 0.1, max(c(dataset()[,5],w)) + 0.1))
            hist(w, breaks = bins1, col = rgb(0,1,0,0.5), border = "white", add = TRUE)
            
        } else if (input$absmaxcheck) {
            
            #If user checks box for absolute max histogram only, then display it
            hist(dataset()[,5], breaks = bins, main = "Histogram of Absolute Max Values", xlab = "Value", col = "blue", border = "white", xlim = c(min(dataset()[,5]) - 0.1, max(dataset()[,5]) + 0.1))
            
        } else if (input$localmaxcheck) {
            
            #If user checks box for local max histogram only, then display it
            hist(w, breaks = bins1, main = "Histogram of Local Max Values", xlab = "Value", col = rgb(0,1,0,0.5), border = "white", xlim = c(min(w) - 0.1, max(w) + 0.1))
            
        }
        
    })
    
    
    ###SMOOTH SPLINE INTERPOLATION GRAPH###
    
    #Create smoothing spline line and plot it
    output$smoothLine <- renderPlot({
         
        #Plot points for given row on graph
        plot(wavelengths(), y_values(), xlab = "Wavelength", ylab = "Normal Intensity", main = paste("Normal Intensity vs. Wavelength \n X =", coordinates()[1],", Y = ", coordinates()[2]))
        
        #Plot smooth spline line, absolute max point, and local max points
        lines(splinesmoothfit(), col = "red", lwd = 2)
        points(abs_max_coordinates()[1], abs_max_coordinates()[2], pch = 20, col = "blue", cex = 3)
        points(local_max_points()[1,],local_max_points()[2,], pch = 20, col = "green", cex = 3)
        legend("topright", c("Smoothing Spline","Absolute Max", "Local Max"),  lwd = c(2,NA,NA), col = c("red", "blue","green"), pch = c(NA,20,20), pt.cex = c(NA,3,3))
        
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
        filename = function(){
            
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
            
            #Make ggplot2 static plot
            p <- ggplot(df_absInt(), aes(X, Y, fill= Normal_Intensity)) + 
                geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Normal Intensity")+ggtitle("Normal Max Intensity Plot")+ scale_fill_gradient(low = "black", high = "red")
            
            #SVG is selected
            if (input$select == 1) {
                
                #Make file SVG
                svg(file)
            
            #JPEG is selected
            } else if (input$select == 2) {
                
                #Make file JPEG
                jpeg(file)
             
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Make file PNG
                png(file)
                
            }
            
            print(p)
            
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
            
            #Make ggplot2 static plot
            p <- ggplot(df_peakWave(), aes(X, Y, fill= Wavelength)) + 
                geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Wavelength")+ggtitle("Peak Wavelength Plot")+ scale_fill_gradient(low = "black", high = "red")
            
            #SVG is selected
            if (input$select == 1) {
                
                #Make file SVG
                svg(file)
                
            #JPEG is selected
            } else if (input$select == 2) {
                
                #Make file JPEG
                jpeg(file)
                
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Make file PNG
                png(file)
                
            }
            
            print(p)
            
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
            
            #Make ggplot2 static plot
            p <- ggplot(df_relInt(), aes(X, Y, fill= Relative_Intensity)) + 
                geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Rel. Intensity") + ggtitle("Relative Intensity Plot")+ scale_fill_gradient(low = "black", high = "red")
            
            #SVG is selected
            if (input$select == 1) {
                
                #Make file SVG
                svg(file)
                
            #JPEG is selected
            } else if (input$select == 2) {
                
                #Make file JPEG
                jpeg(file)
                
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Make file PNG
                png(file)
                
            }
            
            print(p)
            
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
            
            #Make ggplot2 static plot
            p <- ggplot(df_wave1(), aes(X, Y, fill= Normal_Wave_1_Intensity)) + 
                geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Intensity")+ggtitle("Wavelength 1 Intensity Plot") + scale_fill_gradient(low = "black", high = "red")
            
            #SVG is selected
            if (input$select == 1) {
                
                #Make file SVG
                svg(file)
                
            #JPEG is selected
            } else if (input$select == 2) {
                
                #Make file JPEG
                jpeg(file)
                
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Make file PNG
                png(file)
                
            }
            
            print(p)
            
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
            
            #Make ggplot2 static plot
            p <- ggplot(df_wave2(), aes(X, Y, fill= Normal_Wave_2_Intensity)) + 
                geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Intensity")+ggtitle("Wavelength 2 Intensity Plot") + scale_fill_gradient(low = "black", high = "red")
            
            #SVG is selected
            if (input$select == 1) {
                
                #Make file SVG
                svg(file)
                
            #JPEG is selected
            } else if (input$select == 2) {
                
                #Make file JPEG
                jpeg(file)
                
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Make file PNG
                png(file)
                
            }
            
            print(p)
            
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
            
            #Assign local peak wavelength vector
            w <- unlist(local_max_list()[1:(length(local_max_list())/2)], use.names = FALSE)
            
            #Calculate break points based on lowest & highest peak wavelength values along with number of bins 
            bins <- seq(min(dataset()[,3]),max(dataset()[,3]), length.out = input$inslider + 1)
            
            #Calculate break points based on lowest & highest local peak wavelength values along with number of bins
            bins1 <- seq(min(w),max(w), length.out = input$inslider1 + 1)
            
            #SVG is selected
            if (input$select == 1) {
                
                #Make file SVG
                svg(file)
                
            #JPEG is selected
            } else if (input$select == 2) {
                
                #Make file JPEG
                jpeg(file)
                
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Make file PNG
                png(file)
                
            }
            
            #If user checks both boxes, then display both histograms
            if (input$peakcheck & input$localpeakcheck) {
                
                #Display both histograms
                hist(dataset()[,3], breaks = bins, main = "Histogram of Peak/Local Peak Wavelengths", xlab = "Wavelength", col = "blue", border = "white", xlim = c(min(c(dataset()[,3],w)) - 10, max(c(dataset()[,3],w)) + 10))
                hist(w, breaks = bins1, col = rgb(0,1,0,0.5), border = "white", add = TRUE)
                
            } else if (input$peakcheck) {
                
                #If user checks box for peak wavelength histogram only, then display it
                hist(dataset()[,3], breaks = bins, main = "Histogram of Peak Wavelengths", xlab = "Wavelength", col = "blue", border = "white", xlim = c(min(dataset()[,3]) - 10, max(dataset()[,3]) + 10))
                
            } else if (input$localpeakcheck) {
                
                #If user checks box for local peak wavelength histogram only, then display it
                hist(w, breaks = bins1, main = "Histogram of Local Peak Wavelengths", xlab = "Wavelength", col = rgb(0,1,0,0.5), border = "white", xlim = c(min(w) - 10, max(w) + 10))
                
            }
            
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
            
            #Assign local max value vector
            w <- unlist(local_max_list()[(length(local_max_list())/2+1): length(local_max_list())], use.names = FALSE)
            
            #Calculate break points based on lowest & highest absolute max values along with number of bins
            bins <- seq(min(dataset()[,5]), max(dataset()[,5]), length.out = input$inslider2 + 1)
            
            #Calculate break points based on lowest & highest local max values along with number of bins
            bins1 <- seq(min(w), max(w), length.out = input$inslider3 + 1)
            
            #SVG is selected
            if (input$select == 1) {
                
                #Make file SVG
                svg(file)
                
            #JPEG is selected
            } else if (input$select == 2) {
                
                #Make file JPEG
                jpeg(file)
                
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Make file PNG
                png(file)
                
            }
            
            #If user checks both boxes, then display both histograms
            if (input$absmaxcheck & input$localmaxcheck) {
                
                hist(dataset()[,5], breaks = bins, main = "Histogram of Absolute/Local Max Values", xlab = "Value", col = "blue", border = "white", xlim = c(min(c(dataset()[,5],w)) - 0.1, max(c(dataset()[,5],w)) + 0.1))
                hist(w, breaks = bins1, col = rgb(0,1,0,0.5), border = "white", add = TRUE)
                
            } else if (input$absmaxcheck) {
                
                #If user checks box for absolute max histogram only, then display it
                hist(dataset()[,5], breaks = bins, main = "Histogram of Absolute Max Values", xlab = "Value", col = "blue", border = "white", xlim = c(min(dataset()[,5]) - 0.1, max(dataset()[,5]) + 0.1))
                
            } else if (input$localmaxcheck) {
                
                #If user checks box for local max histogram only, then display it
                hist(w, breaks = bins1, main = "Histogram of Local Max Values", xlab = "Value", col = rgb(0,1,0,0.5), border = "white", xlim = c(min(w) - 0.1, max(w) + 0.1))
                
            }
            
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
                jpeg(file)
                
            #PNG is selected   
            } else if (input$select == 3) {
                
                #Make file PNG
                png(file)
                
            }
            
            #Plot points for given row on graph
            plot(wavelengths(), y_values(), xlab = "Wavelength", ylab = "Normal Intensity", main = paste("Normal Intensity vs. Wavelength \n X =", coordinates()[1],", Y = ", coordinates()[2]))
            
            #Plot smooth spline line, absolute max point, and local max points
            lines(splinesmoothfit(), col = "red", lwd = 2)
            points(abs_max_coordinates()[1], abs_max_coordinates()[2], pch = 20, col = "blue", cex = 3)
            points(local_max_points()[1,],local_max_points()[2,], pch = 20, col = "green", cex = 3)
            legend("topright", c("Smoothing Spline","Absolute Max", "Local Max"),  lwd = c(2,NA,NA), col = c("red", "blue","green"), pch = c(NA,20,20), pt.cex = c(NA,3,3))
            
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
    
    #Output information for interactive Heat map (max int.)
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
        
        #Assign local peak wavelength vector
        w <- unlist(local_max_list()[1:(length(local_max_list())/2)], use.names = FALSE)
        
        #Calculate break points
        bins <- seq(min(dataset()[,3]),max(dataset()[,3]), length.out = input$inslider + 1)
        
        #Calculate break points
        bins1 <- seq(min(w),max(w), length.out = input$inslider1 + 1)
        
        #Create reactive value for current peak wavelength histogram
        z <- reactive({
            
            hist(dataset()[,3], breaks = bins)
            
        })
        
        #Create reactive value for current local peak wavelength histogram
        y <- reactive({
            
            hist(w, breaks = bins1)
            
        })
        
        #Determine indices of limits where mouse is between (peak wavelength histogram)
        lower_lim_ind_peak <- tail(which(z()$breaks <= input$click4$x),1)
        higher_lim_ind_peak <- head(which(z()$breaks >= input$click4$x),1)
        
        ##Determine indices of limits where mouse is between (local peak wavelength histogram)
        lower_lim_ind_local <- tail(which(y()$breaks <= input$click4$x),1)
        higher_lim_ind_local <- head(which(y()$breaks >= input$click4$x),1)
        
        #Find the limits for each histogram
        lower_lim_peak <- z()$breaks[lower_lim_ind_peak]
        higher_lim_peak <- z()$breaks[higher_lim_ind_peak]
        lower_lim_local <- y()$breaks[lower_lim_ind_local]
        higher_lim_local <- y()$breaks[higher_lim_ind_local]
        
        #Determine the count
        peak_count <- z()$counts[lower_lim_ind_peak]
        local_count <- y()$counts[lower_lim_ind_local]
        
        #Create vectors for each histogram consisting of lower and upper limits, midpoint, and frequency
        peak_info <- c(lower_lim_peak, higher_lim_peak, z()$mids[lower_lim_peak < z()$mids & z()$mids < higher_lim_peak], peak_count)
        local_info <- c(lower_lim_local, higher_lim_local, y()$mids[lower_lim_local < y()$mids & y()$mids < higher_lim_local], local_count)
        
        #Create dataframe
        df <- cbind(peak_info, local_info)
        
        #If both checkboxes are marked
        if (input$peakcheck & input$localpeakcheck) {
            
            #If the user clicks within the range of either one or both histograms
            if(input$click4$x < min(c(bins,bins1)) | input$click4$x > max(c(bins,bins1))) {
                
                #If user clicks out of the range for both histograms
                print("Click on a bin that is within the range of either histogram.")
                
                
            } else if (input$click4$x < min(bins) | input$click4$x > max(bins)) {
                
                #If the user clicks on a green bin that is out of the range of the blue histogram
                df[,1] <- NA * integer(nrow(df))
                colnames(df) <- c("Peak Wavelength", "Local Peak Wavelength")
                rownames(df) <- c("Lower Bin Limit", "Higher Bin Limit", "Midpoint", "Frequency")
                df
                
            } else if (input$click4$x < min(bins1) | input$click4$x > max(bins1)) {
                
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
            if (input$click4$x < min(bins) | input$click4$x > max(bins)) {
                
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
            if (input$click4$x < min(bins1) | input$click4$x > max(bins1)) {
                
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
        
        #If a bin is clicked on
        #Assign local max vector
        w <- unlist(local_max_list()[(length(local_max_list())/2 + 1):length(local_max_list())], use.names = FALSE)
        
        #Calculate break points
        bins <- seq(min(dataset()[,5]),max(dataset()[,5]), length.out = input$inslider2 + 1)
        
        #Calculate break points
        bins1 <- seq(min(w),max(w), length.out = input$inslider3 + 1)
        
        #Create reactive value for current peak wavelength histogram
        z <- reactive({
            
            hist(dataset()[,5], breaks = bins)
            
        })
        
        #Create reactive value for current local peak wavelength histogram
        y <- reactive({
            
            hist(w, breaks = bins1)
            
        })
        
        #Determine indices of limits where mouse is between (peak wavelength histogram)
        lower_lim_ind_abs <- tail(which(z()$breaks <= input$click5$x),1)
        higher_lim_ind_abs <- head(which(z()$breaks >= input$click5$x),1)
        
        ##Determine indices of limits where mouse is between (local peak wavelength histogram)
        lower_lim_ind_local <- tail(which(y()$breaks <= input$click5$x),1)
        higher_lim_ind_local <- head(which(y()$breaks >= input$click5$x),1)
        
        #Find the limits for each histogram
        lower_lim_abs <- z()$breaks[lower_lim_ind_abs]
        higher_lim_abs <- z()$breaks[higher_lim_ind_abs]
        lower_lim_local <- y()$breaks[lower_lim_ind_local]
        higher_lim_local <- y()$breaks[higher_lim_ind_local]
        
        #Determine the count for each bin that was clicked on using lower limit index
        abs_count <- z()$counts[lower_lim_ind_abs]
        local_count <- y()$counts[lower_lim_ind_local]
        
        #Create vectors for each histogram consisting of lower and upper limits, midpoint, and frequency
        abs_info <- c(lower_lim_abs, higher_lim_abs, z()$mids[lower_lim_abs < z()$mids & z()$mids < higher_lim_abs], abs_count)
        local_info <- c(lower_lim_local, higher_lim_local, y()$mids[lower_lim_local < y()$mids & y()$mids < higher_lim_local], local_count)
        
        #Create dataframe
        df <- cbind(abs_info, local_info)
        
        #If both checkboxes are marked
        if (input$absmaxcheck & input$localmaxcheck) {
            
            #If the user clicks within the range of either one or both histograms
            if(input$click5$x < min(c(bins,bins1)) | input$click5$x > max(c(bins,bins1))) {
                
                #If user clicks out of the range for both histograms
                print("Click on a bin that is within the range of either histogram.")
                
                
            } else if (input$click5$x < min(bins) | input$click5$x > max(bins)) {
                
                #If the user clicks on a green bin that is out of the range of the blue histogram
                df[,1] <- NA * integer(nrow(df))
                colnames(df) <- c("Absolute Max", "Local Max")
                rownames(df) <- c("Lower Bin Limit", "Higher Bin Limit", "Midpoint", "Frequency")
                df
                
            } else if (input$click5$x < min(bins1) | input$click5$x > max(bins1)) {
                
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
            if (input$click5$x < min(bins) | input$click5$x > max(bins)) {
                
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
            if (input$click5$x < min(bins1) | input$click5$x > max(bins1)) {
                
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
        
        #Use slider to determine the class width
        #Calculate break points based on lowest & highest peak wavelength values along with number of bins 
        bins <- seq(min(dataset()[,3]),max(dataset()[,3]), length.out = input$inslider + 1)
        
        #Display Class Width
        paste("Bin width is", (tail(bins,1) - head(bins,1))/(length(bins)-1), "nm")
        
    })
    
    #Display class width for Local Peak Wavelength Histogram
    output$classwidth1 <- renderText({
        
        #Assign local peak wavelength vector
        w <- unlist(local_max_list()[1:(length(local_max_list())/2)], use.names = FALSE)
        
        #Use slider to determine the class width
        #Calculate break points based on lowest & highest peak wavelength values along with number of bins 
        bins <- seq(min(w),max(w), length.out = input$inslider1 + 1)
        
        #Display Class Width
        paste("Bin width is", (tail(bins,1) - head(bins,1))/(length(bins)-1), "nm")
        
    })
    
    #Display class width for Absolute Max Histogram
    output$classwidth2 <- renderText({
        
        #Use slider to determine the class width
        #Calculate break points based on lowest & highest peak wavelength values along with number of bins 
        bins <- seq(min(dataset()[,5]),max(dataset()[,5]), length.out = input$inslider2 + 1)
        
        #Display Class Width
        paste("Bin width is", (tail(bins,1) - head(bins,1))/(length(bins)-1))
        
    })
    
    #Display class width for Local Max Histogram
    output$classwidth3 <- renderText({
        
        #Assign local max value vector
        w <- unlist(local_max_list()[(length(local_max_list())/2+1): length(local_max_list())], use.names = FALSE)
        
        #Calculate break points based on lowest & highest local max values along with number of bins
        bins <- seq(min(w), max(w), length.out = input$inslider3 + 1)
        
        #Display Class Width
        paste("Bin width is", (tail(bins,1) - head(bins,1))/(length(bins)-1))
        
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
