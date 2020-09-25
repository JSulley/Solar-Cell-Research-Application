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
        menuItem("Peak Wavelength Histogram", tabName = "peakhistogram"),
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
                    tabBox(width = 9,
                           
                           #Notes for uploading the file
                           tabPanel("File Format for Upload",
                                    
                                    #Make note to user for what the app is expecting as an input
                                    p("The program assumes the CSV file as the following format:"),
                                    p("1. The first row is the wavelengths."),
                                    p("2. The first two columns are the x and y coordinates."),
                                    p("3. Nothing in the first two boxes."),
                                    p("4. The rest are the intensities."),
                                    strong(em("Example:")),
                                    br(),
                                    
                                    #Image demonstrating format of CSV file
                                    img(src = "Example Data.PNG", height = 200, width = 350),
                           ),
                           
                           #Note regarding downloading plots.
                           tabPanel("Downloading Plots/Tables",
                                    p("You are able to download the plots/tables displayed!"),
                                    p("The plots are downloaded in SVG format, while the tables are downloaded as a CSV file."),
                                    p("For Table 2 to appear, you", strong("must"), "input two different wavelengths first."),
                                    
                                    #Note what calculation is being done for relative intensity
                                    p(strong(em("Note:")), "For the relative intensity, it is the ratio of", HTML(paste0("&lambda;",tags$sub(1))) ,"intensity to",
                                      HTML(paste0("&lambda;",tags$sub(2))) ,"intensity")),
                           
                           #Note recent features
                           tabPanel("More Features",
                                    
                                    #Interactive Peak Wavelength Histogram
                                    h3("Peak Wavelength Interactive Histogram"),
                                    p("The distribution of the peak wavelengths can be now viewed in the tab labeled 'Peak Wavelength Histogram'. Make sure to upload the file first!"),
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
                           img(src = "Screenshot.PNG", height = 520, width = 870))
                    )   
                    
                )
        ),
        
        #Content for "Dataset Plots"
        tabItem(tabName = "datasetDash",
                
                #Download button for Normalized Absolute Max Intensity Heat Map
                downloadButton("downloadgraph", "Download Normal Max Intensity Plot"),
                
                #Output heat map for Normalized Absolute Max Intensity and Information
                plotOutput("normMaxIntHeatMap", click = "click") %>% withSpinner(),
                verbatimTextOutput("info1"),
                
                #Add border
                hr(),
                
                #Make download button for Peak Wavelength Heat Map
                downloadButton("downloadgraph1", "Download Peak Wavelength Plot"),
                
                #Output heat map for Peak Wavelength and Information
                plotOutput("peakWaveHeatMap", click = "click") %>% withSpinner(),
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
                           plotOutput("RelIntHeatMap", click = "click1") %>% withSpinner(),
                           verbatimTextOutput("info3")
                           
                    ),
                    
                    
                    #Make fluid row for interactive intensities for both inputted wavelengths
                    fluidRow(
                        
                        column(6,
                               
                               #Make download button for heat map of inputted wavelength 1 
                               downloadButton("downloadgraph3", "Download Wavelength 1 Intensity Plot"),
                               
                               #Heat map plot for intensities of wavelength 1
                               plotOutput("Wave1IntenHeatMap", click = "click2") %>% withSpinner(),
                               verbatimTextOutput("info4"),
                        ),
                        
                        column(6,
                               
                               #Make download button for heat map of inputted wavelength 1 
                               downloadButton("downloadgraph4", "Download Wavelength 2 Intensity Plot"),
                               
                               #Heat map plot for intensities of wavelength 1
                               plotOutput("Wave2IntenHeatMap", click = "click3") %>% withSpinner(),
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
        
        #Content for "Peak Wavelength Histogram" tab
        tabItem(tabName = "peakhistogram",
                
                #Create slider for user
                uiOutput("slider"),
                
                #Download button for histogram
                downloadButton("downloadHist","Download Histogram Plot"),
                
                #Output Peak Wavelength Histogram and Information
                plotOutput("peakhist", click = "click4") %>% withSpinner(),
                verbatimTextOutput("info6"),
                
                #Make sliders for both histograms
                uiOutput("slider1"),
                uiOutput("slider2"),
                
                #Create histogram for absolute/local histograms
                plotOutput("abslochist") %>% withSpinner()
                
        ),
        
        #Content for "Export Allspectra" tab
        tabItem(tabName = "smoothSpline",
                
                #Download button for smoothing spline plot
                downloadButton("downloadSpectrum", "Download Spectrum Plot"),
                
                #Output smoothing spline plot
                plotOutput("smoothLine", brush = brushOpts("smoothPlot_brush",resetOnNew = TRUE)),
                
                #Output zoomed smoothing spline plot
                plotOutput("zoomsmooth"),
                verbatimTextOutput("info7")
                
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
server <- function(input, output) {
    
    #Increase file size input
    options(shiny.maxRequestSize = 30*1024^2, spinner.type = 8)
    
    
    ###PREPARATION###
    
    #Read uploaded file
    first_df <- eventReactive(input$file, {
        
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
        sliderInput("inslider", "Number of Bins:", min = 1, max = length(dataset()[,3]), value = 2)
        
    })
    
    #Slider for Absolute Max Histogram
    output$slider1 <- renderUI({
        
        #Indicate number of bins
        #Lowest number is 1; highest number is how many elements are in each vector
        #Default value is 2
        sliderInput("inslider1", "Number of Bins (Blue):", min = 1, max = length(dataset()[,5]), value = 2)
        
    })
    
    #Slider for Local Max Histogram
    output$slider2 <- renderUI({
        
        #Indicate number of bins
        #Lowest number is 1; highest number is how many elements are in each vector
        #Default value is 2
        sliderInput("inslider2", "Number of Bins (Green):", min = 1, max = length(unlist(local_max_list()[(length(local_max_list())/2+1): length(local_max_list())], use.names = FALSE)), value = 2)
        
    })
    
    
    ###HEAT MAP PLOTS###
    
    #Normalized Absolute Max Intensity Heat Map
    output$normMaxIntHeatMap <- renderPlot({
        
        #Make reactive value for reactive expression: dataset()
        dataset_2 <- reactive({
            
            #Assign object
            test <- dataset()
            
            #Make column names for 'test'
            colnames(test) <- c("X","Y","Wavelength","Intensity", "Normal_Intensity")
            
            #Print 'test'
            test
        })
        
        #Make ggplot2 static plot using dataset_2()
        ggplot(dataset_2(), aes(X, Y, fill= Normal_Intensity)) + 
            geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Normal Intensity")+ggtitle("Normal Max Intensity Plot")+ scale_fill_gradient(low = "black", high = "red")
        
        
    })
    
    #Peak Wavelength Heat Map 
    output$peakWaveHeatMap <- renderPlot({
        
        #Make reactive value for reactive expression: dataset()
        dataset_2 <- reactive({
            
            #Assign object
            test <- dataset()
            
            #Make column names for 'test'
            colnames(test) <- c("X","Y","Wavelength","Intensity", "Normal Intensity")
            
            #Print 'test'
            test
        })
        
        #Make ggplot2 static plot
        ggplot(dataset_2(), aes(X, Y, fill= Wavelength)) + 
            geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Wavelength")+ggtitle("Peak Wavelength Plot")+ scale_fill_gradient(low = "black", high = "red")
        
        
    })
    
    #Relative Intensity Heat Map
    output$RelIntHeatMap <- renderPlot({
        
        #Make reactive value for reactive expression: bigger_data()
        dataset_2 <- reactive({
            
            #Assign object
            test <- bigger_data()
            
            #Make column names for 'test'
            colnames(test) <- c("X","Y","Wavelength","Intensity", "Normal Intensity","Wavelength 1 Intensity","Wavelength 2 Intensity", "Normal Wave 1 Intensity", "Normal Wave 2 Intensity" , "Relative_Intensity")
            
            #Print 'test'
            test
        })
        
        #Make ggplot2 static plot
        ggplot(dataset_2(), aes(X, Y, fill= Relative_Intensity)) + 
            geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Rel. Intensity") + ggtitle(paste("Relative Intensity", paste0("(",input$wavelength1, "/", input$wavelength2,")"),"Plot"))+ scale_fill_gradient(low = "black", high = "red")
        
        
    })
    
    #Intensities for wavelength 1
    output$Wave1IntenHeatMap <- renderPlot({
        
        #Make reactive value for reactive expression: bigger_data()
        dataset_2 <- reactive({
            
            #Assign object
            test <- bigger_data()
            
            #Make column names for 'test'
            colnames(test) <- c("X","Y","Wavelength","Intensity", "Normal Intensity","Wavelength 1 Intensity","Wavelength 2 Intensity", "Normal_Wave_1_Intensity", "Normal Wave 2 Intensity" , "Relative Intensity")
            
            #Print 'test'
            test
        })
        
        #Make ggplot2 static plot
        ggplot(dataset_2(), aes(X, Y, fill= Normal_Wave_1_Intensity)) + 
            geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Normal Intensity") + ggtitle(paste("Wavelength 1",paste0("(",input$wavelength1,")"),"Intensity Plot")) + scale_fill_gradient(low = "black", high = "red")
        
        
    })
    
    #Intensities for wavelength 2
    output$Wave2IntenHeatMap <- renderPlot({
        
        #Make reactive value for reactive expression: bigger_data()
        dataset_2 <- reactive({
            
            #Assign object
            test <- bigger_data()
            
            #Make column names for 'test'
            colnames(test) <- c("X","Y","Wavelength","Intensity", "Normal Intensity","Wavelength 1 Intensity","Wavelength 2 Intensity", "Normal Wave 1 Intensity", "Normal_Wave_2_Intensity" , "Relative Intensity")
            
            #Print 'test'
            test
        })
        
        #Make ggplot2 static plot
        ggplot(dataset_2(), aes(X, Y, fill= Normal_Wave_2_Intensity)) + 
            geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Normal Intensity") + ggtitle(paste("Wavelength 2",paste0("(",input$wavelength2,")"),"Intensity Plot")) + scale_fill_gradient(low = "black", high = "red")
        
        
    })
    
    
    ###PEAK WAVELENGTH HISTOGRAM###
    
    #Make histogram representing the distribution of the peak wavelengths 
    output$peakhist <- renderPlot({
        
        #Calculate break points based on lowest & highest peak wavelength value along with number of bins 
        bins <- seq(min(dataset()[,3]),max(dataset()[,3]), length.out = input$inslider + 1)
        
        #Use 'bins' as breaks in histogram
        hist(dataset()[,3], breaks = bins, main = "Histogram of Peak Wavelengths", xlab = "Peak Wavelengths", col = "blue", border = "white")
    
    })
    
    #Make histogram representing the distribution of the absolute/local max values
    output$abslochist <- renderPlot({
        
        w <- unlist(local_max_list()[(length(local_max_list())/2+1): length(local_max_list())], use.names = FALSE)
        
        #Calculate break points based on lowest & highest absolute max values along with number of bins
        bins <- seq(min(dataset()[,5]), max(dataset()[,5]), length.out = input$inslider1 + 1)
        
        #Calculate break points based on lowest & highest local max values along with number of bins
        bins1 <- seq(min(w), max(w), length.out = input$inslider2 + 1)
        
        #Use 'bins' as breaks in each histogram
        hist(dataset()[,5], breaks = bins, main = "Histogram of Absolute/Local Max Values", xlab = "Absolute/Local Max Value", col = "blue", border = "white", xlim = c(min(c(dataset()[,5],w)),max(c(dataset()[,5],w))))
        hist(w, breaks = bins1, col = rgb(0,1,0,0.5), border = "white", add = TRUE)
        
    })
    
    
    ###SMOOTH SPLINE INTERPOLATION GRAPH###
    
    #Create smoothing spline line and plot it
    output$smoothLine <- renderPlot({
        
        #Predict values for each wavelength
        predicted <- predict(splinesmoothfit(), x = wavelengths())
        
        #Plot points for given row on graph
        plot(wavelengths(), y_values(), xlab = "Wavelength", ylab = "Normal Intensity", main = paste("Normal Intensity vs. Wavelength \n X =", coordinates()[1],", Y = ", coordinates()[2]))
        
        #Plot smooth spline line, absolute max point, and local max points
        lines(splinesmoothfit(), col = "red", lwd = 2)
        points(predicted$x[which.max(predicted$y)], max(predicted$y), pch = 20, col = "blue", cex = 3)
        points(local_max_points()[1,],local_max_points()[2,], pch = 20, col = "green", cex = 3)
        legend("topright", c("Smoothing Spline","Absolute Max", "Local Max"),  lwd = c(2,NA,NA), col = c("red", "blue","green"), pch = c(NA,20,20), pt.cex = c(NA,3,3))
        
    })
    
    output$zoomsmooth <- renderPlot({
        
        #Predict values for each wavelength
        predicted <- predict(splinesmoothfit(), x = wavelengths())
        
        #Plot smooth spline line on graph
        plot(wavelengths(), y_values(), xlab = "Wavelength", ylab = "Normal Intensity", xlim = window_margins$x, ylim = window_margins$y)
        
        #Plot smooth spline line, absolute max point, and local max points
        lines(splinesmoothfit(), col = "red", lwd = 2)
        points(predicted$x[which.max(predicted$y)], max(predicted$y), pch = 20, col = "blue", cex = 3)
        points(local_max_points()[1,],local_max_points()[2,], pch = 20, col = "green", cex = 3)
        legend("topright", c("Smoothing Spline","Absolute Max", "Local Max"),  lwd = c(2,NA,NA), col = c("red", "blue","green"), pch = c(NA,20,20), pt.cex = c(NA,3,3))
        
    })
    
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
    
    
    ###DOWNLOAD BUTTONS###
    
    #Download button for heat map of normalized max intensity
    output$downloadgraph <- downloadHandler(
        
        #Allow user to make file name
        filename = function(){
            
            #Name file
            paste0("Normal Max Intensity Plot", ".svg")
            
        },
        
        #Content for file
        content = function(file){
            
            #Transform dataset() into dataset_2
            dataset_2 <- reactive({
                test <- dataset()
                colnames(test) <- c("X","Y","Wavelength","Intensity", "Normal_Intensity")
                test
            })
            
            #Make ggplot2 static plot
            p <- ggplot(dataset_2(), aes(X, Y, fill= Normal_Intensity)) + 
                geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Normal Intensity")+ggtitle("Normal Max Intensity Plot")+ scale_fill_gradient(low = "black", high = "red")
            
            #Make svg
            svg(file)
            
            print(p)
            
            dev.off()    
        }
        
    )
    
    #Download button for Peak Wavelength Heat Map Plot
    output$downloadgraph1 <- downloadHandler(
        
        #Allow user to make file name
        filename = function(){
            
            #Name file
            paste0("Peak Wavelength Plot", ".svg")
            
        },
        
        #Content for file
        content = function(file){
            
            #Transform dataset() into dataset_2
            dataset_2 <- reactive({
                test <- dataset()
                colnames(test) <- c("X","Y","Wavelength","Intensity", "Normal Intensity")
                test
            })
            
            #Make ggplot2 static plot
            p <- ggplot(dataset_2(), aes(X, Y, fill= Wavelength)) + 
                geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Wavelength")+ggtitle("Peak Wavelength Plot")+ scale_fill_gradient(low = "black", high = "red")
            
            #Make svg
            svg(file)
            
            print(p)
            
            dev.off()    
        }
        
    )
    
    #Download button for Relative Intensity Heat Map Plot
    output$downloadgraph2 <- downloadHandler(
        
        #Allow user to make file name
        filename = function(){
            
            #Name file
            paste0("Relative Intensity Plot", ".svg")
            
        },
        
        #Content for file
        content = function(file){
            
            #Transform bigger_data() into dataset_2
            dataset_2 <- reactive({
                test <- bigger_data()
                colnames(test) <- c("X","Y","Wavelength","Intensity", "Normal Intensity","Wavelength 1 Intensity","Wavelength 2 Intensity", "Normal Wave 1 Intensity", "Normal Wave 2 Intensity" , "Relative_Intensity")
                test
            })
            
            #Make ggplot2 static plot
            p <- ggplot(dataset_2(), aes(X, Y, fill= Relative_Intensity)) + 
                geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Rel. Intensity") + ggtitle("Relative Intensity Plot")+ scale_fill_gradient(low = "black", high = "red")
            
            #Make svg
            svg(file)
            
            print(p)
            
            dev.off()    
        }
        
    )
    
    #Download button for Wavelength 1 Intensity Heat Map
    output$downloadgraph3 <- downloadHandler(
        
        #Allow user to make file name
        filename = function(){
            
            #Name file
            paste0("Wavelength 1 Intensity Plot", ".svg")
            
        },
        
        #Content for file
        content = function(file){
            
            #Transform bigger_data() into dataset_2
            dataset_2 <- reactive({
                
                test <- bigger_data()
                colnames(test) <- c("X","Y","Wavelength","Intensity", "Normal Intensity","Wavelength 1 Intensity","Wavelength 2 Intensity", "Normal_Wave_1_Intensity", "Normal Wave 2 Intensity" , "Relative Intensity")
                test
                
            })
            
            #Make ggplot2 static plot
            p <- ggplot(dataset_2(), aes(X, Y, fill= Normal_Wave_1_Intensity)) + 
                geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Intensity")+ggtitle("Wavelength 1 Intensity Plot") + scale_fill_gradient(low = "black", high = "red")
            
            #Make svg
            svg(file)
            
            print(p)
            
            dev.off()    
        }
        
    )
    
    #Download button for Wavelength 2 Intensity Heat Map
    output$downloadgraph4 <- downloadHandler(
        
        #Allow user to make file name
        filename = function(){
            
            #Name file
            paste0("Wavelength 2 Intensity Plot", ".svg")
            
        },
        
        #Content for file
        content = function(file){
            
            #Transform bigger_data() into dataset_2
            dataset_2 <- reactive({
                
                test <- bigger_data()
                colnames(test) <- c("X","Y","Wavelength","Intensity", "Normal Intensity","Wavelength 1 Intensity","Wavelength 2 Intensity", "Normal Wave 1 Intensity", "Normal_Wave_2_Intensity" , "Relative Intensity")
                test
                
            })
            
            #Make ggplot2 static plot
            p <- ggplot(dataset_2(), aes(X, Y, fill= Normal_Wave_2_Intensity)) + 
                geom_tile() + theme_ipsum() + xlab("X") + ylab("Y") + labs(fill = "Intensity")+ggtitle("Wavelength 2 Intensity Plot") + scale_fill_gradient(low = "black", high = "red")
            
            #Make svg
            svg(file)
            
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
            
            #Transform dataset() into dataset_2
            dataset_2 <- reactive({
                
                test <- dataset()
                colnames(test) <- c("X","Y","Wavelength","Intensity", "Normal Intensity")
                test
                
            })
            
            #Create CSV
            write.csv(dataset_2(), file, row.names = FALSE, sep = ',')
            
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
            
            #Use both first_df() and abs_max_func_2 (Absolute Max Function 2.0.R)
            data <- abs_max_func_2(first_df(), input$wavelength1, input$wavelength2)
            
            #Assign column names
            colnames(data) <- c(paste("Wavelength 1 Intensity",input$wavelength1), paste("Wavelength 2 Intensity",input$wavelength2), "Wavelength 1 Normal Intensity", "Wavelength 2 Normal Intensity", "Relative Intensity")
            
            #Create CSV File
            write.csv(data, file, row.names = FALSE, sep = ',')
        }
        
    )
    
    #Download button for Peak Wavelength histogram
    output$downloadHist <- downloadHandler(
        
        #Allow user to make file name
        filename = function() {
            
            #Name of file
            paste0("Peak Wavelength Histogram", ".svg")
            
        },
        
        #Content for file
        content = function(file){
            
            #Calculate break points with lowest/highest peak wavelengths and number of bins
            bins <- seq(min(dataset()[,3]),max(dataset()[,3]), length.out = input$inslider + 1)
            
            
            #Make svg
            svg(file)
            
            #Create histogram
            hist(dataset()[,3], breaks = bins, main = "Histogram of Peak Wavelengths", xlab = "Peak Wavelengths", col = "blue", border = "white")
            
            dev.off()    
        }
        
    )
    
    #Download button for current spectrum plot
    output$downloadSpectrum <- downloadHandler(
        
        #Create file name
        filename = function() {
            
            #Name of file
            paste0("Spectrum Plot", ".svg")
            
        },
        
        #Content for file
        content = function(file){
            
            #Predict values for each wavelength
            predicted <- predict(splinesmoothfit(), x = wavelengths())
            
            #Make svg
            svg(file)
            
            #Plot points for given row on graph
            plot(wavelengths(), y_values(), xlab = "Wavelength", ylab = "Normal Intensity", main = paste("Normal Intensity vs. Wavelength \n X =", coordinates()[1],", Y = ", coordinates()[2]))
            
            #Plot smooth spline line, absolute max point, and local max points
            lines(splinesmoothfit(), col = "red", lwd = 2)
            points(predicted$x[which.max(predicted$y)], max(predicted$y), pch = 20, col = "blue", cex = 3)
            points(local_max_points()[1,],local_max_points()[2,], pch = 20, col = "green", cex = 3)
            legend("topright", c("Smoothing Spline","Absolute Max", "Local Max"),  lwd = c(2,NA,NA), col = c("red", "blue","green"), pch = c(NA,20,20), pt.cex = c(NA,3,3))
            
            dev.off()    
        }
        
    )
    
    
    ###OUTPUT INFORMATION###
    
    #Output information for interactive Heat map (max int.)
    output$info1 <- renderPrint({
        
        #Transform dataset() into dataset_2
        dataset_2 <- reactive({
            test <- dataset()
            colnames(test) <- c("X","Y","Wavelength","Intensity", "Normal Intensity")
            test
        })
        
        #Display row information
        nearPoints(dataset_2()[,-c(3,4)], input$click, threshold = 15, maxpoints = 1)
    })
    
    #Output information for interactive heat map (peak wavelength)
    output$info2 <- renderPrint({
        
        #Transform dataset() into dataset_2
        dataset_2 <- reactive({
            test <- dataset()
            colnames(test) <- c("X","Y","Wavelength","Intensity", "Normal Intensity")
            test
        })
        
        #Display row information
        nearPoints(dataset_2()[,-c(4,5)], input$click, threshold = 15, maxpoints = 1)
    })
    
    #Output information for interactive heat map (Relative Intensity)
    output$info3 <- renderPrint({
        
        #Transform bigger_data() into dataset_2
        dataset_2 <- reactive({
            test <- bigger_data()
            colnames(test) <- c("X","Y","Wavelength","Intensity","Normal Intensity","Wavelength 1 Intensity", "Wavelength 2 Intensity","Normal Wavelength 1 Intensity", "Normal Wavelength 2 Intensity", "Relative Intensity")
            test
        })
        
        #Display row information
        nearPoints(dataset_2()[,-c(3,4,5)], input$click1, threshold = 15, maxpoints = 1)
        
    })
    
    #Output Information for interactive heat map (Wavelength 1)
    output$info4 <- renderPrint({
        
        #Transform bigger_data() into dataset_2
        dataset_2 <- reactive({
            test <- bigger_data()
            colnames(test) <- c("X","Y","Wavelength","Intensity","Normal Intensity","Wavelength 1 Intensity", "Wavelength 2 Intensity","Normal Wavelength 1 Intensity", "Normal Wavelength 2 Intensity", "Relative Intensity")
            test
        })
        
        #Display row information
        nearPoints(dataset_2()[,-c(3:5,7:9)], input$click2, threshold = 15, maxpoints = 1)
        
    })
    
    #Output Information for interactive heat map (Wavelength 2)
    output$info5 <- renderPrint({
        
        #Transform bigger_data() into dataset_2
        dataset_2 <- reactive({
            test <- bigger_data()
            colnames(test) <- c("X","Y","Wavelength","Intensity","Normal Intensity","Wavelength 1 Intensity", "Wavelength 2 Intensity","Normal Wavelength 1 Intensity", "Normal Wavelength 2 Intensity", "Relative Intensity")
            test
        })
        
        #Display row information
        nearPoints(dataset_2()[,-c(3:6,8:9)], input$click3, threshold = 15, maxpoints = 1)
        
    })
    
    #Output Information for interactive histogram (Midpoint)
    output$info6 <-renderPrint({
        
        #If no bin is clicked on
        if(is.null(input$click4$x)) return()
        
        #If a bin is clicked on
        #Calculate break points
        bins <- seq(min(dataset()[,3]),max(dataset()[,3]), length.out = input$inslider + 1)
        
        #Create reactive value for current peak wavelength histogram
        z <- reactive({
            hist(dataset()[,3], breaks = bins, main = "Histogram of Peak Wavelengths", xlab = "Peak Wavelengths", col = "blue", border = "white")
        })
        
        #Transform dataset() into dataset_2
        dataset_2 <- reactive({
            test <- dataset()
            colnames(test) <- c("X","Y","Wavelength","Intensity","Normal Intensity")
            test
        })
        
        #Determine indexes of limits where mouse is between
        lower_lim_ind <- tail(which(z()$breaks <= input$click4$x),1)
        higher_lim_ind <- head(which(z()$breaks >= input$click4$x),1)
        
        #Find the limits
        lower_lim <- z()$breaks[lower_lim_ind]
        higher_lim <- z()$breaks[higher_lim_ind]
        
        #Paste information
        paste("Midpoint between",lower_lim,"and", higher_lim,":", z()$mids[lower_lim < z()$mids & z()$mids < higher_lim])
        
        
    })
    
    #Output information for smoothing spline: local max through brush
    output$info7 <- renderPrint({
        
        #Determine x and y values of local max
        Wavelength <- local_max_points()[1,]
        Normal_Intensity <- local_max_points()[2,]
        
        #Print local max/min points
        brushedPoints(data.frame(Wavelength, Normal_Intensity), input$smoothPlot_brush, xvar = "Wavelength", yvar = "Normal_Intensity")
        
    })
    
    
    ###DATA TABLES###
    
    #Table 1 Output
    output$table <- renderTable({
        
        #Transform dataset() into dataset_2
        dataset_2 <- reactive({
            test <- dataset()
            colnames(test) <- c("X","Y",paste0("&lambda;",tags$sub("peak")),"Intensity", "Normal Intensity")
            test
        })
        
        #Print dataset_2
        dataset_2()
    }, sanitize.text.function = function(x) x)
    
    #Table 2 Output
    output$table1 <- renderTable({
        
        #Use both absolute max functions
        dataset_df <- abs_max_func_2(first_df(), input$wavelength1, input$wavelength2)
        
        #Create column names
        colnames(dataset_df) <- c(paste(paste0("&lambda;",tags$sub(1)),"Intensity"), paste(paste0("&lambda;",tags$sub(2)),"Intensity"), paste("Normal", paste0("&lambda;",tags$sub(1)),"Intensity"), paste("Normal", paste0("&lambda;",tags$sub(2)),"Intensity"), "Relative Intensity")
        
        #Display data
        dataset_df
        
    }, sanitize.text.function = function(x) x)
    
}


# Run the application 
shinyApp(ui = ui, server = server)
