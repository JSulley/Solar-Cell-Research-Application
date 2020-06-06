#Absolute Max Function
abs_max_func <- function(x) {
  
  #Assign data frame  as x and format it to a dataframe
  x <- as.data.frame(x)
  
  #Subset the wavelengths as a vector by unlisting (Wavelengths are in the
  #first row). 
  dataset_wavelengths <- x[1,]
  
  #Get rid of the names associated with each vector element.
  dataset_wavelengths <- unlist(dataset_wavelengths, use.names = FALSE)
  
  #Since we are subsetting the wavelengths, we need to get rid of the first row of x
  #Note -c(1,2) will get rid of the first two elements because they are NA
  dataset_wavelengths <- dataset_wavelengths[-c(1,2)]
  x <- x[-1,]
  
  #Subset coordinates from x (first two columns)
  dataset_coordinates <- x[,c(1,2)]
  
  #Subset x coordinates (first column of dataset_coordinates)
  x_coordinate <- unlist(dataset_coordinates[,1], use.names = FALSE)
  
  #Subset y coordinates (second column of dataset_coordinates)
  y_coordinate <- unlist(dataset_coordinates[,2], use.names = FALSE)
  
  #Omit first two columns from x
  x <- x[,-c(1,2)]
  
  #Determine the dimensions of x. 
  #The number of rows (first entry) indicate the number of iterations needed to extract the desired information
  num_of_rows <- dim(x)[1]
  
  #Make a list for the x,y coordinate and the wavelength
  wavelength_list <- list()
  
  #Make for loop
  for (i in 1:num_of_rows){
    
    #Subset the ith row and convert to a vector by unlisting along with
    #getting rid of the names associated with each element.
    ith_row <- x[i,]
    ith_row_vector <- unlist(ith_row, use.names = FALSE)
    
    #Make smooth spline with wavelengths on the horizontal axis and the corresponding intensities
    #on the vertical axis
    splinesmoothfit <- smooth.spline(x = dataset_wavelengths, y = ith_row_vector)
    
    #Use the smooth spline line to predict the values for each wavelength
    #Find the y-values for each wavelength on smooth interpolation
    xyTabSmooth <- predict(splinesmoothfit, x = data.frame(dataset_wavelengths))
    
    #Make xyTabSmooth into data frame
    xyTabSmooth <- as.data.frame(xyTabSmooth)
    colnames(xyTabSmooth) <- c("Wavelength", "Y Value")
    
    #Determine the highest y-value for it and locate the 
    #wavelength that corresponds to it and search for duplicates
    maxYvalSmooth <- max(xyTabSmooth[,2])
    index <- which(xyTabSmooth[,2]==maxYvalSmooth)
    wavelength_list[[i]] <- data.frame(x_coordinate[i],y_coordinate[i],dataset_wavelengths[index],xyTabSmooth[index,2])
    
  }
  
  #Bind all the rows of the list together and make the object a data frame
  wavelength_df <- rbindlist(wavelength_list)
  wavelength_df <- as.data.frame(wavelength_df)
  
  #Add normalized column by finding the max intensity and dividing 
  #every number in the "intensity" column by the max intensity.
  max_intensity_value = max(wavelength_df[,4])
  normalized_int = wavelength_df[,4]/max_intensity_value
  
  #Combine data frame with the normalized intensity vector
  wavelength_df <- cbind(wavelength_df, normalized_int)
  
  #Display dataframe
  wavelength_df
}