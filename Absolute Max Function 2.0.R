#Absolute Max Function
abs_max_func_2 <- function(x, wave1, wave2) {
  
  #Assign data frame as x and format it to a dataframe
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
  
  #Make a list of data frames with the x,y coordinates and the wavelength
  intensity_list <- list()
  
  #Make for loop
  for (i in 1:num_of_rows){
    
    #Subset the ith row and convert to a vector by unlisting along with
    #getting rid of the names associated with each element.
    ith_row <- x[i,]
    ith_row_vector <- unlist(ith_row, use.names = FALSE)
    
    #Make smooth spline with wavelengths as independent var. and the corresponding intensities as dependent var.
    splinesmoothfit <- smooth.spline(x = dataset_wavelengths, y = ith_row_vector)
    
    #Use smooth spline again to predict the values for wave1 and wave2
    xyTabSmooth <- predict(splinesmoothfit, x = data.frame(wave1,wave2))
    
    #Make xyTabSmooth into data frames
    xyTabSmooth <- as.data.frame(xyTabSmooth)
    
    #Make another column for the relative intensity and column bind the vector to the data frame
    relative_intensity <- xyTabSmooth[1,3]/xyTabSmooth[1,4]
    xyTabSmooth <- cbind(xyTabSmooth, relative_intensity)
    
    intensity_list[[i]] <- xyTabSmooth[,c(3,4,5)]
    
  }
  
  #Bind all the rows together and make object into dataframe
  intensity_df <- rbindlist(intensity_list)
  intensity_df <- as.data.frame(intensity_df)
  
  #Normalize intensities
  max_int_wave1 <- max(intensity_df[,1])
  max_int_wave2 <- max(intensity_df[,2])
  
  #Divide every number in said column by maximum number
  norm_wave1_int <- intensity_df[,1]/max_int_wave1
  norm_wave2_int <- intensity_df[,2]/max_int_wave2
  
  #Combine normalized columns with the existing table and rearrange accordingly
  intensity_df <- cbind(intensity_df, norm_wave1_int, norm_wave2_int)
  intensity_df <- intensity_df[,c(1,2,4,5,3)]
  
  #Display dataframe
  intensity_df
}