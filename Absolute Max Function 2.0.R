#Absolute Max Function
abs_max_func_2 <- function(x, wave1, wave2) {
  
  #Convert to matrix
  x <- as.matrix(x)
  
  #Get rid of the coordinates from the data set
  x <- x[,-c(1,2)]
  
  #Subset the wavelengths as a vector by unlisting (Wavelengths are in the
  #first row). Get rid of the names associated with each vector element.
  #Since we are subsetting the wavelengths, we need to get rid of the first
  #row in the big dataset
  #Note -c(1,2) will get rid of the first two elements because they are 
  #NA
  dataset_wavelengths <- unlist(x[1,], use.names = FALSE)
  x <- x[-1,]
  
  
  #Make a list for the predicted wavelength 1 and 2, their respective normalized values, and relative intensity
  intensity_list <- list()
  
  #Make for loop
  #The number of rows will be the iterations needed to get the information we need.
  for (i in 1:nrow(x)){
    
    #Subset the ith row and convert to a vector by unlisting along with
    #getting rid of the names associated with each element.
    ith_row <- unlist(x[i,], use.names = FALSE)
    
    #Make smooth spline with wavelengths on the horizontal axis and the corresponding intensities
    #on the vertical axis
    splinesmoothfit <- smooth.spline(x = dataset_wavelengths, y = ith_row)
    
    #Use smooth spline again to predict the values for wave1 and wave2
    xyTabSmooth <- predict(splinesmoothfit, x = data.frame(wave1,wave2))
    
    #Make xyTabSmooth into data frames
    xyTabSmooth <- as.data.frame(xyTabSmooth)
    
    #Make another column for the relative intensity and column bind the vector to the data frame
    relative_intensity <- xyTabSmooth[1,3]/xyTabSmooth[1,4]
    xyTabSmooth <- cbind(xyTabSmooth, relative_intensity)
    
    intensity_list[[i]] <- xyTabSmooth[,c(3,4,5)]
    
  }
  
  #Convert list to dataframe
  intensity_df <- as.data.frame(rbindlist(intensity_list))
  
  #Normalize intensities
  #Divide every number in said column by maximum number
  norm_wave1_int <- intensity_df[,1]/max(intensity_df[,1])
  norm_wave2_int <- intensity_df[,2]/max(intensity_df[,2])
  
  #Combine normalized columns with the existing table and rearrange accordingly
  intensity_df <- cbind(intensity_df, norm_wave1_int, norm_wave2_int)
  intensity_df <- intensity_df[,c(1,2,4,5,3)]
  
  #Display dataframe
  intensity_df
}
