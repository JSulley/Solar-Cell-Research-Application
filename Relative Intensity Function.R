# About this function:
# This function accepts two wavelength values entered by the user and the uploaded 
# dataset and predicts the intensity values using each coordinate's respective smoothing spline
# It outputs a dataframe with the following columns:
# Wavelength 1 Intensity, Wavelength 2 Intensity, Wavelength 1 NI, Wavelength 2 NI, Relative Intensity
# 
# Note: 
# NI means Normal Intensity
# The intensity values for each wavelength are normalized by dividing every intensity value
# their respective max intensity value  

################################################################################

relative_intensity_function <- function(dataframe, wavelength1, wavelength2) {
  
  #Convert 'dataframe' to matrix
  data_matrix <- as.matrix(dataframe)
  
  #Remove the coordinates from the data set
  #Coordinates are in the first two columns
  data_matrix <- data_matrix[,-(1:2)]
  
  #Subset the wavelengths from 'data_matrix'
  #Wavelengths are located on the first row
  #The first two entries in that row are NA so remove them
  dataset_wavelengths <- unlist(data_matrix[1,], use.names = FALSE)
  data_matrix <- data_matrix[-1,]
  
  #Get the number of rows
  number_of_rows <- nrow(data_matrix)
  
  #Initialize matrix with all zeros
  #Size: 'number_of_rows' by 5
  #5 columns: 
  #wavelength 1 intensity, wavelength 2 intensity, wavelength 1 normal intensity, wavelength 2 normal intensity, relative intensity
  #The number of entries = number_of_rows * 5
  number_of_columns <- 5
  entries <- rep(0, number_of_rows * number_of_columns)
  
  relative_intensity_matrix <- matrix(entries, ncol = number_of_columns)
  
  for (i in 1:number_of_rows) {
    
    #Subset the ith row and convert to a vector by unlisting along with
    #getting rid of the names associated with each element.
    ith_row <- unlist(data_matrix[i,], use.names = FALSE)
    
    #Make smooth spline with wavelengths on the horizontal axis and the corresponding intensities
    #on the vertical axis
    splinesmoothfit <- smooth.spline(x = dataset_wavelengths, y = ith_row)
    
    #Use smooth spline again to predict the values for wavelength1 and wavelength2
    xyTabSmooth <- predict(splinesmoothfit, x = data.frame(wavelength1,wavelength2))
    
    #Make xyTabSmooth into data frames
    xyTabSmooth <- data.frame(xyTabSmooth)
    xyTabSmooth <- as.matrix(xyTabSmooth)
    
    #Make another column for the relative intensity and column bind the vector to the data frame
    relative_intensity <- xyTabSmooth[1,3]/xyTabSmooth[1,4]
    xyTabSmooth <- cbind(xyTabSmooth, relative_intensity)
    
    relative_intensity_matrix[i,1:3] <- xyTabSmooth[,3:5]
    
  }
  
  #Normalize intensities
  #Divide every number in said column by maximum number
  max_intensity_value_1 <- max(relative_intensity_matrix[,1])
  max_intensity_value_2 <- max(relative_intensity_matrix[,2])
  normal_wavelength1_intensity <- relative_intensity_matrix[,1]/max_intensity_value_1
  normal_wavelength2_intensity <- relative_intensity_matrix[,2]/max_intensity_value_2
  
  #Place 'normal_wavelength1_intensity' as the 4th column of 'relative_intensity_matrix'
  #Place 'normal_wavelength2_intensity' as the 5th column of 'relative_intensity_matrix'
  relative_intensity_matrix[,4] <- normal_wavelength1_intensity
  relative_intensity_matrix[,5] <- normal_wavelength2_intensity
  
  #Rearrange columns and make matrix into dataframe
  relative_intensity_matrix <- relative_intensity_matrix[,c(1,2,4,5,3)]
  relative_intensity_df <- as.data.frame(relative_intensity_matrix)
  
  relative_intensity_df
}
