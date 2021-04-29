# About this function:
# This function requires two wavelength values entered by the user and a list 
# of smoothing spline models. It predicts the intensity values using each coordinate's respective smoothing spline and outputs a dataframe with the following columns:
# Wavelength 1 Intensity, Wavelength 2 Intensity, Wavelength 1 NI, Wavelength 2 NI, Relative Intensity
#  
# Note: 
# NI means Normal Intensity.
# Each wavelength's intensity values are normalized by dividing them
# by the wavelength's max intensity value.

################################################################################


relative_intensity_function <- function(wavelength1, wavelength2, ss_model_list) {
  
  number_of_rows <- length(ss_model_list)
  
  # Initialize matrix with all zeros
  # Size: 'number_of_rows' by 5
  # 5 columns: 
  # wavelength 1 intensity, wavelength 2 intensity, wavelength 1 normal intensity, wavelength 2 normal intensity, relative intensity
  # The number of entries = number_of_rows * 5
  number_of_columns <- 5
  entries <- rep(0, number_of_rows * number_of_columns)
  
  relative_intensity_matrix <- matrix(entries, ncol = number_of_columns)
  
  for (i in 1:number_of_rows) {
    
    # Get smoothing spline model for the ith row
    splinesmoothfit <- ss_model_list[[i]]
    
    # Predict intensity value at both inputted wavelengths using 'splinesmoothfit'
    xyTabSmooth <- predict(splinesmoothfit, x = data.frame(wavelength1,wavelength2))
    
    xyTabSmooth <- data.frame(xyTabSmooth)
    xyTabSmooth <- as.matrix(xyTabSmooth)
    
    # Make another column for the relative intensity and column bind the relative intensities to the dataframe
    wavelength_1_intensity <- xyTabSmooth[1,3]
    wavelength_2_intensity <- xyTabSmooth[1,4]
    relative_intensity <- wavelength_1_intensity/wavelength_2_intensity
    xyTabSmooth <- cbind(xyTabSmooth, relative_intensity)
    
    relative_intensity_matrix[i,1:3] <- xyTabSmooth[,3:5]
    
  }
  
  # Normalize intensities for wavelength 1 and 2 by dividing each column
  # by their largest respective absolute max intensity 
  max_intensity_value_1 <- max(relative_intensity_matrix[,1])
  max_intensity_value_2 <- max(relative_intensity_matrix[,2])
  normal_wavelength1_intensity <- relative_intensity_matrix[,1]/max_intensity_value_1
  normal_wavelength2_intensity <- relative_intensity_matrix[,2]/max_intensity_value_2
  
  # Place 'normal_wavelength1_intensity' as the 4th column of 'relative_intensity_matrix'
  # Place 'normal_wavelength2_intensity' as the 5th column of 'relative_intensity_matrix'
  relative_intensity_matrix[,4] <- normal_wavelength1_intensity
  relative_intensity_matrix[,5] <- normal_wavelength2_intensity
  
  # Rearrange columns and make matrix into dataframe
  relative_intensity_matrix <- relative_intensity_matrix[,c(1,2,4,5,3)]
  relative_intensity_df <- as.data.frame(relative_intensity_matrix)
  
  relative_intensity_df
}
