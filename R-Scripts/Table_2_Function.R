# About this function ----
# Obtaining Table 2
# This function uses the spline list (obtained from Spline_List_Function.R) to
# predict the intensity at two wavelengths on every curve. 
# It outputs a dataframe with the following columns:
# Wavelength 1 Int., Wavelength 2 Int., Normal Wavelength 1 Int., Normal Wavelength 2 Int., Relative Intensity
#
# This dataframe is known as  Table 2 in the program.
#
# Parameters
# wavelength1 - Wavelength 1 inputted by user
# wavelength2 - Wavelength 2 inputted by user
# spline_list - List of smoothing splines
#
# Note: 
# Each wavelength's intensity is normalized by dividing the value
# by the wavelength's respective largest max intensity value.
# Int. is short for intensity


# Initializing Function ----
get_table_2 <- function(wavelength1, wavelength2, spline_list) {
  # Initialize zero matrix
  number_of_rows <- length(spline_list)  # Find number of rows
  number_of_columns <- 5  # Assign number of columns (see "About This Function")
  number_of_entries <- number_of_rows * number_of_columns  # Calculate number of entries
  entries <- rep(0, number_of_entries)  # Create zero vector with length "number_of_entries"
  table_2_mat <- matrix(entries, ncol = number_of_columns)  # Set zero matrix
  
  # Create for loop
  for (i in 1:number_of_rows) {
    # Get smoothing spline model for the ith row
    smoothing_spline <- spline_list[[i]]
    
    # Predict intensity value at both inputted wavelengths using "smoothing_spline"
    coordinates_list <- predict(smoothing_spline, data.frame(wavelength1, wavelength2))
    coordinates_df <- data.frame(coordinates_list)  # Convert to dataframe
    coordinates_mat <- as.matrix(coordinates_df)  # Convert to matrix
    
    wavelength_1_intensity <- coordinates_mat[1, 3]  # Assign wavelength 1 intensity
    wavelength_2_intensity <- coordinates_mat[1, 4]  # Assign wavelength 2 intensity
    
    # Calculate relative intensity
    relative_intensity <- wavelength_1_intensity/wavelength_2_intensity
    
    # Bind "coordinates_mat" and "relative_intensity" by column
    xyTabSmooth <- cbind(coordinates_mat, relative_intensity)
    
    # Place values "Wavelength 1 Intensity", "Wavelength 2 Intensity" and
    # "Relative Intensity" in the first 3 columns (ith row) of 
    # "table_2_mat"
    table_2_mat[i, 1:3] <- xyTabSmooth[, 3:5]
    
  }
  
  # Normalize wavelength 1 and 2 intensities by dividing by their largest
  # respective intensity 
  max_wavelength1_intensity <- max(table_2_mat[, 1])
  max_wavelength2_intensity <- max(table_2_mat[, 2])
  normal_wavelength1_intensity <- table_2_mat[, 1]/max_wavelength1_intensity
  normal_wavelength2_intensity <- table_2_mat[, 2]/max_wavelength2_intensity
  
  # Make "normal_wavelength1_intensity" as the 4th column of "table_2_mat"
  # Make "normal_wavelength2_intensity" as the 5th column of "table_2_mat"
  table_2_mat[, 4] <- normal_wavelength1_intensity
  table_2_mat[, 5] <- normal_wavelength2_intensity
  table_2_mat <- table_2_mat[, c(1, 2, 4, 5, 3)]  # Rearrange columns
  table_2 <- as.data.frame(table_2_mat)  # Make matrix into dataframe
  table_2  # Print "table_2"
}
