# About This Function:
# For each coordinate, the function finds a smoothing spline
# that describes the relationship between the given intensity values and
# wavelengths. The absolute max point on this curve is then calculated.
# The coordinate, peak wavelength, and absolute max intensity
# are recorded, and the function goes to the next coordinate repeating
# the same process.
# The absolute max intensities are then normalized by dividing each value
# by the largest absolute max intensity, and these values are placed next to the original 
# values. 
# The output is a dataframe with the following columns:
# x-coordinate, y-coordinate, Peak Wavelength, Absolute Max Intensity, Normal Intensity
# 
# Note: 
# Peak Wavelength is the wavelength that outputs the absolute max intensity

################################################################################


absolute_max_function <- function(dataframe) {
  
  # Convert 'dataframe' to matrix
  data_matrix <- as.matrix(dataframe)
  
  # Subset the wavelengths from 'data_matrix'
  # Wavelengths are located on the first row
  # The first two entries in that row are NA so remove them
  dataset_wavelengths <- unlist(data_matrix[1,-(1:2)], use.names = FALSE)
  
  # Remove the first row from 'data_matrix'
  data_matrix <- data_matrix[-1,]
  
  # Create vector consisting of the smallest and largest wavelengths
  min_wavelength <- min(dataset_wavelengths)
  max_wavelength <- max(dataset_wavelengths)
  wavelength_limits <- c(min_wavelength, max_wavelength)
  
  # Subset the coordinates from 'data_matrix' 
  # x-coordinates are in the first column; y-coordinates are in the second column
  x_coordinates <- unlist(data_matrix[,1], use.names = FALSE)
  y_coordinates <- unlist(data_matrix[,2], use.names = FALSE)
  data_matrix <- data_matrix[,-(1:2)]
  
  # Get the number of rows of 'data_matrix'
  number_of_rows <- nrow(data_matrix)
  
  # Initialize matrix with all zeros
  # Size: 'number_of_rows' by 5
  # 5 columns: x-coordinate, y-coordinate, peak wavelength, intensity, normal intensity
  # The number of entries = number_of_rows * 5
  number_of_columns <- 5
  entries <- rep(0, number_of_rows * number_of_columns)
  
  absolute_max_matrix <- matrix(entries, ncol = number_of_columns)
  
  # Make for loop from 1 to 'number_of_rows'
  for (i in 1:number_of_rows) {
    
    # Subset the ith row of 'data_matrix' and convert to a vector
    ith_row <- unlist(data_matrix[i,], use.names = FALSE)
    
    # Fit smooth curve with wavelengths (dataset_wavelengths) as x
    # and intensities (ith_row) as y 
    smoothSplineFit <- smooth.spline(x = dataset_wavelengths, y = ith_row)
    
    # Solve absolute max coordinate problem by first calculating the intensity at the
    # lower/upper limits of the wavelengths (wavelength_limits) using the
    # smooth curve (smoothSplineFit)
    coordinate_list <- predict(smoothSplineFit, x = wavelength_limits)
    
    # y-values in 'coordinate_list' are the predicted intensity values
    intensity_values <- unlist(coordinate_list$y, use.names = FALSE)
    
    # Remove names from each element
    intensity_values_endpoints <- unlist(intensity_values, use.names = FALSE)
    
    # Finish by first finding the critical values (where the derivative is 0)
    piecePolySpline <- SmoothSplineAsPiecePoly(smoothSplineFit)
    critical_values <- solve(piecePolySpline, deriv = 1)
    
    # Calculate intensity for each critical value
    intensity_values_critical <- predict(piecePolySpline, critical_values)
    
    # Determine if max value corresponds to a critical value or not
    if (max(intensity_values_critical) > max(intensity_values_endpoints)) {
      
      # If local extrema contains the absolute max
      absolute_max_intensity <- max(intensity_values_critical)
      peak_wavelength <- critical_values[which.max(intensity_values_critical)]
      
    } else {
      
      # If either endpoint has absolute max
      absolute_max_intensity <- max(intensity_values_endpoints)
      peak_wavelength <- wavelength_limits[which.max(intensity_values_endpoints)]
      
    }
    
    absolute_max_matrix[i, 1:4] <- c(x_coordinates[i], y_coordinates[i], peak_wavelength, absolute_max_intensity)
    
  }
  
  # Add normalized column by finding the max intensity and dividing
  # every number in the "intensity" column by the max intensity.
  max_intensity_value <- max(absolute_max_matrix[,4])
  normalized_intensity <- absolute_max_matrix[,4]/max_intensity_value
  
  # Combine data frame with the normalized intensity values
  absolute_max_matrix[,5] <- normalized_intensity
  
  # Make it into dataframe
  absolute_max_dataframe <- as.data.frame(absolute_max_matrix)
  
  absolute_max_dataframe
  
}
