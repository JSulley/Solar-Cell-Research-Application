# About This Function ----
# Obtaining Table 1
# For each pair of coordinates in the uploaded dataset, the function calculates
# a smoothing spline where intensity is a function of wavelengths. It then finds
# the absolute max coordinates using the spline. Afterward, the function records
# the dataset coordinates with the absolute max coordinates in a 5-column matrix
# filling the first four columns.
#
# Next, the function normalizes the absolute max intensities by dividing each
# value by the largest one, and these modified values occupy the 5th column of
# the matrix mentioned previously.
#
# The matrix is then converted to a dataframe with the following columns:
# x-coordinate, y-coordinate, Peak Wavelength, Absolute Max Intensity, Normal Intensity
#
# This dataframe is known as Table 1 in the program.
#
# Parameters
# wavelengths - Vector of wavelengths from the uploaded dataset.
# coordinates - Two-column matrix of coordinates from the uploaded dataset.
# spline_list - List of smoothing splines (Refer to Spline_List_Function.R)
#
# Note:
# Peak Wavelength is the wavelength corresponding to the absolute max intensity.


# Initializing Function ----
get_table_1 <- function(wavelengths, coordinates, spline_list) {
  min_wavelength <- min(wavelengths)  # Find minimum wavelength
  max_wavelength <- max(wavelengths)  # Find maximum wavelength
  
  # Initialize vector of limits of wavelengths
  endpoints <- c(min_wavelength, max_wavelength)
  
  # Initialize zero matrix
  number_of_rows <- nrow(coordinates)  # Find number of rows
  number_of_columns <- 5  # Number of columns needed (see "About This Function")
  number_of_entries <- number_of_rows * number_of_columns  # Calculate number of entries
  entries <- rep(0, number_of_entries)  # Create zero vector with length "number_of entries"
  table_1_mat <- matrix(entries, ncol = number_of_columns)  # Build zero matrix
  
  # Create for loop
  for (i in 1:number_of_rows) {
    smoothing_spline <- spline_list[[i]]  # Refer ith row smoothing spline
    
    # Solve absolute max coordinates problem by first calculating the intensity
    # at each value in "endpoints" with the ith row's smooth curve
    # from "spline_list"
    coordinates_list <- predict(smoothing_spline, endpoints)
    
    # Predicted intensities are in "coordinates_list$y"
    endpoint_intensities <- unlist(coordinates_list$y, use.names = FALSE)
    
    # Remove names from each element
    endpoint_intensities <- unlist(endpoint_intensities, use.names = FALSE)
    
    # Find values where first derivative is 0
    piece_poly_spline <- SmoothSplineAsPiecePoly(smoothing_spline)
    critical_values <- solve(piece_poly_spline, deriv = 1)
    
    # Calculate intensity at each critical value
    critical_value_intensities <- predict(piece_poly_spline, critical_values)
    
    # Determine if absolute max intensity corresponds to a critical value or
    # endpoint by finding the max values in both "critical_value_intensities"
    # and "endpoints_intensities"
    greatest_critical_int <- max(critical_value_intensities)  
    greatest_endpoint_int <- max(endpoint_intensities)
    
    # Compare "greatest_critical_int" with "greatest_endpoint_int"
    if (greatest_critical_int > greatest_endpoint_int) {
      # Local extrema contain the absolute max
      absolute_max_intensity <- greatest_critical_int
      peak_wavelength <- critical_values[which.max(critical_value_intensities)]
    } else {
      # Endpoints contain the absolute max
      absolute_max_intensity <- greatest_endpoint_int
      peak_wavelength <- endpoints[which.max(endpoint_intensities)]
    }
    
    # Record values in row i of "table_1_mat"
    table_1_mat[i, 1:4] <- c(coordinates[i, 1],
                             coordinates[i, 2],
                             peak_wavelength,
                             absolute_max_intensity)
    
  }
  
  # Find the largest absolute max intensity (called Dataset Max Intensity)
  dataset_max_intensity <- max(table_1_mat[, 4])
  
  # Divide each absolute max intensity by "dataset_max_intensity"
  # to normalize them
  normalized_intensities <- table_1_mat[, 4] / dataset_max_intensity
  
  # 5th column of "table_1_mat" is normalized intensities
  table_1_mat[, 5] <- normalized_intensities
  
  # Convert to dataframe
  table_1 <- as.data.frame(table_1_mat)
  table_1 # Print "table_1"
}
