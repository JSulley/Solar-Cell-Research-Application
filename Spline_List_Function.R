# About This Function ----
# Getting spline list for referencing to speed up rendering of smoothing spline
# plots in the application.
#
# Parameters:
# wavelengths - Vector of wavelengths from uploaded dataset.
# intensities - Matrix of intensity values from uploaded dataset.


# Initializing Function ----
get_spline_list <- function(wavelengths, intensities) {
  number_of_rows <- nrow(intensities)  # Determine number of rows
  
  # Initialize spline list with length "number_of_rows"
  spline_list <- vector("list", number_of_rows)
  
  # Create for loop
  for (i in 1:number_of_rows) {
    # Remove names associated with intensity values
    intensities_vec <- unlist(intensities[i, ], use.names = FALSE)
    
    # Calculate smoothing spline
    spline_list[[i]] <- smooth.spline(x = wavelengths, y = intensities_vec)
  }
  spline_list  # Print spline_list
}
