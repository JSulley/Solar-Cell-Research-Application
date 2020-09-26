#Absolute Max Function
abs_max_func <- function(x) {
  
  #Convert to matrix
  x <- as.matrix(x)
  
  #Subset the wavelengths as a vector by unlisting (Wavelengths are in the
  #first row). Get rid of the names associated with each vector element.
  #Since we are subsetting the wavelengths, we need to get rid of the first
  #row in the big dataset
  #Note c(-1,-2) will get rid of the first two elements because they are 
  #NA
  dataset_wavelengths <- unlist(x[1,-c(1,2)], use.names = FALSE)
  x <- x[-1,]
  
  #Determine x values of endpoints
  endpoints_x <- c(min(dataset_wavelengths), max(dataset_wavelengths))
  
  #Get the coordinates from the data set and omit the first two columns from data frame
  x_coordinate <- unlist(x[,1], use.names = FALSE)
  y_coordinate <- unlist(x[,2], use.names = FALSE)
  x <- x[,-c(1,2)]
  
  #Make a list for the x,y coordinate and the wavelength
  wavelength_list <- list()
  
  #Make for loop
  #The number of rows will be the iterations needed to get the information we need.
  for (i in 1:nrow(x)){
    
    #Subset the ith row and convert to a vector by unlisting along with
    #getting rid of the names associated with each element.
    ith_row <- unlist(x[i,], use.names = FALSE)
    
    #Make smooth spline with wavelengths on the horizontal axis and the corresponding intensities
    #on the vertical axis
    splinesmoothfit <- smooth.spline(x = dataset_wavelengths, y = ith_row)
    predicted <- predict(splinesmoothfit, x = endpoints_x)
    endpoints_y <- unlist(predicted$y, use.names = FALSE)
    
    #Solve for critical values where derivative equals to 0
    newsmooth <- SmoothSplineAsPiecePoly(splinesmoothfit)
    xs <- solve(newsmooth, deriv = 1)
    ys <- predict(newsmooth, xs)
    
    #Determine where max value lies
    if (max(ys) > max(endpoints_y)) {
      
      #If local extrema contains the absolute max
      abs_y <- max(ys)
      abs_x <- xs[which.max(ys)]
      
    } else {
      
      #If either endpoints has absolute max
      abs_y <- max(endpoints_y)
      abs_x <- endpoints_x[which.max(endpoints_y)]
      
    }
    
    #Place row's coordinates and absolute max coordinates into dataframe
    wavelength_list[[i]] <- data.frame(x_coordinate[i],y_coordinate[i], abs_x, abs_y)
    
  }
  
  #Bind all the rows of the list together and make the object a data frame
  wavelength_df <- as.data.frame(rbindlist(wavelength_list))
  
  #Add normalized column by finding the max intensity and dividing 
  #every number in the "intensity" column by the max intensity.
  normalized_int <- wavelength_df[,4]/max(wavelength_df[,4])
  
  #Combine data frame with the normalized intensity values
  wavelength_df <- cbind(wavelength_df, normalized_int)
  
  wavelength_df
}
