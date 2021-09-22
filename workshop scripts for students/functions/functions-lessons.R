fahr_to_kelvin <- function(temp) { # name the function, identify the arguments
  kelvin <- ((temp - 32)) * (5/9) + 273.15 # actual function
  return(kelvin) # return a result
}



## Challenge 1
# Write a function called kelvin_to_celsius() that takes a temperature in Kelvin and returns that temperature in Celsius.

kelvin_celsius <- function(temp){
  celsius <- ((temp - 273.15))
  return(celsius)
}



## Challenge 2
# Define the function to convert directly from Fahrenheit to Celsius, by reusing the two functions above (or using your own functions if you prefer).


fahr_celsius <- function(temp){
  temp_kelvin <- fahr_to_kelvin(temp)
  result <- kelvin_celsius(temp_kelvin)
  return(result)
}

calcGDP <- function(dat, year = NULL, country = NULL) { # create the function, null are default
  # arguments:
  # dat: required! a dataset with pop and gdpPercap
  # year: optional, a numeric vector, a year
  # country: opetion, character vector, a country
  if(!is.null(year)) { # if year is specified
    dat <- dat[dat$year %in% year, ] # subset out just the user-specified year
  }
  if(!is.null(country)) { # if country is specified
    dat <- dat[dat$country %in% country,] # and, if so, subset out just that country 
  }
  
  gdp <- dat$pop * dat$gdpPercap # then, calculate the gdp for each row in the subset
  new <- cbind(dat, gdp = gdp) # stick that gdp on as a new column in the dataset
  return(new) # return your enw dataframe with the gdp
}



