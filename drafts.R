# Set number of significant digits
options(digits = 3)

# The 'load_lib' function installs and loads
# a vector of libraries
load_lib <- function(libs) {
  sapply(libs, function(lib) {
    
    # Load the package. If it doesn't exists, install and load.
    if(!require(lib, character.only = TRUE)) {
      
      # Install the package
      install.packages(lib)
      
      # Load the package
      library(lib, character.only = TRUE)
    }
  })}

# Load the libraries used in this section
libs <- c("tidyverse", "icesTAF", "readr", 
          "lubridate", "caret")

load_lib(libs)