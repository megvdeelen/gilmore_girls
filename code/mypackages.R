
# Packages ----------------------------------------------------------------

#List of packages. Installs and loads missing packages. 
#Function that installs missing packages 
using <- function(...) {
  libs <- unlist(list(...))
  req <- unlist(lapply(libs, require, character.only = TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

mypackages <- c("dplyr", "stringr", "tidyverse", "lubridate", "ggplot2",
                "ggvis", "shiny", "lme4", "survival", "caret",
                "rmarkdown", "readr", "styler", "xtable","janitor", "devtools", "expm",
                "heemod", "diagram", "microbenchmark")

using(mypackages)


#microbenchmark() paste in code to check which is faster
#don't forget you can use return in R as a function
# amazing keyboard shortcuts ----------------------------------------------

#switch doe to comments. ctrl + shift + c
#pipe cntrl + shift + m 
#assignment operator 
#select all of the same thing ctrl + alt + shift + m

