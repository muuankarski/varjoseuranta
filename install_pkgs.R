# Asenna puuttuvat paketit
list.of.packages <- c("shiny", "dplyr","leaflet","sf","bslib","shinyWidgets","ragg","shinycssloaders","lubridate","leaflet.extras")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
