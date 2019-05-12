# EveryBreathYouTake
The visualisation can be viewed at http://shiny.evl.uic.edu:3838/g3/Every_Breath_You_Take/
It is an interactive visualisation concerning United States and Hong Kong's weather data on a YEARLY, DAILY and HOURLY basis.

The pre-processed and cleaned data files can be found at: https://drive.google.com/file/d/1bhHpvdwdsZ8Pvzw00yPRwgaSnrYMtAXp/view


Project 2 - Every Breath You Take - CS 424 Visualization Analytics - Spring'19

Please follow the below to run the code.

Software required to run the code: R and R Studio

To download and install R: From site https://cran.cnr.berkeley.edu/ and installing it. (R (20178-12-20 Eggshell Igloo))

To download and install R-Studio: From site https://www.rstudio.com/products/rstudio/download/ and installing it. (R-Studio (1.1.463))

Libraries needed to run the application:

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(markdown)
library(shinythemes)
library(shinycssloaders)
library(plyr)
library(lubridate)
library(dplyr)
library(data.table)

To see what libraries are currently installed, in the terminal at the > prompt, type installed.packages()[,1:2]

If any library is missing, install them using install.packages(). Example: install.packages(shiny)

Please follow the below folder structure while running the preprocess code.

Raw Files/Daily --> (Place the extracted daily raw files for the US here)
Raw Files/Monthly --> (Place the extracted Monthly files for the US here)
Raw Files/Hong Kong --> (Place the Individual pollutant file here, download the data for Hong Kong separately for each pollutants CO, NO2, Ozone, PM 2.5, PM 10 and SO2)

Naming for the Hong Kong raw files.
Ozone file --> O3.csv
NO2 file --> NO2.csv
CO file --> CO.csv
PM 2.5 --> PM2_5.csv
PM 10 --> PM10.csv
SO2 --> SO2.csv

The naming for all other files will remain same as when downloaded and extracted.

Place the preprocess.R files one folder level above the Raw Files directory and the preprocessed files will be generated. This may take up to 10 to 15 minutes of time. 
