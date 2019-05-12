# Project 2 - Every Breath You Take 
# Course - CS 424 Visualization Analytics - Spring'19 

# Team(Group 3): 
# Sai Krishnan Thiruvarpu Neelakantan - sthiru5@uic.edu 
# Praveen Chandrasekaran - pchand34@uic.edu 
# Varsha Jayaraman - vjayar6@uic.edu 
# Abdullah Aleem - aaleem2@uic.edu 

#libraries used

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(grid)
library(leaflet)
library(scales)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse)
library(tmap)
library(tmaptools)
library(sf)
library(splitstackshape)
library(cdlTools)
library(plotly)
pdf(NULL)

#read all the annual data files and combine it as a single data 

temp = list.files(pattern="*.csv")
Data <- lapply(temp, read.csv)
AnnualData <- do.call(rbind, Data)
years<-c(2018:1990)

# Create a Shiny dashboard with a sidebar and tabs inside sidebar for navigation purposes across the page

ui <- dashboardPage(
	skin = 'red',
	dashboardHeader(title = "Every Breath You Take"),
	dashboardSidebar(
	sidebarMenu(
    htmlOutput("list_radio"),
		selectInput("year", "Choose a year:",years,selected = 2018),
    htmlOutput("choose_counties"),
		htmlOutput("getState"),
    htmlOutput("getCounty"),
    menuItem("Annual Data", tabName = "annualdata"),
    menuItem("Daily Data", tabName = "dailydata"),
    menuItem("Hourly Data", tabName = "hourlydata"),
    menuItem("Hong Kong", tabName = "hongkongdata"),
    htmlOutput("location_hk"),
    menuItem("Information", tabName = "information")
	)	
	),
	dashboardBody(

	#custom css
    	tags$head( 
    		tags$style(
    			HTML(".fa-calendar { display:none !important;} 
    				.shiny-output-error:before,.shiny-output-error{visibility: hidden !important;}
    				.compare-class > .tabbable > .nav-tabs {margin-bottom: 20px !important;} 
    				.box.box-solid.box-primary {border: 1px solid #dd4b39 !important;} 
    				.box.box-solid.box-primary>.box-header { background: #dd4b39 !important; background-color: #dd4b39 !important; }
    				.sidebar-menu>li {font-size:18px;}
    				.dayswithaqitext { font-size:22px;font-weight:bold;margin-bottom:20px; } 
            .sidebar-menu {margin-top: 60% !important; }
            .content > .tab-content {margin-top:3%} 
            a,span,label,.selectize-input,.sw-air-picker { font-size : 20px !important; }
            #content { font-size : 22px !important; }")
    	)
    ),

    tabItems(
    	tabItem(
        	tabName = "annualdata",

        	div(class='compare-class',

        	tabsetPanel(

        		tabPanel('AQI data for the year',

        		div(class="dayswithaqitext",textOutput("dayswithaqi")),
		  		fluidRow(
		    		column(6,
		    			box(title = "Categorized by Days", solidHeader = TRUE, status = "primary", width = 12,
		  					tabsetPanel(
		    					tabPanel("Pie Chart", withSpinner(plotOutput("piechart",height = 1000))),
		    					tabPanel("Bar Chart", withSpinner(plotOutput("barchart",height = 1000))),
		    					tabPanel("Table", withSpinner(dataTableOutput("table",height = 1000)))
		  					)
		  				)	

		    		),
		    		column(6,
		    			box(title = "Percentage of days as main Pollutant", solidHeader = TRUE, status = "primary", width = 12,
		  					tabsetPanel(
                  tabPanel("NO2", withSpinner(plotOutput("piechartpollutantNO2",height = 1000))),
                  tabPanel("Ozone", withSpinner(plotOutput("piechartpollutantOzone",height = 1000))),
                  tabPanel("PM2.5", plotOutput("piechartpollutantPM2.5",height = 1000)),
                  tabPanel("PM10", withSpinner(plotOutput("piechartpollutantPM10",height = 1000))),
                  tabPanel("SO2", withSpinner(plotOutput("piechartpollutantSO2",height = 1000))),
                  tabPanel("CO", withSpinner(plotOutput("piechartpollutantCO",height = 1000))),
		    					tabPanel("Bar Chart", withSpinner(plotOutput("barchartpollutant",height = 1000))),
		    					tabPanel("Table", withSpinner(dataTableOutput("tablepollutant",height = 1000)))
		  						)
		  					)	

		    			)
		    	)	
        	),

        		tabPanel('Over the Years (AQI)',
        			withSpinner(plotOutput("daysTime", height = 1000)),
                    checkboxGroupInput("lines1", "", c("Max" = "mx","90th Percentile" = "pr", "Median" = "md"),inline=TRUE, selected = c("mx"))
        		),
        		tabPanel('Over the Years (Pollutants)',
        			withSpinner(plotOutput("pollutantTime", height = 1000)),
                    checkboxGroupInput("lines2", "", c("CO" = "co","NO2" = "no", "SO2" = "so", "Ozone" = "oz","PM2.5" = "pm2", "PM10" = "pm10"),inline=TRUE, selected = c("co","so","no","oz","pm2","pm10"))
        		),
        		tabPanel('Pollutants Table',
        			withSpinner(dataTableOutput("tablepercentage",height = 1000))
        		),
            tabPanel('Pollutants - HeatMap',
              fluidRow(
                column(2, 
                  radioButtons("pollutant", "Select Pollutant", c("CO" = "co","NO2" = "no", "SO2" = "so", "Ozone" = "oz","PM2.5" = "pm2", "PM10" = "pm10"),inline=TRUE, selected = c("pm2")),
                  htmlOutput('createslider'),
                  radioButtons("paletteYearly", "Select Palette", c("Blue/Purple" = "BuPu", "Red/Yellow" = "YlOrRd"),inline=TRUE, selected = c("YlOrRd")),
                  radioButtons("breakYearly", "Select Break", c("Pretty" = "pretty", "Equal" = "equal", "Mean" = "kmeans"),inline=TRUE, selected = c("pretty"))
                ),
                column(10,withSpinner(leafletOutput("pollutantMap", height = 1200)))  
              )

              
              
            ),
            tabPanel('AQI - HeatMap',
              fluidRow(
                column(2,
                  radioButtons("Stat", "Select Stat", c("Max" = "mx","90th Percentile" = "pr", "Median" = "md"),inline=TRUE, selected = c("md")),
                  radioButtons("paletteYearlyAQI", "Select Palette", c("Blue/Purple" = "BuPu", "Red/Yellow" = "YlOrRd"),inline=TRUE, selected = c("YlOrRd")),
                  radioButtons("breakYearlyAQI", "Select Break", c("Pretty" = "pretty", "Equal" = "equal", "Mean" = "kmeans"),inline=TRUE, selected = c("pretty"))
                ),
                column(10,withSpinner(leafletOutput("aqiMap", height = 1200)))
                
              )
            ),
        		tabPanel('County Map',
        			withSpinner(leafletOutput("leaf",height=1000))
        		)

        	)
        	

        	)


        	),
    	tabItem(
        	tabName = "dailydata",
          tabsetPanel(
            tabPanel('Stacked Bar Chart',
              withSpinner(plotOutput("StackedBarChart",height=1000))
            ),
            tabPanel('Line Chart for Pollutants',
              withSpinner(plotlyOutput("linepollutant",height=1000))
            ),
            tabPanel('Heatmap - Pollutants',
              fluidRow(
                column(2, 
                  htmlOutput("choosedailydate"),
                  htmlOutput("choosedailypollutant"),
                  radioButtons("palettedaily", "Select Palette", c("Blue/Purple" = "BuPu", "Red/Yellow" = "YlOrRd"),inline=TRUE, selected = c("YlOrRd")),
                  radioButtons("breakdaily", "Select Break", c("Pretty" = "pretty", "Equal" = "equal", "Mean" = "kmeans"),inline=TRUE, selected = c("pretty"))
                  ),
                column(10,
                  withSpinner(leafletOutput("pollutantMapDaily", height = 1200))
                  )
                
              )
            ),
            tabPanel('Table',
              withSpinner(dataTableOutput("DailyTable"))
            )
          )

        	),
    	tabItem(
        	tabName = "hourlydata",
        	div(class="hourly-wrapper",
        		htmlOutput("getDate"),
            fluidRow(
              column(6,htmlOutput("getPollutants")),
              column(6,htmlOutput("getUnits"))
            ),
            withSpinner(plotOutput("linehourly",height=900))
        		)
        	),
      tabItem(
          tabName = "hongkongdata",
          tabsetPanel(
            tabPanel("Line Chart", 
                column(12,radioButtons("category_data", "Select Data", c("Hourly" = "hourly", "Daily" = "daily", "Monthly" = "monthly"),inline=TRUE, selected = c("hourly"))),
                column(12,htmlOutput("getdate_hk")),
                column(6,htmlOutput("getPollutants_hk")),
                column(6,htmlOutput("getUnits_hk")),
                plotOutput("linechart_hk",height=900)
                ),
            tabPanel("Location of the site",
                withSpinner(leafletOutput("leaf_hk",height=1000))
              )
            ) 
          ),
      tabItem(
          tabName = "information",
            div(htmlOutput("content"),style="font-size:18px;font-weight:bold;margin-bottom:20px;")
        )
      
    )	

)

)


server <- function(input, output, session) { 

# change default font size
theme_set(theme_light(base_size = 22))

# All the reactive inputs are assigned below

# Read the daily and year data file based on the year selected by the user

Dailydata <- reactive({ read.csv(paste0("Daily/daily_aqi_by_county_",input$year,".csv")) })

Yearlydata <- reactive({ read.csv(paste0("annual_aqi_by_county_",input$year,".csv")) })

#read the sites data for the getting the latitue/longitudes value

temp_map <- read.csv("leaflet/aqs_sites.csv")
temp_map<-temp_map[!(temp_map$Latitude==0),]
final_map<-subset(temp_map, select = c(Latitude, Longitude, State.Name, County.Name))

# selected year data retrieved from AnnualData variable 

selectedyeardata <- reactive({subset(AnnualData, AnnualData$Year == input$year)})

Overtheyears <- reactive({ subset(AnnualData, AnnualData$State == input$state_sel & AnnualData$County == input$county_sel) })

# Read the All_Gas_Dates.csv to get the pollutant array for the chosen date

Hourlydate <- reactive({ read.csv("Hourly/All_Gas_Dates.csv") })

#Read all the monthly pollutant file based on the month selected from the date

MonthlyCO <- reactive({ read.csv(paste0("Hourly/CO/CO_",format(input$date,"%m"),".csv")) })
MonthlyNO2 <- reactive({ read.csv(paste0("Hourly/NO2/NO2_",format(input$date,"%m"),".csv")) })
MonthlyOzone <- reactive({ read.csv(paste0("Hourly/Ozone/Ozone_",format(input$date,"%m"),".csv")) })
MonthlyPM2_5 <- reactive({ read.csv(paste0("Hourly/PM2_5/PM2_5_",format(input$date,"%m"),".csv")) })
MonthlyPM10 <- reactive({ read.csv(paste0("Hourly/PM10/PM10_",format(input$date,"%m"),".csv")) })
MonthlySO2 <- reactive({ read.csv(paste0("Hourly/SO2/SO2_",format(input$date,"%m"),".csv")) })
MonthlyTemp <- reactive({ read.csv(paste0("Hourly/Temp/Temp_",format(input$date,"%m"),".csv")) })
MonthlyWind <- reactive({ read.csv(paste0("Hourly/Wind/Wind_",format(input$date,"%m"),".csv")) })

#Heatmap data file for daily data
DailyMapDate <- reactive({ read.csv(paste0("Daily/Map/",format(input$dailydate,"%m"),".csv")) })

#Hong Kong Daily/Hourly/Monthly files

DailyHK_CO <- reactive({ read.csv("Hong_Kong/Daily/HK_CO_Daily.csv") })
DailyHK_NO2 <- reactive({ read.csv("Hong_Kong/Daily/HK_NO2_Daily.csv") })
DailyHK_O3 <- reactive({ read.csv("Hong_Kong/Daily/HK_O3_Daily.csv") })
DailyHK_PM10 <- reactive({ read.csv("Hong_Kong/Daily/HK_PM10_Daily.csv") })
DailyHK_PM2 <- reactive({ read.csv("Hong_Kong/Daily/HK_PM2_Daily.csv") })
DailyHK_SO2 <- reactive({ read.csv("Hong_Kong/Daily/HK_SO2_Daily.csv") })

HourlyHK_CO <- reactive({ read.csv("Hong_Kong/Hourly/HK_CO_Hourly.csv") })
HourlyHK_NO2 <- reactive({ read.csv("Hong_Kong/Hourly/HK_NO2_Hourly.csv") })
HourlyHK_O3 <- reactive({ read.csv("Hong_Kong/Hourly/HK_O3_Hourly.csv") })
HourlyHK_PM10 <- reactive({ read.csv("Hong_Kong/Hourly/HK_PM10_Hourly.csv") })
HourlyHK_PM2 <- reactive({ read.csv("Hong_Kong/Hourly/HK_PM2_Hourly.csv") })
HourlyHK_SO2 <- reactive({ read.csv("Hong_Kong/Hourly/HK_SO2_Hourly.csv") })

MonthlyHK_CO <- reactive({ read.csv("Hong_Kong/Monthly/HK_CO_Monthly.csv") })
MonthlyHK_NO2 <- reactive({ read.csv("Hong_Kong/Monthly/HK_NO2_Monthly.csv") })
MonthlyHK_O3 <- reactive({ read.csv("Hong_Kong/Monthly/HK_O3_Monthly.csv") })
MonthlyHK_PM10 <- reactive({ read.csv("Hong_Kong/Monthly/HK_PM10_Monthly.csv") })
MonthlyHK_PM2 <- reactive({ read.csv("Hong_Kong/Monthly/HK_PM2_Monthly.csv") })
MonthlyHK_SO2 <- reactive({ read.csv("Hong_Kong/Monthly/HK_SO2_Monthly.csv") })

#Preset Count,State selectinput value

SplitInput <- reactive({ input$county_list })

#Hong Kong Map files

leafletdataHK <- reactive({ read.csv("Hong_Kong/Map/HK_Location.csv") })

# Colours for days analysis
clrs = c("#146B3A", "peru", "#A84533", "#A30C15", "#580000", "black")

# data processing for heatmap
data_shp <- st_read("us_comp/county_geometry.shp", stringsAsFactors = FALSE)
data_shp$STATE = fips(as.numeric(data_shp$STATEFP), to='Name')
data_map = data.frame(
  State = data_shp$STATE,
  County = data_shp$NAME,
  Geometry = data_shp$geometry
)
data_map <- data_map[-c(1707),]
data_map$County = sub("St. ", "Saint ", data_map$County)
data_map$County = sub("Ste. ", "Sainte ", data_map$County)
data_map$County = sub("LaSalle", "La Salle", data_map$County)
data_map$County = sub("Charles City", "Charles", data_map$County)
data_map$County = sub("Carson City", "Carson", data_map$County)
data_map$County = sub("Doña Ana", "Dona Ana", data_map$County)
data_map$County = sub("Cataño", "Catano", data_map$County)
data_map$County = sub("Bayamón", "Bayamon", data_map$County)
data_map$State = sub("Deleware", "Delaware", data_map$State)


# Choose from given set of counties
output$choose_counties = renderUI({

  counties = c('Cook, Illinois', 'Hawaii, Hawaii', 'New York, New York', 'Los Angeles, California','King, Washington','Harris, Texas','Miami-Dade, Florida','San Juan, New Mexico','Hennepin, Minnesota','Wake, North Carolina','Dallas, Texas','Gila, Arizona')
  selectInput(inputId = "county_list", 
   label = "Choose from county list: ", 
   choices = counties)

})


output$list_radio = renderUI({

radioButtons("radiolistcounty", "", c("Default Counties" = "default", "All Counties" = "all"), selected = c("default"))

})

# Get the state from the selected data file(selected based on the year chosen by the user)
output$getState = renderUI({ 

  if(input$radiolistcounty == "all")
  {
  	Yearlydata <- Yearlydata()
  	states = Yearlydata$State
  }
  else
  {
    split_input = input$county_list
    selected_value = unlist(strsplit(split_input, ", "))
    states = selected_value[2]
  }

  selectInput(inputId = "state_sel", 
     label = "Choose a state: ", 
     choices = unique(states)
     )

})

# Get the County for that particular state
output$getCounty = renderUI({

  if(input$radiolistcounty == "all")
  {
    Yearlydata <- Yearlydata()
	  counties = subset(Yearlydata$County, Yearlydata$State == input$state_sel)
  }
  else
  {
    split_input = input$county_list
    selected_value = unlist(strsplit(split_input, ", "))
    counties = selected_value[1]
  }

  selectInput(inputId = "county_sel", 
     label = "Choose a county: ", 
     choices = unique(counties)
     )

})


#prints the days with AQI for the selected year
output$dayswithaqi <- renderText({
	selectedyeardata <- selectedyeardata()
	data <- subset(selectedyeardata, selectedyeardata$State==input$state_sel & selectedyeardata$County ==input$county_sel)
	total <- data$Days.with.AQI
	paste0("Total days with AQI (Air Quality Index) - ",total)
})

# pie chart showing percentage of days for the selected year
output$piechart <- renderPlot({
	selectedyeardata <- selectedyeardata()
	data <- subset(selectedyeardata, selectedyeardata$State==input$state_sel & selectedyeardata$County ==input$county_sel)
	pie = data.frame("Days" = c("Good","Moderate","Unhealthy(Sensitive)","Unhealthy","Very Unhealthy","Hazardous"),"Count" = c(data$Good.Days,data$Moderate.Days,data$Unhealthy.for.Sensitive.Groups.Days,data$Unhealthy.Days,data$Very.Unhealthy.Days,data$Hazardous.Days))

  pie$Days = paste0(pie$Days, " - ", paste0(round((pie$Count/sum(pie$Count))*100,digits = 2), "%"))
  pie$Days <- factor(pie$Days, levels = (as.character(pie$Days)))

  ggplot(pie, aes(x=1, y=Count, fill=Days)) + 
  geom_bar(width = 1, stat = "identity") + coord_polar("y", , start=0, direction = -1) + 
  labs(x = NULL, y = NULL, fill = NULL) + theme_classic() + 
  theme(legend.title=element_text(size=15), legend.text=element_text(size=18), axis.line = element_blank(),
       axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5, color = "#000000")) +
  scale_fill_manual(values = clrs)

})

# bar chart showing values(categorized by days) for the selected year
output$barchart <- renderPlot({
	selectedyeardata <- selectedyeardata()
	data <- selectedyeardata[ which(selectedyeardata$State==input$state_sel & selectedyeardata$County ==input$county_sel), ]
	pie = data.frame("Days" = c("Good","Moderate","Unhealthy(Sensitive)","Unhealthy","Very Unhealthy","Hazardous"),"Count" = c(data$Good.Days,data$Moderate.Days,data$Unhealthy.for.Sensitive.Groups.Days,data$Unhealthy.Days,data$Very.Unhealthy.Days,data$Hazardous.Days))
    
  pie$Days<- factor(pie$Days, levels = (as.character(pie$Days)))

  ggplot(data=pie, aes(x=Days, y=Count, fill=Days)) + geom_bar(stat="identity") +
  scale_fill_manual(values = clrs) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
       axis.text.y = element_text(size = 10),
       panel.background = element_rect(fill = "white", color = "white"), 
       panel.grid.major = element_line(size = 0.25, linetype = "solid", color = "lightgray"),
       plot.title = element_text(size = 20)) + ylab("No. of Days") + xlab("")
})

# Table showing values(categorized by days) for the selected year
output$table <- DT::renderDataTable(
    DT::datatable({ 
    selectedyeardata <- selectedyeardata()
	data <- selectedyeardata[ which(selectedyeardata$State==input$state_sel & selectedyeardata$County ==input$county_sel), ]
	pie = data.frame("AQI" = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"),"No. of Days" = c(data$Good.Days,data$Moderate.Days,data$Unhealthy.for.Sensitive.Groups.Days,data$Unhealthy.Days,data$Very.Unhealthy.Days,data$Hazardous.Days))	
    pie
  }, 
  options = list(searching = TRUE, pageLength = 20, lengthChange = FALSE, order = list(list(1, 'desc'))
  ), rownames = FALSE 
    ))

# pie chart showing percentage of pollutants for the selected year where that pollutant is the Main Pollutant

output$piechartpollutantCO <- renderPlot({

  selectedyeardata <- selectedyeardata()
  data <- selectedyeardata[ which(selectedyeardata$State==input$state_sel & selectedyeardata$County ==input$county_sel), ]
  pollutants = data.frame("Pollutant" = c("CO","Rest"),"Count" = c(round((data$Days.CO/data$Days.with.AQI)*100,digits=2), round(((data$Days.with.AQI - data$Days.CO)/data$Days.with.AQI)*100,digits=2)))
  
  pollutants$Pollutant = paste0(pollutants$Pollutant, " - ", paste0(pollutants$Count, "%"))
  
  ggplot(pollutants[which(pollutants$Count>0),], aes(x="", y=Count, fill=Pollutant)) + geom_bar(width = 1, stat = "identity") + coord_polar("y") +  
  labs(x = NULL, y = NULL, fill = NULL) + 
  scale_fill_brewer(palette="Reds") + theme_classic() + theme(legend.title=element_text(size=15),legend.text=element_text(size=18),axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),plot.title = element_text(hjust = 0.5, color = "#000000"))

})

output$piechartpollutantNO2 <- renderPlot({

  selectedyeardata <- selectedyeardata()
  data <- selectedyeardata[ which(selectedyeardata$State==input$state_sel & selectedyeardata$County ==input$county_sel), ]
  pollutants = data.frame("Pollutant" = c("NO2","Rest"),"Count" = c(round((data$Days.NO2/data$Days.with.AQI)*100,digits=2), round(((data$Days.with.AQI - data$Days.NO2)/data$Days.with.AQI)*100,digits=2)))
  
  pollutants$Pollutant = paste0(pollutants$Pollutant, " - ", paste0(pollutants$Count, "%"))

  ggplot(pollutants[which(pollutants$Count>0),], aes(x="", y=Count, fill=Pollutant)) + geom_bar(width = 1, stat = "identity") + coord_polar("y") + 
  labs(x = NULL, y = NULL, fill = NULL) + 
  scale_fill_brewer(palette="Reds") + theme_classic() + theme(legend.title=element_text(size=15),legend.text=element_text(size=18),axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),plot.title = element_text(hjust = 0.5, color = "#000000"))

})

output$piechartpollutantOzone <- renderPlot({

  selectedyeardata <- selectedyeardata()
  data <- selectedyeardata[ which(selectedyeardata$State==input$state_sel & selectedyeardata$County ==input$county_sel), ]
  pollutants = data.frame("Pollutant" = c("Ozone","Rest"),"Count" = c(round((data$Days.Ozone/data$Days.with.AQI)*100,digits=2), round(((data$Days.with.AQI - data$Days.Ozone)/data$Days.with.AQI)*100,digits=2)))
  
  pollutants$Pollutant = paste0(pollutants$Pollutant, " - ", paste0(pollutants$Count, "%"))

  ggplot(pollutants[which(pollutants$Count>0),], aes(x="", y=Count, fill=Pollutant)) + geom_bar(width = 1, stat = "identity") + coord_polar("y") + 
  labs(x = NULL, y = NULL, fill = NULL) + 
  scale_fill_brewer(palette="Reds") + theme_classic() + theme(legend.title=element_text(size=15),legend.text=element_text(size=18),axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),plot.title = element_text(hjust = 0.5, color = "#000000"))

})

output$piechartpollutantPM10 <- renderPlot({

  selectedyeardata <- selectedyeardata()
  data <- selectedyeardata[ which(selectedyeardata$State==input$state_sel & selectedyeardata$County ==input$county_sel), ]
  pollutants = data.frame("Pollutant" = c("PM10","Rest"),"Count" = c(round((data$Days.PM10/data$Days.with.AQI)*100,digits=2), round(((data$Days.with.AQI - data$Days.PM10)/data$Days.with.AQI)*100,digits=2)))
  
  pollutants$Pollutant = paste0(pollutants$Pollutant, " - ", paste0(pollutants$Count, "%"))

  ggplot(pollutants[which(pollutants$Count>0),], aes(x="", y=Count, fill=Pollutant)) + geom_bar(width = 1, stat = "identity") + coord_polar("y") + 
  labs(x = NULL, y = NULL, fill = NULL) + 
  scale_fill_brewer(palette="Reds") + theme_classic() + theme(legend.title=element_text(size=15),legend.text=element_text(size=18),axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),plot.title = element_text(hjust = 0.5, color = "#000000"))

})

output$piechartpollutantPM2.5 <- renderPlot({

  selectedyeardata <- selectedyeardata()
  data <- selectedyeardata[ which(selectedyeardata$State==input$state_sel & selectedyeardata$County ==input$county_sel), ]
  pollutants = data.frame("Pollutant" = c("PM2.5","Rest"),"Count" = c(round((data$Days.PM2.5/data$Days.with.AQI)*100,digits=2), round(((data$Days.with.AQI - data$Days.PM2.5)/data$Days.with.AQI)*100,digits=2)))
  
  pollutants$Pollutant = paste0(pollutants$Pollutant, " - ", paste0(pollutants$Count, "%"))

  ggplot(pollutants[which(pollutants$Count>0),], aes(x="", y=Count, fill=Pollutant)) + geom_bar(width = 1, stat = "identity") + coord_polar("y") + 
  labs(x = NULL, y = NULL, fill = NULL) + 
  scale_fill_brewer(palette="Reds") + theme_classic() + theme(legend.title=element_text(size=15),legend.text=element_text(size=18),axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),plot.title = element_text(hjust = 0.5, color = "#000000"))

})


output$piechartpollutantSO2 <- renderPlot({

  selectedyeardata <- selectedyeardata()
  data <- selectedyeardata[ which(selectedyeardata$State==input$state_sel & selectedyeardata$County ==input$county_sel), ]
  pollutants = data.frame("Pollutant" = c("SO2","Rest"),"Count" = c(round((data$Days.SO2/data$Days.with.AQI)*100,digits=2), round(((data$Days.with.AQI - data$Days.SO2)/data$Days.with.AQI)*100,digits=2)))
  
  pollutants$Pollutant = paste0(pollutants$Pollutant, " - ", paste0(pollutants$Count, "%"))

  ggplot(pollutants[which(pollutants$Count>0),], aes(x="", y=Count, fill=Pollutant)) + geom_bar(width = 1, stat = "identity") + coord_polar("y") + 
  labs(x = NULL, y = NULL, fill = NULL) + 
  scale_fill_brewer(palette="Reds") + theme_classic() + theme(legend.title=element_text(size=15),legend.text=element_text(size=18),axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),plot.title = element_text(hjust = 0.5, color = "#000000"))

})

# bar chart showing values(categorized by pollutants) for the selected year
output$barchartpollutant <- renderPlot({
	selectedyeardata <- selectedyeardata()
	data <- selectedyeardata[ which(selectedyeardata$State==input$state_sel & selectedyeardata$County ==input$county_sel), ]
	pollutants = data.frame("Pollutant" = c("CO","NO2","Ozone","SO2","PM2.5","PM10"),"Count" = c(data$Days.CO,data$Days.NO2,data$Days.Ozone,data$Days.SO2,data$Days.PM2.5,data$Days.PM10))
    ggplot(data=pollutants[which(pollutants$Count>0),], aes(x=Pollutant, y=Count, fill=Pollutant)) +geom_bar(stat="identity") +
    scale_fill_brewer(palette="Set1") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
       axis.text.y = element_text(size = 10),
       panel.background = element_rect(fill = "white", color = "white"), 
       panel.grid.major = element_line(size = 0.25, linetype = "solid", color = "lightgray"),
       plot.title = element_text(size = 20)) + ylab("No. of Days") + xlab("")
})

# Table showing values(categorized by pollutants) for the selected year
output$tablepollutant <- DT::renderDataTable(
    DT::datatable({ 
    selectedyeardata <- selectedyeardata()
	data <- selectedyeardata[ which(selectedyeardata$State==input$state_sel & selectedyeardata$County ==input$county_sel), ]
	pollutants = data.frame("Pollutant" = c("CO","NO2","Ozone","SO2","PM2.5","PM10"),"No. of Days" = c(data$Days.CO,data$Days.NO2,data$Days.Ozone,data$Days.SO2,data$Days.PM2.5,data$Days.PM10))	
    pollutants
  }, 
  options = list(searching = TRUE, pageLength = 20, lengthChange = FALSE, order = list(list(1, 'desc'))
  ), rownames = FALSE 
    ))

#line chart to show the change in Median AQI, 90th Percentile AQI, Max AQI over the years for the selected county
output$daysTime <- renderPlot({
    
	county1 <- Overtheyears()

   # Chaning Year for Axis
   county1$Year1 <- lubridate::ymd(county1$Year, truncated = 2L)
   
   # Axis Set for background
   xMn <- as.Date("1990-01-01", "%Y-%m-%d")
   xMx <- as.Date("2018-01-01",  "%Y-%m-%d")
   
   
   daysYear <- ggplot() +
     geom_rect(data=NULL,aes(xmin=xMn,xmax=xMx,ymin=0,ymax=50),fill=clrs[1]) +
     geom_rect(data=NULL,aes(xmin=xMn,xmax=xMx,ymin=50,ymax=100),fill=clrs[2]) +
     geom_rect(data=NULL,aes(xmin=xMn,xmax=xMx,ymin=100,ymax=150),fill=clrs[3]) +
     geom_rect(data=NULL,aes(xmin=xMn,xmax=xMx,ymin=150,ymax=200),fill=clrs[4]) +
     geom_rect(data=NULL,aes(xmin=xMn,xmax=xMx,ymin=200,ymax=300),fill=clrs[5]) +
     geom_rect(data=NULL,aes(xmin=xMn,xmax=xMx,ymin=300,ymax=Inf),fill=clrs[6]) +
     scale_x_date(date_breaks = "1 year", date_labels =  "%Y", expand = c(0, 0)) +
     scale_y_continuous(expand = c(0,0), breaks=c(seq(0,1000,50)))+
     labs(x = NULL, y = "Air Quality Index", title = NULL)+ 
     theme_light(18)+
     theme(legend.key = element_rect(fill = "grey30"), 
           legend.title = element_text(color = "white"), 
           legend.text=element_text(size=18))+
     scale_linetype_manual(values=c('Max'=1, '90th Percentile'=2, 'Median'=3)) +
     guides(linetype=guide_legend(keywidth = 3, keyheight = 2))
     
   
   if ("mx" %in% input$lines1){
     daysYear <- daysYear + geom_line(aes(x=county1$Year1, y=county1$Max.AQI, linetype = 'Max'), color='white', size=1.05)
   }
   if ("pr" %in% input$lines1){
     daysYear <- daysYear + geom_line(aes(x=county1$Year1, y=county1$X90th.Percentile.AQI, linetype = '90th Percentile'), color='white', size=1.05)
   }
   if ("md" %in% input$lines1){
     daysYear <- daysYear + geom_line(aes(x=county1$Year1, y=county1$Median.AQI, linetype = 'Median'), color='white', size=1.05)
   } 
   
   daysYear

})

#line chart to show the change in Pollutants value over the years for the selected county
output$pollutantTime <- renderPlot({

	county1 <- Overtheyears()      
   
   # Chaning Year for Axis
   county1$Year1 <- lubridate::ymd(county1$Year, truncated = 2L)
   
   pollutantYr <- ggplot() +
         labs(x = NULL, y = "Percentage", title = NULL)+ 
         theme_light(18)+
         theme(legend.title = element_text(color = "white"),  
               panel.grid = element_blank(),
               legend.text=element_text(size=18),
               legend.key = element_rect(fill = "grey30"),
               panel.background = element_rect(fill = 'grey30'))+
         scale_x_date(date_breaks = "1 year", date_labels =  "%Y", expand = c(0, 0))+
         ylim(-1,100)+
         guides(color=guide_legend(keywidth = 3, keyheight = 2))

 if ("co" %in% input$lines2){
   pollutantYr <- pollutantYr + geom_line(aes(x=county1$Year1, y=county1$Days.CO/county1$Days.with.AQI*100, colour = "CO"),size=1)
 }
 if ("so" %in% input$lines2){
   pollutantYr <- pollutantYr + geom_line(aes(x=county1$Year1, y=county1$Days.SO2/county1$Days.with.AQI*100, colour = "SO2"),size=1)
 }
 if ("no" %in% input$lines2){
   pollutantYr <- pollutantYr + geom_line(aes(x=county1$Year1, y=county1$Days.NO2/county1$Days.with.AQI*100, colour = "NO2"),size=1)
 }
 if ("oz" %in% input$lines2){
   pollutantYr <- pollutantYr + geom_line(aes(x=county1$Year1, y=county1$Days.Ozone/county1$Days.with.AQI*100, colour = "Ozone"),size=1) 
 }
 if ("pm2" %in% input$lines2){
   pollutantYr <- pollutantYr + geom_line(aes(x=county1$Year1, y=county1$Days.PM2.5/county1$Days.with.AQI*100, colour = "PM2.5"),size=1)    
 }
 if ("pm10" %in% input$lines2){
   pollutantYr <- pollutantYr + geom_line(aes(x=county1$Year1, y=county1$Days.PM10/county1$Days.with.AQI*100, colour = "PM10"),size=1)
 }
 pollutantYr

})

#Table to show the percentage change in Pollutants value over the years for the selected county
output$tablepercentage <- DT::renderDataTable(
    DT::datatable({ 
	linedata <- Overtheyears()
	linedata$CO <- round((linedata$Days.CO/linedata$Days.with.AQI)*100,digits=2)
	linedata$NO2 <- round((linedata$Days.NO2/linedata$Days.with.AQI)*100,digits=2)
	linedata$Ozone <- round((linedata$Days.Ozone/linedata$Days.with.AQI)*100,digits=2)
	linedata$SO2 <- round((linedata$Days.SO2/linedata$Days.with.AQI)*100,digits=2)
	linedata$PM2.5 <- round((linedata$Days.PM2.5/linedata$Days.with.AQI)*100,digits=2)
	linedata$PM10 <- round((linedata$Days.PM10/linedata$Days.with.AQI)*100,digits=2)

  final <- subset(linedata, select = c(Year,Days.with.AQI,CO,NO2,Ozone,SO2,PM2.5,PM10))
  }, 
  options = list(searching = TRUE, pageLength = 20, lengthChange = FALSE, order = list(list(0, 'desc'))
  ), rownames = FALSE 
    ))

# Leaflet to plot the selected state,county

output$leaf <- renderLeaflet({

	Selected_Data <- subset(final_map, final_map$State.Name == input$state_sel & final_map$County.Name == input$county_sel)
  map <- leaflet()
  map <- addTiles(map)
  map <- setView(map, lng = Selected_Data$Longitude[1], lat = Selected_Data$Latitude[1], zoom = 6)
  map <- addMarkers(map, lng = Selected_Data$Longitude[1], lat = Selected_Data$Latitude[1], popup = paste0(input$county_sel,", ", input$state_sel))
  map
})

#Heatmap for the pollutants across all state/counties in the US. The User can choose any pollutant to see its distribution intensity across the US
output$pollutantMap <- renderLeaflet({

	  data_air = Yearlydata()
    data_air$County = trimws(data_air$County)
    data_air$State = trimws(data_air$State)
    data_air$County = sub(" City", "", data_air$County)
    data_air$County = sub("St. ", "Saint ", data_air$County)
    data_air$State = sub(" Of ", " of ", data_air$State)
    
    
    if ("co" == input$pollutant){
      data_air$"Percentage" = data_air$Days.CO/data_air$Days.with.AQI * 100
    }
    if ("so" == input$pollutant){
      data_air$"Percentage" = data_air$Days.SO2/data_air$Days.with.AQI * 100
    }
    if ("no" == input$pollutant){
      data_air$"Percentage" = data_air$Days.NO2/data_air$Days.with.AQI * 100
    }
    if ("oz" == input$pollutant){
      data_air$"Percentage" = data_air$Days.Ozone/data_air$Days.with.AQI * 100
    }
    if ("pm2" == input$pollutant){
      data_air$"Percentage" = data_air$Days.PM2.5/data_air$Days.with.AQI * 100
    }
    if ("pm10" == input$pollutant){
      data_air$"Percentage" = data_air$Days.PM10/data_air$Days.with.AQI * 100
    }
    
    sliderinput = input$slideryearly
    
    data_air_sorted <- data_air[order(-data_air$Percentage),] 
    data_air_top <- head(data_air_sorted,sliderinput)
    
    data <- left_join(data_map, data_air_top, by = c("State", "County"))
    
    
    tmap_mode("view")
    map <- tm_shape(st_as_sf(data)) + 
      tm_polygons("Percentage",
                  style = input$breakYearly, 
                  palette = input$paletteYearly, 
                  border.col = "white", 
                  border.alpha = 0.3,
                  popup.vars = c("County: " = "County", "State: " = "State", "Percentage:"= "Percentage")) + 
      tm_layout(inner.margins = c(0.10, 0.10, 0.10, 0.20)) +
      tm_shape(aggregate_map(st_as_sf(data_map), by = "State")) +
      tm_borders(col = "black", alpha = 0.4) + 
      tm_view(basemaps = "OpenStreetMap",view.legend.position = c("left","top"),set.view = c(-97, 37, 4))
   
    
    tmap_leaflet(map)
    
  })

#Slider to limit the number of county values in the Pollutant Heatmap
output$createslider = renderUI({
  Yearlydata <- Yearlydata()
  slider = nrow(Yearlydata)
  sliderInput('slideryearly','No. of Counties(initially top 100)',min=1,max=slider,value=100,sep="")
})

#Heatmap for the AQI data across all state/counties in the US. The User can choose Median/Max/90th percentile value to see its distribution intensity across the US
output$aqiMap <- renderLeaflet({
    
    data_air = Yearlydata()
    data_air$County = trimws(data_air$County)
    data_air$State = trimws(data_air$State)
    data_air$County = sub(" City", "", data_air$County)
    data_air$County = sub("St. ", "Saint ", data_air$County)
    data_air$State = sub(" Of ", " of ", data_air$State)
    
    
    if ("mx" == input$Stat){
      data_air$"AQI" = data_air$Max.AQI
    }
    if ("pr" == input$Stat){
      data_air$"AQI" = data_air$X90th.Percentile.AQI
    }
    if ("md" == input$Stat){
      data_air$"AQI" = data_air$Median.AQI
    }

    
    data <- left_join(data_map, data_air, by = c("State", "County"))
    
    
    tmap_mode("view")
    map <- tm_shape(st_as_sf(data)) + 
      tm_polygons("AQI",
                  style = input$breakYearlyAQI, 
                  palette = input$paletteYearlyAQI, 
                  border.col = "white", 
                  border.alpha = 0.3,
                  popup.vars = c("County: " = "County", "State: " = "State", "AQI:"= "AQI")) + 
      tm_layout(inner.margins = c(0.10, 0.10, 0.10, 0.20)) +
      tm_shape(aggregate_map(st_as_sf(data_map), by = "State")) +
      tm_borders(col = "black", alpha = 0.4) + 
      tm_view(basemaps = "OpenStreetMap",view.legend.position = c("left","top"),set.view = c(-97, 37, 4))
    
    
    tmap_leaflet(map)
    
  })






#Stacked Bar chart for the Daily data categorized by Good days, Moderate days, Unhealthy days, Unhealthy for Sensitive days, Very Unhealthy days, Hazardous days
output$StackedBarChart <- renderPlot({
    
    aqiData <- Dailydata()
    aqiStateData <- subset(aqiData, State.Name==input$state_sel)
    aqiCountyData <- subset(aqiStateData, county.Name==input$county_sel)
    
    validate(
      need(nrow(aqiCountyData) != 0, "ERROR: CHOSEN COUNTY HAS NO DATA.")
    )
    aqiCountyData$Month <- month(aqiCountyData$Date)
    
    aqiCountyData$Count <- 1
    c<-aggregate(Count~Month+Category, data=aqiCountyData, sum)
    
    for(row in 1:nrow(c)){
      monthName = c$Month[row]
      
      data <- aggregate(Count~Month, data=c, sum)
      
      c$Percentage[row] = round(c$Count[row]/filter(data, Month==monthName)$Count[1], 4)*100
    }
    
    c <- c %>% 
      mutate(month = factor(
        month(Month, label = FALSE),   # thing you're converting
        1:12,                                  # values it could take
        labels =                               # how they should appear
          c("Jan", "Feb", "Mar", "Apr",
            "May", "Jun", "Jul", "Aug",
            "Sep", "Oct", "Nov", "Dec"),
        ordered = TRUE))        
    c$Category <- factor(c$Category, levels = c("Good","Moderate","Unhealthy for Sensitive Groups","Unhealthy","Very Unhealthy","Hazardous") )
    ggplot(c, aes(fill=Category, y=Percentage, x=month)) + 
    geom_bar( stat="identity", position = position_fill(reverse = TRUE)) + 
    ggtitle((paste0(input$county_sel,", ",input$state_sel)))+
    scale_fill_manual(values=clrs)

})

#Line chart for the pollutants showing days where that pollutant is the main pollutant
output$linepollutant <- renderPlotly({

    aqiData <- Dailydata()
    aqiStateData <- subset(aqiData, State.Name==input$state_sel)
    aqiCountyData <- subset(aqiStateData, county.Name==input$county_sel)
    aqiCountyData$Month <- month(aqiCountyData$Date)
    aqiCountyData$Date <- as.Date(aqiCountyData$Date)
    pdf(NULL)

    x <- list(
      title = "Month",
      range=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    )
    plot <- plot_ly(aqiCountyData, x=~Date, y=~AQI)%>%add_trace(aqiCountyData, y=~AQI, type="scatter", mode="line",showlegend=FALSE) %>% add_markers(color = ~Defining.Parameter) %>% layout(xaxis=x)
    # plot <- ggplot(data=aqiCountyData, aes(x=Date, y=AQI, group=1)) + 
    # geom_line(color="black") +
    # geom_point(aes(text=paste0(aqiCountyData$Defining.Parameter)), colour="red", alpha=1/2) 
    dev.off()
    plot
})

#Table showing the category( Good, Moderate etc) of days for each Month with the count
output$DailyTable <- DT::renderDataTable(DT::datatable({

    
    aqiData <- Dailydata()
    aqiStateData <- subset(aqiData, State.Name==input$state_sel)
    aqiCountyData <- subset(aqiStateData, county.Name==input$county_sel)

    validate(
      need(nrow(aqiCountyData) != 0, "ERROR: CHOSEN COUNTY HAS NO DATA.")
    )
    aqiCountyData$Month <- months(as.Date(aqiCountyData$Date))
    aqiCountyData$Count <- 1
    c<-aggregate(Count~Month+Category, data=aqiCountyData, sum)

    newDataFrame = c[order(c$Category, c$Month),]

    
    categories <- unique(aqiCountyData$Category)

    newDataFrame
    }, 
    options = list(searching = TRUE, pageLength = 20, lengthChange = FALSE, order = list(list(1, 'asc'))
    ), rownames = FALSE )
  )
#Heatmap for the pollutants across all state/counties in the US for the selected date. The User can choose any pollutant to see its distribution intensity across the US
output$pollutantMapDaily <- renderLeaflet({
    
    read_air_daily = DailyMapDate()
    data_air_selected = subset(read_air_daily, read_air_daily$Date.Local == as.character(input$dailydate) & read_air_daily$Pollutant == input$pollutantYearly)
    
    data_air = data.frame(
      State = data_air_selected$State.Name,
      County = data_air_selected$County.Name,
      PPM = data_air_selected$V1
    )
    data_air$County = trimws(data_air$County)
    data_air$State = trimws(data_air$State)
    data_air$County = sub(" City", "", data_air$County)
    data_air$County = sub("St. ", "Saint ", data_air$County)
    data_air$State = sub(" Of ", " of ", data_air$State)
    
    
    sliderinput = 100
    
    data_air_sorted <- data_air[order(-data_air$PPM),] 
    data_air_top <- head(data_air_sorted,sliderinput)
    
    data <- left_join(data_map, data_air_top, by = c("State", "County"))
    
    
    tmap_mode("view")
    map <- tm_shape(st_as_sf(data)) + 
      tm_polygons("PPM",
                  style = input$breakdaily, 
                  palette = input$palettedaily, 
                  border.col = "white", 
                  border.alpha = 0.3,
                  popup.vars = c("County: " = "County", "State: " = "State", "PPM:"= "PPM")) + 
      tm_layout(inner.margins = c(0.10, 0.10, 0.10, 0.20)) +
      tm_shape(aggregate_map(st_as_sf(data_map), by = "State")) +
      tm_borders(col = "black", alpha = 0.4) + 
      tm_view(basemaps = "OpenStreetMap",view.legend.position = c("left","top"),set.view = c(-97, 37, 4))
    
    
    tmap_leaflet(map)
    
})

#datepicker to choose date - used for the Daily data
output$choosedailydate = renderUI({ 

  airDatepickerInput(
      inputId = "dailydate",
      label = "Choose date:",
      multiple = FALSE,
      value='2018-01-02',minDate = '2018-01-02', maxDate = '2019-01-01',
      autoClose = TRUE, update_on = c("change", "close")
    )

})

#radiobuttons to choose pollutant - used for the Daily data
output$choosedailypollutant = renderUI({

DailyMapDate <- DailyMapDate()
Pollutants = subset(DailyMapDate$Pollutant, DailyMapDate$Date.Local == as.character(input$dailydate) )
validate(need(Pollutants, 'No Pollutant available'))
radioButtons("pollutantYearly", "Select Pollutant", choices = unique(Pollutants), inline=TRUE)

});

#datepicker to choose date - used for the Hourly data
output$getDate = renderUI({	

	airDatepickerInput(
    	inputId = "date",
    	label = "Choose date:",
    	multiple = FALSE,
    	value='2018-01-02',minDate = '2018-01-02', maxDate = '2019-01-01',
    	autoClose = TRUE, update_on = c("change", "close")
  	)

})

#Checkbox to choose one or more pollutant - used for the Hourly data
output$getPollutants = renderUI({

	Hourlydate <- Hourlydate()
  Pollutants = subset(Hourlydate$Gas , Hourlydate$State.Name == input$state_sel & Hourlydate$County.Name == input$county_sel & Hourlydate$Date.Local == as.character(input$date) )
  
  validate(need(Pollutants, 'No Pollutant available'))

  checkboxGroupInput("pollutantscheckbox", label = "Choose Pollutant", choices = unique(Pollutants),selected=Pollutants[1],inline=TRUE)

}) 

# Radiobuttons to change the Units from Imperial to Metric or vice versa
output$getUnits = renderUI({

  validate(need(input$pollutantscheckbox, 'No Pollutant available'))

  radioButtons("units", label = "Choose Units",
    choices = list("Imperial" = "imperial", "Metric" = "metric"), 
    selected = "imperial",inline=TRUE)

})

#Line chart to plot the hourly data based on the date and pollutants selected

output$linehourly <- renderPlot({

  MonthlyCO <- MonthlyCO()
  MonthlyNO2 <- MonthlyNO2()
  MonthlyOzone <- MonthlyOzone()
  MonthlyPM2_5 <- MonthlyPM2_5()
  MonthlyPM10 <- MonthlyPM10()
  MonthlySO2 <- MonthlySO2()
  MonthlyTemp <- MonthlyTemp()
  MonthlyWind <- MonthlyWind()  

  validate(need(input$pollutantscheckbox, 'No data available'))

  finalplot = ggplot()

  if("imperial" %in% input$units)
  {

      if ("CO" %in% input$pollutantscheckbox)
      {
        MonthlyCOSubset = subset( MonthlyCO , MonthlyCO$State.Name == input$state_sel & MonthlyCO$County.Name == input$county_sel & MonthlyCO$Date.Local == as.character(input$date) )
        MonthlyCOSubset = aggregate(MonthlyCOSubset, list(MonthlyCOSubset$Time.Local), mean)
        finalplot = finalplot + geom_line(aes(x=MonthlyCOSubset$Group.1,y = MonthlyCOSubset$PPM,group=1,color="CO"),size=1.5) + geom_point(aes(x=MonthlyCOSubset$Group.1,y = MonthlyCOSubset$PPM),size=2) 
        
      }

      if ("NO2" %in% input$pollutantscheckbox)
      {
        MonthlyNO2Subset = subset( MonthlyNO2 , MonthlyNO2$State.Name == input$state_sel & MonthlyNO2$County.Name == input$county_sel & MonthlyNO2$Date.Local == as.character(input$date) )
        MonthlyNO2Subset = aggregate(MonthlyNO2Subset, list(MonthlyNO2Subset$Time.Local), mean)
        finalplot = finalplot + geom_line(aes(x=MonthlyNO2Subset$Group.1,y = MonthlyNO2Subset$PPM,group=1,color="NO2"),size=1.5) + geom_point(aes(x=MonthlyNO2Subset$Group.1,y = MonthlyNO2Subset$PPM),size=2)

      }
      
      if ("Ozone" %in% input$pollutantscheckbox)
      {
        MonthlyOzoneSubset = subset( MonthlyOzone , MonthlyOzone$State.Name == input$state_sel & MonthlyOzone$County.Name == input$county_sel & MonthlyOzone$Date.Local == as.character(input$date) )
        MonthlyOzoneSubset = aggregate(MonthlyOzoneSubset, list(MonthlyOzoneSubset$Time.Local), mean)
        finalplot = finalplot + geom_line(aes(x=MonthlyOzoneSubset$Group.1,y = MonthlyOzoneSubset$PPM,group=1,color="Ozone"),size=1.5) + geom_point(aes(x=MonthlyOzoneSubset$Group.1,y = MonthlyOzoneSubset$PPM),size=2)
      }

      if ("PM2_5" %in% input$pollutantscheckbox)
      {
        MonthlyPM2_5Subset = subset( MonthlyPM2_5 , MonthlyPM2_5$State.Name == input$state_sel & MonthlyPM2_5$County.Name == input$county_sel & MonthlyPM2_5$Date.Local == as.character(input$date) )
        MonthlyPM2_5Subset = aggregate(MonthlyPM2_5Subset, list(MonthlyPM2_5Subset$Time.Local), mean)
        finalplot = finalplot + geom_line(aes(x=MonthlyPM2_5Subset$Group.1,y = MonthlyPM2_5Subset$PPM,group=1,color="PM2_5"),size=1.5) + geom_point(aes(x=MonthlyPM2_5Subset$Group.1,y = MonthlyPM2_5Subset$PPM),size=2)
      }

      if ("PM10" %in% input$pollutantscheckbox)
      {
        MonthlyPM10Subset = subset( MonthlyPM10 , MonthlyPM10$State.Name == input$state_sel & MonthlyPM10$County.Name == input$county_sel & MonthlyPM10$Date.Local == as.character(input$date) )
        MonthlyPM10Subset = aggregate(MonthlyPM10Subset, list(MonthlyPM10Subset$Time.Local), mean)
        finalplot = finalplot + geom_line(aes(x=MonthlyPM10Subset$Group.1,y = MonthlyPM10Subset$PPM,group=1,color="PM10"),size=1.5) + geom_point(aes(x=MonthlyPM10Subset$Group.1,y = MonthlyPM10Subset$PPM),size=2)
      } 

      if ("SO2" %in% input$pollutantscheckbox)
      {
        MonthlySO2Subset = subset( MonthlySO2 , MonthlySO2$State.Name == input$state_sel & MonthlySO2$County.Name == input$county_sel & MonthlySO2$Date.Local == as.character(input$date) )
        MonthlySO2Subset = aggregate(MonthlySO2Subset, list(MonthlySO2Subset$Time.Local), mean)
        finalplot = finalplot + geom_line(aes(x=MonthlySO2Subset$Group.1,y = MonthlySO2Subset$PPM,group=1,color="SO2"),size=1.5) + geom_point(aes(x=MonthlySO2Subset$Group.1,y = MonthlySO2Subset$PPM),size=2)
      }  

      if ("Temp" %in% input$pollutantscheckbox)
      {
        MonthlyTempSubset = subset( MonthlyTemp , MonthlyTemp$State.Name == input$state_sel & MonthlyTemp$County.Name == input$county_sel & MonthlyTemp$Date.Local == as.character(input$date) )
        MonthlyTempSubset = aggregate(MonthlyTempSubset, list(MonthlyTempSubset$Time.Local), mean)
        finalplot = finalplot + geom_line(aes(x=MonthlyTempSubset$Group.1,y = MonthlyTempSubset$F,group=1,color="Temp(F)"),size=1.5) + geom_point(aes(x=MonthlyTempSubset$Group.1,y = MonthlyTempSubset$F),size=2)
      }

      if ("Wind" %in% input$pollutantscheckbox)
      {
        MonthlyWindSubset = subset( MonthlyWind , MonthlyWind$State.Name == input$state_sel & MonthlyWind$County.Name == input$county_sel & MonthlyWind$Date.Local == as.character(input$date) )
        MonthlyWindSubset = aggregate(MonthlyWindSubset, list(MonthlyWindSubset$Time.Local), mean)
        finalplot = finalplot + geom_line(aes(x=MonthlyWindSubset$Group.1,y = MonthlyWindSubset$MPH,group=1,color="Wind(MPH)"),size=1.5) + geom_point(aes(x=MonthlyWindSubset$Group.1,y = MonthlyWindSubset$MPH),size=2)
      }

  }

  if("metric" %in% input$units)
  {

      if ("CO" %in% input$pollutantscheckbox)
      {
        MonthlyCOSubset = subset( MonthlyCO , MonthlyCO$State.Name == input$state_sel & MonthlyCO$County.Name == input$county_sel & MonthlyCO$Date.Local == as.character(input$date) )
        MonthlyCOSubset = aggregate(MonthlyCOSubset, list(MonthlyCOSubset$Time.Local), mean)
        finalplot = finalplot + geom_line(aes(x=MonthlyCOSubset$Group.1,y = MonthlyCOSubset$m_m3,group=1,color="CO"),size=1.5) + geom_point(aes(x=MonthlyCOSubset$Group.1,y = MonthlyCOSubset$m_m3),size=2) 
        
      }

      if ("NO2" %in% input$pollutantscheckbox)
      {
        MonthlyNO2Subset = subset( MonthlyNO2 , MonthlyNO2$State.Name == input$state_sel & MonthlyNO2$County.Name == input$county_sel & MonthlyNO2$Date.Local == as.character(input$date) )
        MonthlyNO2Subset = aggregate(MonthlyNO2Subset, list(MonthlyNO2Subset$Time.Local), mean)
        finalplot = finalplot + geom_line(aes(x=MonthlyNO2Subset$Group.1,y = MonthlyNO2Subset$m_m3,group=1,color="NO2"),size=1.5) + geom_point(aes(x=MonthlyNO2Subset$Group.1,y = MonthlyNO2Subset$m_m3),size=2)
      }
      
      if ("Ozone" %in% input$pollutantscheckbox)
      {
        MonthlyOzoneSubset = subset( MonthlyOzone , MonthlyOzone$State.Name == input$state_sel & MonthlyOzone$County.Name == input$county_sel & MonthlyOzone$Date.Local == as.character(input$date) )
        MonthlyOzoneSubset = aggregate(MonthlyOzoneSubset, list(MonthlyOzoneSubset$Time.Local), mean)
        finalplot = finalplot + geom_line(aes(x=MonthlyOzoneSubset$Group.1,y = MonthlyOzoneSubset$m_m3,group=1,color="Ozone"),size=1.5) + geom_point(aes(x=MonthlyOzoneSubset$Group.1,y = MonthlyOzoneSubset$m_m3),size=2)
      }

      if ("PM2_5" %in% input$pollutantscheckbox)
      {
        MonthlyPM2_5Subset = subset( MonthlyPM2_5 , MonthlyPM2_5$State.Name == input$state_sel & MonthlyPM2_5$County.Name == input$county_sel & MonthlyPM2_5$Date.Local == as.character(input$date) )
        MonthlyPM2_5Subset = aggregate(MonthlyPM2_5Subset, list(MonthlyPM2_5Subset$Time.Local), mean)
        finalplot = finalplot + geom_line(aes(x=MonthlyPM2_5Subset$Group.1,y = MonthlyPM2_5Subset$m_m3,group=1,color="PM2_5"),size=1.5) + geom_point(aes(x=MonthlyPM2_5Subset$Group.1,y = MonthlyPM2_5Subset$m_m3),size=2)
      }

      if ("PM10" %in% input$pollutantscheckbox)
      {
        MonthlyPM10Subset = subset( MonthlyPM10 , MonthlyPM10$State.Name == input$state_sel & MonthlyPM10$County.Name == input$county_sel & MonthlyPM10$Date.Local == as.character(input$date) )
        MonthlyPM10Subset = aggregate(MonthlyPM10Subset, list(MonthlyPM10Subset$Time.Local), mean)
        finalplot = finalplot + geom_line(aes(x=MonthlyPM10Subset$Group.1,y = MonthlyPM10Subset$m_m3,group=1,color="PM10"),size=1.5) + geom_point(aes(x=MonthlyPM10Subset$Group.1,y = MonthlyPM10Subset$m_m3),size=2)
      }

      if ("SO2" %in% input$pollutantscheckbox)
      {
        MonthlySO2Subset = subset( MonthlySO2 , MonthlySO2$State.Name == input$state_sel & MonthlySO2$County.Name == input$county_sel & MonthlySO2$Date.Local == as.character(input$date) )
        MonthlySO2Subset = aggregate(MonthlySO2Subset, list(MonthlySO2Subset$Time.Local), mean)
        finalplot = finalplot + geom_line(aes(x=MonthlySO2Subset$Group.1,y = MonthlySO2Subset$m_m3,group=1,color="SO2"),size=1.5) + geom_point(aes(x=MonthlySO2Subset$Group.1,y = MonthlySO2Subset$m_m3),size=2)
      } 

      if ("Temp" %in% input$pollutantscheckbox)
      {
        MonthlyTempSubset = subset( MonthlyTemp , MonthlyTemp$State.Name == input$state_sel & MonthlyTemp$County.Name == input$county_sel & MonthlyTemp$Date.Local == as.character(input$date) )
        MonthlyTempSubset = aggregate(MonthlyTempSubset, list(MonthlyTempSubset$Time.Local), mean)
        finalplot = finalplot + geom_line(aes(x=MonthlyTempSubset$Group.1,y = MonthlyTempSubset$C,group=1,color="Temp(C)"),size=1.5) + geom_point(aes(x=MonthlyTempSubset$Group.1,y = MonthlyTempSubset$C),size=2)
      }

      if ("Wind" %in% input$pollutantscheckbox)
      {
        MonthlyWindSubset = subset( MonthlyWind , MonthlyWind$State.Name == input$state_sel & MonthlyWind$County.Name == input$county_sel & MonthlyWind$Date.Local == as.character(input$date) )
        MonthlyWindSubset = aggregate(MonthlyWindSubset, list(MonthlyWindSubset$Time.Local), mean)
        finalplot = finalplot + geom_line(aes(x=MonthlyWindSubset$Group.1,y = MonthlyWindSubset$KPH,group=1,color="Wind(KPH)"),size=1.5) + geom_point(aes(x=MonthlyWindSubset$Group.1,y = MonthlyWindSubset$KPH),size=2)
      }

  }

  finalplot = finalplot + theme(legend.title = element_blank()) + theme(legend.text=element_text(size=18)) + xlab(label="Time") + ylab(label="Measured Values") + guides(linetype=guide_legend(keywidth = 3, keyheight = 2), color=guide_legend(keywidth = 3, keyheight = 2))
  finalplot

})

#get all the Sites in Hong Kong

output$location_hk = renderUI({

locationHK = leafletdataHK() 

sites = locationHK$location
  
  selectInput(inputId = "site_sel", 
   label = "Choose site: ", 
   choices = unique(sites))

})

#datepicker to choose date - used for the Hourly data for Hong Kong

output$getdate_hk = renderUI({ 

  airDatepickerInput(
      inputId = "date_hk",
      label = "Choose date:",
      multiple = FALSE,
      value='2018-12-17',minDate = '2018-12-17', maxDate = '2019-03-17',
      autoClose = TRUE, update_on = c("change", "close")
    )

})

#Checkbox to get all the pollutants for site selected in Hong Kong

output$getPollutants_hk = renderUI({

    HourlyHK_CO <- HourlyHK_CO()
    HourlyHK_CO$Pollutant = "CO"
    
    HourlyHK_NO2 <- HourlyHK_NO2()
    HourlyHK_NO2$Pollutant = "NO2"

    HourlyHK_O3 <- HourlyHK_O3()
    HourlyHK_O3$Pollutant = "O3"
    
    HourlyHK_PM10 <- HourlyHK_PM10()
    HourlyHK_PM10$Pollutant = "PM10"
    
    HourlyHK_PM2 <- HourlyHK_PM2()
    HourlyHK_PM2$Pollutant = "PM2"
    
    HourlyHK_SO2 <- HourlyHK_SO2()
    HourlyHK_SO2$Pollutant = "SO2"

    total <- rbind(HourlyHK_CO,HourlyHK_NO2)
    total <- rbind(total,HourlyHK_O3)
    total <- rbind(total,HourlyHK_PM10)
    total <- rbind(total,HourlyHK_PM2)
    total <- rbind(total,HourlyHK_SO2)

    Pollutants = subset(total$Pollutant , total$location == input$site_sel & total$Date == as.character(input$date_hk))
    
    validate(need(Pollutants, 'No Pollutant available'))

    checkboxGroupInput("pollutantscheckbox_hk", label = "Choose Pollutant", choices = unique(Pollutants),inline=TRUE,selected=Pollutants[1])

}) 

# Radiobutton to change the Units from Inperial to Metric

output$getUnits_hk = renderUI({

  validate(need(input$pollutantscheckbox_hk, 'No Pollutant available'))

  radioButtons("units_hk", label = "Choose Units",
    choices = list("Imperial" = "imperial", "Metric" = "metric"),
    inline = TRUE, 
    selected = "imperial")

})

# Line chart for Hourly/Daily/Monthly data based on the date and Pollutants selected

output$linechart_hk = renderPlot({

  validate(need(input$pollutantscheckbox_hk, 'No data available'))

  finalplot = ggplot()

  if(input$category_data == "hourly" )
  {
      HourlyHK_CO <- HourlyHK_CO()
      HourlyHK_NO2 <- HourlyHK_NO2()
      HourlyHK_O3 <- HourlyHK_O3()
      HourlyHK_PM10 <- HourlyHK_PM10()
      HourlyHK_PM2 <- HourlyHK_PM2()
      HourlyHK_SO2 <- HourlyHK_SO2()

      if(input$units_hk == "imperial")
      {

        if ("CO" %in% input$pollutantscheckbox_hk)
        {
          HourlyHK_COSubset = subset( HourlyHK_CO , HourlyHK_CO$location == input$site_sel & HourlyHK_CO$Date == as.character(input$date_hk) )
          finalplot = finalplot + geom_line(aes(x=HourlyHK_COSubset$Time,y = HourlyHK_COSubset$PPB,group=1,color="CO(PPB)"),size=1.5) + geom_point(aes(x=HourlyHK_COSubset$Time,y = HourlyHK_COSubset$PPB),size=2) 
        }

        if ("NO2" %in% input$pollutantscheckbox_hk)
        {
          HourlyHK_NO2Subset = subset( HourlyHK_NO2 , HourlyHK_NO2$location == input$site_sel & HourlyHK_NO2$Date == as.character(input$date_hk) )
          finalplot = finalplot + geom_line(aes(x=HourlyHK_NO2Subset$Time,y = HourlyHK_NO2Subset$PPB,group=1,color="NO2(PPB)"),size=1.5) + geom_point(aes(x=HourlyHK_NO2Subset$Time,y = HourlyHK_NO2Subset$PPB),size=2) 
        }

        if ("O3" %in% input$pollutantscheckbox_hk)
        {
          HourlyHK_O3Subset = subset( HourlyHK_O3 , HourlyHK_O3$location == input$site_sel & HourlyHK_O3$Date == as.character(input$date_hk) )
          finalplot = finalplot + geom_line(aes(x=HourlyHK_O3Subset$Time,y = HourlyHK_O3Subset$PPB,group=1,color="O3(PPB)"),size=1.5) + geom_point(aes(x=HourlyHK_O3Subset$Time,y = HourlyHK_O3Subset$PPB),size=2) 
        }

        if ("PM10" %in% input$pollutantscheckbox_hk)
        {
          HourlyHK_PM10Subset = subset( HourlyHK_PM10 , HourlyHK_PM10$location == input$site_sel & HourlyHK_PM10$Date == as.character(input$date_hk) )
          finalplot = finalplot + geom_line(aes(x=HourlyHK_PM10Subset$Time,y = HourlyHK_PM10Subset$PPB,group=1,color="PM10(PPB)"),size=1.5) + geom_point(aes(x=HourlyHK_PM10Subset$Time,y = HourlyHK_PM10Subset$PPB),size=2) 
        }

        if ("PM2" %in% input$pollutantscheckbox_hk)
        {
          HourlyHK_PM2Subset = subset( HourlyHK_PM2 , HourlyHK_PM2$location == input$site_sel & HourlyHK_PM2$Date == as.character(input$date_hk) )
          finalplot = finalplot + geom_line(aes(x=HourlyHK_PM2Subset$Time,y = HourlyHK_PM2Subset$PPB,group=1,color="PM2(PPB)"),size=1.5) + geom_point(aes(x=HourlyHK_PM2Subset$Time,y = HourlyHK_PM2Subset$PPB),size=2) 
        }

        if ("SO2" %in% input$pollutantscheckbox_hk)
        {
          HourlyHK_SO2Subset = subset( HourlyHK_SO2 , HourlyHK_SO2$location == input$site_sel & HourlyHK_SO2$Date == as.character(input$date_hk) )
          finalplot = finalplot + geom_line(aes(x=HourlyHK_SO2Subset$Time,y = HourlyHK_SO2Subset$PPB,group=1,color="SO2(PPB)"),size=1.5) + geom_point(aes(x=HourlyHK_SO2Subset$Time,y = HourlyHK_SO2Subset$PPB),size=2) 
        }

      }

      if(input$units_hk == "metric")
      {
        if ("CO" %in% input$pollutantscheckbox_hk)
        {
          HourlyHK_COSubset = subset( HourlyHK_CO , HourlyHK_CO$location == input$site_sel & HourlyHK_CO$Date == as.character(input$date_hk) )
          finalplot = finalplot + geom_line(aes(x=HourlyHK_COSubset$Time,y = HourlyHK_COSubset$ug_m3,group=1,color="CO(ug/m3)"),size=1.5) + geom_point(aes(x=HourlyHK_COSubset$Time,y = HourlyHK_COSubset$ug_m3),size=2) 
        }

        if ("NO2" %in% input$pollutantscheckbox_hk)
        {
          HourlyHK_NO2Subset = subset( HourlyHK_NO2 , HourlyHK_NO2$location == input$site_sel & HourlyHK_NO2$Date == as.character(input$date_hk) )
          finalplot = finalplot + geom_line(aes(x=HourlyHK_NO2Subset$Time,y = HourlyHK_NO2Subset$ug_m3,group=1,color="NO2(ug/m3)"),size=1.5) + geom_point(aes(x=HourlyHK_NO2Subset$Time,y = HourlyHK_NO2Subset$ug_m3),size=2) 
        }

        if ("O3" %in% input$pollutantscheckbox_hk)
        {
          HourlyHK_O3Subset = subset( HourlyHK_O3 , HourlyHK_O3$location == input$site_sel & HourlyHK_O3$Date == as.character(input$date_hk) )
          finalplot = finalplot + geom_line(aes(x=HourlyHK_O3Subset$Time,y = HourlyHK_O3Subset$ug_m3,group=1,color="O3(ug/m3)"),size=1.5) + geom_point(aes(x=HourlyHK_O3Subset$Time,y = HourlyHK_O3Subset$ug_m3),size=2) 
        }

        if ("PM10" %in% input$pollutantscheckbox_hk)
        {
          HourlyHK_PM10Subset = subset( HourlyHK_PM10 , HourlyHK_PM10$location == input$site_sel & HourlyHK_PM10$Date == as.character(input$date_hk) )
          finalplot = finalplot + geom_line(aes(x=HourlyHK_PM10Subset$Time,y = HourlyHK_PM10Subset$ug_m3,group=1,color="PM10(ug/m3)"),size=1.5) + geom_point(aes(x=HourlyHK_PM10Subset$Time,y = HourlyHK_PM10Subset$ug_m3),size=2) 
        }

        if ("PM2" %in% input$pollutantscheckbox_hk)
        {
          HourlyHK_PM2Subset = subset( HourlyHK_PM2 , HourlyHK_PM2$location == input$site_sel & HourlyHK_PM2$Date == as.character(input$date_hk) )
          finalplot = finalplot + geom_line(aes(x=HourlyHK_PM2Subset$Time,y = HourlyHK_PM2Subset$ug_m3,group=1,color="PM2(ug/m3)"),size=1.5) + geom_point(aes(x=HourlyHK_PM2Subset$Time,y = HourlyHK_PM2Subset$ug_m3),size=2) 
        }

        if ("SO2" %in% input$pollutantscheckbox_hk)
        {
          HourlyHK_SO2Subset = subset( HourlyHK_SO2 , HourlyHK_SO2$location == input$site_sel & HourlyHK_SO2$Date == as.character(input$date_hk) )
          finalplot = finalplot + geom_line(aes(x=HourlyHK_SO2Subset$Time,y = HourlyHK_SO2Subset$ug_m3,group=1,color="SO2(ug/m3)"),size=1.5) + geom_point(aes(x=HourlyHK_SO2Subset$Time,y = HourlyHK_SO2Subset$ug_m3),size=2) 
        }

      }

  }

  if(input$category_data == "daily" )
  {
      DailyHK_CO <- DailyHK_CO()
      DailyHK_NO2 <- DailyHK_NO2()
      DailyHK_O3 <- DailyHK_O3()
      DailyHK_PM10 <- DailyHK_PM10()
      DailyHK_PM2 <- DailyHK_PM2()
      DailyHK_SO2 <- DailyHK_SO2()

      if(input$units_hk == "imperial")
      {

        if ("CO" %in% input$pollutantscheckbox_hk)
        {
          DailyHK_COSubset = subset( DailyHK_CO , DailyHK_CO$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(DailyHK_COSubset$Date),y = DailyHK_COSubset$PPB,group=1,color="CO(PPB)"),size=1.5) + geom_point(aes(x=as.Date(DailyHK_COSubset$Date),y = DailyHK_COSubset$PPB),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

        if ("NO2" %in% input$pollutantscheckbox_hk)
        {
          DailyHK_NO2Subset = subset( DailyHK_NO2 , DailyHK_NO2$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(DailyHK_NO2Subset$Date),y = DailyHK_NO2Subset$PPB,group=1,color="NO2(PPB)"),size=1.5) + geom_point(aes(x=as.Date(DailyHK_NO2Subset$Date),y = DailyHK_NO2Subset$PPB),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

        if ("O3" %in% input$pollutantscheckbox_hk)
        {
          DailyHK_O3Subset = subset( DailyHK_O3 , DailyHK_O3$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(DailyHK_O3Subset$Date),y = DailyHK_O3Subset$PPB,group=1,color="O3(PPB)"),size=1.5) + geom_point(aes(x=as.Date(DailyHK_O3Subset$Date),y = DailyHK_O3Subset$PPB),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

        if ("PM10" %in% input$pollutantscheckbox_hk)
        {
          DailyHK_PM10Subset = subset( DailyHK_PM10 , DailyHK_PM10$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(DailyHK_PM10Subset$Date),y = DailyHK_PM10Subset$PPB,group=1,color="PM10(PPB)"),size=1.5) + geom_point(aes(x=as.Date(DailyHK_PM10Subset$Date),y = DailyHK_PM10Subset$PPB),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

        if ("PM2" %in% input$pollutantscheckbox_hk)
        {
          DailyHK_PM2Subset = subset( DailyHK_PM2 , DailyHK_PM2$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(DailyHK_PM2Subset$Date),y = DailyHK_PM2Subset$PPB,group=1,color="PM2(PPB)"),size=1.5) + geom_point(aes(x=as.Date(DailyHK_PM2Subset$Date),y = DailyHK_PM2Subset$PPB),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

        if ("SO2" %in% input$pollutantscheckbox_hk)
        {
          DailyHK_SO2Subset = subset( DailyHK_SO2 , DailyHK_SO2$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(DailyHK_SO2Subset$Date),y = DailyHK_SO2Subset$PPB,group=1,color="SO2(PPB)"),size=1.5) + geom_point(aes(x=as.Date(DailyHK_SO2Subset$Date),y = DailyHK_SO2Subset$PPB),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

      }

      if(input$units_hk == "metric")
      {
        if ("CO" %in% input$pollutantscheckbox_hk)
        {
          DailyHK_COSubset = subset( DailyHK_CO , DailyHK_CO$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(DailyHK_COSubset$Date),y = DailyHK_COSubset$ug_m3,group=1,color="CO(ug/m3)"),size=1.5) + geom_point(aes(x=as.Date(DailyHK_COSubset$Date),y = DailyHK_COSubset$ug_m3),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

        if ("NO2" %in% input$pollutantscheckbox_hk)
        {
          DailyHK_NO2Subset = subset( DailyHK_NO2 , DailyHK_NO2$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(DailyHK_NO2Subset$Date),y = DailyHK_NO2Subset$ug_m3,group=1,color="NO2(ug/m3)"),size=1.5) + geom_point(aes(x=as.Date(DailyHK_NO2Subset$Date),y = DailyHK_NO2Subset$ug_m3),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

        if ("O3" %in% input$pollutantscheckbox_hk)
        {
          DailyHK_O3Subset = subset( DailyHK_O3 , DailyHK_O3$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(DailyHK_O3Subset$Date),y = DailyHK_O3Subset$ug_m3,group=1,color="O3(ug/m3)"),size=1.5) + geom_point(aes(x=as.Date(DailyHK_O3Subset$Date),y = DailyHK_O3Subset$ug_m3),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

        if ("PM10" %in% input$pollutantscheckbox_hk)
        {
          DailyHK_PM10Subset = subset( DailyHK_PM10 , DailyHK_PM10$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(DailyHK_PM10Subset$Date),y = DailyHK_PM10Subset$ug_m3,group=1,color="PM10(ug/m3)"),size=1.5) + geom_point(aes(x=as.Date(DailyHK_PM10Subset$Date),y = DailyHK_PM10Subset$ug_m3),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

        if ("PM2" %in% input$pollutantscheckbox_hk)
        {
          DailyHK_PM2Subset = subset( DailyHK_PM2 , DailyHK_PM2$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(DailyHK_PM2Subset$Date),y = DailyHK_PM2Subset$ug_m3,group=1,color="PM2(ug/m3)"),size=1.5) + geom_point(aes(x=as.Date(DailyHK_PM2Subset$Date),y = DailyHK_PM2Subset$ug_m3),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

        if ("SO2" %in% input$pollutantscheckbox_hk)
        {
          DailyHK_SO2Subset = subset( DailyHK_SO2 , DailyHK_SO2$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(DailyHK_SO2Subset$Date),y = DailyHK_SO2Subset$ug_m3,group=1,color="SO2(ug/m3)"),size=1.5) + geom_point(aes(x=as.Date(DailyHK_SO2Subset$Date),y = DailyHK_SO2Subset$ug_m3),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

      }

  }

  if(input$category_data == "monthly" )
  {
  
      MonthlyHK_CO <- MonthlyHK_CO()
      MonthlyHK_NO2 <- MonthlyHK_NO2()
      MonthlyHK_O3 <- MonthlyHK_O3()
      MonthlyHK_PM10 <- MonthlyHK_PM10()
      MonthlyHK_PM2 <- MonthlyHK_PM2()
      MonthlyHK_SO2 <- MonthlyHK_SO2()

      if(input$units_hk == "imperial")
      {

        if ("CO" %in% input$pollutantscheckbox_hk)
        {
          MonthlyHK_COSubset = subset( MonthlyHK_CO , MonthlyHK_CO$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(MonthlyHK_COSubset$Date),y = MonthlyHK_COSubset$PPB,group=1,color="CO(PPB)"),size=1.5) + geom_point(aes(x=as.Date(MonthlyHK_COSubset$Date),y = MonthlyHK_COSubset$PPB),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

        if ("NO2" %in% input$pollutantscheckbox_hk)
        {
          MonthlyHK_NO2Subset = subset( MonthlyHK_NO2 , MonthlyHK_NO2$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(MonthlyHK_NO2Subset$Date),y = MonthlyHK_NO2Subset$PPB,group=1,color="NO2(PPB)"),size=1.5) + geom_point(aes(x=as.Date(MonthlyHK_NO2Subset$Date),y = MonthlyHK_NO2Subset$PPB),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

        if ("O3" %in% input$pollutantscheckbox_hk)
        {
          MonthlyHK_O3Subset = subset( MonthlyHK_O3 , MonthlyHK_O3$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(MonthlyHK_O3Subset$Date),y = MonthlyHK_O3Subset$PPB,group=1,color="O3(PPB)"),size=1.5) + geom_point(aes(x=as.Date(MonthlyHK_O3Subset$Date),y = MonthlyHK_O3Subset$PPB),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

        if ("PM10" %in% input$pollutantscheckbox_hk)
        {
          MonthlyHK_PM10Subset = subset( MonthlyHK_PM10 , MonthlyHK_PM10$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(MonthlyHK_PM10Subset$Date),y = MonthlyHK_PM10Subset$PPB,group=1,color="PM10(PPB)"),size=1.5) + geom_point(aes(x=as.Date(MonthlyHK_PM10Subset$Date),y = MonthlyHK_PM10Subset$PPB),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

        if ("PM2" %in% input$pollutantscheckbox_hk)
        {
          MonthlyHK_PM2Subset = subset( MonthlyHK_PM2 , MonthlyHK_PM2$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(MonthlyHK_PM2Subset$Date),y = MonthlyHK_PM2Subset$PPB,group=1,color="PM2(PPB)"),size=1.5) + geom_point(aes(x=as.Date(MonthlyHK_PM2Subset$Date),y = MonthlyHK_PM2Subset$PPB),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

        if ("SO2" %in% input$pollutantscheckbox_hk)
        {
          MonthlyHK_SO2Subset = subset( MonthlyHK_SO2 , MonthlyHK_SO2$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(MonthlyHK_SO2Subset$Date),y = MonthlyHK_SO2Subset$PPB,group=1,color="SO2(PPB)"),size=1.5) + geom_point(aes(x=as.Date(MonthlyHK_SO2Subset$Date),y = MonthlyHK_SO2Subset$PPB),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

      }

      if(input$units_hk == "metric")
      {
        if ("CO" %in% input$pollutantscheckbox_hk)
        {
          MonthlyHK_COSubset = subset( MonthlyHK_CO , MonthlyHK_CO$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(MonthlyHK_COSubset$Date),y = MonthlyHK_COSubset$ug_m3,group=1,color="CO(ug/m3)"),size=1.5) + geom_point(aes(x=as.Date(MonthlyHK_COSubset$Date),y = MonthlyHK_COSubset$ug_m3),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

        if ("NO2" %in% input$pollutantscheckbox_hk)
        {
          MonthlyHK_NO2Subset = subset( MonthlyHK_NO2 , MonthlyHK_NO2$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(MonthlyHK_NO2Subset$Date),y = MonthlyHK_NO2Subset$ug_m3,group=1,color="NO2(ug/m3)"),size=1.5) + geom_point(aes(x=as.Date(MonthlyHK_NO2Subset$Date),y = MonthlyHK_NO2Subset$ug_m3),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

        if ("O3" %in% input$pollutantscheckbox_hk)
        {
          MonthlyHK_O3Subset = subset( MonthlyHK_O3 , MonthlyHK_O3$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(MonthlyHK_O3Subset$Date),y = MonthlyHK_O3Subset$ug_m3,group=1,color="O3(ug/m3)"),size=1.5) + geom_point(aes(x=as.Date(MonthlyHK_O3Subset$Date),y = MonthlyHK_O3Subset$ug_m3),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

        if ("PM10" %in% input$pollutantscheckbox_hk)
        {
          MonthlyHK_PM10Subset = subset( MonthlyHK_PM10 , MonthlyHK_PM10$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(MonthlyHK_PM10Subset$Date),y = MonthlyHK_PM10Subset$ug_m3,group=1,color="PM10(ug/m3)"),size=1.5) + geom_point(aes(x=as.Date(MonthlyHK_PM10Subset$Date),y = MonthlyHK_PM10Subset$ug_m3),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

        if ("PM2" %in% input$pollutantscheckbox_hk)
        {
          MonthlyHK_PM2Subset = subset( MonthlyHK_PM2 , MonthlyHK_PM2$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(MonthlyHK_PM2Subset$Date),y = MonthlyHK_PM2Subset$ug_m3,group=1,color="PM2(ug/m3)"),size=1.5) + geom_point(aes(x=as.Date(MonthlyHK_PM2Subset$Date),y = MonthlyHK_PM2Subset$ug_m3),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

        if ("SO2" %in% input$pollutantscheckbox_hk)
        {
          MonthlyHK_SO2Subset = subset( MonthlyHK_SO2 , MonthlyHK_SO2$location == input$site_sel)
          finalplot = finalplot + geom_line(aes(x=as.Date(MonthlyHK_SO2Subset$Date),y = MonthlyHK_SO2Subset$ug_m3,group=1,color="SO2(ug/m3)"),size=1.5) + geom_point(aes(x=as.Date(MonthlyHK_SO2Subset$Date),y = MonthlyHK_SO2Subset$ug_m3),size=2) + scale_x_date(date_labels = "%b/%Y") 
        }

      }

  }

  finalplot = finalplot + theme(legend.title = element_blank()) + theme(legend.text=element_text(size=18)) + xlab(label="Time/Month") + ylab(label="Measured Values") + guides(linetype=guide_legend(keywidth = 3, keyheight = 2), color=guide_legend(keywidth = 3, keyheight = 2))
  finalplot


})

#Leaflet for site selected in Hong Kong

output$leaf_hk <- renderLeaflet({

  leafletdataHK <- leafletdataHK()

  dataleaf <- subset(leafletdataHK, leafletdataHK$location == input$site_sel)
    map <- leaflet()
    map <- addTiles(map)
    map <- setView(map, lng = dataleaf$longitude, lat = dataleaf$latitude, zoom = 10)
    map <- addMarkers(map, lng = dataleaf$longitude, lat = dataleaf$latitude, label = paste0(input$site_sel),labelOptions = labelOptions(noHide = T, textOnly = TRUE), popup = paste0(input$site_sel))
    map
}) 

#Necessary information about the project

output$content <- renderUI({
  text = "Project 2 - Every Breath You Take <br>
  Course - CS 424 Visualization Analytics - Spring'19 <br><br>
  
  Team(Group 3): <br>
  Sai Krishnan Thiruvarpu Neelakantan - sthiru5@uic.edu <br>
  Praveen Chandrasekaran - pchand34@uic.edu <br>
  Varsha Jayaraman - vjayar6@uic.edu <br>
  Abdullah Aleem - aaleem2@uic.edu <br><br>

  Libraries Used : <br>
  shiny, shinydashboard, ggplot2, lubridate, DT, grid, leaflet, scales, shinycssloaders, shinyWidgets
  tidyverse, tmap, tmaptools, sf, splitstackshape, cdlTools, plotly <br><br> 

  Data Source : <br>
  The data was collected from the US EPA website <br>
  You can download all the Annual, Daily, Hourly data files from <a target='_BLANK' href='https://aqs.epa.gov/aqsweb/airdata/download_files.html'>https://aqs.epa.gov/aqsweb/airdata/download_files.html</a><br>
  The data files for Hong Kong can be downloaded from <a target='_BLANK' href='https://openaq.org/#/countries'>https://openaq.org/#/countries'>https://openaq.org/#/countries'>https://openaq.org/#/countries</a> <br>
  The data files for all Heatmaps can be downloaded from <a target='_BLANK' href='https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html'>https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html</a> <br><br>


  Description : <br>
  The application analyses the Air Quality data for different states and counties across the US from 1990-2018. The Air Quality data has
  been categorized into Annual, Daily, Hourly data files. The application analyses the same data in multiple ways (Pie Chart, Bar Chart, Line Chart, HeatMap, Table) to derive insights from the changing 
  trends over the years. Apart from analysing the Air Quality for the US, this application also analyses the Air Quality data for Hong Kong as well. <br><br>   

  Purpose : <br>

  `I’m supposed to be in school, but instead I’m out here trying to make sure that my kids don’t grow up in a wasteland.` — Arielle Geismar, 17, An excerpt from The New York Times.<br>
   The above statement was stated by a teen student from Manhattan concerning the Climate Change. When many such youngsters took to the roads on March the sixteenth, concerned about their progeny at such a young age, we know that something is really not going the way it should. Today, Climate Change is an impending doom that we are facing collectively. It is not something that we can afford to be negligent or ignorant about.
   But, is it just a myth? Are we really doomed? Where's the proof? If true, how do we go about it? What do we do about it? WHAT DO WE KNOW ABOUT IT?
   In this epoch, there's very little that we cannot do to help or change any situation for the better. This is that time, wherein we can conquer the world by just clicking a few keys on our electronic devices. But how do we use this super power to solve our crisis? By studying data, by drawing meaningful patterns from it, by infering a sequence of events correlating a consequence which could serve as a forewarning to us, thus saving millions in terms of nature and other resources.
   This is precisely what we aim to do with our Project, Every Breath We Take. We have collated the Air Quality Index data of The United States of America to kickoff what could serve as a forceful reminder of where we are headed with respect to Our Existence. We have collected data regarding various attributes which correspond to the Air Quality Index such as Wind Speed, Temperature, Levels of various Pollutants in the air such as Carbon-Monoxise, Nitrous Oxide, Sulphur-Dioxide, Ozone, etc. We have analysed the trends of the above stated components over the span of the last forty years to establish a significant change in our ecosystem. We have toyed with the data from all angles and explored various means of communicating this with our Brethren.
   We have presented various normal/alarming trends in our project visually, apart from this write-up as we humans respond and process visual data better than any other type of data. We have illustrated the patterns via Bar Graphs, Stacked Graphs, Line Charts, Pie Graphs, Heatmaps and what not. We have experimented with different layouts considering the user experience. <br><br>
   Please click <a target='_BLANK' href='https://sthiru5.people.uic.edu/CS424/project2.html'>here</a> to view the detailed report about the project.
  "


  HTML(text)

})


}


shinyApp(ui = ui, server = server)
