# Brian Baez
# Project 3: We've Got the Power
# CS 424 - UIC Spring 2021

library(shiny)
library(shinydashboard)
library(readxl)
library(mapview)
library(tigris)
library(dplyr)
library(forcats)
library(DT)
library(ggplot2)
library(leaflet)
library(leaflet.extras)

################################################################################
# Data
################################################################################

# read data from "Energy_Usage_2010.csv"
data <- read.csv("Energy_Usage_2010.csv")

# select the columns needed
data <- select(data, 1, 2, 3, 4, 5:17, 20:32, 64, 65, 66, 67, 70, 72)

# rename the columns
colnames(data) <- c("AREA_NAME", "CENSUS_BLOCK", "TYPE", "SUBTYPE",
                    "JAN_ELECTRICITY",
                    "FEB_ELECTRICITY",
                    "MAR_ELECTRICITY",
                    "APR_ELECTRICITY",
                    "MAY_ELECTRICITY",
                    "JUNE_ELECTRICITY",
                    "JULY_ELECTRICITY",
                    "AUG_ELECTRICITY",
                    "SEPT_ELECTRICITY",
                    "OCT_ELECTRICITY",
                    "NOV_ELECTRICITY",
                    "DEC_ELECTRICITY",
                    "TOTAL_ELECTRICITY",
                    "JAN_GAS",
                    "FEB_GAS",
                    "MAR_GAS",
                    "APR_GAS",
                    "MAY_GAS",
                    "JUNE_GAS",
                    "JULY_GAS",
                    "AUG_GAS",
                    "SEPT_GAS",
                    "OCT_GAS",
                    "NOV_GAS",
                    "DEC_GAS",
                    "TOTAL_GAS",
                    "TOTAL_POPULATION",
                    "TOTAL_UNITS",
                    "AVG_STORIES",
                    "AVG_BUILDING_AGE",
                    "OCCUPIED_PERCENTAGE",
                    "RENTER_PERCENTAGE")

# remove any rows with NA values
data <- na.omit(data)

# sums of gas data
gasSums <- data[c(1, 18:29)]
gasSums <- subset(gasSums, gasSums$AREA_NAME=="Near West Side")
gasSums <- gasSums[,-1]
gasSums <- colSums(gasSums)
gasSums <- as.data.frame(gasSums)
colnames(gasSums) <- "Amount"

# electricity data
elecData <- data[c(1, 5:16)]
elecData <- subset(elecData, elecData$AREA_NAME=="Near West Side")
colnames(elecData) <- c("Community Area", "January", "February", "March", "April", "May", "June",
                        "July", "August", "September", "October", "November", "December")

# gas data
gasData <- data[c(1, 18:29)]
gasData <- subset(gasData, gasData$AREA_NAME=="Near West Side")
colnames(gasData) <- c("Community Area", "January", "February", "March", "April", "May", "June",
                        "July", "August", "September", "October", "November", "December")

# names of months
months <- as.factor(c("January", "February", "March", "April", "May", "June",
            "July", "August", "September", "October", "November", "December"))

# community areas
areas <- as.data.frame(unique(data$AREA_NAME))

################################################################################
# UI
################################################################################
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title="CS 424 Project 3"),
  dashboardSidebar(
    sidebarMenu(
      # names of tabs
      menuItem("Near West Side Power Usage", tabName = "nearWestSidePage", icon = icon("lightbulb")),
      menuItem("Community Comparison", tabName = "comparisonPage", icon = icon("plug")),
      menuItem("Chicago Power Usage", tabName = "chicagoPage", icon = icon("bolt")),
      menuItem("About", tabName = "aboutPage", icon = icon("question"))
    ) # end sidebarMenu
  ), # end dashboardSidebar
  dashboardBody(
    tabItems(
      # start of nearWestSidePage tab
      tabItem(tabName = "nearWestSidePage",
              fluidRow(
                column(width=9,
                    box(title="Heatmap of Electricity Usage in Near West Side", solidHeader=TRUE,
                        width=12,
                        leafletOutput("nearWestSideMap", height = 300, width ="100%")
                        ),
                    
                    box(solidHeader=TRUE,
                        width=12,
                        tabBox(width=12,
                               selected="Electricity",
                                tabPanel("Electricity",
                                      plotOutput("graph1", height=200, width="100%")  
                                ),
                                tabPanel("Gas",
                                      plotOutput("graph2", height=200, width="100%")
                                )
                        )
                    ),
                        
                    
                    box(solidHeader=TRUE,
                        width=12,
                        tabBox(width=12,
                               selected="Electricity",
                               tabPanel("Electricity",
                                        dataTableOutput("table1", height=200, width="100%")
                                ),
                               tabPanel("Gas",
                                        dataTableOutput("table2", height=200, width="100%")
                               )
                        )
                        
                    )
                ),
                
                column(width=3,
                      box(title="Filter Your Search", solidHeader=TRUE, width=12,
                          selectInput("dataShown", "Data Shown:",
                                      c("Gas" = "gas",
                                        "Electricity" = "elec",
                                        "Building Age" = "age",
                                        "Building Type" = "type",
                                        "Building Height" = "height",
                                        "Total Population" = "population"),
                                      selected = "elec"
                          ), # end data selection menu
                          
                          selectInput("monthShown", "Month Shown:",
                                      c("All" = "all",
                                        "January" = "jan",
                                        "February" = "feb",
                                        "March" = "mar",
                                        "April" = "apr",
                                        "May" = "may",
                                        "June" = "june",
                                        "July" = "july",
                                        "August" = "aug",
                                        "September" = "sept",
                                        "October" = "oct",
                                        "November" = "nov",
                                        "December" = "dec"
                                        ),
                                      selected = "all"
                          ), # end month menu
                      )
                )
              )
      ), # end nearWestSidePage tab
      
      # start of comparisonPage tab
      tabItem(tabName = "comparisonPage",
              box(solidHeader = TRUE,
                  width = 6,
                  fluidRow(
                    box(width=12, status="warning",
                        selectInput("areaShown", "Select a Community Area:",
                                    areas,
                                    selected="Near West Side"
                        )
                        
                      )
                  ),
                  
                  fluidRow(
                    box(width=6, status="success",
                        selectInput("dataShown", "Data Shown:",
                                    c("Gas" = "gas",
                                      "Electricity" = "elec",
                                      "Building Age" = "age",
                                      "Building Type" = "type",
                                      "Building Height" = "height",
                                      "Total Population" = "population"),
                                    selected = "elec"
                        ) # end data selection menu
                    ),
                    box(width=6, status="primary",
                        selectInput("monthShown", "Month Shown:",
                                    c("All" = "all",
                                      "January" = "jan",
                                      "February" = "feb",
                                      "March" = "mar",
                                      "April" = "apr",
                                      "May" = "may",
                                      "June" = "june",
                                      "July" = "july",
                                      "August" = "aug",
                                      "September" = "sept",
                                      "October" = "oct",
                                      "November" = "nov",
                                      "December" = "dec"
                                    ),
                                    selected = "all"
                        ) # end month menu
                    )
                  ),
                  
                  tabBox(width=12,
                         selected="Electricity",
                         tabPanel("Electricity",
                                  plotOutput("graph3", height=200, width="100%")
                         ),
                         tabPanel("Gas",
                                  plotOutput("graph4", height=200, width="100%")
                         )
                  )
              ),
              
              box(solidHeader = TRUE,
                  width = 6,
                  fluidRow(
                    box(width=12, status="warning",
                        selectInput("areaShown", "Select a Community Area:",
                                    areas,
                                    selected="Loop"
                        )
                        
                    )
                  ),
                  
                  fluidRow(
                    box(width=6, status="success",
                        selectInput("dataShown", "Data Shown:",
                                    c("Gas" = "gas",
                                      "Electricity" = "elec",
                                      "Building Age" = "age",
                                      "Building Type" = "type",
                                      "Building Height" = "height",
                                      "Total Population" = "population"),
                                    selected = "elec"
                        ) # end data selection menu
                    ),
                    box(width=6, status="primary",
                        selectInput("monthShown", "Month Shown:",
                                    c("All" = "all",
                                      "January" = "jan",
                                      "February" = "feb",
                                      "March" = "mar",
                                      "April" = "apr",
                                      "May" = "may",
                                      "June" = "june",
                                      "July" = "july",
                                      "August" = "aug",
                                      "September" = "sept",
                                      "October" = "oct",
                                      "November" = "nov",
                                      "December" = "dec"
                                    ),
                                    selected = "all"
                        ) # end month menu
                    )
                  ),
                  
                  tabBox(width=12,
                         selected="Electricity",
                         tabPanel("Electricity",
                                  plotOutput("graph5", height=200, width="100%")
                         ),
                         tabPanel("Gas",
                                  plotOutput("graph6", height=200, width="100%")
                         )
                  )
              )
              
      ), # end comparisonPage tab
      
      # start of chicagoPage tab
      tabItem(tabName = "chicagoPage",
              box(title="Chicago Census Blocks",
                  width=12, solidHeader = TRUE,
                  leafletOutput("chicagoMap", height = 550, width ="100%")
                  )
      ), # end chicagoPage tab
      
      # start of aboutPage tab
      tabItem(tabName = "aboutPage",
              box(title = "Application created by Brian Baez", solidHeader = TRUE, width = 10,
                  HTML("The data used for this application was acquired from https://data.cityofchicago.org/Environment-Sustainable-Development/Energy-Usage-2010/8yq3-m6wp"))
      ) # end aboutPage tab
    )  # end tabItems
  )  # end dashboardBody
)

################################################################################
# Server
################################################################################

server <- function(input, output, session) {
  # Electricity graph for Near West Side
  output$graph1 <- renderPlot({
    elecSums <- data[c(1, 5:16)]
    elecSums <- subset(elecSums, elecSums$AREA_NAME=="Near West Side")
    elecSums <- elecSums[,-1]
    elecSums <- colSums(elecSums)
    elecSums <- as.data.frame(elecSums)
    colnames(elecSums) <- "Amount"
    
    
    ggplot(data=elecSums, aes(x=fct_inorder(months), y=Amount)) + ggtitle("Power Usage of Electricity in 2010") +
      geom_bar(stat="identity", fill="#2199D1") + xlab("Month") + ylab("Power Usage (Kilowatt-Hours)")
  })
  
  # Gas graph for Near West Side
  output$graph2 <- renderPlot({
    gasSums <- data[c(1, 18:29)]
    gasSums <- subset(gasSums, gasSums$AREA_NAME=="Near West Side")
    gasSums <- gasSums[,-1]
    gasSums <- colSums(gasSums)
    gasSums <- as.data.frame(gasSums)
    colnames(gasSums) <- "Amount"
    
    ggplot(data=gasSums, aes(x=fct_inorder(months), y=Amount)) + ggtitle("Power Usage of Gas in 2010") +
      geom_bar(stat="identity", fill="#E8A251") + xlab("Month") + ylab("Power Usage (Kilowatt-Hours)")
  })
  
  # Electricity graph for Near West Side
  output$graph3 <- renderPlot({
    elecSums <- data[c(1, 5:16)]
    elecSums <- subset(elecSums, elecSums$AREA_NAME=="Near West Side")
    elecSums <- elecSums[,-1]
    elecSums <- colSums(elecSums)
    elecSums <- as.data.frame(elecSums)
    colnames(elecSums) <- "Amount"
    
    ggplot(data=elecSums, aes(x=fct_inorder(months), y=Amount)) + ggtitle("Power Usage of Electricity in 2010") +
      geom_bar(stat="identity", fill="#2199D1") + xlab("Month") + ylab("Power Usage (Kilowatt-Hours)")
  })
  
  # Gas graph for Near West Side
  output$graph4 <- renderPlot({
    gasSums <- data[c(1, 18:29)]
    gasSums <- subset(gasSums, gasSums$AREA_NAME=="Near West Side")
    gasSums <- gasSums[,-1]
    gasSums <- colSums(gasSums)
    gasSums <- as.data.frame(gasSums)
    colnames(gasSums) <- "Amount"
    
    ggplot(data=gasSums, aes(x=fct_inorder(months), y=Amount)) + ggtitle("Power Usage of Gas in 2010") +
      geom_bar(stat="identity", fill="#E8A251") + xlab("Month") + ylab("Power Usage (Kilowatt-Hours)")
  })
  
  # Electricity graph for Loop
  output$graph5 <- renderPlot({
    elecSums <- data[c(1, 5:16)]
    elecSums <- subset(elecSums, elecSums$AREA_NAME=="Loop")
    elecSums <- elecSums[,-1]
    elecSums <- colSums(elecSums)
    elecSums <- as.data.frame(elecSums)
    colnames(elecSums) <- "Amount"
    
    ggplot(data=elecSums, aes(x=fct_inorder(months), y=Amount)) + ggtitle("Power Usage of Electricity in 2010") +
      geom_bar(stat="identity", fill="#2199D1") + xlab("Month") + ylab("Power Usage (Kilowatt-Hours)")
  })
  
  # Gas graph for Loop
  output$graph6 <- renderPlot({
    gasSums <- data[c(1, 18:29)]
    gasSums <- subset(gasSums, gasSums$AREA_NAME=="Loop")
    gasSums <- gasSums[,-1]
    gasSums <- colSums(gasSums)
    gasSums <- as.data.frame(gasSums)
    colnames(gasSums) <- "Amount"
    
    ggplot(data=gasSums, aes(x=fct_inorder(months), y=Amount)) + ggtitle("Power Usage of Gas in 2010") +
      geom_bar(stat="identity", fill="#E8A251") + xlab("Month") + ylab("Power Usage (Kilowatt-Hours)")
  })
  
  # Electricity for Near West Side
  output$table1 <- DT::renderDataTable({
    elecData
  })
  
  # Gas for Near West Side
  output$table2 <- DT::renderDataTable({
    gasData
  })
  
  # renders map for Near West Side in 2010
  output$nearWestSideMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addResetMapButton() %>%
      setView(lat=41.8668, lng=-87.6664, zoom=13)
    
  })
  
  # renders map for Chicago in 2010
  output$chicagoMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addResetMapButton() %>%
      setView(lat=41.8781, lng=-87.6298, zoom=12)
    
  })
  
}

################################################################################
################################################################################

shinyApp(ui, server)