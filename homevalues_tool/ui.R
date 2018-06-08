library(shiny)
library(vars)
library(tseries, quietly = T)
library(forecast, quietly = T)
library('reshape2')
library('data.table')
library('stringr')
library('ggplot2')
library('plotly')

Mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
setwd("/home/intrepidinsight/ShinyApps/homevalues")
load("data/cities_counties.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("City-to-City Housing Value Impact Modeler"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectizeInput("selectcounty", "Select County:", sort(unique(cities_counties[num>1,CountyName])), multiple = FALSE,
                     options = NULL),
      uiOutput("Box1"),
      uiOutput("Box2"),
      htmlOutput("instruct"),
      p("All impulse responses are computed based on a VAR model in levels, and are not orthogonalized."),
      p("This project uses data courtesy of Zillow. The relevant series is:"),
      uiOutput("source")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      verticalLayout(plotlyOutput("Plot1"), plotlyOutput("Plot2"))
    )
  )
)

