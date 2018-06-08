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
# Define server logic required to draw a histogram
server <- function(input, output) {
  url<-a("Median Home Values per Sq Ft, By City", href="https://www.zillow.com/research/data/")
  output$instruct<-renderText({
    "<b>To use: First select a city name in the first drop down menu. 
    The second menu will then be populated with all cities within the county.
    <b>"
  })
  output$source<-renderUI({
    tagList(url)
  })
  output$Box1 = renderUI(selectizeInput("selectcity1", "Select 1st City (Within County):", sort(unique(cities_counties[CountyName==input$selectcounty,city])), multiple = FALSE,
                                        options = NULL))
  output$Box2 = renderUI(selectizeInput("selectcity2", "Select 2nd City (Within County):", sort(unique(cities_counties[CountyName==input$selectcounty & city!=input$selectcity1,city])), multiple = FALSE,
                                        options = NULL))
  
  output$Plot1 <- renderPlotly({
    req(input$selectcity2, input$selectcity1,(file.exists(paste0("graphs/",str_replace_all(paste(input$selectcity1, input$selectcity2), " ", "_"),".RData")) | file.exists(paste0("graphs/",str_replace_all(paste(input$selectcity2, input$selectcity1), " ", "_"),".RData"))), cancelOutput = TRUE)
    
    if (file.exists(paste0("graphs/",str_replace_all(paste(input$selectcity1, input$selectcity2), " ", "_"),".RData"))){
      file<-paste0("graphs/",str_replace_all(paste(input$selectcity1, input$selectcity2), " ", "_"),".RData")
      z<-2
      zz<-1
    } else {
      file<-paste0("graphs/",str_replace_all(paste(input$selectcity2, input$selectcity1), " ", "_"),".RData")
      z<-1
      zz<-2
    }
    
    load(file)
    graph1<-plot_ly(get(paste0("dt_city", zz,"_irf")), x=~ord, y = ~get(paste0("city", z, "_upper")), type='scatter', mode='lines',
                    line = list(color = 'transparent'),
                    showlegend = FALSE, name = 'Upper Bound') %>%
      add_trace(x = ~ord,y = ~get(paste0("city",z,"_lower")), type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                showlegend = FALSE, name = 'Lower Bound') %>%
      add_trace(x = ~ord, y = ~get(paste0("city",z)), type = 'scatter', mode = 'lines',
                line = list(color='rgb(0,100,80)'),
                name = 'Average') %>%
      layout(title = paste("Impulse Response of",get(paste0("selectname",z)),"\nto a $1 Increase in", get(paste0("selectname",zz))),
             paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
             margin = list(l=125, r=50, b=100, t=100, pad=6),
             xaxis = list(title = "Months Since Shock",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE),
             yaxis = list(title = "Change in Median Value per Square Foot",
                          tickprefix="$",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE))
    graph1$elementId<-NULL
    graph1
  })
  output$Plot2 <- renderPlotly({
    req(input$selectcity2, input$selectcity1,(file.exists(paste0("graphs/",str_replace_all(paste(input$selectcity1, input$selectcity2), " ", "_"),".RData")) | file.exists(paste0("graphs/",str_replace_all(paste(input$selectcity2, input$selectcity1), " ", "_"),".RData"))), cancelOutput = TRUE)     
    if (file.exists(paste0("graphs/",str_replace_all(paste(input$selectcity1, input$selectcity2), " ", "_"),".RData"))){
      file<-paste0("graphs/",str_replace_all(paste(input$selectcity1, input$selectcity2), " ", "_"),".RData")
      v<-2
      vv<-1
    } else {
      file<-paste0("graphs/",str_replace_all(paste(input$selectcity2, input$selectcity1), " ", "_"),".RData")
      v<-1
      vv<-2
    }
    
    load(file)
    graph2<-plot_ly(get(paste0("dt_city", v,"_irf")), x=~ord, y = ~get(paste0("city",vv,"_upper")), type='scatter', mode='lines',
                    line = list(color = 'transparent'),
                    showlegend = FALSE, name = 'Upper Bound') %>%
      add_trace(x = ~ord,y = ~get(paste0("city",vv,"_lower")), type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                showlegend = FALSE, name = 'Lower Bound') %>%
      add_trace(x = ~ord, y = ~get(paste0("city",vv)), type = 'scatter', mode = 'lines',
                line = list(color='rgb(0,100,80)'),
                name = 'Average') %>%
      layout(title = paste("Impulse Response of",get(paste0("selectname", vv)),"\nto a $1 Increase in", get(paste0("selectname", v))),
             paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
             margin = list(l=125, r=50, b=100, t=100, pad=6),
             xaxis = list(title = "Months Since Increase",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE),
             yaxis = list(title = "Change in Median Value per Square Foot",
                          tickprefix="$",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE))
    graph2$elementId<-NULL
    graph2
  })
}
