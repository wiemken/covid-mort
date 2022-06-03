
source("helpers.R")
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(RCurl)

    ui<-dashboardPage( skin = "black",
        dashboardHeader(title = "COVID-19 Mortality", titleWidth = "100%"
                                ),
        dashboardSidebar(width = 320,
            sidebarMenu(
                br(),
                menuItem("COVID-19 Mortality by Country", tabName = "countrymort"),
                #p("All figures plot"), 
                #p(HTML(paste(HTML('&emsp;'), "exponentially weighted moving averages"))),
                hr(),
                p(a("Learn more about anomaly detection.", target="_blank", href="https://business-science.github.io/anomalize/")),
                #HTML(paste(HTML('&emsp;'), "We used STL decomposition and GESD for Anomaly Detection")),
                p(),
                p(a("Learn more about breakout detection.", target="_blank", href="https://blog.twitter.com/engineering/en_us/a/2014/breakout-detection-in-the-wild.html")),
                #HTML(paste(HTML('&emsp;'), "We used Twitter's Breakout Detection method")),
                hr(),
                HTML(paste(strong("Created by:"), "Timothy Wiemken, PhD", "Samson Niemotka, BS", "Christopher Prener, PhD", sep="<br/>")),
                p(a("Email Us", target="_blank", href="mailto:timothy.wiemken@pfizer.com")), 
                hr(),
                "Version 1.5, June 4, 2022"
            )),
        dashboardBody(
                tags$head(tags$style(HTML('
                  .main-header .logo {
                    font-family: "Georgia", Times, "Times New Roman", serif;
                    font-weight: bold;
                    font-size: 24px;
                  }
                '))),
            tabItems(
                tabItem(tabName = "countrymort",
                        column(4, selectInput('countrydropdown', "Choose your country of interest", choices=sort(countrylist), selected="US")),
                        column(4, textInput('movavg', "Choose window for the exponentially weighted moving average (larger values result in more influence of older data)", value=14*4)),
                        column(4, sliderInput('breakslider_1', "Minimum points in a breakout",min=3, max=30, value=7, step=1, width=200)),
                        plotOutput("plot_both"),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        p(tags$a(href="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", "Data Source (Mortality by Country), Center for Systems Science and Engineering at Johns Hopkins University, GitHub"), style = "font-size:10px"),
                )
                
               ) #TabItems
    ) #DashboardBody 
) #DashboardPage
    
    

server <- function(input, output, session) {

    output$plot_both <- renderPlot({
        anombreak.covid.mort(alpha = 0.05, max_anoms = 0.2, country = input$countrydropdown, n.break=input$breakslider_1, ma.period = as.numeric(input$movavg))
    })


}

shinyApp(ui = ui, server = server)