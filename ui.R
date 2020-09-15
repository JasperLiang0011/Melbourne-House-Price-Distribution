# Student name: Kun Liang
# Student ID: 30118891
# Last modified: 18 June, 2020

# Used packages
library(shiny)
library(leaflet)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(shinycssloaders)
library(tidyverse)
library(scales)
library(knitr)
library(kableExtra)
library(ggfortify)
library(plyr)
library(dplyr)
library(plotly)
library(zoo)
library(grid)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(markdown)
library(memoise)


# Used packages
packs = c("shiny", "shinydashboard", "shinythemes", "plotly", "shinycssloaders",
          "scales","dplyr","plotly")

# Comment out below codes to install the required packages
# install.packages(packs)

# setwd("~/KunLiang_30118891_Code")

# Define working directory
data.1 <- read.csv('house_data.csv')

# preparing the data for visualisation
housetypes <- list("House" = 'h', 
                     "Townhouse" = 't', 
                     "Unit" = 'u')

# filter the data for better visualisation
filter.sub = as.character(unique(data.1$Suburb))
for (i in filter.sub){
  subset = data.1 %>% filter(Suburb==i)
  if (nrow(subset) <= 3){
    filter.sub = filter.sub[filter.sub != i]
  }
  if (length(unique(subset$Room)) < 3 | length(unique(subset$Bathroom)) < 3 | length(unique(subset$Car)) < 3){
    filter.sub = filter.sub[filter.sub != i]
  }
}
filter.data = data.1 %>% filter(Suburb %in% filter.sub)
sub.list = as.character(unique(filter.data$Suburb))
sub.list = append(sub.list,'All',0)


shinyUI(navbarPage(
  "Melbourne house price Analysis",
  tabPanel("Map",fluidPage(theme = shinytheme("yeti")),
           tags$head(
             tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
           pageWithSidebar(
             headerPanel('Apply filters'),
             sidebarPanel(width = 4,
                          radioButtons("type", "Choose a house type:",
                                       choices = housetypes, selected = 'h'),
                          #suburbs would load automatically as user select options above.
                          radioButtons("mode", "Sales/Prices:",
                                       choices = c('Sales','Prices'), selected = 'Sales'),
                         
                          submitButton("Change")
             ),
             mainPanel(
               column(6, leafletOutput("mymap", width = 700, height=500),
                      p("To visualize the house price distribution of melbourne, choose the house type on the left side to get
             the corresponding map. You can see the sales and prices.",
                        style = "font-size:16px")
                      
               )
             )
           )),
  
  tabPanel("Time",headerPanel("Time factor"),
           fluidRow(column(4,
                           selectInput('region.1', 
                                         'Choose a region:',as.character(unique(data.1$Regionname)), 
                                         selected = 'Northern Metropolitan',width = 300)),
                    column(8,
                           selectInput('suburb.2', 'Choose a suburb:',
                                       choices = sub.list,selected = 'All')),
                    column(8, offset = 7,
                           submitButton("Change"))
                    ),
           
           p("The top 3 hot-sell months are:",style = "font-size:25px"),
           fluidRow(
             splitLayout(cellWidths = c('40%','60%'),
                         plotOutput("woldcloudplot", width = 500, height=400),
                         plotlyOutput('line',width=750,height=450)))
           ),
  
  tabPanel("House type",fluidPage(theme = shinytheme("yeti")),
             headerPanel('The popular house structure in each suburb'),
           
             selectInput('suburb', 'Choose a suburb:',
                         choices = sub.list,selected = 'All'),
             submitButton("Update filters"),
             fluidRow(
               column(6,box("The number of Rooms",plotlyOutput("radar1", width = 400, height=300))),
               column(6,box("The number of Bathrooms",plotlyOutput("radar2", width = 400, height=300)))),
             fluidRow(
               column(6,box("The number of Garages",plotlyOutput("radar3", width = 400, height=300))),
               column(6,box("The price distribution of each type",plotOutput("hist", width =500, height=300))))
           )
  
))

