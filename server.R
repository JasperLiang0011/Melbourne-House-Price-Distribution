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

# Define working directory
# setwd("./KunLiang_30118891_Code/")

# read data
data.1 <- read.csv('house_data.csv')
data.2 = read.csv('sub_median_price.csv')

nm <- data.1 %>% filter(Regionname == "Northern Metropolitan")
sm <- data.1 %>% filter(Regionname == "Southern Metropolitan")
em <- data.1 %>% filter(Regionname == "Eastern Metropolitan")
sem <- data.1 %>% filter(Regionname == "South-Eastern Metropolitan")
wm <- data.1 %>% filter(Regionname == "Western Metropolitan")
ev <- data.1 %>% filter(Regionname == "Eastern Victoria")
nv <- data.1 %>% filter(Regionname == "Northern Victoria")
wv <- data.1 %>% filter(Regionname == "Western Victoria")

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
# convert number into english word for radar charts
convert.num <- function(num){
  if(num==1){output='one'}
  else if(num==2){output='two'}
  else if(num==3){output='three'}
  else if(num==4){output='four'}
  else if(num==5){output='five'}
  else if(num==6){output='six'}
  else if(num==7){output='seven'}
  else if(num==8){output='eight'}
  else if(num==9){output='nine'}
  else if(num==10){output='ten'}
  else if(num==0){output='zero'}
  return(output)
}


shinyServer(function(input, output, session) {
  
  selectedData1 <- reactive({
    the.type = get(input$type)
    output = unique(subset(data.1, Type == the.type))
  })

  # Choropleth map
  output$mymap <- renderLeaflet({ # create leaflet map
    # load the geographic data
    geo.map <- geojsonio::geojson_read('postcode_polygon.json',what='sp')
    # filter the data
    sub.set <- data.1 %>% filter(Type==input$type)
    # calculate the sales
    post_data <- data.frame(table(sub.set$Postcode))
    names(post_data) = c('Postcode','count')
    # calculate the mean price
    price <- aggregate( Price ~ Postcode, sub.set, mean )

    # filter the geojson
    target <- subset(geo.map, postcode %in% price$Postcode)
    geo.pc <- data.frame(Postcode=target$postcode)
    pc.count <- merge(x = geo.pc, y = post_data, by = "Postcode",sort=FALSE,all.x=TRUE)
    pc.data <- merge(x = pc.count, y = price, by = "Postcode",sort=FALSE,all.x=TRUE)
    #extract the suburb
    pc.data['sub_list'] = rep(0,nrow(pc.data))
    for(i in 1:nrow(pc.data)){
      code = pc.data[i,'Postcode']
      pc.data[i,'sub_list'] = paste(unlist(unique(subset(sub.set, Postcode==code)$Suburb)),collapse = ',')
    }
    # set the labels
    labels<-paste('<p>', 'Suburbs: ', pc.data$sub_list, '</p>',
                  '<p>', 'Postcode: VIC', pc.data$Postcode, '</p>',
                  '<p>', 'Sales: ', pc.data$count, '</p>',
                  '<p>', 'Average price: $ ', round(pc.data$Price, digits = 0), '</p>',
                  sep = '')
    # set the colors
    c.bin<-c(1,50,100,200,300,400,500,600,700,800)
    p.bin <- c(0,500000,800000,1000000,1200000,1400000,1600000,1800000,2000000,3500000)
    if (input$mode=='Sales'){
      pal<-colorBin('YlOrRd', domain = pc.data$count, bins = c.bin)
      # draw the map
      leaflet()%>%setView( 144.9634,-37.8129, zoom=12)%>%
        addPolygons(data = target,
                    weight =1,
                    smoothFactor = 0.5,
                    color = 'white',
                    fillOpacity = 0.8,
                    fillColor = pal(pc.data$count),
                    highlight=highlightOptions(
                      weight = 5,
                      color = '#666666',
                      fillOpacity = 0.7,
                      bringToFront = TRUE
                    ),
                    label = lapply(labels, HTML))%>% fitBounds(144.50, -38.10, 145.40, -37.52)%>%
        addLegend(pal= pal, 
                  values = pc.data$count, 
                  opacity = 0.7,title = 'Sales from 2016 to 2019',
                  position = 'topright')
    }
    else if (input$mode=='Prices'){
      pal<-colorBin('YlOrRd', domain = pc.data$Price, bins = p.bin)
      # draw the map
      leaflet()%>%setView( 144.9634,-37.8129, zoom=12)%>%
        addPolygons(data = target,
                    weight =1,
                    smoothFactor = 0.5,
                    color = 'white',
                    fillOpacity = 0.8,
                    fillColor = pal(pc.data$Price),
                    highlight=highlightOptions(
                      weight = 5,
                      color = '#666666',
                      fillOpacity = 0.7,
                      bringToFront = TRUE
                    ),
                    label = lapply(labels, HTML))%>% fitBounds(144.50, -38.10, 145.40, -37.52)%>%
        addLegend(pal= pal, 
                  values = pc.data$Price, 
                  opacity = 0.7,title = 'Prices from 2016 to 2019',
                  position = 'topright')      
    }
    })
  
  # Word cloud
  output$woldcloudplot <- renderPlot({

    # generate data based on user input
    if(input$region.1 == 'Northern Metropolitan') { marker = nm }
    if(input$region.1 == 'Southern Metropolitan') { marker = sm }
    if(input$region.1 == 'Eastern Metropolitan') { marker = em }
    if(input$region.1 == 'South-Eastern Metropolitan') { marker = sem }
    if(input$region.1 == 'Western Metropolitan') { marker = wm }
    if(input$region.1 == 'Eastern Victoria') { marker = ev }
    if(input$region.1 == 'Northern Victoria') { marker = nv }
    if(input$region.1 == 'Western Victoria') { marker = wv }
    # calculate the sales for each month
    month.count = data.frame(table(marker$Month))
    names(month.count) = c('Month','count')
    # draw the wordcloud
    wordcloud(words = month.count$Month, freq = month.count$count,
              colors=brewer.pal(11, "RdBu"))
  })
  
  # Room radar
  output$radar1 <- renderPlotly({
    
    if (input$suburb=='All'){subset=filter.data}
    else{subset = filter.data %>% filter(Suburb==input$suburb)}
    room.count =  data.frame(table(subset$Rooms))
    names(room.count) = c('Room','count')
    room.count$Room = lapply(room.count$Room,convert.num)
    
    cate = t(room.count)[1,]
    maximum = (max(room.count$count) %/% 10 + 1)*10

    
    p<-plot_ly(
      type = 'scatterpolar',
      mode = "closest",
      fill = 'toself'
    ) %>%
      add_trace(
        r = as.matrix(t(room.count)[2,]),
        theta = cate,
        showlegend = TRUE,
        mode = "markers",
        name = t(room.count)[1,]
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,maximum)
          )
        ),
        showlegend=FALSE
      )
    
  })
  
  # Bathroom radar
  output$radar2 <- renderPlotly({
    
    if (input$suburb=='All'){subset=filter.data}
    else{subset = filter.data %>% filter(Suburb==input$suburb)}
    bath.count =  data.frame(table(subset$Bathroom))
    names(bath.count) = c('Bathroom','count')
    bath.count$Bathroom = lapply(bath.count$Bathroom,convert.num)
    
    cate = t(bath.count)[1,]
    maximum = (max(bath.count$count) %/% 10 + 1)*10
    
    
    p<-plot_ly(
      type = 'scatterpolar',
      mode = "closest",
      fill = 'toself'
    ) %>%
      add_trace(
        r = as.matrix(t(bath.count)[2,]),
        theta = cate,
        showlegend = TRUE,
        mode = "legendonly",
        name = t(bath.count)[1,]
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,maximum)
          )
        ),
        showlegend=FALSE
      )
    
  })
  
  # Car radar
  output$radar3 <- renderPlotly({
    
    if (input$suburb=='All'){subset=filter.data}
    else{subset = filter.data %>% filter(Suburb==input$suburb)}
    car.count =  data.frame(table(subset$Car))
    names(car.count) = c('Car','count')
    car.count$Car = lapply(car.count$Car,convert.num)
    
    cate = t(car.count)[1,]
    maximum = (max(car.count$count) %/% 10 + 1)*10
    
    
    p<-plot_ly(
      type = 'scatterpolar',
      mode = "closest",
      fill = 'toself'
    ) %>%
      add_trace(
        r = as.matrix(t(car.count)[2,]),
        theta = cate,
        showlegend = TRUE,
        mode = "markers",
        name = t(car.count)[1,]
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,maximum)
          )
        ),
        showlegend=FALSE
      )
    
  })
  
  # Histograme 
  output$hist <- renderPlot({
    # use the selected suburb as input
    if (input$suburb=='All'){subset=filter.data}
    else{subset = filter.data %>% filter(Suburb==input$suburb)}
    mu <- ddply(subset, "Type", summarise, grp.mean=mean(Price)/1000000)
    ggplot(subset, aes(x=Price/1000000, fill=Type, color=Type)) +
      geom_histogram(position="identity", alpha=0.5,binwidth = 0.1) + 
      geom_vline(data=mu, aes(xintercept=grp.mean, color=Type),
                 linetype="dashed") +
      labs(title = paste(input$suburb,' :'),
           x = "Price", y = "Count") +
      theme(axis.text.x = element_text(face="bold", color="#993333", 
                                       size=12),
            axis.text.y = element_text(face="bold", color="#993333", 
                                       size=9, angle=45)) +
      scale_x_continuous(labels=dollar_format(prefix="$",suffix=" m"))
  })
  
  # Line chart
  output$line <- renderPlotly({
    # use the selected suburb as input
    if (input$suburb.2=='All'){
      trial <- ddply(data.2, "Quarter", summarise, grp.mean=mean(Price))
      names(trial) = c('Quarter','Price')}
    else{trial = data.2 %>% filter(Suburb==input$suburb.2)}
    # get the labels messages  
    x.axis = trial$Quarter
    
    convert.quarter <- function(x){
      return(as.Date(as.yearqtr(x, format = "%YQ%q")))
    }
    
    trial$Quarter = lapply(trial$Quarter,convert.quarter)
    
    df1 <- trial[trial$Quarter >= as.Date('2017-01-01') & trial$Quarter<=as.Date('2018-01-01'),]
    df2 <- trial[trial$Quarter >= as.Date('2018-01-01') & trial$Quarter<=as.Date('2019-01-01'),]
    df3 <- trial[trial$Quarter >= as.Date('2019-01-01') & trial$Quarter<=as.Date('2020-01-01'),]
    
    myarrow=arrow(angle = 15, type = "closed")
    
    colors <- c("2016" = "#138D75", "2017" = "#F1C40F", "2018" = "#EC7063","2019" = "#5DADE2")
    
    p<-ggplot(trial) + labs(title='The Price changes from 2016 to 2019',x='',y='Price')+
      geom_line(aes(x= as.numeric(rownames(trial)),y=Price/1000000,color='2016'),size=1)+
      geom_line(data = df1,aes(x= as.numeric(rownames(df1)),y=Price/1000000,color='2017'),size=1) +
      geom_line(data = df2,aes(x= as.numeric(rownames(df2)),y=Price/1000000,color='2018'),size=1) +
      geom_line(data = df3,aes(x= as.numeric(rownames(df3)),y=Price/1000000,color='2019'),size=1,arrow=myarrow) +
      geom_point(aes(x= as.numeric(rownames(trial)),y=Price/1000000,group=1,
                     text = paste("Time: ", x.axis,
                                  "<br>Suburb: ", trial$Suburb,
                                  "<br>Average Price: $", round(Price/1000000,1),'million')),size=2)+
      scale_color_manual(values=colors)+
      scale_x_continuous(breaks = as.numeric(rownames(trial)), labels=as.character(x.axis)) + 
      scale_y_continuous(labels=dollar_format(prefix="$",suffix=" m")) +
      theme(axis.text.x = element_text(face="bold", color="#993333", 
                                       size=7, angle=45),
            axis.text.y = element_text(face="bold", color="#993333", 
                                       size=9, angle=45))
    ggplotly(p,tooltip = "text")
    
  })
  
})
