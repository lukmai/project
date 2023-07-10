library(shiny)
library(tidyr)
library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)
library(gdata)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Car data"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing car brand ----
      selectInput(inputId = "dataset",
                  label = "Choose a Car Brand:",
                  choices = c("Audi", "BMW", "Mercedes-Benz","Porsche","Volkswagen")),
      
      # Input: Selector for make ----
      selectInput("make", "Make:",
                  choices = NULL,
                  multiple = TRUE),
      
      # Input: Selector for years ----
      selectInput("Year", "Year:",
                  choices = c("2017", "2018", "2019","2020","2021","2022"),
                  multiple = TRUE),
      
      # Button
      
      downloadButton("downloadData", "Download")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Overall Plot", 
                           fluidRow(
                             column(width = 4,
                                    plotOutput("plot1", height = 300,
                                               # Equivalent to: click = clickOpts(id = "plot_click")
                                               click = "plot1_click",
                                               brush = brushOpts(
                                                 id = "plot1_brush"
                                               )
                                    )
                             )
                           ),
                           fluidRow(
                             column(width = 6,
                                    h4("Points near click"),
                                    verbatimTextOutput("click_info")
                             ),
                             column(width = 6,
                                    h4("Brushed points"),
                                    verbatimTextOutput("brush_info")
                             )
                           )
                           
                  ),
                  tabPanel("Statistics Summary", verbatimTextOutput("summary")),
                  tabPanel("Depreciation trend",
                           plotOutput("plot2", height = 300,
                                      # Equivalent to: click = clickOpts(id = "plot_click")
                                      
                           ))
      )
    )
  )
  
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output,session) {
  
  # import data
  audi = read.csv("Data/audi.csv")
  bmw = read.csv("Data/bmw.csv")
  mercedes = read.csv("Data/mercedes_benz.csv")
  porsche = read.csv("Data/porsche.csv")
  volkswagen = read.csv("Data/volkswagen.csv")
  
  # data cleaning
  car.clean = function(car) {
    car[,3] = as.numeric(str_remove(substring(car[,3], 2),","))
    car[,4] = as.numeric(str_remove(str_remove(car[,4],",")," mi."))
    car = na.omit(car)
    
    # split year and make
    car.col.2 = car[,2]
    car.col.2 = as.data.frame(car.col.2)
    car.col.2.temp = car.col.2 %>%
      mutate(Year = str_extract(car.col.2, "^[^\\s]+"),
             Brand = str_extract(car.col.2, "[\\s]+([^\\s]+)"),
             Make = str_remove(car.col.2, '\\w+\\s\\w+\\s')) %>%
      select(-car.col.2)
    car = cbind(car.col.2.temp,car)
    car = car[,-c(4,5)] 
    car$Brand = trim(car$Brand)
    car$Mileage = car$Mileage/1000
    car$Price = car$Price/1000
    car
  }
  
  # cleaned data
  suppressWarnings({
    
    audi.clean = car.clean(audi)
    audi.clean <- transform(audi.clean, Type = ifelse(grepl("Premium", Make), "Premium", ifelse(grepl("Prestige", Make), "Prestige", "Other")))
    audi.clean$make = audi.clean$Type
    bmw.clean = car.clean(bmw)
    bmw.clean <- transform(bmw.clean, Type = ifelse(grepl("xDrive", Make), "xDrive", ifelse(grepl("sDrive", Make), "sDrive", "Other")))
    bmw.clean <- transform(bmw.clean, Style = ifelse(grepl("X3", Make), "X3", ifelse(grepl("X4", Make), "X4",  ifelse(grepl("X5", Make), "X5", ifelse(grepl("X6", Make), "X6",  ifelse(grepl("X7", Make), "X7", "Other"))))))
    bmw.clean$make = bmw.clean$Type
    mercedes.clean = car.clean(mercedes)
    mercedes.clean <- transform(mercedes.clean, Type = ifelse(grepl("4MATIC", Make), "4MATIC", "Other"))
    mercedes.clean <- transform(mercedes.clean, Drive = ifelse(grepl("Base", Make), "Base", "Not Base"))
    mercedes.clean <- transform(mercedes.clean, Style = ifelse(grepl("S-", Make) | grepl("-S", Make), "S class", ifelse(grepl("C-", Make) | grepl("-C", Make), "C class", ifelse(grepl("E-", Make) | grepl("-E", Make), "E class", ifelse(grepl("G-", Make) | grepl("-G", Make), "G class", "Other")))))
    head(mercedes.clean, 50)
    mercedes.clean$make = mercedes.clean$Type
    porsche.clean = car.clean(porsche)
    porsche.clean <- transform(porsche.clean, Style = ifelse(grepl("Cayenne", Make), "Cayenne", ifelse(grepl("Panamera", Make), "Panamera", ifelse(grepl("911", Make), "911", ifelse(grepl("Carrera", Make), "Carrera", ifelse(grepl("Cayman", Make), "Cayman", ifelse(grepl("Macan", Make), "Macan", ifelse(grepl("Boxster", Make), "Boxster", "Other"))))))))
    porsche.clean$make = porsche.clean$Style
    mercedes.clean$make = mercedes.clean$Style
    volkswagen.clean = car.clean(volkswagen)
    volkswagen.clean <- transform(volkswagen.clean, Style = ifelse(grepl("Atlas", Make), "Atlas", ifelse(grepl("Beetle", Make), "Beetle", ifelse(grepl("Golf", Make), "Golf", ifelse(grepl("Jetta", Make), "Jetta", ifelse(grepl("Touareg", Make), "Touareg", ifelse(grepl("Passat", Make), "Passat", ifelse(grepl("Tiguan", Make), "Tiguan", "Other"))))))))
    volkswagen.clean$make = volkswagen.clean$Style
    
    
    audi.clean <-transform(audi.clean,Years_Old = 2022 - as.numeric(Year))
    bmw.clean <-transform(bmw.clean,Years_Old = 2022 - as.numeric(Year))
    mercedes.clean <-transform(mercedes.clean,Years_Old = 2022 - as.numeric(Year))
    porsche.clean <-transform(porsche.clean,Years_Old = 2022 - as.numeric(Year))
    volkswagen.clean <-transform(volkswagen.clean,Years_Old = 2022 - as.numeric(Year))
    
    
    audi.clean.g = audi.clean %>%
      group_by(make,Year) %>%
      summarise(Price = mean(Price),
                Mileage = mean(Mileage),
                Age = mean(Years_Old))
    
    bmw.clean.g = bmw.clean %>%
      group_by(make,Year) %>%
      summarise(Price = mean(Price),
                Mileage = mean(Mileage),
                Age = mean(Years_Old))
    
    mercedes.clean.g = mercedes.clean %>%
      group_by(make,Year) %>%
      summarise(Price = mean(Price),
                Mileage = mean(Mileage),
                Age = mean(Years_Old))
    
    porsche.clean.g = porsche.clean %>%
      group_by(make,Year) %>%
      summarise(Price = mean(Price),
                Mileage = mean(Mileage),
                Age = mean(Years_Old))
    
    volkswagen.clean.g = volkswagen.clean %>%
      group_by(make,Year) %>%
      summarise(Price = mean(Price),
                Mileage = mean(Mileage),
                Age = mean(Years_Old))
    
    
    # all.clean = rbind(audi.clean,bmw.clean, mercedes.clean, porsche.clean, volkswagen.clean)
    
  })
  
  
  # Return the requested dataset ----
  
  datasetInput <- reactive( {
    switch(input$dataset,
           "Audi" = audi.clean, 
           "BMW" = bmw.clean, 
           "Mercedes-Benz" = mercedes.clean,
           "Porsche" = porsche.clean,
           "Volkswagen" = volkswagen.clean,
           
    )
  })
  
  
  # datasetInput.g <- reactive({
  #   switch(input$dataset,
  #          "Audi" = audi.clean.g, 
  #          "BMW" = bmw.clean.g, 
  #          "Mercedes-Benz" = mercedes.clean.g,
  #          "Porsche" = porsche.clean.g,
  #          "Volkswagen" = volkswagen.clean.g,
  #          
  #   )
  # })
  
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    data = datasetInput()[datasetInput()$Year == input$Year & datasetInput()$make == input$make,]
    dataset <- data[,c("Price","Mileage")]
    summary(dataset)
  })
  
  # Generate a plot of the dataset ----
  output$plot1 <- renderPlot({
    data = datasetInput()[datasetInput()$Year == input$Year & datasetInput()$make == input$make,]
    ggplot(data,aes(x=Mileage,y=Price,color = make)) +
      geom_point() +
      xlab("Mileage(per 1000 mile)") +
      ylab("Price(per $1000)")+
      ggtitle(paste("Visualization for",input$dataset,input$make,"in",input$Year))
  })
  
  # depreciation plot
  output$plot2 <- renderPlot({
    data = datasetInput() %>%
      filter(make == input$make) %>%
      group_by(make,Year) %>%
      summarise(Price = mean(Price),
                Mileage = mean(Mileage),
                Age = mean(Years_Old))
    
    ggplot(data,aes(x=Age,y=Price, color=make)) +
      geom_line()+
      ggtitle(paste("Depreciation trend for",input$dataset))
  })
  
  output$click_info <- renderPrint({
    data0 = datasetInput()[datasetInput()$Year == input$Year & datasetInput()$make == input$make,]
    data = data0[,c("Make","Mileage","Price")]
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    nearPoints(data, input$plot1_click)
  })
  
  output$brush_info <- renderPrint({
    data0 = datasetInput()[datasetInput()$Year == input$Year & datasetInput()$make == input$make,]
    data = data0[,c("Make","Mileage","Price")]
    brushedPoints(data, input$plot1_brush)
  })
  
  
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  # update make based on brand
  
  observeEvent(input$dataset,
               {
                 updateSelectizeInput(session, inputId = "make",
                                      choices = levels(as.factor(datasetInput()$make)),
                                      "Make")
               })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)