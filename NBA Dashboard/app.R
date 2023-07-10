#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#####ui######
library(shiny)
library(shinyWidgets)
library(leaflet)
library(tidyverse)
library(ggplot2)  # for the dataset
library(DataCombine)
library(httr)
library(jsonlite)
library(dplyr)
library(DT)
library(shinycssloaders)
library(dplyr)
library(shinythemes)

#import team data
get_team_url <- 'https://www.balldontlie.io/api/v1/teams'
team_api_call <- GET(get_team_url)
all_team_data <- fromJSON(base::rawToChar(team_api_call$content))$data


#add latitude and longitude
library(tmaptools)
library(sf)
library(leaflet)
cities <- all_team_data$city
cities_new <- geocode_OSM(cities)
cities_new<-rename(cities_new,city = query)
nbateams_dataset <-inner_join(all_team_data,cities_new,by = "city")
start <- "<center></br><b>"
sb <- "</b>"
b <- "<b>"
sbr <- "</br>"
nbateams_dataset <- nbateams_dataset %>% mutate(popup = paste0(start, "Full Name: ", sb, full_name, sbr,
                                                               b, "Abbreviation: ", sb, abbreviation, sbr,
                                                               b, "City: ", sb, city, sbr,
                                                               b, "Conference: ", sb, conference, sbr,
                                                               b, "Division: ", sb, division, sbr, "</center>"))

#ADD LOGO for example -> https://github.com/AllezCannes/WorldCupSquads/blob/master/app.R
teamLOGO <- makeIcon(
  iconUrl = case_when(
    nbateams_dataset$full_name == "Atlanta Hawks" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Boston Celtics" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Brooklyn Nets" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Charlotte Hornets" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Chicago Bulls" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Cleveland Cavaliers" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Dallas Mavericks" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Denver Nuggets" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Detroit Pistons" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Golden State Warriors" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Houston Rockets" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Indiana Pacers" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "LA Clippers" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Los Angeles Lakers" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Memphis Grizzlies" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Miami Heat" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Milwaukee Bucks" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Minnesota Timberwolves" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "New Orleans Pelicans" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "New York Knicks" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Oklahoma City Thunder" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Orlando Magic" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Philadelphia 76ers" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Phoenix Suns" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Portland Trail Blazers" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Sacramento Kings" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "San Antonio Spurs" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Toronto Raptors" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Utah Jazz" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png"),
    nbateams_dataset$full_name == "Washington Wizards" ~ paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(nbateams_dataset$full_name, " ", "%20"),".png")),
  iconWidth = 30, iconHeight = 30,
  shadowWidth = 10, shadowHeight = 10)

ui<- navbarPage(title = "NBA Teams", id="nav", theme = shinytheme("flatly"),
                
                tabPanel("Interactive Map",
                         div(class="outer",
                             
                             leafletOutput("map", width="1300", height="550"),
                             absolutePanel(top = 80, right = 30,
                                           pickerInput(inputId = "team_name1", 
                                                       label = "Select a team:",
                                                       choices = c("All NBA Teams", 
                                                                   nbateams_dataset$full_name))))),
                tabPanel("Team Stat",
                         fluidPage(
                           sidebarLayout(
                             sidebarPanel(
                               selectInput(inputId = "team_name",
                                           label = "Select a team:",
                                           choices = all_team_data$ full_name),
                               hr(),
                               sliderTextInput("Seasons",
                                           "Select seasons:",
                                           choices =  paste(as.character(as.list(1979:2022)),"-",as.character(as.list(1980:2023))),
                                           selected = paste(as.character(as.list(1979:2022)),"-",as.character(as.list(1980:2023)))[c(22,23)]),
                               br(),
                               actionButton(inputId = "refresh", 
                                            label  = "Refresh Data"),
                               br(),
                               br(),
                               tags$div(id="cite",
                                        '** Rate limit of 60 requests per minute')
                             ),
                             mainPanel(
                               fluidRow(
                                 column(width = 12,
                                        h3("Team Statistics"),
                                        shinycssloaders::withSpinner(
                                          DT::dataTableOutput("summary_table")
                                        )
                                 )
                               )
                               ,
                               h3("Win Rate by Season"),
                               shinycssloaders::withSpinner(
                                 plotOutput("win_rate")
                               ),
                               h3("Average Points by Season"),
                               shinycssloaders::withSpinner(
                                 plotOutput("average_score")
                               )
                             )))),
                tabPanel("Teams Comparison",
                         fluidPage(
                           sidebarLayout(
                             sidebarPanel(
                               selectInput(inputId = "team_name2",
                                           label = "Select multiple teams:",
                                           choices = all_team_data$ full_name,
                                           multiple = TRUE,
                                           selected = all_team_data$ full_name[1:2]),
                               hr(),
                               selectInput(inputId = "Seasons2",
                                           label = "Select a season:",
                                           choices = paste(as.character(as.list(1979:2022)),"-",as.character(as.list(1980:2023))),
                                           selected = "2021-2022"),
                               br(),
                               actionButton(inputId = "refresh2", 
                                            label  = "Refresh Data"),
                               br(),
                               br(),
                               tags$div(id="cite",
                                        '** Rate limit of 60 requests per minute')
                             ),
                             mainPanel(
                               column(width = 12),
                               h3("Point per Game by Teams"),
                               shinycssloaders::withSpinner(
                                 plotOutput("box_plot")
                               )
                             ))))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  filteredData <- reactive({
    if (input$team_name1 == "All NBA Teams") {
      nbateams_dataset
    } else {
      filter(nbateams_dataset, full_name == input$team_name1)
    }
  })
  filteredIcon <- reactive({
    if (input$team_name1 == "All NBA Teams") {
      teamLOGO
    } else {
      teamLOGO$iconUrl <- paste0("https://raw.githubusercontent.com/wanchai2/NBAlogo/main/",str_replace_all(input$team_name1, " ", "%20"),".png")
    }
    teamLOGO
  })
  output$map <- renderLeaflet({
    leaflet(filteredData()) %>%
      addTiles() %>%
      addMarkers(~lon,
                 ~lat,
                 icon = filteredIcon(),
                 label = ~full_name,
                 labelOptions = labelOptions(textsize = "12px"),
                 popup = ~popup
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
      setMaxBounds(lng1 = -70
                   , lat1 = 25
                   , lng2 = -130
                   , lat2 = 49)
  })
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addMarkers(~lon, 
                 ~lat,
                 icon = filteredIcon(),
                 label = ~full_name, 
                 labelOptions = labelOptions(textsize = "12px"),
                 popup = ~popup)
  })
  #get match function
  get_match <- function(team_name = "Atlanta Hawks", start_season = 2000, end_season = 2001) {
    team_id = as.numeric(all_team_data[all_team_data$full_name==team_name, 1])
    match_data <- data.frame()
    for (i in start_season:end_season) {
      match_url <- paste0("https://www.balldontlie.io/api/v1/games?seasons[]=", i, "&team_ids[]=", team_id,"&per_page=100")
      match_api_call <- GET(match_url)
      match_data_i <- data.frame(fromJSON(base::rawToChar(match_api_call$content), simplifyVector = TRUE,
                                          flatten = FALSE)$data)
      match_data <- bind_rows(match_data, match_data_i)
    }
    final_data <- match_data %>% mutate(focus_team_id	= team_id,
                                        focus_team_abb = ifelse(home_team$id==team_id, home_team$abbreviation, visitor_team$abbreviation),
                                        focus_team_score = ifelse(home_team$id==team_id, home_team_score, visitor_team_score),
                                        opponent_team_id = ifelse(home_team$id==team_id, visitor_team$id, home_team$id),
                                        opponent_team_abb =	ifelse(home_team$id==team_id, visitor_team$abbreviation, home_team$abbreviation),
                                        opponent_team_score	= ifelse(home_team$id==team_id, visitor_team_score, home_team_score),
                                        score_diff = focus_team_score - opponent_team_score,
                                        win_flag = ifelse(score_diff>0, "Win", ifelse(score_diff<0, "Lose", "Draw"))) %>%
      select(id, date, season, focus_team_id, focus_team_abb, focus_team_score, opponent_team_id, opponent_team_abb, opponent_team_score, score_diff, win_flag)
    return(final_data)
  }
  RV <- reactiveValues(data = get_match())
  
  # Recall API for new data
  observeEvent(input$refresh, {
    RV$data <- get_match(input$team_name, as.numeric(substr(input$Seasons[1],1,4)), as.numeric(substr(input$Seasons[2],1,4))) 
  }) 
  
  output$summary_table <-  DT::renderDataTable({
    input$refresh
    by_win_flag <- RV$data %>% 
      group_by(win_flag) %>%
      summarize(`Matches Played` = n(),
                `Percent to MP` = n()/nrow(RV$data),
                `Max Point` = max(focus_team_score),
                `Avg Point` = mean(focus_team_score),
                `Min Point` = min(focus_team_score),
                `Avg Point Difference` = mean(score_diff)) %>%
      arrange(desc(win_flag))
    total <- RV$data %>% 
      summarize(win_flag = "Total",
                `Matches Played` = n(),
                `Percent to MP` = n()/nrow(RV$data),
                `Max Point` = max(focus_team_score),
                `Avg Point` = mean(focus_team_score),
                `Min Point` = min(focus_team_score),
                `Avg Point Difference` = mean(score_diff))
    final_data <- bind_rows(by_win_flag, total) %>% rename("Result" = "win_flag") 
    final_data <- datatable(final_data, options = list(searching = FALSE, dom = 'ft')) %>% formatPercentage("Percent to MP", 2) %>% formatRound(c("Avg Point", "Avg Point Difference"), 2)
  })
  
  output$win_rate <-  renderPlot({
    input$refresh
    b <- RV$data %>% 
      group_by(win_flag, season) %>%
      summarize(count = n(), .groups = "keep") %>%
      group_by(season) %>%
      mutate(pct= prop.table(count)*100)
    ggplot(b, aes(fill=win_flag, y=pct, x=as.factor(paste(season,"-",season+1)))) + 
      geom_bar(stat="identity") +
      labs(x= "by Season" ,y = "Percentage") + 
      geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
                position=position_stack(vjust=0.5)) +
      scale_fill_manual("Result", values = c("Win" = "darkolivegreen2", "Lose" = "coral1", "Draw" = "burlywood1")) +
      scale_x_discrete("season") +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      theme(axis.text = element_text(size = 12), axis.title = element_text(size=15), panel.background = element_blank(), panel.grid = element_line(color="gray", linetype=3))  
  })
  
  output$average_score <-  renderPlot({
    input$refresh
    c <- RV$data %>% 
      group_by(season) %>%
      summarize(average_score = mean(focus_team_score), .groups = "keep")
    ggplot(data=c, aes(x=as.factor(paste(season,"-",season+1)), y=average_score, group = 1)) +
      geom_line(color="coral1") +
      geom_point(color="coral4") +
      labs(x= "by Season" ,y = "Average Points") +
      scale_x_discrete("season") +
      theme(axis.text = element_text(size = 12), axis.title = element_text(size=15), panel.background = element_blank(), panel.grid = element_line(color="gray", linetype=3)) +
      geom_text(aes(y=average_score+0.1, label = round(average_score, digits = 2)), vjust = 0)
  })
  
  get_match_compare <- function(team_name = c("Atlanta Hawks", "Boston Celtics"), season = 2021) {
    match_data <- data.frame()
    for (i in team_name) {
      team_id = as.numeric(all_team_data[all_team_data$full_name==i, 1])
      match_url <- paste0("https://www.balldontlie.io/api/v1/games?seasons[]=", season, "&team_ids[]=", team_id,"&per_page=100")
      match_api_call <- GET(match_url)
      match_data_i <- data.frame(fromJSON(base::rawToChar(match_api_call$content), simplifyVector = TRUE,
                                          flatten = FALSE)$data)
      match_data_i <- match_data_i %>% mutate(focus_team_id = team_id,
                                              focus_team_full_name = ifelse(home_team$id==team_id, home_team$full_name, visitor_team$full_name),
                                              focus_team_score = ifelse(home_team$id==team_id, home_team_score, visitor_team_score),
                                              opponent_team_id = ifelse(home_team$id==team_id, visitor_team$id, home_team$id),
                                              opponent_team_full_name = ifelse(home_team$id==team_id, visitor_team$full_name, home_team$full_name),
                                              opponent_team_score = ifelse(home_team$id==team_id, visitor_team_score, home_team_score),
                                              score_diff = focus_team_score - opponent_team_score,
                                              win_flag = ifelse(score_diff>0, "Win", ifelse(score_diff<0, "Lose", "Draw"))) %>%
        select(id, date, season, focus_team_id, focus_team_full_name, focus_team_score, opponent_team_id, opponent_team_full_name, opponent_team_score, score_diff, win_flag)
      match_data <- bind_rows(match_data, match_data_i)
    }
    return(match_data)
  }
  
  RV2 <- reactiveValues(data = get_match_compare())
  
  # Recall API for new data
  observeEvent(input$refresh2, {
    if(length(input$team_name2)>0) {
      RV2$data <- get_match_compare(as.vector(input$team_name2), as.numeric(substr(input$Seasons2,1,4)))
    }
    else{RV2$data <- data.frame()}
  })
  
  output$box_plot <-  renderPlot({
    input$refresh2
    if(length(RV2$data) == 0) {
      input$refresh2
      ggplot() + 
        labs(title="Please select at least one team.")+
        theme(panel.background = element_blank(),title = element_text(colour = "red", size=15))}
    else {
      input$refresh2
      ggplot(RV2$data, aes(x=focus_team_full_name, y=focus_team_score, color = focus_team_full_name)) +
        geom_boxplot() +
        theme(axis.text = element_text(size = 12), axis.title = element_text(size=15), panel.background = element_blank(), panel.grid = element_line(color="gray", linetype=3)) +
        labs(x= "by Teams" ,y = "Points per Game") +
        guides(color = guide_legend(title = "Teams"))
    }
  })
}
# Run the application 
shinyApp(ui = ui, server = server)