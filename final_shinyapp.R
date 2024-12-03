
# shiny app for final 
# socioeconomic voting data shiny app 
# Eleanor Byrd rvt9bx 11/2024

# load libraries
library(tidyverse)
library(shiny)
library(shinythemes)
library(stringr)
library(plotly)
library(bslib)
library(maps)

# import data 
vote = read.csv("socioeconomic_voting.csv") 
ga_2024 = read.csv("clean_ga_results_2024.csv")

# clean up column names
vote = vote%>%rename('County'='County.Name', 
                     'fips'='FIPS', 
                     'Unenemployment Rate'='Unemployment.Rate.2020', 
                     'Median Income'='County.Median.Household.Income..2021.',
                     "Bachelor's Degree or Higher Percentage"='Bachelor.s.Degree.or.Higher.Percentage..2018.2022.', 
                     'Vote Percentage'='Vote.Percentage', 
                     'Urban Influence'='Urban.Influence.Code.2013', 
                     'County Income Percentile'='County.Income.Percentile.Within.State..2021.') 
ga_2024 = ga_2024%>%rename('Majority Percentage'='Vote.Percentage', 
                           'Vote Percentage'='Turnout.Percentage')%>%select(-X)

# subset georgia 
ga_2020 = vote%>%filter(State=="GEORGIA")

# import map data 
ga_counties <- map_data("county", "georgia")%>%select(lon = long, lat, group, County = subregion)

# join map data with ga_2020 and ga_2024 data 
ga_2024 = ga_2024%>%
  mutate(County = gsub(" County", "", County),
         Party = as.numeric(ifelse(grepl("DEMOCRAT",Party), 
                                   0,
                                   ifelse(grepl("REPUBLICAN",Party),
                                          1,
                                          Party
                                   ))),
         `Majority Percentage` = 100*`Majority Percentage`)
ga_counties = ga_counties%>%
  mutate(County = str_to_title(County))
ga_2020 = ga_2020%>%
  mutate(County = str_to_title(County),
         Party = as.numeric(ifelse(grepl("DEMOCRAT",Party), 
                                   0,
                                   ifelse(grepl("REPUBLICAN",Party),
                                          1,
                                          Party
                                   ))))
ga_2024_map = full_join(ga_2024, ga_counties, by="County")
ga_2020_map = full_join(ga_2020, ga_counties, by="County")

combined_df = bind_rows(ga_2020_map, ga_2024_map)

# make ui
ui = fluidPage(
  theme = shinytheme("superhero"),
  
  titlePanel("US Voting Trends: Comparison for Georgia, 2020 vs 2024"),
  
  sidebarLayout(
    sidebarPanel(
      input_switch(
        "plot_choice",
        "Check to display all counties",
        value=TRUE
      ),
      selectInput(
        "fillvar2020",
        "Select a variable for 2020 data:",
        choices = c("Unenemployment Rate","Median Income","Urban Influence",
                    "County Income Percentile","Bachelor's Degree or Higher Percentage",
                    "Vote Percentage", "Party"
                    ),
        selected="Party"
        ),
      selectInput(
        "fillvar2024",
        "Select a variable for 2024 data:",
        choices = c("Majority Percentage", "Party", "Vote Percentage"
                    ),
        selected="Party"
      ),
      selectInput(
        "county_pick",
        "Select a county:",
        choices = unique(ga_counties$County)
        ),
      tags$b("This dashboard provides the viewer with the ability to explore election 
      results from 2024 and 2020, as well as socioeconomic trends from around 2020."),
      p("Use the check box to toggle between viewing all counties in Georgia and zooming to a specific county, 
      chosen with the county dropdown. 
      When visualizing the 'Party' variable, 0 (blue) corresponds to majority Democratic 
      and 1 (red) corresponds to majority Republican. It is important to keep in mind that 
      the 'Majority Percentage' variable in the 2024 data represents the percent of votes that the winning party received,
      while 'Vote Percentage' in both 2020 and 2024 represents the turnout, or the percent of the eligible population that voted."),
      tags$div(
        "Data sourced from ",
        tags$a(href="https://www.kaggle.com/datasets/adamcuculich/county-socioeconomic-education-and-voting-data", 
               "Kaggle"),
        " and ",
        tags$a(href="https://app.enhancedvoting.com/results/public/appling-county-ga/elections/2024NovGen", 
               "enhancedvoting.com"),
        "."
      )
      ),
    mainPanel(
      plotlyOutput("map2020"),
      plotlyOutput("map2024")
      )
    )
  )

# make server
server = function(input, output) {
  
  ga_2020_subset = reactive({
    req(input$county_pick)
    filter(ga_2020_map, County %in% input$county_pick)
  })
  
  ga_2024_subset = reactive({
    req(input$county_pick)
    filter(ga_2024_map, County %in% input$county_pick)
  })
  
  fillvar2020_df = reactive({
    req(input$fillvar2020)
    select(combined_df, input$fillvar2020)
  })
  
  fillvar2024_df = reactive({
    req(input$fillvar2024)
    select(combined_df, input$fillvar2024)
  })
  
  
  # make 2020 per county map 
  map_2020_county = reactive({
    ggplotly(ggplot(ga_2020_subset(), aes(x=lon, y=lat, group=group, fill=.data[[input$fillvar2020]]))+
               geom_polygon(color="black")+
               coord_quickmap()+
               scale_fill_gradient(low="blue",high="red", limits=c(min(fillvar2020_df(),na.rm=T), max(fillvar2020_df(),na.rm=T)))+
               labs(title=paste0("2020 Data per County: ", input$county_pick))+
               theme_void()
    )
  })
  
  # make 2024 per county map
  map_2024_county = reactive({
    ggplotly(ggplot(ga_2024_subset(), aes(x=lon, y=lat, group=group, fill=.data[[input$fillvar2024]]))+
               geom_polygon(color="black")+
               coord_quickmap()+
               scale_fill_gradient(low="blue",high="red", limits=c(min(fillvar2024_df(),na.rm=T), max(fillvar2024_df(),na.rm=T)))+
               labs(title=paste0("2024 Data per County: ", input$county_pick))+
               theme_void())
  })
  
  # make 2020 whole state map
  map_2020_state = reactive({
    ggplotly(ggplot(ga_2020_map, aes(x=lon, y=lat, group=group, fill=.data[[input$fillvar2020]], label=County))+
               geom_polygon(color="black")+
               coord_quickmap()+
               scale_fill_gradient(low="blue",high="red",limits=c(min(fillvar2020_df(),na.rm=T), max(fillvar2020_df(),na.rm=T)))+
               labs(title="2020 Data for State")+
               theme_void())
  })
  
  # make 2024 whole state map
  map_2024_state = reactive({
    ggplotly(ggplot(ga_2024_map, aes(x=lon, y=lat, group=group, fill=.data[[input$fillvar2024]], label=County))+
               geom_polygon(color="black")+
               coord_quickmap()+
               scale_fill_gradient(low="blue",high="red", limits=c(min(fillvar2024_df(),na.rm=T), max(fillvar2024_df(),na.rm=T)))+
               labs(title="2024 Data for State")+
               theme_void())
  })
  
  # reactive functions to show county or whole state maps, party or socioecon
  
  which_graph_2020 = reactive({
    if (input$plot_choice == TRUE) {return(map_2020_state())}
    if (input$plot_choice == FALSE) {return(map_2020_county())}
  })
  
  which_graph_2024 <- reactive({
    if (input$plot_choice == FALSE) return(map_2024_county())
    if (input$plot_choice == TRUE) return(map_2024_state())
  })
  
  output$map2020 <- renderPlotly({   
    which_graph_2020()
  })
    
  output$map2024 <- renderPlotly({   
      which_graph_2024()
  })
  
}
  
  
  

# run app
shinyApp(ui, server)

