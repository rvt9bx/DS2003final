# Load necessary libraries
library(shiny)
library(ggplot2)
library(plotly)

# Load your dataset and preprocess
data <- read.csv("socioeconomic_voting.csv")
data <- data[ , !names(data) %in% "FIPS"] # Remove the FIPS column

# Change Column Names
colnames(data)[colnames(data) == 'County.Name'] <- 'County'
colnames(data)[colnames(data) == 'Unemployment.Rate.2020'] <- 'Unemployment_Rate'
colnames(data)[colnames(data) == 'County.Median.Household.Income..2021.'] <- 'Median_Household_Income'
colnames(data)[colnames(data) == 'County.Income.Percentile.Within.State..2021.'] <- 'Income_Percentile'
colnames(data)[colnames(data) == 'Bachelor.s.Degree.or.Higher.Percentage..2018.2022.'] <- 'College_Educated_Percent'
colnames(data)[colnames(data) == 'Vote.Percentage'] <- 'Voter_Turnout_Percent'
colnames(data)[colnames(data) == 'Urban.Influence.Code.2013'] <- 'Urban_Influence'

# UI
ui <- fluidPage(
 
  titlePanel("US Voting Trends: Factors Influencing Turnout in 2020"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select State:",
                  choices = c("All", unique(data$State)), 
                  selected = "VIRGINIA"), # Default selection is ALABAMA
      selectInput("xvar", "Select X-axis Variable:",
                  choices = names(data)[sapply(data, is.numeric)],
                  selected = "College_Educated_Percent"),
      selectInput("yvar", "Select Y-axis Variable:",
                  choices = names(data)[sapply(data, is.numeric)],
                  "Voter_Turnout_Percent"),
      selectInput("sizevar", "Select Size Variable:",
                  choices = c("None", names(data)[sapply(data, is.numeric)]),
                  selected = "None") # Default size variable is "None"
    ),
    mainPanel(
      plotlyOutput("scatterPlot"),
      p("This app displays a scatterplot, which can be used to look at the correlation between several different variables in the data, including 
        Income, Education, Voter Turnout, Unemployment, and Urban Influence. The plot displays county results by state, and the state of interest can be
        selected on the left side menu. Each dot on the scatterplot also shows if the county was won by the Republican or Democratic
        party in the 2020 Presidential Election.")
    )
  )
)

# Server
server <- function(input, output) {
  # Filter the dataset based on the selected state
  filteredData <- reactive({
    if (input$state == "All") {
      data
    } else {
      subset(data, State == input$state)
    }
  })
  
  # Generate the scatter plot
  output$scatterPlot <- renderPlotly({
    plotData <- filteredData()
    
    # Create the scatter plot
    p <- ggplot(plotData, aes_string(
      x = input$xvar, 
      y = input$yvar, 
      size = if (input$sizevar == "None") "NULL" else input$sizevar, # Dynamically set size
      color = "Party", # Color by PARTY
      text = "County"
    )) +
      geom_point(alpha = 0.9) + # Size controlled by aes
      scale_size_continuous(range = c(0.5, 3.5)) + # Pronounced size range
      scale_color_manual(values = c("REPUBLICAN" = "red", "DEMOCRAT" = "blue")) +
      labs(title = "2020 Election Data by State",
           x = input$xvar, y = input$yvar, size = input$sizevar, color = "Party") +
      theme_minimal()
    
    # Convert ggplot to ggplotly for interactivity
    ggplotly(p, tooltip = c("x", "y", "size", "color", "text"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
