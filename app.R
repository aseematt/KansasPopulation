#

library(shiny)


data <- read.csv("C:\\Users\\WHS_c\\Downloads\\824-Data Visualization\\KansasPopulation.csv")

library(shiny)
library(ggplot2)
library(maps)
library(dplyr)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Kansas Population Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("population_range", "Select Population Range:",
                  choices = c("Less than 10,000" = "<10000",
                              "10,001 to 50,000" = "10001-50000",
                              "50,001 to 90,000" = "50001-90000",
                              "Greater than 90,001" = ">90001")),
      uiOutput("county_ui"),
      helpText("You can select up to 3 counties."),
      actionButton("update", "Update Plot")
    ),
    mainPanel(
      plotOutput("plot"),
      plotOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  filtered_data <- reactive({
    req(input$population_range)
    if (input$population_range == "<10000") {
      subset(data, Population < 10000)
    } else if (input$population_range == "10001-50000") {
      subset(data, Population >= 10001 & Population <= 50000)
    } else if (input$population_range == "50001-90000") {
      subset(data, Population >= 50001 & Population <= 90000)
    } else {
      subset(data, Population > 90001)
    }
  })
  
  output$county_ui <- renderUI({
    selectInput("counties", "Select Counties:", 
                choices = unique(filtered_data()$County), 
                selected = unique(filtered_data()$County)[1], 
                multiple = TRUE, 
                selectize = TRUE)
  })
  
  selected_data <- reactive({
    req(input$counties)
    validate(
      need(length(input$counties) <= 3, "Please select up to 3 counties.")
    )
    subset(data, County %in% input$counties)
  })
  
  output$plot <- renderPlot({
    input$update  # Trigger reactivity on button click
    isolate({
      ggplot(selected_data(), aes(x = Year, y = Population, color = County)) +
        geom_line() +
        geom_point() +
        labs(title = "Population of Selected Counties (2010-2018)",
             x = "Year", y = "Population") +
        theme_minimal()
    })
  })
  
  output$map <- renderPlot({
    input$update  # Trigger reactivity on button click
    isolate({
      kansas_map <- map_data("county", "kansas")
      selected_counties <- tolower(input$counties)
      kansas_map$highlight <- ifelse(kansas_map$subregion %in% selected_counties, "Selected", "Not Selected")
      
      ggplot(kansas_map, aes(x = long, y = lat, group = group, fill = highlight)) +
        geom_polygon(color = "black") +
        scale_fill_manual(values = c("Selected" = "blue", "Not Selected" = "grey")) +
        labs(title = "Selected Counties in Kansas") +
        theme_void()
    })
  })
}
# Run the application 
shinyApp(ui = ui, server = server)