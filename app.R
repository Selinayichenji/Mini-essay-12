library(shiny)
library(ggplot2)
library(lubridate)
library(dplyr)
library(googlesheets4)
library(readr)


x <- read_csv("death_certificate.csv")


#Build graphs
ui <- fluidPage(
  titlePanel("Histogram of Monthly Deaths"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("birthplaceInput", "Choose a Birthplace:", 
                  choices = c('All', sort(unique(x$Birthplace)))),
      selectInput("residenceInput", "Choose a Residence:", 
                  choices = c('All', sort(unique(x$Residence)))), 
      selectInput("religionInput", "Choose a Religion:", 
                  choices = c('All', sort(unique(x$Religion)))), 
      sliderInput("ageSlider", "Select Age Range:",
                  min = min(x$Age_at_Death, na.rm = TRUE), 
                  max = max(x$Age_at_Death, na.rm = TRUE), 
                  value = c(min(x$Age_at_Death, na.rm = TRUE), max(x$Age_at_Death, na.rm = TRUE)))
    ),
    
    mainPanel(
      plotOutput("histPlot")
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    # Start with the full dataset
    subset_x <- x
    
    # Filter by Birthplace if not "All"
    if (input$birthplaceInput != "All") {
      subset_x <- subset_x[subset_x$Birthplace == input$birthplaceInput, ]
    }
    
    # Filter by Residence if not "All"
    if (input$residenceInput != "All") {
      subset_x <- subset_x[subset_x$Residence == input$residenceInput, ]
    }
    
    # Filter by Religion if not "All"
    if (input$religionInput != "All") {
      subset_x <- subset_x[subset_x$Religion == input$religionInput, ]
    }
    
    # Filter by Age Range using the slider input
    subset_x <- subset_x[subset_x$Age_at_Death >= input$ageSlider[1] & subset_x$Age_at_Death <= input$ageSlider[2], ]
    
    subset_x %>%
      group_by(YearMonth) %>%
      summarise(Count = n()) %>%
      arrange(YearMonth)
  })
  
  output$histPlot <- renderPlot({
    # Ensure that filtered data is not empty
    if (nrow(filtered_data()) > 0) {
      ggplot(filtered_data(), aes(x = YearMonth, y = Count)) +
        geom_col() +
        scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
        labs(x = "Month", y = "Number of Deaths", title = "Monthly Deaths") +
        theme(axis.text.x = element_text(angle=90, hjust=1))
    } else {
      ggplot() + 
        labs(x = "Month", y = "Number of Deaths", title = "No Data for Selected Criteria")
    }
  })
}

shinyApp(ui = ui, server = server)
