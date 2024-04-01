library(shiny)
library(ggplot2)
library(lubridate)
library(dplyr)
library(googlesheets4)


x <- read_sheet('https://docs.google.com/spreadsheets/d/1tGFHHICtguU5_S_QuCBpW0awQIGF-X7gY30j0URRlok/edit#gid=864982580')

#Cleaning
x$Date_of_Birth <- as.Date(x$`Date of Birth`, format = "%Y-%m-%d")
x$Date_of_Death <- as.Date(x$`Date of Death`, format = "%Y-%m-%d")

x <- x[!is.na(x$Date_of_Birth) & !is.na(x$Date_of_Death),]

x$Age_at_Death <- as.integer(format(x$Date_of_Death, "%Y")) - as.integer(format(x$Date_of_Birth, "%Y"))

x <- x %>% filter(Age_at_Death <= 100)

x$Birthplace <- as.character(x$Birthplace)
x$Residence <- as.character(x$Residence)
x <- x %>%
  filter(!grepl("\\.\\.\\.|\\?| '|---", Birthplace))
x <- x %>%
  filter(!grepl("\\.\\.\\.|\\?| '|---", Residence))

x <- x %>%
  mutate(YearMonth = format(Date_of_Death, "%Y-%m"))
x <- x %>%
  mutate(YearMonth = as.Date(paste0(YearMonth, "-01")))

#Build graphs
ui <- fluidPage(
  titlePanel("Histogram of Monthly Deaths"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("birthplaceInput", "Choose a Birthplace:", 
                  choices = c('All', sort(unique(x$Birthplace)))),
      selectInput("residenceInput", "Choose a Residence:", 
                  choices = c('All', sort(unique(x$Residence)))), # 添加了sort()函数
      selectInput("religionInput", "Choose a Religion:", 
                  choices = c('All', sort(unique(x$Religion)))), # 添加了sort()函数
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

# Server逻辑
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
      # Assuming YearMonth is in the format "YYYY-MM"
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

# 运行应用
shinyApp(ui = ui, server = server)
