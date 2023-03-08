library(shiny)
library(rsconnect)
library(tidyverse)
library(ggplot2)
library(plotly)

colleges <- read_delim("colleges.csv")

# UI
ui <- navbarPage("PS6 Colleges Data Analysis", # add theme
                 
 tabPanel("Introduction Page",
          titlePanel("Average Cost of Undergrad College by State Dataset"),
              mainPanel(
              # INSERT IMAGE HERE
              h3("About the dataset:"),
              p("The data was found on Kaggle, a subsidary website of Google hosting a community and data for
                data scientists and ML engineers.The dataset was compiled by the National Center of Education Statistics Annual Digest."),
              p("The dataset contains 6 rows and 3548 columns. It also has no missing values and includes variables such as 
                year, state, college type, length, expenses, cost."),
              p("Overall the dataset looks reliable and good for analysis!"),
              h3("Questions to answer"),
              p(" - Where, on average, are the most and least expensive places to study?"),
              p(" - How much variation is there in the cost of tuition across the United States by state?"),
              p(" - How do private vs. public university tuition vary (especially by state)?"),
              p(" - How is the cost of undergraduate education related to poverty rates in specific states?"),
              h3("About the app:"),
              p("The app includes a plot that shows the cost of attending college of each U.S state each year from 
                2013-2021. The plot includes features filtering features such as year, type, length, etc."),
              p("The app also a reference to look at the raw dataset of the plot and allows you to 
                filter by selecting a range of years to look at the data.")
          )
 ),
 
 tabPanel("Cost of Attending College Plot",
    titlePanel("Cost Plot"),
    sidebarLayout(
      sidebarPanel(
        p("Select a year, expenses, and type of college to begin:"), 
        
        selectInput("n",
                    "Year",
                    unique(colleges$Year)
                    
        ),
      fluidRow(
        column(12,
               uiOutput("checkboxCut")
        ),
        column(12,
               uiOutput("expenseChoice")
        ),
        column(12,
               uiOutput("collegeLength")
          
        )),
             radioButtons("color", "Choose color:",
                          choices = c("skyblue", "lightgreen", "orangered",
                                               "magenta", "gold")),
      textOutput("filtersText")
      
      ),
    mainPanel(
      plotOutput("costPlot"),
      fluidRow(
        column(10,      
          p("Insert top 5 highest costing college by state")
      ), column(10, 
          p("Insert top 5 lowest costing college by state"))
      ),
    )
    )
 ), 
 
 tabPanel("Data Tables", "",
          sidebarLayout(
            sidebarPanel(
              radioButtons("dataChoice", "Dataset:",
                           choices = c("College Costs", "Household Income")),
              sliderInput("range", "Select year range",
                          min = min(colleges$Year),
                          max = max(colleges$Year),
                          value = c(2013, 2021)),
              textOutput("rangeText")
            ),
            mainPanel(
              dataTableOutput("dataTable")
            )
 )),
 tabPanel("Household Income Map", "Information about Map",
          sidebarLayout(
            sidebarPanel(
              p("Select a year to see the average household income:"),
              selectInput("mapYear",
                          "Year",
                          unique(hhIncome2013_21$year)
                          
              ),
              
            ),
            mainPanel(
              plotlyOutput("plotlyMap")
            )
          )
 ),
 tabPanel("Conclusion", 
   fluidRow(
     mainPanel(
       p("Insert information about conclusion"))
     )),
 
)

    
# SERVER
server <- function(input, output) {
  
  ## COST TAB
  output$checkboxCut <- renderUI({
    checkboxGroupInput("list", "Types of College:",
                       choices = unique(colleges$Type)
    )
  })
  
  output$expenseChoice <- renderUI ({
    checkboxGroupInput("expense", "Expenses:",
                       choices = unique(colleges$Expense))
  })
  
  output$collegeLength <- renderUI({
    checkboxGroupInput("length", "Length of College:",
                       choices = unique(colleges$Length))
  })
  
  sample <- reactive ({
    joe <- colleges %>% 
      filter(Type %in% input$list, Length %in% input$length, Expense %in% input$expense) 
    if(nrow(joe) > 1) 
      joe <- colleges %>% 
        filter(Type %in% input$list,Year == input$n, Length %in% input$length, Expense %in% input$expense)
    else
      joe
  })
  
  output$costPlot <- renderPlot({
      sample() %>% 
      ggplot(aes(Value, State)) +
      geom_col(fill=input$color) + 
      ggtitle("Average Cost of Attending Undergraduate College in Every US State")
      
  })
  
  ## DATA TAB
  output$dataTable <- renderDataTable({
    if(input$dataChoice == 'Household Income') {
      joe <- hhIncome2013_21
      joe %>% 
        filter(hhIncome2013_21$year >= input$range[1],
               hhIncome2013_21$year <= input$range[2])
     } else if(input$dataChoice == 'College Costs') {
      joe <- colleges
      joe %>%
        filter(Year >= input$range[1],
               Year <= input$range[2])
     }
    joe
  })
  
  output$filtersText <- renderText ({
    getCost <- colleges %>% 
      filter(Type %in% input$list,Year == input$n, Length %in% input$length) %>% 
      select(Value)
    
    avgCost <- round(sum(getCost)/nrow(getCost), 4)
    if(is.na(avgCost))
      avgCost <- 0
    paste("Average Cost of Attendance Based on Selected Filters: $", 
          avgCost)
  })
  
  output$rangeText <- renderText({
    paste("Currently Selected Range:", input$range[1],
          "-", input$range[2])
  })
  
  ## MAP TAB
  mapData <- reactive({
    joe <- hhIncome2013_21 %>% 
      filter(hhIncome2013_21$year %in% input$mapYear) 
    if(nrow(joe) > 1) 
      joe <- hhIncome2013_21 %>% 
        filter(hhIncome2013_21$year == input$mapYear)
    else
      joe
  })
    
  
  output$plotlyMap <- renderPlotly({
    plot_ly(mapData(), 
            dataSet <- mapData(),
            dataSet$hover <- with(dataSet, paste(dataSet$state,
                               '<br>', "Household Income", dataSet$avgHHIncome,
                               '<br>' , "Poverty Rate", dataSet$povertyRate)),
            
            l <- list(color = toRGB("white"), width = 2),
            # specify some map projection/options
            graphics <- list(scope = 'usa'),
            
            map <- plot_geo(dataSet, locationmode = 'USA-states'),
            map <- map %>% add_trace(
              z = dataSet$avgHHIncome, text = dataSet$hover, locations = dataSet$abbrev,
              color = dataSet$colorsID, colors = 'Purples'
              ),
            
            map <- map %>% colorbar(title = "Thousands USD"),
            
            map <- map %>% layout(
              title = '2013-2021 Household Income in each U.S State<br>(Hover for breakdown)'
              , geo = graphics )
    )
    map
  }) 
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

