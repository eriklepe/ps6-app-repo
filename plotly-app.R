#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
  plotlyOutput("graph")
)

server <- function(input, output, session){
  
  output$graph <- renderPlotly({
    plot_ly(hhIncome, 
            hhIncome$hover <- with(hhIncome, paste(state,
                                                   '<br>', "Household Income", hhIncome$avgHHIncome,
                                                   '<br>' , "Poverty Rate", hhIncome$povertyRate)),
            l <- list(color = toRGB("white"), width = 2),
            # specify some map projection/options
            g <- list(scope = 'usa'),
            
            fig <- plot_geo(hhIncome, locationmode = 'USA-states'),
            fig <- fig %>% add_trace(
              z = hhIncome$avgHHIncome, text = hhIncome$hover, locations = hhIncome$abbrev,
              color = df$total.exports, colors = 'Purples'),
            
            fig <- fig %>% colorbar( title = "Thousands USD"),
            
            fig <- fig %>% layout(
              title = '2013-2021 Household Income in each U.S State<br>(Hover for breakdown)'
              , geo = g)
              )
    fig
  })
}

shinyApp(ui, server)

