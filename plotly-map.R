library(tidyverse)
library(ggplot2)
library(plotly)
library(readxl)


hhIncome <- read_excel("est21us.xls")
names(hhIncome)
drop <- c("Table with column headers in rows 3 and 4","...4", "...5", "...6",
          "...8",  "...9", "...10", "...8",  "...9",  "...10","...11",
          "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19",
          "...20", "...21", "...23", "...24", "...25", "...26", "...27","...28", 
          "...29", "...30")
hhIncome = hhIncome[,!(names(hhIncome) %in% drop)]
hhIncome <- hhIncome[-(1:3),]
colnames(hhIncome)[1] = "abbrev"
colnames(hhIncome)[2] = "state"
colnames(hhIncome)[3] = "povertyRate"
colnames(hhIncome)[4] = "avgHHIncome"


hhIncome$povertyRate <- substring(hhIncome2$povertyRate,1,4)


### Actual code

l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa'
)

hhIncome$hover <- with(hhIncome, paste(state,
                           '<br>', "Household Income", hhIncome$avgHHIncome,
                           '<br>' , "Poverty Rate", hhIncome$povertyRate))

fig <- plot_geo(hhIncome, locationmode = 'USA-states')
fig <- fig %>% add_trace(
  z = hhIncome$avgHHIncome, text = hhIncome$hover, locations = hhIncome$abbrev)

fig <- fig %>% colorbar( title = "Thousands USD")

fig <- fig %>% layout(
  title = '2013-2021 Household Income in each U.S State<br>(Hover for breakdown)',
  geo = g)

fig


