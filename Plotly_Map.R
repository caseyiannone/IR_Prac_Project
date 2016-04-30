
#https://plot.ly/r/reference/#layout-geo documentation for graphs

library(plotly)

#Plotly login info
Sys.setenv("plotly_username"="iannonecasey")
Sys.setenv("plotly_api_key"="jqnw0z587h")

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_us_cities.csv')
df$hover <- paste(df$name, "Population", df$pop/1e6, " million")

df$q <- with(df, cut(pop, quantile(pop)))
levels(df$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
df$q <- as.ordered(df$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

p <-  plot_ly(df, lon = lon, lat = lat, text = hover, marker = list(size = sqrt(pop/10000) + 1),
        color = q, type = 'scattergeo', locationmode = 'USA-states') %>%
  layout(title = '2014 US city populations<br>(Click legend to toggle)', geo = g)


#Pushing to plotly 
plotly_POST(p, filename = "map")

#filename sets the name of the file inside your online plotly account.
#world_readable sets the privacy of your chart. If TRUE, the graph is publically viewable, 
#if FALSE, only you can view it.





