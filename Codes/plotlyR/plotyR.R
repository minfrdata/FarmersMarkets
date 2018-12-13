#plotly bar
df<-read.csv("payment_marketshare.csv")
df$X

library(plotly)

p <- plot_ly(
  x=df$X,
  y = df$market.share,
  name = "Market share of all pay channels",
  type = "bar"
)

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="bar-basic")
chart_link



