ui <- fluidPage(
  
  # App title ----
  titlePanel("Distribution of food category in farmer's market"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against mpg ----
   #   selectInput("variable", "Variable:",
    #           c("has_bakegoods" = "has_bakegoods",
     #            "has_seafood"="has_seafood"
      #            ),
      
      # Input: Simple integer interval ----
      sliderInput("has_organic", "Organic or Not?",
                  min = 0, max = 1,
                  value =0,step = 1)
      
      
      # Input: Checkbox for whether outliers should be included ----
      #checkboxInput("outliers", "Show outliers", TRUE)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      plotOutput("distPlot")
      
    )
    
    
  )
)
