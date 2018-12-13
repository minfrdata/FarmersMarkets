#setwd("/Users/minxiaocn/Desktop/Georgetown/ANLY503 Visualization/exam")
farm<-read.csv("Farmer'sMarket.csv")
head(farm)

#convert factor to num
for (i in c(24:53)){
  farm[,i]<-as.integer(as.logical(farm[,i]))
}

farm["num_food_category"]<-NA
farm["num_food_category"]<-rowSums(farm[,25:53])
head(farm)


# remove some of the wrong records:
farm<-farm[farm$num_food_category!=0,]
farm<-farm[farm$has_organic %in% c(0,1),]
#mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))
mpgData<-farm

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
#  sliderValues <- reactive({
 #   data.frame(
  #    Name = c("No of farmer's markerts"),
   #   Value = mpgData[mpgData[,input$variable],]
  #})
  
  
  output$distPlot <- renderPlot({
    x    <- mpgData[mpgData["has_organic"]==input$has_organic,"num_food_category"]
    #bins <- seq(min(x), max(x))
    hist(x,main="histogram of No. of food categories",col="#990000",xlab="No. of food categories")
    })
  
 # output$table
  
}
