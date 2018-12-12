library(threejs)
library(htmlwidgets)


setwd("/Users/minxiaocn/Desktop/Georgetown/ANLY503 Visualization/exam")
data<-read.csv("states_farm.csv")
x1<-data$population
y1<-data$n_farms
z1<-data$X..households.Purchasing.Organic.Products
col1_ind=which(z1<=0.75)
col2_ind=which((0.75<z1) & (z1<0.85))
col3_ind=which(z1>=0.85)
  
cols_=c(rep("#FF99CC",length(z1)))


MyJ3=scatterplot3js(z1, x1, y1,color =cols_, height=450,width = 600,
                    num.ticks = c(4, 6, 6),  grid = T, cex.lab=0.7, cex.axis=0.5, cex.main=1.5, cex.sub=0.5, flip.y = F,
                   axisLabels=c("%households.Purchasing.Organic.Products","Population","No. of Farmers' Markets")
                    ,main="3D plot of No. of markets, population and shopping habits "
                   ,signif = 4, bg = "#ffffff") 
MyJ3




# }

MyJ3
saveWidget(MyJ3, "d3j3_pop_farm.html", selfcontained = TRUE)

