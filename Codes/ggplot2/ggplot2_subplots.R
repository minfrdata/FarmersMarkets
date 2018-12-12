setwd("C:/Users/nikhi/Desktop/Min/503project/Exam")
data<-read.csv("paymentShare.csv")

library(ggplot2)
library(dplyr)
library(maps)

us <- map_data("state")

col_class<-sapply(data, class)
col_class[1]<-"character"
col_class[2:6]<-"numeric"
data<-read.csv("paymentShare.csv",colClasses = col_class)
sapply(data, class)

for (i in c(1:dim(data)[1]))
     {
  data[i,"State"]<-tolower(data[i,"State"])
}


us=merge(us, data, by.x = "region", by.y = "State", all = TRUE)



g1<- ggplot()+ geom_map(data=us, map=us,
                    aes(x=long, y=lat, map_id=region),
                    fill="#ffffff", color="#ffffff", size=0.15)+
      geom_map(data=us, map=us,
                    aes(fill=Accepts.Credit, map_id=region),
                    color="#ffffff", size=0.15)+
      scale_fill_continuous(low='thistle2', high='darkred', 
                                 guide='colorbar')+
      ggtitle("Credit Card  Geographical Mix of Farmers Markets")+
       theme(legend.title=element_text("% of farmers accepting credit cards in each states "))
      

g2<- ggplot()+ geom_map(data=us, map=us,
                        aes(x=long, y=lat, map_id=region),
                        fill="#ffffff", color="#ffffff", size=0.15)+
  geom_map(data=us, map=us,
           aes(fill=Accepts.Sfmnp, map_id=region),
           color="#ffffff", size=0.15)+
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar')+
  ggtitle("Sfmnp  Geographical Mix of Farmers Markets")+
  theme(legend.title=element_text("% of farmers accepting Sfmnp in each states "))



g3<- ggplot()+ geom_map(data=us, map=us,
                        aes(x=long, y=lat, map_id=region),
                        fill="#ffffff", color="#ffffff", size=0.15)+
  geom_map(data=us, map=us,
           aes(fill=Accepts.Snap, map_id=region),
           color="#ffffff", size=0.15)+
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar')+
  ggtitle("Snap Geographical Mix of Farmers Markets")+
  theme(legend.title=element_text("% of farmers accepting Snap in each states "))




g4<- ggplot()+ geom_map(data=us, map=us,
                        aes(x=long, y=lat, map_id=region),
                        fill="#ffffff", color="#ffffff", size=0.15)+
  geom_map(data=us, map=us,
           aes(fill=Accepts.Wic, map_id=region),
           color="#ffffff", size=0.15)+
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar')+
  ggtitle("Wic Geographical Mix of Farmers Markets")+
  theme(legend.title=element_text("% of farmers accepting Wic in each states "))



g5<- ggplot()+ geom_map(data=us, map=us,
                        aes(x=long, y=lat, map_id=region),
                        fill="#ffffff", color="#ffffff", size=0.15)+
  geom_map(data=us, map=us,
           aes(fill=Accepts.Wiccash, map_id=region),
           color="#ffffff", size=0.15)+
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar')+
  ggtitle("Wiccash Geographical Mix of Farmers Markets")+
  theme(legend.title=element_text("% of farmers accepting Wiccash in each states "))

## subplots

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(g1,g2,g3,g4,g5,cols=2)



