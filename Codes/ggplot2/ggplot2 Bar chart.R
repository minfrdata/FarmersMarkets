setwd("/Users/minxiaocn/Desktop/Georgetown/ANLY503 Visualization/exam")
farm<-read.csv("Farmer'sMarket.csv")
food_col=c('has_organic', 'has_bakedgoods', 'has_cheese', 'has_crafts',
           'has_flowers', 'has_eggs', 'has_seafood', 'has_herbs', 'has_vegetables',
           'has_honey', 'has_jams', 'has_maple', 'has_meat', 'has_nursery',
           'has_nuts', 'has_plants', 'has_poultry', 'has_prepared', 'has_soap',
           'has_trees', 'has_wine', 'has_coffee', 'has_beans', 'has_fruits',
           'has_grains', 'has_juices', 'has_mushrooms', 'has_petfood', 'has_tofu',
           'has_wildharvested')
t=farm[food_col]

for (i in food_col){
  t[i]<-as.logical(t[i])
}

t2=sapply(t,as.logical)

t3=data.frame(matrix(0, ncol = 30, nrow = 8742))
colnames(t3)<-food_col
t3[t2]<-1
# which products are more popular
food_pct=colSums(t3)/8742
#food_pct=sort(food_pct,decreasing = T)

library(ggplot2)

food_pct<-as.data.frame(food_pct)
food_pct["food_name"]<-row.names(food_pct)
food_pct=food_pct[order(food_pct$food_pct),]


# Change the width of bars
ggplot(data=food_pct, aes(x=food_name,y=food_pct)) +
  geom_bar(stat="identity", width=0.2)+ 
  theme(text = element_text(size=12),
    axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_bar(stat="identity", color="blue", fill="orange")+
  xlab("Food Category")+
  ylab("Percent of the markets having each food category")+
  ggtitle("Proportion of markets having each food categry")
