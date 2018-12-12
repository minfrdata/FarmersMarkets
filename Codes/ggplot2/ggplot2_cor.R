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

colSums(t3)/8742


t3[t2]<-1
colnames(t3)<-food_col
library(ggplot2)
ggplot(data =t3)
+ geom_tile()

cor_food=cor(t3)
library(reshape2)
cor.m<-melt(cor_food)

#plot
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cor_food)
upper_tri

# Melt the correlation matrix
#library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
#library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  ggtitle("Correlation of Food Categories in Farmers Markets")+
  xlab("Food Categories")+
  xlab("Food Categories")+
  coord_fixed()


