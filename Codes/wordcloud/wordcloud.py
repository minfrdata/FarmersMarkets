from wordcloud import WordCloud, STOPWORDS
import matplotlib.pyplot as plt
import pandas as pd
data=pd.read_csv("Farmer'sMarket.csv")
word_dict=data["market_name"].values

word_text=""
for i in word_dict:
    word_list=i.split(" ")
    
    for j in word_list:
        
  
        if j not in ["MARKETS","MARKET","Markets","Market","Farmer's","FARMERS","FARMER","Farmers Market","Farmers' Market","Farmer Market","Farmers", "Market","Farmers'","Farmer","Farmer "," Farmer"]:
            print(j)
            word_text=word_text+" "+j

wordcloud=WordCloud(width=800,height=800).generate(word_text)

#PLOT AND SAVE
import matplotlib.pyplot as plt
plt.figure(figsize=(8,8),facecolor=None)
plt.imshow(wordcloud)
plt.axis("off")
plt.tight_layout(pad=0)
#plt.show()
plt.savefig("wordcloud.png")