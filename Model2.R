Wine_Model2 <- Wine_Model1
str(Wine_Model2)
levels(Wine_Model2$Variety)
Wine_Model2$Variety <- as.character(Wine_Model2$Variety)

library(stringr)
Wine_Model2$Variety <- str_replace_all(Wine_Model2$Variety,"[\t\n]","")

Wine_Model2$Variety

Grape_Variety <- grep("100%",Wine_Model2$Variety)
Wine_Model2 <- Wine_Model2[Grape_Variety,]

Grape <- str_extract_all(Wine_Model2$Variety,"[a-zA-Z]+")
#install.packages("rlist")
#library(rlist)
#list.map(Grape[[1]],1:length(Grape[[1]]))
#Grape_Variety <- gsub(pattern = "[+Cabernet+]",replacement = "Cabernet",Wine_Model2$Variety)
#rm(Grape)
 
for (i in 1:259){
  #j <- length(Grape[[i]])
  Grape[i] <- Grape[[i]][1]
  
}

Wine_Model2$Variety <- unlist(Grape)
Wine_Model2$Variety <- as.character(Wine_Model2$Variety)
Wine_Model2 <- (na.omit(Wine_Model2))
Wine_Model2$Variety <- tolower(Wine_Model2$Variety)
Wine_Model2$Variety <- as.factor(Wine_Model2$Variety)
levels(Wine_Model2$Variety)
#Wine_Model2$Variety[Grape_Variety] <- "Cabernet"



Model2 <- lm(log(Wine_Model2$RecentSoldnum)~log(Wine_Model2$Price)+Wine_Model2$COO+Wine_Model2$Variety+Wine_Model2$Reserve+Wine_Model2$Protected_indicator_of_origin+Wine_Model2$Alcohol_Content+Wine_Model2$Body+Wine_Model2$RatingNo_of_reviews)

summary(Model2)

anova(Model2)

Model21 <- lm(log(Wine_Model2$RecentSoldnum)~log(Wine_Model2$Price)+Wine_Model2$COO+Wine_Model2$Reserve+Wine_Model2$Protected_indicator_of_origin+Wine_Model2$Alcohol_Content+Wine_Model2$Body+Wine_Model2$RatingNo_of_reviews)

summary(Model21)

anova(Model21)

Model23 <- lm(log(Wine_Model2$RecentSoldnum)~log(Wine_Model2$Price)+Wine_Model2$Variety+Wine_Model2$Reserve+Wine_Model2$Protected_indicator_of_origin+Wine_Model2$Alcohol_Content+Wine_Model2$Body+Wine_Model2$RatingNo_of_reviews)

summary(Model23)

anova(Model23)


Wine_COO_Avg_Soldnum <- aggregate(RecentSoldnum~COO,data = Wine_Model2,FUN = "mean")
colnames(Wine_COO_Avg_Soldnum[2]) <- "Wine_COO_Avg_Soldnum"
library(ggplot2)
ggplot(Wine_COO_Avg_Soldnum,aes(x=COO,y=RecentSoldnum,fill=COO)) + geom_bar(stat = "identity") + ggtitle("Wine_COO_Avg_Soldnum") + theme(axis.text.x = element_text(angle = 90,hjust = 1))


Wine_COO_Avg_Price <- aggregate(Price~COO,data = Wine_Model2,FUN = "mean")
#colnames(Wine_COO_Avg_Price)
ggplot(Wine_COO_Avg_Price,aes(x=COO,y=Price,fill=COO)) + geom_bar(stat = "identity") + ggtitle("Wine_COO_Avg_Price") + theme(axis.text.x = element_text(angle = 90,hjust = 1))

#ggplot(Wine_Model2,aes(x=Price,y=RecentSoldnum)) + geom_point() + ggtitle("Soldnum with Price") + geom_smooth()
#plot(Wine_Model2$RecentSoldnum~Wine_Model2$Price)

summary(Wine_Model2$COO)
summary(Wine_Model2$RecentSoldnum)
summary(Wine_Model2$Price)
summary(Wine_Model2$Variety)
summary(Wine_Model2$Body)
summary(Wine_Model2$Alcohol_Content)
summary(Wine_Model2$Reserve)
summary(Wine_Model2$Protected_indicator_of_origin)
summary(Wine_Model2$Rating)
summary(Wine_Model2$No_of_reviews)

Body_Avg_Soldnum <- aggregate(RecentSoldnum~Body,data = Wine_Model2,FUN = "mean")
library(ggplot2)
ggplot(Body_Avg_Soldnum,aes(x=Body,y=RecentSoldnum,fill=Body)) + geom_bar(stat = "identity") + ggtitle("Body_Avg_Soldnum")


Cabernet <- grep("Cabernet",Wine_Model2$Variety)
Carmenere <- grep("Carmenere",Wine_Model2$Variety)
Gamay <- grep("Gamay",Wine_Model2$Variety)
Grenache <- grep("Grenache",Wine_Model2$Variety)
Malbecn <- grep("Malbec",Wine_Model2$Variety)
Merlot <- grep("Merlot",Wine_Model2$Variety)
Mixed <- grep("Mixed",Wine_Model2$Variety)
Monastrell <- grep("Monastrell",Wine_Model2$Variety)
Negroamaro <- grep("Negroamaro",Wine_Model2$Variety)
Paez <- grep("Paez",Wine_Model2$Variety)
Pinot <- grep("Pinot",Wine_Model2$Variety)
Sangiovese <- grep("Sangiovese",Wine_Model2$Variety)
Shiraz <- grep("Shiraz",Wine_Model2$Variety)
Tempranillo <- grep("Tempranillo",Wine_Model2$Variety)

Wine_Model2$Variety[Cabernet] <- "Cabernet"

Wine_Model2$Variety[Carmenere] <- "Carmenere"

Wine_Model2$Variety[Gamay] <- "Gamay"

Wine_Model2$Variety[Grenache] <- "Grenache"

Wine_Model2$Variety[Malbecn] <- "Malbecn"

Wine_Model2$Variety[Merlot] <- "Merlot"

Wine_Model2$Variety[Mixed] <- "Mixed"

Wine_Model2$Variety[Monastrell] <- "Monastrell"

Wine_Model2$Variety[Negroamaro] <- "Negroamaro"

Wine_Model2$Variety[Paez] <- "Paez"

Wine_Model2$Variety[Pinot] <- "Pinot"

Wine_Model2$Variety[Sangiovese] <- "Sangiovese"

Wine_Model2$Variety[Shiraz] <- "Shiraz"

Wine_Model2$Variety[Tempranillo] <- "Tempranillo"

library(stringr)
Wine_Model2$Variety <- str_replace_all(Wine_Model2$Variety,"[\t\n]","")
Wine_Model2$Variety <- as.factor(Wine_Model2$Variety)
levels(Wine_Model2$Variety)
#which(Grape_Variety == "Cabernet")
#|+Carmenere+|+Gamay+|+Grenache+|+Malbec+|+Merlot+|+Mixed+|+Monastrell+|+Negroamaro+|+Paez+|+Pinot+|+Sangiovese+|+Shiraz+|+Tempranillo+]",)

#  Syrah  Ortega   Bacchus    Moscato Bianco      Chenin Blanc     Harslevelu   Sauvignon Blanc    100%赤霞珠  Dornfelder   Furmint   Grillo
#  Nero d'Avola    pinot noir    Garganega      Graciano    100%卡里纳罗   Regent  Riesling      Mazuelo    malvasia  Macabeo  Magliocco   merlot
#  Mencia   Muscat   Verdejo   Verdelho    用正则表达式取（）里面的英文名字 取小写 然后因子化