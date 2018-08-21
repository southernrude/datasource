setwd("F:\\data\\Model1")
GoodsDetails_806 <- fromJSON("F:\\data\\GoodsDetails8.6.json")
GoodsDetails_806 <- as.data.frame(GoodsDetails_806)

Wine_Model1 <- (na.omit(GoodsDetails_806))


clean_numeric <- function(s){
  s <- gsub("%|\\$|,|\\)|\\(","",s)
  s <- numeric(s)
}

Wine_Model1 <- as.data.frame(cbind(Wine_Model1$商品ID,Wine_Model1$COO原产地,Wine_Model1$评分,Wine_Model1$评价数,Wine_Model1$近期售出数,Wine_Model1$价格,Wine_Model1$葡萄品种,Wine_Model1$年份,Wine_Model1$级别,Wine_Model1$酒精度,Wine_Model1$酒体))
colnames(Wine_Model1) <- c("GoodID","COO","Rating","No_of_reviews","RecentSoldnum","Price","Variety","Reserve","Protected_indicator_of_origin","Alcohol_Content","Body")

#table(Wine_Model1$GoodID)
str(Wine_Model1)
library(stringr)
Wine_Model1$COO <- as.character(Wine_Model1$COO)
Wine_Model1$COO <- str_replace_all(Wine_Model1$COO,"[\t\n]","")
 
for(i in 1:1862){
   Wine_Model1$COO[i] <- c(strsplit(Wine_Model1$COO," ")[[i]][1])
}

table(Wine_Model1$COO)
Wine_Model1$COO[which(Wine_Model1$COO == "中国")] <- "中国(China)"

Wine_Model1$Rating <- str_replace_all(Wine_Model1$Rating,"[\t\n%]","")
class(Wine_Model1$Rating)
Wine_Model1$Rating <- as.numeric(Wine_Model1$Rating)
Wine_Model1$Rating <- Wine_Model1$Rating/100

class(Wine_Model1$No_of_reviews)
Wine_Model1$No_of_reviews <- as.character(Wine_Model1$No_of_reviews)
Wine_Model1$No_of_reviews <- str_replace_all(Wine_Model1$No_of_reviews,"[\t\n]","")
Wine_Model1$No_of_reviews <- as.numeric(Wine_Model1$No_of_reviews)

Wine_Model1$RecentSoldnum <- as.character(Wine_Model1$RecentSoldnum)
Wine_Model1$RecentSoldnum <- as.numeric(Wine_Model1$RecentSoldnum)

Wine_Model1$Price <- as.character(Wine_Model1$Price)
Wine_Model1$Price <- as.numeric(Wine_Model1$Price)

Wine_Model1$Reserve <- as.character(Wine_Model1$Reserve)
Wine_Model1$Reserve <- str_replace_all(Wine_Model1$Reserve,"[\t\n]","")
Wine_Model1 <- Wine_Model1[-which(Wine_Model1$Reserve == "  "),]
Wine_Model1$Reserve <- as.numeric(Wine_Model1$Reserve)
Wine_Model1 <- (na.omit(Wine_Model1))

Wine_Model1$Reserve <- ifelse(Wine_Model1$Reserve==0,0,1)

Wine_Model1$Body <- as.character(Wine_Model1$Body)
Wine_Model1$Body <- str_replace_all(Wine_Model1$Body,"[\t\n]","")
Wine_Model1$Body
Wine_Model1 <- Wine_Model1[-which(Wine_Model1$Body == "  "),]
table(Wine_Model1$Body)

Wine_Model1$Alcohol_Content <- as.character(Wine_Model1$Alcohol_Content)
Wine_Model1$Alcohol_Content <- str_replace_all(Wine_Model1$Alcohol_Content,"[\t\n]","")
test <- strsplit(Wine_Model1$Alcohol_Content,"%")
for (i in 1:731) {
  Wine_Model1$Alcohol_Content[i] <- test[[i]][1]
}

test1 <- as.numeric(Wine_Model1$Alcohol_Content)
summary(test1)
Wine_Model1$Alcohol_Content <- test1
Wine_Model1 <- (na.omit(Wine_Model1))


Wine_Model1$Protected_indicator_of_origin <- as.character(Wine_Model1$Protected_indicator_of_origin)
Wine_Model1$Protected_indicator_of_origin <- str_replace_all(Wine_Model1$Protected_indicator_of_origin,"[\t\n]","")
Wine_Model1$Protected_indicator_of_origin
Wine_Model1$Protected_indicator_of_origin <- ifelse(Wine_Model1$Protected_indicator_of_origin  == "  ",0,1)

str(Wine_Model1)
Wine_Model1$COO <- as.factor(Wine_Model1$COO)
Wine_Model1$Body <- as.factor(Wine_Model1$Body)
Wine_Model1$Reserve <- as.factor(Wine_Model1$Reserve)
Wine_Model1$Reserve <- as.character(Wine_Model1$Reserve)
Wine_Model1$Reserve <- as.numeric(Wine_Model1$Reserve)
Wine_Model1$Protected_indicator_of_origin <- as.factor(Wine_Model1$Protected_indicator_of_origin)
Wine_Model1$Protected_indicator_of_origin <- as.character(Wine_Model1$Protected_indicator_of_origin)
Wine_Model1$Protected_indicator_of_origin <- as.numeric(Wine_Model1$Protected_indicator_of_origin)

Wine_Model1$RatingNo_of_reviews <- Wine_Model1$Rating*Wine_Model1$No_of_reviews
Model11 <- lm(log(Wine_Model1$RecentSoldnum)~log(Wine_Model1$Price)+Wine_Model1$COO+Wine_Model1$Body+Wine_Model1$RatingNo_of_reviews+Wine_Model1$Reserve+Wine_Model1$Protected_indicator_of_origin+Wine_Model1$Alcohol_Content)
summary(Model11)
anova(Model11)

COO_Avg_Price <- aggregate(Price~COO,data = Wine_Model1,FUN = "mean")
colnames(COO_Avg_Price)[2] <- "COO_Avg_Price"
library(ggplot2)
ggplot(COO_Avg_Price,aes(x=COO,y=COO_Avg_Price,fill=COO)) + geom_bar(stat="identity") + ggtitle("COO_Avg_Price") + theme(axis.text.x = element_text(angle = 90,hjust = 1))
summary(Wine_Model1$Price)
COO_Avg_RecentSoldnum <- aggregate(RecentSoldnum~COO,data = Wine_Model1,FUN = "mean")
colnames(COO_Avg_RecentSoldnum)[2] <- "COO_Avg_RecentSoldnum"
ggplot(COO_Avg_RecentSoldnum,aes(x=COO,y=COO_Avg_RecentSoldnum,fill=COO)) + geom_bar(stat = "identity") + ggtitle("COO_Avg_RecentSoldnum") + theme(axis.text.x = element_text(angle = 90,hjust = 1))

table(Wine_Model1$COO)
summary(Wine_Model1$RecentSoldnum)
summary(Wine_Model1$Price)
summary(Wine_Model1$Alcohol_Content)