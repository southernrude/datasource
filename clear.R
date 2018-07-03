install.packages("RJSONIO")
library(RJSONIO)
install.packages("jsonlite")
library(jsonlite)
#readlines <- readLines("F:\\GoodsDetails6.29.json", warn = FALSE)
#readlines <- readLines("F:\\GoodsDetails7.3.json", warn = FALSE)
library(stringr)
#GoodsDetails <- fromJSON(readlines,flatten = TRUE)
GoodsDetails_629 <- fromJSON("F:\\GoodsDetails6.29.json")
GoodsDetails_703 <- fromJSON("F:\\GoodsDetails7.3.json")
GoodsDetails_629 <- as.data.frame(GoodsDetails_629)
GoodsDetails_703 <- as.data.frame(GoodsDetails_703)
colnames(GoodsDetails_629) <- c("中文名","英文名","评分","评价数","近期售出数","价格","种类","葡萄品种","年份","COO原产地","级别","酒精度","容量","香味分类","色泽","酒体","口感分类","建议醒酒时间","搭配菜肴","品鉴","商品ID")
#write.csv(GoodsDetails,file = "F:\\GoodsDetails6.29.csv")
#write.csv(GoodsDetails,file = "F:\\GoodsDetails7.3.csv")
colnames(GoodsDetails_703) <- c("中文名","英文名","评分","评价数","近期售出数","价格","种类","葡萄品种","年份","COO原产地","级别","酒精度","容量","香味分类","色泽","酒体","口感分类","建议醒酒时间","搭配菜肴","品鉴","商品ID")
soldnum <- str_extract_all(GoodsDetails_629$近期售出数,"[0-9]+")
GoodsDetails_629$近期售出数 <- as.numeric(soldnum)
soldnum_703 <- str_extract_all(GoodsDetails_703$近期售出数,"[0-9]+")
GoodsDetails_703$近期售出数 <-  as.numeric(soldnum_703)
GoodsDetails_629$商品ID <- as.numeric(GoodsDetails_629$商品ID)
GoodsDetails_703$商品ID <- as.numeric(GoodsDetails_703$商品ID)
GoodsDetails_629$价格 <- as.numeric(GoodsDetails_629$价格)
GoodsDetails_703$价格 <- as.numeric(GoodsDetails_703$价格)

GoodsDetails_629$评价数 <- as.numeric(GoodsDetails_629$评价数)
GoodsDetails_703$评价数 <- as.numeric(GoodsDetails_703$评价数)

GoodsDetails_629 <- GoodsDetails_629[order(GoodsDetails_629$商品ID),]
GoodsDetails_703 <- GoodsDetails_703[order(GoodsDetails_703$商品ID),]

Soldnum <- GoodsDetails_703$近期售出数 - GoodsDetails_629$近期售出数
Price <- GoodsDetails_703$价格
Alcohol_Content <- as.numeric(str_replace_all(GoodsDetails_703$酒精度,"[\t\n%]",""))
Protected_indicator_of_origin <- str_replace_all(GoodsDetails_703$级别,"[\t\n]","")
Reserve <- str_replace_all(GoodsDetails_703$年份,"[\t\n]","")
Variety <- str_replace_all(GoodsDetails_703$葡萄品种,"[\t\n]","")
COO <- str_replace_all(GoodsDetails_703$COO原产地,"[\t\n]","")
Body <- str_replace_all(GoodsDetails_703$酒体,"[\t\n]","")
Rating <- as.numeric(str_replace_all(GoodsDetails_703$评分,"[\t\n%]",""))
No_of_reviews <- as.numeric(str_replace_all(GoodsDetails_703$评价数,"[\t\n]",""))

Wine_Sample <- as.data.frame(cbind(Soldnum,Price,Alcohol_Content,Reserve,COO,Variety,Body,Protected_indicator_of_origin,Rating,No_of_reviews)) 
write.csv(Wine_Sample,file = "F:\\wineSample.csv")
