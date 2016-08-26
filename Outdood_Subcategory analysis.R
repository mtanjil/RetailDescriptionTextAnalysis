library(RODBC)
library(gridBase)
library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)
library(cluster)
library(lattice)
library(ape)
library(rJava)
library(xlsx)
library(reshape2)
library(tm)
#library(RTextTools)
library(tau)
library(SnowballC)
library(qdap)
library(slam)
library(lubridate)


.libPaths() #Gives library location
library() #Gives all packages installed
search() #Gives a list of loaded packages

SalesData = read.csv("C:/Data/Sales data/Sales_Data_with_descriptions_2015-071716/Sales_Data_with_descriptions_2015-071716.csv",
                     col.names = c("Venue","InvoiceDate","Cat","SubCat","Description","Qty","UnitPrice","OriginalUnitPrice","Supplier"))

names(SalesData)           #Displays the name of variables in a dataset
class(SalesData$UnitPrice) #Displays the types of a variable such as integer or factor
table(SalesData$Cat)       #Gives the number of observations for each levels of a factor


#### For analyzing sporting goods 

Outdoors <- filter(SalesData, SalesData$SubCat == "Outdoors")
hist(Outdoors$Qty, xlim = c(-5,10), breaks =200)
hist(Outdoors$UnitPrice, main = "Histogram for Unit Price", col = "blue", xlim = c(-100,500), breaks = 100)

Outdoors$SubCat1 = " "


# Calculating discount

Outdoors$Discount = round (((Outdoors$OriginalUnitPrice - Outdoors$UnitPrice) / Outdoors$OriginalUnitPrice) * 100, digits = 0)
hist(Outdoors$Discount, main = "Histogram of Discount", col = "blue", xlim = c(0,100), breaks = 100)


#Removing inf,-inf,NAN and negative values from the dataset
# Outdoors_nonnegative<- Outdoors[is.finite(Outdoors$Discount), ] ##It did not remove negative discount

Outdoors_nonnegative <- filter(Outdoors, Outdoors$Discount >= 0)
Outdoors_nonnegative <- Outdoors_nonnegative[is.finite(Outdoors_nonnegative$Discount), ]

hist(Outdoors_nonnegative$Discount, main = "Histogram of Discount", col = "blue", xlim = c(0,100), breaks = 50, prob=TRUE)
lines(density(Outdoors_nonnegative$Discount))

# as.character (sportinggoods_withoutreturn$Description)
Outdoors_nonnegative$Description <- tolower(Outdoors_nonnegative$Description)  ##It converts the entire column as one large character



##### Text analytics


#sportinggoods_withoutreturn <- paste (sportinggoods_withoutreturn$Description, collapse = " ")
#sportinggoods_withoutreturn_text <- strsplit(paste(sportinggoods_withoutreturn$Description,collapse=" ")," ")
#sportinggoods_withoutreturn <- sportinggoods_withoutreturn [1:685363, ]

corpus_description <- Corpus(VectorSource(Outdoors_nonnegative$Description))
corpus_description <- tm_map(corpus_description, removePunctuation)
corpus_description <- tm_map(corpus_description, removeNumbers)
corpus_description <- tm_map(corpus_description, removeWords,stopwords("english"))

length(stopwords("english")) #Gives the number of stopwords
stopwords("en") #Provides the list of stopwords

#corpus_description <- tm_map(corpus_description, removeWords, c("a", "an","the","and", "or","with"))

corpus_description <- tm_map(corpus_description, removeWords, c("rubber","recycled","inch","coleman","outdoor","pack","set","black","feet","outdoors","blue","quart","ounce","metal","long","round",
                                                                "led","melnor","tools","ames","replacement","steel","green","gas","gallon","miracle","gro","plant","string","lawn","kit","foot","folding",
                                                                "pro","electric","red","charbroil","duty","fire","threshold","white","heavy","bbq","top","home","square","stainless","pound","killer",
                                                                "discontinued","pattern","miraclegro","yard","clear","manufacturer","control","hanging","room","portable","combo","water","classic",
                                                                "westinghouse","copper","essentials","size","floral","one","handle","deluxe","free","brass","propane","glass","food","brown","indoor",
                                                                "piece","disney","tan","beach","arm","deep","wood","suncast","products","pressure","premium","weather","side","addison","bird","weatherx",
                                                                "deck","decorative","instant","amp","series","sleeping","multi","wicker","seat","box","indooroutdoor","yellow","sun","path","popup","grass",
                                                                "power","kess","person","unscented","fiskars","picnic","large","inhouse","gpx","original","scotts","lilly","miller","hammer","natural",
                                                                "accessory","extension","bagged","soap","herb","worry","charbroilã‚","thresholdã","easy","plus","design","embark","psi","reel","accessories",
                                                                "color","shade","aluminum","dark","high","rain","coupling","pocket","family","frozen","woods","joe","grow","count","performance","roundup",
                                                                "organic","single","boys","disposable","kenmore","wall","adjustable","model","select","btu","universal","gpm","tall","amfm","dpi","kids",
                                                                "house","double",'inflatable',"oversized","pic"))

corpus_description <- tm_map(corpus_description, stripWhitespace)
corpus_description <- tm_map(corpus_description, PlainTextDocument) 

corpus_description [[1]]$content  # To view the 1st observation of the corpus

dtm <- DocumentTermMatrix(corpus_description)

inspect(dtm[1:10,1:10]) #To inspect document term matrix
findFreqTerms(dtm, lowfreq = 2000, highfreq = 5000)
findFreqTerms(dtm, 2000) #To view words which occured more than 2000 times

## dtm_w_sparse <- removeSparseTerms(dtm, 0.999) ## Did not need to use, matrix was not very big

freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)

wf = data.frame(word=names(freq), freq=freq)
wf1 = subset(wf,freq>100)
print(ggplot(wf1, aes(word, freq))
      + geom_bar(stat="identity")
      + theme(axis.text.x=element_text(angle=45, hjust=1))
      +ggtitle("Housewares SubCat Word Frequency") )


findAssocs( dtm, "cooks", 0.25)

write.csv(wf1, "C:/Temporary data from R/outdoors_stopwords.csv", row.names=FALSE, na =" ")


## Assigning values for Sub Category

Outdoors_nonnegative[grep("[0-9]",Outdoors_nonnegative$Description), ]$SubCat1 = "Bar code"
Outdoors_nonnegative[grep("canopy|tent|gazebo|radio|camping|flag|arrow|hammock|camp|stake|stakes|ground|stand|sleeping bag|poncho|blanket|torch|briquettes|ratchet|jug|trampoline|trampolines|,
                          |timer|baking soda|tarp|fork",Outdoors_nonnegative$Description),]$SubCat1 = "Camping gear"
Outdoors_nonnegative[grep("cover|pool|rake|shrub|pump|overhang|frame|pools|leaf|leaf bag|leaf bags|thermometer|filter",Outdoors_nonnegative$Description),]$SubCat1 = "Pool supplies"
Outdoors_nonnegative[grep("hose|nozzle|patrn|nozzlegrn|coil|coils|grn|wand|garden|watering|can|sprinkler|gardening|mower|bulbs|shovel|feed|shake|trimmer|washer|,
                          |seed|gloves|pad|scoop|lopper",Outdoors_nonnegative$Description),]$SubCat1 = "Gardening supplies"
Outdoors_nonnegative[grep("patio|umbrella|chair|light|table|solar|lights|lantern|base|swing|blobe|flashlight|chaise|cushion|pillow|pillwo|door|mats|doormat|,
                          |planter|mat|lounge|floor|seat cover|seat cushion|candle|kneeler|broom", Outdoors_nonnegative$Description), ]$SubCat1 = "Patio accessories"
Outdoors_nonnegative[grep("grill|cooler|brush|wheeled|burner|pit|smoker|basket|kettle|rack|grilling|grate|iron|skewer|vacuum",Outdoors_nonnegative$Description),]$SubCat1 = "Outdoor appliances"
Outdoors_nonnegative[grep("repellent|spray|insect|weed|mosquito|trap|fungicide|bug|aerosol|jacket|sprayer|fertilizer|killer|moisture|refill|refills|snail",Outdoors_nonnegative$Description),]$SubCat1 = "Pest and weed control"
Outdoors_nonnegative[grep("feeder|plastic|statue|hummingbird|marker|flower pot|spinner|sisal",Outdoors_nonnegative$Description),]$SubCat1 = "Garden decor"
Outdoors_nonnegative <- within(Outdoors_nonnegative, SubCat1[SubCat1 == " "] <- "Other")

t <- data.frame(Outdoors_nonnegative[,])
t[!grepl("[A-Z]",t$Description), ]$SubCat1 = "BC"


test <- filter(Outdoors_nonnegative, Outdoors_nonnegative$SubCat1 == " ") # To test how many observations do not have Sub category

table(Outdoors_nonnegative$SubCat1)

Outdoors_nonnegative$Year <- year(Outdoors_nonnegative$InvoiceDate)
Outdoors_nonnegative$Month <- month(Outdoors_nonnegative$InvoiceDate)


### group_by belongs to dplyr package. plyr package needs to be detached first to get accurate result

test1 <- group_by(Outdoors_nonnegative, SubCat1) 
test1 <- summarise(test1, totalSales = sum(UnitPrice, na.rm=T), totalunits = sum(Qty, na.rm = T), DiscountRate = mean(Discount, na.rm = T))
write.csv(test1, "C:/Temporary data from R/outdoors.csv", row.names=FALSE, na =" ") 


test2 <- group_by(Outdoors_nonnegative, Venue, Year, SubCat1)
test2 <- summarise(test2, totalSales = sum(UnitPrice, na.rm=T), totalunits = sum(Qty, na.rm = T), DiscountRate = mean(Discount, na.rm = T))
write.csv(test2, "C:/Temporary data from R/outdoors_venue.csv", row.names=FALSE, na =" ") 

test3 <- group_by(Outdoors_nonnegative, Year, SubCat1)
test3 <- summarise(test3, totalSales = sum(UnitPrice, na.rm=T), totalunits = sum(Qty, na.rm = T), DiscountRate = mean(Discount, na.rm = T))
write.csv(test3, "C:/Temporary data from R/outdoors_year.csv", row.names=FALSE, na =" ") 

test4 <- group_by(Outdoors_nonnegative, Year, Month, SubCat1)
test4 <- summarise(test4, TotalSales = sum(UnitPrice, na.rm=T), TotalUnits = sum(Qty, na.rm = T), DiscountRate = mean(Discount, na.rm=T))
write.csv(test4, "C:/Temporary data from R/outdoors_month.csv", row.names=FALSE, na =" ")


# Subsetting the outdoor data to analyse first 7 months

Outdoors_7months <- filter(Outdoors_nonnegative, Outdoors_nonnegative$Month <= 7)

test5 <- group_by(Outdoors_7months, Year, SubCat1)
test5 <- summarise(test5, totalSales = sum(UnitPrice, na.rm=T), totalunits = sum(Qty, na.rm = T), DiscountRate = mean(Discount, na.rm = T))
write.csv(test5, "C:/Temporary data from R/outdoors_7months.csv", row.names=FALSE, na =" ") 

### Another way of calculating total sales, units and discount rate

total_sales <- sum(sportinggoods_withoutreturn_nn$UnitPrice, na.rm=T)
total_units <- sum(sportinggoods_withoutreturn_nn$Qty, na.rm=T)

DiscountRate_Subcategory <- aggregate(sportinggoods_withoutreturn_nn$Discount, list(SubCat1=sportinggoods_withoutreturn_nn$SubCat1), FUN = mean)
TotalSales_Subcategory <- aggregate(sportinggoods_withoutreturn_nn$UnitPrice, list(SubCat1=sportinggoods_withoutreturn_nn$SubCat1), FUN = sum)
TotalUnits_Subcategory <- aggregate(sportinggoods_withoutreturn_nn$Qty, list(SubCat1=sportinggoods_withoutreturn_nn$SubCat1), FUN = sum)



### Creating a sample for Stackoverflow

Product_Description <- c("4 blenders", "3- Food Processors", "7-food processors", "Grill", "1-Grill", "123987456321","987654213688","1247986312","box","boxes")
Product <- data.frame (Product_Description)
Product$Category <- ""
Product$Product_Description <- tolower(Product$Product_Description)
Product[grep("blenders|food|processor|grill",Product$Product_Description),]$Category = "Kitchen appliances"
Product[grep("box",Product$Product_Description),]$Category = "Storage"
Product$Value[Product$Category =="Kitchen appliances"] <- 12

