#required library

library(xml2)
library(rvest)
library(stringr)
library(dplyr)

#URL creation 

flipkarturl <- rep('a',30)
for (i in 1:30){
  flipkarturl[i] <- paste('https://www.flipkart.com/mobiles/pr?sid=tyy%2C4io&otracker=categorytree&sort=price_asc&p%5B%5D=facets.rating%255B%255D%3D4%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.rating%255B%255D%3D3%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.rating%255B%255D%3D2%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.rating%255B%255D%3D1%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.price_range.from%3D4000&p%5B%5D=facets.price_range.to%3D25000&p%5B%5D=facets.operating_system%255B%255D%3DAndroid&page=',i,sep = "")
}

#datacollection

func <- function(url){
  webpage <- read_html(url)
  col1 <- html_nodes(webpage,"._4rR01T") %>% html_text()
  col1 <- unlist(col1)
  col2 <- html_nodes(webpage,"._1_WHN1") %>% html_text()
  col2 <- unlist(col2)
  col3 <- html_nodes(webpage,"._3LWZlK") %>% html_text()
  col3 <- unlist(col3)
  col4 <- html_nodes(webpage,".fMghEO") %>% html_text()
  col4 <- unlist(col4)
  col5 <- html_nodes(webpage,".gUuXy-") %>% html_text()
  col5 <- unlist(col5)
  data <- data.frame(col1=col1,col2=col2,col3=col3,col4=col4,col5=col5)
  return(data)
}

data1 <- lapply(flipkarturl[1], func)
data2 <- lapply(flipkarturl[2], func)
data3 <- lapply(flipkarturl[3], func)
data4 <- lapply(flipkarturl[4], func)
data5 <- lapply(flipkarturl[5], func)
data6 <- lapply(flipkarturl[6], func)
data7 <- lapply(flipkarturl[7], func)
data8 <- lapply(flipkarturl[8], func)
data9 <- lapply(flipkarturl[9], func)
data10 <- lapply(flipkarturl[10], func)
data11 <- lapply(flipkarturl[11], func)
data12 <- lapply(flipkarturl[12], func)
data13 <- lapply(flipkarturl[13], func)
data14 <- lapply(flipkarturl[14], func)
data15 <- lapply(flipkarturl[15], func)
data16 <- lapply(flipkarturl[16], func)
data17 <- lapply(flipkarturl[17], func)
data18 <- lapply(flipkarturl[18], func)
data19 <- lapply(flipkarturl[19], func)
data20 <- lapply(flipkarturl[20], func)
data21 <- lapply(flipkarturl[21], func)
data22 <- lapply(flipkarturl[22], func)
data23 <- lapply(flipkarturl[23], func)
data24 <- lapply(flipkarturl[24], func)
data25 <- lapply(flipkarturl[25], func)
data26 <- lapply(flipkarturl[26], func)
data27 <- lapply(flipkarturl[27], func)
data28 <- lapply(flipkarturl[28], func)
data29 <- lapply(flipkarturl[29], func)
data30 <- lapply(flipkarturl[30], func)

dataraw <- rbind(data1[[1]],
                 data2[[1]],
                 data3[[1]],
                 data4[[1]],
                 data5[[1]],
                 data6[[1]],
                 data7[[1]],
                 data8[[1]],
                 data9[[1]],
                 data10[[1]],
                 data11[[1]],
                 data12[[1]],
                 data13[[1]],
                 data14[[1]],
                 data15[[1]],
                 data16[[1]],
                 data17[[1]],
                 data18[[1]],
                 data19[[1]],
                 data20[[1]],
                 data21[[1]],
                 data22[[1]],
                 data23[[1]],
                 data24[[1]],
                 data25[[1]],
                 data26[[1]],
                 data27[[1]],
                 data28[[1]],
                 data29[[1]],
                 data30[[1]])

c1 <- as.character(dataraw[,1])
c2 <- as.character(dataraw[,2])
c3 <- as.character(dataraw[,3])
c4 <- as.character(dataraw[,4])
c5 <- as.character(dataraw[,5])

#data cleaning

c11 <- gsub("I Kall","IKall",c1)
c11 <- gsub( " .*$", "", c11)
c12 <- sub("\\(.*$", "",sub(".*? ", "", c1))
c12 <- str_trim(c12, side = "right")
c13 <- sub(",.*$", "",sub(".*?\\(", "", c1))
c14 <- sub(" .*$", "",str_trim(sub(".*?,", "", c1), side = "left"))
c14 <- gsub("512","0.5",c14)

c21 <- gsub("\u20b9","",c2)
c21 <- gsub(",","",c21)

c51 <- gsub( " .*$", "", c5 )
c51 <- gsub(",","",c51)
for (i in 1:720){
  c51[i] <- gsub(c3[i],"",c51[i])
}
c52 <- gsub(",","",gsub(" .*$","",gsub(".*?\\&","",c5)))
c52 <- str_trim(c52, side = "left")

c41 <- gsub(" .*$","",c4)
c41 <- gsub("512","0.5",c41)
c42 <- gsub(".*?cm","",c4)
c42 <- str_trim(c42, side = "left")
c42 <- gsub("\\(","",c42)
c42 <- gsub( " .*$", "", c42)
c43 <- gsub("mAh.*$","",c4)
c43 <- str_trim(c43, side = "right")
c43 <- word(c43,-1)
c43 <- gsub("Camera","",c43)

#naming the data columns

c11 <- as.factor(c11)
Brand_Name <- c11
c12 <- as.factor(c12)
Model_Name <- c12
c13 <- as.factor(c13)
Color_Name <- c13
c14 <- as.numeric(c14)
ROM_Capacity <- c14
c21 <- as.numeric(c21)
Price <- c21
c3 <- as.numeric(c3)
Star <- c3
c41 <- as.numeric(c41)
RAM_Capacity <- c41
c42 <- as.numeric(c42)
Display_Size <- c42
c43 <- as.numeric(c43)
Battery_Size <- c43
c51 <- as.numeric(c51)
Ratings <- c51
c52 <- as.numeric(c52)
Reviews <- c52

#data frame creation

Flipkart_Mobile_Data <- data.frame(Brand_Name = Brand_Name,
                                   Model_Name = Model_Name,
                                   Price = Price,
                                   ROM_Capacity = ROM_Capacity,
                                   RAM_Capacity = RAM_Capacity,
                                   Display_Size = Display_Size,
                                   Battery_Size = Battery_Size,
                                   Color_Name = Color_Name,
                                   Star = Star,
                                   Ratings = Ratings,
                                   Reviews = Reviews)
View(Flipkart_Mobile_Data)
summary(Flipkart_Mobile_Data)

#data pre-processings

Flipkart_Mobile_Data$ROM_Capacity[which(Flipkart_Mobile_Data$ROM_Capacity == 165)] <- 165/1024
summary(as.factor(Flipkart_Mobile_Data$ROM_Capacity))
Flipkart_Mobile_Data$ROM_Capacity[is.na(Flipkart_Mobile_Data$ROM_Capacity)] <- 32
summary(as.factor(Flipkart_Mobile_Data$Battery_Size[which(Flipkart_Mobile_Data$Brand_Name == "Honor")]))
Flipkart_Mobile_Data$Battery_Size[is.na(Flipkart_Mobile_Data$Battery_Size)] <- 3000
summary(as.factor(Flipkart_Mobile_Data$Ratings[which(Flipkart_Mobile_Data$Brand_Name == "Itel")]))
Flipkart_Mobile_Data$Ratings[is.na(Flipkart_Mobile_Data$Ratings)] <- 269

#export the data in a csv file

write.csv(Flipkart_Mobile_Data,"E:/MyPortfolio/Data Collection/Web Scraping on Flipkart Mobile Data/Web_Scraping_on_Flipkart_Mobile_Data_updated.csv",row.names = FALSE)
