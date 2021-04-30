#required library

library(xml2)
library(rvest)
library(stringr)
library(dplyr)


#datacollection

c1 <- lapply(paste0('https://www.flipkart.com/mobiles/pr?sid=tyy%2C4io&otracker=categorytree&sort=price_asc&p%5B%5D=facets.rating%255B%255D%3D4%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.rating%255B%255D%3D3%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.rating%255B%255D%3D2%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.rating%255B%255D%3D1%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.price_range.from%3D4000&p%5B%5D=facets.price_range.to%3D25000&p%5B%5D=facets.operating_system%255B%255D%3DAndroid&page=',1:30,sep = ""),
                function(url){
                    url %>% read_html() %>% 
                    html_nodes("._4rR01T") %>% 
                    html_text()
                })

c2 <- lapply(paste0('https://www.flipkart.com/mobiles/pr?sid=tyy%2C4io&otracker=categorytree&sort=price_asc&p%5B%5D=facets.rating%255B%255D%3D4%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.rating%255B%255D%3D3%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.rating%255B%255D%3D2%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.rating%255B%255D%3D1%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.price_range.from%3D4000&p%5B%5D=facets.price_range.to%3D25000&p%5B%5D=facets.operating_system%255B%255D%3DAndroid&page=',1:30,sep = ""),
                function(url){
                    url %>% read_html() %>% 
                    html_nodes("._1_WHN1") %>% 
                    html_text()
                })

c3 <- lapply(paste0('https://www.flipkart.com/mobiles/pr?sid=tyy%2C4io&otracker=categorytree&sort=price_asc&p%5B%5D=facets.rating%255B%255D%3D4%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.rating%255B%255D%3D3%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.rating%255B%255D%3D2%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.rating%255B%255D%3D1%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.price_range.from%3D4000&p%5B%5D=facets.price_range.to%3D25000&p%5B%5D=facets.operating_system%255B%255D%3DAndroid&page=',1:30,sep = ""),
                function(url){
                    url %>% read_html() %>% 
                    html_nodes("._3LWZlK") %>% 
                    html_text()
                })

c4 <- lapply(paste0('https://www.flipkart.com/mobiles/pr?sid=tyy%2C4io&otracker=categorytree&sort=price_asc&p%5B%5D=facets.rating%255B%255D%3D4%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.rating%255B%255D%3D3%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.rating%255B%255D%3D2%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.rating%255B%255D%3D1%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.price_range.from%3D4000&p%5B%5D=facets.price_range.to%3D25000&p%5B%5D=facets.operating_system%255B%255D%3DAndroid&page=',1:30,sep = ""),
                function(url){
                    url %>% read_html() %>% 
                    html_nodes(".fMghEO") %>% 
                    html_text()
                })

c5 <- lapply(paste0('https://www.flipkart.com/mobiles/pr?sid=tyy%2C4io&otracker=categorytree&sort=price_asc&p%5B%5D=facets.rating%255B%255D%3D4%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.rating%255B%255D%3D3%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.rating%255B%255D%3D2%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.rating%255B%255D%3D1%25E2%2598%2585%2B%2526%2Babove&p%5B%5D=facets.price_range.from%3D4000&p%5B%5D=facets.price_range.to%3D25000&p%5B%5D=facets.operating_system%255B%255D%3DAndroid&page=',1:30,sep = ""),
                function(url){
                    url %>% read_html() %>% 
                    html_nodes(".gUuXy-") %>% 
                    html_text()
                })

#data unlisting

c1 <- unlist(c1)
c2 <- unlist(c2)
c3 <- unlist(c3)
c4 <- unlist(c4)
c5 <- unlist(c5)

#data cleaning

c11 <- gsub("I Kall","IKall",c1)
c11 <- gsub( " .*$", "", c11)
c12 <- sub("\\(.*$", "",sub(".*? ", "", c1))
c12 <- str_trim(c12, side = "right")
c13 <- sub(",.*$", "",sub(".*?\\(", "", c1))
c14 <- sub(" .*$", "",str_trim(sub(".*?,", "", c1), side = "left"))

c21 <- gsub("\u20b9","",c2)
c21 <- gsub(",","",c21)

c51 <- gsub( " .*$", "", c5 )
c51 <- gsub(",","",c51)
for (i in 1:720){
  c51[i] <- gsub(c3[i],"",c51[i])
}
c51 <- gsub(".*?\\.","NA",c51)
c51 <- gsub("NA.","",c51)
c52 <- gsub(",","",gsub(" .*$","",gsub(".*?\\&","",c5)))
c52 <- str_trim(c52, side = "left")

c41 <- gsub(" .*$","",c4)
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
c14 <- as.numeric(c14,na.rm = TRUE)
ROM_Capacity <- c14
c21 <- as.numeric(c21,na.rm = TRUE)
Price <- c21
c3 <- as.numeric(c3,na.rm = TRUE)
Star <- c3
c41 <- as.numeric(c41,na.rm = TRUE)
RAM_Capacity <- c41
c42 <- as.numeric(c42,na.rm = TRUE)
Display_Size <- c42
c43 <- as.numeric(c43,na.rm = TRUE)
Battery_Size <- c43
c51 <- as.numeric(c51,na.rm = TRUE)
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

#export the data in a csv file

write.csv(Flipkart_Mobile_Data,"E:/MyPortfolio/Data Collection/Web Scraping on Flipkart Mobile Data/Web_Scraping_on_Flipkart_Mobile_Data.csv",row.names = FALSE)