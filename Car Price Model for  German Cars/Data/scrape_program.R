library(rvest)

final_data <- data.frame(Detail = c(), Price = c(), Mileage = c())
i <- 1

#change URL for other makes---------------------
repeat{
prop1_url <- paste("https://www.cars.com/shopping/results/?list_price_max=&makes[]=volkswagen&maximum_distance=200&models[]=&page=",i,"&page_size=100&stock_type=used&zip=61820", sep = "")
html <- html_session(prop1_url)
name <- data.frame(html %>% html_elements(".title") %>% html_text())
price <- data.frame(html %>% html_elements(".primary-price") %>% html_text())

#mileage <- data.frame(html %>% html_elements(".vehicle-details") %>% html_elements(".mileage") %>% html_text())
mileage <- html %>% html_elements(".vehicle-details")
mileage2 <- vector(length = length(mileage))
for (j in 1:length(mileage)){
  mileage2[j] = ifelse(length(mileage[j] %>% html_elements(".mileage") %>% html_text())>0, mileage[j] %>% html_elements(".mileage") %>% html_text(), "Missing Mileage")
} 
mileage2 <- data.frame(mileage2)

#for checking ending page
error <- html %>% html_elements(".sds-notification__title") %>% html_text()

round_data <- cbind(name,price, mileage2)
if(length(error)>0) break
final_data <- rbind(final_data,round_data)
i <- i + 1
print(i)
print(nrow(round_data))
print(length(error))
}

colnames(final_data) <- c("Detail", "Price", "Mileage")
#change file name here-----------------------
write.csv(final_data,"D:\\Fall 2022\\STAT 447\\cars\\volkswagen.csv", row.names = TRUE)


