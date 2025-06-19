# Scrape corona and other data
library(rvest)
library(data.table)
library(lubridate)
library(zoo)

###2020_data ####

#specify the webpages
weeks_2020 <- data.frame(
  start = c(3,7,11,16,20,24,29,33,38,42,46,50),
  end = c(6,10,15,19,23,28,32,37,41,45,49,2)
)

# Create storage object
storage_2020 <- data.frame(matrix(ncol = 7))
colnames(storage_2020) <- c("Adenovirus", "Coronavirus", "Parainfluenza",
                            "Rhinovirus", "RSV","Year", "Week")
storage_2020_ages <- data.frame(matrix(ncol = 10))
colnames(storage_2020_ages) <- c("Adenovirus", "Coronavirus", "Influenza A",
                                 "Influenza B", "Parainfluenza", "RSV",
                                 "Rhinovirus","Year", "startweek", "Age")

for(page in 1:nrow(weeks_2020)) {
  
  # define the webpage
  week_from <- weeks_2020[page,"start"]
  week_to <- weeks_2020[page,"end"]
  week_diff <- week_to - week_from
  if(page == 12) { week_diff <- 6}
  if (page == 12){
    url <- "https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2020/reports-of-respiratory-infections-made-to-phe-from-phe-and-nhs-laboratories-in-england-and-wales-week-50-2020-to-week-2-2021"
  } else {
    url <- paste0("https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2020/reports-of-respiratory-infections-made-to-phe-from-phe-and-nhs-laboratories-in-england-and-wales-weeks-",week_from,"-to-",week_to,"-2020")
  }
  
  # read in the webpage
  webpage <- read_html(url)
  #Using CSS selectors to scrape the rankings section
  rank_data_html <- html_nodes(webpage,'td')
  #Converting the ranking data to text
  rank_data <- html_text(rank_data_html)
  
  # THIS IS FOR THE WEEKLY CASE NUMBER
  # NOTE - only works if the first mention of adeno is in required table
  strt_num <- which(rank_data == "Adenovirus*" |rank_data == "Adenovirus *")
  if (page == 12) {
    end_num <- which(rank_data == "RSV" | rank_data == "Respiratory Syncytial Virus (RSV)" |
                       rank_data == "Respiratory syncytial virus") + (week_diff + 1)
  } else {
    end_num <- which(rank_data == "RSV" | rank_data == "Respiratory Syncytial Virus (RSV)" |
                       rank_data == "Respiratory syncytial virus") + (week_diff + 2)
  }

  #create matrix
  if (page == 12) {
    temp_matrix <- data.frame(matrix(rank_data[strt_num:end_num], ncol = 5, byrow = F))
  } else {
    temp_matrix <- data.frame(matrix(rank_data[strt_num:end_num], ncol = 5, byrow = F))
  }
  
  # remove total row, add week numbers
  if(page == 12){
    temp_matrix <- temp_matrix[2:(week_diff + 1),]
    temp_matrix$Year <- c(2020,2020,2020,2020,2021,2021)
    temp_matrix$Week <- c(50,51,52,53,1,2)
  } else {
    temp_matrix <- temp_matrix[2:(week_diff + 2),]
    temp_matrix$Year <- 2020
    temp_matrix$Week <- week_from:week_to
  }
  colnames(temp_matrix) <- c("Adenovirus", "Coronavirus", "Parainfluenza",
                             "Rhinovirus", "RSV","Year", "Week")
  
  # THIS IS FOR THE MONTHLY AGE DIST
  strt_num2 <- which(rank_data == "Adenovirus *" |rank_data == "Adenovirus*")
  end_num2 <- which(rank_data == "Rhinovirus")[2] + 8
  temp_matrix2 <- data.frame(matrix(rank_data[strt_num2:end_num2], ncol = 7, byrow = F))
  temp_matrix2 <- temp_matrix2[2:7,]
  temp_matrix2$Year <- 2020
  temp_matrix2$startweek <- week_from
  temp_matrix2$Age <- c("Under 1", "1-4 Y", "5-14 Y", "15-44 Y", "45-64 Y", "65 Y+")
  colnames(temp_matrix2) <- c("Adenovirus", "Coronavirus", "Influenza A",
                              "Influenza B", "Parainfluenza", "RSV", 
                              "Rhinovirus","Year", "startweek", "Age")
  
  storage_2020 <- rbind(storage_2020, temp_matrix)
  storage_2020_ages <- rbind(storage_2020_ages, temp_matrix2)
  
}


### 2019 data ####

#specify the webpages
weeks_2019 <- data.frame(
  start = c(1,5,9,14,18,23,27,31,36,40,44,49),
  end = c(4,8,13,17,22,26,30,35,39,43,48,2)
)

# Create storage object
storage_2019 <- data.frame(matrix(ncol = 7))
colnames(storage_2019) <- c("Adenovirus", "Coronavirus", "Parainfluenza",
                            "Rhinovirus", "RSV","Year", "Week")
storage_2019_ages <- data.frame(matrix(ncol = 10))
colnames(storage_2019_ages) <- c("Adenovirus", "Coronavirus", "Influenza A",
                                 "Influenza B", "Parainfluenza", "RSV", 
                                 "Rhinovirus","Year", "startweek", "Age")

for(page in 1:nrow(weeks_2019)) {
  # define the webpage
  week_from <- weeks_2019[page,"start"]
  week_to <- weeks_2019[page,"end"]
  week_diff <- week_to - week_from
  if(page == 12) { week_diff <- 6}
  url <- paste0("https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2019/reports-of-respiratory-infections-made-to-phe-from-phe-and-nhs-laboratories-in-england-and-wales-weeks-",week_from,"-to-",week_to,"-2019")
  # manually specify for the ones with odd names
  if(page == 3 ){
    url <- "https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2019/reports-of-respiratory-infections-made-to-phe-from-phe-and-nhs-laboratories-in-england-and-wales-weeks-9-to-x-2019--2" } 
  if (page == 7){
    url <- "https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2019/reports-of-respiratory-infections-made-to-phe-from-phe-and-nhs-laboratories-in-england-and-wales-weeks-27-to-30-2019--2"}
  if (page == 12){
    url <- "https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2019/reports-of-respiratory-infections-made-to-phe-from-phe-and-nhs-laboratories-in-england-and-wales-weeks-49-2019-to-02-2020-to-48-2019"}
  # read in the webpage
  webpage <- read_html(url)
  #Using CSS selectors to scrape the rankings section
  rank_data_html <- html_nodes(webpage,'td')
  #Converting the ranking data to text
  rank_data <- html_text(rank_data_html)
  # Pick the required content
  strt_num <- which(rank_data == "Adenovirus*")
  end_num <- which(rank_data == "RSV" | rank_data == "Respiratory Syncytial Virus (RSV)") + (week_diff + 2)
  #create matrix
  if(page == 4){
    temp_matrix <- data.frame(matrix(rank_data[strt_num:(end_num + 1)], ncol = 5, byrow = F))
  } else {
    temp_matrix <- data.frame(matrix(rank_data[strt_num:end_num], ncol = 5, byrow = F))}
  # remove total row, add week numbers
  if(page == 12){
    temp_matrix <- temp_matrix[2:(week_diff + 1),]
    temp_matrix$Year <- c(2019,2019,2019,2019,2020,2020)
    temp_matrix$Week <- c(49,50,51,52,1,2)
  } else {
    temp_matrix <- temp_matrix[2:(week_diff+2),]
    temp_matrix$Year <- 2019
    temp_matrix$Week <- week_from:week_to
  }
  colnames(temp_matrix) <- c("Adenovirus", "Coronavirus", "Parainfluenza",
                             "Rhinovirus", "RSV","Year", "Week")
  
  # THIS IS FOR THE MONTHLY AGE DIST
  strt_num2 <- which(rank_data == "Adenovirus *")
  end_num2 <- which(rank_data == "Rhinovirus")[2] + 8
  temp_matrix2 <- data.frame(matrix(rank_data[strt_num2:end_num2], ncol = 7, byrow = F))
  temp_matrix2 <- temp_matrix2[2:7,]
  temp_matrix2$Year <- 2019
  temp_matrix2$startweek <- week_from
  temp_matrix2$Age <- c("Under 1", "1-4 Y", "5-14 Y", "15-44 Y", "45-64 Y", "65 Y+")
  colnames(temp_matrix2) <- c("Adenovirus", "Coronavirus", "Influenza A",
                              "Influenza B", "Parainfluenza", "RSV", 
                              "Rhinovirus","Year", "startweek", "Age")
  
  storage_2019 <- rbind(storage_2019, temp_matrix)
  storage_2019_ages <- rbind(storage_2019_ages, temp_matrix2)
  
}


####### 2018 ##### 

#specify the webpages
weeks_2018 <- data.frame(
  start = c(1,5,9,14,18,22,27,31,36,40,44,49),
  end = c(4,8,13,17,21,26,30,35,39,43,48,52)
)
# Create storage object
storage_2018 <- data.frame(matrix(ncol = 7))
colnames(storage_2018) <- c("Adenovirus", "Coronavirus", "Parainfluenza",
                            "Rhinovirus", "RSV","Year", "Week")
storage_2018_ages <- data.frame(matrix(ncol = 10))
colnames(storage_2018_ages) <- c("Adenovirus", "Coronavirus", "Influenza A",
                                 "Influenza B", "Parainfluenza", "RSV", 
                                 "Rhinovirus","Year", "startweek", "Age")

for(page in 1:nrow(weeks_2018)) {
  
  # define the webpage
  week_from <- weeks_2018[page,"start"]
  week_to <- weeks_2018[page,"end"]
  week_diff <- week_to - week_from
  # manually specify for the ones with odd names
  if(page > 2){
    url <- paste0("https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2018/laboratory-reports-of-respiratory-infections-made-to-phe-from-phe-and-nhs-laboratories-in-england-and-wales-weeks-",week_from,"-to-",week_to,"-2018")
  }else {
    url <- paste0("https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2018/reports-of-respiratory-infections-made-to-phe-from-phe-and-nhs-laboratories-in-england-and-wales-weeks-",week_from,"-to-",week_to,"-2018")
  }
  # read in the webpage
  webpage <- read_html(url)
  #Using CSS selectors to scrape the rankings section
  rank_data_html <- html_nodes(webpage,'td')
  #Converting the ranking data to text
  rank_data <- html_text(rank_data_html)
  # Pick the required content
  strt_num <- which(rank_data == "Adenovirus*")
  end_num <- which(rank_data == "RSV" | rank_data == "Respiratory Syncytial Virus (RSV)") + (week_diff + 2)
  #create matrix
  temp_matrix <- data.frame(matrix(rank_data[strt_num:end_num], ncol = 5, byrow = F))
  # remove total row, add week numbers
  
  temp_matrix <- temp_matrix[2:(week_diff + 2),]
  temp_matrix$Year <- 2018
  temp_matrix$Week <- week_from:week_to
  
  colnames(temp_matrix) <- c("Adenovirus", "Coronavirus", "Parainfluenza",
                             "Rhinovirus", "RSV","Year", "Week")
  
  # THIS IS FOR THE MONTHLY AGE DIST
  strt_num2 <- which(rank_data =="Adenovirus *")
  end_num2 <- which(rank_data == "Rhinovirus")[2] + 8
  temp_matrix2 <- data.frame(matrix(rank_data[strt_num2:end_num2], ncol = 7, byrow = F))
  temp_matrix2 <- temp_matrix2[2:7,]
  temp_matrix2$Year <- 2018
  temp_matrix2$startweek <- week_from
  temp_matrix2$Age <- c("Under 1", "1-4 Y", "5-14 Y", "15-44 Y", "45-64 Y", "65 Y+")
  colnames(temp_matrix2) <- c("Adenovirus", "Coronavirus", "Influenza A",
                              "Influenza B", "Parainfluenza", "RSV", 
                              "Rhinovirus","Year", "startweek", "Age")
  
  storage_2018 <- rbind(storage_2018, temp_matrix)
  storage_2018_ages <- rbind(storage_2018_ages, temp_matrix2)
  
}


####### 2017 ######

#specify the webpages
weeks_2017 <- data.frame(
  start = c(1,5,9,14,18,22,27,31,35,40,44,48),
  end = c(4,8,13,17,21,26,30,34,39,43,47,52)
)
# Create storage object
storage_2017 <- data.frame(matrix(ncol = 7))
colnames(storage_2017) <- c("Adenovirus", "Coronavirus", "Parainfluenza",
                            "Rhinovirus", "RSV","Year", "Week")
storage_2017_ages <- data.frame(matrix(ncol = 10))
colnames(storage_2017_ages) <- c("Adenovirus", "Coronavirus", "Influenza A",
                                 "Influenza B", "Parainfluenza", "RSV", 
                                 "Rhinovirus","Year", "startweek", "Age")


for(page in 1:nrow(weeks_2017)) {
  
  # define the webpage
  week_from <- weeks_2017[page,"start"]
  week_to <- weeks_2017[page,"end"]
  week_diff <- week_to - week_from
  # manually specify for the ones with odd names
  if(page == 7){
    url <- paste0("https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2017/weeks-",week_from,"-to-",week_to)
  } else{
    url <- paste0("https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2017/laboratory-reports-of-respiratory-infections-made-to-phe-from-phe-and-nhs-laboratories-in-england-and-wales-weeks-",week_from,"-to-",week_to,"-2017")
  }
  # read in the webpage
  webpage <- read_html(url)
  #Using CSS selectors to scrape the rankings section
  rank_data_html <- html_nodes(webpage,'td')
  #Converting the ranking data to text
  rank_data <- html_text(rank_data_html)
  # Pick the required content
  strt_num <- which(rank_data == "Adenovirus*")
  end_num <- which(rank_data == "RSV" | rank_data == "Respiratory Syncytial Virus (RSV)" |
                     rank_data == "Respiratory syncytial virus" ) + (week_diff + 2)
  #create matrix
  temp_matrix <- data.frame(matrix(rank_data[strt_num:end_num[1]], ncol = 5, byrow = F))
  # remove total row, add week numbers
  
  temp_matrix <- temp_matrix[2:(week_diff + 2),]
  temp_matrix$Year <- 2017
  temp_matrix$Week <- week_from:week_to
  
  colnames(temp_matrix) <- c("Adenovirus", "Coronavirus", "Parainfluenza",
                             "Rhinovirus", "RSV","Year", "Week")
  # THIS IS FOR THE MONTHLY AGE DIST
  strt_num2 <- which(rank_data =="Adenovirus *")
  end_num2 <- which(rank_data == "Rhinovirus")[2] + 8
  temp_matrix2 <- data.frame(matrix(rank_data[strt_num2:end_num2], ncol = 7, byrow = F))
  temp_matrix2 <- temp_matrix2[2:7,]
  temp_matrix2$Year <- 2017
  temp_matrix2$startweek <- week_from
  temp_matrix2$Age <- c("Under 1", "1-4 Y", "5-14 Y", "15-44 Y", "45-64 Y", "65 Y+")
  colnames(temp_matrix2) <- c("Adenovirus", "Coronavirus", "Influenza A",
                              "Influenza B", "Parainfluenza", "RSV", 
                              "Rhinovirus","Year", "startweek", "Age")
  
  storage_2017 <- rbind(storage_2017, temp_matrix)
  storage_2017_ages <- rbind(storage_2017_ages, temp_matrix2)
  
}


####### 2016 ######

#specify the webpages
weeks_2016 <- data.frame(
  start = c(1,5,9,14,18,22,26,31,35,40,44,48),
  end = c(4,8,13,17,21,25,30,34,39,43,47,52)
)
# Create storage object
storage_2016 <- data.frame(matrix(ncol = 7))
colnames(storage_2016) <- c("Adenovirus", "Coronavirus", "Parainfluenza",
                            "Rhinovirus", "RSV","Year", "Week")
storage_2016_ages <- data.frame(matrix(ncol = 10))
colnames(storage_2016_ages) <- c("Adenovirus", "Coronavirus", "Influenza A",
                                 "Influenza B", "Parainfluenza", "RSV", 
                                 "Rhinovirus","Year", "startweek", "Age")

for(page in 1:nrow(weeks_2016)) {
  
  # define the webpage
  week_from <- weeks_2016[page,"start"]
  week_to <- weeks_2016[page,"end"]
  week_diff <- week_to - week_from
  # manually specify for the ones with odd names
  if(page == 7) {
    url <- paste0("https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2016/weeks-",week_from,"-to-",week_to)
  } else {
    url <- paste0("https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2016/laboratory-reports-of-respiratory-infections-made-to-phe-from-phe-and-nhs-laboratories-in-england-and-wales-weeks-",week_from,"-to-",week_to,"-2016")
  }
  # read in the webpage
  webpage <- read_html(url)
  #Using CSS selectors to scrape the rankings section
  rank_data_html <- html_nodes(webpage,'td')
  #Converting the ranking data to text
  rank_data <- html_text(rank_data_html)
  # Pick the required content
  strt_num <- which(rank_data == "Adenovirus*")
  end_num <- which(rank_data == "RSV" | rank_data == "Respiratory Syncytial Virus (RSV)" |
                     rank_data == "Respiratory syncytial virus" ) + (week_diff + 2)
  #create matrix
  temp_matrix <- data.frame(matrix(rank_data[strt_num:end_num[1]], ncol = 5, byrow = F))
  # remove total row, add week numbers
  temp_matrix <- temp_matrix[2:(week_diff + 2),]
  temp_matrix$Year <- 2016
  temp_matrix$Week <- week_from:week_to
  
  colnames(temp_matrix) <- c("Adenovirus", "Coronavirus", "Parainfluenza",
                             "Rhinovirus", "RSV","Year", "Week")
  
  # THIS IS FOR THE MONTHLY AGE DIST
  strt_num2 <- which(rank_data =="Adenovirus *")
  end_num2 <- which(rank_data == "Rhinovirus")[2] + 8
  temp_matrix2 <- data.frame(matrix(rank_data[strt_num2:end_num2], ncol = 7, byrow = F))
  temp_matrix2 <- temp_matrix2[2:7,]
  temp_matrix2$Year <- 2016
  temp_matrix2$startweek <- week_from
  temp_matrix2$Age <- c("Under 1", "1-4 Y", "5-14 Y", "15-44 Y", "45-64 Y", "65 Y+")
  colnames(temp_matrix2) <- c("Adenovirus", "Coronavirus", "Influenza A",
                              "Influenza B", "Parainfluenza", "RSV", 
                              "Rhinovirus","Year", "startweek", "Age")
  if(page == 5) {
    temp_matrix2[,"Parainfluenza"] <- c(151,119,18,66,81,1)
  }
  
  storage_2016 <- rbind(storage_2016, temp_matrix)
  storage_2016_ages <- rbind(storage_2016_ages, temp_matrix2)
  
}

# ####### 2015 ######
# 
# #specify the webpages
# weeks_2015 <- data.frame(
#   start = c(2,6,10,14,18,23,27,32,36,40,45,49),
#   end = c(5,9,13,17,22,26,31,35,39,44,48,53)
# )
# # Create storage object
# storage_2015 <- data.frame(matrix(ncol = 7))
# colnames(storage_2015) <- c("Adenovirus", "Coronavirus", "Parainfluenza",
#                             "Rhinovirus", "RSV","Year", "Week")
# storage_2015_ages <- data.frame(matrix(ncol = 10))
# colnames(storage_2015_ages) <- c("Adenovirus", "Coronavirus", "Influenza A",
#                                  "Influenza B", "Parainfluenza", "RSV", 
#                                  "Rhinovirus","Year", "startweek", "Age")
# 
# for(page in 1:nrow(weeks_2015)) {
#   
#   # define the webpage
#   week_from <- weeks_2015[page,"start"]
#   week_to <- weeks_2015[page,"end"]
#   week_diff <- week_to - week_from
#   # manually specify for the ones with odd names
#   if(page >= 7) {
#     url <- paste0("https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2015/laboratory-reports-of-respiratory-infections-made-to-phe-from-phe-and-nhs-laboratories-in-england-and-wales-weeks-",week_from,"-to-",week_to,"-2015")
#   } else {
#     url <-paste0("https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2015/laboratory-reports-of-respiratory-infections-made-to-cidsc-from-phe-and-nhs-laboratories-in-england-and-wales-weeks-",week_from,"-to-",week_to,"-2015")
#   }
#   # read in the webpage
#   webpage <- read_html(url)
#   #Using CSS selectors to scrape the rankings section
#   rank_data_html <- html_nodes(webpage,'td')
#   #Converting the ranking data to text
#   rank_data <- html_text(rank_data_html)
#   # Pick the required content
#   strt_num <-which(rank_data == "Adenovirus*")
#   end_num <- which(rank_data == "RSV" | rank_data == "Respiratory Syncytial Virus (RSV)" |
#                      rank_data == "Respiratory syncytial virus" ) + (week_diff + 2)
#   #create matrix
#   temp_matrix <- data.frame(matrix(rank_data[strt_num:end_num[1]], ncol = 5, byrow = F))
#   # remove total row, add week numbers
#   temp_matrix <- temp_matrix[2:(week_diff + 2),]
#   temp_matrix$Year <- 2015
#   temp_matrix$Week <- week_from:week_to
#   
#   colnames(temp_matrix) <- c("Adenovirus", "Coronavirus", "Parainfluenza", "Rhinovirus", "RSV","Year", "Week")
#   storage_2015 <- rbind(storage_2015, temp_matrix)
#   
#   # THIS IS FOR THE MONTHLY AGE DIST
#   strt_num2 <- which(rank_data =="Adenovirus *")
#   end_num2 <- which(rank_data == "Rhinovirus")[2] + 8
#   temp_matrix2 <- data.frame(matrix(rank_data[strt_num2:end_num2], ncol = 7, byrow = F))
#   temp_matrix2 <- temp_matrix2[2:7,]
#   temp_matrix2$Year <- 2015
#   temp_matrix2$startweek <- week_from
#   temp_matrix2$Age <- c("Under 1", "1-4 Y", "5-14 Y", "15-44 Y", "45-64 Y", "65 Y+")
#   colnames(temp_matrix2) <- c("Adenovirus", "Coronavirus", "Influenza A",
#                               "Influenza B", "Parainfluenza", "RSV", 
#                               "Rhinovirus","Year", "startweek", "Age")
#   
#   storage_2015_ages <- rbind(storage_2015_ages, temp_matrix2)
#   
# }

# ####### 2014 ######
# 
# #specify the webpages
# weeks_2014 <- data.frame(
#   start = c(23,27,31,36,40,45,49),
#   end = c(26,30,35,39,44,48,1)
# )
# # Create storage object
# storage_2014 <- data.frame(matrix(ncol=7))
# colnames(storage_2014) <- c("Adenovirus", "Coronavirus", "Parainfluenza", "Rhinovirus", "RSV","Year", "Week")
# storage_2014_ages <- data.frame(matrix(ncol=10))
# colnames(storage_2014_ages) <- c("Adenovirus", "Coronavirus", "Influenza A",
#                                  "Influenza B", "Parainfluenza", "RSV", 
#                                  "Rhinovirus","Year", "startweek", "Age")
# 
# for(page in 1:nrow(weeks_2014)){
#   # define the webpage
#   week_from <- weeks_2014[page,"start"]
#   week_to <- weeks_2014[page,"end"]
#   week_diff <- week_to - week_from
#   
#   if (page ==7){week_diff <- 4}
#   # manually specify for the ones with odd names
#   if(page ==7){
#     url <- "https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2014/laboratory-reports-of-respiratory-infections-made-to-cidsc-from-phe-and-nhs-laboratories-in-england-and-wales-weeks-49-to-52-2014-and-week-1-2015"
#    } else if(page>=4){
#      url <- paste0("https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2014/laboratory-reports-of-respiratory-infections-made-to-cidsc-from-phe-and-nhs-laboratories-in-england-and-wales-weeks-",week_from,"-to-",week_to)
#    } else{
#     url <-paste0("https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2014/laboratory-reports-of-respiratory-infections-made-to-cidsc-from-phe-and-nhs-laboratories-in-england-and-wales-weeks-",week_from,"-to-",week_to,"-2014")
#    }
#   # read in the webpage
#   webpage <- read_html(url)
#   #Using CSS selectors to scrape the rankings section
#   rank_data_html <- html_nodes(webpage,'td')
#   #Converting the ranking data to text
#   rank_data <- html_text(rank_data_html)
#   # Pick the required content
#   strt_num <-which(rank_data == "Adenovirus*")
#   end_num <- which(rank_data == "RSV" | rank_data == "Respiratory Syncytial Virus (RSV)" |
#                      rank_data == "Respiratory syncytial virus" ) + (week_diff +2)
#   #create matrix
# 
#   temp_matrix <- data.frame(matrix(rank_data[strt_num[1]:end_num[1]],ncol = 5, byrow=F))
#   # remove total row, add week numbers
#   
#   temp_matrix <- temp_matrix[2:(week_diff+2),]
#   if(page==7){
#     temp_matrix$Year <- c(2014,2014,2014,2014,2015)
#     temp_matrix$Week <- c(49,50,51,52,1)
#   }else{
#   temp_matrix$Year <- 2014
#   temp_matrix$Week <- week_from:week_to
#   }
#   colnames(temp_matrix) <- c("Adenovirus", "Coronavirus", "Parainfluenza", "Rhinovirus", "RSV","Year", "Week")
#   
#   # THIS IS FOR THE MONTHLY AGE DIST
#   if (page !=4){
#   strt_num2 <- which(rank_data =="Adenovirus *")
#   if (page >= 5){
#     end_num2 <- which(rank_data == "Rhinovirus")[2] + 8
#   }else {  end_num2 <- which(rank_data == "Respiratory syncytial virus") + 8}
# 
#   
#   
#   temp_matrix2 <- data.frame(matrix(rank_data[strt_num2:end_num2],ncol = 7, byrow=F))
#   temp_matrix2 <- temp_matrix2[2:7,]
#   temp_matrix2$Year <- 2014
#   temp_matrix2$startweek <- week_from
#   temp_matrix2$Age <- c("Under 1", "1-4 Y", "5-14 Y", "15-44 Y", "45-64 Y", "65 Y+")
#   colnames(temp_matrix2) <- c("Adenovirus", "Coronavirus", "Influenza A",
#                               "Influenza B", "Parainfluenza", "RSV", 
#                               "Rhinovirus","Year", "startweek", "Age")
#   storage_2014_ages <- rbind(storage_2014_ages, temp_matrix2)}
#   storage_2014 <- rbind(storage_2014, temp_matrix)
# 
# }



######COMBINE#####
all_data <- data.table(rbind(storage_2016, storage_2017, storage_2018,
                             storage_2019, storage_2020))
all_data <- all_data[!is.na(all_data$RSV), ]
all_data[, year_week := paste0(Year, "_", Week)]
all_data[which(all_data$Adenovirus == "–"), "Adenovirus"] <- 0
all_data[which(all_data$Coronavirus == "–"), "Coronavirus"] <- 0
all_data[which(all_data$Parainfluenza == "–"), "Parainfluenza"] <- 0
all_data[which(all_data$Rhinovirus == "–"), "Rhinovirus"] <- 0
all_data[which(all_data$RSV == "–"), "RSV"] <- 0

exclude <- c("2016_1", "2016_2", "2016_3", "2016_4", "2016_5", "2016_6",
             "2016_7", "2016_8", "2016_9", "2016_10", "2016_11", "2016_12",
             "2016_13", "2016_14", "2016_15", "2016_16", "2016_17", "2016_18",
             "2016_19", "2016_20", "2016_21", "2016_22", "2016_23", "2016_24",
             "2016_25", "2016_26", "2016_27", "2016_28", "2016_29", "2016_30",
             "2016_31", "2016_32", "2016_33", "2016_34")

all_data <- all_data[!(year_week %in% exclude), ]

# saveRDS(all_data, file = paste0(Sys.Date(),"-Respiratory viral defections by any method UK end 2020.RDS"))

# all_data_ages <- data.table(rbind(storage_2014_ages, storage_2015_ages, storage_2016_ages, storage_2017_ages,
#                             storage_2018_ages,storage_2019_ages,storage_2020_ages))
# all_data_ages<-all_data_ages[!is.na(all_data_ages$RSV),]
# all_data_ages$startweek <- as.character(all_data_ages$startweek)
# all_data_ages[nchar(startweek)==1, startweek := paste0("0",startweek)]
# 
# all_data_ages[, year_week := paste0(Year,startweek, "1")]
# all_data_ages[,year_week := as.Date(year_week, "%Y%W%w")]
# all_data_ages[,timestep := 1:nrow(all_data_ages)]
# 
# 
# all_data_ages[which(all_data_ages$Adenovirus == "–"),"Adenovirus"] <- 0
# all_data_ages[which(all_data_ages$Coronavirus == "–"), "Coronavirus"] <- 0
# all_data_ages[which(all_data_ages$Parainfluenza == "–"),"Parainfluenza"] <- 0
# all_data_ages[which(all_data_ages$Rhinovirus == "–"), "Rhinovirus"] <- 0
# all_data_ages[which(all_data_ages$RSV == "–"), "RSV"] <- 0
# all_data_ages[which(all_data_ages$"Influenza A" == "–"), "Influenza A"] <- 0
# all_data_ages[which(all_data_ages$"Influenza B"== "–"),"Influenza B"] <- 0
# 
# saveRDS(all_data_ages, file = paste0(Sys.Date(),"Respiratory viral defections by any method UK end 2020.RDS"))

all_data <- all_data[, date := ymd(paste0(Year, "-01", "-01")) + 7*(as.numeric(Week))]
all_data <- all_data[, month := as.Date(as.yearmon(date))]
all_data <- all_data[, year := year(month)]
all_data <- all_data[, c("RSV", "month", "year"), with = FALSE]
all_data <- all_data[, RSV := sum(as.numeric(RSV)), by = c("month", "year")]
all_data <- unique(all_data)

write.csv(all_data, file = here::here(
  "post_check", "exploratory_analyses", "surveillance",
  "UKHSA_reports_RSV.csv"), row.names = FALSE)
