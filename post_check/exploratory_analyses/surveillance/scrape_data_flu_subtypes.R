# scraping data flu subtypes
library(rvest)
library(data.table)
library(zoo)

###2020_data ####

#specify the webpages
weeks_2020 <- data.frame(
  start = c(3,7,11,16,20,24,29,33,38,42,46,50),
  end = c(6,10,15,19,23,28,32,37,41,45,49,2)
)
# Create storage object
storage_2020_flus <- data.frame(matrix(ncol = 4))
colnames(storage_2020_flus) <-  c("flu_A", "flu_B","year", "week")

for(page in 1:nrow(weeks_2020)){
  
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
  
  strt_num_A <- which(rank_data == "Influenza A")[1] + 1
  end_num_A <- which(rank_data == "Isolation")[1] - 2
  strt_num_B <- which(rank_data == "Influenza B")[1] + 1
  end_num_B <- which(rank_data == "Isolation")[2] - 2
  #create matrix
  temp_matrix <- data.frame(matrix(c(rank_data[strt_num_A:end_num_A],
                                     rank_data[strt_num_B:end_num_B]),
                                   ncol = 2, byrow = F))
  # remove total row, add week numbers
  if(page == 12){
    temp_matrix <- temp_matrix[2:(week_diff + 1),]
    temp_matrix$Year <- c(2020,2020,2020,2020,2021,2021)
    temp_matrix$Week <- c(50,51,52,53,1,2)
  } else {
    temp_matrix <- temp_matrix[1:(week_diff + 1),]
    temp_matrix$Year <- 2020
    temp_matrix$Week <- week_from:week_to
  }
  colnames(temp_matrix) <- c("flu_A", "flu_B","year", "week")
  
  storage_2020_flus <- rbind(storage_2020_flus, temp_matrix)
  
}


### 2019 data ####

#specify the webpages
weeks_2019 <- data.frame(
  start = c(1,5,9,14,18,23,27,31,36,40,44,49),
  end = c(4,8,13,17,22,26,30,35,39,43,48,2)
)

# Create storage object
storage_2019_flus <- data.frame(matrix(ncol = 4))
colnames(storage_2019_flus) <-  c("flu_A", "flu_B","year", "week")

for(page in 1:nrow(weeks_2019)){
  
  # define the webpage
  week_from <- weeks_2019[page,"start"]
  week_to <- weeks_2019[page,"end"]
  week_diff <- week_to - week_from
  if(page == 12) {week_diff <- 6}
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
  rank_data <- html_text(rank_data_html)
  #Converting the ranking data to text
  strt_num_A <- which(rank_data == "Influenza A")[1] + 1
  end_num_A <- which(rank_data == "Isolation")[1] - 2
  strt_num_B <- which(rank_data == "Influenza B")[1] + 1
  end_num_B <- which(rank_data == "Isolation")[2] - 2
 
  if(page == 12){
    temp_matrix <- data.frame(matrix(c(rank_data[strt_num_A:end_num_A],
                                       rank_data[strt_num_B:end_num_B]),
                                     ncol = 2, byrow = F))
    temp_matrix$Year <- c(2019,2019,2019,2019,2020,2020)
    temp_matrix$Week <- c(49,50,51,52,1,2)
  } else {
    temp_matrix <- data.frame(matrix(c(rank_data[strt_num_A:end_num_A],
                                       rank_data[strt_num_B:end_num_B]),
                                     ncol = 2, byrow = F))
    # remove total row, add week numbers
    temp_matrix$Year <- 2019
    temp_matrix$Week <- week_from:week_to
    colnames(temp_matrix) <- c("flu_A", "flu_B","year", "week")
  }
  colnames(temp_matrix) <- c("flu_A", "flu_B","year", "week")
  
  storage_2019_flus <- rbind(storage_2019_flus, temp_matrix)
  
  
}


####### 2018 ##### 

#specify the webpages
weeks_2018 <- data.frame(
  start = c(1,5,9,14,18,22,27,31,36,40,44,49),
  end = c(4,8,13,17,21,26,30,35,39,43,48,52)
)
# Create storage object
# Create storage object
storage_2018_flus <- data.frame(matrix(ncol = 4))
colnames(storage_2018_flus) <-  c("flu_A", "flu_B","year", "week")

for(page in 1:nrow(weeks_2018)){
  
  # define the webpage
  week_from <- weeks_2018[page,"start"]
  week_to <- weeks_2018[page,"end"]
  week_diff <- week_to - week_from
  # manually specify for the ones with odd names
  if(page > 2){
    url <- paste0("https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2018/laboratory-reports-of-respiratory-infections-made-to-phe-from-phe-and-nhs-laboratories-in-england-and-wales-weeks-",week_from,"-to-",week_to,"-2018")
  } else {
    url <- paste0("https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2018/reports-of-respiratory-infections-made-to-phe-from-phe-and-nhs-laboratories-in-england-and-wales-weeks-",week_from,"-to-",week_to,"-2018")
  }
  # read in the webpage
  webpage <- read_html(url)
  #Using CSS selectors to scrape the rankings section
  rank_data_html <- html_nodes(webpage,'td')
  #Converting the ranking data to text
  rank_data <- html_text(rank_data_html)
  # Pick the required content
  strt_num_A <- which(rank_data == "Influenza A")[1] + 1
  end_num_A <- which(rank_data == "Isolation")[1] - 2
  strt_num_B <- which(rank_data == "Influenza B")[1] + 1
  end_num_B <- which(rank_data == "Isolation")[2] - 2
  #create matrix
  temp_matrix <- data.frame(matrix(c(rank_data[strt_num_A:end_num_A],
                                     rank_data[strt_num_B:end_num_B]),
                                   ncol = 2, byrow = F))
  # remove total row, add week numbers
  temp_matrix$Year <- 2018
  temp_matrix$Week <- week_from:week_to
  colnames(temp_matrix) <- c("flu_A", "flu_B","year", "week")
  
  storage_2018_flus <- rbind(storage_2018_flus, temp_matrix)

}


####### 2017 ######

#specify the webpages
weeks_2017 <- data.frame(
  start = c(1,5,9,14,18,22,27,31,35,40,44,48),
  end = c(4,8,13,17,21,26,30,34,39,43,47,52)
)
# Create storage object
storage_2017_flus <- data.frame(matrix(ncol = 4))
colnames(storage_2017_flus) <-  c("flu_A", "flu_B","year", "week")

for(page in 1:nrow(weeks_2017)){
  
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
  strt_num_A <-which(rank_data == "Influenza A")[1] + 1
  end_num_A <-which(rank_data == "Isolation")[1] - 2
  strt_num_B <-which(rank_data == "Influenza B")[1] + 1
  end_num_B <-which(rank_data == "Isolation")[2] - 2
  #create matrix
  temp_matrix <- data.frame(matrix(c(rank_data[strt_num_A:end_num_A],
                                     rank_data[strt_num_B:end_num_B]),
                                   ncol = 2, byrow = F))
  # remove total row, add week numbers
  temp_matrix$Year <- 2017
  temp_matrix$Week <- week_from:week_to
  colnames(temp_matrix) <- c("flu_A", "flu_B","year", "week")
  
  storage_2017_flus <- rbind(storage_2017_flus, temp_matrix)
  
}


####### 2016 ######

#specify the webpages
weeks_2016 <- data.frame(
  start = c(1,5,9,14,18,22,26,31,35,40,44,48),
  end = c(4,8,13,17,21,25,30,34,39,43,47,52)
)

# Create storage object
storage_2016_flus <- data.frame(matrix(ncol = 4))
colnames(storage_2016_flus) <-  c("flu_A", "flu_B","year", "week")

for(page in 1:nrow(weeks_2016)){
  
  # define the webpage
  week_from <- weeks_2016[page,"start"]
  week_to <- weeks_2016[page,"end"]
  week_diff <- week_to - week_from
  # manually specify for the ones with odd names
  if(page==7){
    url <- paste0("https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2016/weeks-",week_from,"-to-",week_to)
  } else{
    url <- paste0("https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2016/laboratory-reports-of-respiratory-infections-made-to-phe-from-phe-and-nhs-laboratories-in-england-and-wales-weeks-",week_from,"-to-",week_to,"-2016")
  }
  # read in the webpage
  webpage <- read_html(url)
  #Using CSS selectors to scrape the rankings section
  rank_data_html <- html_nodes(webpage,'td')
  #Converting the ranking data to text
  rank_data <- html_text(rank_data_html)
  # Pick the required content
  strt_num_A <-which(rank_data == "Influenza A")[1] + 1
  strt_num_B <-which(rank_data == "Influenza B")[1] + 1
  # - 3 here as theres an empty space in the table
  if (page == 1){
    end_num_A <-which(rank_data == "Isolation")[1] - 3
    end_num_B <-which(rank_data == "Isolation")[2] - 3
  } else {
    end_num_A <-which(rank_data == "Isolation")[1] - 2
    end_num_B <-which(rank_data == "Isolation")[2] - 2
  }
  #create matrix
  temp_matrix <- data.frame(matrix(c(rank_data[strt_num_A:end_num_A],
                                     rank_data[strt_num_B:end_num_B]),
                                   ncol = 2, byrow = F))
  # remove total row, add week numbers
  temp_matrix$Year <- 2016
  temp_matrix$Week <- week_from:week_to
  colnames(temp_matrix) <- c("flu_A", "flu_B","year", "week")
  
  storage_2016_flus <- rbind(storage_2016_flus, temp_matrix)
  
}


# ####### 2015 ######
# 
# #specify the webpages
# weeks_2015 <- data.frame(
#   start = c(2,6,10,14,18,23,27,32,36,40,45,49),
#   end = c(5,9,13,17,22,26,31,35,39,44,48,53)
# )
# # Create storage object
# storage_2015_flus <- data.frame(matrix(ncol=4))
# colnames(storage_2015_flus) <-  c("flu_A", "flu_B","year", "week")
# 
# for(page in 1: nrow(weeks_2015)){
#   # define the webpage
#   week_from <- weeks_2015[page,"start"]
#   week_to <- weeks_2015[page,"end"]
#   week_diff <- week_to - week_from
#   # manually specify for the ones with odd names
#   if(page>=7){
#     url <- paste0("https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2015/laboratory-reports-of-respiratory-infections-made-to-phe-from-phe-and-nhs-laboratories-in-england-and-wales-weeks-",week_from,"-to-",week_to,"-2015")
#   } else{
#     url <-paste0("https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2015/laboratory-reports-of-respiratory-infections-made-to-cidsc-from-phe-and-nhs-laboratories-in-england-and-wales-weeks-",week_from,"-to-",week_to,"-2015")
#   }
#   # read in the webpage
#   webpage <- read_html(url)
#   #Using CSS selectors to scrape the rankings section
#   rank_data_html <- html_nodes(webpage,'td')
#   #Converting the ranking data to text
#   rank_data <- html_text(rank_data_html)
#   # Pick the required content
#   strt_num_A <-which(rank_data == "Influenza A" | rank_data =="INFLUENZA A")[1] +1
#   end_num_A <-which(rank_data == "Isolation")[1] -2
#   strt_num_B <-which(rank_data == "Influenza B" | rank_data =="INFLUENZA B")[1] +1
#   end_num_B <-which(rank_data == "Isolation")[2] -2
#   #create matrix
#   temp_matrix <- data.frame(matrix(c(rank_data[strt_num_A:end_num_A],
#                                      rank_data[strt_num_B:end_num_B]),
#                                    ncol = 2, byrow=F))
#   # remove total row, add week numbers
#   temp_matrix$Year <- 2015
#   temp_matrix$Week <- week_from:week_to
#   colnames(temp_matrix) <- c("flu_A", "flu_B","year", "week")
#   
#   storage_2015_flus <- rbind(storage_2015_flus, temp_matrix)
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
# storage_2014_flus <- data.frame(matrix(ncol=4))
# colnames(storage_2014_flus) <-  c("flu_A", "flu_B","year", "week")
# 
# for(page in 1: nrow(weeks_2014)){
#   # define the webpage
#   week_from <- weeks_2014[page,"start"]
#   week_to <- weeks_2014[page,"end"]
#   week_diff <- week_to - week_from
#   
#   if (page ==7){week_diff <- 4}
#   # manually specify for the ones with odd names
#   if(page ==7){
#     url <- "https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2014/laboratory-reports-of-respiratory-infections-made-to-cidsc-from-phe-and-nhs-laboratories-in-england-and-wales-weeks-49-to-52-2014-and-week-1-2015"
#   } else if(page>=4){
#     url <- paste0("https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2014/laboratory-reports-of-respiratory-infections-made-to-cidsc-from-phe-and-nhs-laboratories-in-england-and-wales-weeks-",week_from,"-to-",week_to)
#   } else{
#     url <-paste0("https://www.gov.uk/government/publications/respiratory-infections-laboratory-reports-2014/laboratory-reports-of-respiratory-infections-made-to-cidsc-from-phe-and-nhs-laboratories-in-england-and-wales-weeks-",week_from,"-to-",week_to,"-2014")
#   }
#   # read in the webpage
#   webpage <- read_html(url)
#   #Using CSS selectors to scrape the rankings section
#   rank_data_html <- html_nodes(webpage,'td')
#   #Converting the ranking data to text
#   rank_data <- html_text(rank_data_html)
#   # Pick the required content
#   strt_num_A <-which(rank_data == "Influenza A" | rank_data =="INFLUENZA A")[1] +1
#   end_num_A <-which(rank_data == "Isolation")[1] -2
#   strt_num_B <-which(rank_data == "Influenza B" | rank_data =="INFLUENZA B")[1] +1
#   end_num_B <-which(rank_data == "Isolation")[2] -2
#   #create matrix
#   temp_matrix <- data.frame(matrix(c(rank_data[strt_num_A:end_num_A],
#                                      rank_data[strt_num_B:end_num_B]),
#                                    ncol = 2, byrow=F))
#   if(page==7){
#     temp_matrix$Year <- c(2014,2014,2014,2014,2015)
#     temp_matrix$Week <- c(49,50,51,52,1)
#   }else{
#     temp_matrix$Year <- 2014
#     temp_matrix$Week <- week_from:week_to
#   }
#   colnames(temp_matrix) <- c("flu_A", "flu_B","year", "week")
#   storage_2014_flus <- rbind(storage_2014_flus, temp_matrix)
#   
# }



######COMBINE#####

all_data <- data.table(rbind(storage_2016_flus, storage_2017_flus,
                             storage_2018_flus,storage_2019_flus,
                             storage_2020_flus))
all_data <- all_data[!is.na(all_data$flu_A), ]
all_data[, year_week := paste0(year, "_", week)]
all_data[which(all_data$flu_A == "–"), "flu_A"] <- 0
all_data[which(all_data$flu_B == "–"), "flu_B"] <- 0
all_data$flu_A <- as.numeric(all_data$flu_A)
all_data$flu_B <- as.numeric(all_data$flu_B)
all_data[, flu := flu_A + flu_B]
setcolorder(all_data, c("flu", "flu_A", "flu_B", "year", "week", "year_week"))

exclude <- c("2016_1", "2016_2", "2016_3", "2016_4", "2016_5", "2016_6",
             "2016_7", "2016_8", "2016_9", "2016_10", "2016_11", "2016_12",
             "2016_13", "2016_14", "2016_15", "2016_16", "2016_17", "2016_18",
             "2016_19", "2016_20", "2016_21", "2016_22", "2016_23", "2016_24",
             "2016_25", "2016_26", "2016_27", "2016_28", "2016_29", "2016_30",
             "2016_31", "2016_32", "2016_33", "2016_34")

all_data <- all_data[!(year_week %in% exclude), ]

all_data <- all_data[, date := ymd(paste0(year, "-01", "-01")) + 7*(as.numeric(week))]
all_data <- all_data[, month := as.Date(as.yearmon(date))]
all_data <- all_data[, year := year(month)]
all_data <- all_data[, c("flu", "month", "year"), with = FALSE]
all_data <- all_data[, flu := sum(as.numeric(flu)), by = c("month", "year")]
all_data <- unique(all_data)

write.csv(all_data, file = here::here(
  "post_check", "exploratory_analyses", "surveillance",
  "UKHSA_reports_flu.csv"), row.names = FALSE)
