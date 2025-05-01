##libraries
library(data.table)
library(plyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(zoo)
library(dplyr)
library(ggthemes)
options(scipen = 999)

##import data
data <- read.csv(paste0(here::here(
  "post_check", "exploratory_analyses", "surveillance"), "/VIW_FNT.csv"))
data <- as.data.table(data)
data <- data[COUNTRY_AREA_TERRITORY %in% c("United Kingdom, England",
             "United Kingdom, Wales", "United Kingdom, Scotland", 
             "United Kingdom, Northern Ireland")]
data <- data[order(as.Date(ISO_WEEKSTARTDATE, format = "%Y-%M-%D"))]
data <- data[, RSV_new := 
               ifelse(str_starts(data$OTHER_RESPVIRUS_DETAILS, "RSV"), 1, 
               ifelse(str_starts(data$OTHER_RESPVIRUS_DETAILS, "rsv"), 1, RSV))]
data <- data[, INF_new := INF_A + INF_B]
data <- data[, Date := ymd(ISO_WEEKSTARTDATE)]
data <- data[, week := floor_date(Date, unit = "week")]
data <- data[, total_flu := sum(INF_new, na.rm = TRUE), by = week]
data <- data[, total_rsv := sum(RSV_new, na.rm = TRUE), by = week]

flu_rsv <- rlang::duplicate(data)
flu_rsv <- flu_rsv[, .(week, total_flu, total_rsv)]
flu_rsv <- unique(flu_rsv)

latest_date <- flu_rsv[week == max(week)]

data_new <- read.csv(paste0(here::here(
  "post_check", "exploratory_analyses", "surveillance"), "/VIW_FNT_recent.csv"))
data_new <- as.data.table(data_new)
data_new <- data_new[COUNTRY_AREA_TERRITORY %in% c(
  "United Kingdom, England", "United Kingdom, Wales",
  "United Kingdom, Scotland", "United Kingdom, Northern Ireland")]
data_new <- data_new[order(as.Date(ISO_WEEKSTARTDATE, format = "%Y-%M-%D"))]
data_new <- data_new[, RSV_new := 
               ifelse(str_starts(data_new$OTHER_RESPVIRUS_DETAILS, "RSV"), 1, 
                      ifelse(str_starts(data_new$OTHER_RESPVIRUS_DETAILS, "rsv"),
                             1, RSV))]
data_new <- data_new[, INF_new := INF_A + INF_B]
data_new <- data_new[, Date := ymd(ISO_WEEKSTARTDATE)]
data_new <- data_new[, week := floor_date(Date, unit = "week")]
data_new <- data_new[, total_flu := sum(INF_new, na.rm = TRUE), by = week]
data_new <- data_new[, total_rsv := sum(RSV_new, na.rm = TRUE), by = week]

flu_rsv_new <- rlang::duplicate(data_new)
flu_rsv_new <- flu_rsv_new[, .(week, total_flu, total_rsv)]
flu_rsv_new <- unique(flu_rsv_new)

flu_rsv_new <- flu_rsv_new[week > latest_date$week]

flu_rsv <- rbind(flu_rsv, flu_rsv_new)

data2 <- read.csv(paste0(here::here(
  "post_check", "exploratory_analyses", "surveillance"),
  "/WHO-COVID-19-global-data.csv"))
data2 <- as.data.table(data2)
data2 <- data2[Country == "The United Kingdom"]
data2 <- data2[, Date := ymd(Date_reported)]
data2 <- data2[, week := floor_date(Date, unit = "week")]
data2 <- data2[, total_covid := sum(New_cases, na.rm = TRUE), by = week]

covid <- rlang::duplicate(data2)
covid <- covid[, .(week, total_covid)]
covid <- unique(covid)

latest_date_covid <- covid[week == max(week)]

data2_new <- read.csv(paste0(here::here(
  "post_check", "exploratory_analyses", "surveillance"),
  "/WHO-COVID-19-global-daily-data.csv"))
data2_new <- as.data.table(data2_new)
data2_new <- data2_new[Country == "United Kingdom of Great Britain and Northern Ireland"]
data2_new <- data2_new[, Date := ymd(Date_reported)]
data2_new <- data2_new[, week := floor_date(Date, unit = "week")]
data2_new <- data2_new[, total_covid := sum(New_cases, na.rm = TRUE), by = week]

covid_new <- rlang::duplicate(data2_new)
covid_new <- covid_new[, .(week, total_covid)]
covid_new <- unique(covid_new)

covid_new <- covid_new[week > latest_date_covid$week]

covid <- rbind(covid, covid_new)

coeff <- 200
all <- full_join(flu_rsv, covid, by = "week")
all <- all %>%
  mutate(covid_scaled = if_else(is.na(total_covid), NA, total_covid/coeff))
all <- all %>%
  subset(week >= as.Date("2016-09-01") & week <= as.Date("2024-08-31"))

##plot

rects <- tibble(
  xmin = seq(as.Date("2016-11-01"), as.Date("2023-11-01"), by = "year"),
  xmax = seq(as.Date("2017-03-01"), as.Date("2024-03-01"), by = "year"),
  ymin = 0,
  ymax = Inf
)

ggplot(data = all) + geom_line(aes(x = week, y = total_flu, col = "Influenza")) +
  geom_line(aes(x = week, y = total_rsv, col = "RSV")) + 
  geom_line(aes(x = week, y = covid_scaled, col = "COVID-19")) +
  scale_y_continuous(
    limits = c(0, round_any(max(all$total_rsv, all$total_flu), 2000)),
    breaks = seq(0, round_any(max(all$total_rsv, all$total_flu), 2000), by = 2000),
    sec.axis = sec_axis(trans = ~.*coeff, name = "COVID-19 Weekly Infections")) +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") + 
  geom_rect(data = rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.25, col = NA) +
  annotate(label = "Usual Transmission Period (Nov-Mar)", x = as.Date("2019-12-15"), 
           y = 5000, geom = "text", col = "black", size = 4) +
  labs(x = "Year", y = "RSV/Influenza Weekly Infections", col = "Disease") + 
  theme_bw(base_size = 15)

#save
ggsave(here::here("post_check", "plots", "exploratory_analyses",
                  "seasonality_weekly.png"), width = 12, height = 8)

#export data
write.csv(all, here::here("post_check", "exploratory_analyses", "surveillance",
            "seasonality_weekly.csv"), row.names = FALSE)
