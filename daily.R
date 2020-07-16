library(readr)
library(knitr)
library(tidyverse)
library(zoo)
library(ggrepel)
library(viridis)
library(RColorBrewer)


#Use COUNTRIES dataset if you want to look at cases since 1/22
#Use COUNTRIES_100 data to look at cases >100 from day 0
#you must run all of the code below in order to create these datasets
#make sure to set your wd to the proper location 
#download case data from JHU github
update_data <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
raw_data <- read_csv(update_data)
#colnames(raw_data) <- c("province", "country", "lat", "long", seq(as.Date("2020-01-22"), Sys.Date(), by = "days")) #run this if late PM
colnames(raw_data) <- c("province", "country", "lat", "long", seq(as.Date("2020-01-22"), Sys.Date()-1, by = "days")) #run this in AM

#create subset of each country of interest
canada <- subset(raw_data, country == "Canada")
china_all <- subset(raw_data, country == "China")
china.nohubei <- subset(china_all, province != "Hubei")
china.nohubei$country = "China.nohubei"
france <- subset(raw_data, country == "France")
germany <- subset(raw_data, country == "Germany")
hubei <- subset(raw_data, province == "Hubei")
iran <- subset(raw_data, country == "Iran")
italy <- subset(raw_data, country == "Italy")
japan <- subset(raw_data, country == "Japan")
korea <- subset(raw_data, country == "Korea, South")
netherlands <- subset(raw_data, country == "Netherlands")
norway <- subset(raw_data, country=="Norway")
spain <- subset(raw_data, country == "Spain")
sweden <- subset(raw_data, country == "Sweden")
switzerland <- subset(raw_data, country == "Switzerland")
uk <- subset(raw_data, country == "United Kingdom")
us <- subset(raw_data, country == "US")

#create global dataset for manipulation based on raw data
all_countries <- rbind(canada, china.nohubei, france, germany, hubei, iran, italy, japan, korea,
                       netherlands, norway, spain, sweden, switzerland, uk, us)
#View(all_countries)

#for US and china.nohubei, must sum cases over all province/states
#first put data into long format 
all_countries_long <- all_countries %>% gather(date, cases, colnames(raw_data)[5:ncol(raw_data)])
#convert date (chr) into Date
all_countries_long$date <- as.Date(as.numeric(all_countries_long$date), origin="1970-01-01")
#sum cases for us, france, and china.nohubei
canada_sum <- all_countries_long %>%
  subset(country=="Canada") %>% group_by(date) %>%
  summarise(cases = sum(cases, na.rm = FALSE))
#head(china.nohubei_sum)

china.nohubei_sum <- all_countries_long %>% 
  subset(country=="China.nohubei") %>% group_by(date) %>%
  summarise(cases = sum(cases, na.rm = FALSE))
#head(china.nohubei_sum)

france_sum <- all_countries_long %>% 
  subset(country=="France") %>% group_by(date) %>%
  summarise(cases = sum(cases, na.rm = FALSE))
#head(france_sum)

netherlands_sum <- all_countries_long %>%
  subset(country=="Netherlands") %>% group_by(date) %>%
  summarise(cases=sum(cases, na.rm=FALSE))

uk_sum <- all_countries_long %>% 
  subset(country=="United Kingdom") %>% group_by(date) %>%
  summarise(cases = sum(cases, na.rm = FALSE))

us_sum <- all_countries_long %>% 
  subset(country=="US") %>% group_by(date) %>%
  summarise(cases = sum(cases, na.rm = FALSE))
#head(us_sum)
#create new datasets for each country
#i realize there's probably a faster way to do this but this gets it done
canada <- cbind(rep(canada[1,2]), canada_sum)
china.nohubei <- cbind(rep(china.nohubei[1,2]), china.nohubei_sum)
france <- cbind(rep(france[1,2]), france_sum)
netherlands <- cbind(rep(netherlands[1,2]), netherlands_sum)
uk <- cbind(rep(uk[1,2]), uk_sum) 
us <- cbind(rep(us[1,2]), us_sum)  

germany <- all_countries_long %>% subset(country == "Germany")
germany <- germany[,c(2,5,6)]
#head(germany)

hubei <- all_countries_long %>% subset(province == "Hubei")
hubei$country = "Hubei"
hubei <- hubei[,c(2,5,6)]
#head(hubei)

italy <- all_countries_long %>% subset(country == "Italy")
italy <- italy[,c(2,5,6)]
#head(italy)

iran <- all_countries_long %>% subset(country == "Iran")
iran <- iran[,c(2,5,6)]
#head(iran)

japan <- all_countries_long %>% subset(country == "Japan")
japan <- japan[,c(2,5,6)]
#head(japan)

korea <- all_countries_long %>% subset(country == "Korea, South")
korea$country = "Korea"  
korea <- korea[,c(2,5,6)]
#head(korea)

norway <- all_countries_long %>% subset(country == "Norway")
norway <- norway[,c(2,5,6)]

spain <- all_countries_long %>% subset(country == "Spain")
spain <- spain[,c(2,5,6)]
#head(spain)

sweden <- all_countries_long %>% subset(country == "Sweden")
sweden <- sweden[,c(2,5,6)]

switzerland <- all_countries_long %>% subset(country == "Switzerland")
switzerland <- switzerland[,c(2,5,6)]

#create clean dataset with just countries and cases
countries <- rbind(canada, china.nohubei, france, germany, hubei, iran, italy, japan, korea,
                   netherlands, norway, spain, sweden, switzerland, uk, us)
#View(countries)

#calculate first_diff of cases
countries <- countries %>% group_by(country) %>% 
  mutate(first_diff = cases - lag(cases))

#add column for ln(cases)
countries$log_cum_cases <- log(countries$cases)

#first_diff of ln(cases)
countries <- countries %>% group_by(country) %>% 
  mutate(first_diff_log_cases = log_cum_cases - lag(log_cum_cases))


#create day variable
countries <- countries %>% group_by(country) %>%
  mutate(day = rep(1:7, length.out=length(country)))

#3d MA for each value
#countries <- countries %>% group_by(country) %>% 
#  mutate(three_MA = rollmean(cases, 3, align="center", fill=cases))

#first_diff for 3d MA
#countries <- countries %>% group_by(country) %>% 
#  mutate(first_diff_three_MA = three_MA - lag(three_MA))

#log 3d MA
#countries$log_three_MA <- log(countries$three_MA)

#first_diff of log 3d MA
#countries <- countries %>% group_by(country) %>% 
#  mutate(first_diff_log_three_MA = log_three_MA - lag(log_three_MA))

####################################################
#create dataset starting with cases > 100 at time 0#
####################################################

countries_100 <- countries %>% subset(cases >99) 


countries_100 <- countries_100 %>% group_by(country) %>%
  mutate(time = as.numeric(date) - min(as.numeric(date)))

#create time_squared variable
countries_100$time_sq <- countries_100$time^2

#create time_3 variable
countries_100$time_3 <- countries_100$time^3

#create root_time variable
countries_100$time_sqrt <- sqrt(countries_100$time)

#create t^1/3 variable
countries_100$time_cubrt <- countries_100$time^(1/3)

#create t^-1 variable
countries_100$time_inv <- countries_100$time^(-1)

#countries_100 <- countries_100 %>% group_by(country) %>% slice(-1)
#head(countries_100)
####################################################
####################################################
####################################################

write_csv(countries_100, "Daily_Datafiles/daily_latest.csv")

####################################################
####################################################???
#plot log of cases over time
colorcount <- length(unique(countries_100$country))
mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(colorcount)
mylines <- c("solid", "22", "42", "44", "13", "1343", "73", "2262", "12223242",
             "F282", "F4448444", "224282F2", "F1", "22", "44", "2263")

plot.daily_log_cases <- 
  ggplot(countries_100, aes(x=time, y=log_cum_cases, color=country)) +
  geom_line(aes(linetype=country, color=country), size=1) +
  scale_fill_manual(values = mycolors) +
  scale_linetype_manual(values = mylines)+
  #geom_point(aes(shape=country, color=country)) +
  ggtitle("Log of Cumulative Cases Since 100th Case Reported") +
  xlab("Days Since 100th Case Reported") +
  ylab("Log of Cumulative Cases") +
  theme_bw()  +
  theme(legend.position = "none") +
  geom_text_repel(data = countries_100[countries_100$date  == Sys.Date()-1,],
                  aes(label = country), hjust = 0.7, vjust = 1)

plot.daily_log_cases

# countries.mature <- countries_100[countries_100$country %in% c("China.nohubei", "Hubei", "Iran", "Italy", "Korea", "US"),]
# plot.first_diff <- 
#   ggplot(countries.mature, aes(x=time, y=first_diff_log_cases)) +
#   geom_line(aes(linetype=country, color=country), size=1)
# plot.first_diff

##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################

#death panel setup
#setwd("C:/Users/thia9/EconoMedRx Dropbox/Cynthia G/E-value Team/COVID daily data")
#download death data from JHU github
update_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
raw_deaths <- read_csv(update_deaths)

colnames(raw_deaths) <- c("province", "country", "lat", "long", seq(as.Date("2020-01-22"), Sys.Date()-1, by = "days")) #run this if AM
#colnames(raw_deaths) <- c("province", "country", "lat", "long", seq(as.Date("2020-01-22"), Sys.Date(), by = "days")) #run this if late PM

deaths.canada <- subset(raw_deaths, country=="Canada")
deaths.china_all <- subset(raw_deaths, country == "China")
deaths.china.nohubei <- subset(deaths.china_all, province != "Hubei")
deaths.china.nohubei$country = "China.nohubei"
deaths.france <- subset(raw_deaths, country == "France")
deaths.germany <- subset(raw_deaths, country == "Germany")
deaths.hubei <- subset(raw_deaths, province == "Hubei")
deaths.italy <- subset(raw_deaths, country == "Italy")
deaths.iran <- subset(raw_deaths, country == "Iran")
deaths.japan <- subset(raw_deaths, country == "Japan")
deaths.korea <- subset(raw_deaths, country == "Korea, South")
deaths.netherlands <- subset(raw_deaths, country == "Netherlands")
deaths.norway <- subset(raw_deaths, country == "Norway")
deaths.spain <- subset(raw_deaths, country == "Spain")
deaths.sweden <- subset(raw_deaths, country == "Sweden")
deaths.switzerland <- subset(raw_deaths, country == "Switzerland")
deaths.uk <- subset(raw_deaths, country == "United Kingdom")
deaths.us <- subset(raw_deaths, country == "US")

all_deaths <- rbind(deaths.canada, deaths.china.nohubei, deaths.france, deaths.germany, deaths.hubei, 
                    deaths.iran, deaths.italy, deaths.japan, deaths.korea, deaths.netherlands,
                    deaths.norway, deaths.spain, deaths.sweden, deaths.switzerland, deaths.uk, deaths.us)
all_deaths_long <- all_deaths %>% gather(date, deaths, colnames(raw_deaths)[5:ncol(raw_deaths)])
all_deaths_long$date <- as.Date(as.numeric(all_deaths_long$date), origin="1970-01-01")

#sum deaths for us, france, netherlands, and china.nohubei
deaths.canada_sum <- all_deaths_long %>% 
  subset(country=="Canada") %>% group_by(date) %>%
  summarise(deaths = sum(deaths, na.rm = FALSE))

deaths.netherlands_sum <- all_deaths_long %>%
  subset(country=="Netherlands") %>% group_by(date) %>%
  summarise(deaths = sum(deaths, na.rm = FALSE))

deaths.us_sum <- all_deaths_long %>% 
  subset(country=="US") %>% group_by(date) %>%
  summarise(deaths = sum(deaths, na.rm = FALSE))

deaths.france_sum <- all_deaths_long %>% 
  subset(country=="France") %>% group_by(date) %>%
  summarise(deaths = sum(deaths, na.rm = FALSE))

deaths.china.nohubei_sum <- all_deaths_long %>% 
  subset(country=="China.nohubei") %>% group_by(date) %>%
  summarise(deaths = sum(deaths, na.rm = FALSE))

deaths.uk_sum <- all_deaths_long %>% 
  subset(country=="United Kingdom") %>% group_by(date) %>%
  summarise(deaths = sum(deaths, na.rm = FALSE))

#create new datasets for each country
#i realize there's probably a faster way to do this but this gets it done

deaths.canada <- cbind(rep(deaths.canada[1,2]), deaths.canada_sum)
deaths.china.nohubei <- cbind(rep(deaths.china.nohubei[1,2]), deaths.china.nohubei_sum)
deaths.netherlands <- cbind(rep(deaths.netherlands[1,2]), deaths.netherlands_sum)
deaths.us <- cbind(rep(deaths.us[1,2]), deaths.us_sum)
deaths.uk <- cbind(rep(deaths.uk[1,2]), deaths.uk_sum)
deaths.france <- cbind(rep(deaths.france[1,2]), deaths.france_sum)

deaths.germany <- all_deaths_long %>% subset(country == "Germany")
deaths.germany <- deaths.germany[,c(2,5,6)]

deaths.hubei <- all_deaths_long %>% subset(province == "Hubei")
deaths.hubei$country = "Hubei"
deaths.hubei <- deaths.hubei[,c(2,5,6)]

deaths.iran <- all_deaths_long %>% subset(country == "Iran")
deaths.iran <- deaths.iran[,c(2,5,6)]

deaths.italy <- all_deaths_long %>% subset(country == "Italy")
deaths.italy <- deaths.italy[,c(2,5,6)]

deaths.japan <- all_deaths_long %>% subset(country == "Japan")
deaths.japan <- deaths.japan[,c(2,5,6)]

deaths.korea <- all_deaths_long %>% subset(country == "Korea, South")
deaths.korea$country = "Korea"  
deaths.korea <- deaths.korea[,c(2,5,6)]

deaths.spain <- all_deaths_long %>% subset(country == "Spain")
deaths.spain <- deaths.spain[,c(2,5,6)]

deaths.norway <- all_deaths_long %>% subset(country == "Norway")
deaths.norway <- deaths.norway[,c(2,5,6)]

deaths.sweden <- all_deaths_long %>% subset(country == "Sweden")
deaths.sweden <- deaths.sweden[,c(2,5,6)]

deaths.switzerland <- all_deaths_long %>% subset(country == "Switzerland")
deaths.switzerland <- deaths.switzerland[,c(2,5,6)]

#create clean dataset with just countries and deaths
deaths.countries <- rbind(deaths.canada, deaths.china.nohubei, deaths.france, deaths.germany, deaths.hubei, 
                          deaths.iran, deaths.italy, deaths.japan, deaths.korea, deaths.netherlands,
                          deaths.norway, deaths.spain, deaths.sweden, deaths.switzerland, deaths.uk, deaths.us)
#View(countries)

#calculate first_diff of deaths
deaths.countries <- deaths.countries %>% group_by(country) %>% 
  mutate(first_diff = deaths - lag(deaths))

#add column for ln(deaths)
deaths.countries$log_cum_deaths <- log(deaths.countries$deaths)

#first_diff of ln(deaths)
deaths.countries <- deaths.countries %>% group_by(country) %>% 
  mutate(first_diff_log_deaths = log_cum_deaths - lag(log_cum_deaths))

deaths.countries <- deaths.countries %>% group_by(country) %>%
  mutate(day = rep(1:7, length.out=length(country)))
####################################################
#create dataset starting with deaths > 10 at time 0#
####################################################

deaths.countries_10 <- deaths.countries %>% subset(deaths >9) 

#%>%
# group_by(country) %>% slice(-n()) #this code was to deal with 3dMA data

deaths.countries_10 <- deaths.countries_10 %>% group_by(country) %>%
  mutate(time = as.numeric(date) - min(as.numeric(date)))

#create time_squared variable
deaths.countries_10$time_sq <- deaths.countries_10$time^2
deaths.countries_10$time_3 <- deaths.countries_10$time^3
#create root_time variable
deaths.countries_10$time_sqrt <- sqrt(deaths.countries_10$time)

#create t^1/3 variable
deaths.countries_10$time_cubrt <- deaths.countries_10$time^(1/3)

#create t^-1 variable
deaths.countries_10$time_inv <- deaths.countries_10$time^(-1)

write_csv(deaths.countries_10, "Daily_Datafiles/deaths_latest.csv")


#plot log cumulative deaths over time
plot.daily_log_deaths <- 
  ggplot(deaths.countries_10, aes(x=time, y=log_cum_deaths, col=country)) +
  geom_line(aes(linetype=country, color=country), size=1) +
  scale_fill_manual(values = mycolors) +
  scale_linetype_manual(values = mylines) +
  ggtitle("Log of Cumulative Deaths since 10th Death Reported") +
  xlab("Days since 10th Death Reported") +
  ylab("Log of Cumulative Deaths") +
  theme_bw() +
  theme(legend.position = "none") +
  geom_text_repel(data = deaths.countries_10[deaths.countries_10$date  == Sys.Date()-1,],
                  aes(label = country), hjust = 0.7, vjust = 1)

plot.daily_log_deaths

# deaths.countries.mature <- deaths.countries_10[deaths.countries_10$country %in% c("China.nohubei", "Hubei", "Iran", "Italy", "Korea", "US"),]
# plot.first_diff.deaths <- 
#   ggplot(deaths.countries.mature, aes(x=time, y=first_diff_log_deaths)) +
#   geom_point(aes(color=country), size=3) +
#   xlim(34,46) +
#   xlab("days since 10th death reported") +
#   ylab("first difference of ln(cum_deaths)") +
#   ggtitle("first diff of log deaths over time")+
#   theme_bw()
# plot.first_diff.deaths

#plot of first diff log deaths
#ggplot(deaths.countries_10, aes(x=time, y=first_diff_log_deaths)) +
#  geom_point() +
#    ggtitle("First Difference Log(Deaths)")+
#  xlab("Days since 10th Reported Death") +
#  ylab("first difference of ln(cumulative deaths)") +
#  theme_bw()

###############################
#calculate total global deaths#
###############################

raw_deaths_long <- raw_deaths %>% gather(date, deaths, colnames(raw_deaths)[5:ncol(raw_deaths)])
colnames(raw_deaths) <- c("province", "country", "lat", "long", seq(as.Date("2020-01-22"), Sys.Date()-1, by = "days")) #run this if AM

raw_deaths_long$date <- as.Date(as.numeric(raw_deaths_long$date), origin="1970-01-01")

total_global_deaths <- raw_deaths_long %>% group_by(country, date) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE))

total_global_deaths <- total_global_deaths[,-1]

total_global_deaths <- raw_deaths_long %>% group_by(date) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE))

total_global_deaths$log_deaths <- log(total_global_deaths$deaths)

#head(total_global_deaths)
#plot.total_log_deaths_global <-   
#ggplot(total_global_deaths, aes(x=date, y=log_deaths)) +
#    geom_point() +
#    ggtitle("Global Log(Deaths) Over Time") +
#    xlab("Date") +
#    ylab("Log of Cumulative Deaths") +
#    theme_bw()
#plot.total_log_deaths_global

##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################

#create time series panels for cases and deaths
#setwd("EconoMedRx Dropbox/Roy Zawadzki/E-value Team/COVID daily data/")
#covid_panel_cases <- countries_100
setwd("~/daily_scripts")
covid_panel_cases <- read_csv("Daily_Datafiles/daily_latest.csv")
covid_panel_cases[is.na(covid_panel_cases)] <- 0
covid_panel_cases <- covid_panel_cases %>% arrange(country)


#head(covid_panel_cases)

############################
#create country indicators #
############################
country_names <- c("canada", "china.nohubei", "france", "germany", "hubei", "iran", "italy", "japan", 
                   "korea", "netherlands", "norway", "spain", "sweden", "switzerland", "uk", "us")
country_ind_names <- paste0(c(country_names), rep("_indicator", length(country_names)))
# creating dummies
covid_panel_cases$country_factor <- factor(covid_panel_cases$country)#, ordered = TRUE, levels = country_names)
dummy_country <- model.matrix(~0+covid_panel_cases$country_factor)
attr(dummy_country, "dimnames")[[2]] <- country_ind_names

# adding to the dataset, add [,-1] to drop a column of dummies for hubei
covid_panel_cases <- cbind(covid_panel_cases, dummy_country)
#head(covid_panel_cases)

##########################
#create time  indicators #
##########################

time_ind_names <- as.vector(paste0("t", 0:max(covid_panel_cases$time)))
# creating dummies
covid_panel_cases$time_factor <- as.factor(covid_panel_cases$time)
dummy_time <- model.matrix(~0+covid_panel_cases$time_factor)
attr(dummy_time, "dimnames")[[2]] <- time_ind_names

# adding to the dataset and add [,-1] to drop a column of dummies
covid_panel_cases <- cbind(covid_panel_cases, dummy_time)

###############################
#create daily     indicators  #
###############################

day_ind_names <- as.vector(paste0("day", 1:7))
covid_panel_cases$day_factor <- as.factor(covid_panel_cases$day)
dummy_day <- model.matrix(~0+covid_panel_cases$day_factor)
attr(dummy_day, "dimnames")[[2]] <- day_ind_names
covid_panel_cases <- cbind(covid_panel_cases, dummy_day)

###################################
#export time series panel to excel#
###################################
write_csv(covid_panel_cases, "Time_Series_Datafiles/covid_ts_panel_latest.csv" )


########################################################################################
#######################create  death panel #############################################
########################################################################################

covid_panel_deaths <- read_csv("Daily_Datafiles/deaths_latest.csv")
covid_panel_deaths[is.na(covid_panel_deaths)] <- 0
covid_panel_deaths <- covid_panel_deaths %>% arrange(country)

############################
#create country indicators #
############################
country_names <- c("canada", "china.nohubei", "france", "germany", "hubei", "iran", "italy", "japan", 
                   "korea", "netherlands", "norway", "spain", "sweden", "switzerland", "uk", "us")
country_ind_names <- paste0(c(country_names), rep("_indicator", length(country_names)))
# creating dummies
covid_panel_deaths$country_factor <- factor(covid_panel_deaths$country)#, ordered = TRUE, levels = country_names)
dummy_country <- model.matrix(~0+covid_panel_deaths$country_factor)
attr(dummy_country, "dimnames")[[2]] <- country_ind_names

# adding to the dataset, add [,-1] to drop a column of dummies for hubei
covid_panel_deaths <- cbind(covid_panel_deaths, dummy_country)

##########################
#create time  indicators #
##########################

time_ind_names <- as.vector(paste0("t", 0:max(covid_panel_deaths$time)))
# creating dummies
covid_panel_deaths$time_factor <- as.factor(covid_panel_deaths$time)
dummy_time <- model.matrix(~0+covid_panel_deaths$time_factor)
attr(dummy_time, "dimnames")[[2]] <- time_ind_names

# adding to the dataset and add [,-1] to drop a column of dummies
covid_panel_deaths <- cbind(covid_panel_deaths, dummy_time)

###############################
#create daily     indicators  #
###############################

day_ind_names <- as.vector(paste0("day", 1:7))
covid_panel_deaths$day_factor <- as.factor(covid_panel_deaths$day)
dummy_day <- model.matrix(~0+covid_panel_deaths$day_factor)
attr(dummy_day, "dimnames")[[2]] <- day_ind_names
covid_panel_deaths <- cbind(covid_panel_deaths, dummy_day)

###################################
#export time series death to excel#
###################################
write_csv(covid_panel_deaths, "Time_Series_Datafiles/covid_ts_deaths_panel_latest.csv" )

##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################

#pull global data for all countries
# library(readr)
# library(knitr)
# library(tidyverse)
# library(zoo)
# library(xlsx)
# library(writexl)
# library(ggrepel)
library(lubridate)
setwd("~/daily_scripts/")
#Use COUNTRIES dataset if you want to look at cases since 1/22
#Use global_cases_100 data to look at cases >100 from day 0
#you must run all of the code below in order to create these datasets
#make sure to set your wd to the proper location 
#download case data from JHU github
update_data <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
raw_data <- read_csv(update_data)
#colnames(raw_data) <- c("province", "country", "lat", "long", seq(as.Date("2020-01-22"), Sys.Date(), by = "days")) #run this if late PM
colnames(raw_data) <- c("province", "country", "lat", "long", seq(as.Date("2020-01-22"), Sys.Date()-1, by = "days")) #run this in AM

#put dataset into long format for easier data manipulation
global <- raw_data %>% gather(date, cases, colnames(raw_data)[5:ncol(raw_data)])
global$date <- as.Date(as.numeric(global$date), origin="1970-01-01")
global_names <- as.vector(unique(global$country))

#sum cases by country, note "Diamond Princess" is its own country!
global_cases <- global %>% group_by(country, date) %>%  summarise(cases = sum(cases, na.rm = FALSE))

#normalize weeks to start at week 0 = 1/22
global_cases$week <- week(global_cases$date)-4

#case count by week = max number of cases reported in that week
global_cases <- global_cases %>% group_by(country, week) %>%   summarise(cases = max(cases, na.rm = FALSE))

#take log of cases
global_cases$log_cum_cases <- log(global_cases$cases)

#calculate first diff of log
global_cases <- global_cases %>% group_by(country) %>%  mutate(first_diff_log_cases = log_cum_cases - lag(log_cum_cases))

#select countries where case count >99
global_cases_100 <- global_cases %>% subset(cases >99)
global_cases_100 <- global_cases_100 %>% group_by(country) %>% mutate(week_count = week - min(week))

write_csv(global_cases_100, "global_cases_100_latest.csv")

########################################################################################
########################################################################################
########################################################################################
update_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
raw_deaths <- read_csv(update_deaths)

colnames(raw_deaths) <- c("province", "country", "lat", "long", seq(as.Date("2020-01-22"), Sys.Date()-1, by = "days")) #run this if AM
#colnames(raw_deaths) <- c("province", "country", "lat", "long", seq(as.Date("2020-01-22"), Sys.Date(), by = "days")) #run this if late PM

deaths.global <- raw_deaths %>% gather(date, deaths, colnames(raw_deaths)[5:ncol(raw_deaths)])
deaths.global$date <- as.Date(as.numeric(deaths.global$date), origin="1970-01-01")
deaths.global_names <- as.vector(unique(deaths.global$country))

#sum cases by country
global_deaths <- deaths.global %>% group_by(country, date) %>%  summarise(deaths = sum(deaths, na.rm = FALSE))

#normalize week 0 start = 1/22/20
global_deaths$week <- week(global_deaths$date)-4

#cases for the week = max number of deaths reported that week
global_deaths <- global_deaths %>% group_by(country, week) %>% summarise(deaths = max(deaths, na.rm = FALSE))

#take log of deaths
global_deaths$log_cum_deaths <- log(global_deaths$deaths)

#calculate first diff of log deaths
global_deaths <- global_deaths %>% group_by(country) %>%  mutate(first_diff_log_deaths = log_cum_deaths - lag(log_cum_deaths))

#select countries where deaths >9
global_deaths_10 <- global_deaths %>% subset(deaths >9) 
global_deaths_10 <- global_deaths_10 %>% group_by(country) %>%  mutate(week_count = week - min(week))

write_csv(global_deaths_10, "global_deaths_10_latest.csv")


#create time variable
global_deaths_time <- deaths.global %>% group_by(country, date) %>%  summarise(deaths = sum(deaths, na.rm = FALSE))

#take log of deaths
global_deaths_time$log_cum_deaths <- log(global_deaths_time$deaths)

#calculate first diff of log deaths
global_deaths_time <- global_deaths_time %>% group_by(country) %>%  mutate(first_diff_log_deaths = log_cum_deaths - lag(log_cum_deaths))

#take deaths >9 and set time t=0
global_deaths_time <- global_deaths_time %>% subset(deaths>9)
global_deaths_time <- global_deaths_time %>% group_by(country) %>%
  mutate(time = as.numeric(date) - min(as.numeric(date)))

write_csv(global_deaths_time, "Daily_Datafiles/global_deaths_time.csv")
########################################################################################
########################################################################################
##############CREATE TIME SERIES PANELS FOR DEATHS/CASES################################
########################################################################################
########################################################################################
########################################################################################
#create vector of country names in case data
global_cases_names <- as.vector(unique(global_cases_100$country))

#create country indicator variable names
global_cases_ind_names <- paste0(c(global_cases_names), rep("_indicator", length(global_cases_names)))

#create vector of country names in death data
global_deaths_names <- as.vector(unique(global_deaths_10$country))

#create country indicator variable names
global_deaths_ind_names <- paste0(c(global_deaths_names), rep("_indicator", length(global_deaths_names)))
############################
#create country indicators #
############################
ts.global_cases <- as.data.frame(global_cases_100)
is.na(ts.global_cases) <- sapply(ts.global_cases, is.infinite)
ts.global_cases[is.na(ts.global_cases)] <- 0

# creating dummies
ts.global_cases$country_factor <- factor(ts.global_cases$country)# ordered = TRUE, levels = alphabetical global_cases_names
dummy_country <- model.matrix(~0+ts.global_cases$country_factor)
attr(dummy_country, "dimnames")[[2]] <- global_cases_ind_names
dummy_country <- as.data.frame(dummy_country)
# adding to the dataset
ts.global_cases <- cbind(ts.global_cases, dummy_country)

##########################
#create week indicators  #
##########################
global_week_ind_names <- as.vector(paste0("w", 0:max(ts.global_cases$week_count)))
# creating dummies
ts.global_cases$week_count_factor <- as.factor(ts.global_cases$week_count)
dummy_week <- model.matrix(~0+ts.global_cases$week_count_factor)
attr(dummy_week, "dimnames")[[2]] <- global_week_ind_names
dummy_week <- as.data.frame(dummy_week)
# adding to the dataset 
ts.global_cases <- cbind(ts.global_cases, dummy_week)
#head(ts.global_cases)

############################
#create country indicators #
############################
ts.global_deaths <- as.data.frame(global_deaths_10)
is.na(ts.global_deaths) <- sapply(ts.global_deaths, is.infinite)
ts.global_deaths[is.na(ts.global_deaths)] <- 0

ts.global_deaths$country_factor <- factor(ts.global_deaths$country)# ordered = TRUE, levels = alphabetical global_deaths_names
dummy_country <- model.matrix(~0+ts.global_deaths$country_factor)
attr(dummy_country, "dimnames")[[2]] <- global_deaths_ind_names
dummy_country <- as.data.frame(dummy_country)
# adding to the dataset
ts.global_deaths <- cbind(ts.global_deaths, dummy_country)

##########################
#create week indicators  #
##########################
global_deaths_week_ind_names <- as.vector(paste0("w", 0:max(ts.global_deaths$week_count)))
# creating dummies
ts.global_deaths$week_count_factor <- as.factor(ts.global_deaths$week_count)
dummy_week <- model.matrix(~0+ts.global_deaths$week_count_factor)
attr(dummy_week, "dimnames")[[2]] <- global_deaths_week_ind_names
dummy_week <- as.data.frame(dummy_week)
# adding to the dataset 
ts.global_deaths <- cbind(ts.global_deaths, dummy_week)
#head(ts.global_deaths)

write_csv(ts.global_cases, "Time_Series_Datafiles/ts.global_cases.csv")
write_csv(ts.global_deaths, "Time_Series_Datafiles/ts.global_deaths.csv")

#create panel of countries past US in epidemic

deaths <- read_csv("Daily_Datafiles/deaths_latest.csv")
deaths.mature <- deaths[deaths$country %in% c("China.nohubei", "France", "Germany", "Hubei", "Iran", "Italy", "Japan", "Korea", "Spain", "US"), ] #add one each day
deaths.mature_countries <- unique(deaths.mature$country)
############################
#create country indicators #
############################
deaths.mature_country_indicators <- paste0(c(deaths.mature_countries), rep("_indicator", length(deaths.mature_countries)))

deaths.mature <- as.data.frame(deaths.mature)
is.na(deaths.mature) <- sapply(deaths.mature, is.infinite)
deaths.mature[is.na(deaths.mature)] <- 0

deaths.mature$country_factor <- factor(deaths.mature$country)# ordered = TRUE, levels = alphabetical global_deaths_names
dummy_country <- model.matrix(~0+deaths.mature$country_factor)
attr(dummy_country, "dimnames")[[2]] <- deaths.mature_country_indicators

# adding to the dataset
deaths.mature <- cbind(deaths.mature, dummy_country)

##########################
#create week indicators  #
##########################
deaths.mature_time_indicators <- as.vector(paste0("t", 0:max(deaths.mature$time)))
# creating dummies
deaths.mature$time <- as.factor(deaths.mature$time)
dummy_time <- model.matrix(~0+deaths.mature$time)
attr(dummy_time, "dimnames")[[2]] <- deaths.mature_time_indicators
# adding to the dataset 
deaths.mature <- cbind(deaths.mature, dummy_time)
options(scipen = 50)
write_csv(deaths.mature, "Time_Series_Datafiles/deaths.mature.csv")
