# Data visualisation

library(vroom)
library(tidyverse)

covid_dat <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/time_series_covid19_deaths_global.csv")
names(covid_dat)[1:2] = c("Province.State", "Country.Region")

covid_long <- covid_dat %>% pivot_longer(cols = -c(Province.State:Long),
                                         names_to = "Date",
                                         values_to = "Deaths")

library(devtools)
install_github("gshs-ornl/wbstats")


library("wbstats")
pop_data <- wb_data(indicator = "SP.POP.TOTL", 
                    start_date = 2002, 
                    end_date = 2020)


##convert it to a tibble
pop_data <- as_tibble(pop_data)

#The max data
max(pop_data$date)

#Filtering the data
pop_2020 <- pop_data %>% filter(date == 2020)

pop_2020

#Cleaning data
covid_long


head(unique(pop_2020$country), 10) ; tail(unique(pop_2020$country), 10)

covid_dat %>% filter(Country.Region == "Australia")
pop_2020 %>% filter(country == "Australia")
#Task to join two data frames.

#Since now we see the problem is quite apparent, We need to see deaths in the Covid data as a sum of death in the entire nation, NOT split up by states.

#We now need to reformat our table.

covid_country <- covid_long %>% group_by(Country.Region, Date) %>% summarise(Deaths = sum(Deaths))

covid_country

tail(pop_2020)
install.packages("countrycode")
library(countrycode)
covid_country$code <- countrycode(covid_country$Country.Region,
                                  origin = "country.name",
                                  destination = "iso3c")

head(covid_country, 1)
tail(pop_2020)


pop_2020 %>% filter(iso3c == "AFG")

names(pop_2020)[5] <- "value"
head(pop_2020 %>% select(iso3c, value))

covid_w_pop <- left_join(covid_country, pop_2020 %>% select(iso3c, value), by = c("code" = "iso3c"))

covid_w_pop


names(covid_w_pop)
names(covid_w_pop) == "value"
which(names(covid_w_pop) == "value")
names(covid_w_pop)[which(names(covid_w_pop) == "value")] <- "Population"
names(covid_w_pop)
covid_w_pop %>% filter(Country.Region == "Afghanistan" & Date == "1/22/20")
pop_2020 %>% filter(country == "Afghanistan")


#Calculating deaths

max_death <- covid_country %>% filter(Date == max(covid_country$Date))

sum(max_death$Deaths)

#Lets find out the number of deaths globally

global_deaths_day <- covid_country %>% group_by(Date) %>% summarise("Global.deaths" = sum(Deaths))
which(is.na(global_deaths_day$Global.deaths))


#Calculate covid deaths per million

covid_w_pop$Deaths_p_m <- (covid_w_pop$Deaths/covid_w_pop$Population) *1000000
tail(covid_w_pop)


#Data visualisation with GGplot
library(ggplot2)
ggplot(data = global_deaths_day, aes(x = Date, y = Global.deaths)) + geom_point()

global_deaths_day$Date.corrected <- as.Date(global_deaths_day$Date, format = "%m/%d/%y")
ggplot(data = global_deaths_day, aes(x = Date.corrected, y= Global.deaths)) + geom_point(col = "darkgrey") + geom_line(col = "red")

p1 <- ggplot(data = global_deaths_day, aes(x = Date.corrected, y = Global.deaths))
p1

p1 <- p1 + geom_line()

p2 <- p1 + geom_point()


covid_w_pop
covid_w_pop$Date.corrected <- as.Date(covid_w_pop$Date, format = "%m/%d/%y")
covid_w_pop


by_country <- ggplot(covid_w_pop, aes(x = Date.corrected, y = Deaths))
by_country + geom_point(aes(col = Country.Region))
by_country + geom_point(aes(col = Country.Region)) + theme(legend.position = "none")

select_countries <- c("United Kingdom", "China", "US", "Italy", "France", "Germany")
sel_country_plot <- ggplot(covid_w_pop %>% filter(Country.Region %in% select_countries), aes(x = Date.corrected, y = Deaths ))
sel_country_plot + geom_line(aes(col=Country.Region))


#Point line plotting

sel_country_plot + geom_point(aes(shape = Country.Region))

sel_country_plot + geom_line(aes(col=Country.Region)) + facet_wrap(. ~Country.Region)

#Saving your work

pdf("~/Desktop/Work/Biofinformatics/Plots/Deaths by country.pdf", width = 6, height = 4)
sel_country_plot + 
  ##add lines
  geom_line(aes(col = Country.Region)) + 
  ##add facets
  facet_wrap(. ~ Country.Region)
dev.off()
