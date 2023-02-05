
#Final analysis SR YLL paper


#1) Overall map (distribution of selected study) - Figure 2

   #load data
library(readxl)
yll <- read_excel("Data v5 YLL.xlsx", sheet = "all_YLL")
summary (yll)
duplicated(yll$author_year)
sum(duplicated(yll$author_year))

  #remove duplicate to create distribution map
library(dplyr)
single_yll <- print(yll[!duplicated(yll$author_year), ])
dim(single_yll) #to double check total paper is  46
table(single_yll$regionWHO) #no of studies by region
table(single_yll$country_data) #no of studies by country
table(single_yll$country_data, single_yll$regionWHO) #no of studies by region&country

  #create world map
install.packages("tidyverse")
library(tidyverse)

world_coordinates <- map_data("world") #This is the coordinate for the world map

dat_world <- 
  single_yll %>% 
  select(country_data) %>% 
  count(country_data) %>% 
  rename(region = country_data)
dat_world %>% head(10) # First 10 rows

world_map <- 
  world_coordinates %>% 
  left_join(dat_world, by = "region") %>% 
  mutate(region2 = ifelse(is.na(n), NA, region))

ggplot(data = world_map, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "grey", linewidth = 0.3) +
  geom_map(data = world_map, aes(map_id = region2, fill = n), map = world_map) +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(fill = "Number of studies:") +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(caption = expression(paste(italic("Note: A paper may contain a few studies from a few countries"))))

  
#2) YPLL - Descriptive analysis - Table 3

  # read data
ypll <- read_excel("Data v5 YLL.xlsx", sheet = "YPLL")
summary(ypll)
dim (ypll)
  #change all characters to factors
ypll$id<- as.character(ypll$id)
ypll$year_data_grp <- as.factor(ypll$year_data_grp)
ypll$source_data<- as.factor(ypll$source_data)
ypll$continents<- as.factor(ypll$continents)
ypll$regionWHO<- as.factor(ypll$WHO_regions)
ypll$cvd_type<- as.factor(ypll$cvd_type)
ypll$sex<- as.factor(ypll$sex)
ypll$formulaYLL<- as.factor(ypll$formulaYLL)
summary(ypll)
  #compute 2 group study time from decade
library(plyr)
ypll$time2grp <- revalue(ypll$year_data_grp, c("1"="2000-2022","2"="2000-2022",
                                               "3" = "1970-1999", "4" = "1970-1999",
                                               "5" = "1970-1999"))                                          
summary(ypll)
  #rename coding for study time 5 group 

levels(ypll$year_data_grp) <- c("2010-2022", "2000-2009",
                                "1990-1999", "1980-1989", "1970-1979")
summary(ypll)

  #Normality test - all data not normally distributed- use median 
shapiro.test(ypll$YPLLrate_100_000)
shapiro.test(ypll$meanYLL)
hist(ypll$YPLLrate_100_000)
hist(ypll$meanYLL)

  #Descriptive analysis 
summary(ypll$YPLLrate_100_000) #total median YPLL rate
summary(ypll$meanYLL) #total median YPLL per death
    #total YPLL Rate by regionwho, cvdtype, sex, decade
tapply(ypll$YPLLrate_100_000, ypll$WHO_regions, summary)  
tapply(ypll$YPLLrate_100_000, ypll$year_data_grp, summary) 
tapply(ypll$YPLLrate_100_000, ypll$time2grp, summary)
tapply(ypll$YPLLrate_100_000, ypll$cvd_type, summary) 
tapply(ypll$YPLLrate_100_000, ypll$sex, summary) 
    #total mean YPLL  by regionwho, cvdtype, sex, decade
tapply(ypll$meanYLL, ypll$WHO_regions, summary)  
tapply(ypll$meanYLL, ypll$year_data_grp, summary)  
tapply(ypll$meanYLL, ypll$time2grp, summary) 
tapply(ypll$meanYLL, ypll$cvd_type, summary) 
tapply(ypll$meanYLL, ypll$sex, summary) 

#use gtsummary to get presented table for Table 3

install.packages("gtsummary")
library(gtsummary)

ypllrate1 <- tbl_continuous(
  data = ypll,
  variable = YPLLrate_100_000, digits = everything() ~ 1,
  include = c(WHO_regions, cvd_type, sex, year_data_grp, time2grp))

ypllrate2 <- tbl_continuous(
  data = ypll,
  variable = YPLLrate_100_000, digits = everything() ~ 1,
  include = c(WHO_regions, cvd_type, sex, year_data_grp, time2grp),
  by = time2grp) # stratified by study time

ypllmean1 <- tbl_continuous(
  data = ypll,
  variable = meanYLL, digits = everything() ~ 1,
  include = c(WHO_regions, cvd_type, sex, year_data_grp, time2grp))

ypllmean2 <- tbl_continuous(
  data = ypll,
  variable = meanYLL, digits = everything() ~ 1,
  include = c(WHO_regions, cvd_type, sex, year_data_grp, time2grp),
  by = time2grp) # stratified by study time

#merge all in 1 table
tbl_merge(tbls = list(ypllrate1, ypllrate2, ypllmean1, ypllmean2 ))

#count total paper YPLL rate for Table 3
ypllrate_no_NA <- ypll %>% drop_na(YPLLrate_100_000)
single_ypllrate <- print(ypllrate_no_NA[!duplicated(ypllrate_no_NA$author_year), ])
dim (single_ypllrate) #total paper reported YPLL rate is  12
table(single_ypllrate$regionWHO) #no of studies by region
#count total paper YPLL per death for Table 3
yplldeath_no_NA <- ypll %>% drop_na(meanYLL)
single_yplldeath <- print(yplldeath_no_NA[!duplicated(yplldeath_no_NA$author_year), ])
dim (single_yplldeath) #total paper reported YPLL rate is  12
table(single_yplldeath$regionWHO) #no of studies by region

# countries by region - to add text (result)
table (single_ypllrate$country_data, single_ypllrate$WHO_regions)
table (single_yplldeath$country_data, single_yplldeath$WHO_regions)



# 3) SEYLL - Descriptive analysis - Table 3

  #read data
seyll <- read_excel("Data v5 YLL.xlsx", sheet = "SEYLL")
summary(seyll)
  #change all characters to factors
seyll$id<- as.character(seyll$id)
seyll$year_data_grp <- as.factor(seyll$year_data_grp)
seyll$source_data<- as.factor(seyll$source_data)
seyll$continents<- as.factor(seyll$continents)
seyll$regionWHO<- as.factor(seyll$WHO_regions)
seyll$cvd_type<- as.factor(seyll$cvd_type)
seyll$sex<- as.factor(seyll$sex)
seyll$formulaYLL<- as.factor(seyll$formulaYLL)
summary(seyll)
head(seyll)

  #compute 2 group study time from decade
library(plyr)
seyll$time2grp <- revalue(seyll$year_data_grp, c("1"="2000-2022", 
                                                 "2"="2000-2022",
                                                 "3" = "1970-1999"))
summary(seyll)

  #rename level for study time 5 group 
levels(seyll$year_data_grp) <- c("2010-2022", "2000-2009",
                                 "1990-1999", "1980-1989", "1970-1979")
summary(seyll)

  #Normality test - all data not normally distributed
shapiro.test(seyll$SEYLLrate_100_000)
shapiro.test(seyll$meanYLL)
hist(seyll$SEYLLrate_100_000)
hist(seyll$meanYLL)

  #Descriptive analysis 
summary(seyll$SEYLLrate_100_000) #total median SEYLL rate
summary(seyll$meanYLL) #total median SEYLL per death
   #total YPLL Rate by regionwho, cvdtype, sex, decade
tapply(seyll$SEYLLrate_100_000, seyll$WHO_regions, summary)  
tapply(seyll$SEYLLrate_100_000, seyll$year_data_grp, summary) 
tapply(seyll$SEYLLrate_100_000, seyll$time2grp, summary)
tapply(seyll$SEYLLrate_100_000, seyll$cvd_type, summary) 
tapply(seyll$SEYLLrate_100_000, seyll$sex, summary) 
   #total mean YPLL  by regionwho, cvdtype, sex, decade
tapply(seyll$meanYLL, seyll$WHO_regions, summary)  
tapply(seyll$meanYLL, seyll$year_data_grp, summary) 
tapply(seyll$meanYLL, seyll$time2grp, summary)
tapply(seyll$meanYLL, seyll$cvd_type, summary) 
tapply(seyll$meanYLL, seyll$sex, summary) 

#use gtsummary to get presented table for Table 4
library(gtsummary)

seyllrate1 <- tbl_continuous(
  data = seyll,
  variable = SEYLLrate_100_000, digits = everything() ~ 1,
  include = c(WHO_regions, cvd_type, sex, year_data_grp, time2grp))

seyllrate2 <- tbl_continuous(
  data = seyll,
  variable = SEYLLrate_100_000, digits = everything() ~ 1,
  include = c(WHO_regions, cvd_type, sex, year_data_grp, time2grp),
  by = time2grp) # stratified by study time

seyllmean1 <- tbl_continuous(
  data = seyll,
  variable = meanYLL, digits = everything() ~ 1,
  include = c(WHO_regions, cvd_type, sex, year_data_grp, time2grp))

seyllmean2 <- tbl_continuous(
  data = seyll,
  variable = meanYLL, digits = everything() ~ 1,
  include = c(WHO_regions, cvd_type, sex, year_data_grp, time2grp),
  by = time2grp) # stratified by study time

#merge all in 1 table
tbl_merge(tbls = list(seyllrate1, seyllrate2, seyllmean1, seyllmean2 ))

#count total paper YPLL rate for Table 3
seyllrate_no_NA <- seyll %>% drop_na(SEYLLrate_100_000)
single_seyllrate <- print(seyllrate_no_NA[!duplicated(seyllrate_no_NA$author_year), ])
dim (single_seyllrate) #total paper reported SEYLL rate is  18
table(single_seyllrate$regionWHO) #no of studies by region
#count total paper YPLL per death for Table 3
seylldeath_no_NA <- seyll %>% drop_na(meanYLL)
single_seylldeath <- print(seylldeath_no_NA[!duplicated(seylldeath_no_NA$author_year), ])
dim (single_seylldeath) #total paper reported YPLL rate is  12
table(single_seylldeath$regionWHO) #no of studies by region


# countries by region - to add text (result)
table (single_seyllrate$country_data, single_seyllrate$WHO_regions)
table (single_seylldeath$country_data, single_seylldeath$WHO_regions)

#3) Plot YLL rate by country 


## a) Vertical plot for YPLL rate by country - Figure 3

bar_ypll <- 
  ypll %>% 
  select(author_year, country_data, year_data, sex, cvd_type, WHO_regions, YPLLrate_100_000) %>% 
  drop_na(YPLLrate_100_000) 

bar2_ypll <- aggregate(bar_ypll$YPLLrate_100_000,
                       by = list(bar_ypll$country_data, bar_ypll$WHO_regions),
                       FUN = median) 

names(bar2_ypll)[names(bar2_ypll) == "Group.1"] <- "Country"
names(bar2_ypll)[names(bar2_ypll) == "Group.2"] <- "WHO_regions"
names(bar2_ypll)[names(bar2_ypll) == "x"] <- "YPLLrate"

bar2_ypll

ggplot(bar2_ypll, aes(y= YPLLrate, x = fct_reorder(Country, YPLLrate), fill=WHO_regions )) +
  geom_col(position="dodge") + coord_flip() + theme_bw() +
  labs(y = "YPLL rate per 100,000", x = "Country") +
  scale_fill_brewer(palette="Oranges")


## b) Vertical plot for PYLL rate by country -Figure 4

bar_seyll <- 
  seyll %>% 
  select(author_year, country_data, year_data, sex, cvd_type, WHO_regions, SEYLLrate_100_000) %>% 
  drop_na(SEYLLrate_100_000) 

bar2_seyll <- aggregate(bar_seyll$SEYLLrate_100_000,
                        by = list(bar_seyll$country_data, bar_seyll$WHO_regions),
                        FUN = median) 

names(bar2_seyll)[names(bar2_seyll) == "Group.1"] <- "Country"
names(bar2_seyll)[names(bar2_seyll) == "Group.2"] <- "WHO_regions"
names(bar2_seyll)[names(bar2_seyll) == "x"] <- "SEYLLrate"

bar2_seyll

ggplot(bar2_seyll, aes(y= SEYLLrate, x = fct_reorder(Country, SEYLLrate), fill=WHO_regions)) +
  geom_col(position="dodge") + coord_flip() + theme_bw() +
  labs(y = "SEYLL rate per 100,000", x = "Country") +
  scale_fill_brewer(palette="Oranges")



#4) Trend YPLL (pattern)

library(tidyverse)

  #trend all year by cvd type and sex - trend look reducing for cvd type
ypll %>% ggplot (aes(x = Year, y = YPLLrate_100_000)) + 
  geom_point(aes(colour = sex)) +
  geom_smooth(method='lm') +
  facet_wrap( ~ cvd_type)
ypll %>% ggplot (aes(x = Year, y = YPLLrate_100_000)) + 
  geom_point(aes(colour = WHO_regions)) +
  geom_smooth(method='lm') +
  facet_wrap( ~ sex)

  # remove year before 1990 for trend 
ypll2<-ypll[!(ypll$Year < "1990"),]
dim (ypll2)
    #a) trend ypll rate >90an  by cvd type -use this
     #rename label CVA  to cerebrovascular disease
library(dplyr)
levels(ypll2$cvd_type)[levels(ypll2$cvd_type)=='CVA'] <- 'Cerebrovascular Disease'
ypll2 %>% ggplot (aes(x = Year, y = YPLLrate_100_000)) + 
  geom_point(aes(colour = WHO_regions)) +
  labs(x = "Year of study", y = "YPLL rate per 100,000") + #change title
  geom_smooth(method='lm') +
  facet_wrap( ~ cvd_type) +
  labs(caption = expression(paste(italic("(a) Pattern of years of potential life lost (YPLL) per 100,000 from 1990 to 2022 by CVD types"))))

    #b) trend ypll rate >90an by sex
ypll2 %>% ggplot (aes(x = Year, y = YPLLrate_100_000)) + 
  geom_point(aes(colour = WHO_regions)) +
  labs(x = "Year of study", y = "YPLL rate per 100,000") + #change title
  geom_smooth(method='lm') +
  facet_wrap( ~ sex)
      #remove all sex
ypll3 <- subset(ypll2, sex != "all")
ypll3 %>% ggplot (aes(x = Year, y = YPLLrate_100_000)) + 
  geom_point(aes(colour = WHO_regions)) +
  labs(x = "Year of study", y = "YPLL rate per 100,000") + #change title
  geom_smooth(method='lm') +
  facet_wrap( ~ sex)
     #rename label sex - capitalize first word
library(dplyr)
levels(ypll3$sex)[levels(ypll3$sex)=='female'] <- 'Female'
levels(ypll3$sex)[levels(ypll3$sex)=='male'] <- 'Male'
    #use this for ypll rate by sex
ypll3 %>% ggplot (aes(x = Year, y = YPLLrate_100_000)) + 
  geom_point(aes(colour = WHO_regions)) +
  labs(x = "Year of study", y = "YPLL rate per 100,000") + #change title
  geom_smooth(method='lm') +
  facet_wrap( ~ sex) +
  labs(caption = expression(paste(italic("(a) Pattern of years of potential life lost (YPLL) per 100,000 from 1990 to 2022 by sex"))))

#5) Trend SEYLL

   #a) trend YPLL rate by cvd type - use this
     #rename label CVA  to cerebrovascular disease
library(dplyr)
levels(seyll$cvd_type)[levels(seyll$cvd_type)=='CVA'] <- 'Cerebrovascular Disease'
seyll %>% ggplot (aes(x = single_year, y = SEYLLrate_100_000)) + 
  geom_point(aes(colour = WHO_regions)) +
  labs(x = "Year of study", y = "SEYLL rate per 100,000") + #change title
  geom_smooth(method='lm') +
  facet_wrap( ~ cvd_type) +
  labs(caption = expression(paste(italic("(b) Pattern of standard expected years of life lost (SEYLL) per 100,000 from 1990 to 2022 by CVD types"))))

   #b) trend YPLL rate by sex 
seyll %>% ggplot (aes(x = single_year, y = SEYLLrate_100_000)) + 
  geom_point(aes(colour = WHO_regions)) +
  labs(x = "Year of study", y = "SEYLL rate per 100,000") + #change title
  geom_smooth(method='lm') +
  facet_wrap( ~ sex)
    #remove all sex
seyll2 <- subset(seyll, sex != "all")
seyll2 %>% ggplot (aes(x = single_year, y = SEYLLrate_100_000)) + 
  geom_point(aes(colour = WHO_regions)) +
  labs(x = "Year of study", y = "SEYLL rate per 100,000") + #change title
  geom_smooth(method='lm') +
  facet_wrap( ~ sex)
    #rename label sex - capitalize first word
library(dplyr)
levels(seyll2$sex)[levels(seyll2$sex)=='female'] <- 'Female'
levels(seyll2$sex)[levels(seyll2$sex)=='male'] <- 'Male'
#use this for seyll rate by sex
seyll2 %>% ggplot (aes(x = single_year, y = SEYLLrate_100_000)) + 
  geom_point(aes(colour = WHO_regions)) +
  labs(x = "Year of study", y = "SEYLL rate per 100,000") + #change title
  geom_smooth(method='lm') +
  facet_wrap( ~ sex) + 
  labs(caption = expression(paste(italic("(b) Pattern of standard expected years of life lost (SEYLL) per 100,000 from 1990 to 2022 by sex"))))

  #c) trend SEYLL rate by region 
seyll %>% ggplot (aes(x = single_year, y = SEYLLrate_100_000)) + 
  geom_point(aes(colour = cvd_type)) +
  labs(x = "Year of study", y = "SEYLL rate per 100,000") + #change title
  geom_smooth(method='lm') +
  facet_wrap( ~ WHO_regions) + 
  labs(caption = expression(paste(italic("Pattern of standard expected years of life lost (SEYLL) per 100,000 from 1990 to 2022 by WHO regions"))))

#not presented YPLL - not enough data 
ypll2 %>% ggplot (aes(x = Year, y = YPLLrate_100_000)) + 
  geom_point(aes(colour = cvd_type)) +
  labs(x = "Year of study", y = "YPLL rate per 100,000") + #change title
  geom_smooth(method='lm') +
  facet_wrap( ~ WHO_regions) + 
  labs(caption = expression(paste(italic("Pattern of years of potential life lost (YPLL) per 100,000 from 1990 to 2022 by WHO regions"))))




# 6) World may by CVD type - using SEYLL only

  #Load package for world map
install.packages("choroplethr")
install.packages("choroplethrMaps")
library(choroplethr)
library(choroplethrMaps)
library(tidyverse)

  #a) Map for IHD (SEYLL rate)
   #read data
map3 <- read_excel("SEYLL_IHD_map.xlsx")
glimpse(map3)
head(unique(map3$country_data), 20)
  #prepare dataset
plotdata3 <- map3 %>%
  rename(region = country_data,
         value = SEYLLrate_100_000) %>%
  mutate(region = tolower(region))  %>%
  mutate(region = recode(region, 
                         "usa"    = "united states of america",
                         "trinidad & tobago" = "trinidad and tobago",
                         "korea" = "south korea",
                         "serbia" = "republic of serbia"))
head(unique(plotdata3$region), 20)
  #create map
country_choropleth(plotdata3,
                   num_colors=9) +
  scale_fill_brewer(palette="YlOrRd") +
  labs(title = "Standard expected years of life lost (SEYLL) per 100,000 from Ischemic Heart Disease (IHD)",
       fill = "SEYLL per 100,000")

#b) Map for CVA (SEYLL rate)
  #read data
map4 <- read_excel("SEYLLcva_map.xlsx")
glimpse(map4)
head(unique(map4$country_data), 20)
  #prepare dataset
plotdata4 <- map4 %>%
  rename(region = country_data,
         value = SEYLLrate_100_000) %>%
  mutate(region = tolower(region))  %>%
  mutate(region = recode(region, 
                         "usa"    = "united states of america",
                         "trinidad & tobago" = "trinidad and tobago",
                         "korea" = "south korea",
                         "serbia" = "republic of serbia"))
head(unique(plotdata4$region), 20)
  #Create map
country_choropleth(plotdata4,
                   num_colors=9) +
  scale_fill_brewer(palette="YlOrRd") +
  labs(title = "Standard expected years of life lost (SEYLL) per 100,000 from Cerebrovascular Disease",
       fill = "SEYLL per 100,000")




##------------END Analysis---------------------##





