
#Final analysis SR YLL paper


setwd("C:/Users/shakirarodzlan/Desktop/PhD/PhD Chapters/Chapter 2-Meta analysis/Publication/Publish SR YLL/Final analysis YLL")

#1) Overall map (distribution of selected study) - Figure 2

   #load data
library(readxl)
yll <- read_excel("Data v3 YLL.xlsx", sheet = "MA_YLL")
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

   #try add country name - not nice
install.packages("ggh4x")
library(ggh4x) # For stat_midpoint 
ggplot(data = world_map, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "grey", linewidth = 0.3) +
  geom_map(data = world_map, aes(map_id = region2, fill = n), map = world_map) +
  stat_midpoint(aes(x = long, y = lat, label = region2), geom = "text", size = 3.5, fontface = "bold", color = "black", na.rm = T,
                check_overlap = T) +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(fill = "Number of studies:") +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(caption = expression(paste(italic("Note: A paper may contain a few studies from a few countries"))))


#2) YPLL - Descriptive analysis - Table 2

  # read data
ypll <- read_excel("YPLL2.xlsx")
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
shapiro.test(ypll$total_YPLL)
shapiro.test(ypll$YPLLrate_100_000)
shapiro.test(ypll$meanYLL)
hist(ypll$total_YPLL)
hist(ypll$YPLLrate_100_000)
hist(ypll$meanYLL)

  #Descriptive analysis 
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


# 3) SEYLL - Descriptive analysis - Table 3

  #read data
seyll <- read_excel("SEYLL2.xlsx")
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
shapiro.test(seyll$total_SEYLL)
shapiro.test(seyll$SEYLLrate_100_000)
shapiro.test(seyll$meanYLL)
hist(seyll$total_SEYLL)
hist(seyll$SEYLLrate_100_000)
hist(seyll$meanYLL)

  #Descriptive analysis 
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


#3) Plot YLL rate (min max plot) by country 

  #a) plot for YPLL
dat_ypll <- 
  yll %>% 
  select(author_year, country_data, formulaYLL, YPLLrate_100_000) %>% 
  drop_na(YPLLrate_100_000) %>% 
  group_by(country_data, formulaYLL) %>% 
  summarise(min = min(YPLLrate_100_000), max = max(YPLLrate_100_000), .groups = "drop")
dat_ypll

ggplot(dat_ypll, aes(x = fct_reorder(country_data, max))) +
  geom_linerange(aes(ymin = min, ymax = max))+
  geom_point(aes(y = min), size = 3, color = "blue") +
  geom_point(aes(y = max), size = 3, color = "blue") +
  theme_bw() +
  xlab("Country") +
  ylab("Years of potential life lost (YPLL) rate per 100,000") +
  labs(caption = expression(paste(italic("Note: All countries used formula by Gardner1990/Romeder1977")))) +
  coord_flip()

  #b) plot for SEYLL
dat_seyll <- 
  yll %>% 
  select(author_year, country_data, SEYLLrate_100_000) %>% 
  drop_na(SEYLLrate_100_000) %>% 
  group_by(country_data) %>% 
  summarise(min = min(SEYLLrate_100_000), max = max(SEYLLrate_100_000), .groups = "drop") 
dat_seyll

ggplot(dat_seyll, aes(x = fct_reorder(country_data, max))) +
  geom_linerange(aes(ymin = min, ymax = max))+
  geom_point(aes(y = min), size = 3, color = "blue") +
  geom_point(aes(y = max), size = 3, color = "blue") +
  theme_bw() +
  xlab("Country") +
  ylab("Standard expected years of life lost (SEYLL) rate per 100,000") +
  labs(caption = expression(paste(italic("Note: All countries used formula by Global Burden of Disease (1996)")))) +
  coord_flip()


#4) Trend YPLL

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
  labs(caption = expression(paste(italic("(a) Trends in years of potential life lost (YPLL) per 100,000 by CVD types"))))

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
  labs(caption = expression(paste(italic("(a) Trends in years of potential life lost (YPLL) per 100,000 by gender"))))

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
  labs(caption = expression(paste(italic("(b) Trends in standard expected years of life lost (SEYLL) per 100,000 by CVD types"))))

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
  labs(caption = expression(paste(italic("(b) Trends in standard expected years of life lost (SEYLL) per 100,000 by gender"))))




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





##---------------------------------------------------------------------##

###Trial plot min max by country  - not to present, better use world map

# plot for IHD 
seyll_ihd <-subset(seyll,cvd_type=="IHD")
seyll_ihd2 <- seyll_ihd %>%
  select(author_year, country_data, formulaYLL, SEYLLrate_100_000) %>%
  group_by(country_data) %>% 
  drop_na(SEYLLrate_100_000) %>% 
  summarise(min = min(SEYLLrate_100_000), max = max(SEYLLrate_100_000), .groups = "drop")
seyll_ihd2
ggplot(seyll_ihd2, aes(x = fct_reorder(country_data, max))) +
  geom_linerange(aes(ymin = min, ymax = max)) +
  geom_point(aes(y = min), size = 3, color = "blue") +
  geom_point(aes(y = max), size = 3, color = "blue") +
  theme_bw() +
  xlab("Country") +
  ylab("Standard expected years of life lost (per 100,000) from IHD") +
  labs(caption = expression(paste(italic("Note: All countries used formula by Global Burden of Disease study")))) +
  coord_flip()

# Iran outlier- to remove Iran
seyll_ihd3 <-subset(seyll,cvd_type=="IHD" & country_data!= "Iran") 
seyll_ihd4 <- seyll_ihd3 %>%
  select(author_year, country_data, formulaYLL, SEYLLrate_100_000) %>%
  group_by(country_data) %>% 
  drop_na(SEYLLrate_100_000) %>% 
  summarise(min = min(SEYLLrate_100_000), max = max(SEYLLrate_100_000), .groups = "drop")
seyll_ihd4

ggplot(seyll_ihd4, aes(x = fct_reorder(country_data, max))) +
  geom_linerange(aes(ymin = min, ymax = max)) +
  geom_point(aes(y = min), size = 3, color = "blue") +
  geom_point(aes(y = max), size = 3, color = "blue") +
  theme_bw() +
  xlab("Country") +
  ylab("Standard expected years of life lost (per 100,000) from Ischemic Heart Disease (IHD)") +
  labs(caption = expression(paste(italic("Note: All countries used formula by Global Burden of Disease study")))) +
  coord_flip()

# plot for CVA
seyll_cva <-subset(seyll,cvd_type=="CVA" & country_data!= "Iran") 
seyll_cva2 <- seyll_cva %>%
  select(author_year, country_data, formulaYLL, SEYLLrate_100_000) %>%
  group_by(country_data) %>% 
  drop_na(SEYLLrate_100_000) %>% 
  summarise(min = min(SEYLLrate_100_000), max = max(SEYLLrate_100_000), .groups = "drop")
seyll_cva2
ggplot(seyll_cva2, aes(x = fct_reorder(country_data, max))) +
  geom_linerange(aes(ymin = min, ymax = max)) +
  geom_point(aes(y = min), size = 3, color = "blue") +
  geom_point(aes(y = max), size = 3, color = "blue") +
  theme_bw() +
  xlab("Country") +
  ylab("Standard expected years of life lost (per 100,000) from Cerebrovascular Disease") +
  labs(caption = expression(paste(italic("Note: All countries used formula by Global Burden of Disease study")))) +
  coord_flip()
