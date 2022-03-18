## Loading packages 
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)

##########################
###### Reading Data ######
##########################
## Read list of all datasets
crop_data_ls<- tt_load('2020-09-01')

## Extract each table from main list
arable_land<- crop_data_ls[[1]]
fertilizer<- crop_data_ls[[2]]
tractors<- crop_data_ls[[3]]
key_crop_yields<-crop_data_ls[[4]]
land_use<- crop_data_ls[[5]]


##########################
##### Cleaning Data ######
##########################
### Arable Land 
str(arable_land)


### Fertilizer
str(fertilizer)


### Tractors
str(tractors)


### Key Crop Yields
str(key_crop_yields)

#Change column name 
colnames(key_crop_yields)<- c("entity", "code", "year", "wheat(tpha)", "rice(tpha)","maize(tpha)","soybeans(tpha)","potatoes(tpha)", "beans(tpha)", "peas(tpha)", "cassava(tpha)","barley(tpha)","cocoa_beans(tpha)","bananas(tpha)")

#Split into region and countries 
key_crop_yields_region<- key_crop_yields%>%
  filter(is.na(code))
key_crop_yields_countries<- key_crop_yields%>%
  filter(!is.na(code))

##Plotting biofuel crops 
ggplot(key_crop_yields_region, aes(year, `wheat(tpha)`))+
  geom_line()+
  facet_wrap(~entity) 

key_crop_yields_region %>%
  filter(entity == "Small island developing States") %>%
  ggplot(aes(year, `wheat(tpha)`))+
  geom_line()+
  facet_wrap(~entity) 

ggplot(key_crop_yields_region, aes(year, `soybeans(tpha)`))+
  geom_line()+
  facet_wrap(~entity) 

key_crop_yields_countries%>%
  filter(!is.na(`soybeans(tpha)`))%>%
  ggplot(aes(year, `soybeans(tpha)`))+
  geom_line()+
  geom_vline(xintercept = 2004, color="blue")+
  facet_wrap(~entity) 

key_crop_yields_region %>%
  filter(entity == "Small island developing States") %>%
  ggplot(aes(year, `wheat(tpha)`))+
  geom_line()+
  facet_wrap(~entity) 

key_crop_yields_countries%>%
  filter(!is.na(`maize(tpha)`))%>%
  ggplot(aes(year, `maize(tpha)`))+
  geom_line()+
  geom_vline(xintercept = 2004, color="blue")+
  facet_wrap(~entity) 

#Heatmap
key_crop_yields_region%>%
  filter(!is.na(`maize(tpha)`))%>%
  ggplot(aes(year, entity, fill = `maize(tpha)`))+
  geom_tile()+
  geom_vline(xintercept = 1990, color = "red")+
  geom_vline(xintercept = 2004, color = "red")+
  scale_fill_viridis_c()

key_crop_yields_countries%>%
  filter(!is.na(`maize(tpha)`))%>%
  ggplot(aes(year, entity, fill = `maize(tpha)`))+
  geom_tile()+
  geom_vline(xintercept = 1990, color = "red")+
  geom_vline(xintercept = 2004, color = "red")+
  scale_fill_viridis_c()

key_crop_yields_region%>%
  filter(!is.na(`soybeans(tpha)`))%>%
  ggplot(aes(year, entity, fill = `soybeans(tpha)`))+
  geom_tile()+
  geom_vline(xintercept = 1990, color = "red")+
  geom_vline(xintercept = 2004, color = "red")+
  scale_fill_viridis_c()

key_crop_yields_countries%>%
  filter(!is.na(`soybeans(tpha)`))%>%
  ggplot(aes(year, entity, fill = `soybeans(tpha)`))+
  geom_tile()+
  geom_vline(xintercept = 1990, color = "red")+
  geom_vline(xintercept = 2004, color = "red")+
  scale_fill_viridis_c()

key_crop_yields_region%>%
  filter(!is.na(`wheat(tpha)`))%>%
  ggplot(aes(year, entity, fill = `wheat(tpha)`))+
  geom_tile()+
  geom_vline(xintercept = 1990, color = "red")+
  geom_vline(xintercept = 2004, color = "red")+
  scale_fill_viridis_c()

key_crop_yields_countries%>%
  filter(!is.na(`wheat(tpha)`))%>%
  ggplot(aes(year, entity, fill = `wheat(tpha)`))+
  geom_tile()+
  geom_vline(xintercept = 1990, color = "red")+
  geom_vline(xintercept = 2004, color = "red")+
  scale_fill_viridis_c()


### Land Use
str(land_use)

#Change column name
colnames(land_use)<- c("entity", "code", "year", "cereal_yield_idx", "land_area_change", "tot_pop")

#Detecting the issue
land_use%>%
  mutate(year = as.numeric(year)) #throws NA

na_year<- land_use%>%
  mutate(year = as.numeric(year))%>%
  filter(is.na(year))

na_year[1,]
land_use%>%
  filter(entity=="Africa", tot_pop==241901) #we find that the year is in BCE

#to ensure all NAs are from BCE
nrow(filter(land_use, str_detect(year, "BCE")))==nrow(na_year)

#Filter out all data from 1961 to 2018
land_use_clean<-land_use%>%
  filter(!str_detect(year, "BCE"))%>%   #remove BCE columns (no more NAs produced)
  mutate(year = as.numeric(year))%>%    #Convert year to a numeric data 
  filter(!is.na(cereal_yield_idx))      #remove NA columns i.e. observations from before 1961

#More NA values 
view(land_use_clean%>%
       filter(is.na(code)))   #NA in tot_pop and code occurs for entities that are not countries, but a region/continent

#we can separate data by region and data by each country
land_use_by_region<- land_use_clean%>%
  filter(is.na(code)||entity=="World")%>%
  select(-code,-tot_pop)

land_use_clean<-land_use_clean%>%
  filter(!is.na(code))

#no more NAs found
sum(is.na(land_use_clean$cereal_yield_idx)) 
sum(is.na(land_use_clean$entity)) 


##########################
###### Joining Data ######
##########################

##########################
##### Exploring Data #####
##########################
land_use_clean%>%
  mutate(entity = as.factor(entity))%>%
  ggplot(aes(year,cereal_yield_idx))+
  geom_line()+
  ggtitle("Cereal Yield Index Over 1961 to 2018 by Country")+
  facet_wrap(~entity)

land_use_by_region%>%
  mutate(entity = as.factor(entity))%>%
  ggplot(aes(year,cereal_yield_idx))+
  geom_line()+
  ggtitle("Cereal Yield Index Over 1961 to 2018 by Region")+
  facet_wrap(~entity)

land_use_clean%>%
  mutate(entity = as.factor(entity))%>%
  ggplot(aes(cereal_yield_idx, land_area_change))+
  geom_point(size=0.1)+
  ggtitle("Cereal Yield Index vs Change in Land Area")+
  facet_wrap(~entity)


land_use_clean%>%
  filter(entity=="Belize")%>%
  ggplot(aes(cereal_yield_idx, land_area_change))+
  geom_point(size=0.1)+
  ggtitle("Cereal Yield Index vs Change in Land Area")

##########################
##### Visualizations #####
##########################


##########################
##### Miscellaneous ######
##########################
