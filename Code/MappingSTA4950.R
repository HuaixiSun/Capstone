#Breanna Blackwood
#3/31/2026
#Mapping

#Libraries
library(zipcodeR)
library(tigris)
library(tidycensus)
library(sf)
library(ggplot2)
library(mapview)
library(dplyr)
library(leafpop)
library(RColorBrewer)
library(webshot)
library(leaflet)
library(leaflet.extras2)
library(gganimate)
library(gifski)
library(broom)
library(remotes)
library(chromote)
library(webshot2)

###############################################################################

####Zip codes####

#Get zipcodes for Gainesville, Fl
zipcodes <- search_city("Gainesville", "FL")$zipcode

#Get zipcode shapes for Gainesville
zipshape_FL <- zctas(state = "FL", year = 2010)
zipshape_GNV <- zipshape_FL[zipshape_FL$ZCTA5CE10 %in% zipcodes, ]

#Plot zip codes
ggplot(zipshape_GNV) + 
  geom_sf(aes(fill = ZCTA5CE10)) + 
  labs(title = "Gainesville Zip Code Map",
       fill = "Zip Code Numbers") + 
  theme(legend.title = element_text(size = 8), 
        legend.text = element_text(size = 7)) 
ggsave("zipcode_outlines.png")

mapview(zipshape_GNV) #Gives a map we can interact with. 

#Read in the 2020 Resource Guide (the edited one with all zip codes separate)
resource2020 <- read.csv("~/Downloads/2020_Resource_Guide_Edited.csv")
#Filter out the organizations that do not have Gainesville zip codes 
resource2020 <- filter(resource2020, Zip.Code %in% zipcodes)

#Join with the Gainesville zip code shapes 
#code inspiration from https://rpubs.com/drkblake/FMR3 
resource2020map <- left_join(resource2020, zipshape_GNV, by = c("Zip.Code" = "ZCTA5CE10"))

#Turn into the map
#Convert 
resource2020map <- st_as_sf(resource2020map)

#Map time! 
#Map for spanish
mapspanish <- mapview(resource2020map, 
                      hide = FALSE,
               zcol = "Spanish", 
               col.regions = brewer.pal(n = 3, "PuRd"),
               layer.name = "Translation Availability for Spanish", type = "seq", 
               popup = popupTable(resource2020map, 
                                  feature.id = FALSE, 
                                  row.numbers = FALSE, 
                                  zcol = c("Zip.Code", "Category"))) #will show these attributes when clicking on it over
mapspanish #Note will show the proportion of availability aka between 0-1 for translation availability 0 = no translation, between 0-1 is there is some osrt of translation available, 1 is all the organizations offer translation for the language

#Map for Korean
mapkorean <- mapview(resource2020map, 
                zcol = "Korean", 
                col.regions = brewer.pal(n = 3, "PuRd"),
                layer.name = "Translation Availability for Korean", type = "seq", 
                popup = popupTable(resource2020map, 
                          feature.id = FALSE, 
                          row.numbers = FALSE, 
                          zcol = c("Zip.Code", "Category")))
mapkorean

#Map for Chinese
mapchinese <- mapview(resource2020map, 
                zcol = "Chinese", 
                col.regions = brewer.pal(n = 3, "PuRd"),
                layer.name = "Translation Availability for Chinese", type = "seq", 
                popup = popupTable(resource2020map, 
                          feature.id = FALSE, 
                          row.numbers = FALSE, 
                          zcol = c("Zip.Code", "Category")))
mapchinese

#Map for Tagalog
maptagalog <- mapview(resource2020map, 
                      zcol = "Tagalog..incl..Filipino.", 
                      col.regions = brewer.pal(n = 3, "PuRd"),
                      layer.name = "Translation Availability for Tagalog (including Filipino)", type = "seq", 
                      popup = popupTable(resource2020map, 
                                         feature.id = FALSE, 
                                         row.numbers = FALSE, 
                                         zcol = c("Zip.Code", "Category")))
maptagalog

#Map for Portuguese
mapportuguese <- mapview(resource2020map, 
                      zcol = "Portuguese", 
                      col.regions = brewer.pal(n = 3, "PuRd"),
                      layer.name = "Translation Availability for Portuguese", type = "seq", 
                      popup = popupTable(resource2020map, 
                                         feature.id = FALSE, 
                                         row.numbers = FALSE, 
                                         zcol = c("Zip.Code", "Category")))
mapportuguese

#Map for German
mapgerman <- mapview(resource2020map, 
                         zcol = "German", 
                         col.regions = brewer.pal(n = 3, "PuRd"),
                         layer.name = "Translation Availability for German", type = "seq", 
                         popup = popupTable(resource2020map, 
                                            feature.id = FALSE, 
                                            row.numbers = FALSE, 
                                            zcol = c("Zip.Code", "Category")))
mapgerman

#Map for Haitian
maphc <- mapview(resource2020map, 
                     zcol = "Haitian..Creole.", 
                     col.regions = brewer.pal(n = 3, "PuRd"),
                     layer.name = "Translation Availability for Haitian (Creole)", type = "seq", 
                     popup = popupTable(resource2020map, 
                                        feature.id = FALSE, 
                                        row.numbers = FALSE, 
                                        zcol = c("Zip.Code", "Category")))
maphc

#Map for Vietnamese
mapvietnamese <- mapview(resource2020map, 
                 zcol = "Vietnamese", 
                 col.regions = brewer.pal(n = 3, "PuRd"),
                 layer.name = "Translation Availability for Vietnamese", type = "seq", 
                 popup = popupTable(resource2020map, 
                                    feature.id = FALSE, 
                                    row.numbers = FALSE, 
                                    zcol = c("Zip.Code", "Category")))
mapvietnamese

#Map for French
mapfrench <- mapview(resource2020map, 
                         zcol = "French", 
                         col.regions = brewer.pal(n = 3, "PuRd"),
                         layer.name = "Translation Availability for French", type = "seq", 
                         popup = popupTable(resource2020map, 
                                            feature.id = FALSE, 
                                            row.numbers = FALSE, 
                                            zcol = c("Zip.Code", "Category")))
mapfrench

#Map for Hindi
maphindi <- mapview(resource2020map, 
                     zcol = "Hindi", 
                     col.regions = brewer.pal(n = 3, "PuRd"),
                     layer.name = "Translation Availability for Hindi", type = "seq", 
                     popup = popupTable(resource2020map, 
                                        feature.id = FALSE, 
                                        row.numbers = FALSE, 
                                        zcol = c("Zip.Code", "Category")))
maphindi

#Write out as a file png mapshot2() png (static) or html (interactive)
#doing only html because that is higher quality than the static
#Just take a screenshot of the .html output
install_phantomjs()
mapshot(mapspanish, url = "Spanish_Availability.html")
mapshot(mapkorean, url = "Korean_Availability.html")
mapshot(mapchinese, url = "Chinese_Availability.html")
mapshot(maptagalog, url = "Tagalog_Availability.html")
mapshot(mapportuguese, url = "Portuguese_Availability.html")
mapshot(mapgerman, url = "German_Availability.html")
mapshot(maphc, url = "Haitian_Availability.html")
mapshot(mapvietnamese, url = "Vietnamese_Availability.html")
mapshot(mapfrench, url = "French_Availability.html")
mapshot(maphindi, url = "Hindi_Availability.html") 


#Map by the category column 
#Keep ONLY the ones with the translation availability being yes
resource2020yesonly <- filter(resource2020, Availability == "Y")

#Join with the map geometry 
resource2020yesonly_map <- left_join(resource2020yesonly, zipshape_GNV, by = c("Zip.Code" = "ZCTA5CE10"))

#Turn into the map
#Convert 
resource2020yesonly_map <- st_as_sf(resource2020yesonly_map)

map_category <- mapview(resource2020yesonly_map,  #Most common categories in that offer any translation
                    zcol = "Category", 
                    col.regions = brewer.pal(n = 6, "PuRd"),
                    layer.name = "Translation Availability by Category", #type = "seq", 
                    popup = popupTable(resource2020yesonly_map, 
                                       feature.id = FALSE, 
                                       row.numbers = FALSE,
                                       zcol = c("Zip.Code", "Affiliation")))
map_category

mapshot(map_category, url = "Category_Availability.html") 

############################################################################


####Alachua County Census Tracts###

#Mapping for demographics 
#Pull census tracts for Alachua County Gainesville 
alachua_tracts24 <- tracts(state = "FL", county = "Alachua", year = 2024)
alachua_tracts1419 <- tracts(state = "FL", county = "Alachua", year = 2019)

#Add GEOIDFQ column manually
alachua_tracts1419$GEOIDFQ <- paste0("1400000US", alachua_tracts1419$GEOID)

#Plot the tracts for 2024 - i am only including those ones on the slides 
ggplot(alachua_tracts24)  +
  geom_sf(aes(fill = NAME)) +
  labs(title = "Alachua County Census Tracts",
           fill = "Census Tract Codes")  +
  theme(legend.title = element_text(size = 8), 
            legend.text = element_text(size = 7)) 
ggsave("alachua_tracts.png")

#### S1602 Limited English Speaking Households####

#Read in the data
s1602_ct_2014 <- read.csv("S1602_2014CensusTracts.csv", header = TRUE)
s1602_ct_2014[s1602_ct_2014 == "-"] <- 0.0 #to prevent errors later on? 
s1602_ct_2019 <- read.csv("S1602_2019CensusTracts.csv", header = TRUE)
s1602_ct_2019[s1602_ct_2019 == "-"] <- 0.0 
s1602_ct_2024 <- read.csv("S1602_2024CensusTracts.csv", header = TRUE)
s1602_ct_2024[s1602_ct_2024 == "-"] <- 0.0 

#Merge the data with the map 
s1602_2014_map <- left_join(alachua_tracts1419, s1602_ct_2014, 
                            by = c("GEOIDFQ" = "Geography"))
s1602_2019_map <- left_join(alachua_tracts1419, s1602_ct_2019, 
                            by = c("GEOIDFQ" = "Geography"))
s1602_2024_map <- left_join(alachua_tracts24, s1602_ct_2024, 
                            by = c("GEOIDFQ" = "Geography"))

#Map the data by the percentage: overall, spanish, indo-european, asian-pacific
#Overall
s1602_overall_2014 <- ggplot(s1602_2014_map) + 
  geom_sf(aes(fill = No_one_age_14_and_over_speaks_English_only_or_speaks_English_very_well_Estimate_All_households), 
          color = "black") + 
  labs(title = "Limited English Households in 2014 Overall (%)",
       fill = "Ages 14+ Limited English") + 
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9))
s1602_overall_2014
ggsave("Overall_S1602_2014.png")#Do the save right after running the code, because it will save the most recent ggplot. 
s1602_overall_2019 <- ggplot(s1602_2019_map) + 
  geom_sf(aes(fill = Estimate_Percent_limited_English.speaking_households_All_households), 
          color = "black") + 
  labs(title = "Limited English Households in 2019 Overall (%)",
       fill = "Limited English") + 
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9))
s1602_overall_2019
ggsave("Overall_S1602_2019.png")
s1602_overall_2024 <- ggplot(s1602_2024_map) + 
  geom_sf(aes(fill = Estimate_Percent_limited_English.speaking_households_All_households), 
          color = "black") + 
  labs(title = "Limited English Households in 2024 Overall (%)",
       fill = "Limited English") + 
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9))
s1602_overall_2024
ggsave("Overall_S1602_2024.png")

#spanish only
#s1602_2014_map[s1602_2014_map == "-"] <- 0
s1602_2014_map$No_one_age_14_and_over_speaks_English_only_or_speaks_English_very_well_Estimate_Households_speaking_Spanish <- as.numeric(as.character(s1602_2014_map$No_one_age_14_and_over_speaks_English_only_or_speaks_English_very_well_Estimate_Households_speaking_Spanish))
s1602_spanish_2014 <- ggplot(s1602_2014_map) + 
  geom_sf(aes(fill = No_one_age_14_and_over_speaks_English_only_or_speaks_English_very_well_Estimate_Households_speaking_Spanish), 
          color = "black") + 
  labs(title = "Limited English Households in 2014 speaking Spanish (%)",
       fill = "Ages 14+ Limited English") + 
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9)) 
s1602_spanish_2014
ggsave("Spanish_S1602_2014.png")
s1602_2019_map$Estimate_Percent_limited_English.speaking_households_All_households_Households_speaking_.._Spanish <- as.numeric(as.character(s1602_2019_map$Estimate_Percent_limited_English.speaking_households_All_households_Households_speaking_.._Spanish))
s1602_spanish_2019 <- ggplot(s1602_2019_map) + 
  geom_sf(aes(fill = Estimate_Percent_limited_English.speaking_households_All_households_Households_speaking_.._Spanish), 
          color = "black") + 
  labs(title = "Limited English Households in 2019 Spanish (%)",
       fill = "Limited English") + 
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9)) 
s1602_spanish_2019
ggsave("Spanish_S1602_2019.png")
s1602_spanish_2024 <- ggplot(s1602_2024_map) + 
  geom_sf(aes(fill = Estimate_Percent_limited_English.speaking_households_All_households_Households_speaking_.._Spanish), 
          color = "black") + 
  labs(title = "Limited English Households in 2024 Spanish (%)",
       fill = "Limited English") + 
  theme(legend.title = element_text(size =9), 
        legend.text = element_text(size = 9))
s1602_spanish_2024
ggsave("Spanish_S1602_2024.png")

#Indo-European languages
s1602_2014_map$No_one_age_14_and_over_speaks_English_only_or_speaks_English_very_well_Estimate_Households_speaking_Other_Indo.European_languages <- as.numeric(as.character(s1602_2014_map$No_one_age_14_and_over_speaks_English_only_or_speaks_English_very_well_Estimate_Households_speaking_Other_Indo.European_languages))
s1602_ie_2014 <- ggplot(s1602_2014_map) + 
  geom_sf(aes(fill = No_one_age_14_and_over_speaks_English_only_or_speaks_English_very_well_Estimate_Households_speaking_Other_Indo.European_languages), 
          color = "black") + 
  labs(title = "Limited English Households in 2014 Other Indo-European (%)",
       fill = "Ages 14+ Limited English") + 
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9)) 
s1602_ie_2014
ggsave("OtherIE_S1602_2014.png")
s1602_2019_map$Estimate_Percent_limited_English.speaking_households_All_households_Households_speaking_.._Other_Indo.European_languages <- as.numeric(as.character(s1602_2019_map$Estimate_Percent_limited_English.speaking_households_All_households_Households_speaking_.._Other_Indo.European_languages))
s1602_ie_2019 <- ggplot(s1602_2019_map) + 
  geom_sf(aes(fill = Estimate_Percent_limited_English.speaking_households_All_households_Households_speaking_.._Other_Indo.European_languages), 
          color = "black") + 
  labs(title = "Limited English Households in 2019 Other Indo-European (%)",
       fill = "Limited English") + 
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9)) 
s1602_ie_2019
ggsave("OtherIE_S1602_2019.png")
s1602_2024_map$Estimate_Percent_limited_English.speaking_households_All_households_Households_speaking_.._Other_Indo.European_languages <- as.numeric(as.character(s1602_2024_map$Estimate_Percent_limited_English.speaking_households_All_households_Households_speaking_.._Other_Indo.European_languages))
s1602_ie_2024 <- ggplot(s1602_2024_map) + 
  geom_sf(aes(fill = Estimate_Percent_limited_English.speaking_households_All_households_Households_speaking_.._Other_Indo.European_languages), 
          color = "black") + 
  labs(title = "Limited English Households in 2024 Other Indo-European (%)",
       fill = "Limited English") + 
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9))
s1602_ie_2024
ggsave("OtherIE_S1602_2024.png")

#Asian and Pacific Islander
s1602_2014_map$No_one_age_14_and_over_speaks_English_only_or_speaks_English_very_well_Estimate_Households_speaking_Asian_and_Pacific_Island_languages <- as.numeric(as.character(s1602_2014_map$No_one_age_14_and_over_speaks_English_only_or_speaks_English_very_well_Estimate_Households_speaking_Asian_and_Pacific_Island_languages))
s1602_ap_2014 <- ggplot(s1602_2014_map) + 
  geom_sf(aes(fill = No_one_age_14_and_over_speaks_English_only_or_speaks_English_very_well_Estimate_Households_speaking_Asian_and_Pacific_Island_languages), 
          color = "black") + 
  labs(title = "Limited English Households in 2014 Asian & Pacific Island (%)",
       fill = "Ages 14+ Limited English") + 
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9)) 
s1602_ap_2014
ggsave("AP_S1602_2014.png")
s1602_2019_map$Estimate_Percent_limited_English.speaking_households_All_households_Households_speaking_.._Asian_and_Pacific_Island_languages <- as.numeric(as.character(s1602_2019_map$Estimate_Percent_limited_English.speaking_households_All_households_Households_speaking_.._Asian_and_Pacific_Island_languages))
s1602_ap_2019 <- ggplot(s1602_2019_map) + 
  geom_sf(aes(fill = Estimate_Percent_limited_English.speaking_households_All_households_Households_speaking_.._Asian_and_Pacific_Island_languages), 
          color = "black") + 
  labs(title = "Limited English Households in 2019 Asian & Pacific Island (%)",
       fill = "Limited English") + 
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9)) 
s1602_ap_2019
ggsave("AP_S1602_2019.png")
s1602_2024_map$Estimate_Percent_limited_English.speaking_households_All_households_Households_speaking_.._Asian_and_Pacific_Island_languages <- as.numeric(as.character(s1602_2024_map$Estimate_Percent_limited_English.speaking_households_All_households_Households_speaking_.._Asian_and_Pacific_Island_languages))
s1602_ie_2024 <- ggplot(s1602_2024_map) + 
  geom_sf(aes(fill = Estimate_Percent_limited_English.speaking_households_All_households_Households_speaking_.._Asian_and_Pacific_Island_languages), 
          color = "black") + 
  labs(title = "Limited English Households in 2024 Asian & Pacific Island (%)",
       fill = "Limited English") + 
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9))
s1602_ie_2024
ggsave("AP_S1602_2024.png")



#### S1601 Language Spoken at Home ####

######## IGNORE THIS VARIABLE FOR NOW MAYBE SOME OTHER TIME 


#Read in the data
s1601_ct_2014 <- read.csv("S1601_2014CensusTracts.csv", header = TRUE)
s1601_ct_2019 <- read.csv("S1601_2019CensusTracts.csv", header = TRUE)
s1601_ct_2024 <- read.csv("S1601_2024CensusTracts.csv", header = TRUE)

#Merge with the map
s1601_2014_map <- left_join(alachua_tracts1419, s1601_ct_2014, 
                            by = c("GEOIDFQ" = "Geography"))
s1601_2019_map <- left_join(alachua_tracts1419, s1601_ct_2019, 
                            by = c("GEOIDFQ" = "Geography"))
s1601_2024_map <- left_join(alachua_tracts24, s1601_ct_2024, 
                            by = c("GEOIDFQ" = "Geography"))


#Map adults 18-64 spanish, asian pacific and other indo-european
#2014
s1601_1864_spanish_2014 <- 
  ggplot(s1601_2014_map) + 
  geom_sf(aes(fill = as.numeric(Percent_of_specified_language_speakers_Speak_English__less_than_very_well_Estimate_SPEAK_A_LANGUAGE_OTHER_THAN_ENGLISH_Spanish_or_Spanish_Creole_18.64_years)), 
          color = "black") + 
  labs(title = "Do not speak English very well (%)",
       fill = "Spanish Speakers 2014") + 
  theme(legend.title = element_text(size = 7), 
        legend.text = element_text(size = 6)) 
s1601_1864_spanish_2014
ggsave("Spanish_S1601_2014.png")

#2019

s1601_1864_spanish_2019 <- 
  ggplot(s1601_2019_map) + 
  geom_sf(aes(fill = as.numeric(Estimate_Percent_speak_English_less_than_very_well_Percent_of_specified_language_speakers_Population_5_years_and_over_SPEAK_A_LANGUAGE_OTHER_THAN_ENGLISH_Spanish_18_to_64_years_old)), 
          color = "black") + 
  labs(title = "Do not speak English very well (%)",
       fill = "Spanish Speakers 2019") + 
  theme(legend.title = element_text(size = 7), 
        legend.text = element_text(size = 6)) 
s1601_1864_spanish_2019
ggsave("Spanish_S1601_2019.png")

s1601_1864_ap_2019 <- 
  ggplot(s1601_2019_map) + 
  geom_sf(aes(fill = as.numeric(Estimate_Percent_speak_English_less_than_very_well_Percent_of_specified_language_speakers_Population_5_years_and_over_SPEAK_A_LANGUAGE_OTHER_THAN_ENGLISH_Asian_and_Pacific_Island_languages_18_to_64_years_old)), 
          color = "black") + 
  labs(title = "Do not speak English very well (%)",
       fill = "Asian/PI Speakers 2019") + 
  theme(legend.title = element_text(size = 7), 
        legend.text = element_text(size = 6)) 
s1601_1864_ap_2019
ggsave("AP_S1601_2019.png")

s1601_1864_ie_2019 <- 
  ggplot(s1601_2019_map) + 
  geom_sf(aes(fill = as.numeric(Estimate_Percent_speak_English_less_than_very_well_Percent_of_specified_language_speakers_Population_5_years_and_over_SPEAK_A_LANGUAGE_OTHER_THAN_ENGLISH_Other_Indo.European_languages_18_to_64_years_old)), 
          color = "black") + 
  labs(title = "Do not speak English very well (%)",
       fill = "Other IE Speakers 2019") + 
  theme(legend.title = element_text(size = 7), 
        legend.text = element_text(size = 6)) 
s1601_1864_ie_2019
ggsave("IE_S1601_2019.png")

#2024

s1601_1864_spanish_2019 <- 
  ggplot(s1601_2019_map) + 
  geom_sf(aes(fill = as.numeric(Estimate_Percent_speak_English_less_than_very_well_Percent_of_specified_language_speakers_Population_5_years_and_over_SPEAK_A_LANGUAGE_OTHER_THAN_ENGLISH_Spanish_18_to_64_years_old)), 
          color = "black") + 
  labs(title = "Do not speak English very well (%)",
       fill = "Spanish Speakers 2019") + 
  theme(legend.title = element_text(size = 7), 
        legend.text = element_text(size = 6)) 
s1601_1864_spanish_2019
ggsave("Spanish_S1601_2019.png")

#Map 65+ adults spanish, asian pacific and other indo-european 

#2014

#2019

s1601_65_spanish_2019 <- 
  ggplot(s1601_2019_map) + 
  geom_sf(aes(fill = as.numeric(Estimate_Percent_speak_English_less_than_very_well_Percent_of_specified_language_speakers_Population_5_years_and_over_SPEAK_A_LANGUAGE_OTHER_THAN_ENGLISH_Other_Indo.European_languages_18_to_64_years_old)), 
          color = "black") + 
  labs(title = "Do not speak English very well (%)",
       fill = "Spanish Speakers 2019") + 
  theme(legend.title = element_text(size = 7), 
        legend.text = element_text(size = 6)) 
s1601_65_spanish_2019
ggsave("Spanish65_S1601_2019.png")

s1601_65_ap_2019 <- 
  ggplot(s1601_2019_map) + 
  geom_sf(aes(fill = as.numeric(Estimate_Percent_speak_English_less_than_very_well_Percent_of_specified_language_speakers_Population_5_years_and_over_SPEAK_A_LANGUAGE_OTHER_THAN_ENGLISH_Other_Indo.European_languages_18_to_64_years_old)), 
          color = "black") + 
  labs(title = "Do not speak English very well (%)",
       fill = "Asian Pacific Speakers 2019") + 
  theme(legend.title = element_text(size = 7), 
        legend.text = element_text(size = 6)) 
s1601_65_ap_2019
ggsave("AP65_S1601_2019.png")

#2024


#### B16005 nativity by language spoken ####

#Read in the data 
b16005_ct_2014 <- read.csv("B16005_2014CensusTracts.csv")
b16005_ct_2019 <- read.csv("B16005_2019CensusTracts.csv")
b16005_ct_2024 <- read.csv("B16005_2024CensusTracts.csv")

#join together with the geography 
b16005_map2014 <- left_join(alachua_tracts1419, b16005_ct_2014, 
                              by = c("GEOIDFQ" = "Geography"))
b16005_map2019 <- left_join(alachua_tracts1419, b16005_ct_2019, 
                            by = c("GEOIDFQ" = "Geography"))
b16005_map2024 <- left_join(alachua_tracts24, b16005_ct_2024, 
                            by = c("GEOIDFQ" = "Geography"))



#Mapping 2014 Foreign-Born colunmns
ggplot(b16005_map2014) + #Spanish doesn't speak well/at all
  geom_sf(aes(fill = as.numeric(FB_Doesnt_Speak_Well_At_All_Spanish)), 
          color = "black") + 
  labs(title = "Don't speak English Well/At All Spanish 2014",
       fill = "Number of People") + 
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9)) 
ggsave("Spanish_B16005_2014.png")


ggplot(b16005_map2014) + #Other Indo-European doesn't speak well/at all
  geom_sf(aes(fill = as.numeric(FB_Doesnt_Speak_Well_At_All_Other_Indo_European)), 
          color = "black") + 
  labs(title = "Don't speak English Well/At All Other Indo-European 2014",
       fill = "Number of People") + 
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9)) 
ggsave("OIE_B16005_2014.png")

ggplot(b16005_map2014) + # Asian Pacific Islander doesn't speak well/at all
  geom_sf(aes(fill = as.numeric(FB_Doesnt_Speak_Well_At_All_Asian_Pacific_Island)), 
          color = "black") + 
  labs(title = "Don't speak English Well/At All Asian & Pacific Islander 2014",
       fill = "Number of People") + 
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9)) 
ggsave("API_B16005_2014.png")


#2019
ggplot(b16005_map2019) + #Spanish doesn't speak well/at all
  geom_sf(aes(fill = as.numeric(FB_Doesnt_Speak_Well_At_All_Spanish)), 
          color = "black") + 
  labs(title = "Don't speak English Well/At All Spanish 2019",
       fill = "Number of People") + 
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9)) 
ggsave("Spanish_B16005_2019.png")


ggplot(b16005_map2019) + #Other Indo-European doesn't speak well/at all
  geom_sf(aes(fill = as.numeric(FB_Doesnt_Speak_Well_At_All_Other_Indo_European)), 
          color = "black") + 
  labs(title = "Don't speak English Well/At All Other Indo-European 2019",
       fill = "Number of People") + 
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9)) 
ggsave("OIE_B16005_2019.png")

ggplot(b16005_map2019) + #Asian Pacific Islander doesn't speak well/at all
  geom_sf(aes(fill = as.numeric(FB_Doesnt_Speak_Well_At_All_Asian_Pacific_Island)), 
          color = "black") + 
  labs(title = "Don't speak English Well/At All Asian & Pacific Islander 2019",
       fill = "Number of People") + 
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9)) 
ggsave("API_B16005_2019.png")

#2024
ggplot(b16005_map2024) + #Spanish doesn't speak well/at all
  geom_sf(aes(fill = as.numeric(FB_Doesnt_Speak_Well_At_All_Spanish)), 
          color = "black") + 
  labs(title = "Don't speak English Well/At All Spanish 2024",
       fill = "Number of People") + 
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9)) 
ggsave("Spanish_B16005_2024.png")


ggplot(b16005_map2024) + #Other Indo-European doesn't speak well/at all
  geom_sf(aes(fill = as.numeric(FB_Doesnt_Speak_Well_At_All_Other_Indo_European)), 
          color = "black") + 
  labs(title = "Don't speak English Well/At All Other Indo-European 2024",
       fill = "Number of People") + 
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9)) 
ggsave("OIE_B16005_2024.png")

ggplot(b16005_map2024) + #Asian Pacific Islander doesn't speak well/at all
  geom_sf(aes(fill = as.numeric(FB_Doesnt_Speak_Well_At_All_Asian_Pacific_Island)), 
          color = "black") + 
  labs(title = "Don't speak English Well/At All Asian & Pacific Islander 2024",
       fill = "Number of People") + 
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9)) 
ggsave("API_B16005_2024.png")









