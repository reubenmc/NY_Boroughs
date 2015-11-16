library(magrittr)
library(dplyr)
library(data.table)
library(lubridate)
library(stringr)
library(ggplot2)


#Read in the 311 data and save as a dataframe called nyc#
nyc = fread("~cr173/Sta523/data/nyc/nyc_311.csv") %>% 
  as.data.frame() %>%
  tbl_df()

#Load in intersection data#
#Dataframe called data#
load("/home/vis/cr173/Sta523/data/nyc/intersections/intersections.Rdata")
#Change column Stree1 to Street1#
data = rename(data, Street1 = Stree1)

#Load pluto address data#
#Dataframe called pluto#
load("/home/vis/cr173/Sta523/data/nyc/pluto/pluto.Rdata")


#Remove unnecessary variables from the data set#
nyc = nyc %>%
  select(-Unique.Key,
         -Resolution.Description,
         -(Park.Facility.Name:Ferry.Terminal.Name))

#Convert zip codes to integers#
#Format facility.type to have uniform NA values, not character N/A#
nyc %>%
  mutate(Incident.Zip = as.integer(Incident.Zip),
         Facility.Type = ifelse(Facility.Type == "N/A", NA, Facility.Type))
#Possibly remove facility.type as a variable later#

#No zip codes begin with 0, so mark any such zips as NA#
nyc %>%
  mutate(Incident.Zip = ifelse(Incident.Zip < 10000, NA, Incident.Zip))

#Change borough name to borough code#
boroughs = c("BRONX"="BX","BROOKLYN"="BK",
             "MANHATTAN"="MN","QUEENS"="QN",
             "STATEN ISLAND"="SI")

#Create subset of data that has an incident address and borough specified#
addr = nyc %>%
  select(Created.Date,Complaint.Type, contains("Incident"), Borough) %>%
  filter(Incident.Address != "",Borough != "Unspecified" ) %>%
  mutate(Borough, Borough = boroughs[Borough]) 

#Filter so we only select unique address#
addr = addr[!duplicated(addr$Incident.Address),]

#Change column name from Incident.Address to Address#
#Change column name from Incident.Zip to ZipCode#
addr = rename(addr, ZipCode = Incident.Zip, Address = Incident.Address)


#Create subset of data that has location specified by intersection and has the#
#borough labeled#
inter = nyc %>%
  select(Created.Date,Complaint.Type, Incident.Zip, 
         contains("Intersection"), Address.Type, Borough) %>%
  filter(Address.Type == "INTERSECTION",Borough != "Unspecified") %>%
  mutate(Borough, Borough = boroughs[Borough]) 

#Change column name from Intersection.Street.1/2 to Street1/2#
#Change column name from Incident.Zip to ZipCode#
inter = rename(inter, Street1 = Intersection.Street.1,
               Street2 = Intersection.Street.2)


#Filter so we only select unique intersections#
inter = inter %>%
  filter(Street1 != Street2)


#Change street/aveunue, etc. to abbreviation#
inter = as.data.frame(sapply(inter,str_replace,
                              pattern="AVENUE",replacement = "AVE"))
inter =as.data.frame(sapply(inter, str_replace,
                            pattern="STREET",replacement = "ST"))
inter = as.data.frame(sapply(inter,str_replace,
                            pattern="BOULEVARD",replacement = "BVD"))
inter = as.data.frame(sapply(inter,str_replace,
                             pattern="LANE",replacement = "LN"))
inter = as.data.frame(sapply(inter,str_replace,
                             pattern="DRIVE",replacement = "DR"))
inter = as.data.frame(sapply(inter,str_replace,
                             pattern="COURT",replacement = "CT"))
#Convert string columns back to characters#
inter = inter %>%
  mutate(Street1, Street1 = as.character(Street1)) %>%
  mutate(Street2, Street2 = as.character(Street2))
data = data %>%
  mutate(Street1, Street1 = as.character(Street1)) %>%
  mutate(Street2, Street2 = as.character(Street2))


#Join addr and pluto to find matches#
#Call new dataframe latLongs#
latLongs = inner_join(addr, pluto, by = c("Borough", "Address"))
#Filter so we only select unique address#
latLongs = latLongs[!duplicated(latLongs$Address),]

#Join inter and data to find matches#
#Call new dataframe intersections#
intersections = inner_join(inter, data, by = c("Street1", "Street2"))



#Plot latutide and longitude of matches#
ggplot(latLongs, aes(x=x, y=y, colour=Borough)) +
  geom_point(size = 0.1) + ggtitle("New York Boroughs") +
  guides(colour = guide_legend(override.aes = list(size=3)))

#Merge intersections and latLongs data frames#
#First only select variables of interest for recreating boroughs and visualization#
latLongs = latLongs %>%
  select(Created.Date, Complaint.Type, Borough, x, y)
#Rename x and y to longitude and latitude#
latLongs = rename(latLongs, longitude = x, latitude = y)
intersections = intersections %>%
  select(Created.Date, Complaint.Type, Borough, longitude, latitude)

nyBoroughs = unique(rbind(latLongs, intersections))
save(nyBoroughs,file="nyBoroughs.Rdata")
