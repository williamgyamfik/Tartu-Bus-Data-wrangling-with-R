
library(tidyr)
library(tidyverse)
library(sf)
library(ggrepel)
library(psych)

#Extracting boundary Shapefile of Tartu county  
asustusyksu <- st_read("asustusyksus_20220401.shp")
tartu_admin_units <-asustusyksu %>% filter( MNIMI == 'Tartu maakond') 


#Tartu settlement units Centroid
tartu_admin_units_center <- tartu_admin_units  %>% st_centroid() %>% select(ANIMI)
tartu_admin_units_xy <- tartu_admin_units_center %>% st_coordinates() %>% as_tibble()
tartu_admin_units_names <- tartu_admin_units_center %>% st_drop_geometry()
tartu_admin_units_xy <- cbind(tartu_admin_units_names,tartu_admin_units_xy)

#Import bus data from local storage 
Data_agg2019<- read_csv("C:/Data_Spatialagg2019.csv",locale = locale(encoding = "ISO-8859-1"))

#Set time to local time
Sys.setlocale("LC_TIME", "English")

Sys.setlocale("LC_TIME", "Estonian")
glimpse(data_tmp)

#convert date column into year,month and day  
data_tmp <- data_tmp %>% mutate(Date = dmy(Date),
                                wday = lubridate :: wday(Date, abbr = T, label = T))


data_tmp <- data_tmp %>% mutate(year = year(Date), month = month(Date), day = day(Date))


#Convert POSIX time format
data_tmp <- data_tmp %>% mutate(Start = as.POSIXct(Start,format = "%Y-%m-%d %H:%M : %S"))
data_tmp <- data_tmp %>% mutate(Time = format(Start, format = "%H:%M:%S"),
                                hour = as.integer(format(Start, format = "%H")),
                                minute = as.integer(format(Start, format = "%M")))


data_tmp <- data_tmp %>% mutate(Time = as.hms(Time))
glimpse(data_tmp)

#create backup
data_tmp_copy <- data_tmp
glimpse(data_tmp)

# deleanate year 2019 for analysis 
data_tmp_2019 <- data_tmp %>% filter(year == 2019)
nrow(data_tmp_2019)

#filter NA's
data_tmp_2019 <- data_tmp_2019 %>% filter(!is.na(Dest_lon))
nrow(data_tmp_2019)
#about 4% data missing (53331 rows)

#aggregate data
data_tmp_2019_agg <- data_tmp_2019 %>% dplyr::group_by(Stop_lon,Stop_lat,Departure_stop,Time,Trip,Date,Line,Dest_lat,Dest_lon, Destination,wday,year,month,day,hour,minute) %>% 
    dplyr::summarise(n = sum(Amount)) %>% ungroup()

#create unique ids
data_tmp_2019_agg <- tibble::rowid_to_column(data_tmp_2019_agg, 'id')

#create backup
data_tmp_2019_agg_copy <- data_tmp_2019_agg
view(data_tmp_2019_agg)

write.csv(data_tmp_2019_agg,file='C:/DataAgg/data_tmp_2019_agg.csv',row.names=FALSE)


#split data into origin and destination then convert to WGS84
data_tmp2019_wgs_a <- select(data_tmp_2019_agg,id,Stop_lon,Stop_lat,Departure_stop,n,Time,Trip,Date,Line) 
data_tmp2019_wgs_b <- select(data_tmp_2019_agg,id,Dest_lon,Dest_lat,Destination,Departure_stop,wday,year,month,day,hour,minute)


data_tmp2019_wgs_a <- st_as_sf(data_tmp2019_wgs_a,coords = c('Stop_lon','Stop_lat') ,crs=4326)
data_tmp2019_wgs_b <- st_as_sf(data_tmp2019_wgs_b, coords = c('Dest_lon','Dest_lat'), crs = 4326)


#convert from WGS84 to Estonian crs 3301

data_tmp_2019_lest97_a <- st_transform(data_tmp2019_wgs_a,crs = 3301)
data_tmp_2019_lest97_b <- st_transform(data_tmp2019_wgs_b,crs = 3301)

data_tmp_2019_SF <- cbind (data_tmp_2019_lest97_a, data_tmp_2019_lest97_b)

#Spatial join to data frame
data_tmp_2019_SF_join <- st_join(st_transform(data_tmp_2019_SF, 3301),
                                 st_transform(tartu_admin_units,3301), join = st_within) 

#Split into Origin-Destination data frames
data_tmp_2019_SF_join_origin <- data_tmp_2019_SF_joinA %>% 
    select(id,n,geometry,Time,Date,Line,ANIMI,Departure_stop)

data_tmp_2019_SF_join_dest <- data_tmp_2019_SF_joinB %>% 
    select(id,Destination, wday,year,month,day,hour,minute,geometry)

#Origin wgs
originWGSelect <- data_tmp_2019_SF_join_origin %>% dplyr::select(geometry)
originWGS <- st_transform(originWGSelect, 4326) 
originWGS <- originWGS %>% st_coordinates(originWGS)


#Destination wgs
destWGSelect <- data_tmp_2019_SF_join_dest %>% dplyr::select(geometry)
destWGS <- st_transform(destWGSelect, 4326) 
destWGS <- destWGS %>% st_coordinates(destWGS)

#Column bind data
data_tmp_2019_SF_join_origin <- cbind(data_tmp_2019_SF_join_origin,originWGS)
data_tmp_2019_SF_join_dest <- cbind(data_tmp_2019_SF_join_dest,destWGS )

#Rename column
data_tmp_2019_SF_join_origin <- data_tmp_2019_SF_join_origin %>%dplyr:: rename(origin_x_wgs = X, origin_y_wgs = Y )
data_tmp_2019_SF_join_origin <- data_tmp_2019_SF_join_origin %>%dplyr:: rename(origin_animi = ANIMI)
data_tmp_2019_SF_join_origin <- data_tmp_2019_SF_join_origin %>%dplyr:: rename( origin_geometry= geometry)

data_tmp_2019_SF_join_dest <- data_tmp_2019_SF_join_dest %>% dplyr::rename(dest_geometry = geometry,dest_x_wgs = X, dest_y_wgs = Y )

#spatial Join Origin-destination data frames to settlement units
data_tmp_2019_SF_join_origin_admin_units<- st_join(st_transform(data_tmp_2019_SF_join_origin, 3301),
                                                   st_transform(tartu_admin_units,3301), join = st_within)  



data_tmp_2019_SF_join_dest_admin_units<-sf::st_join(st_transform(data_tmp_2019_SF_join_dest, 3301),
                                                    st_transform(tartu_admin_units,3301), join = st_within) 


 

#filter NA's
data_tmp_2019_SF_join_dest_admin_units <- data_tmp_2019_SF_join_dest_admin_units %>% dplyr:: filter(!is.na(ANIMI))
data_tmp_2019_SF_join_origin_admin_units <- data_tmp_2019_SF_join_origin_admin_units %>% dplyr:: filter(!is.na(ANIMI)) 

#remove unecessary columns
data_tmp_2019_SF_join_dest_admin_units <- data_tmp_2019_SF_join_dest_admin_units %>% 
    select(-MKOOD,-MNIMI,-TYYP,-AKOOD,-MKOOD)

data_tmp_2019_SF_join_origin_admin_units <- data_tmp_2019_SF_join_origin_admin_units %>% 
    select(-AKOOD,-MKOOD,-MNIMI,-TYYP,-ANIMI) 

 
# data_tmp_2019_SF_join_origin_admin_units <- data_tmp_2019_SF_join_origin_admin_units %>% select(-geometry) %>%  as.data.frame()
data_tmp_2019_SF_join_origin_admin_units <- data_tmp_2019_SF_join_origin_admin_units %>% dplyr::rename(ANIMI = origin_animi)

origin_join_2019 <- left_join(tartu_admin_units_xy,data_tmp_2019_SF_join_origin_admin_units, by=c('ANIMI'))

nrow(origin_join_2019)
origin_join_2019 <-origin_join_2019  %>% dplyr:: select(-origin_geometry)

origin_join_2019 <- origin_join_2019 %>%dplyr:: rename(origin_animi = ANIMI, origin_x = X, origin_y = Y)


#Destination join
data_tmp_2019_SF_join_dest_admin_units <- data_tmp_2019_SF_join_dest_admin_units %>% 
    select(-dest_geometry) %>%  as.data.frame()


destination_join_2019 <- left_join(tartu_admin_units_xy,data_tmp_2019_SF_join_dest_admin_units, by=c('ANIMI')) 
data_tmp_2019_SF_join_dest_admin_units <- data_tmp_2019_SF_join_dest_admin_units %>% dplyr::rename(dest_animi = ANIMI)

destination_join_2019 <- destination_join_2019 %>%  dplyr::rename(dest_animi = ANIMI, dest_x = X, dest_y = Y)

Merge dataframe with id as key
Data_agg2019 <- merge(x=origin_join_2019,y=destination_join_2019, by.x='id', by.y ='id', all = FALSE)

colnames(Data_agg2019)
Data_agg2019Bkup <- Data_agg2019
Data_agg2019 <- Data_agg2019 %>% dplyr::select(-ONIMI.y, -OKOOD.y,-dest_geometry, -OKOOD.x)
Data_agg2019 <- Data_agg2019 %>% dplyr::select(-Departure_stop,-Destination)

#create backup
Data_agg2019Copy <- Data_agg2019

write.csv(Data_agg2019, file = 'C:/Data_agg2019.csv', row.names = FALSE)

colnames(Data_agg2019)


#Spatial data analysis

# Total trips for the year 2019 aggregated for Origin-Destination trips 
Data_agg2019TotalTrips <- Data_agg2019 %>% dplyr::group_by(origin_animi,origin_x,origin_y,dest_animi,dest_x,dest_y,origin_x_wgs,origin_y_wgs,dest_x_wgs,dest_y_wgs) %>% 
    dplyr:: summarise(n = sum(n))
nrow(Data_agg2019TotalTrips)

Data_agg2019TotalTrips <- Data_agg2019TotalTrips %>% filter(!is.na(n))
Data_agg2019TotalTrips <- Data_agg2019TotalTrips %>% dplyr:: filter(dest_x != origin_x)




