##R Code to generate Figure 1a


#Download and plot NOAA climate data
#code modified from: https://github.com/SamMajumder/GeoVizHub/tree/main/TempDiffUS

###########################################################

options(scipen=999)

packages <- list("ncdf4","here","tidyverse","terra","sf", "devtools")

lapply(packages, require,character.only = T) 

source_url("https://github.com/SamMajumder/GeoVizHub/blob/main/TempDiffUS/Code/Functions.R")

# List of directories to create
dirs_to_create <- c("RawDatasets","ProcessedDatasets2021")

# Loop through the directories and create them if they don't exist
for (dir_name in dirs_to_create) {
  dir_path <- here(dir_name)
  
  if (!dir.exists(dir_path)) {
    dir.create(dir_path)
  }
}   

### Creating an additional folder within RawDatasets ###

# List of directories to create
dirs_to_create <- c("USCountyCentroid")

# Loop through the directories and create them if they don't exist
for (dir_name in dirs_to_create) {
  dir_path <- here("RawDatasets",
                   dir_name)
  
  if (!dir.exists(dir_path)) {
    dir.create(dir_path)
  }
}  



###################################
##### Download gridded climate data ###

#### setting a high timeout limit of 10 minutes 

options(timeout=600)

######### download monthly gridded tmin from 1895 to present

#Data Source: Average temperature data from NOAA Monthly U.S. Climate Gridded Dataset. https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ncdc:C00332
#Citation: Vose, Russell S., Applequist, Scott, Squires, Mike, Durre, Imke, Menne, Matthew J., Williams, Claude N. Jr., Fenimore, Chris, Gleason, Karin, and Arndt, Derek (2014): NOAA Monthly U.S. Climate Gridded Dataset (NClimGrid), Version 1. [1960 and 2022]. NOAA National Centers for Environmental Information. DOI:10.7289/V5SX6B56 [11/4/2023].

# Define the URL
url <- "https://www.ncei.noaa.gov/data/nclimgrid-monthly/access/nclimgrid_tmin.nc"


# Define the destination file path using here()
dest_file <- here("RawDatasets", "nclimgrid_tmin.nc")


# Download the file
download.file(url, dest_file, mode = "wb")

######### download Feb 2021 temperature daily gridded data
# Define the URL
url <- "https://www.ncei.noaa.gov/data/nclimgrid-daily/access/grids/2021/ncdd-202102-grd-scaled.nc"


# Define the destination file path using here()
dest_file <- here("RawDatasets", "nclimgrid_feb2021.nc")


# Download the file
download.file(url, dest_file, mode = "wb")

##########
## download the US county centroid shapefile ##
######## 
#Data Source: US county centroid shapefile from the National Weather Service. https://www.weather.gov/gis/Counties
#Citation Information:
#Originator: National Weather Service
#Publication Date: 1995
#Title: Counties of U.S.
#Geospatial Data Presentation Form: vector digital data
#Publication Place: Silver Spring, MD
#Publisher: National Weather Service
#Online Linkage: National Weather Service Geodata

# Define the URL
url <- "https://www.weather.gov/source/gis/Shapefiles/County/c_08mr23.zip"


# Define the destination file path using here()
dest_file <- here("RawDatasets", "c_08mr23.zip")


# Download the file
download.file(url, dest_file, mode = "wb")


# Define the path to the zip file
centroid_shapefile_zip <- here("RawDatasets", "c_08mr23.zip")

# Define the directory where the unzipped files should be stored
centroid_shapefile_unzipped <- here("RawDatasets", "USCountyCentroid")

# Unzip the file
unzip(zipfile = centroid_shapefile_zip, exdir = centroid_shapefile_unzipped)


############
#### export the layers for Febuary Tmin from 1971 - 2020 (50 years)
##########
tmin <- terra::rast(here("RawDatasets","nclimgrid_tmin.nc"))

output_dir <- here("ProcessedDatasetsMonthlyTmin")

#dataset starts in 1895 and has 12 months per year, select second month of each year from 1971 through 2020
export_layers(tmin,seq(from=(1+(1971-1895)*12),to=(1+(2020-1895)*12), by=12),output_dir)


#### exporting layers tmin from Feb 11-20, 2021

feb21 <- terra::rast(here("RawDatasets","nclimgrid_feb2021.nc"))

output_dir_2 <- here("ProcessedDatasets2021")


export_layers(feb21,39:48,output_dir_2)


#########
## now read in the county centroid shapefile #
########### 

US_county_centroid <- sf::st_read(here("RawDatasets",
                                       "USCountyCentroid",
                                       "c_08mr23.shp"))


Coordinates <- US_county_centroid %>% 
                            dplyr::select(STATE,COUNTYNAME,
                                          LON,LAT) %>%
                            sf::st_drop_geometry()





########## 
### Extracting values at points for Feb 2021
#######


rasters_2021 <- list.files(path = here("ProcessedDatasets2021"),
                           pattern = ".*\\.tif$",full.names = TRUE)

Feb_2021_list <- geoRflow_raster_pipeline_point(rasters_2021,
                                                 Coordinates,
                                                 lat_col = "LAT",
                                                 lon_col = "LON",
                                                 method = "terra",
                                                 crs = "EPSG:4326")





################ 
#### Extracting the list that contains the dataframes with values ###

# Define the new column names
new_column_names <- c("STATE", "COUNTYNAME", "LON", "LAT", 
                      "Feb11", "Feb12", "Feb13", "Feb14", 
                      "Feb15", "Feb16", "Feb17", "Feb18", 
                      "Feb19", "Feb20")
                     

Min_temp_2021_monthly <- process_dataframes(Feb_2021_list,
                                    "dataframes_with_values",
                                    new_col_names = new_column_names,
                                    extract_index = 1) %>% 
                                         tidyr::drop_na()



Min_temp_2021 <- Min_temp_2021_monthly %>% 
                                tidyr::drop_na() %>% 
                                dplyr::rowwise() %>%
                                dplyr::summarise(Feb_2021 = min(Feb11:Feb20)) %>%
                                dplyr::mutate(STATE = Min_temp_2021_monthly$STATE,
                                              COUNTYNAME =Min_temp_2021_monthly$COUNTYNAME,
                                              LON = Min_temp_2021_monthly$LON,
                                              LAT = Min_temp_2021_monthly$LAT)






########## 
### Extracting values at points for all Februaries before 2021
#######





## listing all tmin files 

rasters_tmin <- list.files(path = here("ProcessedDatasetsMonthlyTmin"),
                           pattern = ".*\\.tif$",full.names = TRUE)

Tmin_list <- geoRflow_raster_pipeline_point(rasters_tmin,
                                                Coordinates,
                                                lat_col = "LAT",
                                                lon_col = "LON",
                                                method = "terra",
                                                crs = "EPSG:4326")




################ 
#### Extracting the list that contains the dataframes with values ###

# Define the new column names
 new_column_names <- c("STATE", "COUNTYNAME", "LON", "LAT", 
                      "Feb1971", "Feb1972", "Feb1973", "Feb1974", "Feb1975", "Feb1976", "Feb1977", "Feb1978", "Feb1979", "Feb1980", "Feb1981", "Feb1982", "Feb1983", "Feb1984", "Feb1985", "Feb1986", "Feb1987", "Feb1988", "Feb1989", "Feb1990", "Feb1991", "Feb1992", "Feb1993", "Feb1994", "Feb1995", "Feb1996", "Feb1997", "Feb1998", "Feb1999", "Feb2000", "Feb2001", "Feb2002", "Feb2003", "Feb2004", "Feb2005", "Feb2006", "Feb2007", "Feb2008", "Feb2009", "Feb2010", "Feb2011", "Feb2012", "Feb2013", "Feb2014", "Feb2015", "Feb2016", "Feb2017", "Feb2018", "Feb2019", "Feb2020")



Min_temp_Feb_yearly <- process_dataframes(Tmin_list,
                                    "dataframes_with_values",
                                    new_col_names = new_column_names,
                                    extract_index = 1) %>% 
                                         tidyr::drop_na()


#mean minimum temp in Feb from 1971 to 2020
Min_temp_Feb <- Min_temp_Feb_yearly %>% 
                                tidyr::drop_na() %>% 
                                dplyr::rowwise() %>%
                                dplyr::summarise(FebavgMin = mean(Feb1971:Feb2020)) %>%
                                dplyr::mutate(STATE = Min_temp_Feb_yearly$STATE,
                                              COUNTYNAME =Min_temp_Feb_yearly$COUNTYNAME,
                                              LON = Min_temp_Feb_yearly$LON,
                                              LAT = Min_temp_Feb_yearly$LAT)




##### Joining the two datasets ####
## and joining the county centroids data 

Avg_temp_change <- Min_temp_2021 %>%
                        dplyr::inner_join(Min_temp_Feb) %>% 
                        dplyr::mutate(Temp_change = Feb_2021 - FebavgMin) %>%
                        dplyr::inner_join(US_county_centroid) %>%
                        sf::st_as_sf()

##############################################################################

# PMCA Scout Arrival data for the 2021 season up to Feb 20

pmca <- read.csv("2021_scout-arrival-data-pre_Feb20.csv")

# prepare PMCA data for mapping
pmca_sf <- pmca %>% 
  # convert to spatial points
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_transform(crs = st_crs("ESRI:102003"))


##############################################################################
#plot

library(raster)
library(rgeos)
library(RColorBrewer)
library(ggplot2)
library("maps")
library("tools")


states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

states <- cbind(states, st_coordinates(st_centroid(states)))

states$ID <- toTitleCase(states$ID)

crop_custom <- function(poly.sf) {
  poly.sp <- as(poly.sf, "Spatial")
  poly.sp.crop <- crop(poly.sp, extent(c(-107, -77, 24, 38)))
  st_as_sf(poly.sp.crop)
}

cropped <- crop_custom(Avg_temp_change)
cropped_states <- crop_custom(states)

#sampling sites
sites <- read.csv("Table_S4.csv")
coords_sites<-st_as_sf(sites, coords=c("Longitude","Latitude"), crs = 4326) #storm samples
coords_after_sites<-st_as_sf(sites[c(2,3,12,13,16,18,21:25,30),], coords=c("Longitude","Latitude"), crs = 4326) #May 2021 samples

pal <- hcl.colors(n = 11, palette = "Blue-Red2", rev=TRUE)

p <- ggplot(data = cropped) +
  geom_sf(aes(fill = Temp_change), color = NA) + #min temp from Feb 11-20
  scale_fill_gradientn(colours=rev(pal), rescaler = ~ scales::rescale_mid(.x, mid = 0)) +
  geom_sf(data=pmca_sf, color="black") + #PMCA Scout arrival data 
  geom_sf(data=coords_sites, shape = 22, color = "black", size=5, stroke=2)+ 
  geom_sf(data=coords_after_sites, shape = 22, color = "white", size=5, stroke=2)+ 
  geom_sf(data = cropped_states, fill = NA) + 
  #geom_text(data = cropped_states, aes(X, Y, label = ID), size = 6) +
  theme_void() +  # Use a minimal theme
  labs(fill = "Min. Temp. Anomaly") +
  coord_sf(crs = 4326) +
  theme(panel.background = element_rect(fill = "white",
                                        color = NA),
        plot.background = element_rect(fill = "white",
                                       color = NA),
        legend.title = element_text(face ="bold",
                                    size = 20),
        legend.text = element_text(size = 20),
        legend.position = "right", 
        legend.key.size = unit(2, 'cm'),
        plot.caption = element_text(size = 12))

ggsave("Fig1a.png", p, 
       width = 22, height = 10)
