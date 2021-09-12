##### Here there are a few functions that I always use to install and load libraries #####
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)>0){
    print(paste0("Installing ", paste(new.pkg,collapse = ", ")))
    if (!requireNamespace("BiocManager", quietly = TRUE)){
      install.packages("BiocManager")
    }
    bioconductor_packages <- BiocManager::available()
    bioconductor_packages <- bioconductor_packages[bioconductor_packages %in% new.pkg]
    if (length(bioconductor_packages) > 0){
      BiocManager::install(bioconductor_packages, dependencies = TRUE,ask=FALSE)
    }
    new.pkg = new.pkg[!(new.pkg %in% bioconductor_packages)]
    if (length(new.pkg)>0){
      install.packages(new.pkg, dependencies = TRUE)
    }
  }
  res <- lapply(pkg,load.packages)
}

load.packages <- function(pkg){
  print(paste0("Loading ",pkg))
  suppressMessages(require(pkg,character.only = T))
}

##### Install and loading the libraries that are going to be used #####
check.packages(c("GADMTools","raster","RColorBrewer","dplyr","gdata","glue","stringr","leaflet","ggplot2","sf"))

##### GADM and GADMTools #####

browseURL("https://gadm.org/") # Open GADM website where are located geographical data for the entire world

?GADMTools #Instead of downloading the data by hand, we will use the library named GADMTools


### Basic operations ###
level0_spain = gadm_sf_loadCountries(fileNames = "ESP", level=0, basefile="./")$sf # gadm_sf_loadCountries download the information from a country and saves in a RDS

#sf is a dataframe with coordenates in R
print(level0_spain)

#sf can be plotted with ggplot
ggplot(data = level0_spain) +
  geom_sf()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())



### Which data?
data("GADM36SF") # With this dataframe we can check the country codes and the subregions
View(GADM36SF)

# Different Levels
print(GADM36SF[GADM36SF$LEVEL_0 == "Spain",])  # ID is the code, Level 0 is the entire country, Level 1 are Autonomous Communities...
print(GADM36SF[GADM36SF$LEVEL_0 == "France",]) # ID is the code, Level 0 is the entire country, Level 1 are Region...


level1_france = gadm_sf_loadCountries("FRA", level=1, basefile="./")$sf # Download Region coordinates from France
level1_spain = gadm_sf_loadCountries("ESP", level=1, basefile="./")$sf # Download Autonomous Communities coordinates from Spain

### Plot the coords ###
ggplot(data = level1_france) +
  geom_sf()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())

ggplot(data = level1_spain) +
  geom_sf()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())

#Change Level
level2_spain = gadm_sf_loadCountries("ESP", level=2, basefile="./")$sf
ggplot(data = level2_spain) +
  geom_sf(fill="white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())


#### Basic Operation: rbind

spain_and_france = rbind(level1_france,level1_spain)
ggplot(data = spain_and_france) +
  geom_sf(aes(fill=NAME_0))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())


#### Basic Operation: subset

andalusia = subset(level1_spain,level1_spain$NAME_1 == "Andalucía") # Subset Andalusia
ggplot(data = andalusia) +
  geom_sf()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())


andalusia = subset(level2_spain,level2_spain$NAME_1 == "Andalucía") # Subset Andalusia
ggplot(data = andalusia) +
  geom_sf()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())

#### Basic Operation: Intersect

points_to_map = data.frame(Name=c("Pico Veleta","Alhambra","Catedral","Aeropuerto"),
                           Longitude=c(-3.36,-3.58,-3.60,-3.78),
                           Latitude=c(37.05,37.17,37.18,37.19))

level4_spain = gadm_sf_loadCountries("ESP", level=4, basefile="./")$sf
granada_city = subset(level4_spain,level4_spain$NAME_4 == "Granada") # Extract all municipalities from the province of Granada

points_to_map = st_as_sf(points_to_map, coords=c("Longitude","Latitude"))
st_crs(points_to_map) = st_crs(granada_city) #Set the coordinate reference system
res = st_intersects(points_to_map,granada_city)
sel_logical = lengths(res) > 0
points_to_map$Ubication = ifelse(sel_logical,"Granada","Not Granada")
print(points_to_map)

### Add points to map
granada_prov = subset(level4_spain,level4_spain$NAME_2 == "Granada")

ggplot(data = granada_prov) +
  geom_sf()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())

ggplot(data = granada_prov) +
  geom_sf(fill="white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  geom_sf(data = points_to_map, size = 1, 
          shape = 16, aes(col = Ubication))

### Get coordinates from sf points
print(st_coordinates(points_to_map))
points_to_map = cbind(points_to_map,st_coordinates(points_to_map))

### Add text to map
ggplot(data = granada_prov) +
  geom_sf(fill="white")+
  geom_sf(data = points_to_map, size = 1, 
          shape = 16, aes(col = Ubication))+
  geom_text(data = points_to_map,aes(x=X,y=Y,label=Name),
            color = "darkblue", check_overlap = FALSE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  xlab("")+ylab("")


#### Example ####

browseURL("https://discomap.eea.europa.eu/map/fme/AirQualityExport.htm")

download.file("https://discomap.eea.europa.eu/map/fme/metadata/PanEuropean_metadata.csv","metadata.csv")
metadata = read.delim("metadata.csv")

metadata = unique(metadata[,c("AirQualityStationEoICode","Longitude","Latitude")])

#### Which station are located in Germany? Which station are located in Berlin?

#### Select the sf DataFrame ###
GADM36SF[GADM36SF$LEVEL_0 == "Germany",]
level1_germany = gadm_sf_loadCountries(fileNames = "DEU",level = 1,basefile = "./")$sf

print(level1_germany)

### First, all stations from Germany
metadata = st_as_sf(metadata, coords=c("Longitude","Latitude"))
st_crs(metadata) = st_crs(level1_germany) #Set the coordinate reference system

res = st_intersects(metadata,level1_germany)
sel_logical = lengths(res) > 0
germany_stations = metadata[sel_logical,]

print(head(germany_stations))

ggplot(data = level1_germany) +
  geom_sf(fill="white") +
  geom_sf(data = germany_stations, size = 1, 
          shape = 16, col = "darkred")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())


### Filter to select only stations from Berlin

berlin_state = subset(level1_germany,level1_germany$NAME_1 == "Berlin")

res = st_intersects(metadata,berlin_state)
sel_logical = lengths(res) > 0
berlin_stations = metadata[sel_logical,]

ggplot(data = level1_germany) +
  geom_sf(fill="white") +
  geom_sf(data = berlin_stations, size = 1, 
          shape = 16, col = "darkred")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())


#### Interactive Maps
germany_stations = cbind(germany_stations,st_coordinates(germany_stations))

m <- leaflet(level1_germany) %>% 
  addTiles() %>%
  addMarkers(lng=germany_stations$X, lat=germany_stations$Y)
m


berlin_stations = cbind(berlin_stations,st_coordinates(berlin_stations))

m <- leaflet(level1_germany) %>% 
  addTiles() %>%
  addMarkers(lng=berlin_stations$X, lat=berlin_stations$Y)
m
