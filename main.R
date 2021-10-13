

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
check.packages(c("GADMTools","ggrepel","raster","RColorBrewer","dplyr","gdata","glue","stringr","ggplot2","sf"))

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

level1_italy = gadm_sf_loadCountries("ITA", level=1, basefile="./")$sf
spain_and_france_and_italy = rbind(level1_france,level1_spain,level1_italy)
ggplot(data = spain_and_france_and_italy) +
  geom_sf(aes(fill=NAME_0))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())


#### Basic Operation: subset

andalusia = subset(level1_spain,level1_spain$NAME_1 == "Andalucía") # Subset Andalusia
ggplot(data = andalusia) +
  geom_sf(fill="white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())


andalusia = subset(level2_spain,level2_spain$NAME_1 == "Andalucía") # Subset Andalusia
ggplot(data = andalusia) +
  geom_sf(aes(fill=andalusia$NAME_2))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(),legend.position = "none")

#### Basic Operation: Intersect

points_to_map = data.frame(Name=c("Pico Veleta","Alhambra","Catedral","Aeropuerto"),
                           Longitude=c(-3.36,-3.58,-3.60,-3.78),
                           Latitude=c(37.05,37.17,37.18,37.19))

level4_spain = gadm_sf_loadCountries("ESP", level=4, basefile="./")$sf

ggplot(data = level4_spain) +
  geom_sf(fill="white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(),legend.position = "none")

granada_city = subset(level4_spain,level4_spain$NAME_4 == "Granada") # Extract the city of Granada

points_to_map = st_as_sf(points_to_map, coords=c("Longitude","Latitude"))
st_crs(points_to_map) = st_crs(granada_city) #Set the coordinate reference system
res = st_intersects(points_to_map,granada_city)
sel_logical = lengths(res) > 0
points_to_map$Ubication = ifelse(sel_logical,"Granada","Not Granada")
print(points_to_map)

### Add points to map
granada_prov = subset(level4_spain,level4_spain$NAME_2 == "Granada") #Extract municipalities from the province of Granada

ggplot(data = granada_prov) +
  geom_sf(fill="white")+
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
  geom_text_repel(data = points_to_map,aes(x=X,y=Y,label=Name),
            color = "darkblue")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  xlab("")+ylab("")


#### Example ####

browseURL("https://discomap.eea.europa.eu/map/fme/AirQualityExport.htm")

download.file("https://discomap.eea.europa.eu/map/fme/metadata/PanEuropean_metadata.csv","metadata.csv")
metadata = read.delim("metadata.csv")

metadata = unique(metadata[,c("AirQualityStationEoICode","Longitude","Latitude")])

head(metadata,20)

#### Which stations are located in Belgium?

#### Select the sf DataFrame ###
GADM36SF[GADM36SF$LEVEL_0 == "Belgium",]
level1_belgium = gadm_sf_loadCountries(fileNames = "BEL",level = 1,basefile = "./")$sf

print(level1_belgium)

### First, all stations from Belgium
metadata = st_as_sf(metadata, coords=c("Longitude","Latitude"))
st_crs(metadata) = st_crs(level1_belgium) #Set the coordinate reference system

res = st_intersects(metadata,level1_belgium)
sel_logical = lengths(res) > 0
belgium_stations = metadata[sel_logical,]

print(head(belgium_stations))

ggplot(data = level1_belgium) +
  geom_sf(fill="white") +
  geom_sf(data = belgium_stations, size = 1, 
          shape = 16, col = "darkred")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())


### Which stations are located in Bruxelles?

bruxelles_state = subset(level1_belgium,level1_belgium$NAME_1 == "Bruxelles")

res = st_intersects(metadata,bruxelles_state)
sel_logical = lengths(res) > 0
bruxelles_stations = metadata[sel_logical,]

ggplot(data = level1_belgium) +
  geom_sf(fill="white") +
  geom_sf(data = bruxelles_stations, size = 1, 
          shape = 16, col = "darkred")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())


#### Donwload Pollulant Data

requestFile = "https://fme.discomap.eea.europa.eu/fmedatastreaming/AirQualityDownload/AQData_Extract.fmw?CountryCode=BE&CityName=&Pollutant=10&Year_from=2021&Year_to=2021&Station=&Samplingpoint=&Source=All&Output=HTML&UpdateDate=&TimeCoverage=Year"
download.file(requestFile,"requestFile.html")

page <- readLines("requestFile.html")
content = page[!(page %in% c("</html>","<html>","</dl>","<dl>"))]
content = unlist(strsplit(content,'<dt><a href=\"'))
content = content[content != ""]
content = unlist(strsplit(content,'\" download=\"'))
requestFile = content[startsWith(content,"https:")]

bruxelles_climate_data = data.frame(Concentration=0,Date=0)[0,]
i = 1
for (i in 1:length(requestFile)){
  download.file(requestFile[i],basename(requestFile[i]))
  content = read.csv(file = requestFile[i])
  content = content[,c("AirQualityStationEoICode","Concentration","DatetimeBegin")]
  content$Date = str_split_fixed(content$DatetimeBegin, " ", 2)[,1]
  if (unique(content$AirQualityStationEoICode) %in% bruxelles_stations$AirQualityStationEoICode){
    bruxelles_climate_data = rbind(bruxelles_climate_data,content[,c("Concentration","Date")])
  }
  unlink(basename(requestFile[i]))
}

df = aggregate(x = bruxelles_climate_data$Concentration, by = list(bruxelles_climate_data$Date), FUN = "mean",na.rm = TRUE)
colnames(df) = c("Date","Concentration")
df$Date = as.Date(df$Date)

ggplot(df, aes(x=Date, y=Concentration)) +
  geom_line() + 
  xlab("")
