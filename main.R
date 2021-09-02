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
check.packages(c("GADMTools","raster","RColorBrewer","dplyr","gdata","glue","stringr","ggplot2","sf"))

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

# Examples
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


spain_and_france = rbind(level1_france,level1_spain)
ggplot(data = spain_and_france) +
  geom_sf(aes(fill=NAME_0))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())


### Do subset of the data ###
andalusia = subset(level1_spain,level1_spain$NAME_1 == "Andalucía") # Subset Andalusia
ggplot(data = andalusia) +
  geom_sf()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())


### Obtain different levels of coords ###
level4_spain = gadm_sf_loadCountries("ESP", level=4, basefile="./")$sf # download Municipality data
granada = subset(level4_spain,level4_spain$NAME_2 == "Granada") # Extract all municipalities from the province of Granada
ggplot(data = granada) +
  geom_sf()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())

granada = subset(level4_spain,level4_spain$NAME_4 %in% 
                   c("Granada","Huétor Vega","Armilla","Albolote","Maracena","Ogíjares","Atarfe")) #Extract a few the municipalities of Granada

ggplot(data = granada) +
  geom_sf()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())
  

granada <- cbind(granada, st_coordinates(st_centroid(granada$geometry)))
ggplot(data = granada) +
  geom_sf() +
  geom_text(aes(x=X, y=Y, label=NAME_4),
            color = "darkblue", check_overlap = FALSE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  xlab("")+ylab("")






### Air Quality Data ###
browseURL("https://discomap.eea.europa.eu/map/fme/AirQualityExport.htm")

download.file("https://discomap.eea.europa.eu/map/fme/metadata/PanEuropean_metadata.csv","metadata.csv")
metadata = read.delim("metadata.csv")

View(metadata)

level0_spain = gadm_sf_loadCountries("ESP", level=0, basefile="./")$sf
metadata = st_as_sf(metadata, coords=c("Longitude","Latitude"))
st_crs(metadata) = st_crs(level0_spain) #Set the coordinate reference system

res = st_intersects(metadata,level0_spain)
sel_logical = lengths(res) > 0
spain_stations = metadata[sel_logical,]

ggplot(data = level0_spain) +
  geom_sf() +
  geom_sf(data = spain_stations, size = 1, 
             shape = 16, col = "darkred")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())

### Filtering points from two spdf
level0_spain = gadm_sf_loadCountries("ESP", level=0, basefile="./")$sf
level0_france = gadm_sf_loadCountries("FRA", level=0, basefile="./")$sf

spain_and_france = rbind(level0_spain,level0_france)

ggplot(data = spain_and_france) +
  geom_sf() +
  geom_sf(data = spain_stations, size = 1, 
          shape = 16, col = "darkred")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())





### Mapping stations to specific region ###
level1_spain = gadm_sf_loadCountries("ESP", level=1, basefile="./")$sf
regions = level1_spain$NAME_1
region = regions[1]
sub_level1_spain = subset(level1_spain,level1_spain$NAME_1 == region)
st_crs(metadata) = st_crs(sub_level1_spain)
res = st_intersects(metadata,sub_level1_spain)
sel_logical = lengths(res) > 0
andalucia_stations = metadata[sel_logical,]

ggplot(data = level1_spain) +
  geom_sf() +
  geom_sf(data = andalucia_stations, size = 1, 
          shape = 16, col = "darkred")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())

ggplot(data = sub_level1_spain) +
  geom_sf() +
  geom_sf(data = andalucia_stations, size = 1, 
          shape = 16, col = "darkred")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())

### Define functions ###
map_region = function(region,country,metadata,sf){
  sub_sf = subset(sf,sf$NAME_1 == region)
  st_crs(metadata) = st_crs(sub_sf) #Projection is used to get or set the coordinate reference system
  res = st_intersects(metadata,sub_sf)
  sel_logical = lengths(res) > 0
  res = metadata[sel_logical,]
  
  
  if (nrow(res) > 0){
    region_stations = unique(res$AirQualityStationEoICode)
    region_stations = unique(metadata[metadata$AirQualityStationEoICode %in% region_stations,c("AirQualityStationEoICode"),drop=F])
    region_stations$Country = country
    region_stations$Region = region
  } else{
    region_stations = NULL
  }
  return(region_stations)
}


stations = data.frame(AirQualityStationEoICode="",Country="",Region="")[0,]

map = NULL

for (country in c("France","Italy","Spain")){
  sp_code = GADM36SF[GADM36SF$LEVEL_0 == country,"ID"]
  sf = gadm_sf_loadCountries(sp_code,level = 1,basefile="./")$sf
  map = rbind(map,sf)
  regions = sf$NAME_1
  res = lapply(regions,map_region,country,metadata,sf)
  region_stations = do.call("rbind", res)
  stations = rbind(stations,region_stations[,colnames(stations)])
}

View(stations)

ggplot(data = map) +
  geom_sf() +
  geom_sf(data = stations, size = 1, 
          shape = 16, col = "darkred")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())





### Download Data ###


pollutants = list("CO"=10,"NO2"=8,"PM10"=5,"SO2"=1,"O3"=7)
pollutant_df = data.frame(Country="",Region="",Pollutant="",Date="",Value="")[0,]

pollutant = 10

dir.create("out_files")

filename = "https://ereporting.blob.core.windows.net/downloadservice-last7days/ES/ES_10_12263_last7days.csv"
download_and_collect_pollullant = function(filename,country,stations,pollutant_df){
  basename_file = basename(filename)
  error = "error"
  while (error == "error"){
    error = tryCatch(download.file(filename,glue("out_files/{basename_file}")),
                     error = function(e) "error")
  }
  content_file = read.csv(glue("out_files/{basename_file}"))
  content_file = merge(content_file,stations ,by = "AirQualityStationEoICode")
  content_file = content_file[,c("Country","Region","AirPollutant","DatetimeBegin","Concentration")]
  colnames(content_file) = colnames(pollutant_df)
  content_file$Date = str_split_fixed(content_file$Date, " ", 2)[,1]
  unlink(glue("out_files/{basename_file}"))
  return(content_file)
}

country = "France"
for (country in c("France","Italy","Spain")){
  iso_code = unique(metadata[metadata$AirQualityStationEoICode %in% stations[stations$Country==country,]$AirQualityStationEoICode,]$Countrycode)
  error = "error"
  while (error == "error"){
    error = tryCatch(download.file(glue("https://fme.discomap.eea.europa.eu/fmedatastreaming/AirQualityDownload/AQData_Extract.fmw?CountryCode={iso_code}&CityName=&Pollutant={pollutant}&Year_from=2021&Year_to=2021&Station=&Samplingpoint=&Source=All&Output=HTML&UpdateDate=&TimeCoverage=Year"),"manifest.html"),
                     error = function(e) "error")
  }
  page <- readLines("manifest.html")
  content = page[!(page %in% c("</html>","<html>","</dl>","<dl>"))]
  content = unlist(strsplit(content,'<dt><a href=\"'))
  content = content[content != ""]
  content = unlist(strsplit(content,'\" download=\"'))
  content = content[startsWith(content,"https:")]
  res = lapply(content,download_and_collect_pollullant,country,stations,pollutant_df)
  country_content = do.call("rbind",res)
  
  df = aggregate(x = country_content$Value, by = list(country_content$Region,country_content$Date), FUN = "mean",na.rm = TRUE)
  colnames(df) = c("Region","Date","Value")
  df$Pollutant = unique(country_content$Pollutant)
  df$Country = country
  df = df[,colnames(pollutant_df)]
  
  sp_code = GADM36SF[GADM36SF$LEVEL_0 == country,"ID"]
  sf = gadm_sf_loadCountries(sp_code,level = 1,basefile="./")$sf
  regions = sf$NAME_1
  
  for (region in regions){
    if (!(region %in% df$Region)){
      df = rbind(df,data.frame(Country=country,Region=region,Pollutant=NA,Date=unique(df$Date),Value=NA))
    }
  }
  
  pollutant_df = rbind(pollutant_df,df)
}


dateDay = "2021-02-03"

pollutant_df_day = pollutant_df[pollutant_df$Date == dateDay,]

level1_spain = gadm_sf_loadCountries("ESP", level=1, basefile="./")$sf
level1_france = gadm_sf_loadCountries("FRA", level=1, basefile="./")$sf
level1_italy = gadm_sf_loadCountries("ITA", level=1, basefile="./")$sf

map = rbind(level1_spain,level1_france,level1_italy) #rbind function is used to join sf objects

map <- merge(map, pollutant_df_day,by.x="NAME_1",by.y="Region",sort = F)

ggplot(data = map) +
  geom_sf(aes(fill=Value))+
  scale_fill_gradient(low="yellow", high="red")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())
