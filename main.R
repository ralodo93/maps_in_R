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
check.packages(c("GADMTools","raster","RColorBrewer","dplyr","leaflet","gdata","glue","stringr"))

##### GADM and GADMTools #####

browseURL("https://gadm.org/") # Open GADM website where are located geographical data for the entire world

?GADMTools #Instead of downloading the data by hand, we will use the library named GADMTools


### Basic operations ###
level1_spain = gadm_sp_loadCountries(fileNames = "ESP", level=1, basefile="./")$spdf # gadm_sp_loadCountries download the information from a country and saves in a RDS

#spdf is a OBJECT in R
print(level1_spain)

#spdf can be plotted
plot(level1_spain)



### Which data?
data("GADM36SF") # With this dataframe we can check the country codes and the subregions
View(GADM36SF)

# Examples
print(GADM36SF[GADM36SF$LEVEL_0 == "Spain",])  # ID is the code, Level 0 is the entire country, Level 1 are Autonomous Communities...
print(GADM36SF[GADM36SF$LEVEL_0 == "France",]) # ID is the code, Level 0 is the entire country, Level 1 are Region...


level1_france = gadm_sp_loadCountries("FRA", level=1, basefile="./")$spdf # Download States coordinates from USA
level1_spain = gadm_sp_loadCountries("ESP", level=1, basefile="./")$spdf # Download Autonomous Communities coordinates from Spain

### Plot the coords ###
plot(level1_france) # Plot France
plot(level1_spain) # Plot Spain


### Do subset of the data ###
andalusia = subset(level1_spain,level1_spain$NAME_1 == "Andalucía") # Subset Andalusia
plot(andalusia) # Plot Andalusia


### Obtain different levels of coords ###
level4_spain = gadm_sp_loadCountries("ESP", level=4, basefile="./")$spdf # download Municipality data
granada = subset(level4_spain,level4_spain$NAME_2 == "Granada") # Extract all municipalities from the province of Granada
plot(granada)

granada = subset(level4_spain,level4_spain$NAME_4 %in% 
                   c("Granada","Huétor Vega","Armilla","Albolote","Maracena","Ogíjares","Atarfe")) #Extract a few the municipalities of Granada
plot(granada)





### Joining Objects ###
GADM36SF[GADM36SF$LEVEL_0 == "Portugal",]
level0_portugal = gadm_sp_loadCountries("PRT", level=0, basefile="./")$spdf # Download full country coords from Portugal

GADM36SF[GADM36SF$LEVEL_0 == "France",]
level2_france = gadm_sp_loadCountries("FRA", level=2, basefile="./")$spdf # Download Departments from France

GADM36SF[GADM36SF$LEVEL_0 == "Italy",]
level1_italy = gadm_sp_loadCountries("ITA", level=1, basefile="./")$spdf # Download Regions from Italy

map = bind(level1_spain,level0_portugal,level2_france,level1_italy) #bind function is used to join spdf objects

map@data$Country = as.factor(map@data$NAME_0) #Convert countries in factors

palette(c("grey","red","green3","blue"))

dev.off()
pdf("south_europe.pdf",width = 10,height = 10)
plot(map,col=map@data$Country)
dev.off()

system2('open', args = c('-a Preview.app', 'south_europe.pdf'), wait = FALSE)



### Air Quality Data ###
browseURL("https://discomap.eea.europa.eu/map/fme/AirQualityExport.htm")

download.file("https://discomap.eea.europa.eu/map/fme/metadata/PanEuropean_metadata.csv","metadata.csv")
metadata = read.delim("metadata.csv")

View(metadata)

level1_spain = gadm_sp_loadCountries("ESP", level=1, basefile="./")$spdf
plot(level1_spain)
spain_stations = metadata
coordinates(spain_stations) = ~ Longitude + Latitude #coordinates function transform dataframe to spdf using the columns Longitude and Latitude
projection(spain_stations) = projection(level1_spain) #Projection function is used to get or set the coordinate reference system

### Filtering points from two spdf
res = over(level1_spain,spain_stations,returnList = TRUE) #over function is used to detect which spatialpoints (spain_stations) are located inside the spatialpolygons (level0_spain)
res = do.call("rbind",res)
spain_stations = unique(res$AirQualityStationEoICode)
spain_stations = unique(metadata[metadata$AirQualityStationEoICode %in% spain_stations,c("Longitude","Latitude","AirQualityStationEoICode")])
points(spain_stations,col=4,pch=16)



### Mapping stations to specific region ###
level1_spain = gadm_sp_loadCountries("ESP", level=1, basefile="./")$spdf
regions = level1_spain@data$NAME_1
region = regions[1]
sub_level1_spain = subset(level1_spain,level1_spain$NAME_1 == region)
region_stations = metadata
coordinates(region_stations) = ~ Longitude + Latitude #coordinates function transform dataframe to spdf using the columns Longitude and Latitude
projection(region_stations) = projection(sub_level1_spain) #Projection is used to get or set the coordinate reference system
res = over(sub_level1_spain,region_stations,returnList = TRUE)[[1]] #over function is used to detect which spatialpoints (spain_stations) are located inside the spatialpolygons (level0_spain)
region_stations = unique(res$AirQualityStationEoICode)
region_stations = unique(metadata[metadata$AirQualityStationEoICode %in% region_stations,c("Longitude","Latitude","AirQualityStationEoICode")])
plot(level1_spain)
points(region_stations,col=4,pch=16)
plot(sub_level1_spain)
points(region_stations,col=4,pch=16)


### Define functions ###
map_region = function(region,country,metadata,spdf){
  sub_spdf = subset(spdf,spdf$NAME_1 == region)
  region_stations = metadata
  coordinates(region_stations) = ~ Longitude + Latitude #coordinates function transform dataframe to spdf using the columns Longitude and Latitude
  projection(region_stations) = projection(sub_spdf) #Projection is used to get or set the coordinate reference system
  res = over(sub_spdf,region_stations,returnList = TRUE)[[1]] #over function is used to detect which spatialpoints (spain_stations) are located inside the spatialpolygons (level0_spain)
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
for (country in c("Portugal","France","Italy","Spain")){
  sp_code = GADM36SF[GADM36SF$LEVEL_0 == country,"ID"]
  spdf = gadm_sp_loadCountries(sp_code,level = 1,basefile="./")$spdf
  regions = spdf@data$NAME_1
  res = lapply(regions,map_region,country,metadata,spdf)
  region_stations = do.call("rbind", res)
  stations = rbind(stations,region_stations[,colnames(stations)])
}

View(stations)


### Download Data ###


pollutants = list("CO"=10,"NO2"=8,"PM10"=5,"SO2"=1,"O3"=7)
full_content = data.frame(Country="",Region="",Pollutant="",Date="",Value="")[0,]

pollutant = 10

dir.create("out_files")

filename = "https://ereporting.blob.core.windows.net/downloadservice-last7days/ES/ES_10_12263_last7days.csv"
download_and_collect_pollullant = function(filename,country,stations,full_content){
  basename_file = basename(filename)
  error = "error"
  while (error == "error"){
    error = tryCatch(download.file(filename,glue("out_files/{basename_file}")),
                     error = function(e) "error")
  }
  content_file = read.csv(glue("out_files/{basename_file}"))
  content_file = merge(content_file,stations ,by = "AirQualityStationEoICode")
  content_file = content_file[,c("Country","Region","AirPollutant","DatetimeBegin","Concentration")]
  colnames(content_file) = colnames(full_content)
  content_file$Date = str_split_fixed(content_file$Date, " ", 2)[,1]
  unlink(glue("out_files/{basename_file}"))
  return(content_file)
}

country = "France"
for (country in c("Portugal","France","Italy","Spain")){
  iso_code = unique(metadata[metadata$AirQualityStationEoICode %in% stations[stations$Country==country,"AirQualityStationEoICode"],"Countrycode"])
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
  res = lapply(content,download_and_collect_pollullant,country,stations,full_content)
  country_content = do.call("rbind",res)
  
  df = aggregate(x = country_content$Value, by = list(country_content$Region,country_content$Date), FUN = "mean",na.rm = TRUE)
  colnames(df) = c("Region","Date","Value")
  df$Pollutant = unique(country_content$Pollutant)
  df$Country = country
  df = df[,colnames(full_content)]
  
  sp_code = GADM36SF[GADM36SF$LEVEL_0 == country,"ID"]
  spdf = gadm_sp_loadCountries(sp_code,level = 1,basefile="./")$spdf
  regions = spdf@data$NAME_1
  
  for (region in regions){
    if (!(region %in% df$Region)){
      df = rbind(df,data.frame(Country=country,Region=region,Pollutant=NA,Date=unique(df$Date),Value=NA))
    }
  }
  
  full_content = rbind(full_content,df)
}

table(full_content$Date)

dateDay = "2021-02-03"

full_content_day = full_content[full_content$Date == dateDay,]


level1_portugal = gadm_sp_loadCountries("PRT", level=1, basefile="./")$spdf
level1_spain = gadm_sp_loadCountries("ESP", level=1, basefile="./")$spdf
level1_france = gadm_sp_loadCountries("FRA", level=1, basefile="./")$spdf
level1_italy = gadm_sp_loadCountries("ITA", level=1, basefile="./")$spdf

map = bind(level1_spain,level1_portugal,level1_france,level1_italy) #bind function is used to join spdf objects

map@data <- merge(map@data, full_content_day,by.x="NAME_1",by.y="Region",sort = F)

palette_custom = colorRampPalette(c("darkblue", "cyan","green", "orange","firebrick1"))(30)

dev.off()
pdf("south_europe_CO_concentration.pdf",width = 10,height = 10)
spplot(map, "Value", main = "CO concentration",col.regions=palette_custom)
dev.off()

system2('open', args = c('-a Preview.app', 'south_europe_CO_concentration.pdf'), wait = FALSE)
