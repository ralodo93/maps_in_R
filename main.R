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
check.packages(c("GADMTools","raster","RColorBrewer","ncdf4","dplyr","leaflet"))

##### GADM and GADMTools #####

browseURL("https://gadm.org/") # Open GADM website where are located geographical data for the entire world

?GADMTools #Instead of downloading handly the data, we will use the library named GADMTools


### Basic operations ###
level1_spain = gadm_sp_loadCountries("ESP", level=1, basefile="./")$spdf # gadm_sp_loadCountries download the information from a country and saves in a RDS

#spdf is a OBJECT in R
print(level1_spain)

data("GADM36SF") # With this dataframe we can check the country codes and the subregions
View(GADM36SF)

# Examples
GADM36SF[GADM36SF$LEVEL_0 == "Spain",]  # ID is the code, Level 0 is the entire country, Level 1 are Autonomous Communities...
GADM36SF[GADM36SF$LEVEL_0 == "United States",] # ID is the code, Level 0 is the entire country, Level 1 are States...

level1_usa = gadm_sp_loadCountries("USA", level=1, basefile="./")$spdf # Download States coordinates from USA

level1_spain = gadm_sp_loadCountries("ESP", level=1, basefile="./")$spdf # Download Autonomous Communities coordinates from Spain

plot(level1_usa) # Plot Spain

plot(level1_spain) # Plot Spain
text(getSpPPolygonsLabptSlots(level1_spain), # Get the centre point of each polygons
     labels=as.character(level1_spain$NAME_1), 
     cex=0.5, col="black")

andalusia = subset(level1_spain,level1_spain$NAME_1 == "Andalucía") # Subset Andalusia
plot(andalusia) # Plot Andalusia


cities <- data.frame(longitude = c(-3.35,-5.59), # Adding some points in the map
                     latitude = c(37.10,37.23),
                     monument = c("Granada","Sevilla"))
points(cities,col=2) # Plot the points added


level4_spain = gadm_sp_loadCountries("ESP", level=4, basefile="./")$spdf # download Municipality data
plot(level4_spain) # Plot Municipalities

andalusia = subset(level4_spain,level4_spain$NAME_1 == "Andalucía") # Extract all municipalities from Andalusia
plot(andalusia)

granada = subset(level4_spain,level4_spain$NAME_2 == "Granada") # Extract all municipalities from the province of Granada
plot(granada)

granada = subset(level4_spain,level4_spain$NAME_4 %in% c("Granada","Huétor Vega","Armilla","Albolote","Maracena","Ogíjares","Atarfe")) #Extract a few the municipalities of Granada
plot(granada)
text(getSpPPolygonsLabptSlots(granada), 
     labels=as.character(granada$NAME_4), 
     cex=0.5, col="black")


### Joining Objects ###
GADM36SF[GADM36SF$LEVEL_0 == "Portugal",]
level0_portugal = gadm_sp_loadCountries("PRT", level=0, basefile="./")$spdf # Download full country coords from Portugal

GADM36SF[GADM36SF$LEVEL_0 == "France",]
level2_france = gadm_sp_loadCountries("FRA", level=2, basefile="./")$spdf # Download Departments from France

map = bind(level1_spain,level0_portugal,level2_france) #bind function is used to join spdf objects

map@data$Regions = as.factor(map@data$NAME_0) #Convert countries in factors
spplot(map, c("Regions"), col.regions=brewer.pal(3, "Set2"),par.settings = list(axis.line = list(col = "transparent"))) #Plot each country by a different color



### Climatic Data ###
browseURL("https://cds.climate.copernicus.eu/#!/home")

# system("python3 api_cds.py") # To use this pipeline you will need to be register in  cds.climate.copernicus and follow the next instructions

browseURL("https://cds.climate.copernicus.eu/api-how-to")


filename = "2021-01-01.nc"

# ncdf4 library
date = gsub(".nc","",filename)

ncin <- nc_open(filename) # Open the file
variables = names(ncin$var) #Get variables (temperature, precipitations and surface presure)

fullDay = data.frame(Combs=0,lon=0,lat=0,variable=0,value=0) # Prepare a dataframe to collect the values for each variable
fullDay=fullDay[0,]

# Get some interesting variables
time <- ncvar_get(ncin,"time")
nt <- dim(time)
lon <- ncvar_get(ncin,"longitude")
nlon <- dim(lon)
lat <- ncvar_get(ncin,"latitude")
nlat <- dim(lat)

var = variables[2]
for (var in variables){ #For each variable
  tmp_array <- ncvar_get(ncin,var) # Get the array
  dlname <- ncatt_get(ncin,var,"long_name")$value
  dunits <- ncatt_get(ncin,var,"units")
  fillvalue <- ncatt_get(ncin,var,"_FillValue")
  tmp_array[tmp_array==fillvalue$value] <- NA
  tmp_vec_long <- as.vector(tmp_array)
  tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)
  lonlat <- as.matrix(expand.grid(lon,lat))
  tmp_df02 <- na.omit(data.frame(cbind(lonlat,tmp_mat)))
  names(tmp_df02) <- c("longitude","latitude",as.character(seq(0,23)))
  
  
  tmp_df02$Combs = paste0(round(tmp_df02$lat,1),"_",round(tmp_df02$lon,1))
  tmp_df02$value <- apply(tmp_df02[3:26],1,mean)
  tmp_df02 = as.data.frame(tmp_df02)
  tmp_df02$variable = dlname
  tmp_df02 = tmp_df02[,c("Combs","longitude","latitude","variable","value")]
  
  if (var %in% c("t2m")){
    tmp_df02$value = tmp_df02$value - 273.15
  }
  fullDay = rbind(fullDay,tmp_df02)
  
}

nc_close(ncin)


fullDay$Date = date
variables = unique(fullDay$variable)

for (var in variables){
  subData = fullDay[fullDay$variable == var,]
  print(head(subData))
}


### Filtering coords ###

combs = unique(fullDay[,c("Combs","longitude","latitude")])

combsdf = combs
combsdf$Region = ""
combsdf$Country = ""


level1_spain = gadm_sp_loadCountries("ESP", level=1, basefile="./")$spdf

ccaas = level1_spain$NAME_1

ccaa = ccaas[1]
print(ccaa)

spdf_reg = subset(level1_spain,level1_spain$NAME_1 == ccaa)

combscoord = combs
coordinates(combscoord) = ~ longitude + latitude
projection(combscoord) = projection(spdf_reg)

res = over(spdf_reg,combscoord,returnList = TRUE) # Check which coords from cds are located in the Andalusia area
res = res[[1]]$Combs
print(res) #Combs that are included in Andalusia area

toyex = fullDay[fullDay$variable == "Leaf area index, high vegetation" & fullDay$Combs %in% res,c("longitude","latitude","value")]
coordinates(toyex) = ~ longitude + latitude
projection(toyex) = projection(spdf_reg)
spplot(toyex, "value") # Plot the value of each coord


spdf = gadm_sp_loadCountries("ESP", level=1, basefile="./")$spdf
regions = spdf@data$NAME_1
remove_regs = c()
## Repeat the same with all regions in Spain
for (region in regions){
  spdf_reg = subset(spdf,spdf$NAME_1 == region)
  combscoord = combs
  coordinates(combscoord) = ~ longitude + latitude
  projection(combscoord) = projection(spdf_reg)
  res = over(spdf_reg,combscoord,returnList = TRUE)
  res = res[[1]]$Combs
  combsdf[combsdf$Combs %in% res,"Region"] = region
  combsdf[combsdf$Combs %in% res,"Country"] = "Spain"
  if (length(res) == 0){ #In case there are no climatic points in a region, we delete it (Ceuta and Melilla)
    remove_regs = c(remove_regs,region)
  }
}

combsdf = combsdf[combsdf$Region != "",]

print(sample_n(combsdf, 10))

fullDay_merge = merge(fullDay[,c("Combs","variable","value","Date")],combsdf,by="Combs") #Merge fullDay and combsdf

palette_custom = colorRampPalette(c("darkblue", "cyan","green", "orange","firebrick1"))(30)

toShow = fullDay_merge[fullDay_merge$variable == "2 metre temperature" & fullDay_merge$Region == "País Vasco",] # Plot points from País Vasco
coordinates(toShow) = ~ longitude + latitude
projection(toShow) = projection(spdf)
spplot(toShow, "value", main = "2 metre temperature", 
       col.regions = palette_custom)


toShow = fullDay_merge[fullDay_merge$variable == "2 metre temperature" & fullDay_merge$Country == "Spain",] # Plot points from the entire country
coordinates(toShow) = ~ longitude + latitude
projection(toShow) = projection(spdf)

spplot(toShow, "value", main = "2 metre temperature", 
       col.regions = palette_custom)

spdf = subset(spdf,!(spdf$NAME_1 %in% remove_regs))

# Instead of plotting each point, now we calculate the mean value by region
toShow = fullDay_merge[fullDay_merge$variable == "2 metre temperature",]
coordinates(toShow) = ~ longitude + latitude
projection(toShow) = projection(spdf)

meanVals = aggregate(value ~ Region+variable,data=fullDay_merge,mean) #Calculate the mean values by variable and region
meanTemp = meanVals[meanVals$variable == "2 metre temperature",]
rownames(meanTemp) = meanTemp$Region
meanTemp = meanTemp[spdf@data$NAME_1,]

map2 = spdf
map2@data <- merge(map2@data, meanTemp,by.x="NAME_1",by.y="Region",sort = F) # Merge mean values
spplot(map2, "value", main = "Mean Temperature",col.regions=palette_custom)

meanVeg = meanVals[meanVals$variable == "Leaf area index, high vegetation",]
rownames(meanVeg) = meanVeg$Region
meanVeg = meanVeg[spdf@data$NAME_1,]

map2 = spdf
map2@data <- merge(map2@data, meanVeg,by.x="NAME_1",by.y="Region",sort = F) # Merge mean values
spplot(map2, "value", main = "Mean High Vegetation Index",col.regions=palette_custom)

########## Interactive Maps ##########

map2 = spdf
map2@data <- merge(map2@data, meanTemp,by.x="NAME_1",by.y="Region",sort = F)

pal <- colorNumeric(
  palette = "Blues",
  domain = map2$value)

map = leaflet(map2)
map %>% addTiles() %>% addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
                                   color = ~pal(value),
                                   label=~stringr::str_c(
                                     NAME_1, ' ---> ',
                                     round(value,digits = 3)))




############
spdf_es = gadm_sp_loadCountries("ESP", level=1, basefile="./")$spdf
spdf_fra = gadm_sp_loadCountries("FRA", level=1, basefile="./")$spdf
spdf_por = gadm_sp_loadCountries("PRT", level=1, basefile="./")$spdf  

map = bind(spdf_es,spdf_fra,spdf_por)

combs = unique(fullDay[,c("Combs","longitude","latitude")])

combsdf = combs
combsdf$Region = ""
combsdf$Country = ""

for (code in c("FRA","PRT","ESP")){
  spdf = gadm_sp_loadCountries(code, level=1, basefile="./")$spdf
  regions = spdf@data$NAME_1
  remove_regs = c()
  ## Repeat the same with all regions in Spain
  for (region in regions){
    spdf_reg = subset(spdf,spdf$NAME_1 == region)
    combscoord = combs
    coordinates(combscoord) = ~ longitude + latitude
    projection(combscoord) = projection(spdf_reg)
    res = over(spdf_reg,combscoord,returnList = TRUE)
    res = res[[1]]$Combs
    combsdf[combsdf$Combs %in% res,"Region"] = region
    combsdf[combsdf$Combs %in% res,"Country"] = code
    if (length(res) == 0){ #In case there are no climatic points in a region, we delete it (Ceuta and Melilla)
      remove_regs = c(remove_regs,region)
    }
  }
}

combsdf = combsdf[combsdf$Region != "",]

fullDay_merge = merge(fullDay[,c("Combs","variable","value","Date")],combsdf,by="Combs") #Merge fullDay and combsdf
print(head(fullDay_merge))


spdf = subset(map,!(map$NAME_1 %in% remove_regs))

meanVals = aggregate(value ~ Region+variable,data=fullDay_merge,mean) #Calculate the mean values by variable and region
meanTemp = meanVals[meanVals$variable == "2 metre temperature",]
rownames(meanTemp) = meanTemp$Region
meanTemp = meanTemp[spdf@data$NAME_1,]

map2 = spdf
map2@data <- merge(map2@data, meanTemp,by.x="NAME_1",by.y="Region",sort = F) # Merge mean values
spplot(map2, "value", main = "Mean Temperature",col.regions=palette_custom)

## Interactive
map2 = spdf
map2@data <- merge(map2@data, meanTemp,by.x="NAME_1",by.y="Region",sort = F)

pal <- colorNumeric(
  palette = "Blues",
  domain = map2$value)

map = leaflet(map2)
map %>% addTiles() %>% addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
                                   color = ~pal(value),
                                   label=~stringr::str_c(
                                     NAME_1, ' ---> ',
                                     round(value,digits = 3)))
