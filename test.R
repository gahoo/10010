library(XML)
geoencode <- function(name) {
  requestUrl<-paste(
    "http://maps.googleapis.com/maps/api/geocode/xml?address=",
    name,"&sensor=false", sep="")
  xmlResult<-xmlTreeParse(requestUrl,isURL=TRUE)
  root <- xmlRoot(xmlResult)
  latitude <-xmlValue(root[['result']][['geometry']][['location']][['lat']])
  longitude <-xmlValue(root[['result']][['geometry']][['location']][['lng']])
  return(data.frame(name=name, latitude=latitude,longitude=longitude))
}

geoencode('china')


library(ggplot2)
broadband<-read.delim('data/broadbandlist.txt',sep='\t',
                      fileEncoding='utf-8', encoding='utf-8')
broadband$SPEED.number<-as.numeric(gsub("M","",as.character(broadband$SPEED)))
broadband<-transform(broadband, SPEED_PRICE=1024 * SPEED.number/PRICE)

city.lat.long<-do.call(rbind, lapply(unique(broadband$CITY.pinyin), geoencode))
write.table(city.lat.long, 'data/city_lat_long.txt', row.names=F, sep='\t', quote = F)
city.lat.long<-read.table('data/city_lat_long.txt',header=T)
colnames(city.lat.long)[1]<-'CITY.pinyin'
broadband.lat.long<-merge(unique(broadband[c("CITY","CITY.pinyin")]), city.lat.long, by='CITY.pinyin')

getCityLatLong<-function(city_name){
  as.list(unique(subset(broadband.lat.long, CITY==city_name, select=c('latitude', 'longitude'))))
}

getCityLatLong('南宁')


pdf('city.province.pdf', height=50, family='GB1')
ggplot(broadband, 
       aes(x=PRICE, y=SPEED.number, size=TARIFF_TYPE.number, color=PROVINCE))+
  geom_point()+
  facet_grid(CITY ~ PROVINCE)
dev.off()

ggplot(broadband, 
       aes(y=CITY, x=SPEED_PRICE, color=SPEED, size=SPEED.number, alpha=PRICE))+
  geom_point() +
  #geom_bar(stat='identity')+
  facet_grid(PROVINCE ~ ., space='free', scales='free')


aes(y=CITY, x=SPEED_PRICE, color=SPEED, size=SPEED.number, alpha=PRICE))

all_values <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(names(x), ": ", format(x), collapse = "<br />")
}

broadband %>%
  ggvis(x = ~SPEED_PRICE, y = ~CITY,
        size = ~SPEED.number,
        fill = ~SPEED,
        stroke = ~PRODUCT_NAME,
        fillOpacity = ~PRICE) %>%
  layer_points() %>%
  add_tooltip(all_values, "hover")
  add_legend(scales = "size", properties = legend_props(legend = list(y = 200)))
  #add_legend(scales = "fillOpacity", properties = legend_props(legend = list(y = 200)))
  

pdf('city.pdf', height=10,width=70,  family='GB1')
ggplot(broadband, 
       aes(x=PRODUCT_NAME, y=SPEED_PRICE, fill=SPEED, alpha=PRICE))+
  #geom_point() +
  geom_bar(stat='identity', position='stack')+
  #coord_flip()+
  facet_wrap(~CITY, scales="free_x", nrow=1)+
  theme(axis.text.x = element_text(angle = 90))
dev.off()

library(jsonlite)
library(leaflet)
chinageojson <- readLines("data/geoJson/city.list.json", warn = FALSE, encoding='UTF-8') %>%
  paste(collapse = "\n") %>%
  fromJSON(simplifyVector = FALSE)


getCityGeoJSON<-function(city_name){
  geoJsonFile<-paste("data/geoJson/china-main-city/",chinageojson[[city_name]],'.json',sep='')
  readLines(geoJsonFile, warn = FALSE, encoding='UTF-8') %>%
    paste(collapse = "\n") %>%
    fromJSON(simplifyVector = FALSE)
}


city_names<-unique(as.character(broadband$CITY))
geocities<-lapply(city_names[1:3], getCityGeoJSON)
library(scales)

mapColors<-function(values, low='white', high='red'){
  colorPal<-seq_gradient_pal(low, high)
  inf_idx<-is.infinite(values)
  max_number<-max(values[!inf_idx])
  values[inf_idx]<-max_number
  colorPal(values/max_number)
}

fillColors<-as.list(mapColors(broadband$SPEED_PRICE))
names(fillColors)<-broadband$CITY

mapCityStyle<-function(geocity, ...){
  StyleOptions = list(...)
  setGeoCityFeature<-function(feat, StyleOptions){
    feat$properties$style<-StyleOptions
    feat
  }
  geocity$features<-lapply(geocity$features, setGeoCityFeature, StyleOptions)
  geocity
}

geocities2<-lapply(city_names, function(city_name){
  geocity<-getCityGeoJSON(city_name)
  geocity<-mapCityStyle(geocity=geocity,
               fillColor=fillColors[[city_name]],
               weight = 1, color = fillColors[[city_name]],
               opacity = 1, fillOpacity = 0.8)
  geocity
})

geocity$style = list(
  weight = 1,
  fillColor = "#000000",
  color = "#555555",
  opacity = 1,
  fillOpacity = 0.8
)
city_latlong<-getCityLatLong('深圳')
leaflet() %>%
  setView(lng=104, lat=36, zoom=4) %>%
  addTiles() %>% addGeoJSON(geocities2)
