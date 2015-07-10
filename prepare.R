broadband<-read.delim('data/broadbandlist.txt',sep='\t',
                      fileEncoding='utf-8', encoding='utf-8')
broadband<-subset(broadband, PRICE!=0)
broadband$SPEED.number<-as.numeric(gsub("M","",as.character(broadband$SPEED)))
broadband<-transform(broadband, SPEED_PRICE=1024 * SPEED.number/PRICE)
city.lat.long<-read.table('data/city_lat_long.txt',header=T)
colnames(city.lat.long)[1]<-'CITY.pinyin'
broadband.lat.long<-merge(unique(broadband[c("CITY","CITY.pinyin")]),
                          city.lat.long, by='CITY.pinyin')

chinageojson <- readLines("data/geoJson/city.list.json", warn = FALSE, encoding='UTF-8') %>%
  paste(collapse = "\n") %>%
  fromJSON(simplifyVector = FALSE)

getCityGeoJSON<-function(city_name){
  geoJsonFile<-paste("data/geoJson/china-main-city/",chinageojson[[city_name]],'.json',sep='')
  readLines(geoJsonFile, warn = FALSE, encoding='UTF-8') %>%
    paste(collapse = "\n") %>%
    fromJSON(simplifyVector = FALSE)
}

city_names<-unique(as.character(broadband$CITY))[1:3]

mapCityStyle<-function(geocity, city_name, ...){
  StyleOptions = list(...)
  setGeoCityFeature<-function(feat, StyleOptions){
    feat$properties$style<-StyleOptions
    feat$properties$parent<-city_name
    feat
  }
  geocity$features<-lapply(geocity$features, setGeoCityFeature, StyleOptions)
  geocity
}

geocities<-lapply(city_names, getCityGeoJSON)
names(geocities)<-city_names

is.NULL<-function(x){
  if(is.null(x)){
    TRUE
  }else if(x == 'NULL'){
    TRUE
  }else{
    FALSE
  }
}

tooltip_helper <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(names(x), ": ", format(x), collapse = "<br />")
}
