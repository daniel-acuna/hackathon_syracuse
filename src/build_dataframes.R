

syracuse_map <- get_map(unlist(geocode("Syracuse University, Syracuse, NY")), zoom = 15)
city_map <- get_map(unlist(geocode("Syracuse University, Syracuse, NY")), zoom = 13)
shapeData <- readOGR("../data/street_shapefile/",'streets')


shp <- spTransform(shapeData, CRS("+proj=longlat +ellps=GRS80"))
shape_data <- shp@data

all_ratings = NULL
for (year in seq(from=2000, to=2015)) {
  ratings <- read.csv(paste0("../data/RoadRatings", year, '.csv'), 
                      stringsAsFactors = FALSE)
  ratings$ratings_year <- year
  if (is.null(df)) {
    all_ratings = ratings
  } else {
    all_ratings <- rbind(all_ratings, ratings)  
  }
}

all_ratings$overall <- as.numeric(all_ratings$overall)
all_ratings <- subset(all_ratings, !is.na(overall))

all_data <- merge(shape_data, 
                  all_ratings,
                  by.x = "STREET_ID", 
                  by.y = "streetID")


all_data$dateLastOverlay <- as.numeric(all_data$dateLastOverlay)
all_data$dateLastOverlay[is.na(all_data$dateLastOverlay)] <- 1985
all_data$pavement <- as.numeric(all_data$pavement)
all_data$length <- as.numeric(all_data$length)
all_data$overall[all_data$overall>10] <- 10
all_data$crack <- as.numeric(all_data$crack)
all_data$pavement <- as.numeric(all_data$pavement)
all_data$patch <- as.numeric(all_data$patch)
all_data$class <- factor(all_data$class)
all_data$yearRated <- as.numeric(str_sub(all_data$dateRated, start = -2, end = -1))
all_data$yearRated <- ifelse(all_data$yearRated <= 30, all_data$yearRated + 2000, all_data$yearRated + 1900)

potholes <- read.csv('../data/potholes.csv')
potholes$dtTime <- mdy_hm(potholes$dtTime, tz = "EST")

build_df_year <- function(year) {
  shp@data <- merge(shape_data, 
                    subset(all_ratings, ratings_year == year), 
                    by.x = "STREET_ID", 
                    by.y = "streetID")
  
  
  data_fort <- fortify(shp)
  
  
  shp.df <- data.frame(id=rownames(shp@data),
                       shp@data, stringsAsFactors=F)
  
  data_merged <- join(data_fort, shp.df, by='id')
  data_merged <- subset(data_merged, !is.na(overall))
  
  data_merged$dateLastOverlay <- as.numeric(data_merged$dateLastOverlay)
  data_merged$dateLastOverlay[is.na(data_merged$dateLastOverlay)] <- 1985
  data_merged$pavement <- as.numeric(data_merged$pavement)
  data_merged$length <- as.numeric(data_merged$length)
  data_merged
}