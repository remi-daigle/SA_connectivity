require(leaflet)
require(sf)
require(tidync)
require(tidyverse)

simulations <- dir("data/simulations/",
                         pattern = ".*run.*")
list_edgelists <- list()

# load pu
pu <- st_read("data/pulayer_v1b2_blank_dd.shp") %>% 
  st_transform(crs=4326)

for(s in simulations){
  file <- file.path("data","simulations",s,"trajectories.nc")
  nc <- tidync(file)
  ncdata <- nc %>% 
    hyper_tibble(force = TRUE) %>% 
    filter(age_seconds %in% c(606600,1816200,3024000)) %>% # use only coordinates from 7, 21, and 35 d
    mutate(PLD = round(age_seconds/86400), #convert to d
           time=as.POSIXct(time, origin = "1970-01-01 00:00:00", tz="GMT")) %>% # stole this from Linda!
    select(lon, lat, PLD, time, trajectory) %>% #remove columns we won't use
    st_as_sf(coords = c("lon","lat"),crs=4326) #convert to spatial sf object, CRS is a guess?
  
  
  edgelist <- st_intersects(pu,ncdata) %>% 
    data.frame() %>% 
    left_join(rowid_to_column(ncdata,var="row.id"),
              by = "row.id") %>% 
    mutate(puid1=row.id,
           puid2=ncdata$trajectory[col.id]) %>% 
    group_by(puid1,puid2,PLD) %>% 
    summarize(count=n())
  
  list_edgelists[[length(list_edgelists)+1]] <- edgelist
}

sum_edgelist <- bind_rows(list_edgelists) %>% 
  group_by(puid1,puid2,PLD) %>% 
  summarize(count=sum(count))

# # load and clean nc data
# file <- "data/trajectories.nc"
# nc <- tidync(file)
# ncdata <- nc %>% 
#   hyper_tibble(force = TRUE) %>% 
#   filter(age_seconds %in% c(606600,1816200,3024000)) %>% # use only coordinates from 7, 21, and 35 d
#   mutate(PLD = round(age_seconds/86400), #convert to d
#          time=as.POSIXct(time, origin = "1970-01-01 00:00:00", tz="GMT")) %>% # stole this from Linda!
#   # select(lon, lat, PLD, time, trajectory) %>% #remove columns we won't use
#   st_as_sf(coords = c("lon","lat"),crs=4326) #convert to spatial sf object, CRS is a guess?
# 
# # load pu
# 
# pu <- st_read("data/pulayer_v1b2_blank_dd.shp") %>% 
#   st_transform(crs=4326)
# 
# 
# edgelist <- st_intersects(pu,ncdata) %>% 
#   data.frame() %>% 
#   left_join(rowid_to_column(ncdata,var="row.id"),
#             by = "row.id") %>% 
#   mutate(puid1=row.id,
#          puid2=ncdata$trajectory[col.id]) %>% 
#   group_by(puid1,puid2,PLD) %>% 
#   summarize(count=n())


# # plot a random set of 100 points to make sure we're reading things in correctly
# leaflet(data %>% sample_n(100)) %>%
#   addTiles() %>%
#   addMarkers()



