# library(raster)
library(maps)
# library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sp)

pollen_data=readRDS('data/pollen-modern-slice_v2.0.RDS')


alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

#sums pollen counts for each site  
# complete_dat <- (pollen_data %>%
#                    group_by(long, lat, sitename) %>%
#                    summarise(across(ARTEMISIA:TAXUS, sum), .groups='keep'))
complete_dat <- (pollen_data %>%
                   group_by(long, lat, sitename) %>%
                   summarise(across(ARTEMISIA:CUPRESSAX, sum), .groups='keep'))

complete_dat = data.frame(complete_dat)

xy = complete_dat[,1:2]

#don't know what to rename k lol
#map.where() indicates what part of the world those coordinates are located
k=map.where(database = "world", xy[,1],xy[,2])

# k=data.frame(k,xy,age=complete_dat[,4],complete_dat[,8:ncol(complete_dat)])
k=data.frame(k,xy, complete_dat[,4:ncol(complete_dat)])
#k[,1] = sapply(k[,1], function(x) if (is.na(x)){x=1} else {x=0})

k_na = k[which(is.na(k$k)),]


ggplot() + 
  geom_point(data=k_na, aes(x=long, y=lat), colour="blue") + 
  geom_point(data=k, aes(x=long, y=lat), colour="black")

k = k[which(!is.na(k$k)),]
#if name has Canada and USA:Al then keep, else give name NA
get_c <- function(x) {if (substr(x, 1,6) =="Canada") {"Canada"} else if (substr(x, 1,6) =="USA:Al") {"Alaska"} else {NA}}

k$country=sapply(k$k, get_c)
#substr(k$k, 1,6) == "Canada"

#delete NAs (which are countries not Canada and Alaska)
k = k[which(!is.na(k$country)),]
k = k[,which(colnames(k)!='country')]

#seperating coordinates in different dataframe
xy_new = k[-c(1,4:ncol(k))]

#assigning a crs to the pollen coordinates
spdf <- SpatialPointsDataFrame(coords = xy_new, data = k,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


#transforming to the albedo crs
pol_transform = spTransform(spdf, alb_proj)


coords = coordinates(pol_transform)
counts = k[,4:ncol(k)]

# filter out taxa with low count numbers
# 40 is still a very small threshhold, probably want larger
counts = counts[,which(colSums(counts)>40)]

colnames(coords)[1] <- 'x'
colnames(coords)[2] <- 'y'


#defining proportion function
prop = function(x){
  x/sum(x, na.rm=TRUE)}

props = t(apply(counts, 1, prop))
# saveRDS(props, 'data/pollen-modern-proportions.RDS')

prop_df = data.frame(coords, props)
saveRDS(prop_df, 'data/pollen-modern-proportions.RDS')

prop_df = data.frame(taxon = names(colSums(props)), prop = colSums(props))
prop_df = prop_df[order(prop_df$prop),]

#calculating proportions for each site
# pollen_props = t(apply(counts, 1, prop))
# rowSums(pollen_props)
# #merging pollen props and new reprojected coords
# dat_pollen = data.frame(coords, pollen_props)


dat_pollen = data.frame(coords, counts)

library(reshape2)
#melt will turn it into the long format 
dat_pollen_melt = melt(dat_pollen, id.vars=c('x', 'y'))


#reading in LCT
LCT = read.csv('data/taxon2LCT_translation_v2.0.csv', stringsAsFactors = FALSE)
LCT = LCT[,c('LCT', 'taxon')]


saveRDS(LCT, "data/LCT_table.RDS")

#matches the taxon from the dat_pollen_melt file to the LCT file and forms a  new column 'LCT' with the classification 
#dat_pollen_melt$variable pulls the column variable from that dataframe
#variable is the column name with all the taxons
#LCT$taxon pulls the column taxon from the csv file 
#dat_pollen_melt$LCT before the equal sign makes a new column witht that name
dat_pollen_melt$LCT = LCT[match(dat_pollen_melt$variable, LCT$taxon), 'LCT']

#removing Na LCT
any(is.na(dat_pollen_melt$LCT))
dat_pollen_melt= dat_pollen_melt[!is.na(dat_pollen_melt$LCT),]

#don't need variable colunmn anymore
dat_pollen_melt= dat_pollen_melt[-c(3)]

dat_pollen_melt$value=as.numeric(dat_pollen_melt$value)


foo = dat_pollen_melt %>%
  group_by(x,y,LCT) %>%
  summarise(count = sum(value))

pivot_mod = foo %>%
  pivot_wider(names_from = LCT, values_from = count)

propor=t(apply(pivot_mod[,3:5], 1, prop))

pivot_mod = data.frame(pivot_mod[,1:2], propor)
saveRDS(pivot_mod, 'data/pollen_modern_pivot.RDS')

library(elevatr)

alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

modern = readRDS('data/pollen_modern_pivot.RDS')

#this changes back to regular lat long
#assigning a crs to the pollen coordinates
spdf <- SpatialPointsDataFrame(coords = modern[,c('x','y')], data = modern,
                               proj4string = CRS('+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'))

#transforming to the albedo crs: epsg 102001
pol_transform = spTransform(spdf, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

latLong =data.frame(coordinates(pol_transform), modern)

latLong = latLong[-c(3:7)]
colnames(latLong) = c('lat', 'long')

ele_proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

ele_get = get_elev_point(latLong, ele_proj, src = "aws")

lct_modern = data.frame(latLong, modern[,c('x','y')], ele_get$elevation, modern[, 3:5])

saveRDS(lct_modern, 'data/lct_modern.RDS')

