# library(raster)
library(maps)
# library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sp)
library(reshape2)

pollen_time = readRDS('data/pollen-sites-times-series-all_v2.0.RDS')

alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

#sums pollen counts for each site  
# complete_dat <- (pollen_data %>%
#                    group_by(long, lat, sitename) %>%
# #                    summarise(across(ARTEMISIA:TAXUS, sum), .groups='keep'))
# complete_dat <- (pollen_data %>%
#                    group_by(long, lat, sitename) %>%
#                    summarise(across(ARTEMISIA:CUPRESSAX, sum), .groups='keep'))

pollen_time = data.frame(pollen_time)

xy = pollen_time[,1:2]

#don't know what to rename k lol
#map.where() indicates what part of the world those coordinates are located
k=map.where(database = "world", xy[,1],xy[,2])

# k=data.frame(k,xy,age=complete_dat[,4],complete_dat[,8:ncol(complete_dat)])
k=data.frame(k,xy, pollen_time[,4:ncol(pollen_time)])
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

# # filter out taxa with low count numbers
# # 40 is still a very small threshhold, probably want larger
# counts = counts[,which(colSums(counts)>40)]

colnames(coords)[1] <- 'x'
colnames(coords)[2] <- 'y'


# dat_pollen = data.frame(coords, pollen_props), merging
dat_pollen = data.frame(coords, counts)
#adding age column 
# df_bind_age = data.frame(age=k[,4], dat_pollen)


#melt will turn it into the long format, id.vars keeps those columns 
dat_pollen_melt = melt(dat_pollen, id.vars=c('x', 'y','age', 'sitename', 'siteid', 'datasetid'))

#read in LCT table
LCT = readRDS("data/LCT_table.RDS")

#matches the taxon from the dat_pollen_melt file to the LCT file and forms a  new column 'LCT' with the classification 
#dat_pollen_melt$variable pulls the column variable from that dataframe
#variable is the column name with all the taxons
#LCT$taxon pulls the column taxon from the csv file 
#dat_pollen_melt$LCT before the equal sign makes a new column witht that name
dat_pollen_melt$LCT = LCT[match(dat_pollen_melt$variable, LCT$taxon), 'LCT']


#removing NA LCT which are taxons we don't want
any(is.na(dat_pollen_melt$LCT))
dat_pollen_melt= dat_pollen_melt[!is.na(dat_pollen_melt$LCT),]

#dividing into chunks 
#paleo <- tree.cores[tree.cores$age >= 150, ]
paleo_bins <- seq(0, 20000, by = 2000)
#using the age column to break up the data by the paleo_bins vector
paleo_cut <- cut(dat_pollen_melt$age, include.lowest = TRUE, breaks = paleo_bins)
#adds the column cut, 1 cut represents data for 2000 years
dat_pollen_melt$cut <- as.integer(paleo_cut)

#deleting age column and variable column which is taxon names
#not necessary for grouping 
dat_pollen_melt = dat_pollen_melt[,!(colnames(dat_pollen_melt) %in% c('variable', 'age'))]

grouped_data <- group_by(dat_pollen_melt, x, y, LCT, cut)

grouped_data$value=as.numeric(grouped_data$value)

#summarizing data by summing the pollen counts 
pol_summary = summarise(grouped_data, summed_counts = sum(value, na.rm=TRUE), .groups = 'keep')
foo = pol_summary %>%
  group_by(x, y, cut) %>%
  mutate(pol_prop = summed_counts / sum(summed_counts))

#deleting summed counts column no long necessary
foo = foo[, -5]

#pivot data so each LCT has its own column
pivot_foo = foo %>%
  pivot_wider(names_from = LCT, values_from = pol_prop)

#deleting cut with NA values because it has a negative NA which will be included in the modern pollen dataset
pivot_foo = pivot_foo[-which(is.na(pivot_foo$cut)),]

#deleting NA ecoregions, still to be determined why 
#there are still Na ecoregions maybe in america?
#come back to this
# pivot_foo = pivot_foo[-which(is.na(pivot_foo$NA_L2NAME)),]

#saved pivot_foo with ecoregions - full dataset 
saveRDS(pivot_foo, 'data/lct_paleo.RDS')


library(elevatr)

alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

lct_paleo = readRDS('data/lct_paleo.RDS')

#this changes back to regular lat long
#assigning a crs to the pollen coordinates
spdf <- SpatialPointsDataFrame(coords = lct_paleo[,c('x','y')], data = lct_paleo,
                               proj4string = CRS('+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'))

#transforming to the albedo crs: epsg 102001
lct_paleo_ll = spTransform(spdf, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

latLong =data.frame(coordinates(lct_paleo_ll), lct_paleo)

latLong = latLong[-c(5:8)]
colnames(latLong) = c('long', 'lat')

ele_proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

ele_get = get_elev_point(latLong, ele_proj, src = "aws")

lct_paleo = data.frame(latLong[,c('lat', 'long')], lct_paleo[,c('x','y')], elev = ele_get$elevation, lct_paleo[, 3:6])


library(data.table)
lct_paleo = data.table(lct_paleo)
lct_paleo = lct_paleo[,site:= .GRP, by=.(lat, long)]
lct_paleo = data.frame(lct_paleo[,'site'], lct_paleo[,1:9])

saveRDS(lct_paleo, 'data/lct_paleo.RDS')
