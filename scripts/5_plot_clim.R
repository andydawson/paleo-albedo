library(ggplot2)

cal_data = readRDS('data/lct_albedo_snow_modern_glob.RDS')#[,2:ncol(cal_data)]

# thorn_out_sm1 = readRDS('data/thorn_output_sm1.RDS')
# 
# thorn_out_sm2 = readRDS('data/thorn_output_sm2.RDS')
clim = read.csv('data/climate_CRU.csv', stringsAsFactors =FALSE)

clim_mean = clim %>% 
  group_by(site, month) %>% 
  summarise(snow = median(S), ppt=median(P), tmin=median(Tn), dd0=median(DD0), dd5=median(DD5), ffd=median(FFD), .groups='keep')
colnames(clim_mean) = c('site', 'month', 'snow', 'ppt', 'tmin', 'dd0', 'dd5', 'ffd')

clim_season = read.csv('data/climate_season_CRU.csv', stringsAsFactors =FALSE)

dd5_annual = clim_mean[,c('site', 'month', 'dd5')] %>%
group_by(site) %>%
  summarize(dd5_annual = sum(dd5))

clim_season_mean = clim_season %>% 
  group_by(site, season) %>% 
  summarise(snow_s = median(pas), dd0_s=median(dd0), dd5_s=median(dd5), ffd_s=median(ffd), .groups='keep')
colnames(clim_season_mean) = c('site', 'season', 'snow_s', 'dd0_s', 'dd5_s', 'ffd_s')

# clim_mean = merge(snow_mean_sm1, snow_mean_sm2[,1:4], by=c('site', 'month'))

psnow = cal_data[,c(1:5, 21:32)]
psnow$site = seq(1,nrow(psnow))
psnow_melt = melt(psnow, id.vars=c('site', 'long', 'lat', 'x', 'y', 'elev'))
psnow_melt$month = substr(psnow_melt$variable, 5,6)
psnow_melt$month = as.numeric(psnow_melt$month)
psnow_melt$variable = substr(psnow_melt$variable, 1,4)
colnames(psnow_melt) = c('site', 'long', 'lat', 'x', 'y', 'elev', 'variable', 'psnow', 'month')

alb = cal_data[,c(1:5, 9:20)]
alb$site = seq(1,nrow(alb))
alb_melt = melt(alb, id.vars=c('site', 'long', 'lat', 'x', 'y', 'elev'))
alb_melt$month = substr(alb_melt$variable, 4, 5)
alb_melt$month = as.numeric(alb_melt$month)
alb_melt$variable = "albedo"#substr(alb_melt$variable, 1,7)
colnames(alb_melt) = c('site', 'long', 'lat', 'x', 'y', 'elev', 'variable', 'alb', 'month')

dat = merge(psnow_melt, clim_mean, by=c('site', 'month'))
dat = merge(dat, alb_melt[,c('site', 'month', 'alb')], by=c('site', 'month'))

saveRDS(dat, 'data/calibration-albedo-climateNA.RDS')

dat_merge = merge(dat, clim_season_mean, by=c('site'))

saveRDS(dat_merge, 'data/calibration-albedo-climateNA-season.RDS')

# dat = data.frame(snow_mean, psnow=psnow_melt$value, alb=alb_melt$value/1000)

###############################################################################################################
## albedo versus other vars
###############################################################################################################

## compare variables with alb
ggplot(data=dat) +
  geom_point(aes(x=psnow, y=alb)) +
  facet_wrap(~month)

# ggplot(data=dat) +
#   geom_point(aes(x=snowpack, y=alb)) +
#   # geom_smooth(aes(x=snowpack, y=alb), formula = y~x, method='lm') +
#   facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(x=snow, y=alb)) +
  # geom_point(aes(x=snowpack_sm2, y=alb), colour="blue", alpha=0.5) +
  facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(x=log(snow), y=alb)) +
  # geom_smooth(aes(x=log(snowpack), y=alb), formula = y~x, method='lm') +
  facet_wrap(~month)


ggplot(data=dat) +
  geom_point(aes(x=dd0, y=alb)) +
  # geom_smooth(aes(x=log(snowpack), y=alb), formula = y~x, method='lm') +
  facet_wrap(~month)


ggplot(data=dat) +
  geom_point(aes(x=dd5, y=alb)) +
  # geom_smooth(aes(x=log(snowpack), y=alb), formula = y~x, method='lm') +
  facet_wrap(~month)

# ggplot(data=dat) +
#   geom_point(aes(x=tsnow, y=alb)) +
#   # geom_smooth(aes(x=tsnow, y=alb), formula = y~x, method='lm') +
#   facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(x=ppt, y=alb)) +
  facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(x=tmin, y=alb)) +
  facet_wrap(~month, scales="free")

ggplot(data=dat) +
  geom_point(aes(x=lat, y=tmin)) +
  facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(x=lat, y=snow)) +
  facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(x=et, y=alb)) +
  # geom_smooth(aes(x=et, y=alb), formula = y~x, method='lm') +
  facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(x=lat, y=ffd)) +
  # geom_smooth(aes(x=et, y=alb), formula = y~x, method='lm') +
  facet_wrap(~month)

## seasonal
ggplot(data=dat_merge) +
  geom_point(aes(x=snow_s, y=alb, colour=season)) +
  geom_smooth(aes(x=snow_s, y=alb, colour=season), formula = y~x, method='lm') +
  facet_wrap(~month)

ggplot(data=dat_merge) +
  geom_point(aes(x=lat, y=snow_s)) +
  geom_smooth(aes(x=lat, y=snow_s), formula = y~x, method='lm') +
  facet_wrap(~season, scales="free_x")

ggplot(data=dat_merge) +
  geom_point(aes(x=lat, y=dd0_s, colour=season)) +
  geom_smooth(aes(x=lat, y=dd0_s, colour=season), formula = y~x, method='lm') +
  facet_wrap(~month)

ggplot(data=dat_merge) +
  geom_point(aes(x=lat, y=dd5_s, colour=season)) +
  geom_smooth(aes(x=lat, y=dd5_s, colour=season), formula = y~x, method='lm') +
  facet_wrap(~month)
# 
# ggplot(data=dat_merge) +
#   geom_point(aes(x=snow_s, y=alb)) +
#   geom_smooth(aes(x=snow_s, y=alb), formula = y~x, method='lm') +
#   facet_grid(month~season, scales="free_x")

ggplot(data=dat_merge) +
  geom_point(aes(x=lat, y=dd0_s)) +
  geom_smooth(aes(x=lat, y=dd0_s), formula = y~x, method='lm') +
  facet_wrap(~season, scales="free_x")

ggplot(data=dat_merge) +
  geom_point(aes(x=lat, y=dd5_s)) +
  geom_smooth(aes(x=lat, y=dd5_s), formula = y~x, method='lm') +
  facet_wrap(~season, scales="free_x")

ggplot(data=dat_merge) +
  geom_point(aes(x=lat, y=ffd_s)) +
  geom_smooth(aes(x=lat, y=ffd_s), formula = y~x, method='lm') +
  facet_wrap(~season, scales="free_x")

ggplot(data=subset(dat_merge, season=='annual')) +
  geom_point(aes(x=snow_s, y=alb, colour=season)) +
  geom_smooth(aes(x=snow_s, y=alb, colour=season), formula = y~x, method='lm') +
  facet_wrap(~month)

ggplot(data=subset(dat_merge, season=='wt')) +
  geom_point(aes(x=snow_s, y=alb, colour=season)) +
  geom_smooth(aes(x=snow_s, y=alb, colour=season), formula = y~x, method='lm') +
  facet_wrap(~month)

ggplot(data=subset(dat_merge, season=='sp')) +
  geom_point(aes(x=snow_s, y=alb, colour=season)) +
  geom_smooth(aes(x=snow_s, y=alb, colour=season), formula = y~x, method='lm') +
  facet_wrap(~month)

ggplot(data=subset(dat_merge, season=='sm')) +
  geom_point(aes(x=snow_s, y=alb, colour=season)) +
  geom_smooth(aes(x=snow_s, y=alb, colour=season), formula = y~x, method='lm') +
  facet_wrap(~month)

ggplot(data=subset(dat_merge, season=='at')) +
  geom_point(aes(x=snow_s, y=alb, colour=season)) +
  geom_smooth(aes(x=snow_s, y=alb, colour=season), formula = y~x, method='lm') +
  facet_wrap(~month)


ggplot(data=subset(dat_merge, season=='annual')) +
  geom_point(aes(x=dd0_s, y=alb, colour=season)) +
  geom_smooth(aes(x=dd0_s, y=alb, colour=season), formula = y~x, method='lm') +
  facet_wrap(~month)

ggplot(data=subset(dat_merge, season=='wt')) +
  geom_point(aes(x=dd0_s, y=alb, colour=season)) +
  geom_smooth(aes(x=dd0_s, y=alb, colour=season), formula = y~x, method='lm') +
  facet_wrap(~month)

ggplot(data=subset(dat_merge, season=='sp')) +
  geom_point(aes(x=dd0_s, y=alb, colour=season)) +
  geom_smooth(aes(x=dd0_s, y=alb, colour=season), formula = y~x, method='lm') +
  facet_wrap(~month)

ggplot(data=subset(dat_merge, season=='sm')) +
  geom_point(aes(x=dd0_s, y=alb, colour=season)) +
  geom_smooth(aes(x=dd0_s, y=alb, colour=season), formula = y~x, method='lm') +
  facet_wrap(~month)

ggplot(data=subset(dat_merge, season=='at')) +
  geom_point(aes(x=dd0_s, y=alb, colour=season)) +
  geom_smooth(aes(x=dd0_s, y=alb, colour=season), formula = y~x, method='lm') +
  facet_wrap(~month)

ggplot(data=subset(dat_merge, season=='annual')) +
  geom_point(aes(x=lat, y=dd0_s, colour=season)) +
  geom_smooth(aes(x=lat, y=dd0_s, colour=season), formula = y~x, method='lm') +
  facet_wrap(~month)

ggplot(data=subset(dat_merge, season=='wt')) +
  geom_point(aes(x=dd0_s, y=alb, colour=season)) +
  geom_smooth(aes(x=dd0_s, y=alb, colour=season), formula = y~x, method='lm') +
  facet_wrap(~month)


ggplot(data=subset(dat_merge, season=='annual')) +
  geom_point(aes(x=lat, y=dd5_s, colour=season)) +
  geom_smooth(aes(x=lat, y=dd5_s, colour=season), formula = y~x, method='lm') +
  facet_wrap(~season)

ggplot(data=subset(dat_merge, season=='wt')) +
  geom_point(aes(x=dd5_s, y=alb, colour=season)) +
  geom_smooth(aes(x=dd5_s, y=alb, colour=season), formula = y~x, method='lm') +
  facet_wrap(~month)

## compare variables with psnow

ggplot(data=dat) +
  geom_point(aes(x=psnow, y=snow)) +
  facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(y=et, x=psnow)) + 
  # geom_smooth(aes(y=et, x=psnow), formula = y~x, method='lm') +
  facet_wrap(~month, scales="free")

ggplot(data=dat) +
  geom_point(aes(y=ppt, x=psnow)) +
  facet_wrap(~month, scales="free")

ggplot(data=dat) +
  geom_point(aes(y=tmin, x=psnow)) +
  facet_wrap(~month, scales="free")

ggplot(data=dat) +
  geom_point(aes(y=snowpack_sm1, x=psnow)) +
  # geom_smooth(aes(y=snowpack, x=psnow), formula = y~x, method='lm') +
  facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(y=log(snowpack_sm1), x=psnow)) +
  geom_smooth(aes(y=log(snowpack_sm1), x=psnow), formula = y~x, method='lm') +
  facet_wrap(~month)

## compare others from thorn

ggplot(data=dat) +
  geom_point(aes(x=snowpack, y=tsnow)) +
  facet_wrap(~month, scales="free")

ggplot(data=dat) +
  geom_point(aes(y=et, x=tsnow)) +
  facet_wrap(~month, scales="free")

ggplot(data=dat) +
  geom_point(aes(y=et, x=snowpack)) +
  facet_wrap(~month, scales="free")


ggplot(data=dat) +
  geom_point(aes(x=lat, y=snow)) +
  geom_smooth(aes(x=lat, y=snow), formula = y~x, method='lm') +
  facet_wrap(~month, scales="free")

ggplot(data=dat) +
  geom_point(aes(x=lat, y=dd0)) +
  geom_smooth(aes(x=lat, y=dd0), formula = y~x, method='lm') +
  facet_wrap(~month, scales="free")

ggplot(data=dat) +
  geom_point(aes(x=lat, y=dd5)) +
  geom_smooth(aes(x=lat, y=dd5), formula = y~x, method='lm') +
  facet_wrap(~month, scales="free")

