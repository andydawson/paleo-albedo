library(ggplot2)

cal_data = readRDS('data/lct_albedo_snow_modern_glob.RDS')#[,2:ncol(cal_data)]

# thorn_out_sm1 = readRDS('data/thorn_output_sm1.RDS')
# 
# thorn_out_sm2 = readRDS('data/thorn_output_sm2.RDS')

thorn_out_sm1 = readRDS('data/thorn_output_glob_sm1.RDS')

thorn_out_sm2 = readRDS('data/thorn_output_glob_sm2.RDS')


snow_mean_sm1 = thorn_out_sm1 %>% 
  group_by(site, month) %>% 
  summarise(snowpack = median(snow), tsnow=mean(snowcover), et=mean(et), ppt=median(P), tmin=median(Tn), .groups='keep')
colnames(snow_mean_sm1) = c('site', 'month', 'snowpack_sm1', 'tsnow_sm1', 'et', 'ppt', 'tmin')

snow_mean_sm2 = thorn_out_sm2 %>% 
  group_by(site, month) %>% 
  summarise(snowpack = median(snow), tsnow=mean(snowcover), et=mean(et), ppt=median(P), tmin=median(Tn), .groups='keep')
colnames(snow_mean_sm2) = c('site', 'month', 'snowpack_sm2', 'tsnow_sm2', 'et', 'ppt', 'tmin')

snow_mean = merge(snow_mean_sm1, snow_mean_sm2[,1:4], by=c('site', 'month'))

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

dat = merge(psnow_melt, snow_mean, by=c('site', 'month'))
dat = merge(dat, alb_melt[,c('site', 'month', 'alb')], by=c('site', 'month'))

saveRDS(dat, 'data/calibration-albedo-climate.RDS')

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
  geom_point(aes(x=snowpack_sm1, y=alb)) +
  geom_point(aes(x=snowpack_sm2, y=alb), colour="blue", alpha=0.5) +
  # geom_smooth(aes(x=snowpack, y=alb), formula = y~x, method='lm') +
  facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(x=log(snowpack), y=alb)) +
  # geom_smooth(aes(x=log(snowpack), y=alb), formula = y~x, method='lm') +
  facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(x=tsnow, y=alb)) +
  # geom_smooth(aes(x=tsnow, y=alb), formula = y~x, method='lm') +
  facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(x=ppt, y=alb)) +
  facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(x=tmin, y=alb)) +
  facet_wrap(~month)

ggplot(data=dat) +
  geom_point(aes(x=et, y=alb)) +
  # geom_smooth(aes(x=et, y=alb), formula = y~x, method='lm') +
  facet_wrap(~month)

## compare variables with psnow

ggplot(data=dat) +
  geom_point(aes(x=psnow, y=tsnow)) +
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
