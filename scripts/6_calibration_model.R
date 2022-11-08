library(gam)
library(mgcv)
library(ggplot2)
library(dplyr)
library(tidyr)


cal_data =readRDS('data/lct_albedo_snow_modern_bluesky.RDS')
cal_data2 = data.frame(site=seq(1,nrow(cal_data)), cal_data)
cal_data3 = cal_data2[,c('site', 'ET', 'OL', 'ST')]


###############################################################################################################
## march calibration model
###############################################################################################################

ctrl <- list(nthreads=4)

# # just x and y
#works better with smaller k values 
mod = gam(bs03 ~ s(x, y, bs="gp", k=130),
          data=cal_data, 
          family=betar(link="logit"), 
          method="REML", 
          na.action=na.omit, 
          control=ctrl)
gam.check(mod)
#vis.gam(mod,theta=30)


# # just x and y
mod = gam(bs03 ~ s(x, y, bs="gp", k=175),
          data=cal_data, 
          family=betar(link="logit"), 
          method="REML", 
          na.action=na.omit, 
          control=ctrl)
gam.check(mod)

vis.gam(mod,theta=30)

# add elevation
#tp vs gp model
mod2 = gam(bs03 ~ s(x, y, bs='tp', k=175) + s(elev, k=30),
          data=cal_data,
          family=betar(link="logit"),
          method="REML",
          na.action=na.omit)

# add eleveation
#gp model works better than tp
mod2 = gam(bs03 ~ s(x, y, bs="gp", k=100) + s(elev, k=30),
           data=cal_data, 
           family=betar(link="logit"), 
           method="REML", 
           na.action=na.omit, 
           control=ctrl)
gam.check(mod2)
#vis.gam(mod,theta=30)

AIC(mod, mod2)

# add OL
mod3 = gam(bs03 ~ s(x, y, bs='gp', k=100) + s(elev, k=30) + s(OL, k=20),
           data=cal_data, 
           family=betar(link="logit"), 
           method="REML", 
           na.action=na.omit, 
           control=ctrl)
gam.check(mod3)

AIC(mod, mod2, mod3)
anova.gam(mod, mod2, mod3)

# add ET
mod4 = gam(bs03 ~ s(x, y, bs='gp', k=80) + s(elev, k=30) + s(OL, k=20) + s(ET, k=20),
           data=cal_data, 
           family=betar(link="logit"), 
           method="REML", 
           na.action=na.omit, 
           control=ctrl)
gam.check(mod4)

AIC(mod, mod2, mod3, mod4)


# add ST
#this takes a while 
mod5 = gam(bs03 ~ s(x, y, bs='gp', k=100) + s(elev, k=30) + s(OL, k=20) + s(ET, k=20) + s(ST, k=20),
           data=cal_data, 
           family=betar(link="logit"), 
           method="REML", 
           na.action=na.omit, 
           control=ctrl)
gam.check(mod5)

AIC(mod, mod2, mod3, mod4, mod5)


saveRDS(mod, 'data/calibration_model1.RDS')

###############################################################################################################
## plotting model fit for bs03
###############################################################################################################
preds3 = predict.gam(mod3, 
                     type="response", 
                     newdata=cal_data,
                     se.fit=TRUE)

cal_data$preds_mod3 = preds3$fit
#dat_all$predict = preds$fit

ggplot(data=cal_data) + 
  geom_point(aes(x=bs03, y=preds_mod3)) +
  geom_abline(slope=1, intercept=0, colour="red") +
  xlim(c(0,1)) + 
  ylim(c(0,1))

plot(mod3)
plot(mod3, residuals=TRUE, all.terms=TRUE, pch=1)

#model 4 
preds = predict.gam(mod4, 
                    type="response", 
                    newdata=cal_data,
                    se.fit=TRUE)

cal_data$preds_mod4 = preds$fit
#dat_all$predict = preds$fit

ggplot(data=cal_data) + 
  geom_point(aes(x=bs03, y=preds_mod4)) +
  geom_abline(slope=1, intercept=0, colour="red") +
  xlim(c(0,1)) + 
  ylim(c(0,1))



###############################################################################################################
## march full model fit with x,y,OL,ET,ST,elevation
###############################################################################################################

cal_model = readRDS('data/calibration_model5.RDS')

preds = predict.gam(cal_model, 
                    type="response", 
                    newdata=cal_data,
                    se.fit=TRUE)

cal_data$preds_mod5 = preds$fit
#dat_all$predict = preds$fit

ggplot(data=cal_data) + 
  geom_point(aes(x=bs03, y=preds_mod5)) +
  geom_abline(slope=1, intercept=0, colour="red", size=1, lty=2) +
  xlim(c(0,1)) + 
  ylim(c(0,1)) +
  coord_fixed() +  
  theme_bw() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14)) +
  xlab('Albedo Data') +
  ylab('Albedo Model')
ggsave('figures/cal_model_vs_datamod5.png')
#ggsave('figures/cal_model_vs_data.pdf')

cal_data$preds = cal_model$fitted.values

################################################################################################
#difference maps 
#################################################################################################
#calculating difference between model and predicted values from model #5
#this is for march 
cal_data$diff_mod5 = cal_data$preds_mod5 - cal_data$bs03
saveRDS(cal_data, 'data/cal_data.RDS')
######################################################################################################
#FIGURES
######################################################################################################
pbs_ll = readRDS('data/map-data/geographic/pbs_ll.RDS')

pbs = readRDS('data/map-data/geographic/pbs.RDS')


#difference figure 
ggplot()+
  geom_polygon(data=world_proj, aes(x=x, y=y, group=group), fill='black')+
  geom_point(data=cal_data, aes(x=x, y=y, colour = diff_mod5))+
  scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white', limits = c(-0.2,0.2))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave('figures/march_modern_diff.png')


#better version of above but the background is white 
ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), fill='black') +
  geom_point(data=cal_data, aes(x=long, y=lat, colour = diff_mod5))+
  scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white', limits = c(-0.2,0.2))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



#frequency of differences 
#we can see that it is mainly focused at 0
ggplot(data=cal_data) + 
  geom_histogram(aes(x=diff_mod5)) +
  theme_bw()
ggsave('figures/march_histo_frequency.png')








ggplot()+
  geom_path(data=pbs_ll, aes(x=long, y=lat, group=group), colour='grey')+
  geom_point(data=cal_data, aes(x=long, y=lat, colour = diff_mod5))+
  scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white', limits = c(-0.2,0.2))+
  theme_bw()
