# IMPORT AND PREP ####
library(dplyr)
library(plotrix)


# Harvest dataset ##############
# These are 131 trees measured and harvested by Anders and Winta in 2016

dat <- read.csv("M:/Anders L Kolstad/systherb data/exported cvs/harvested trees trondelag 2016.csv", sep=";")

colnames(dat)[7] <- "hgt"
colnames(dat)[8] <- "lth"
colnames(dat)[9] <- "DGL"
colnames(dat)[10] <- "DBH"
colnames(dat)[11] <- "leaves"
colnames(dat)[12] <- "stems"

dat$biomass<-(dat$stems+dat$leaves)   


# Excluding birch from thinned sites that had no leaves:
dat <- dat[!is.na(dat$biomass),]


pine <- dat[dat$species == "pine",]
pine2 <- pine[pine$treatment != "UB",]
birch <- dat[dat$species == "birch",]
rowan <- dat[dat$species == "rowan",]
spruce <- dat[dat$species == "spruce",]


# 2016 dataset #####################
# Density dataset from 2016 with accurate heights and diameters:
dat2 <- read.csv("M:/Anders L Kolstad/systherb data/exported cvs/trondelag_hgt_dia_2016.csv", sep=";")

summary(dat2$Treatment) # lots of NA's
dat3 <- filter(dat2,
               Treatment != "")
dat3$Treatment <- droplevels(dat3$Treatment)


colnames(dat3)[11:12]<- c("hgt", "DGL")
dat3 <- dat3[,-c(15:16)]

pine2016 <- dat3[dat3$species=="pine",]
spruce2016 <- dat3[dat3$species=="spruce",]
birch2016 <- dat3[dat3$species=="birch",]
rowan2016 <- dat3[dat3$species=="rowan",]



####  *********************  ######
###################################
## --  PINE MODEL   --   ###########
#####################################

#plot(pine$DGL, pine$hgt, pch=ifelse(pine$treatment=="UB", 16, 1),
#     main = "Pine")
summary(pine)
###################################
##  GLOBAL PINE DGL+HGT ###########
###################################
# for both B and UB
M_pine <- lm(biomass~I(DGL^3)+I(DGL^2)+I(hgt^2)-1, data = pine)
M_pine <- update(M_pine, .~. -I(hgt^2))
summary(M_pine)

#par(mfrow=c(1,2))
#par(mar=c(5, 7,5,5))
plot(pine$DGL, pine$biomass, pch=ifelse(pine$treatment=="UB", 16, 1),
     main = "", xlim=c(-5, 110), ylim=c(-10,3500), xlab="Diameter at ground level (mm)", 
     ylab="Biomass (g)")
#max(pine$DGL)
DGLgrid <- seq(0,92, 1)
pine_p <- predict(M_pine, newdata=list(DGL=DGLgrid))
lines(DGLgrid, pine_p)
text(x=20, y=2500, "y=0.3258390*DAB^2+\n0.0007434*DAB^3\nR-sq=0.9844", cex=0.8)

###############################
## -- B Pine HGT Model  --  ######
##############################
M_pineBhgt <- lm(biomass~hgt+I(hgt^2)-1, data=pine2)
M_pineBhgt <- update(M_pineBhgt, .~. -hgt)
summary(M_pineBhgt)
plot(pine2$hgt, pine2$biomass,
     main = "Browsed Pine", xlab="Height (cm)", ylab="Biomass (g)", xlim=c(-10,250))
grid_M_pineBhgt <- seq(0,250, 20)
pred_M_pineBhgt <- predict(M_pineBhgt, newdata=list(hgt=grid_M_pineBhgt))
lines(grid_M_pineBhgt, pred_M_pineBhgt)

#############################################
# Back-fitted HGT model for UB Pine  #####
##############################################
pine2016$pp <- predict(M_pine, newdata = pine2016)
#head(pine2016[,-c(13:16, 1:6)], 20)

#par(mfrow=c(1,1))
plot(pine2016$hgt[pine2016$Treatment=="unbrowsed"], pine2016$pp[pine2016$Treatment=="unbrowsed"], 
     main = "Un-browsed pine", xlab="Measured height (cm)", ylab="Modelled biomass (g)", 
     ylim=c(0,1800), xlim=c(0,350))
M_pine2 <- lm(pp~I(hgt^2)+hgt-1, data=pine2016[pine2016$Treatment=="unbrowsed",])
M_pine2 <- update(M_pine2, .~. -hgt)
summary(M_pine2)
mhgtGrid <- seq(0,350,5)
pine2016ub_p <- predict(M_pine2, newdata=list(hgt=mhgtGrid))
lines(mhgtGrid, pine2016ub_p)
text(x=110, y=1500, "y=0.015589*hgt^2\nR-sq = 0.8865", cex=1)


#####  **************  ###############
######################################
## --  ROWAN MODELS -- ###############
######################################
summary(rowan)
#plot(rowan$DGL, rowan$hgt, pch=ifelse(rowan$treatment=="UB", 16, 1),
#     main = "Rowan")
#plot(rowan$hgt, rowan$biomass, pch=ifelse(rowan$treatment=="UB", 16, 1),
#     main = "Rowan")

# we have enough points to make  models directly
rowanB <- filter(rowan,
                 treatment!="UB")
rowanUB <- filter(rowan,
                  treatment=="UB")
summary(rowanB)
summary(rowanUB)
###########################################
### --  Rowan UB HGT   #################
###########################################

M_rowanUB <- lm(biomass~I(hgt^2)-1, data = rowanUB)
summary(M_rowanUB)
plot(rowanUB$hgt, rowanUB$biomass, xlim=c(0,420), ylim=c(0,1000), 
     xlab="Height (cm)", ylab="Biomass (g)", main="Un-browsed Rowan")
hgtGridR <- seq(0,420,20)
rowan_ppUB <- predict(M_rowanUB, newdata = list(hgt=hgtGridR))
lines(hgtGridR, rowan_ppUB)
#text(x=90, y=800, "y=0.0053962*hgt^2\nR-sq = 0.9827", cex=0.8)

#######################################
### --  Rowan B  HGT   ###############
#########################################

M_rowanB <- lm(biomass~I(hgt^2)+hgt-1, data = rowanB)
M_rowanB <- update(M_rowanB, .~. -hgt)
summary(M_rowanB)


plot(rowanB$hgt, rowanB$biomass, xlim=c(0,100), ylim=c(0,150), 
     xlab="Height (cm)", ylab="Biomass (g)", main="Browsed Rowan")
hgtGridR2 <- seq(0,85,5)
rowan_ppB <- predict(M_rowanB, newdata = list(hgt=hgtGridR2))
lines(hgtGridR2, rowan_ppB)
#text(x=20, y=110, "y=0.010872*hgt^2\nR-sq = 0.6994", cex=0.8)

########################################
### --  Rowan  HGT+DGL   ###############
########################################
###      for year 2016   
###  same for B and UB   

M_rowan2 <- lm(biomass~hgt+DGL+I(hgt^2)+I(DGL^2)-1, data=rowan)
M_rowan2 <- update(M_rowan2, .~. -hgt)
M_rowan2 <- update(M_rowan2, .~. -DGL)
#summary(M_rowan2)

###  **********************  ######
###################################
#### BIRCH MODELS  ################
###################################

#plot(birch$DGL, birch$hgt, pch=ifelse(birch$treatment=="UB", 16, 1),
#     main = "Birch")
#plot(birch$hgt, birch$biomass, pch=ifelse(birch$treatment=="UB", 16, 1),
#     main = "Birch")
summary(birch)
#################################
### GLOBAL MODEL #################
##################################

M_birch <- lm(biomass~I(DGL^2)+DGL+hgt+I(hgt^2)+0, data = birch)
M_birch <- update(M_birch, .~. -hgt)
M_birch <- update(M_birch, .~. -DGL)
#summary(M_birch)

B_surface <- expand.grid(hgt=seq(0,300,by=20),
                      DGL=seq(0,40,by=5))
B_surface$pp <- predict(M_birch,newdata=B_surface)
library(rgl)
#with(birch,plot3d(hgt, DGL, biomass, type="s", col=ifelse(birch$treatment=="UB", "blue", "gray"), 
#                 xlab="Height (cm)", ylab="Diamater at ground level (mm)",  zlab =  "Biomass (g)"))
#
#with(B_surface,surface3d(unique(hgt),unique(DGL),pp,
#                      alpha=0.3, front="lines"))

###########################################
### B Birch HGT Model #####################
##########################################


birch_B <- filter(birch,
                       treatment != "UB")
plot(birch_B$hgt, birch_B$biomass, xlim=c(0,300), ylim=c(-1,1000), 
     xlab="Height (cm)", ylab="Biomass (g)", main="Browsed Birch")

M_Abirch_B <- lm(biomass~hgt+I(hgt^2)+I(hgt^3)-1, data=birch_B)
M_Abirch_B <- update(M_Abirch_B, .~. -I(hgt^3))
M_Abirch_B <- update(M_Abirch_B, .~. -hgt)
summary(M_Abirch_B)
hgtGridB3 <- seq(0,300,10)
birch_ppB3 <- (predict(M_Abirch_B, newdata = list(hgt=hgtGridB3)))
lines(hgtGridB3, birch_ppB3)

# testing to see if segmented regressino is better - concusion is that it doesn't matter too much

#small_birch <- filter(birch,
#                      hgt<130)
#M_Sbirch_B <- lm(biomass~hgt+I(hgt^2)+I(hgt^3)-1, data=small_birch)
#M_Sbirch_B <- update(M_Sbirch_B, .~. -I(hgt^2))
#summary(M_Sbirch_B)


#hgtGridB <- seq(0,130,10)
#birch_ppB <- (predict(M_Sbirch_B, newdata = list(hgt=hgtGridB)))
#lines(hgtGridB, birch_ppB)
#text(x=80, y=700, "valid for heights <130cm\ny=0.01673*hgt +\n0.00007203*hgt^3\nR-sq = 0.9399", cex=0.8)



#large_birch <- filter(birch,
#                      hgt>100)
#M_Lbirch_B <- lm(biomass~hgt+I(hgt^2)+I(hgt^3)-1, data=large_birch)
#M_Lbirch_B <- update(M_Lbirch_B, .~. -I(hgt^3))
#M_Lbirch_B <- update(M_Lbirch_B, .~. -hgt)
#summary(M_Lbirch_B)
#hgtGridB2 <- seq(100,300,10)
#birch_ppB2 <- (predict(M_Lbirch_B, newdata = list(hgt=hgtGridB2)))
#lines(hgtGridB2, birch_ppB2)


###########################################
### Back-fitted Birch UB HGT Model #########
###########################################

birch2016$pp <- predict(M_birch, newdata = birch2016)
#head(birch2016[,-c(13:16, 1:6)], 20)

par(mfrow=c(1,2))

plot(birch2016$hgt[birch2016$Treatment=="unbrowsed"], birch2016$pp[birch2016$Treatment=="unbrowsed"], 
     main = "Un-browsed birch", xlab="Measured height (cm)", ylab="Modelled biomass (g)", 
     ylim=c(0,4000), xlim=c(0,700))

M_birch2 <- lm(pp~I(hgt^2)+hgt-1, data=birch2016[birch2016$Treatment=="unbrowsed",])
summary(M_birch2)
hgtGridB_UB <- seq(0,600,20)
birch2016ub_p <- predict(M_birch2, newdata=list(hgt=hgtGridB_UB))
lines(hgtGridB_UB, birch2016ub_p)
text(x=200, y=3000, "y=0.1702740*hgt+\n0.0100180*hgt^2\nR-sq = 0.9936", cex=1)

### *********************  ###########
######################################
### --  SPRUCE MODELs  -- ############
######################################

#plot(spruce$DGL, spruce$hgt, pch=ifelse(spruce$treatment=="UB", 16, 1),
#     main = "Spruce")
#plot(spruce$hgt, spruce$biomass, pch=ifelse(spruce$treatment=="UB", 16, 1),
#     main = "Spruce")
# using same model for B and UB...
summary(spruce)
#####################################
### Spruce HGT Model ############
#################################
M_spruce <- lm(biomass~hgt+I(hgt^2)+I(hgt^3)-1, data=spruce)
M_spruce <- update(M_spruce, .~. -I(hgt^3))
M_spruce <- update(M_spruce, .~. -hgt)

summary(M_spruce)

plot(spruce$hgt, spruce$biomass, xlim=c(0,350), ylim=c(0,3600), 
     xlab="Height (cm)", ylab="Biomass (g)", main="Spruce")
hgtGridS <- seq(0,300,10)
spruce_pp <- (predict(M_spruce, newdata = list(hgt=hgtGridS)))
lines(hgtGridS, spruce_pp)
#text(x=50, y=3000, "y=0.038068*hgt^2\nR-sq = 0.9458", cex=0.8)

#####################################
#### Spruce HGT+DGL #################
###################################
M_spruce2 <- lm(biomass~hgt+I(hgt^2)+DGL+I(DGL^2)+I(DGL^3)+I(hgt^3)-1, data=spruce)
M_spruce2 <- update(M_spruce2, .~. -I(hgt^3))
M_spruce2 <- update(M_spruce2, .~. -I(DGL^2))
M_spruce2 <- update(M_spruce2, .~. -DGL)
M_spruce2 <- update(M_spruce2, .~. -hgt)
#summary(M_spruce2)


################################################
##########  *************  ####################
##### Standing Biomass 2016   #################

#summary(dat3$species)
dat4 <- dat3[dat3$species!="",]
dat4$species <- droplevels(dat4$species)
levels(dat4$species)


dat4$biomass <- ifelse(dat4$species=="pine", predict(M_pine, newdata=dat4), "")
dat4$biomass <- ifelse(dat4$species=="rowan", predict(M_rowan2, newdata=dat4), dat4$biomass)
dat4$biomass <- ifelse(dat4$species=="selje", predict(M_rowan2, newdata=dat4), dat4$biomass)
dat4$biomass <- ifelse(dat4$species=="birch", predict(M_birch, newdata=dat4), dat4$biomass)
dat4$biomass <- ifelse(dat4$species=="lavlandsbj?rk", predict(M_birch, newdata=dat4), dat4$biomass)
dat4$biomass <- ifelse(dat4$species=="spruce", predict(M_spruce2, newdata=dat4), dat4$biomass)
dat4$biomass <- ifelse(dat4$species=="juniper", predict(M_spruce2, newdata=dat4), dat4$biomass)

dat4$biomass <- as.numeric(dat4$biomass)
#summary(dat4$biomass)
hist(dat4$biomass, breaks = 20)
#plot(dat4$biomass)
#identify(dat4$biomass)
#dat4[3373,]
# there's a large spruce at sings?s. Would be nice to exclude it from all years, cause its hard to estimate growth on such a alarge tree
dat5 <- dat4[-3373,]
plot(dat5$biomass)
#identify(dat5$biomass)
#dat5[2865,]
# another large spruce at Borgan
dat6 <- dat5[-2865,]
plot(dat6$biomass)
#max(dat6$biomass) # 18097.42
#max(dat6$hgt)
#max(dat5$hgt)
#max(dat4$hgt)
hist(dat6$biomass)
# ** #  PROTOCOL: Excluding trees above 6 meters (one is 6.27 (Borgan) and the nest is 14m (Sings?s))



#Extract decidious biomass ############
levels(dat6$Sub_plot)
dat6$Sub_plot <- droplevels(dat6$Sub_plot)
dat_dec <- dat6[dat6$species!= "pine" & dat6$species!= "spruce" & dat6$species!= "juniper",]

dat_dec_circle <- aggregate(cbind(Dec_Biomass = dat_dec$biomass), 
                  by= list(locationID=dat_dec$locationID, #site = dat_dec$site_name, 
                           Treatment=dat_dec$Treatment, circle=dat_dec$Sub_plot), 
                  FUN = sum, na.rm = T, drop=F)
dim(dat_dec_circle) # 115 -> 120 
dat_dec_plot <- aggregate(cbind(Dec_Biomass = dat_dec_circle$Dec_Biomass), 
                            by= list(locationID=dat_dec_circle$locationID, 
                                     Treatment=dat_dec_circle$Treatment),  
                            FUN = mean, na.rm = F, drop=F)
dim(dat_dec_plot)
dat_dec_plot$dec_biomass_kg_m2 <- (dat_dec_plot$Dec_Biomass/1000)/(pi*2^2)

#Extract coniferous biomass
dat_con <- filter(dat6,
                  species== "pine" |
                  species == "spruce" |
                  species == "juniper")
dat_con_circle <- aggregate(cbind(Biomass = dat_con$biomass), 
                        by= list(locationID=dat_con$locationID, #site = dat_con$site_name, 
                                 Treatment=dat_con$Treatment, circle=dat_con$Sub_plot), 
                        FUN = sum, na.rm = T, drop=F)
dim(dat_con_circle) # 107 -> 120 
dat_con_plot <- aggregate(cbind(Biomass = dat_con_circle$Biomass), 
                            by= list(locationID=dat_con_circle$locationID, 
                                     Treatment=dat_con_circle$Treatment), 
                            FUN = mean, na.rm = F)
dim(dat_con_plot)
summary(dat_con_plot$Biomass)
dat_con_plot$con_biomass_kg_m2 <- (dat_con_plot$Biomass/1000)/(pi*2^2)

DC_data <- dat_dec_plot
DC_data <- select(DC_data, -Dec_Biomass)
DC_data$con_biomass_kg_m2 <- dat_con_plot$con_biomass_kg_m2
DC_data$DC_ratio <- DC_data$dec_biomass_kg_m2/DC_data$con_biomass_kg_m2
#write.csv(DC_data, file="M:/Anders L Kolstad/systherb data/exported cvs/standing biomass con-dec.csv", row.names = F)

#########################

bio_circle <- aggregate(cbind(Biomass_cirle = dat6$biomass), 
                            by= list(locationID=dat6$locationID, #site = dat_dec$site_name, 
                                     Treatment=dat6$Treatment, circle=dat6$Sub_plot), 
                            FUN = sum, drop=F)
dim(bio_circle) #  120 
bio_plot <- aggregate(cbind(Biomass_plot = bio_circle$Biomass_cirle), 
                          by= list(locationID=bio_circle$locationID, 
                                   Treatment=bio_circle$Treatment),  
                          FUN = mean, drop=F)
dim(bio_plot) #30
bio_plot$biomass_kg_m2 <- (bio_plot$Biomass_plot/1000)/(pi*2^2)

bio_plot_excl <- filter(bio_plot, Treatment=="unbrowsed" )

bio_plot_excl <- arrange(bio_plot_excl, biomass_kg_m2)                                    #selbu_flub B is way to high for some reason
par(mar=c(10,5,3,3))
barplot(bio_plot_excl$biomass_kg_m2, 
        names.arg = bio_plot_excl$locationID, las=2, ylab="Biomass (kg/m2)", 
        main="Standing Biomass per 2016 in Exclosures")

# biomass is in g per ~50m2
(pi*2^2)*4
# 1kg/m2 = 10 tons(or Mg) per ha


#########  **************  ####################
#########   BIOMASS 2008-2015 #################
density <- read.csv("M:/Anders L Kolstad/systherb data/exported cvs/density09til15.csv", sep=";")

library(dplyr)      
library(reshape2)   # dcast & melt
library(reshape)    # untable

colnames(density)
den <- select(density,
              lokalitetid,
              region,
              aar,
              rute,
              flate,
              treart,
              skikt,
              antall_,
              arskogbon)




# Insted of height classes I will use the center values for each class:
# but lets see how height was recorded
table(den$skikt, den$aar)
# Max height class is 7 !!! (except in 2014)

# need same protocol for all years
den$skikt[den$skikt >7] <- 7
summary(den$skikt)   # noen nuller
den <- filter(den,
              treart != "ingen",
              treart != "ukjent",
              treart != "klunger",
              treart != "hassel")   # regner klunger og hassel som busker
summary(den$skikt)  # nuller borte

# some trees were 20+ meters !!

den$hgt <- den$skikt*50-25
summary(den$hgt)


#NEED TO MELT/untable "ANTALL" INTO SEPARATE ROWS
#summary(den$antall_) # max = 216, dvs jeg kan ikke gj?re dcast
#hist(den$antall_, breaks = 20)
den_ut <- untable(df = den, num = den$antall_)

#plot(den_ut$hgt[den_ut$lokalitetid==24 & den_ut$aar==2009]) # the large spruce was only counted in 2016
#plot(den_ut$hgt[den_ut$lokalitetid==15 & den_ut$aar==2013])
#plot(den_ut$hgt[den_ut$lokalitetid==15 & den_ut$aar==2014]) # some big trees here that are not counted in other years
#plot(den_ut$hgt[den_ut$lokalitetid==15 & den_ut$aar==2015]) 

#levels(den_ut$treart)


# Predict biomass from model above
# OBS - Models probably not valid for all scenarios at Tingvoll where there are large pine for example
den_ut$biomass <- ifelse(den_ut$treart=="furu" & den_ut$rute=="ub", predict(M_pine2, newdata=den_ut), "")
den_ut$biomass <- ifelse(den_ut$treart=="furu" & den_ut$rute=="b", predict(M_pineBhgt, newdata=den_ut), den_ut$biomass)

den_ut$biomass <- ifelse(den_ut$treart=="gran", predict(M_spruce, newdata=den_ut), den_ut$biomass)

den_ut$biomass <- ifelse(den_ut$treart=="rogn" & den_ut$rute=="ub", predict(M_rowanUB, newdata=den_ut), den_ut$biomass)
den_ut$biomass <- ifelse(den_ut$treart=="rogn" & den_ut$rute=="b", predict(M_rowanB, newdata=den_ut), den_ut$biomass)

den_ut$biomass <- ifelse(den_ut$treart=="bjørk" & den_ut$rute=="ub", predict(M_birch2, newdata=den_ut), den_ut$biomass)
den_ut$biomass <- ifelse(den_ut$treart=="bjørk" & den_ut$rute=="b", predict(M_Abirch_B, newdata=den_ut), den_ut$biomass)

den_ut$biomass <- ifelse(den_ut$treart=="lavlandbjørk" & den_ut$rute=="ub", predict(M_birch2, newdata=den_ut), den_ut$biomass)
den_ut$biomass <- ifelse(den_ut$treart=="lavlandbjørk" & den_ut$rute=="b", predict(M_Abirch_B, newdata=den_ut), den_ut$biomass)

den_ut$biomass <- ifelse(den_ut$treart=="einer" & den_ut$rute=="ub", predict(M_pine2, newdata=den_ut), den_ut$biomass)
den_ut$biomass <- ifelse(den_ut$treart=="einer" & den_ut$rute=="b", predict(M_pineBhgt, newdata=den_ut), den_ut$biomass)

den_ut$biomass <- ifelse(den_ut$treart=="selje" & den_ut$rute=="ub", predict(M_rowanUB, newdata=den_ut), den_ut$biomass)
den_ut$biomass <- ifelse(den_ut$treart=="selje" & den_ut$rute=="b", predict(M_rowanB, newdata=den_ut), den_ut$biomass)

den_ut$biomass <- ifelse(den_ut$treart=="ask" & den_ut$rute=="ub", predict(M_rowanUB, newdata=den_ut), den_ut$biomass)
den_ut$biomass <- ifelse(den_ut$treart=="ask" & den_ut$rute=="b", predict(M_rowanB, newdata=den_ut), den_ut$biomass)

den_ut$biomass <- ifelse(den_ut$treart=="gråor" & den_ut$rute=="ub", predict(M_birch2, newdata=den_ut), den_ut$biomass)
den_ut$biomass <- ifelse(den_ut$treart=="gråor" & den_ut$rute=="b", predict(M_birch2, newdata=den_ut), den_ut$biomass)   # using model for unbrowsed birch because aldre is not typically browsed

den_ut$biomass <- ifelse(den_ut$treart=="hegg" & den_ut$rute=="ub", predict(M_rowanUB, newdata=den_ut), den_ut$biomass)
den_ut$biomass <- ifelse(den_ut$treart=="hegg" & den_ut$rute=="b", predict(M_rowanB, newdata=den_ut), den_ut$biomass)

den_ut$biomass <- ifelse(den_ut$treart=="osp" & den_ut$rute=="ub", predict(M_rowanUB, newdata=den_ut), den_ut$biomass)
den_ut$biomass <- ifelse(den_ut$treart=="osp" & den_ut$rute=="b", predict(M_rowanB, newdata=den_ut), den_ut$biomass)

den_ut$biomass <- ifelse(den_ut$treart=="rødhyll" & den_ut$rute=="ub", predict(M_rowanUB, newdata=den_ut), den_ut$biomass)
den_ut$biomass <- ifelse(den_ut$treart=="rødhyll" & den_ut$rute=="b", predict(M_rowanUB, newdata=den_ut), den_ut$biomass)   # this species is also not browsed by moose

den_ut$biomass <- ifelse(den_ut$treart=="lønn" & den_ut$rute=="ub", predict(M_rowanUB, newdata=den_ut), den_ut$biomass)
den_ut$biomass <- ifelse(den_ut$treart=="lønn" & den_ut$rute=="b", predict(M_rowanB, newdata=den_ut), den_ut$biomass)
#tail(den_ut[den_ut$treart=="furu" & den_ut$rute=="ub",])
#tail(den_ut[den_ut$treart=="gran" ,])



# FILTER ##################

levels(den_ut$treart)
D <- den_ut

#levels(D$region)
#levels(D$rute)
#table(D$lokalitetid, D$rute)
D$treart <- droplevels(D$treart)
#table(D1$lokalitetid, D1$flate)
#table(D1$lokalitetid, D1$aar)
#table(D1$lokalitetid, D1$treart)
#table(D1$region, D1$treart)

D1 <- D
setwd("M:\\Anders L Kolstad\\R\\R_projects\\succession_paper")
#write.csv(D1, file="biomass_per_tree.csv", row.names = F)
#save(D1, file="biomass_per_tree.RData")
load("biomass_per_tree.RData")
D1 <- filter(D1,
             region %in% c("telemark", "trøndelag"))
D1$region <- factor(D1$region)
table(D1$region, D1$treart)
D1$treart <- factor(D1$treart)
#table(D1$region, D1$aar)
#table(D1$lokalitetid, D1$rute)

#D1 <- filter(D,
#             rute=="ub")



# ***********************************#
# ***********************************#
##  YEARLY DATA  #########
# ***********************************#
# ***********************************#



#head(D1)
D1$biomass <- as.numeric(D1$biomass)

#levels(D1$flate)
#summary(D1$flate)
D1$flate <- droplevels(D1$flate)

#summary(D1$biomass)
#hist(D1$biomass)
#plot(D1$biomass)
#identify(D1$biomass)
#D1[c(2860, 7310, 7362),]
#D1[D1$lokalitetid==20,]
bio_circle2 <- aggregate(cbind(Biomass_circle = D1$biomass), 
                        by= list(
                                 locationID=D1$lokalitetid, 
                                 trt = D1$rute,
                                 circle=D1$flate, 
                                 year=D1$aar), 
                        FUN = sum, drop=F)
dim(bio_circle2) #  868

bio_circle2$unique <- paste0(bio_circle2$trt, bio_circle2$circle)
library(ggplot2)
ggplot(data = bio_circle2)+
  geom_line(aes(x=year, y=Biomass_circle, group = unique, colour = unique))+
  facet_wrap( ~ locationID, scales = "free")
# most extreme is location 20 in year 2015
# IMPORTANT - if tree height is recorded with a maximum height class (3m) we cannot model biomass over time and perhaps not make reliable productivity indeces?
# the outlier in location 12 is UBØV
#table(bio_circle2$locationID, bio_circle2$year)



bio_plot2 <- aggregate(cbind(Biomass_plot = bio_circle2$Biomass_circle), 
                      by= list(
                               locationID=bio_circle2$locationID, 
                               year=bio_circle2$year,
                               trt=bio_circle2$trt),  
                      FUN = mean, drop=F)
dim(bio_plot2) #217

# standardise biomass per area
bio_plot2$biomass_tonn_ha <- (bio_plot2$Biomass_plot/1000)/(pi*2^2)*10

# add region (nessesarily lost due to 'drop = F' argument). LocationID for Trøndelag is between 11 and 25
bio_plot2$region <- ifelse(bio_plot2$locationID >26, "Telemark", "Trondelag")

ggplot(data = bio_plot2)+
  geom_line(aes(x=year, y=biomass_tonn_ha, group = trt, linetype = trt))+facet_wrap(~ locationID, scales="free")
# many locations have steeper slopes for the open plots, indicating large spatial variation that may even be bigger 
# than the browsing effect. Therefore, to calculate annual biomass increment as a site productivity measure I could
# use the combined biomass for both treatments. This would, however, make browsing pressure part of the index used to test
# itself (circularity) and high moose density areas will get a lower treatment:productivity interaction. But reversly, 
# if the exclusure has highest annual biomass increments, this interaction will become too large.
# I could use either the sum/average of both treatments or the max. The latter I think is best as as it doesn't pull down the
# productivity index for locations with very high browsing pressure, like Bratsberg.


bio_trt <- aggregate(cbind(biomass_tonn_ha = bio_plot2$biomass_tonn_ha), 
                       by= list(
                         year=bio_plot2$year,
                         trt=bio_plot2$trt,
                         region = bio_plot2$region),  
                       FUN = mean)
ggplot(data = bio_trt, aes(x=year, y=biomass_tonn_ha, group = trt, linetype = trt))+
  #geom_line()+
  geom_smooth(method = "lm")


Dwide <- dcast(bio_plot2, locationID+region+trt ~year, 
                value.var = "biomass_tonn_ha", fun.aggregate = mean, na.rm = F)


#Dwide$site <- c("Kalddalbekken",
#                "Svarthovdveien",
#                "Borgsetran",
#                "Bj?ll?a",
#                "Borgan",
#                "Skj?rholmskogen",
#                "V?l?vatnet",
#                "Bratsberg",
#                "Hilmo",
#                "Malvik",
#                "Fl?neset",
#                "Klesetmarka",
#                "Slindsvannet",
#                "Sings?s",
#                "S?terdalsveien")
#Dwide <- arrange(Dwide, Dwide$'2015')


#Dwide <- within(Dwide, '2016' <- bio_plot_excl$biomass_kg_m2[match(Dwide$locationID, bio_plot_excl$locationID)])
#Dwide[Dwide$locationID==20, 8] <- 0.532872 # replacing extreme year with the center value between neighbour years
#0.36790729+((0.69783663-0.36790729)/2)


TrB <- Dwide[Dwide$region == "Trondelag" & Dwide$trt == "b",]
TrUB <- Dwide[Dwide$region == "Trondelag" & Dwide$trt == "ub",]
TeB <- Dwide[Dwide$region == "Telemark" & Dwide$trt == "b",]
TeUB <- Dwide[Dwide$region == "Telemark" & Dwide$trt == "ub",]

combined <- NULL
TrB$annInc_Mg_ha <-              ((TrB$`2015`- TrB$`2014`) +
                                 (TrB$`2014`- TrB$`2013`) +
                                 (TrB$`2013`- TrB$`2012`) +
                                 (TrB$`2012`- TrB$`2011`) +
                                 (TrB$`2011`- TrB$`2010`) +
                                 (TrB$`2010`- TrB$`2009`) ) /6
TrUB$annInc_Mg_ha <-               ((TrUB$`2015`- TrUB$`2014`) +
                                    (TrUB$`2014`- TrUB$`2013`) +
                                    (TrUB$`2013`- TrUB$`2012`) +
                                    (TrUB$`2012`- TrUB$`2011`) +
                                    (TrUB$`2011`- TrUB$`2010`) +
                                    (TrUB$`2010`- TrUB$`2009`) ) /6
TeUB$annInc_Mg_ha <-                 ((TeUB$`2015`- TeUB$`2014`) +
                                      (TeUB$`2014`- TeUB$`2013`) +
                                      (TeUB$`2013`- TeUB$`2012`) +
                                      (TeUB$`2012`- TeUB$`2011`) +
                                      (TeUB$`2011`- TeUB$`2010`)  ) /5
TeB$annInc_Mg_ha <-                    ((TeB$`2015`- TeB$`2014`) +
                                        (TeB$`2014`- TeB$`2013`) +
                                        (TeB$`2013`- TeB$`2012`) +
                                        (TeB$`2012`- TeB$`2011`) +
                                        (TeB$`2011`- TeB$`2010`)  ) /5

combined_Tr <- data.frame(cbind(Region = TrB$region, 
                             locationID = TrB$locationID, 
                             Exclosures_annInc_Mg_ha = TrUB$annInc_Mg_ha, 
                             OpenPlots_annInc_Mg_ha  = as.numeric(TrB$annInc_Mg_ha)))
combined_Tr$Exclosures_annInc_Mg_ha <- as.numeric(as.character(combined_Tr$Exclosures_annInc_Mg_ha))
combined_Tr$OpenPlots_annInc_Mg_ha <- as.numeric(as.character(combined_Tr$OpenPlots_annInc_Mg_ha))
combined_Tr$max_annual_inc_tonns_ha <- ifelse(combined_Tr$Exclosures_annInc_Mg_ha > combined_Tr$OpenPlots_annInc_Mg_ha,
                                              combined_Tr$Exclosures_annInc_Mg_ha , combined_Tr$OpenPlots_annInc_Mg_ha)

combined_Te <- data.frame(cbind(Region = TeB$region, 
                                locationID = TeB$locationID, 
                                Exclosures_annInc_Mg_ha = TeUB$annInc_Mg_ha, 
                                OpenPlots_annInc_Mg_ha  = as.numeric(TeB$annInc_Mg_ha)))
combined_Te$Exclosures_annInc_Mg_ha <- as.numeric(as.character(combined_Te$Exclosures_annInc_Mg_ha))
combined_Te$OpenPlots_annInc_Mg_ha <- as.numeric(as.character(combined_Te$OpenPlots_annInc_Mg_ha))
combined_Te$max_annual_inc_tonns_ha <- ifelse(combined_Te$Exclosures_annInc_Mg_ha > combined_Te$OpenPlots_annInc_Mg_ha,
                                              combined_Te$Exclosures_annInc_Mg_ha , combined_Te$OpenPlots_annInc_Mg_ha)


combined <- rbind(  combined_Tr, combined_Te)
plot(combined$max_annual_inc_tonns_ha)
identify(combined$max_annual_inc_tonns_ha, plot = T) # [1] 10 -> Mlavik (punching error)
boxplot(combined$max_annual_inc_tonns_ha ~ combined$Region) # similar between regions. Region is a random factor
# so I dont need to standardise per region. 
combined$max_annual_inc_tonns_ha <- combined$max_annual_inc_tonns_ha/max(combined$max_annual_inc_tonns_ha) 
summary(combined$max_annual_inc_tonns_ha) # max = 1


productivity <- select(combined, 
                       Region, locationID, productivity = max_annual_inc_tonns_ha)
save(productivity, file="M:\\Anders L Kolstad\\R\\R_projects\\succession_paper\\prod_index_telemark_and_trondelag.RData")
# calculations checked


