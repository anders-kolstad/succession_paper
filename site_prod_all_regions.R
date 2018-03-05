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
with(birch,plot3d(hgt, DGL, biomass, type="s", col=ifelse(birch$treatment=="UB", "blue", "gray"), 
                 xlab="Height (cm)", ylab="Diamater at ground level (mm)",  zlab =  "Biomass (g)"))

with(B_surface,surface3d(unique(hgt),unique(DGL),pp,
                      alpha=0.3, front="lines"))

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
dat4$biomass <- ifelse(dat4$species=="lavlandsbjørk", predict(M_birch, newdata=dat4), dat4$biomass)
dat4$biomass <- ifelse(dat4$species=="spruce", predict(M_spruce2, newdata=dat4), dat4$biomass)
dat4$biomass <- ifelse(dat4$species=="juniper", predict(M_spruce2, newdata=dat4), dat4$biomass)

dat4$biomass <- as.numeric(dat4$biomass)
#summary(dat4$biomass)
hist(dat4$biomass, breaks = 20)
#plot(dat4$biomass)
#identify(dat4$biomass)
#dat4[3373,]
# there's a large spruce at singsås. Would be nice to exclude it from all years, cause its hard to estimate growth on such a alarge tree
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
# ** #  PROTOCOL: Excluding trees above 6 meters (one is 6.27 (Borgan) and the nest is 14m (Singsås))



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
# Max height class is 7 (except in 2014)

den$hgt <- den$skikt*50-25
#summary(den$høyde)


#NEED TO MELT/untable "ANTALL" INTO SEPARATE ROWS
#summary(den$antall_) # max = 216, dvs jeg kan ikke gjøre dcast
#hist(den$antall_, breaks = 20)
den_ut <- untable(df = den, num = den$antall_)

#plot(den_ut$hgt[den_ut$lokalitetid==24 & den_ut$aar==2009]) # the large spruce was only counted in 2016
#plot(den_ut$hgt[den_ut$lokalitetid==15 & den_ut$aar==2013])
#plot(den_ut$hgt[den_ut$lokalitetid==15 & den_ut$aar==2014]) # some big trees here that are not counted in other years
#plot(den_ut$hgt[den_ut$lokalitetid==15 & den_ut$aar==2015]) 

#levels(den_ut$treart)



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
D <- filter(den_ut,
              treart != "ingen",
              treart != "ukjent",
              treart != "klunger",
              treart != "hassel")   # regner klunger og hassel som busker


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
#table(D1$region, D1$treart)
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

#table(bio_circle2$locationID, bio_circle2$year)



bio_plot2 <- aggregate(cbind(Biomass_plot = bio_circle2$Biomass_circle), 
                      by= list(
                               locationID=bio_circle2$locationID, 
                               year=bio_circle2$year),  
                      FUN = mean, drop=F)
dim(bio_plot2) #217
bio_plot2$biomass_kg_m2 <- (bio_plot2$Biomass_plot/1000)/(pi*2^2)

bio_plot2$region <- ifelse(bio_plot2$locationID >26, "Telemark", "Trondelag")


Dwide <- dcast(bio_plot2, locationID+region ~year, 
                value.var = "biomass_kg_m2", fun.aggregate = sum, na.rm = F)


Dwide$site <- c("Kalddalbekken",
                "Svarthovdveien",
                "Borgsetran",
                "Bjøllåa",
                "Borgan",
                "Skjørholmskogen",
                "Vålåvatnet",
                "Bratsberg",
                "Hilmo",
                "Malvik",
                "Fløneset",
                "Klesetmarka",
                "Slindsvannet",
                "Singsås",
                "Sæterdalsveien")
#Dwide <- arrange(Dwide, Dwide$'2015')


#Dwide <- within(Dwide, '2016' <- bio_plot_excl$biomass_kg_m2[match(Dwide$locationID, bio_plot_excl$locationID)])
#Dwide[Dwide$locationID==20, 8] <- 0.532872 # replacing extreme year with the center value between neighbour years
#0.36790729+((0.69783663-0.36790729)/2)



par(mar=c(2,4,3,3))
par(mfrow=c(2,1))
barplot(Dwide$`2015`, names.arg = "", las=2, ylab="Biomass (g)", main= "Standing Biomass 2015 in exclosures")
barplot(Dwide$`2014`, names.arg = Dwide$site, las=2, main="Standing Biomass 2014 in exclosures")

#head(Dwide)

#remove large spruce from Borgan and Singås
# check it , but they were not recorded in other years...


Dwide$annInc_kg_m2 <- ((Dwide$`2016`- Dwide$`2015`) + 
                                 (Dwide$`2015`- Dwide$`2014`) +
                                 (Dwide$`2014`- Dwide$`2013`)+
                                 (Dwide$`2013`- Dwide$`2012`) +
                                 (Dwide$`2012`- Dwide$`2011`) +
                                 (Dwide$`2011`- Dwide$`2010`) +
                                 (Dwide$`2010`- Dwide$`2009`))/7


write.csv(Dwide, file="M:/Anders L Kolstad/systherb data/exported cvs/biomass_trondelag_UB_sites.csv", row.names = F)
# calculations checked
# moving to new script (site priductivty continued) because computer keeps crashing

# **************************************** ##################

#Dwide$PerCentAnnInc <-  (((Dwide$`2016`- Dwide$`2015`)/Dwide$`2016`/(2016-2015)) + 
                         ((Dwide$`2016`- Dwide$`2014`)/Dwide$`2016`/(2016-2014)) +
                         ((Dwide$`2016`- Dwide$`2013`)/Dwide$`2016`/(2016-2013)) +
                         ((Dwide$`2016`- Dwide$`2012`)/Dwide$`2016`/(2016-2012)) +
                         ((Dwide$`2016`- Dwide$`2011`)/Dwide$`2016`/(2016-2011)) +
                         ((Dwide$`2016`- Dwide$`2010`)/Dwide$`2016`/(2016-2010)) +
                         ((Dwide$`2016`- Dwide$`2009`)/Dwide$`2016`/(2016-2009)))/7

# annInc is flawed because of Skjørmolmskogen, but measurement error has low impact
# percent annInc is flawed due to measurement errors

#Dwide[,c(1,2, 12:14)]

#library(dplyr)
#Dwide <- arrange(Dwide, Dwide$'annIncKG_m2')
#par(mfrow=c(1,1), mar=c(8,5,3,3))
#Dwide$annIncKG_m2 <- (Dwide$annInc/1000)/(pi*2^2)*4
#colnames(Dwide)
#barplot(Dwide[,c(15)], names.arg = Dwide$site, las=2, ylab="Biomass (kg/m2)", main= "Average Annual Biomass Increments")
#grid(NA, NULL)

#write.csv(Dwide, file="M:/Anders L Kolstad/systherb data/exported cvs/biomass_trondelag_UB_sites.csv", row.names = F)

###
#temp############
par(mfrow=c(4,2))

plot(birch_B$hgt, birch_B$biomass, xlim=c(0,700), ylim=c(0,4000), 
     xlab="Height (cm)", ylab="Biomass (g)", main="Browsed Birch")
lines(hgtGridB3, birch_ppB3)

plot(birch2016$hgt[birch2016$Treatment=="unbrowsed"], birch2016$pp[birch2016$Treatment=="unbrowsed"], 
     main = "Un-browsed Birch", xlab="Measured height (cm)", ylab="Modelled biomass (g)", 
     ylim=c(0,4000), xlim=c(0,700))
lines(hgtGridB_UB, birch2016ub_p)

plot(pine2$hgt, pine2$biomass,
     main = "Browsed Pine", xlab="Height (cm)", ylab="Biomass (g)", xlim=c(0,350), ylim=c(0,3100))
lines(grid_M_pineBhgt, pred_M_pineBhgt)

plot(pine2016$hgt[pine2016$Treatment=="unbrowsed"], pine2016$pp[pine2016$Treatment=="unbrowsed"], 
     main = "Un-browsed Pine", xlab="Measured height (cm)", ylab="Modelled biomass (g)", 
     ylim=c(0,3100), xlim=c(0,350))
lines(mhgtGrid, pine2016ub_p)


plot(rowanB$hgt, rowanB$biomass, xlim=c(0,420), ylim=c(0,1000), 
     xlab="Height (cm)", ylab="Biomass (g)", main="Browsed Rowan")
lines(hgtGridR2, rowan_ppB)

plot(rowanUB$hgt, rowanUB$biomass, xlim=c(0,420), ylim=c(0,1000), 
     xlab="Height (cm)", ylab="Biomass (g)", main="Un-browsed Rowan")
lines(hgtGridR, rowan_ppUB)

plot(spruce$hgt, spruce$biomass, xlim=c(0,350), ylim=c(0,3600), 
     xlab="Height (cm)", ylab="Biomass (g)", main="Spruce")
lines(hgtGridS, spruce_pp)

##### OLD #####################

Dlong <- melt(Dwide, id.vars = c("lokalitetid", "arskogbon", "region", "annIncKG_m2", "site", "annInc"), variable_name  = "year", value.name = "bm")
colnames(Dlong)[8] <- "biomass"



# PLOT increases  ##############
plot(as.numeric(Dlong$year), Dlong$biomass)
identify(as.numeric(Dlong$year), Dlong$biomass)

par(mfrow=c(2,2))
plot(as.numeric(Dlong$year[Dlong$lokalitetid == 11]), Dlong$biomass[Dlong$lokalitetid == 11], main="Kalddalbekken") # good
plot(as.numeric(Dlong$year[Dlong$lokalitetid == 12]), Dlong$biomass[Dlong$lokalitetid == 12], main="Svarthovdveien") # very large variation! Check!
# I checked, and the sample trees accumulated hgt is always increasing. The problem lies in the density data.
# It seem like there is a multistemmed sample birch which some years have been counted as one stem and other year as several

plot(as.numeric(Dlong$year[Dlong$lokalitetid == 13]), Dlong$biomass[Dlong$lokalitetid == 13], main="Borgsetran") # not perfectly linear, but high r2
plot(as.numeric(Dlong$year[Dlong$lokalitetid == 14]), Dlong$biomass[Dlong$lokalitetid == 14], main="Bjøllåa") # good
plot(as.numeric(Dlong$year[Dlong$lokalitetid == 15]), Dlong$biomass[Dlong$lokalitetid == 15], main="Borgan") # one very off year
plot(as.numeric(Dlong$year[Dlong$lokalitetid == 16]), Dlong$biomass[Dlong$lokalitetid == 16], main="Skjørholskogen") # good
plot(as.numeric(Dlong$year[Dlong$lokalitetid == 17]), Dlong$biomass[Dlong$lokalitetid == 17], main="Vålåvatnet") # good. one off year
plot(as.numeric(Dlong$year[Dlong$lokalitetid == 18]), Dlong$biomass[Dlong$lokalitetid == 18], main="Bratsberg") # good
plot(as.numeric(Dlong$year[Dlong$lokalitetid == 19]), Dlong$biomass[Dlong$lokalitetid == 19], main="Hilmo") # good
plot(as.numeric(Dlong$year[Dlong$lokalitetid == 20]), Dlong$biomass[Dlong$lokalitetid == 20], main="Malvik") # nice example of what I'm trying to do
plot(as.numeric(Dlong$year[Dlong$lokalitetid == 21]), Dlong$biomass[Dlong$lokalitetid == 21], main="Fløneset") # good
plot(as.numeric(Dlong$year[Dlong$lokalitetid == 22]), Dlong$biomass[Dlong$lokalitetid == 22], main="Klesetmarka") # not perfectly linear
plot(as.numeric(Dlong$year[Dlong$lokalitetid == 23]), Dlong$biomass[Dlong$lokalitetid == 23], main="Slindsvannet") # good
plot(as.numeric(Dlong$year[Dlong$lokalitetid == 24]), Dlong$biomass[Dlong$lokalitetid == 24], main="Singsås") # ok
plot(as.numeric(Dlong$year[Dlong$lokalitetid == 25]), Dlong$biomass[Dlong$lokalitetid == 25], main="Sæterdalsveien") # good


# MODELS (LM) ############
m11 <- lm(D5long$biomass~as.numeric(D5long$year), subset = D5long$lokalitetid == 11)
m12 <- lm(D5long$biomass~as.numeric(D5long$year), subset = D5long$lokalitetid == 12)
m12x <- lm(D5long$biomass~as.numeric(D5long$year), subset = D5long$lokalitetid == 12 & D5long$year!="2009" & D5long$year!="2012")
m13 <- lm(D5long$biomass~as.numeric(D5long$year), subset = D5long$lokalitetid == 13)
m14 <- lm(D5long$biomass~as.numeric(D5long$year), subset = D5long$lokalitetid == 14)
m15 <- lm(D5long$biomass~as.numeric(D5long$year), subset = D5long$lokalitetid == 15)
m15x <- lm(D5long$biomass~as.numeric(D5long$year), subset = D5long$lokalitetid == 15 & D5long$year!="2013")
m16 <- lm(D5long$biomass~as.numeric(D5long$year), subset = D5long$lokalitetid == 16)
m17 <- lm(D5long$biomass~as.numeric(D5long$year), subset = D5long$lokalitetid == 17)
m18 <- lm(D5long$biomass~as.numeric(D5long$year), subset = D5long$lokalitetid == 18)
m19 <- lm(D5long$biomass~as.numeric(D5long$year), subset = D5long$lokalitetid == 19)
m20 <- lm(D5long$biomass~as.numeric(D5long$year), subset = D5long$lokalitetid == 20)
m20x <- lm(D5long$biomass~as.numeric(D5long$year), subset = D5long$lokalitetid == 20 & D5long$year!="2015")
m21 <- lm(D5long$biomass~as.numeric(D5long$year), subset = D5long$lokalitetid == 21)
m22 <- lm(D5long$biomass~as.numeric(D5long$year), subset = D5long$lokalitetid == 22)
m23 <- lm(D5long$biomass~as.numeric(D5long$year), subset = D5long$lokalitetid == 23)
m24 <- lm(D5long$biomass~as.numeric(D5long$year), subset = D5long$lokalitetid == 24)
m25 <- lm(D5long$biomass~as.numeric(D5long$year), subset = D5long$lokalitetid == 25)

# Weighed regression ##################
# the beta is more sensitive to extreme values at either end, so these could be weighed down (by how much?)
# weighing different values of the explanatory variable inherently reduces R2, but thats OK. The justification
# for weighing needs to be theoretical and a priory

D5long$nYear <- as.numeric(D5long$year)
D5long$weights <- length(unique(D5long$nYear))-1-abs(D5long$nYear-mean(unique(D5long$nYear))) 
# weighing down low and high years by max 50%

m11w <- lm(D5long$hgt~as.numeric(D5long$year), subset = D5long$lokalitetid == 11, weights = D5long$weights)
m12w <- lm(D5long$hgt~as.numeric(D5long$year), subset = D5long$lokalitetid == 12, weights = D5long$weights)
m12xw <- lm(D5long$hgt~as.numeric(D5long$year), subset = D5long$lokalitetid == 12 & D5long$year!="2009" & D5long$year!="2012", weights = D5long$weights)
m13w <- lm(D5long$hgt~as.numeric(D5long$year), subset = D5long$lokalitetid == 13, weights = D5long$weights)
m14w <- lm(D5long$hgt~as.numeric(D5long$year), subset = D5long$lokalitetid == 14, weights = D5long$weights)
m15w <- lm(D5long$hgt~as.numeric(D5long$year), subset = D5long$lokalitetid == 15, weights = D5long$weights)
m15xw <- lm(D5long$hgt~as.numeric(D5long$year), subset = D5long$lokalitetid == 15 & D5long$year!="2013",weights = D5long$weights)
m16w <- lm(D5long$hgt~as.numeric(D5long$year), subset = D5long$lokalitetid == 16, weights = D5long$weights)
m17w <- lm(D5long$hgt~as.numeric(D5long$year), subset = D5long$lokalitetid == 17, weights = D5long$weights)
m18w <- lm(D5long$hgt~as.numeric(D5long$year), subset = D5long$lokalitetid == 18, weights = D5long$weights)
m19w <- lm(D5long$hgt~as.numeric(D5long$year), subset = D5long$lokalitetid == 19, weights = D5long$weights)
m20w <- lm(D5long$hgt~as.numeric(D5long$year), subset = D5long$lokalitetid == 20, weights = D5long$weights)
m21w <- lm(D5long$hgt~as.numeric(D5long$year), subset = D5long$lokalitetid == 21, weights = D5long$weights)
m22w <- lm(D5long$hgt~as.numeric(D5long$year), subset = D5long$lokalitetid == 22, weights = D5long$weights)
m23w <- lm(D5long$hgt~as.numeric(D5long$year), subset = D5long$lokalitetid == 23, weights = D5long$weights)
m24w <- lm(D5long$hgt~as.numeric(D5long$year), subset = D5long$lokalitetid == 24, weights = D5long$weights)
m25w <- lm(D5long$hgt~as.numeric(D5long$year), subset = D5long$lokalitetid == 25, weights = D5long$weights)


# robust models ###############
# Huberts, k=1.5
library(MASS)
m11r <- rlm(D5long$hgt~as.numeric(D5long$year), subset = D5long$lokalitetid == 11, k=1.5)

sdm11r <- sd((resid(m11r)))
round(resid(m11r)/sdm11r, 2) 
# this show the sd distance of all residuals. K value is the threshold for how large residuals 
# need to before they are penalized
# here x=4 is the most extreme, but its not very far over the k value of 1.5



#m12r <- lm(D5long$hgt~as.numeric(D5long$year), subset = D5long$lokalitetid == 12, k=1.5)
#sdm12r <- sd((resid(m12r)))
#round(resid(m12r)/sdm12r, 2) 
# the residual sd is so large the model fails! I need to manually remove thos points.
m12r <- rlm(D5long$hgt~as.numeric(D5long$year), 
            subset = D5long$lokalitetid == 12 & D5long$year!="2009" & D5long$year!="2012", k=1.5)
sdm12r <- sd((resid(m12r)))
round(resid(m12r)/sdm12r, 2)  #noe above k value

m13r <- rlm(D5long$hgt~as.numeric(D5long$year), subset = D5long$lokalitetid == 13, k=1.5)
sdm13r <- sd((resid(m13r)))
round(resid(m13r)/sdm13r, 2) # noe above


m15r <- rlm(D5long$hgt~as.numeric(D5long$year), subset = D5long$lokalitetid == 15, k=1.5)
sdm15r <- sd((resid(m15r)))
round(resid(m15r)/sdm15r, 2)
m15r$w # the fifth data value was weighed down 52%
m15r$psi

# ABLINE #################
abline(m11)
abline(m11w, lty=2)
abline(m11r)

#abline(m12)
abline(m12x, lty = 1)
#abline(m12w, lty = 3)
abline(m12xw, lty = 2)
abline(m12r)

abline(m13)
abline(m13w, lty=2)

abline(m14)
abline(m14w, lty=2)

abline(m15)
abline(m15x, lty=2)
#abline(m15w, lty=3)   # with very large error in the middle, weighed models make things worse
#abline(m15xw)
abline(m15r)

abline(m16)
abline(m16w, lty=2)

abline(m17)
abline(m17w, lty=2)

abline(m18)
abline(m18w, lty=2)

abline(m19)
abline(m19w, lty=2)

abline(m20)
abline(m20x, lty = 2)
abline(m20w, lty = 3) # think the weighed is better, but signs of accelerating trend

abline(m21)
abline(m21w, lty=2)

abline(m22)
abline(m22w, lty=2)

abline(m23)
abline(m23w, lty =2)

abline(m24)
abline(m24w, lty=2)

abline(m25)
abline(m25w, lty=2)


# SUMMARY ####################
summary(m11) # r2=0.99
summary(m11w)

summary(m12) # r2 = 0.00 !!
summary(m12x) # r2= 0.79   removed two years with different approaches to multistememd birch
summary(m12w)
summary(m12xw)

summary(m13)
summary(m14)
summary(m15)  # r2 = 0.83
summary(m15x) # r2 = 0.97
summary(m16)
summary(m17)
summary(m18)
summary(m19)
summary(m20)
summary(m20x)
summary(m21)
summary(m22)
summary(m23)
summary(m24)
summary(m25)


# ADD BETA to DF #############
# I think I will change over to use the weighed models...
D5wide$beta <- c(m11$coefficients[2],
                 m12x$coefficients[2],
                 m13$coefficients[2],
                 m14$coefficients[2],
                 m15x$coefficients[2],
                 m16$coefficients[2],
                 m17$coefficients[2],
                 m18$coefficients[2],
                 m19$coefficients[2],
                 m20x$coefficients[2],
                 m21$coefficients[2],
                 m22$coefficients[2],
                 m23$coefficients[2],
                 m24$coefficients[2],
                 m25$coefficients[2])


arrange(D5wide, beta)
# loc 16 Skjærholmskogen should be more productive, but the circles have very few trees by chance,
#     however, that shoulcn't effec the rate of change...
# loc 13 Borgsætran has bonitet14 but here it comes out as medium which I think is more correct
# same for loc 17 Vålåvatnet - its not that productive
# loc 14 Bjøllåa is in fact more productive that this index shows
# loc 22 Klesetmarka comes out as more productive than it is I think



# OLD ##################

colnames(D5wide)[6:12] <- c("y09", "y10", "y11", "y12", "y13", "y14", "y15")   # pass på at colonnene har rett nummer
D5wide$incr10 <- ((D5wide$'2010'-D5wide$'2009')/D5wide$'2009')*100 
D5wide$incr11 <- ((D5wide$'2011'-D5wide$'2010')/D5wide$'2010')*100
D5wide$incr12 <- ((D5wide$'2012'-D5wide$'2011')/D5wide$'2011')*100
D5wide$incr13 <- ((D5wide$'2013'-D5wide$'2012')/D5wide$'2012')*100
D5wide$incr14 <- ((D5wide$'2014'-D5wide$'2013')/D5wide$'2013')*100
D5wide$incr15 <- ((D5wide$'2015'-D5wide$'2014')/D5wide$'2014')*100

barplot(colMeans(D5wide[,-c(1:10)]))
# %inc is not stable

# standardizing according to the first year
D5wide$s10 <- D5wide$'2010' - D5wide$'2009'
D5wide$s11 <- D5wide$'2011' - D5wide$'2009'
D5wide$s12 <- D5wide$'2012' - D5wide$'2009'
D5wide$s13 <- D5wide$'2013' - D5wide$'2009'
D5wide$s14 <- D5wide$'2014' - D5wide$'2009'
D5wide$s15 <- D5wide$'2015' - D5wide$'2009'
barplot(colMeans(D5wide[,-c(1:10)]))
# growth is quite linear. I think I prefer this accumulated growth approach.
# The method is quite sensitive to estimates doen the first year (2009):
# locID 12 had lots og biomass (height) the first year but then decreased.
# locID 22 had very little the first year and so now ssems to have increased drastically

# calculating biomass inc per standardized year
D5wide$s11 <- D5wide$'s11' - D5wide$'s10'
D5wide$s12 <- D5wide$'s12' - D5wide$'s11'
D5wide$s13 <- D5wide$'s13' - D5wide$'s12'
D5wide$s14 <- D5wide$'s14' - D5wide$'s13'
D5wide$s15 <- D5wide$'s15' - D5wide$'s14'
colnames(D5wide)
barplot(colMeans(D5wide[,-c(1:11)]))
# accelerating effect - large trees grow more per year - obvious

#D6 <- melt(D4, id.vars = c("loc", "bon"))
#plot(as.numeric(D6$variable), D6$value, type = "p")

D4$avg <- (D4$incr11+D4$incr12+D4$incr13+D4$incr14+D4$incr15)/5
D4 <- arrange(D4, desc(avg))

barplot(D4$avg, D4$loc, width = 1, space = 0, ylab = "annual % increments", 
        xlab = "locality ID")

boxplot(D4$avg~D4$bon)
plot(D4$avg~as.numeric(D4$bon))


# strange things:
# Setesdalsveien (loc=25) is in reality one of the least productive sites...
# and site 16 is high to moderate productive
# Singsår (24) comes out in the middle
table(D3$lokalitetid) # site 25 has had 38 sample trees up through the years
D3[D3$lokalitetid==25,] # and most of them are still quite small

table(D3$lokalitetid) # site 16 has only 8 rows
D3[D3$lokalitetid==16,] # which means there's very few data points to base an estimate on

# it could correlct itself when using biomass instead if height