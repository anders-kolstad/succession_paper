# Top ####
# Script for forest succession paper Anders L Kolstad

# Overview

# Want to show tree heigth and density
 # problem that max height is 3 m. This makes it impossible to use heigth or biomass

#  Solutoin, Age distribution over time
 # vertical plot with exclosure to one side and open plots on the other. 


# Library #####
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(reshape2)


# data ####
getwd()
setwd("/home/anders/diskF/Anders L Kolstad/R/R_projects/succession_paper")
density <- read_excel("density.xlsx", sheet = "Sheet1")

# housekeeping
density$Region <- as.factor(density$Region)
density$Treatment <- as.factor(density$Treatment)
density$Height_cm <- as.numeric(density$Height_cm)
density$Height_class_50cm <- as.numeric(density$Height_class_50cm)
density$Quantity <- as.numeric(density$Quantity)
density$LocalityName[density$LocalityName=="Stig Dæhlen"] <- "Stig Daehlen"
density$LocalityName[density$LocalityName=="Sørum 1"] <- "Sorum 1"
density$date <- as.Date(density$`_Date`, format = c("%d/%m/%Y"))
density$year <- format(density$date, "%Y")
density$h_class <- density$Height_class_50cm


# calculating year since exclosure (yse)
density$yse <- ""
density$yse <- ifelse(density$Region == "Trøndelag", as.numeric(density$year)-2008, density$yse)
density$yse <- ifelse(density$Region == "Telemark", as.numeric(density$year)-2009, density$yse)


Siteinfo_akershus <- read_excel("Siteinfo_akershus15082016.xlsx",   sheet = "Ark1")
density$trt_start <- Siteinfo_akershus$Exclosure_month_year[match(density$LocalityName, Siteinfo_akershus$LocalityName)]
density$yse <- ifelse(density$Region == "Hedmark", as.numeric(density$year)-density$trt_start, density$yse)
density$yse <- as.numeric(density$yse)
rm(Siteinfo_akershus)

summary(density$yse)
table(density$Region, density$yse)
table(density$LocalityName, density$yse)
# Hedmark has variable duration. Makes it confusing. Excluding that region



# subsetting data
levels(density$Region)
regions <- c("Trøndelag", "Telemark")
density <- density %>% 
  filter(Region %in% regions)
density$Region <- factor(density$Region)

# h_class housekeeping
summary(density$h_class)
#View(density[is.na(density$h_class),])
# easy solution
density$h_class[is.na(density$h_class)] <- 3

# better solution to the above, but didn't work for some reason...
#library(data.table)
#if(density$Height_cm[is.finite(density$Height_cm)] %between% c(100,150)) {
#  density$h_class[is.finite(density$Height_cm)] <- 3
#}
  
#hist(density$h_class)
plot(density$h_class) # height class 0 shouldn't exist
#View(density[density$h_class==0,]) # empty rows
densityX <- density
density <- densityX[densityX$h_class!=0,]
rm(densityX)
#plot(density$Height_cm)


# max value for the height category is 7, but when height was recorded in cm there's no limit.
# forcing max value = 7
density$h_class2 <- density$h_class
density$h_class2[density$h_class2 > 7] <- 7
hist(density$h_class2)
plot(density$h_class2)

density$fH <- as.factor(density$h_class2)
levels(density$fH)
unique(density$LocalityName) #31 locations


# Standardising Quantity To mean number of trees per hectare (10.000 m2)
density$Q_circle <- density$Quantity
density$Quantity <- density$Quantity/(pi*4)*10000


#table(density$fH, density$Region)
#table(density$fH, density$LocalityName)

density$fTaxa <- as.factor(density$Taxa)
levels(density$fTaxa)
table(density$fTaxa)

table(density$fTaxa, density$yse)
tapply(density$Q_circle, list(density$yse, density$Taxa, density$Treatment), FUN = sum)
# 1015 rowan counted in open plot year 1
# 1136 for exclosures
temp <- density[density$Taxa=="Sorbus aucuparia (Rogn)" & density$yse == 7,]
tapply(temp$Q_circle, list(temp$Treatment, temp$fH), FUN = sum)
rm(temp)
# in year 7 the talles rowan was the single individual in height call 4 (150-200 cm)
593/(593 +170+  17+   1)
#75.9% of individuals in class 1 after 7 years

# Compared to 
130+104+44+24
# 302 rown in excloser reaching this height or greater. 
(130+104+44+24)/(217+ 204+ 167+ 130+ 104+ 44+ 24)  #33.9%
217/(217+ 204+ 167+ 130+ 104+ 44+ 24)  #24.4%
# ************************************#
# Canopy dominance  ####
# % decidious trees in the canopy
# Canopy defined as the two top height classes at any given time
# ************************************#
RAT <- density
#RAT$nH <- as.numeric(as.character(RAT$fH))
#head(RAT)
#RAT <- RAT[RAT$nH>5,]


# sum of stems per circle
RAT <- aggregate(data = RAT,
                 Quantity~Region+LocalityName+Treatment+Plot+yse+Taxa+fH,
                 FUN = sum, drop = T) 


RAT <- RAT[RAT$yse < 8,]
RAT <- RAT[RAT$Taxa != "No occurrence (Ingen)",]

# remove remnant trees (as best we can):
RATcast <- dcast(data = RAT, Taxa+Region+LocalityName+Plot+Treatment+fH~yse, value.var = "Quantity", fun.aggregate = sum) 

RATcast2 <- RATcast
RATcast <- RATcast[RATcast$fH== "7",]
RATcast$fH <- factor(RATcast$fH)

for(i in 8:ncol(RATcast)){
  RATcast[,i] <- RATcast[,i]-RATcast[,7]
}

RATcast$`1` <- 0  # No 3m trees in year 1
RATcast3 <- rbind(RATcast2[RATcast2$fH!="7",], RATcast)


RAT <- melt(data=RATcast3, id.vars = c("Taxa", "Region", "LocalityName", "Plot", "Treatment", "fH"),
               measure.vars = c('1','2','3','4','5','6','7'), variable.name = "yse", value.name = "Quantity")
RAT$Quantity[RAT$Quantity<0] <- 0
RAT <- RAT[RAT$Quantity!=0,]

table(RAT$fH, RAT$yse)



RAT$DecOrCon <- ifelse(   RAT$Taxa == "Betula pendula (Lavlandbjørk)" |
                          RAT$Taxa == "Betula pubescens (Bjørk)" |
                          RAT$Taxa == "Populus tremula (Osp)" |
                          RAT$Taxa == "Salix caprea (Selje)" |
                          RAT$Taxa == "Sorbus aucuparia (Rogn)", "decidious", "coniferous")


  
RAT2 <- aggregate(data = RAT, Quantity~Region+LocalityName+Plot+Treatment+fH+yse+DecOrCon,
                  FUN = sum)
RAT3 <- dcast(data = RAT2,
              Region+LocalityName+Plot+Treatment+yse+fH~DecOrCon,
              value.var = "Quantity",
              fun.aggregate = sum)



# Now I want to keep only the two highest growth categories for each plot:time
RAT3$plot_time <- paste(RAT3$LocalityName, RAT3$Treatment, RAT3$Plot, RAT3$yse, sep="_")
length(unique(RAT3$plot_time))

RAT3$nH <- as.numeric(as.character(RAT3$fH))
tempDat <- RAT3[order(-RAT3$nH),]

df <- NULL

for(i in unique(RAT3$plot_time)){
  tempDat2  <- tempDat[tempDat$plot_time==paste(i),]
  ifelse(nrow(tempDat2)>1,  tempDat3 <- tempDat2[1:2,], tempDat3 <- tempDat2[1,])
  ifelse(tempDat3$nH[1] - tempDat3$nH[2] > 1, tempDat3 <- tempDat3[1,], tempDat3)
  df        <- rbind(df, tempDat3) 
}
table(df$LocalityName, df$yse)   # seems as if some sites dont have many trees at all...


# option B, with canopy = largest height class only
#for(i in unique(RAT3$plot_time)){
#  tempDat2  <- tempDat[tempDat$plot_time==paste(i),]
#  tempDat3 <- tempDat2[1,]
#  df        <- rbind(df, tempDat3) 
#}

#View(RAT3[RAT3$LocalityName=="Drangedal3" & RAT3$yse=="1",])
#table(RAT3$LocalityName, RAT3$yse)
#table(RAT2$LocalityName, RAT2$yse)
#table(RAT$LocalityName, RAT$yse)
    # seems to be the case

RATagg <- aggregate(cbind("Coniferous" = df$coniferous, "Decidious" = df$decidious), 
                    by= list("Region" = df$Region,
                             "LocalityName" = df$LocalityName,
                             "Plot" = df$Plot,
                             "Treatment" = df$Treatment,
                             "yse" = df$yse,
                             "plot_time" = df$plot_time), FUN = mean)

RATagg$decidiousness <- (RATagg$Decidious/(RATagg$Decidious+RATagg$Coniferous))*100   
summary(RATagg$decidiousness)



RAT_plot <- aggregate(data = RATagg, decidiousness~Treatment+yse, FUN = mean, na.rm=T)
se <- function(x) sd(x, na.rm=T)/sqrt(length(x))
RAT_plot$SE <- aggregate(data = RATagg, decidiousness~Treatment+yse, FUN = se)[,"decidiousness"]
RAT_plot$yse <- as.numeric(as.character(RAT_plot$yse))



pd <- position_dodge(width=0.4)

canopyDom <- ggplot(data=RAT_plot , aes(y=decidiousness, x=yse, group=Treatment, linetype=Treatment))+
  geom_line(size=1, position = pd)+
  geom_point(size=3, position = pd, aes(shape=Treatment))+
  geom_errorbar(aes(ymax = decidiousness+SE, ymin = decidiousness-SE, linetype=NULL), size=1.1, width=0.5, position=pd)+
  theme_bw()+
  ylab("% deciduous trees in the canopy")+
  scale_x_continuous(name="Years since exclusion", breaks=c(1,3,5,7))+
  scale_linetype_manual(breaks = c("B", "UB"), labels = c("Open plots", "Exclosures"), values=c(1,2))+
  scale_shape_manual(breaks = c("B", "UB"), labels = c("Open plots", "Exclosures"), values=c(16,17))+
  guides(linetype = F, shape = F)
  #theme(legend.justification = c(0.01, 0.01), 
   #     legend.position = c(0.01, 0.01),
    #    #legend.background = element_blank(),
     #   legend.key.width = unit(3,"line"),
      #  legend.text=element_text(size=8),
       # legend.title = element_blank())

#tiff("decidiousness.tiff", height = 15, width = 10, units = "cm", res = 300)
canopyDom
dev.off()
RAT_plot[RAT_plot$yse==7,]
 
  #Treatment yse decidiousness       SE
  #13         B   7      37.40404 3.598741
  #14        UB   7      60.53638 3.578768


# Looking at the differences between regions
RAT_plot2     <- aggregate(data = RATagg, decidiousness~Region+LocalityName+Treatment+yse, FUN = mean, na.rm=T)
RAT_plot3 <- aggregate(data = RATagg, decidiousness~Region+Treatment+yse, FUN = mean, na.rm=T)
RAT_plot3$SE  <- aggregate(data = RATagg, decidiousness~Region+Treatment+yse, FUN = se)[,"decidiousness"]

ggplot(data=RAT_plot3 , aes(y=decidiousness, x=yse, group=Treatment, linetype=Treatment))+
  geom_line(size=1, position = pd)+
  geom_point(size=3, position = pd, aes(shape=Treatment))+
  geom_errorbar(aes(ymax = decidiousness+SE, ymin = decidiousness-SE, linetype=NULL), size=1.1, width=0.5, position=pd)+
  theme_bw()+
  ylab("% Decidiousness")+
  xlab("Years since exclosure")+
  scale_linetype_manual(breaks = c("B", "UB"), labels = c("Open plots", "Exclosures"), values=c(1,2))+
  scale_shape_manual(breaks = c("B", "UB"), labels = c("Open plots", "Exclosures"), values=c(16,17))+
  theme(legend.justification = c(0.01, 0.99), 
        legend.position = c(0.01, 0.99),
        #legend.background = element_blank(),
        legend.key.width = unit(3,"line"),
        legend.text=element_text(size=8),
        legend.title = element_blank())+
  facet_wrap(~Region)


# Interaction plot
RAT_plot4 <- RAT_plot2[RAT_plot2$yse == "7",]
RAT_plot4 <- dcast(data = RAT_plot4, LocalityName~Treatment, value.var = "decidiousness")
RAT_plot4$diff <- RAT_plot4$UB-RAT_plot4$B

# Adding productivity
setwd("M:\\Anders L Kolstad\\R\\R_projects\\succession_paper")
load("prod_index_telemark_and_trondelag.RData")
RAT_plot4$productivity <- productivity$productivity[match(RAT_plot4$LocalityName, productivity$LocalityName)]


RAT_INT <- ggplot(data = RAT_plot4, aes(x = productivity, y = diff))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0, size=2, colour="grey")+
  geom_point(size = 3, stroke = 2, shape =1)+
  theme(strip.text.x = element_blank())+
  ylab("% deciduous trees in the canopy")+
  scale_x_continuous(name="Site productivity", breaks=c(0.25,0.5,0.75))



#  -> LMM ####
# Adding productivity
setwd("M:\\Anders L Kolstad\\R\\R_projects\\succession_paper")
load("prod_index_telemark_and_trondelag.RData")
RATmdat <- RATagg
RATmdat <- RATmdat[RATmdat$yse=="7",]
RATmdat$productivity <- productivity$productivity[match(RATmdat$LocalityName, productivity$LocalityName)]

plot(RATmdat$decidiousness)

library(lmerTest)
RATIOlmm <- lmerTest::lmer(decidiousness~Treatment*productivity + (1|Region/LocalityName), 
                        data = RATmdat, REML = F)
plot(RATIOlmm) # tja
plot(RATmdat$productivity, resid(RATIOlmm))   # ok
plot(RATmdat$Treatment, resid(RATIOlmm))      # ok
plot(as.numeric(RATmdat$Treatment), resid(RATIOlmm))    

summary(RATIOlmm)

RATIOlmm_add <- update(RATIOlmm, .~. -Treatment:productivity)
RATIOlmm_p <- update(RATIOlmm_add, .~. -Treatment)
RATIOlmm_t <- update(RATIOlmm_add, .~. -productivity)
AIC(RATIOlmm, RATIOlmm_add, RATIOlmm_p, RATIOlmm_t)
AIC(RATIOlmm)-AIC(RATIOlmm_add)
AIC(RATIOlmm)-AIC(RATIOlmm_p)
AIC(RATIOlmm)-AIC(RATIOlmm_t)



# removing interaction:
anova(RATIOlmm, RATIOlmm_add) #3.3652      1    0.06659 .

# I think I'll use the exclusion only  model
AIC(RATIOlmm_t)-AIC(RATIOlmm)
AIC(RATIOlmm_t)-AIC(RATIOlmm_p)
AIC(RATIOlmm_t)-AIC(RATIOlmm_add)

# refit with REML
RATIOlmm_t <- lmerTest::lmer(decidiousness~Treatment + (1|Region/LocalityName), 
                           data = RATmdat, REML = T)
plot(RATIOlmm_t) # tja
plot(RATmdat$productivity, resid(RATIOlmm_t))   # ok
plot(RATmdat$Treatment, resid(RATIOlmm_t))      # ok
summary(RATIOlmm_t) # 214.220   5.110 7.12e-07 ***



ICCr <- 2.006e-11/( 3.319e+02+2.006e-11+1.260e+03)
ICCl <-  3.319e+02/( 3.319e+02+2.006e-11+1.260e+03)



# subsetting different species before aggregating to reduce data size

SA <- density[density$fTaxa == "Sorbus aucuparia (Rogn)",]
PA <- density[density$fTaxa == "Picea abies (Gran)",]
PS <- density[density$fTaxa == "Pinus sylvestris (Furu)",]
BP <- density[density$fTaxa == "Betula pendula (Lavlandbjørk)" | density$fTaxa == "Betula pubescens (Bjørk)",]
SC <- density[density$fTaxa == "Salix caprea (Selje)",]



# shold I remove locations where the species is very rare? What is rare?
# Defining rare as below 5 records over 7-9 years
#par(mar=c(12,5,2,2))


SC.sub <- aggregate(data = SC,
                    Q_circle~LocalityName,
                   FUN = sum)
BP.sub <- aggregate(data = BP,
                    Q_circle~LocalityName,
                    FUN = sum)
PS.sub <- aggregate(data = PS,
                    Q_circle~LocalityName,
                    FUN = sum)
PA.sub <- aggregate(data = PA,
                    Q_circle~LocalityName,
                    FUN = sum)
SA.sub <- aggregate(data = SA,
                    Q_circle~LocalityName,
                    FUN = sum)

#SC.sub$fLoc <- as.factor(SC.sub$LocalityName)

#plot(Q_circle~as.factor(LocalityName), las=2, type= "n", data = SC.sub)
#text(as.factor(SC.sub$LocalityName), SC.sub$Q_circle, label=SC.sub$Q_circle)
#plot(Q_circle~as.factor(LocalityName), las=2, type= "n", data = BP.sub)
#text(as.factor(BP.sub$LocalityName), BP.sub$Q_circle, label=BP.sub$Q_circle)
#plot(Q_circle~as.factor(LocalityName), las=2, type= "n", data = PS.sub)
#text(as.factor(PS.sub$LocalityName), PS.sub$Q_circle, label=PS.sub$Q_circle)
#plot(Q_circle~as.factor(LocalityName), las=2, type= "n", data = PA.sub)
#text(as.factor(PA.sub$LocalityName), PA.sub$Q_circle, label=PA.sub$Q_circle)
#plot(Q_circle~as.factor(LocalityName), las=2, type= "n", data = SA.sub)
#text(as.factor(SA.sub$LocalityName), SA.sub$Q_circle, label=SA.sub$Q_circle)


# unique(density$LocalityName) 
SC.remove <- SC.sub$LocalityName[SC.sub$Q_circle < 6]
PS.remove <- PS.sub$LocalityName[PS.sub$Q_circle < 6]
SA.remove <- SA.sub$LocalityName[SA.sub$Q_circle < 6]

SC <- SC[!SC$LocalityName %in% SC.remove, ]
PS <- PS[!PS$LocalityName %in% PS.remove, ]
SA <- SA[!SA$LocalityName %in% SA.remove, ]

unique(SC$LocalityName)                         # 18 locations
unique(SC$LocalityName[SC$Region=="Trøndelag"]) # 11 locations
unique(SC$LocalityName[SC$Region=="Telemark"])  # 7 locations
#unique(SC$LocalityName[SC$Region=="Hedmark"])   # 10 locations

unique(BP$LocalityName) # 31 locations
unique(BP$LocalityName[BP$Region=="Trøndelag"]) # 15 locations
unique(BP$LocalityName[BP$Region=="Telemark"])  # 16 locations
#unique(BP$LocalityName[BP$Region=="Hedmark"])   # 16 locations

unique(PS$LocalityName)                         # 26 locations
unique(PS$LocalityName[PS$Region=="Trøndelag"]) # 12 locations
unique(PS$LocalityName[PS$Region=="Telemark"])  # 14 locations
#unique(PS$LocalityName[PS$Region=="Hedmark"])   # 14 locations

unique(PA$LocalityName) # 31 locations

unique(SA$LocalityName) # 30 locations
unique(SA$LocalityName[SA$Region=="Trøndelag"]) # 15 locations
unique(SA$LocalityName[SA$Region=="Telemark"])  # 15 locations
#unique(SA$LocalityName[SA$Region=="Hedmark"])   # 13 locations

rm(SC.sub, PA.sub, PS.sub, BP.sub, SA.sub)


# creating  big datasets # filling in the zeros with aggregate (drop = F)
SA2 <- aggregate(data = SA,
                      Quantity~LocalityName+Treatment+Plot+yse+Taxa+fH,
                      FUN = sum, drop = F) 
PA2 <- aggregate(data = PA,
                 Quantity~LocalityName+Treatment+Plot+yse+Taxa+fH,
                 FUN = sum, drop = F)
PS2 <- aggregate(data = PS,
                 Quantity~LocalityName+Treatment+Plot+yse+Taxa+fH,
                 FUN = sum, drop = F)
BP2 <- aggregate(data = BP,
                 Quantity~LocalityName+Treatment+Plot+yse+Taxa+fH,
                 FUN = sum, drop = F)
SC2 <- aggregate(data = SC,
                 Quantity~LocalityName+Treatment+Plot+yse+Taxa+fH,
                 FUN = sum, drop = F)
#table(SA2$LocalityName)
#par(mfrow=c(3,2))
#hist(SA2$Quantity)
#hist(PA2$Quantity)
#hist(PS2$Quantity)
#hist(BP2$Quantity)
#hist(SC2$Quantity)

#table(SA2$LocalityName, SA2$yse, SA2$fH)
# zeros are added correctly - dataset is balanced.


# need to add Region back in:
SA2$Region <- density$Region[match(SA2$LocalityName, density$LocalityName)]
PA2$Region <- density$Region[match(PA2$LocalityName, density$LocalityName)]
PS2$Region <- density$Region[match(PS2$LocalityName, density$LocalityName)]
BP2$Region <- density$Region[match(BP2$LocalityName, density$LocalityName)]
SC2$Region <- density$Region[match(SC2$LocalityName, density$LocalityName)]


# ******************************#
# ******************************#
# One line per locations     ####
# ******************************#
# ******************************#

# get means per plot #

#sorbus
SA2.2 <- aggregate(data = SA2,
                 Quantity~LocalityName+Treatment+yse+fH, FUN = mean)
SA2.2 <- SA2.2[SA2.2$yse==7,]   # showing only year 7 after exclosure
SA2.2$cH <- as.character(SA2.2$fH)
SA2.2$nH <- as.numeric(SA2.2$cH)
SA2.2$Quantity[SA2.2$Treatment == "B"] <- SA2.2$Quantity[SA2.2$Treatment == "B"]*(-1)

#picea
PA2.2 <- aggregate(data = PA2,
                   Quantity~LocalityName+Treatment+yse+fH,
                   FUN = mean)
PA2.2 <- PA2.2[PA2.2$yse==7,]
PA2.2$cH <- as.character(PA2.2$fH)
PA2.2$nH <- as.numeric(PA2.2$cH)
PA2.2$Quantity[PA2.2$Treatment == "B"] <- PA2.2$Quantity[PA2.2$Treatment == "B"]*(-1)

#pinus
PS2.2 <- aggregate(data = PS2,
                   Quantity~LocalityName+Treatment+yse+fH,
                   FUN = mean)
PS2.2 <- PS2.2[PS2.2$yse==7,]
PS2.2$cH <- as.character(PS2.2$fH)
PS2.2$nH <- as.numeric(PS2.2$cH)
PS2.2$Quantity[PS2.2$Treatment == "B"] <- PS2.2$Quantity[PS2.2$Treatment == "B"]*(-1)

#betula
BP2.2 <- aggregate(data = BP2,
                   Quantity~LocalityName+Treatment+yse+fH,
                   FUN = mean)
BP2.2 <- BP2.2[BP2.2$yse==7,]
BP2.2$cH <- as.character(BP2.2$fH)
BP2.2$nH <- as.numeric(BP2.2$cH)
BP2.2$Quantity[BP2.2$Treatment == "B"] <- BP2.2$Quantity[BP2.2$Treatment == "B"]*(-1)

#willow
SC2.2 <- aggregate(data = SC2,
                   Quantity~LocalityName+Treatment+yse+fH,
                   FUN = mean)
SC2.2 <- SC2.2[SC2.2$yse==7,]
SC2.2$cH <- as.character(SC2.2$fH)
SC2.2$nH <- as.numeric(SC2.2$cH)
SC2.2$Quantity[SC2.2$Treatment == "B"] <- SC2.2$Quantity[SC2.2$Treatment == "B"]*(-1)
#summary(SC2.2$Quantity)
#hist(SC2.2$Quantity)





(Supp_SA <- ggplot()+
  theme_bw()+
  geom_hline(yintercept=0,size=1)+
  geom_line(data=SA2.2[SA2.2$Treatment=="UB",],aes(x=nH, y=Quantity, group=LocalityName))+
  geom_rect(mapping=aes(xmin=1, xmax=7, ymin=min(SA2.2$Quantity[SA2.2$Treatment=="B"]), ymax=0), color="black", alpha=0.5) +
  geom_line(data=SA2.2[SA2.2$Treatment== "B",],aes(x=nH, y=Quantity, group=LocalityName))+
    scale_x_continuous(name = "Tree heigth (cm)",
                       breaks = c(1, 2, 3, 4, 5, 6, 7),
                       labels=c("25","75","125", "175", "225", "275", ">300"))+
  ylab("Mean number of trees per hectare")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Rowan 7 years after exclosure")+coord_flip())
(Supp_PA <- ggplot()+
    theme_bw()+
    geom_hline(yintercept=0,size=1)+
    geom_line(data=PA2.2[PA2.2$Treatment=="UB",],aes(x=nH, y=Quantity, group=LocalityName))+
    geom_rect(mapping=aes(xmin=1, xmax=7, ymin=min(PA2.2$Quantity[PA2.2$Treatment=="B"]), ymax=0), color="black", alpha=0.5) +
    geom_line(data=PA2.2[PA2.2$Treatment== "B",],aes(x=nH, y=Quantity, group=LocalityName))+
    scale_x_continuous(name = "Tree heigth (cm)",
                       breaks = c(1, 2, 3, 4, 5, 6, 7),
                       labels=c("25","75","125", "175", "225", "275", ">300"))+
    ylab("Mean number of trees per hectare")+
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("Spruce 7 years after exclosure")+coord_flip())
(Supp_PS <- ggplot()+
    theme_bw()+
    geom_hline(yintercept=0,size=1)+
    geom_line(data=PS2.2[PS2.2$Treatment=="UB",],aes(x=nH, y=Quantity, group=LocalityName))+
    geom_rect(mapping=aes(xmin=1, xmax=7, ymin=min(PS2.2$Quantity[PS2.2$Treatment=="B"]), ymax=0), color="black", alpha=0.5) +
    geom_line(data=PS2.2[PS2.2$Treatment== "B",],aes(x=nH, y=Quantity, group=LocalityName))+
    scale_x_continuous(name = "Tree heigth (cm)",
                       breaks = c(1, 2, 3, 4, 5, 6, 7),
                       labels=c("25","75","125", "175", "225", "275", ">300"))+
    ylab("Mean number of trees per hectare")+
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("Pine 7 years after exclosure")+coord_flip())
(Supp_BP <- ggplot()+
    theme_bw()+
    geom_hline(yintercept=0,size=1)+
    geom_line(data=PS2.2[BP2.2$Treatment=="UB",],aes(x=nH, y=Quantity, group=LocalityName))+
    geom_rect(mapping=aes(xmin=1, xmax=7, ymin=min(BP2.2$Quantity[BP2.2$Treatment=="B"]), ymax=0), color="black", alpha=0.5) +
    geom_line(data=BP2.2[BP2.2$Treatment== "B",],aes(x=nH, y=Quantity, group=LocalityName))+
    scale_x_continuous(name = "Tree heigth (cm)",
                       breaks = c(1, 2, 3, 4, 5, 6, 7),
                       labels=c("25","75","125", "175", "225", "275", ">300"))+
    ylab("Mean number of trees per hectare")+
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("Birch 7 years after exclosure")+coord_flip())
(Supp_SC <- ggplot()+
    theme_bw()+
    geom_hline(yintercept=0,size=1)+
    geom_line(data=SC2.2[SC2.2$Treatment=="UB",],aes(x=nH, y=Quantity, group=LocalityName))+
    geom_rect(mapping=aes(xmin=1, xmax=7, ymin=min(SC2.2$Quantity[SC2.2$Treatment=="B"]), ymax=0), color="black", alpha=0.5) +
    geom_line(data=SC2.2[SC2.2$Treatment== "B",],aes(x=nH, y=Quantity, group=LocalityName))+
    scale_x_continuous(name = "Tree heigth (cm)",
                       breaks = c(1, 2, 3, 4, 5, 6, 7),
                       labels=c("25","75","125", "175", "225", "275", ">300"))+
    ylab("Mean number of trees per hectare")+
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("Goat willow 7 years after exclosure")+coord_flip())

# Goat willow is quite rare.

#tiff("age_distribution_year7_per_locations.tiff", height = 35, width = 20, units = "cm", res = 300)
grid.draw(rbind(ggplotGrob(Supp_SA), ggplotGrob(Supp_PA), 
                ggplotGrob(Supp_PS), ggplotGrob(Supp_BP),ggplotGrob(Supp_SC),
                size = "last"))
#dev.off()
# END supp figure####




# ******************************************#
# ******************************************#
# Large trees over time ####
# ******************************************#
# ******************************************#

# Standardise against year 1.
# Nor data for year 0, but trees dont reach 3, in one year
head(SA2)
allSp <- rbind(SA2, PA2, PS2, BP2)
head(allSp)
#tapply(allSp$Quantity, allSp$fH,FUN =mean)
#unique(allSp$Taxa)
allSp$Taxa[allSp$Taxa=="Betula pendula (Lavlandbjørk)" | allSp$Taxa=="Betula pubescens (Bjørk)"] <- "Birch"
allSp$Taxa[allSp$Taxa=="Sorbus aucuparia (Rogn)" ] <- "Rowan"
allSp$Taxa[allSp$Taxa=="Picea abies (Gran)" ] <- "Spruce"
allSp$Taxa[allSp$Taxa=="Pinus sylvestris (Furu)"] <- "Pine"
#unique(allSp$Taxa)


allSp2 <- dcast(data = allSp, Taxa+LocalityName+Plot+fH+Region+Treatment~yse, value.var = "Quantity", fun.aggregate = sum) # sum because there are two birches
#head(allSp2, 20)   # combine birches
#tail(allSp2)
#table(allSp2$LocalityName, allSp2$Region)
allSp2$`1` <- allSp2$`1`-allSp2$`1`
allSp2$`2` <- allSp2$`2`-allSp2$`1`
allSp2$`3` <- allSp2$`3`-allSp2$`1`
allSp2$`4` <- allSp2$`4`-allSp2$`1`
allSp2$`5` <- allSp2$`5`-allSp2$`1`
allSp2$`6` <- allSp2$`6`-allSp2$`1`
allSp2$`7` <- allSp2$`7`-allSp2$`1`
allSp2$`8` <- allSp2$`8`-allSp2$`1`
allSp2$`9` <- allSp2$`9`-allSp2$`1`
head(allSp2)   # just chance?
tail(allSp2)


# melt
allSp3 <- melt(data=allSp2, id.vars = c("Taxa", "LocalityName", "Plot", "fH", "Region", "Treatment"),
               measure.vars = c('1','2','3','4','5','6','7'), variable.name = "yse", value.name = "Quantity")
#head(allSp3)
#table(allSp3$Taxa, allSp3$)
#tapply(X=allSp3$Quantity, allSp3$Taxa,FUN =mean)   # why does it go down?
#tapply(X=allSp3$Quantity, allSp3$fH,FUN =sum)

allSp3$fH <- as.numeric(as.character(allSp3$fH))
allSp3 <- allSp3[allSp3$fH>5,]  # defining large trees as those above 2.25 (make the data less zero inflated)
allSp3 <- aggregate(data = allSp3, Quantity~Region+LocalityName+Treatment+Plot+yse+Taxa, FUN = sum)

# get means per treatment


allSp4 <- aggregate(data = allSp3, Quantity~LocalityName+Treatment+yse+Taxa, FUN = mean, drop = T)   # drop = T adds back in the sites where a species is rare
allSp5 <- aggregate(data = allSp4, Quantity~Treatment+yse+Taxa, FUN = mean, na.rm=T)
se <- function(x) sd(x, na.rm=T)/sqrt(length(x))
allSp5$SE <- aggregate(data = allSp4, Quantity~Treatment+yse+Taxa, FUN = se)[,"Quantity"]
allSp5$yse <- as.numeric(as.character(allSp5$yse))
#allSp6 <- allSp5[allSp5$yse < 8,]


pd <- position_dodge(width=0.4)


LargeTrees <- ggplot(data=allSp5 , aes(y=Quantity, x=yse, group=Treatment, linetype=Treatment))+
  geom_line(size=1, position = pd)+
  geom_point(size=3, position = pd, aes(shape=Treatment))+
  facet_wrap(~ Taxa, ncol = 1)+
  geom_errorbar(aes(ymax = Quantity+SE, ymin = Quantity-SE, linetype=NULL), size=1.1, width=0.5, position=pd)+
  theme_bw()+
  #guides(linetype=F, shape=F)+
  #ylab(expression(atop("Mean number of trees","above 2 m per hectare")))+
  ylab("Mean number of trees above 2 m per hectare")+
  scale_linetype_manual(breaks = c("B", "UB"), labels = c("Open plots", "Exclosures"), values=c(1,2))+
  scale_shape_manual(breaks = c("B", "UB"), labels = c("Open plots", "Exclosures"), values=c(16,17))+
  scale_x_continuous(name="", breaks=c(1,3,5,7))+
    theme(legend.justification = c(0.01, 0.99), 
          legend.position = c(0.01, 0.99),
          #legend.background = element_blank(),
          legend.key.width = unit(3,"line"),
          legend.text=element_text(size=8),
          legend.title = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

setwd("M:/Anders L Kolstad/R/R_projects/succession_paper")


#tiff("Large_trees_and_canopy_dominance_over_time.tiff", height = 25, width = 10, units = "cm", res=300)
#library(cowplot)
plot_grid(LargeTrees, canopyDom, ncol=1, align="hv", rel_heights = c(1, 0.4))
dev.off()





# Add region and productivity inder and try to model 
allSp_datT <- allSp3[allSp3$Region== "Trøndelag" & allSp3$yse=="7",]
allSp_datT2 <- allSp3[allSp3$Region== "Telemark" & allSp3$yse=="7",]

allSp_datT$Region <- factor(allSp_datT$Region)
allSp_datT2$Region <- factor(allSp_datT2$Region)


allSp_dat <- rbind(
  aggregate(data = allSp_datT, Quantity~Region+LocalityName+Treatment+Plot+yse+Taxa, FUN = mean, drop = T),   # I first used drop F, but this is done already previously
  aggregate(data = allSp_datT2, Quantity~Region+LocalityName+Treatment+Plot+yse+Taxa, FUN = mean, drop = T)
)

setwd("M:\\Anders L Kolstad\\R\\R_projects\\succession_paper")
load("prod_index_telemark_and_trondelag.RData")
allSp_dat$productivity <- productivity$productivity[match(allSp_dat$LocalityName, productivity$LocalityName)]

# trying to model here....
# tried normal approach, but there are too many zeros.
birch <- lmerTest::lmer(data=allSp_dat[allSp_dat$Taxa== "Birch",],
                        log(Quantity+1)~Treatment*productivity + (1|Region/LocalityName), REML=F)

plot(birch)
plot(allSp_dat$Treatment[allSp_dat$Taxa== "Birch"], resid(birch)) # not good



allSp_dat2 <- aggregate(data= allSp_dat, Quantity~Region+LocalityName+Treatment+Taxa, FUN = mean, drop = F)
table(allSp_dat2$Taxa)

# trying simle KW test here (ignoring site productivity)
kruskal.test(data= allSp_dat2[allSp_dat2$Taxa=="Birch",],
             Quantity~Treatment)
kruskal.test(data= allSp_dat2[allSp_dat2$Taxa=="Pine",],
             Quantity~Treatment)
kruskal.test(data= allSp_dat2[allSp_dat2$Taxa=="Spruce",],
             Quantity~Treatment)
kruskal.test(data= allSp_dat2[allSp_dat2$Taxa=="Rowan",],
             Quantity~Treatment)

allSp_dat3 <- dcast(data = allSp_dat2, Region+LocalityName+Taxa~Treatment, value.var = "Quantity", fun.aggregate = mean)
allSp_dat3$diff <- allSp_dat3$UB-allSp_dat3$B
setwd("M:\\Anders L Kolstad\\R\\R_projects\\succession_paper")
load("prod_index_telemark_and_trondelag.RData")
allSp_dat3$productivity <- productivity$productivity[match(allSp_dat3$LocalityName, productivity$LocalityName)]

# tried modelling here.
# using the treatment difference as the response
birch <- lm(data=allSp_dat3[allSp_dat3$Taxa== "Birch",],
                        diff~productivity)
plot(birch)   # not looking good
# variance increases with productivity



LargeTrees2 <- ggplot(data = allSp_dat3, aes(x = productivity, y = diff))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0, size=2, colour="grey")+
  geom_point(size = 3, stroke = 2, shape =1)+
  facet_wrap(~Taxa, ncol=4)+
  theme(strip.text.x = element_blank())+
  ylab(expression(atop(paste(Delta, " Mean number of large trees"), "per hectare (year 7 only)")))+
  scale_x_continuous(name="Site productivity", breaks=c(0.25,0.5,0.75))


library(cowplot)
setwd("M:/Anders L Kolstad/R/R_projects/succession_paper")
#tiff("supp_Large_trees_over_time.tiff", height = 15, width = 30, units = "cm", res=300)
plot_grid(LargeTrees, LargeTrees2, ncol=1, align="hv", rel_heights = c(1,0.8))
dev.off()  


# ******************************************#
# ******************************************#
# setting up data for demography plots ####
# ******************************************#
# ******************************************#

SA2$taxa <- "Rowan"
PA2$taxa <- "Spruce"
PS2$taxa <- "Pine"
BP2$taxa <- "Birch"
tempBPdat <- rbind(SA2, PA2, PS2, BP2)

# get means per treatment
tempBPdat2 <- aggregate(data = tempBPdat,
                 Quantity~Treatment+yse+fH+taxa+LocalityName,
                 FUN = mean)
tempBPdat3 <- aggregate(data = tempBPdat2,
                        Quantity~Treatment+yse+fH+taxa,
                        FUN = mean)
tempBPdat4 <- aggregate(data = tempBPdat2,
                        Quantity~Treatment+yse+fH+taxa,
                        FUN = se)
tempBPdat3$se <- tempBPdat4$Quantity
head(tempBPdat3)

SA3 <- aggregate(data = SA2,
                 Quantity~Treatment+yse+fH,
                 FUN = mean)
PA3 <- aggregate(data = PA2,
                 Quantity~Treatment+yse+fH,
                 FUN = mean)
PS3 <- aggregate(data = PS2,
                 Quantity~Treatment+yse+fH,
                 FUN = mean)
BP3 <- aggregate(data = BP2,
                 Quantity~Treatment+yse+fH,
                 FUN = mean)
SC3 <- aggregate(data = SC2,
                 Quantity~Treatment+yse+fH,
                 FUN = mean)



# DONT RUN!!! (unless making supplementary figure)
# get means per treatment and region
levels(density$Region)
# run one region at the time and make figures below.
MyRegion <- "Trøndelag"
MyRegion <- "Telemark"
#MyRegion <- "Hedmark"

thinned <- c("Hi_tydal", "Malvik", "Selbu_Flub")
SA2x <- SA2[!SA2$LocalityName %in% thinned,]
PA2x <- PA2[!PA2$LocalityName %in% thinned,]
PS2x <- PS2[!PS2$LocalityName %in% thinned,]
BP2x <- BP2[!BP2$LocalityName %in% thinned,]



SA3 <- aggregate(data = subset(SA2x, Region == MyRegion),
                 Quantity~Treatment+yse+fH,
                 FUN = mean)
PA3 <- aggregate(data = subset(PA2x, Region == MyRegion),
                 Quantity~Treatment+yse+fH,
                 FUN = mean)
PS3 <- aggregate(data = subset(PS2x, Region == MyRegion),
                 Quantity~Treatment+yse+fH,
                 FUN = mean)
BP3 <- aggregate(data = subset(BP2x, Region == MyRegion),
                 Quantity~Treatment+yse+fH,
                 FUN = mean)
#SC3 <- aggregate(data = subset(SC2x, Region == MyRegion),
#                 Quantity~Treatment+yse+fH,
#                 FUN = mean)

# End of ' dont run'



range <- c(1,4,7)
tempBPdat3$cH <- as.character(tempBPdat3$fH)
tempBPdat3$nH <- as.numeric(tempBPdat3$cH)
tempBPdat3$fyse <- as.factor(tempBPdat3$yse)
tempBPdat3 <- tempBPdat3[tempBPdat3$fyse %in% range,]
table(tempBPdat3$yse)
head(tempBPdat3)


SA3$cH <- as.character(SA3$fH)
SA3$nH <- as.numeric(SA3$cH)
#SA3 <- SA3[SA3$yse<9,]
SA3$fyse <- as.factor(SA3$yse)
SA3 <- SA3[SA3$fyse %in% range,]
table(SA3$yse)
SA3$Quantity[SA3$Treatment == "B"] <- SA3$Quantity[SA3$Treatment == "B"]*(-1)

PA3$cH <- as.character(PA3$fH)
PA3$nH <- as.numeric(PA3$cH)
#PA3 <- PA3[PA3$yse<9,]
PA3$fyse <- as.factor(PA3$yse)
PA3 <- PA3[PA3$fyse %in% range,]
PA3$Quantity[PA3$Treatment == "B"] <- PA3$Quantity[PA3$Treatment == "B"]*(-1)

PS3$cH <- as.character(PS3$fH)
PS3$nH <- as.numeric(PS3$cH)
#PS3 <- PS3[PS3$yse<9,]
PS3$fyse <- as.factor(PS3$yse)
PS3 <- PS3[PS3$fyse %in% range,]
PS3$Quantity[PS3$Treatment == "B"] <- PS3$Quantity[PS3$Treatment == "B"]*(-1)

BP3$cH <- as.character(BP3$fH)
BP3$nH <- as.numeric(BP3$cH)
#BP3 <- BP3[BP3$yse<9,]
BP3$fyse <- as.factor(BP3$yse)
BP3 <- BP3[BP3$fyse %in% range,]
BP3$Quantity[BP3$Treatment == "B"] <- BP3$Quantity[BP3$Treatment == "B"]*(-1)

SC3$cH <- as.character(SC3$fH)
SC3$nH <- as.numeric(SC3$cH)
#SC3 <- SC3[SC3$yse<9,]
SC3$fyse <- as.factor(SC3$yse)
SC3 <- SC3[SC3$fyse %in% range,]
SC3$Quantity[SC3$Treatment == "B"] <- SC3$Quantity[SC3$Treatment == "B"]*(-1)


# ******************************************#
# ******************************************#
# demography plots                       ####
# ******************************************#
# ******************************************#

(SA_fig <- ggplot()+
  geom_hline(yintercept=0,size=1)+
  geom_area(data=SA3[SA3$Treatment=="UB",],aes(x=nH, y=Quantity, fill=fyse, linetype=fyse),
    position = "identity",colour = "black", size = 2, alpha=0.4)+
  geom_area(data=SA3[SA3$Treatment=="B",],aes(x=nH, y=Quantity, fill=fyse, linetype=fyse),
            position = "identity",colour = "black", size = 2, alpha=0.4)+
  scale_fill_manual(name= "Years since exclosure",
                    values= c("7" = "red", "4" = "grey60", "1" = "grey100"))+
  scale_linetype_manual(name= "Years since exclosure",
                         values= c("7" = "solid", "4" = "longdash", "1" = "dotted"))+
  scale_x_continuous(name = "Tree height (cm)",
                     breaks = c(1, 2, 3, 4, 5, 6, 7),
                     labels=c("25","75","125", "175", "225", "275", ">300"))+
  #scale_y_continuous(name = "Mean number of trees per hectare",
  #                 breaks = c(-6000, -3000,0, 3000, 6000),
  #                 labels=c("6000","3000", "0", "3000", "6000"),
  #                 limits=c(-8000, 8000))+
  #scale_y_continuous(name = "Mean number of trees per hectare",
   #                   breaks = c(-8000, -4000,0,4000,8000),
    #                  labels=c("8000", "4000","0","4000", "8000"),
     #                limits= c(-11000, 11000))+
   scale_y_continuous(name = "Mean number of trees per hectare",
                      breaks = c(-4000, -2000,0,2000,4000),
                      labels=c("4000", "2000","0","2000", "4000"),
                      limits= c(-6000, 6000))+
  theme_bw()+theme(legend.justification=c(0,1), 
                   legend.position=c(0,1),
                   legend.background = element_rect(fill=NA),
                   plot.title = element_text(hjust = 0.5, size=22),
                   legend.key.size = unit(2,"line"),
                   legend.text=element_text(size=20))+
  #annotate("text", cex=7, label= "Rowan\nn=30", x=6, y=7200) +      # all regions
  #annotate("text", cex=7, label= "Rowan\nn=12", x=6, y=8000) +      # Trøndelag
  annotate("text", cex=7, label= "Rowan\nn=15", x=6, y=5000) +      # Telemark
  coord_flip()+
  #ggtitle("Open plots | Exclosures")
  #ggtitle("Trøndelag region\nOpen plots | Exclosures")
  ggtitle("Telemark region\nOpen plots | Exclosures")
  #ggtitle("Hedmark region\nOpen plots | Exclosures")
)

(PA_fig <- ggplot()+theme_bw()+
    geom_hline(yintercept=0,size=1)+
    geom_area(data=PA3[PA3$Treatment=="UB",],aes(x=nH, y=Quantity, fill=fyse, linetype=fyse),
              position = "identity",colour = "black", size = 2, alpha=0.4)+
    geom_area(data=PA3[PA3$Treatment=="B",],aes(x=nH, y=Quantity, fill=fyse, linetype=fyse),
              position = "identity",colour = "black", size = 2, alpha=0.4)+
    scale_fill_manual(name= "Years since exclosure",
                      values= c("7" = "red", "4" = "grey60", "1" = "grey100"))+
    scale_linetype_manual(name= "Years since exclosure",
                          values= c("7" = "solid", "4" = "longdash", "1" = "dotted"))+
    scale_x_continuous(name = "Tree height (cm)",
                       breaks = c(1, 2, 3, 4, 5, 6, 7),
                       labels=c("25","75","125", "175", "225", "275", ">300"))+
    #scale_y_continuous(name = "Mean number of trees per hectare",
    #                   breaks = c(-4000, -2000, 0, 2000, 4000),
    #                   labels=c("4000","2000", "0", "2000", "4000"),
    #                   limits=c(-5000, 5000))+
    #scale_y_continuous(name = "Mean number of trees per hectare",
     #                  breaks = c(-1000,0, 1000),
      #                 labels=c("1000","0", "1000"),
       #                limits=c(-1300, 1300))+
    scale_y_continuous(name = "Mean number of trees per hectare",
                       breaks = c(-8000, -4000, 0, 4000, 8000),
                       labels=c("8000","4000", "0", "4000", "8000"),
                       limits=c(-10000, 10000))+
    guides(fill=FALSE, linetype=FALSE)+
    #annotate("text", cex=7, label= "Spruce\nn=31", x=6, y=4500)+        # all regions
    #annotate("text", cex=7, label= "Spruce\nn=12", x=6, y=1000) +       # Trøndelag
    annotate("text", cex=7, label= "Spruce\nn=16", x=6, y=9000) +       # Telemark
    #annotate("text", cex=7, label= "Spruce\nn=16", x=6, y=0.7) +       # Hedmark
    coord_flip()
)
(PS_fig <- ggplot()+theme_bw()+
    geom_hline(yintercept=0,size=1)+
    geom_area(data=PS3[PS3$Treatment=="UB",],aes(x=nH, y=Quantity, fill=fyse, linetype=fyse),
              position = "identity",colour = "black", size = 2, alpha=0.4)+
    geom_area(data=PS3[PS3$Treatment=="B",],aes(x=nH, y=Quantity, fill=fyse, linetype=fyse),
              position = "identity",colour = "black", size = 2, alpha=0.4)+
    scale_fill_manual(name= "Years since exclosure",
                      values= c("7" = "red", "4" = "grey60", "1" = "grey100"))+
    scale_linetype_manual(name= "Years since exclosure",
                          values= c("7" = "solid", "4" = "longdash", "1" = "dotted"))+
    scale_x_continuous(name = "Tree height (cm)",
                       breaks = c(1, 2, 3, 4, 5, 6, 7),
                       labels=c("25","75","125", "175", "225", "275", ">300"))+
    #scale_y_continuous(name = "Mean number of trees per hectare",
    #                   breaks = c(-3000, -1500, 0, 1500, 3000),
    #                   labels=c("3000", "1500", "0", "1500", "3000"),
    #                   limits=c(-4000, 4000))+
    #scale_y_continuous(name = "Mean number of trees per hectare",
     #                  breaks = c(-6000,-3000,0, 3000,6000),
      #                 labels=c("6000","3000","0", "3000", "6000"),
       #                limits=c(-8000, 8000))+
    scale_y_continuous(name = "Mean number of trees per hectare",
                       breaks = c(-2000,-1000,0, 1000,2000),
                       labels=c("2000","1000","0", "1000", "2000"),
                       limits=c(-2500, 2500))+
    guides(fill=FALSE, linetype=FALSE)+
    #annotate("text", cex=7, label= "Pine\nn=26", x=6, y=3600) +       # all regions
    #annotate("text", cex=7, label= "Pine\nn=10", x=6, y=6000) +       # Trøndelag - "Selbu_Sl" &  "steinkjer_2BBb"
    annotate("text", cex=7, label= "Pine\nn=14", x=6, y=2200) +       # Telemark
    #annotate("text", cex=7, label= "Pine\nn=14", x=6, y=0.5) +       # Hedmark
    coord_flip()
)
(BP_fig <- ggplot()+theme_bw()+
    geom_hline(yintercept=0,size=1)+
    geom_area(data=BP3[BP3$Treatment=="UB",],aes(x=nH, y=Quantity, fill=fyse, linetype=fyse),
              position = "identity",colour = "black", size = 2, alpha=0.4)+
    geom_area(data=BP3[BP3$Treatment=="B",],aes(x=nH, y=Quantity, fill=fyse, linetype=fyse),
              position = "identity",colour = "black", size = 2, alpha=0.4)+
    scale_fill_manual(name= "Years since exclosure",
                      values= c("7" = "red", "4" = "grey60", "1" = "grey100"))+
    scale_linetype_manual(name= "Years since exclosure",
                          values= c("7" = "solid", "4" = "longdash", "1" = "dotted"))+
    scale_x_continuous(name = "Tree height (cm)",
                       breaks = c(1, 2, 3, 4, 5, 6, 7),
                       labels=c("25","75","125", "175", "225", "275", ">300"))+
    #scale_y_continuous(name = "Mean number of trees per hectare",
    #                   breaks = c(-2000, -1000, 0, 1000, 2000),
    #                   labels=c("2000","1000","0","1000","2000"),
    #                   limits=c(-2400, 2400))+
    #scale_y_continuous(name = "Mean number of trees per hectare",
     #                  breaks = c(-3000, 0, 3000),
      #                 labels=c("3000","0","3000"),
       #                limits=c(-3000, 3000))+
    scale_y_continuous(name = "Mean number of trees per hectare",
                       breaks = c(-1000,-500, 0, 500, 1000),
                       labels=c("1000", "500", "0", "500", "1000"),
                       limits=c(-1500, 1500))+
    guides(fill=FALSE, linetype=FALSE)+
    #annotate("text", cex=7, label= "Birch spp\nn=31", x=6, y=2160) +     #all Regions
    #annotate("text", cex=7, label= "Birch sp\nn=12", x=6, y=2500) +     #Trøndelag  
    annotate("text", cex=7, label= "Birch sp\nn=16", x=6, y=1200) +     #Telemark
    #annotate("text", cex=7, label= "Birch sp\nn=16", x=6, y=0.3) +     #Hedmark
    coord_flip()
)
(SC_fig <- ggplot()+theme_bw()+
    geom_hline(yintercept=0,size=1)+
    geom_area(data=SC3[SC3$Treatment=="UB",],aes(x=nH, y=Quantity, fill=fyse, linetype=fyse),
              position = "identity",colour = "black", size = 2, alpha=0.4)+
    geom_area(data=SC3[SC3$Treatment=="B",],aes(x=nH, y=Quantity, fill=fyse, linetype=fyse),
              position = "identity",colour = "black", size = 2, alpha=0.4)+
    scale_fill_manual(name= "Years since exclosure",
                      values= c("7" = "red", "4" = "grey60", "1" = "grey100"))+
    scale_linetype_manual(name= "Years since exclosure",
                          values= c("7" = "solid", "4" = "longdash", "1" = "dotted"))+
    scale_x_continuous(name = "Tree height (cm)",
                       breaks = c(1, 2, 3, 4, 5, 6, 7),
                       labels=c("25","75","125", "175", "225", "275", ">300"))+
    scale_y_continuous(name = "Mean number of trees per hectare",
                       breaks = c(-600,0, 600),
                       labels=c("600","0", "600"),
                       limits=c(-600, 600))+
    #scale_y_continuous(name = "Mean number of trees per hectare",
    #                   breaks = c(-200,0, 200),
    #                   labels=c("200","0", "200"),
    #                   limits=c(-300, 300))+
    guides(fill=FALSE, linetype=FALSE)+
    annotate("text", cex=7, label= "Goat willow\nn=18", x=6, y=540) +   # All Regions
    #annotate("text", cex=7, label= "Goat willow\nn=11", x=6, y=250) +    # Trøndelag
    #annotate("text", cex=7, label= "Goat willow\nn=28", x=6, y=0.15) +   # Telemark
    #annotate("text", cex=7, label= "Goat willow\nn=28", x=6, y=0.15) +   # Hedmark
    coord_flip()
)







getwd()
setwd("M:/Anders L Kolstad/R/R_projects/succession_paper")

#tiff("demography_plot_147.tiff", height = 35, width = 20, units = "cm", res = 300)
#tiff("demography_plot_Trondelag_149.tiff", height = 35, width = 20, units = "cm", res = 300)
#tiff("demography_plot_Telemark_147.tiff", height = 35, width = 20, units = "cm", res = 300)
#tiff("demography_plot_Hedmark.tiff", height = 35, width = 20, units = "cm", res = 300)
grid.draw(rbind(ggplotGrob(SA_fig), 
                ggplotGrob(PA_fig), 
                ggplotGrob(PS_fig), 
                ggplotGrob(BP_fig),  
                size = "last"))
dev.off()

           
# BARPLOT DEMOGRAPHY   ####


# Make all values positive again
SA3$Quantity[SA3$Treatment == "B"] <- SA3$Quantity[SA3$Treatment == "B"]*(-1)
PA3$Quantity[PA3$Treatment == "B"] <- PA3$Quantity[PA3$Treatment == "B"]*(-1)
PS3$Quantity[PS3$Treatment == "B"] <- PS3$Quantity[PS3$Treatment == "B"]*(-1)
BP3$Quantity[BP3$Treatment == "B"] <- BP3$Quantity[BP3$Treatment == "B"]*(-1)

SA3$taxa <- "Rowan"
PA3$taxa <- "Spruce"
PS3$taxa <- "Pine"
BP3$taxa <- "Birch"
BPdat <- rbind(SA3, PA3, PS3, BP3)
#BPdat$fyse <- factor(BPdat$yse)
#BPdat$min <- 0

#tiff("demography_linerange.tiff", height = 20, width = 10, units = "cm", res = 300)
ggplot()+theme_bw()+
  geom_hline(yintercept=0,size=1)+
  geom_linerange(data=BPdat[BPdat$Treatment=="UB" & BPdat$fH!="1",],
                 aes(x=nH, ymax = Quantity, ymin = min, group=fyse, colour = fyse),
                 size = 1.8, position = position_dodge(width=0.9))+
  geom_linerange(data=BPdat[BPdat$Treatment=="B" & BPdat$fH!="1",],
                 aes(x=nH, ymax = min, ymin = Quantity, group=fyse, colour = fyse),
                 size = 1.8, position = position_dodge(width=0.9))+
  scale_x_continuous(name = "Tree height (cm)",
                     breaks = c(1, 2, 3, 4, 5, 6, 7),
                     labels=c("0-50","50-100","100-150", "150-200", "200-250", "250-300", ">300"))+
  theme(panel.grid.major = element_blank())+
  coord_flip()+
  facet_wrap(~taxa, scales = "free", ncol = 1)+
  guides(colour=guide_legend(title="Years\nsince\nexclosure"))+
  ggtitle(paste(label = sprintf('\u2190'), " Open plots | Exclosures ", label =sprintf('\u2192')))+
  theme(plot.title = element_text(hjust = 0.5))
  #scale_y_continuous(name = "Mean number of trees per hectare",
  #                   breaks = c(-2000, -1000,0,1000,2000),
  #                   labels=c("2000", "1000","0","1000", "2000"),
  #                   limits= c(-2300, 2300))
dev.off()


BPdat2 <- BPdat
#BPdat2$Quantity[BPdat2$Treatment=="B"] <- BPdat2$Quantity[BPdat2$Treatment=="B"]*(-1)
levels(tempBPdat3$Treatment) <- c("Open plots", "Exclosures")
levels(BPdat2$Treatment) <- c("Open plots", "Exclosures")
#BPdat2 <- BPdat2[BPdat2$yse %in% c(1,4,7),]
#BPdat2$fyse <- factor(BPdat2$fyse)

getwd()
#save(BPdat2, file="BPdat2.RData")
#load("BPdat2.RData")
tempBPdat3$taxa <- factor(tempBPdat3$taxa)
tiff("/home/anders/Desktop/errorbars.tiff", height = 25, width = 15, units = "cm", res = 300)
plot_grid(

ggplot(data=tempBPdat3[tempBPdat3$fH!="1" & tempBPdat3$Treatment == "Open plots",])+
  theme_bw()+
    geom_bar(aes(x = fH, y = Quantity, fill = fyse), stat = "identity",  position= "dodge")+
    geom_linerange(aes(x = fH, ymax = Quantity, ymin=Quantity-se, group = fyse),  position= position_dodge(width = 0.9))+
    scale_x_discrete(name = "Tree height (cm)",
                  breaks = c(1, 2, 3, 4, 5, 6, 7),
                  labels=c("0-50","50-100","100-150", "150-200", "200-250", "250-300", ">300"))+
  scale_fill_manual(values=c(grey(0.9), grey(0.6), grey(0.3)))+
  coord_flip(ylim = c(0,2300))+                                      
  scale_y_continuous(name = "Mean number of trees",
                     breaks = c(0,1000,2000),
                     labels=c("0","1000", "2000"),
                     trans = "reverse"               )+ 
  facet_grid(taxa~Treatment)+
  guides(fill=FALSE)+
  theme(strip.text.y = element_blank(),
        axis.title.x = element_text(hjust = 1),
        panel.grid = element_blank()  ) # dont know how to make this nice - the errors are longer than the bars
,
ggplot(data=tempBPdat3[tempBPdat3$fH!="1" & tempBPdat3$Treatment == "Exclosures",])+
  theme_bw()+
  geom_bar(aes(x = fH, y = Quantity, fill = fyse), stat = "identity",
           position="dodge")+
  geom_linerange(aes(x = fH, ymax = Quantity, ymin=Quantity-se, group = fyse),  position= position_dodge(width = 0.9))+
  theme(panel.grid.major = element_blank())+
  scale_fill_manual(values=c(grey(0.9), grey(0.6), grey(0.3)))+
  coord_flip(ylim = c(0,2300))+
  scale_y_continuous(name = "per hectare",
                     breaks = c(0,1000,2000),
                     labels=c("0","1000", "2000"))+
  facet_grid(taxa~Treatment)+
  guides(fill=guide_legend(title="Years\nsince\nexclosure"))+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_text(hjust = 0),
        panel.grid = element_blank() ),
ncol=2, align="hv", rel_widths = c(0.95, 1))
dev.off()



#tiff("/home/anders/Desktop/no_errorbars.tiff", height = 25, width = 15, units = "cm", res = 300)
plot_grid(
  
  ggplot(data=tempBPdat3[tempBPdat3$fH!="1" & tempBPdat3$Treatment == "Open plots",])+
    theme_bw()+
    geom_bar(aes(x = fH, y = Quantity, fill = fyse), stat = "identity",  position= "dodge")+
    scale_x_discrete(name = "Tree height (cm)",
                     breaks = c(1, 2, 3, 4, 5, 6, 7),
                     labels=c("0-50","50-100","100-150", "150-200", "200-250", "250-300", ">300"))+
    scale_fill_manual(values=c("grey80", "grey40", "black"))+
    coord_flip(ylim = c(0,2300))+                                        
    scale_y_continuous(name = "Mean number of trees",
                       breaks = c(0,1000,2000),
                       labels=c("0","1000", "2000"),
                       trans = "reverse"               )+ 
    facet_grid(taxa~Treatment)+
    guides(fill=FALSE)+
    theme(strip.text.y = element_blank(),
          axis.title.x = element_text(hjust = 1),
          panel.grid = element_blank()  ) # dont know how to make this nice - the errors are longer than the bars
  ,
  ggplot(data=tempBPdat3[tempBPdat3$fH!="1" & tempBPdat3$Treatment == "Exclosures",])+
    theme_bw()+
    geom_bar(aes(x = fH, y = Quantity, fill = fyse), stat = "identity",
             position="dodge")+
    theme(panel.grid.major = element_blank())+
    scale_fill_manual(values=c("grey80", "grey40", "black"))+
    coord_flip(ylim = c(0,2300))+
    scale_y_continuous(name = "per hectare",
                       breaks = c(0,1000,2000),
                       labels=c("0","1000", "2000"))+
    facet_grid(taxa~Treatment)+
    guides(fill=guide_legend(title="Years\nsince\nexclosure"))+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x = element_text(hjust = 0),
          panel.grid = element_blank() ),
  ncol=2, align="hv", rel_widths = c(0.95, 1))
dev.off()



# same but for first height category
p3 <- ggplot(data=BPdat2[BPdat2$fH=="1" & BPdat2$Treatment == "Open plots",])+
  theme_bw()+
  geom_bar(aes(x = fH, y = Quantity, fill = fyse), stat = "identity",
           position=position_dodge())+
  scale_x_discrete(name = "Tree height (cm)",
                   breaks = c(1, 2, 3, 4, 5, 6, 7),
                   labels=c("0-50","50-100","100-150", "150-200", "200-250", "250-300", ">300"))+
  scale_fill_manual(values=c("grey80", "grey40", "black"))+
  coord_flip(ylim = c(0,7500))+                                        
  scale_y_continuous(name = "Mean number of trees",
                     breaks = c(0,3000,6000),
                     labels=c("0","3000", "6000"),
                     trans = "reverse"               )+ 
  facet_grid(taxa~Treatment)+
  guides(fill=FALSE)+
  theme(strip.text.y = element_blank(),
        axis.title.x = element_text(hjust = 1),
        panel.grid = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

p4 <-   ggplot(data=BPdat2[BPdat2$fH=="1" & BPdat2$Treatment == "Exclosures",])+
  theme_bw()+
  geom_bar(aes(x = fH, y = Quantity, fill = fyse), stat = "identity",
           position=position_dodge())+
  theme(panel.grid.major = element_blank())+
  scale_fill_manual(values=c("grey80", "grey40", "black"))+
  coord_flip()+
  scale_y_continuous(name = "< 50 cm per hectare",
                     breaks = c(0,3000,6000),
                     labels=c("0","3000", "6000"),
                     limits = c(0,7500))+
  facet_grid(taxa~Treatment)+
  guides(fill=guide_legend(title="Years\nsince\nexclosure"))+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_text(hjust = 0),
        panel.grid = element_blank() )

#tiff("demography_barplot_small_trees.tiff", height = 25, width = 15, units = "cm", res = 300)
plot_grid(p3, p4, ncol=2, align="hv", rel_widths = c(0.95, 1))
dev.off()

# same but for all height categories
p5 <- ggplot(data=BPdat2[BPdat2$Treatment == "Open plots",])+
  theme_bw()+
  geom_bar(aes(x = fH, y = Quantity, fill = fyse), stat = "identity",
           position=position_dodge())+
  scale_x_discrete(name = "Tree height (cm)",
                   breaks = c(1, 2, 3, 4, 5, 6, 7),
                   labels=c("0-50","50-100","100-150", "150-200", "200-250", "250-300", ">300"))+
  scale_fill_manual(values=c("grey80", "grey40", "black"))+
  coord_flip(ylim = c(0,7500))+                                        
  scale_y_continuous(name = "Mean number of trees",
                     breaks = c(0,3000,6000),
                     labels=c("0","3000", "6000"),
                     trans = "reverse"               )+ 
  facet_grid(taxa~Treatment)+
  guides(fill=FALSE)+
  theme(strip.text.y = element_blank(),
        axis.title.x = element_text(hjust = 1),
        panel.grid = element_blank(),
                axis.ticks.y=element_blank())

p6 <-   ggplot(data=BPdat2[BPdat2$Treatment == "Exclosures",])+
  theme_bw()+
  geom_bar(aes(x = fH, y = Quantity, fill = fyse), stat = "identity",
           position=position_dodge())+
  theme(panel.grid.major = element_blank())+
  scale_fill_manual(values=c("grey80", "grey40", "black"))+
  coord_flip()+
  scale_y_continuous(name = "per hectare",
                     breaks = c(0,3000,6000),
                     labels=c("0","3000", "6000"),
                     limits = c(0,7500))+
  facet_grid(taxa~Treatment)+
  guides(fill=guide_legend(title="Years\nsince\nexclosure"))+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_text(hjust = 0),
        panel.grid = element_blank() )

tiff("demography_barplot_small_trees.tiff", height = 25, width = 15, units = "cm", res = 300)
plot_grid(p5, p6, ncol=2, align="hv", rel_widths = c(0.95, 1))
dev.off()


#scale_y_continuous(name = "Mean number of trees per hectare",
  #                   #breaks = c(-2000, -1000,0,1000,2000),
  #                   labels=c("2000", "1000","0","1000", "2000"),
  #                   limits= c(-2300, 2300))
  
  #ggtitle(paste(label = sprintf('\u2190'), " Open plots | Exclosures ", label =sprintf('\u2192')))+
  #theme(plot.title = element_text(hjust = 0.5))



B_dem <- ggplot(data = BPdat[BPdat$Treatment=="B",], aes(x = fH, y = Quantity, fill = fyse))+
  geom_bar(stat = "identity",
           position=position_dodge())+
  facet_wrap(~taxa, ncol = 1)+
  coord_flip()+
  scale_y_reverse()+
  ggtitle("Open plots")+
  theme_bw()+
  guides(fill=FALSE)
  

UB_dem <- ggplot(data = BPdat[BPdat$Treatment=="UB",], aes(x = fH, y = Quantity, fill = fyse))+
  geom_bar(stat = "identity",
           position=position_dodge())+
  facet_wrap(~taxa, ncol = 1)+
  coord_flip()+
  ggtitle("Exclosures")+
  theme_bw()+
  theme(axis.title.y=element_blank(),
       axis.text.y=element_blank(),
       axis.ticks.y=element_blank())
  
  
library(cowplot)
plot_grid(B_dem, UB_dem, ncol=2, align="hv")

grid.draw(rbind(ggplotGrob(B_dem), 
                ggplotGrob(UB_dem),  
                size = "last"), ncol = 2)






# BOXplot #### 
# Large trees (>2m)
# of last treatment year


#organize data
# average accross the four circles
SAbox <- aggregate(data = subset(SA2, yse == 7 & as.character(fH) > 4),
                   Quantity~Region+LocalityName+Treatment+yse+fH,
                   FUN = mean)
SAbox$taxa <- "Rowan"
PAbox <- aggregate(data = subset(PA2, yse == 7 & as.character(fH) > 4),
                   Quantity~Region+LocalityName+Treatment+yse+fH,
                   FUN = mean)
PAbox$taxa <- "Spruce"
PSbox <- aggregate(data = subset(PS2, yse == 7 & as.character(fH) > 4),
                   Quantity~Region+LocalityName+Treatment+yse+fH,
                   FUN = mean)
PSbox$taxa <- "Pine"
BPbox <- aggregate(data = subset(BP2, yse == 7 & as.character(fH) > 4),
                   Quantity~Region+LocalityName+Treatment+yse+fH,
                   FUN = mean)
BPbox$taxa <- "Birch"
#SCbox <- aggregate(data = subset(SC2, yse == 7 & as.character(fH) > 4),
#                   Quantity~LocalityName+Treatment+yse+fH,
#                   FUN = mean)
#SCbox$taxa <- "Goat willow"


# Bind it together
BPdat <- rbind(SAbox, PAbox, PSbox, BPbox)
head(BPdat)
BPdat2 <- aggregate(data=BPdat, 
                    Quantity~Region+LocalityName+Treatment+taxa,
                    FUN = sum)
#head(BPdat2, 20)
#tail(BPdat2, 20)


# get treatment difference
BPdat3 <- dcast(BPdat2, Region+LocalityName+taxa~Treatment, value.var = "Quantity", FUN = mean)
BPdat3$diff <- BPdat3$UB-BPdat3$B
head(BPdat3,20)
BPdat3$taxa <- as.factor(BPdat3$taxa)
str(BPdat3)
levels(BPdat3$taxa)
BPdat3$taxa <- factor(BPdat3$taxa,
                       levels = c('Spruce',
                                  'Pine',
                                  'Birch',
                                  'Rowan'),ordered = TRUE)


# get mean and standard error of the mean

mean <- c(mean(BPdat3$diff[BPdat3$taxa=="Spruce"]),
          mean(BPdat3$diff[BPdat3$taxa=="Birch"]),
          mean(BPdat3$diff[BPdat3$taxa=="Pine"]),
          mean(BPdat3$diff[BPdat3$taxa=="Rowan"]))
se <-  c(sd(BPdat3$diff[BPdat3$taxa=="Rowan"])/sqrt(nrow(subset(BPdat3, taxa == "Spruce"))),
         sd(BPdat3$diff[BPdat3$taxa=="Rowan"])/sqrt(nrow(subset(BPdat3, taxa == "Birch"))),
         sd(BPdat3$diff[BPdat3$taxa=="Rowan"])/sqrt(nrow(subset(BPdat3, taxa == "Pine"))),
         sd(BPdat3$diff[BPdat3$taxa=="Rowan"])/sqrt(nrow(subset(BPdat3, taxa == "Rowan"))))
forest <- data.frame(mean, se)
forest$upper <- mean+se
forest$lower <- mean-se
forest$taxa <- c("Spruce", "Birch", "Pine", "Rowan")



tiff("large_trees_boxplot.tiff", height = 15, width = 30, units = "cm", res = 300)
ggplot(BPdat3[BPdat3$diff<3000,], aes(x = taxa, y = diff))+
  theme_classic()+
  coord_flip()+
  geom_hline(yintercept=0)  +
  geom_violin(colour = "black",fill = "grey80", alpha=0.1, scale = "width")+
  geom_jitter(shape=16, position=position_jitter(0.2), colour="grey", fill = "grey")+
  geom_pointrange(data=forest, aes(x = taxa, y=mean, ymin=lower, ymax=upper), size =1.2 )+
  ylim(c(-1800,3000))+
  xlab("")+ylab("Differences in mean number of large trees per hectare\n(exclosure minus open plot)")+
  annotate("text", x=4, y= -1600, label="z = 16.91\nobs = 30\np<<0.001")+
  annotate("text", x=3, y= -1600, label="z = \nobs = \np =")+
  annotate("text", x=2, y= -1600, label="z = \nobs = \np =")+
  annotate("text", x=1, y= -1600, label="z = \nobs = \np =")+
  annotate("text", x=4, y= 3000, label="5371\n4177\n6963", size=3)
dev.off()  
# cut tail on rowan plot !!3!! extreme points 
BPdat3[BPdat3$diff>3000,]  #  5371.479, 4177.817, 6963.029

#stat_summary(fun.y=mean, geom="point", shape=16, size=3, colour="red")+
#stat_summary(fun.y=median, geom="point",  shape=17, size=3, colour="black")+


# LMM   #####
# Problem: No or very little variation in the Open plots
# # Solution : use 'diff' as responce and test if intercept != 0


# ROWAN
# strictly positive, right skew, continous data (averages over 4 circles) - gamma
# log(mu_i) = Intercept 
library(glmmTMB)

#hist(BPdat3$diff[BPdat3$taxa=="Rowan"])
#View(subset(BPdat3, taxa == "Rowan"))
#summary(BPdat3$diff[BPdat3$taxa=="Rowan"])
SAlmm <- glmmTMB(diff+0.001 ~ 1 +(1|Region), data = subset(BPdat3, taxa == "Rowan"), family = Gamma(link = "log"))
#E1 <- resid(SAlmm, type = "pearson")
#F1  <- fitted(SAlmm)
#plot(F1, E1)
summary(SAlmm)

exp(SAlmm$fit$par[1])
mean(BPdat3$diff[BPdat3$taxa=="Rowan"])
sd(BPdat3$diff[BPdat3$taxa=="Rowan"])/sqrt(nrow(subset(BPdat3, taxa == "Rowan")))





# OLD ####  
# plot line per species ###

# redo some of the above steps:
# get means per treatment
SA3 <- aggregate(data = SA2,
                 Quantity~Treatment+yse+fH,
                 FUN = mean)
PA3 <- aggregate(data = PA2,
                 Quantity~Treatment+yse+fH,
                 FUN = mean)
PS3 <- aggregate(data = PS2,
                 Quantity~Treatment+yse+fH,
                 FUN = mean)
BP3 <- aggregate(data = BP2,
                 Quantity~Treatment+yse+fH,
                 FUN = mean)
SC3 <- aggregate(data = SC2,
                 Quantity~Treatment+yse+fH,
                 FUN = mean)

SA3 <- SA3[SA3$yse<9,]; SA3$taxa <- "Rowan"
PA3 <- PA3[PA3$yse<9,]; PA3$taxa <- "Spruce"
PS3 <- PS3[PS3$yse<9,]; PS3$taxa <- "Pine"
BP3 <- BP3[BP3$yse<9,]; BP3$taxa <- "Birch sp"
SC3 <- SC3[SC3$yse<9,]; SC3$taxa <- "Goat willow"

line_fig <- rbind(SA3,
                  PA3,
                  PS3,
                  BP3,
                  SC3)
line_fig$cH <- as.character(line_fig$fH)
line_fig$nH <- as.numeric(line_fig$cH)

line_fig1 <- line_fig[line_fig$nH>4,]
#get treatment difference
line_fig1.1 <- dcast(data=line_fig1, yse+taxa~Treatment, value.var = "Quantity", fun.aggregate = sum)



# pick up here

line_fig1$diff <- line_fig1$UB-line_fig1$B
line_figx <- dcast(data=line_fig, taxa~yse, value.var = "diff")

for(i in 3:ncol(line_figx)){
 line_figx[,i] <- line_figx[,i] - line_figx[,2]
 }
line_figx[,2] <- 0
line_figxx <- melt(data=line_figx, id.vars = "taxa", measure.vars = c(2:ncol(line_figx)))

ggplot(data = line_figxx)+theme_classic()+
  geom_line(aes(x=variable, y=value, group=taxa, linetype=taxa, colour=taxa, size=2))+
  geom_hline(yintercept=0)
