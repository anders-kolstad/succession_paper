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

# subsetting different species before aggregating to reduce data size
density$fTaxa <- as.factor(density$Taxa)
levels(density$fTaxa)
SA <- density[density$fTaxa == "Sorbus aucuparia (Rogn)",]
PA <- density[density$fTaxa == "Picea abies (Gran)",]
PS <- density[density$fTaxa == "Pinus sylvestris (Furu)",]
BP <- density[density$fTaxa == "Betula pendula (Lavlandbjørk)" | density$fTaxa == "Betula pubescens (Bjørk)",]
SC <- density[density$fTaxa == "Salix caprea (Selje)",]



# shold I remove locations where the species is very rare? What is rare?
# Defining rare as below 5 records over 7-9 years
par(mar=c(12,5,2,2))


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
# setting up data for demography plots ####
# ******************************************#
# ******************************************#

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
#par(mfrow=c(3,2))
#hist(SA3$Quantity)
#hist(PA3$Quantity)
#hist(PS3$Quantity)
#hist(BP3$Quantity)
#hist(SC3$Quantity)
#table(SA3$Treatment, SA3$yse, SA3$fH)


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

table(SA3$yse)


range <- c(1,4,7)

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
