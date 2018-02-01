# Script for forest succession paper Anders L Kolstad

# Overview

# 1   -  tree heigth and denity
 # problem that max height is 3 m. This makes it impossible to use heigth or biomass

# 1.1  - Age distribution over time
 # vertical plot with exclosure to one side and open plots on the other. First and last year shaded, 
 # other years just lines






# 1 tree height and density ####

# I want to plot the height of the tallest tree inside each circle against time.
# To do this I use the density dataset (obs - max height class?).
# I can first plot a seperate line per treatment, and then possibly a treatment effect plot.
# One pane per tree species. 
# Final plot with treatment differences for all species combined (no error ribbon)

# I want to plot seedling density density in the same way.
# Supplementary plots include seperate lines per site.



# 1.1 ####

# Library
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(reshape2)
# data
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




# subsetting data
levels(density$Region)
regions <- c("Hedmark", "Trøndelag", "Telemark")
density <- density %>% 
  filter(Region %in% regions)


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
View(density[density$h_class==0,]) # empty rows
densityX <- density
density <- densityX[densityX$h_class!=0,]
#plot(density$Height_cm)


# max value for the height category is 7, but when height was recorded in cm there's no limit.
# forcing max value = 7
density$h_class2 <- density$h_class
density$h_class2[density$h_class2 > 7] <- 7
hist(density$h_class2)
plot(density$h_class2)

density$fH <- as.factor(density$h_class2)
levels(density$fH)
unique(density$LocalityName) #47 locations


# Standardising Quantity To mean number of trees per hectare (10.000 m2)
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
# could at least remove the 1's
par(mar=c(12,5,2,2))


SC.sub <- aggregate(data = SC,
                   Quantity~LocalityName,
                   FUN = sum)
BP.sub <- aggregate(data = BP,
                    Quantity~LocalityName,
                    FUN = sum)
PS.sub <- aggregate(data = PS,
                    Quantity~LocalityName,
                    FUN = sum)
PA.sub <- aggregate(data = PA,
                    Quantity~LocalityName,
                    FUN = sum)
SA.sub <- aggregate(data = SA,
                    Quantity~LocalityName,
                    FUN = sum)

#SC.sub$fLoc <- as.factor(SC.sub$LocalityName)

plot(Quantity~fLoc, las=2, type= "n", data = SC.sub)
text(SC.sub$fLoc, SC.sub$Quantity, label=SC.sub$Quantity)
plot(Quantity~as.factor(LocalityName), las=2, type= "n", data = BP.sub)
text(as.factor(BP.sub$LocalityName), BP.sub$Quantity, label=BP.sub$Quantity)
plot(Quantity~as.factor(LocalityName), las=2, type= "n", data = PS.sub)
text(as.factor(PS.sub$LocalityName), PS.sub$Quantity, label=PS.sub$Quantity)
plot(Quantity~as.factor(LocalityName), las=2, type= "n", data = PA.sub)
text(as.factor(PA.sub$LocalityName), PA.sub$Quantity, label=PA.sub$Quantity)
plot(Quantity~as.factor(LocalityName), las=2, type= "n", data = SA.sub)
text(as.factor(SA.sub$LocalityName), SA.sub$Quantity, label=SA.sub$Quantity)


unique(density$LocalityName) 
SC.remove <- SC.sub$LocalityName[SC.sub$Quantity < 6]
PS.remove <- PS.sub$LocalityName[PS.sub$Quantity < 6]
SA.remove <- SA.sub$LocalityName[SA.sub$Quantity < 6]

SC <- SC[!SC$LocalityName %in% SC.remove, ]
PS <- PS[!PS$LocalityName %in% PS.remove, ]
SA <- SA[!SA$LocalityName %in% SA.remove, ]

unique(SC$LocalityName)                         # 28 locations
unique(SC$LocalityName[SC$Region=="Trøndelag"]) # 11 locations
unique(SC$LocalityName[SC$Region=="Telemark"])  # 7 locations
unique(SC$LocalityName[SC$Region=="Hedmark"])   # 10 locations

unique(BP$LocalityName) # 47 locations
unique(BP$LocalityName[BP$Region=="Trøndelag"]) # 15 locations
unique(BP$LocalityName[BP$Region=="Telemark"])  # 16 locations
unique(BP$LocalityName[BP$Region=="Hedmark"])   # 16 locations

unique(PS$LocalityName)                         # 40 locations
unique(PS$LocalityName[PS$Region=="Trøndelag"]) # 12 locations
unique(PS$LocalityName[PS$Region=="Telemark"])  # 14 locations
unique(PS$LocalityName[PS$Region=="Hedmark"])   # 14 locations

unique(PA$LocalityName) # 47 locations

unique(SA$LocalityName) # 43 locations
unique(SA$LocalityName[SA$Region=="Trøndelag"]) # 15 locations
unique(SA$LocalityName[SA$Region=="Telemark"])  # 15 locations
unique(SA$LocalityName[SA$Region=="Hedmark"])   # 13 locations

rm(SC.sub, PA.sub, PS.sub, BP.sub, SA.sub)


# creating  big datasets # filling in the zeros with aggregate (drop = F)
SA2 <- aggregate(data = SA,
                      Quantity~Region+LocalityName+Treatment+Plot+yse+Taxa+fH,
                      FUN = sum, drop = F) 
PA2 <- aggregate(data = PA,
                 Quantity~Region+LocalityName+Treatment+Plot+yse+Taxa+fH,
                 FUN = sum, drop = F)
PS2 <- aggregate(data = PS,
                 Quantity~Region+LocalityName+Treatment+Plot+yse+Taxa+fH,
                 FUN = sum, drop = F)
BP2 <- aggregate(data = BP,
                 Quantity~Region+LocalityName+Treatment+Plot+yse+Taxa+fH,
                 FUN = sum, drop = F)
SC2 <- aggregate(data = SC,
                 Quantity~Region+LocalityName+Treatment+Plot+yse+Taxa+fH,
                 FUN = sum, drop = F)
#par(mfrow=c(3,2))
#hist(SA2$Quantity)
#hist(PA2$Quantity)
#hist(PS2$Quantity)
#hist(BP2$Quantity)
#hist(SC2$Quantity)





# ******************************#
# ******************************#
# One line per locations     ####
# ******************************#
# ******************************#

# get means per plot #

#sorbus
SA2.2 <- aggregate(data = SA2,
                 Quantity~LocalityName+Treatment+yse+fH,
                 FUN = mean)
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

)



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

#tiff("age_distribution_year7_per_locations.tiff", height = 35, width = 20, units = "cm", res = 300)
grid.draw(rbind(ggplotGrob(Supp_SA), ggplotGrob(Supp_PA), 
                ggplotGrob(Supp_PS), ggplotGrob(Supp_BP),ggplotGrob(Supp_SC),
                size = "last"))
dev.off()
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


# DONT RUN!!! (unless making supplementary figure)
# get means per treatment and region
levels(density$Region)

# run one region at the time and make figures below.
MyRegion <- "Trøndelag"
MyRegion <- "Telemark"
MyRegion <- "Hedmark"

SA3 <- aggregate(data = subset(SA2, Region == MyRegion),
                 Quantity~Treatment+yse+fH,
                 FUN = mean)
PA3 <- aggregate(data = subset(PA2, Region == MyRegion),
                 Quantity~Treatment+yse+fH,
                 FUN = mean)
PS3 <- aggregate(data = subset(PS2, Region == MyRegion),
                 Quantity~Treatment+yse+fH,
                 FUN = mean)
BP3 <- aggregate(data = subset(BP2, Region == MyRegion),
                 Quantity~Treatment+yse+fH,
                 FUN = mean)
SC3 <- aggregate(data = subset(SC2, Region == MyRegion),
                 Quantity~Treatment+yse+fH,
                 FUN = mean)

# End of ' dont run'




range <- c(1,4,7)

SA3$cH <- as.character(SA3$fH)
SA3$nH <- as.numeric(SA3$cH)
SA3 <- SA3[SA3$yse<9,]
SA3$fyse <- as.factor(SA3$yse)
SA3 <- SA3[SA3$fyse %in% range,]
SA3$Quantity[SA3$Treatment == "B"] <- SA3$Quantity[SA3$Treatment == "B"]*(-1)

PA3$cH <- as.character(PA3$fH)
PA3$nH <- as.numeric(PA3$cH)
PA3 <- PA3[PA3$yse<9,]
PA3$fyse <- as.factor(PA3$yse)
PA3 <- PA3[PA3$fyse %in% range,]
PA3$Quantity[PA3$Treatment == "B"] <- PA3$Quantity[PA3$Treatment == "B"]*(-1)

PS3$cH <- as.character(PS3$fH)
PS3$nH <- as.numeric(PS3$cH)
PS3 <- PS3[PS3$yse<9,]
PS3$fyse <- as.factor(PS3$yse)
PS3 <- PS3[PS3$fyse %in% range,]
PS3$Quantity[PS3$Treatment == "B"] <- PS3$Quantity[PS3$Treatment == "B"]*(-1)

BP3$cH <- as.character(BP3$fH)
BP3$nH <- as.numeric(BP3$cH)
BP3 <- BP3[BP3$yse<9,]
BP3$fyse <- as.factor(BP3$yse)
BP3 <- BP3[BP3$fyse %in% range,]
BP3$Quantity[BP3$Treatment == "B"] <- BP3$Quantity[BP3$Treatment == "B"]*(-1)

SC3$cH <- as.character(SC3$fH)
SC3$nH <- as.numeric(SC3$cH)
SC3 <- SC3[SC3$yse<9,]
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
  scale_x_continuous(name = "Tree heigth (cm)",
                     breaks = c(1, 2, 3, 4, 5, 6, 7),
                     labels=c("25","75","125", "175", "225", "275", ">300"))+
  scale_y_continuous(name = "Mean number of trees per hectare",
                   breaks = c(-500,0, 500),
                   labels=c("500","0", "500"),
                   limits=c(-800, 800))+
  #scale_y_continuous(name = "Mean number of trees per hectare",
  #                    breaks = c(-1500, -1000,-500,0,500,1000, 1500),
  #                    labels=c("1500", "1000","500","0","500", "1000", "1500"),
  #                   limits= c(-1600, 1600))+
  theme_bw()+theme(legend.justification=c(0,1), 
                   legend.position=c(0,1),
                   legend.background = element_rect(fill=NA),
                   plot.title = element_text(hjust = 0.5, size=22),
                   legend.key.size = unit(2,"line"),
                   legend.text=element_text(size=20))+
  annotate("text", cex=7, label= "Rowan\nn=43", x=6, y=720) +      # all regions
  #annotate("text", cex=7, label= "Rowan\nn=15", x=6, y=1400) +      # Trøndelag
  #annotate("text", cex=7, label= "Rowan\nn=15", x=6, y=0.9) +      # Telemark
  #annotate("text", cex=7, label= "Rowan\nn=13", x=6, y=0.9) +      # Hedmark
  coord_flip()+
  ggtitle("Open plots | Exclosures")
  #ggtitle("Trøndelag region\nOpen plots | Exclosures")
  #ggtitle("Telemark region\nOpen plots | Exclosures")
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
    scale_x_continuous(name = "Tree heigth (cm)",
                       breaks = c(1, 2, 3, 4, 5, 6, 7),
                       labels=c("25","75","125", "175", "225", "275", ">300"))+
    scale_y_continuous(name = "Mean number of trees per hectare",
                       breaks = c(-500,-0, 500),
                       labels=c("500","0","500"),
                       limits=c(-600, 600))+
    #scale_y_continuous(name = "Mean number of trees per hectare",
    #                   breaks = c(-150,0, 150),
    #                   labels=c("150","0", "150"),
    #                   limits=c(-170, 170))+
    guides(fill=FALSE, linetype=FALSE)+
    annotate("text", cex=7, label= "Spruce\nn=47", x=6, y=540) +       # all regions
    #annotate("text", cex=7, label= "Spruce\nn=15", x=6, y=140) +       # Trøndelag
    #annotate("text", cex=7, label= "Spruce\nn=16", x=6, y=0.7) +       # Telemark
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
    scale_x_continuous(name = "Tree heigth (cm)",
                       breaks = c(1, 2, 3, 4, 5, 6, 7),
                       labels=c("25","75","125", "175", "225", "275", ">300"))+
    scale_y_continuous(name = "Mean number of trees per hectare",
                       breaks = c(-500,0,500),
                       labels=c("500","0","500"),
                       limits=c(-500, 500))+
    #scale_y_continuous(name = "Mean number of trees per hectare",
    #                   breaks = c(-800,-400,0, 400,800),
    #                   labels=c("800","400","0", "400", "800"),
    #                   limits=c(-1000, 1000))+
    guides(fill=FALSE, linetype=FALSE)+
    annotate("text", cex=7, label= "Pine\nn=40", x=6, y=450) +       # all regions
    #annotate("text", cex=7, label= "Pine\nn=12", x=6, y=800) +       # Trøndelag
    #annotate("text", cex=7, label= "Pine\nn=14", x=6, y=0.5) +       # Telemark
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
    scale_x_continuous(name = "Tree heigth (cm)",
                       breaks = c(1, 2, 3, 4, 5, 6, 7),
                       labels=c("25","75","125", "175", "225", "275", ">300"))+
    scale_y_continuous(name = "Mean number of trees per hectare",
                       breaks = c(-200, 0, 200),
                       labels=c("200","0","200"),
                       limits=c(-300, 300))+
    #scale_y_continuous(name = "Mean number of trees per hectare",
    #                   breaks = c(-500, 0, 500),
    #                   labels=c("500","0","500"),
    #                   limits=c(-600, 600))+
    guides(fill=FALSE, linetype=FALSE)+
    annotate("text", cex=7, label= "Birch sp\nn=47", x=6, y=270) +     #all Regions
    #annotate("text", cex=7, label= "Birch sp\nn=15", x=6, y=500) +     #Trøndelag  
    #annotate("text", cex=7, label= "Birch sp\nn=16", x=6, y=0.3) +     #Telemark
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
    scale_x_continuous(name = "Tree heigth (cm)",
                       breaks = c(1, 2, 3, 4, 5, 6, 7),
                       labels=c("25","75","125", "175", "225", "275", ">300"))+
    scale_y_continuous(name = "Mean number of trees per hectare",
                       breaks = c(-100,0, 100),
                       labels=c("100","0", "100"),
                       limits=c(-150, 150))+
    #scale_y_continuous(name = "Mean number of trees per hectare",
    #                   breaks = c(-200,0, 200),
    #                   labels=c("200","0", "200"),
    #                   limits=c(-300, 300))+
    guides(fill=FALSE, linetype=FALSE)+
    annotate("text", cex=7, label= "Goat willow\nn=28", x=6, y=125) +   # All Regions
    #annotate("text", cex=7, label= "Goat willow\nn=11", x=6, y=250) +    # Trøndelag
    #annotate("text", cex=7, label= "Goat willow\nn=28", x=6, y=0.15) +   # Telemark
    #annotate("text", cex=7, label= "Goat willow\nn=28", x=6, y=0.15) +   # Hedmark
    coord_flip()
)







getwd()
setwd("M:/Anders L Kolstad/R/R_projects/succession_paper")
#tiff("demography_plot.tiff", height = 35, width = 20, units = "cm", res = 300)
#tiff("demography_plot_Trondelag.tiff", height = 35, width = 20, units = "cm", res = 300)
#tiff("demography_plot_Telemark.tiff", height = 35, width = 20, units = "cm", res = 300)
#tiff("demography_plot_Hedmark.tiff", height = 35, width = 20, units = "cm", res = 300)
grid.draw(rbind(ggplotGrob(SA_fig), 
                ggplotGrob(PA_fig), 
                ggplotGrob(PS_fig), 
                ggplotGrob(BP_fig), 
                ggplotGrob(SC_fig),  
                size = "last"))
dev.off()


           


# BOXplot #### 
#of last treatment year

#organize data
SAbox <- aggregate(data = subset(SA2, yse == 7 & as.character(fH) > 4),
                   Quantity~LocalityName+Treatment+yse+fH,
                   FUN = mean)
SAbox$taxa <- "Rowan"
PAbox <- aggregate(data = subset(PA2, yse == 7 & as.character(fH) > 4),
                   Quantity~LocalityName+Treatment+yse+fH,
                   FUN = mean)
PAbox$taxa <- "Spruce"
PSbox <- aggregate(data = subset(PS2, yse == 7 & as.character(fH) > 4),
                   Quantity~LocalityName+Treatment+yse+fH,
                   FUN = mean)
PSbox$taxa <- "Pine"
BPbox <- aggregate(data = subset(BP2, yse == 7 & as.character(fH) > 4),
                   Quantity~LocalityName+Treatment+yse+fH,
                   FUN = mean)
BPbox$taxa <- "Birch"
SCbox <- aggregate(data = subset(SC2, yse == 7 & as.character(fH) > 4),
                   Quantity~LocalityName+Treatment+yse+fH,
                   FUN = mean)
SCbox$taxa <- "Goat willow"

BPdat <- rbind(SAbox, PAbox, PSbox, BPbox, SCbox)
head(BPdat)
BPdat2 <- aggregate(data=BPdat, 
                    Quantity~LocalityName+Treatment+taxa,
                    FUN = sum)
head(BPdat2, 20)
tail(BPdat2, 20)


BPdat3 <- dcast(BPdat2, LocalityName+taxa~Treatment, value.var = "Quantity", FUN = mean)
BPdat3$diff <- BPdat3$UB-BPdat3$B
head(BPdat3,20)
BPdat3$taxa <- as.factor(BPdat3$taxa)
str(BPdat3)
levels(BPdat3$taxa)
BPdat3$taxa <- factor(BPdat3$taxa,
                       levels = c('Spruce',
                                  'Pine',
                                  'Goat willow',
                                  'Birch',
                                  'Rowan'),ordered = TRUE)
#tiff("large_trees_boxplot.tiff", height = 15, width = 25, units = "cm", res = 300)
ggplot(BPdat3, aes(x = taxa, y = diff))+
  theme_classic()+
  coord_flip()+
  geom_hline(yintercept=0)  +
  geom_violin(colour = "black",fill = "grey80", alpha=0.1, scale = "width")+
  #geom_dotplot(binaxis='y', stackdir='center', binwidth = 1,
   #            position=position_dodge(0.5), dotsize = 0.2)+
  geom_jitter(shape=16, position=position_jitter(0.2), colour="grey", fill = "grey")+
  stat_summary(fun.y=mean, geom="point", shape=16, size=3, colour="red")+
  stat_summary(fun.y=median, geom="point",  shape=17, size=3, colour="black")+
  xlab("")+ylab("Differences in mean number of large trees per hectare\n(exclosure minus open plot)")
#dev.off()  
  


# LMM   #####
#organize data
#organize data
SAlmm <- aggregate(data = subset(SA2, yse == 7 & as.character(fH) > 4),
                   Quantity~LocalityName+Treatment+Plot,
                   FUN = sum)
SAlmm$taxa <- "Rowan"
# No variation in one of the groups (Open plots)!
ggplot()+geom_boxplot(data = SAlmm, aes(x=Treatment, y = log(SAlmm$Quantity+1)))
tapply( SAlmm$Quantity, SAlmm$Treatment, FUN = mean)
# Option 1: turn data into bernoulli responces

PAbox <- aggregate(data = subset(PA2, yse == 7 & as.character(fH) > 4),
                   Quantity~LocalityName+Treatment+yse+fH,
                   FUN = mean)
PAbox$taxa <- "Spruce"
PSbox <- aggregate(data = subset(PS2, yse == 7 & as.character(fH) > 4),
                   Quantity~LocalityName+Treatment+yse+fH,
                   FUN = mean)
PSbox$taxa <- "Pine"
BPbox <- aggregate(data = subset(BP2, yse == 7 & as.character(fH) > 4),
                   Quantity~LocalityName+Treatment+yse+fH,
                   FUN = mean)
BPbox$taxa <- "Birch"
SCbox <- aggregate(data = subset(SC2, yse == 7 & as.character(fH) > 4),
                   Quantity~LocalityName+Treatment+yse+fH,
                   FUN = mean)
SCbox$taxa <- "Goat willow"

BPdat <- rbind(SAbox, PAbox, PSbox, BPbox, SCbox)
head(BPdat)
BPdat2 <- aggregate(data=BPdat, 
                    Quantity~LocalityName+Treatment+taxa,
                    FUN = sum)




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
