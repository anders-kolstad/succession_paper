
############START ###############



# PRC
# Performing a principal responce curve analysis 
# on the field layer vegetation data from Sustherb.
# Goal - visualise the treatment effect over time 
# whilst removing the actuall time effect

# Data:
# 2 Regoins
# 31 locations (15+16)
# Two treatments (paired design)
# 10 plots per treatment
# 7-9 years time series

# Packages
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)

setwd("M:\\Anders L Kolstad\\R\\R_projects\\succession_paper")
cdat <- read_excel("community_data.xlsx", 
                             sheet = "SUSTHERB_export")




#names(cdat)


# remove regions
cdat2 <- cdat
cdat <- cdat[cdat$Region=="Trøndelag" | cdat$Region=="Telemark",]
rm(cdat2)
setwd("M:\\Anders L Kolstad\\R\\R_projects\\succession_paper")

#write.csv(unique(cdat$LocalityName), file = "LocalityNames.csv", row.names = F)

# set data and year
tail(cdat$'_Date')
Date <- as.Date(cdat$`_Date`, format = c("%Y-%m-%d"))
year <- format(Date, "%Y")
cdat <- cbind(year, Date, cdat)
head(cdat$year)


# calculating year since exclosure (yse)
yse <- ifelse(cdat$Region == "Trøndelag", as.numeric(as.character(cdat$year))-2008, 
              as.numeric(as.character(cdat$year))-2009)
cdat <- cbind(yse, cdat)
#plot(cdat$yse)
table(cdat$Region, cdat$yse)

table(cdat$LocalityName, cdat$Plot)
cdat$Plot <- ifelse(cdat$Plot > 10, cdat$Plot-10, cdat$Plot)


# remove some columns that are not vascular plant species
other_types <- c("Bare ground",
                 "Bare ground/branch", 
                 "Bare ground-stone",
                 "Bryophyta",
                 "Cow shit",
                 "Lichens",
                 "Litter",
                 "No occurrence",
                 "Sphagnum sp",
                 "Stone",
                 "Not identified")

mosses <- c("Cirriphyllum piliferum",
                      "Dicranum sp",
                      "Hylocomiastrum umbratum",
                      "Hylocomium splendens",
                      "Marchantiophyta",
                      "Plagiochila asplenioides",
                      "Plagiomnium ellipticum",
                      "Plagiomnium undulatum",
                      "Plagiothecium laetum/curvifolium",
                      "Plagiothecium undulatum",
                      "Pleurozium schreberi",
                      "Polytrichum/-astrum",
                      "Ptilidium ciliare",
                      "Ptilium crista-castrensis",
                      "Rhodobryum roseum",
                      "Rhytidiadelphus loreus",
                      "Rhytidiadelphus squarrosus/subpinnatus",
                      "Rhytidiadelphus triquetrus",
                      "Sciuro-hypnum reflexum",
                      "Sciuro-hypnum starkei")

trees <- c("Alnus incana",
           "Betula pubescens",
           "Quercus sp_",
           "Salix caprea",
           "Betula pendula",
           "Corylus avellana",
           "Picea abies",
           "Pinus sylvestris",
           "Populus tremula",
           "Prunus padus",
           "Sorbus aucuparia",
           "Juniperus communis",
           "Sambucus sp_")

cdat2 <- cdat[,!colnames(cdat) %in% c(other_types, mosses, trees)]
cdat <- cdat2
rm(cdat2)



# Turning all species into numeric
to_be_numbers <- c(16:ncol(cdat))
cdat[,to_be_numbers] <- as.numeric(as.character(unlist(cdat[,to_be_numbers])))
summary(colSums(cdat[,16:ncol(cdat)], na.rm = T))

# removing 'observed' species and retaining only measured species
cdat4 <- cdat[cdat$Method=="Point_Intercept",]
cdat <- cdat4
rm(cdat4)

# removing species with no records
cdat2 <- cdat[,c(rep(TRUE, times = 15),
                 colSums(cdat[,16:ncol(cdat)], na.rm = T) > 0)]
summary(colSums(cdat2[,16:ncol(cdat2)], na.rm=T) == 0)
cdat <- cdat2;rm(cdat2)






# look for empty plots
summary(rowSums(cdat[,16:ncol(cdat)], na.rm = T))  # obs - note some non-species at the end of df
# some zeros.
#plot(rowSums(cdat[,16:ncol(cdat)], na.rm = T))
#View(cdat[rowSums(cdat[,9:ncol(cdat)], na.rm = T) == 0,])
# Bjøllåa B plot 5 and Slindsvann UB plot 7 were not found in 2016 so data exists
# assuming the same is tru for the rest
# removing 14 rows:
cdat2 <- cdat[rowSums(cdat[,16:ncol(cdat)], na.rm = T) > 0,]
cdat <- cdat2;rm(cdat2)

# Balance?
table(cdat$LocalityName, cdat$Treatment)
# 20 rows with no location data
#View(cdat[cdat$LocalityName=="ingen",])
# These are from a site that was never initated. Safe to remove
cdat2 <- cdat[!(cdat$LocalityCode == "1RB" | cdat$LocalityCode == "1RUB"),]
cdat <- cdat2;rm(cdat2)
# balance good



# remove thinned plots
loc_year <- interaction(cdat$LocalityName, cdat$yse)
cdat <- cbind(loc_year, cdat)
#unique(cdat$loc_year)
#thinned <- c("Selbu_Flub.8", "Malvik.8", "Hi_tydal.8")
#cdat2 <- cdat[!cdat$loc_year %in% thinned,]
#cdat <- cdat2;rm(cdat2)



#Field layer height and moss depth ####
#plot(cdat$Field_avg_Height)
#identify(cdat$Field_avg_Height)
#View(cdat[83,])
# I wont analyse this data as it includes tree height.





# Species groups ####
# group grasses that proved difficult to identify in the field
# Keeping Avenella and Des Cea as they are. 
# Carex is grouped together. 




sub_other_graminoids2 <- c("Anthoxanthum odoratum",
                      #"Arrhenaterum elatius",
                      #"Bromus hordeaceus",
                      "Calamagrostis sp_",
                      #"Elymus caninus",
                      "Eriophorum vaginatum",
                      "Gras 1",
                      #"Holcus mollis",
                      "Milium effusum",
                      "Poa nemoralis",
                      "Poaceae",
                      "Pooideae sp_",
                      "Agrostis capillaris",
                      #"Alopecurus pratensis",
                      "Calamagrostis phragmitoides",
                      #"Elytrigia repens",
                      "Eriophorum angustifolium",
                      "Festuca sp",
                      "Gras 2",
                      #"Holcus lanatus",
                      "Melica nutans",
                      "Molinia caerulea",
                      "Nardus stricta")
sub_other_graminoids <- sub_other_graminoids2[sub_other_graminoids2 %in% names(cdat)]                



carex2 <- c("Cares flava"      ,               
            "Carex canescens",
            "Carex echinata",
            "Carex nigra",                       
            "Carex pallescens",
            "Carex pilulifera",
            "Carex sp_",
            "Carex vaginata")
carex <- carex2[carex2 %in% names(cdat)]



epilobium <- c("Epilobium montanum",
               "Epilobium hornemannii",
               "Epilobium sp_")

alchemilla <- c("Alchemilla glabra",
                "Alchemilla sp_")

galeopsis <- c( "Galeopsis sp_",  
                "Galeopsis tetrahit")

melampyrum <- c("Melampyrum pratense",
                "Melampyrum sp_",                    
                "Melampyrum sylvaticum")



BLS_taxa <- c("Vaccinium myrtillus",
              "Vaccinium vitis-idaea",
              "Vaccinium uliginosum",
              #"Daphne mezereum",
              "Salix sp_")

NLS_taxa <- c("Calluna vulgaris",
              "Empetrum nigrum",
              "Andromeda polifolia")



TH_taxa <- c( #"Actaea spicata", 
             "Aqonitium lycoctonium",    
             "Athyrium filix-femina" ,    
             "Blechnum spicant"  ,
             "Circium heterophyllum"  ,
             #"Cirsium arvense"         ,          
             #"Cirsium palustre"         ,         
             #"Cirsium vulgare"           ,
             "Dryopteris carthusiana"     ,        
             "Dryopteris expansa"          ,      
             "Dryopteris filix-mas"         ,
             #"Dryopteris sp_"    ,
             "Epilobium angustifolium"                     ,
             "Filipendula ulmaria"            ,
             "Geranium sylvaticum" ,
             "Geum rivale"          ,             
             "Geum urbanum" ,
             #"Matteuccia struthiopteris"                          ,
             #"Mycelis muralis",
             #"Polystichum braunii"  ,
             "Pteridium aquilinum"   ,            
             "Ranunculus acris"       ,           
             "Ranunculus repens" ,
             "Rubus idaeus"  ,
             #"Salix sp_"      ,                   
             "Solidago virgaurea"  ,
             "Stachys sylvatica"    ,              
             #"Succisa pratensis"     ,            
             #"Thistle",            
             "Phegopteris connectilis",
             "Veronica officinalis")          # small, but quite woody


SH_taxa2 <- c(
"Alchemilla sp_",
"Alchemilla glabra",
"Anemone nemorosa",                  
"Cerastium fontanum",
"Chamaepericlymenum suecicum",       
"Chrysosplenium alternifolium",
"Cystopteris fragilis",
"Dactylorhiza fuchsii",
"Dactylorhiza maculata",
"Epilobium hornemannii",
"Epilobium montanum",
"Epilobium sp_",
"Equisetum arvense",
"Equisetum pratense",
"Equisetum sylvaticum",
"Fragaria vesca",
"Galeopsis sp_","Galeopsis tetrahit",
"Goodyera repens",                   
"Gymnocarpium dryopteris",
"Hieracium sp_",
"Huperzia selago",                   
"Hypericum sp",
"Linnaea borealis",                  
"Listera cordata",
"Listera ovata",
"Lycopodium annotinum",
"Maianthemum bifolium",
"Melampyrum pratense",               
"Melampyrum sylvaticum",
"Orthilia secunda",
"Oxalis acetosella",
"Oxycoccus sp_",
"Pinguicula vulgaris",
"Potentilla erecta",
"Rubus chamaemorus",
"Rubus saxatilis",
"Rumex acetosa",
"Rumex acetosella",
"Selaginella selaginoides",
"Stellaria graminea",
"Taraxacum officinale",
"Trientalis europaea" ,              
"Veronica chamaedrys" ,              
"Viola sp_")
SH_taxa <- SH_taxa2[SH_taxa2 %in% names(cdat)]

BLG_taxa2 <- c("Deschampsia cespitosa", 
              sub_other_graminoids[!sub_other_graminoids == c("Nardus stricta", "Eriophorum vaginatum")],
              carex  )
BLG_taxa <- BLG_taxa2[BLG_taxa2 %in% names(cdat)]

NLG_taxa <- c("Avenella flexuosa",
              "Nardus stricta", 
              "Eriophorum vaginatum")



graminoids_fg <- c(BLG_taxa, NLG_taxa)
shrubs_fg <- c(BLS_taxa, NLS_taxa)
ferns_fg <- c("Athyrium filix-femina",        
              "Blechnum spicant",
              "Cystopteris fragilis",
              "Dryopteris carthusiana",
              "Dryopteris expansa",                
              "Dryopteris filix-mas",
              "Dryopteris sp_",
              "Gymnocarpium dryopteris",
              "Matteuccia struthiopteris",
              "Phegopteris connectilis",
              "Polystichum braunii", 
              "Pteridium aquilinum")

# end sp groups ####
#cdat$other_graminoids <- rowSums(cdat[,other_graminoids], na.rm = TRUE)



# estimate biomass ####


cdatBM <- cdat   
cdatBM[,16:ncol(cdatBM)] <- NULL

cdat[,16:ncol(cdat)] <- cdat[,16:ncol(cdat)]/16   # standardising the intercept frequency


# calculating biomass
cdatBM[, BLS_taxa] <- cdat[, BLS_taxa]  * 74.4101   + 1.2857
cdatBM[, NLS_taxa] <- cdat[, NLS_taxa]  * 74.184  + 9.2471
cdatBM[, TH_taxa]  <- cdat[, TH_taxa]   * 36.4849 + 4.0373
cdatBM[, SH_taxa]  <- cdat[, SH_taxa]   * 15.1209 + 0.7713
cdatBM[, BLG_taxa] <- cdat[, BLG_taxa]  * 23.3885   + 0.6384
cdatBM[, NLG_taxa] <- cdat[, NLG_taxa]  * 5.9653  + 0.8747

summary(colSums(cdatBM[,16:ncol(cdatBM)], na.rm=T))
which.max(colSums(cdatBM[,16:ncol(cdatBM)], na.rm=T))
which.min(colSums(cdatBM[,16:ncol(cdatBM)], na.rm=T))
#which(colSums(cdatBM[,16:ncol(cdatBM)], na.rm=T) == 0)


# combining species that are hard to identify in the field
cdatBM2 <- cdatBM
cdatBM$other_graminoids <- rowSums(cdatBM[, sub_other_graminoids], na.rm=T)
cdatBM$Carex <- rowSums(cdatBM[, carex], na.rm=T)
cdatBM$Epilobium <-  rowSums(cdatBM[, epilobium], na.rm=T)
cdatBM$Alchemilla <- rowSums(cdatBM[, alchemilla], na.rm=T)
cdatBM$Galeopsis <-  rowSums(cdatBM[, galeopsis], na.rm=T)
cdatBM2 <- cdatBM[,!colnames(cdatBM) %in% c(carex, epilobium, alchemilla, galeopsis)]
cdatBM <- cdatBM2;rm(cdatBM2)







# Vascular plant species 
vasc_names <- names(cdatBM[,16:ncol(cdatBM)])[!names(cdatBM[,16:ncol(cdatBM)]) %in% mosses]


# TOTAL BIOMASS
biom <- rowSums(cdatBM[,names(cdatBM) %in% vasc_names], na.rm=T)
summary(biom)
plot(biom)
cdatBM$biom <- biom


# Functional groups ####
FG <- cdatBM
grasses <- c("other_graminoids",
             "Carex",
             "Avenella flexuosa",
             "Deschampsia cespitosa")
FG$grasses <- rowSums(FG[,names(FG) %in% grasses], na.rm=TRUE)
ferns <- ferns_fg[ferns_fg %in% names(cdatBM)]
FG$ferns <- rowSums(FG[,names(FG) %in% ferns], na.rm=TRUE)
large_herbs <- TH_taxa[TH_taxa != ("Veronica officinalis")]
large_herbs <- large_herbs[!large_herbs %in% ferns]
FG$large_herbs <- rowSums(FG[,names(FG) %in% large_herbs], na.rm=TRUE)
small_herbs <- c(SH_taxa[SH_taxa %in% names(cdatBM)], "Epilobium", "Alchemilla", "Galeopsis")
FG$small_herbs <- rowSums(FG[,names(FG) %in% small_herbs], na.rm=TRUE)
shrubs_fg <- shrubs_fg[shrubs_fg %in% names(cdatBM)]
FG$shrubs <- rowSums(FG[,names(FG) %in% shrubs_fg], na.rm=TRUE)

library(reshape2)

FG2 <- select(FG, 
              yse, Region, LocalityName, Treatment, Plot,
              grasses, large_herbs, small_herbs,
              ferns, shrubs, biom, Field_avg_Height, moss_depth= 'Bryophyte height_avg_Height')
FG3 <- melt(data = FG2, id.vars = names(FG2[,1:5]), measure.vars = names(FG2[,6:ncol(FG2)]))
length(unique(FG3$LocalityName))
names(FG3)[names(FG3) == "variable"] <- "group"
names(FG3)[names(FG3) == "value"]    <- "biomass"



FG4 <- aggregate(data=FG3, biomass~yse+Treatment+group, FUN = mean)
std <- function(x) sd(x, na.rm=T)/sqrt(length(x))
FG4$SE  <- aggregate(data=FG3, biomass~yse+Treatment+group, FUN = std)[,4]
pd <- position_dodge(width=0.4)


ff1 <- ggplot(data = FG4[FG4$group=="grasses",], aes(x=yse, y= biomass, group=Treatment, linetype=Treatment))+
  ggtitle("Grasses")+
  #geom_smooth(method = "lm", size = 2, alpha = 0.2, colour = "blue", se=F, aes(linetype=NULL, group=NULL))+ 
  geom_line(size=1, position=pd)+ 
  geom_point(size =3, position=pd, aes(shape=Treatment))+
  geom_errorbar(aes(ymax=biomass+SE, ymin=biomass-SE, linetype = NULL), size=1.1, width=0.5, position=pd)+
  theme_bw()+
  xlab("")+ylab("")+
  theme(legend.justification = c(0.01, 0.99), 
        legend.position = c(0.01, 0.99),
        #legend.background = element_blank(),
        legend.key.width = unit(3,"line"),
        legend.text=element_text(size=8),
        legend.title = element_blank())+
  scale_linetype_manual(breaks = c("B", "UB"), labels = c("Open plots", "Exclosures"), values=c(1,2))+
  scale_shape_manual(breaks = c("B", "UB"), labels = c("Open plots", "Exclosures"), values=c(16,17))
  

ff2 <- ggplot(data = FG4[FG4$group=="large_herbs",], aes(x=yse, y= biomass, group=Treatment, linetype=Treatment))+
  geom_line(size=1, position=pd)+ 
  geom_point(size =3, position=pd, aes(shape=Treatment))+
  geom_errorbar(aes(ymax=biomass+SE, ymin=biomass-SE, linetype = NULL), size=1.1, width=0.5, position=pd)+
  theme_bw()+
  guides(linetype = FALSE, shape=FALSE)+
  xlab("")+ylab("")+
  ggtitle("Large herbs")+
  scale_linetype_manual(breaks = c("B", "UB"), labels = c("Open plots", "Exclosures"), values=c(1,2))
  

ff3 <- ggplot(data = FG4[FG4$group=="small_herbs",], aes(x=yse, y= biomass, group=Treatment, linetype=Treatment))+
  geom_line(size=1, position=pd)+ 
  geom_point(size =3, position=pd, aes(shape=Treatment))+
  geom_errorbar(aes(ymax=biomass+SE, ymin=biomass-SE, linetype = NULL), size=1.1, width=0.5, position=pd)+
  theme_bw()+
  guides(linetype = FALSE, shape=FALSE)+
  xlab("")+
  ylab(expression(paste("Biomass (g m"^"-2", ")")))  +
  ggtitle("Small herbs")+
  scale_linetype_manual(breaks = c("B", "UB"), labels = c("Open plots", "Exclosures"), values=c(1,2))

ff4 <- ggplot(data = FG4[FG4$group=="ferns",], aes(x=yse, y= biomass, group=Treatment, linetype=Treatment))+
  geom_line(size=1, position=pd)+ 
  geom_point(size =3, position=pd, aes(shape=Treatment))+
  geom_errorbar(aes(ymax=biomass+SE, ymin=biomass-SE, linetype = NULL), size=1.1, width=0.5, position=pd)+
  theme_bw()+
  guides(linetype = FALSE, shape=FALSE)+
  xlab("")+ylab("")+
  ggtitle("Ferns")+
  scale_linetype_manual(breaks = c("B", "UB"), labels = c("Open plots", "Exclosures"), values=c(1,2))

ff5 <- ggplot(data = FG4[FG4$group=="shrubs",], aes(x=yse, y= biomass, group=Treatment, linetype=Treatment))+
  geom_line(size=1, position=pd)+ 
  geom_point(size =3, position=pd, aes(shape=Treatment))+
  geom_errorbar(aes(ymax=biomass+SE, ymin=biomass-SE, linetype = NULL), size=1.1, width=0.5, position=pd)+
  theme_bw()+
  guides(linetype = FALSE, shape=FALSE)+
  xlab("")+ylab("")+
  ggtitle("Dwarf shrubs")+
  scale_linetype_manual(breaks = c("B", "UB"), labels = c("Open plots", "Exclosures"), values=c(1,2))


ff6 <- ggplot(data = FG4[FG4$group=="biom",], aes(x=yse, y= biomass, group=Treatment, linetype=Treatment))+
  geom_line(size=1, position=pd)+ 
  geom_point(size =3, position=pd, aes(shape=Treatment))+
  geom_errorbar(aes(ymax=biomass+SE, ymin=biomass-SE, linetype = NULL), size=1.1, width=0.5, position=pd)+
  theme_bw()+
  guides(linetype = FALSE, shape=FALSE)+
  ylab("")+
  xlab("Years since exclusion")+
  ggtitle("Field layer biomass")+
  scale_linetype_manual(breaks = c("B", "UB"), labels = c("Open plots", "Exclosures"), values=c(1,2))
  



#ggplot(data = FG4[FG4$group=="Field_avg_Height",], aes(x=yse, y= biomass, group=Treatment, linetype=Treatment))+
#  geom_line(size=1, position=pd)+ 
#  geom_point(size =3, position=pd, aes(shape=Treatment))+
#  geom_errorbar(aes(ymax=biomass+SE, ymin=biomass-SE, linetype = NULL), size=1.1, width=0.5, position=pd)+
#  theme_bw()+
#  ylab("Field layer height (cm)")+
#  xlab("Years since exclusion")+
#  ggtitle("Field layer height")


setwd("M:\\Anders L Kolstad\\R\\R_projects\\succession_paper")
load("prod_index_telemark_and_trondelag.RData")
#productivity <- read.csv("M:/Anders L Kolstad/R/R_projects/succession_paper/prod_index_ID_name_converter.csv", sep=";")

FG3$productivity <- productivity$productivity[match(FG3$LocalityName, productivity$LocalityName)]
summary(FG3$productivity) # no NA's
FG3$uniquePlot <- paste(FG3$LocalityName, FG3$Treatment, FG3$Plot, sep = "_")
modDat <- FG3[FG3$yse==8,]
#yearZ <- FG3[FG3$yse == 0,]
#modDat$start_biomass <- yearZ$biomass[match(modDat$uniquePlot, yearZ$uniquePlot)]
#modDat$biomass2 <- modDat$biomass-modDat$start_biomass
modDat$Treatment <- as.factor(modDat$Treatment)







# ***********************************#
# ***********************************#
# LMM                             ####
# ***********************************#
# ***********************************#

#*****************************************
# -large herbs ####

library(lmerTest)
modFS <- lmerTest::lmer(log(biomass+1)~Treatment*productivity + (1|Region/LocalityName), 
                         data = modDat[modDat$group == "large_herbs",], REML = F)
plot(modFS) # tja
plot(modDat$productivity[modDat$group == "large_herbs"], resid(modFS))   # ok
plot(modDat$Treatment[modDat$group == "large_herbs"], resid(modFS))      # ok
summary(modFS)
modFS2 <- update(modFS, .~. -Treatment:productivity)
modFS3 <- update(modFS2, .~. -Treatment)
modFS4 <- update(modFS2, .~. -productivity)
AIC(modFS, modFS2, modFS3, modFS4)
#anova(modFS, modFS2)
AIC(modFS)-AIC(modFS2)
AIC(modFS)-AIC(modFS3)
AIC(modFS)-AIC(modFS4)

modFS <- lmerTest::lmer(log(biomass+1)~Treatment*productivity + (1|Region/LocalityName), 
                        data = modDat[modDat$group == "large_herbs",], REML = T)
summary(modFS)
ICCr <- 0.1462/(0.1462+0.2401+0.8940)
ICCl <- 0.2401/(0.1462+0.2401+0.8940)


library(glmmTMB)
modFS2 <- glmmTMB(biomass+1~Treatment*productivity + (1|Region/LocalityName), 
                        data = modDat[modDat$group == "large_herbs",], family = list(family = "Gamma", link = "log"))

plot(fitted(modFS2), resid(modFS2)) # funell, but with Gamma i'm not sure thats a problem. It should be taken care of by the dispersion parameter
plot(modDat$productivity[modDat$group == "large_herbs"], resid(modFS2)) # this one is worse
plot(modDat$Treatment[modDat$group == "large_herbs"], resid(modFS2)) # and this is not good
# I think the log transformation was best in this case






#*****************************************
# -grasses ####
modG <- lmerTest::lmer(log(biomass+1)~Treatment*productivity + (1|Region/LocalityName), 
                        data = modDat[modDat$group == "grasses",], REML = F)
plot(modG) # tja
plot(modDat$productivity[modDat$group == "grasses"], resid(modG))   # ok
plot(modDat$Treatment[modDat$group == "grasses"], resid(modG))      # ok
summary(modG)
modG2 <- update(modG, .~. -Treatment:productivity)
modG3 <- update(modG2, .~. -Treatment)
modG4 <- update(modG2, .~. -productivity)
AIC(modG, modG2, modG3, modG4)
AIC(modG2) - AIC(modG)
AIC(modG2) - AIC(modG3)
AIC(modG2) - AIC(modG4)

modG2 <- lmerTest::lmer(log(biomass+1)~Treatment+productivity + (1|Region/LocalityName), 
                        data = modDat[modDat$group == "grasses",])
summary(modG2)



anova(modG2, modG3)
anova(modG2, modG4)

ICCr <- 0
ICCl <- 0.8301/(0.8301+0.9349)


modFS <- lmerTest::lmer(log(biomass+1)~Treatment+productivity + (1|Region/LocalityName), 
                        data = modDat[modDat$group == "large_herbs",])










#*****************************************
# -small herbs ####
modSH <- lmerTest::lmer(log(biomass+1)~Treatment*productivity + (1|Region/LocalityName), 
                       data = modDat[modDat$group == "small_herbs",], REML =F)
plot(modSH) # tja
plot(modDat$productivity[modDat$group == "small_herbs"], resid(modSH))   # ok
plot(modDat$Treatment[modDat$group == "small_herbs"], resid(modSH))      # ok

modSH_add <- update(modSH, .~. - Treatment:productivity)
modSH_pi <- update(modSH_add, .~. - Treatment)
modSH_exc <- update(modSH_add, .~. - productivity)
AIC(modSH, modSH_add, modSH_exc, modSH_pi)
AIC(modSH)-AIC(modSH_add)
AIC(modSH)-AIC(modSH_exc)
AIC(modSH)-AIC(modSH_pi)

modSH <- lmerTest::lmer(log(biomass+1)~Treatment*productivity + (1|Region/LocalityName), 
                        data = modDat[modDat$group == "small_herbs",], REML =T)
summary(modSH)
summary(modSH_exc)
summary(modSH_pi)

ICCr <- 0.3256/(0.3256+0.4837+0.6492)
ICCl <- 0.4837/(0.3256+0.4837+0.6492)









#*****************************************
# -ferns ####
modF <- lmerTest::lmer(log(biomass+1)~Treatment*productivity + (1|Region/LocalityName), 
                        data = modDat[modDat$group == "ferns",], REML = F)
plot(modF) # tja
plot(modDat$productivity[modDat$group == "ferns"], resid(modF))   # ok
plot(modDat$Treatment[modDat$group == "ferns"], resid(modF))      # ok

modF_add <- update(modF, .~. -Treatment:productivity)
modF_exc <- update(modF_add, .~. -productivity)
modF_pi <- update(modF_add, .~. -Treatment)
AIC(modF, modF_add, modF_exc, modF_pi)
AIC(modF)-AIC(modF_add)
AIC(modF)-AIC(modF_exc)
AIC(modF)-AIC(modF_pi)

modF <- lmerTest::lmer(log(biomass+1)~Treatment*productivity + (1|Region/LocalityName), 
                       data = modDat[modDat$group == "ferns",], REML = T)
summary(modF)
ICCr <- 0
ICCl <- 0.8961/(0.8961+0.7190)









#*****************************************
# -shrubs ####
modS <- lmerTest::lmer(log(biomass+1)~Treatment*productivity + (1|Region/LocalityName), 
                       data = modDat[modDat$group == "shrubs",], REML = F)
plot(modS) # tja
plot(modDat$productivity[modDat$group == "shrubs"], resid(modS))   # ok, sligtly increasing
plot(modDat$Treatment[modDat$group == "shrubs"], resid(modS))      # ok
summary(modS)


modS_add <- update(modS, .~. -Treatment:productivity)
modS_exl <- update(modS_add, .~. -Treatment)
modS_pi <- update(modS_add, .~. -productivity)
AIC(modS, modS_add, modS_exl, modS_pi)
BIC(modS, modS_add, modS_exl, modS_pi)
AIC(modS_add)-AIC(modS)
AIC(modS_add)-AIC(modS_pi)
AIC(modS_add)-AIC(modS_exl)
anova(modS_add, modS_exl)
anova(modS_add, modS_pi)

modS_add <- lmerTest::lmer(log(biomass+1)~Treatment+productivity + (1|Region/LocalityName), 
                       data = modDat[modDat$group == "shrubs",])
summary(modS_add)

ICCr <- 0.4674/(0.4674+0.9791+1.5550)
ICCl <- 0.9791/(0.4674+0.9791+1.5550)









#*****************************************
# -total biomass  ####
library(lmerTest)
modTB <- lmerTest::lmer(log(biomass+1)~Treatment*productivity + (1|Region/LocalityName), 
                        data = modDat[modDat$group == "biom",], REML = F)
plot(modTB) # ok
plot(modDat$productivity[modDat$group == "biom"], resid(modTB))   # ok
plot(modDat$Treatment[modDat$group == "biom"], resid(modTB))      # ok
summary(modTB)
modTB_add <- update(modTB, .~. -Treatment:productivity)
modTB_pi <- update(modTB_add, .~. -Treatment)
modTB_exc <- update(modTB_add, .~. -productivity)
AIC(modTB, modTB_add, modTB_exc, modTB_pi)


#anova(modFS, modFS2)
AIC(modTB)-AIC(modTB_add)
AIC(modTB)-AIC(modTB_exc)
AIC(modTB)-AIC(modTB_pi)

modTB <- lmerTest::lmer(log(biomass+1)~Treatment*productivity + (1|Region/LocalityName), 
                        data = modDat[modDat$group == "biom",], REML = T)
summary(modTB)
ICCr <- 0.14581/(0.14581+0.05562+0.33360)
ICCl <- 0.05562/(0.14581+0.05562+0.33360)




#*****************************************
# -Moss depth  ####
#the response is called biomass but is actually height in cm
mossDat <- modDat[modDat$Region=="Trøndelag" & modDat$group=="moss_depth",]
mossDat <- mossDat[!is.na(mossDat$biomass),]
mod_moss <- lmerTest::lmer(biomass~Treatment*productivity + (1|LocalityName), 
                        data = mossDat, REML = F)
plot(mod_moss) # ok
plot(mossDat$productivity, resid(mod_moss))   # ok
plot(mossDat$Treatment, resid(mod_moss))      # ok
summary(mod_moss)
mod_moss_add <- update(mod_moss, .~. -Treatment:productivity)
mod_moss_pi <- update(mod_moss_add, .~. -Treatment)
mod_moss_exc <- update(mod_moss_add, .~. -productivity)
AIC(mod_moss, mod_moss_add, mod_moss_exc, mod_moss_pi)


#anova(modFS, modFS2)
AIC(mod_moss_pi)-AIC(mod_moss)
AIC(mod_moss_pi)-AIC(mod_moss_add)
AIC(mod_moss_pi)-AIC(mod_moss_exc)

mod_moss_pi <- lmerTest::lmer(biomass~productivity + (1|LocalityName), 
                              data = mossDat, REML = T)
summary(mod_moss_pi)
ICCl <- 1.911/(1.911+7.070)




#*****************************************
# -Moss depth variance  ####
mossDat2 <- aggregate(data=mossDat, biomass~LocalityName+Treatment, FUN = var)
boxplot(mossDat2$biomass~ mossDat2$Treatment)
mossDat2$productivity <- productivity$productivity[match(mossDat2$LocalityName, productivity$LocalityName)]

mod_mossV <- lmerTest::lmer(biomass~Treatment*productivity + (1|LocalityName), 
                           data = mossDat2, REML = F)
plot(mod_mossV) # ok
plot(mossDat2$productivity, resid(mod_mossV))   # ok
plot(mossDat2$Treatment, resid(mod_mossV))      # ok
summary(mod_mossV)
mod_mossV_add <- update(mod_mossV, .~. -Treatment:productivity)
mod_mossV_pi <- update(mod_mossV_add, .~. -Treatment)
mod_mossV_exc <- update(mod_mossV_add, .~. -productivity)
AIC(mod_mossV, mod_mossV_add, mod_mossV_exc, mod_mossV_pi)


#anova(modFS, modFS2)
AIC(mod_mossV_pi)-AIC(mod_mossV)
AIC(mod_mossV_pi)-AIC(mod_mossV_add)
AIC(mod_mossV_pi)-AIC(mod_mossV_exc)

mod_mossV_pi <- lmerTest::lmer(biomass~productivity + (1|LocalityName), 
                               data = mossDat2, REML = T)
summary(mod_mossV_pi)
ICCl <- 0

#mossBoxDat <- aggregate(data=mossDat, biomass~LocalityName+Treatment, FUN = mean)
#boxplot(mossBoxDat$biomass~mossBoxDat$Treatment)




# Canopy Cover index ####
# CCI was analysed in the 2017 ECOSYSTEMS paper. Here I only look at the within plot variance in CCI 
# data from Trøndelag sites, in year 2016
CCI <- read_excel("CanopyCover_SUSTHERB_export.xlsx", 
                  sheet = "SUSTHERB_export")
# remove thinned sites
unique(CCI$LocalityName)
thinned <- c("Selbu_Flub" , "Hi_tydal", "Malvik" )
CCI2 <- CCI[!CCI$LocalityName %in% thinned,]

uniquePlot <- interaction(CCI2$LocalityName, CCI2$Treatment, CCI2$Plot)
CCI3 <- cbind(uniquePlot, CCI2)
CCI4 <- CCI3[CCI3$Plot != "Experiment area",]
CCI_subplot <- aggregate(data = CCI4, CanopyCoverIndex~uniquePlot+LocalityName+Treatment+Plot, FUN =mean, na.rm=T)
CCI_plot <- aggregate(data = CCI_subplot, CanopyCoverIndex~LocalityName+Treatment, FUN =mean, na.rm=T)
plot(CCI_plot$CanopyCoverIndex~as.factor(CCI_plot$Treatment)) # this is the significant relationship that is analysed elsewere

CCI_plotVar <- aggregate(data = CCI_subplot, CanopyCoverIndex~LocalityName+Treatment, FUN = var, na.rm=T)
plot(CCI_plotVar$CanopyCoverIndex~as.factor(CCI_plotVar$Treatment)) # this is the significant relationship that is analysed elsewere


CCI_plotVar$productivity <- productivity$productivity[match(CCI_plotVar$LocalityName, productivity$LocalityName)]
CCI_plotVar$Treatment <- as.factor(CCI_plotVar$Treatment)


mod_CCIvar <- lmerTest::lmer(CanopyCoverIndex~Treatment*productivity + (1|LocalityName), 
                            data = CCI_plotVar, REML = F)
plot(mod_CCIvar) # ok
plot(CCI_plotVar$productivity, resid(mod_CCIvar))   # ok
plot(CCI_plotVar$Treatment, resid(mod_CCIvar))      # ok
summary(mod_CCIvar)

mod_CCIvar_add <- update(mod_CCIvar, .~. -Treatment:productivity)
mod_CCIvar_pi <- update(mod_CCIvar_add, .~. -Treatment)
mod_CCIvar_exc <- update(mod_CCIvar_add, .~. -productivity)
AIC(mod_CCIvar, mod_CCIvar_add, mod_CCIvar_exc, mod_CCIvar_pi)


#anova(modFS, modFS2)
AIC(mod_CCIvar_add)-AIC(mod_CCIvar)
AIC(mod_CCIvar_add)-AIC(mod_CCIvar_pi)
AIC(mod_CCIvar_add)-AIC(mod_CCIvar_exc)

mod_CCIvar_add <- lmerTest::lmer(CanopyCoverIndex~Treatment+productivity + (1|LocalityName), 
                                 data = CCI_plotVar, REML = T)

mod_CCIvar_pi <- update(mod_CCIvar_add, .~. -Treatment)
mod_CCIvar_exc <- update(mod_CCIvar_add, .~. -productivity)
anova(mod_CCIvar_add, mod_CCIvar_pi)
anova(mod_CCIvar_add, mod_CCIvar_exc)

summary(mod_CCIvar_add)


ICCl <- 2.814e-10/(2.814e-10 + 9.319e+04)



# ***********************************#
# ***********************************#
# Interaction plots               ####
# ***********************************#
# ***********************************#

modDat2 <- aggregate(data = modDat,
                     biomass~Treatment+LocalityName+group,
                     FUN = mean)
modDat3 <- dcast(data = modDat2, 
                 LocalityName+group~Treatment,
                 value.var = "biomass")
modDat3$diff <- modDat3$UB - modDat3$B
modDat3$productivity <- modDat$productivity[match(modDat3$LocalityName, modDat$LocalityName)]


ggplot(data = modDat3, aes(x = productivity, y = diff, group = group, colour = group))+
  geom_smooth(method = "lm", se = F)+
  ylab("Relative difference in biomass\n(Exclosure minus Open plots)")+
  xlab("Site productivity")

# expect the larges interaction effects for large herbs, shrubs, and ferns
levels(modDat3$group)

F1 <- ggplot(data = modDat3[modDat3$group == "grasses",], aes(x = productivity, y = diff))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0, size=2, colour="grey")+
  #geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=0),
  #          fill="lightgreen")+
  #theme(text = element_text(size=15))+
  geom_point(size = 3, stroke = 2, shape =1)+
  #geom_smooth(method = "lm", se = T, size = 2, colour = "black")+
  ylab("")+
  xlab("")
  #ggtitle("Grasses")
F2 <- ggplot(data = modDat3[modDat3$group == "large_herbs",], aes(x = productivity, y = diff))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0, size=2, colour = "grey")+
  #geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=0),
  #          fill="lightgreen")+
  #theme(text = element_text(size=15))+
  geom_point(size = 3, stroke = 2, shape =1)+
  geom_smooth(method = "lm", se = F, size = 2, colour = "black")+
  ylab("")+
  xlab("")
#  ggtitle("Large herbs")
F3 <- ggplot(data = modDat3[modDat3$group == "small_herbs",], aes(x = productivity, y = diff))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0, size=2, colour = "grey")+
  #geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=0),
  #          fill="lightgreen")+
  #theme(text = element_text(size=15))+
  geom_point(size = 3, stroke = 2, shape =1)+
  geom_smooth(method = "lm", se = F, size = 2, colour = "black")+
  ylab(expression(paste(Delta, " Biomass (g m"^"-2", ")")))+
  xlab("")
#  ggtitle("Small herbs")
F4 <- ggplot(data = modDat3[modDat3$group == "ferns",], aes(x = productivity, y = diff))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0, size=2, colour = "grey")+
  #geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=0),
  #          fill="lightgreen")+
  #theme(text = element_text(size=15))+
  geom_point(size = 3, stroke = 2, shape =1)+
  geom_smooth(method = "lm", se = F, size = 2, colour = "black")+
  ylab("")+
  xlab("")
#  ggtitle("Ferns")
F5 <- ggplot(data = modDat3[modDat3$group == "shrubs",], aes(x = productivity, y = diff))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0, size=2, colour = "grey")+
  #geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=0),
  #          fill="lightgreen")+
  #theme(text = element_text(size=15))+
  geom_point(size = 3, stroke = 2, shape =1)+
  #geom_smooth(method = "lm", se = T, size = 2, colour = "black")+
  ylab("")+
  xlab("")
  #ggtitle("Dwarf shrubs")
F6 <- ggplot(data = modDat3[modDat3$group == "biom",], aes(x = productivity, y = diff))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0, size=2, colour = "grey")+
  #geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=0),
  #          fill="lightgreen")+
  #theme(text = element_text(size=15))+
  geom_point(size = 3, stroke = 2, shape =1)+
  geom_smooth(method = "lm", se = F, size = 2, colour = "black")+
  ylab("")+
  xlab("Site productivity")
#ggtitle("Total biomass")

#grid.draw(rbind(ggplotGrob(F1), ggplotGrob(F2), 
#                ggplotGrob(F3), ggplotGrob(F4),ggplotGrob(F5),
#                size = "last"))







#******************************************#
#******************************************#
# Boxplot                               ####
#******************************************#
#******************************************#

f1 <- ggplot(data = FG4[FG4$group=="grasses",], aes(x = Treatment, y = biomass+1))+
  theme_classic()+
  #theme(text = element_text(size=15))+
  geom_boxplot()+scale_y_log10()+
  ylab("")+
  xlab("")+
  scale_x_discrete("", labels = c("B" = "Open plots", "UB" = "Exclosures"))
  #ggtitle("Grasses")
f2 <- ggplot(data = FG4[FG4$group=="large_herbs",], aes(x = Treatment, y = biomass+1))+
  theme_classic()+
  #theme(text = element_text(size=15))+
  geom_boxplot()+scale_y_log10()+
  ylab("")+
  xlab("")+
  scale_x_discrete("", labels = c("B" = "Open plots", "UB" = "Exclosures"))
#  ggtitle("Large herbs")
f3 <- ggplot(data = FG4[FG4$group=="small_herbs",], aes(x = Treatment, y = biomass+1))+
  theme_classic()+
  #theme(text = element_text(size=15))+
  geom_boxplot()+scale_y_log10()+
  ylab(expression(paste("Biomass (g m"^"-2", ")")))+
  xlab("")+
  scale_x_discrete("", labels = c("B" = "Open plots", "UB" = "Exclosures"))
  #ggtitle("Small herbs")
f4 <- ggplot(data = FG4[FG4$group=="ferns",], aes(x = Treatment, y = biomass+1))+
  theme_classic()+
  #theme(text = element_text(size=15))+
  geom_boxplot()+scale_y_log10()+
  ylab("")+
  xlab("")+
  scale_x_discrete("", labels = c("B" = "Open plots", "UB" = "Exclosures"))
#  ggtitle("Ferns")
f5 <- ggplot(data = FG4[FG4$group=="shrubs",], aes(x = Treatment, y = biomass+1))+
  theme_classic()+
  #theme(text = element_text(size=15))+
  geom_boxplot()+scale_y_log10()+
  ylab("")+
  xlab("Treatment")+
  ggtitle("")+
  scale_x_discrete("Treatment", labels = c("B" = "Open plots", "UB" = "Exclosures"))




# ********************************************#
# ********************************************#
# plot grid                                ####
# ********************************************#
# ********************************************#

library(cowplot)

setwd("M:/Anders L Kolstad/R/R_projects/succession_paper")
tiff("Functional_groups_all.tiff", height = 40, width = 20, units = "cm", res=300)

plot_grid(ff1, F1,
          ff2, F2,
          ff3, F3,
          ff4, F4,
          ff5, F5,
          ff6, F6,
          align = "hv",
          ncol=2,rel_widths = c(1,1))
dev.off()  



# other functional group plots ....  ####
FG5 <- dcast(FG4, yse+group~Treatment, value.var = "biomass", fun.aggregate = mean)
FG5$diff <- FG5$UB-FG5$B


ggplot(data = FG5, aes(x=yse, y= diff, group=group, colour=group, linetype= group))+
  geom_line(size=2)+theme_bw()+geom_hline(yintercept = 0, size=2)+
  xlab("Years since exclosure")+ylab("Grams of biomass (Exclosure - open plots)")+
  theme(legend.position="top")+
  theme(legend.title=element_blank())
  
FG6 <- aggregate(data=FG3, biomass~yse+LocalityName+Treatment+group, FUN = mean)
FG6 <- dcast(FG6, yse+group+LocalityName~Treatment, value.var = "biomass", fun.aggregate = mean)
FG6$diff <- FG6$UB-FG6$B
exl <- rownames(FG6[FG6$group=="ferns" & FG6$diff >80,])
FG7 <- FG6[!rownames(FG6) %in% exl,]

#tiff("Functional_groups_facet.tiff", height = 10, width = 10, units = "cm", res=300)
ggplot(data = FG7, aes(x=yse, y= diff))+
  geom_point(size=2)+
  theme_bw()+
  geom_hline(yintercept = 0, size=2)+
  geom_smooth(method = "lm")+
  xlab("Years since exclosure")+ylab("Grams of biomass (Exclosure - open plots)")+
  theme(legend.position="top")+
  theme(legend.title=element_blank())+
  facet_wrap( ~ group, scales = "free")
#dev.off()


#tiff("Functional_groups_onePlot.tiff", height = 10, width = 10, units = "cm", res=300)
ggplot(data = FG7, aes(x=yse, y= diff, group = group, colpur = group))+
  theme_bw()+
  geom_hline(yintercept = 0, size=2)+
  geom_smooth(method = "lm", se = F, size = 3, aes(colour = group))+
  xlab("Years since exclosure")+ylab("Grams of biomass (Exclosure - open plots)")+
  theme(legend.position="top")+
  theme(legend.title=element_blank())
#dev.off()  







# ********************************************#
# ********************************************#
# PRC                                     ####
# ********************************************#
# ********************************************#
library(vegan)
library(reshape2)


cdatBM[is.na(cdatBM)] <- 0
cdatx <- cdatBM


# removing singletons ####
# and species rarer than 10%
# -- this shold probaly only be done for the PRS analysis, not the species richness etc.
# total nr location:year = 155 (-3)
#length(unique(cdatBM$loc_year))
# define cut of at 15

names(cdatx)
cdatx2 <- cdatx
cdatx2 <- cdatx2[,c(ncol(cdatx2), 1:ncol(cdatx2)-1)]
names(cdatx2)

cdatx2 <- cdatx2[,c(rep(TRUE, times=16), colSums(cdatx2[,17:ncol(cdatx2)] >1, na.rm=T) >  15)]




# Prepare data
long <- melt(data = cdatx2, id.vars = c("yse", "Region", "LocalityName", "Treatment", "Plot") , 
             measure.vars = names(cdatx2[,17:ncol(cdatx2)]))
names(long)[names(long) == "variable"] <- "taxa"
names(long)[names(long) == "value"]    <- "biomass"

wide <- dcast(data = long, yse + Region + LocalityName + Treatment ~ taxa, 
              value.var = "biomass", fun.aggregate = mean)
wide2 <- dcast(data = long, yse + Region + LocalityName + Treatment+Plot ~ taxa, 
              value.var = "biomass", fun.aggregate = mean)
thinned <- c("Selbu_Flub", "Hi_tydal", "Malvik")
unique(wide2$LocalityName)
wide2_red <- wide2[!wide2$LocalityName %in% thinned,]

#save(wide2, file ="PRCdat.RData")


#  partial PRC method on plot level data with Hellinger transformed data:
Hell_dat <- cbind(
  wide2[,1:5],
  decostand(wide2[,6:ncol(wide2)], method="hellinger")
)

rda.out2 <- rda(Hell_dat[,6:ncol(Hell_dat)] ~ Hell_dat$LocalityName) # equivalent of "rda.out = rda(X ~ Condition(Z))"
rda.out2 # location explains 34%
#plot(rda.out2)
rda.res2 <- as.data.frame(residuals(rda.out2) )   # does this carry only the residuals from the first axis?
Hell_dat2 <- cbind(Hell_dat[,1:5], rda.res2)


prc.out3 <- prc(Hell_dat2[,6:ncol(Hell_dat2)], as.factor(Hell_dat2$Treatment), 
                as.factor(Hell_dat2$yse))
ctrl <- how(plots = Plots(Hell_dat2$LocalityName),
            within = Within(type = "series"), nperm = 1000    )
anova(prc.out3,permutations = ctrl)
sum_mod_prc3 <- summary(prc.out3, scaling = 2)

#species <- colSums(rda.res)
par(mar=c(5,5,10,10))
prc.out3 # time: 11%; treatment = 2%; residual variance = 87%

#tiff("PRC.tiff", height = 10, width = 10, units = "cm", res=300)
plot(prc.out3, select = abs(sum_mod_prc3$sp) > 0.05, scaling = 2,legpos = NULL, xlab = "Year since exclosure")
dev.off()




# PRC on plot level data
prc.out <- prc(log(wide[,5:ncol(wide)]+1), 
                      as.factor(wide$Treatment), 
                      as.factor(wide$yse))

sum_mod_prc <- summary(prc.out, scaling = 0)
#species <- colSums(rda.res)
par(mar=c(5,5,10,10))
prc.out
#summary(prc.out)
plot(prc.out, select = abs(sum_mod_prc$sp) > 0.1, scaling = 0)
ctrl <- how(plots = Plots(wide$LocalityName),
            within = Within(type = "series"), nperm = 500)
anova(prc.out, permutations = ctrl, first=TRUE)
# not significant 



# PRC on SUBplot level data
prc.out.sub <- prc(log(wide2[,6:ncol(wide2)]+1), 
               as.factor(wide2$Treatment), 
               as.factor(wide2$yse))
prc.out.sub # inertia very low - ignoring locations and plot level variation here
sum_mod_prc_sub <- summary(prc.out.sub, scaling = 2)
#species <- colSums(rda.res)
par(mar=c(5,5,10,10))



plot(prc.out.sub, select = abs(sum_mod_prc_sub$sp) > 0.12, scaling = 2, 
     legpos = NULL, xlab = "Year since exclosure")
dev.off()
# results comparable


# PRS - removing thinned sites
prc.out.sub2 <- prc(log(wide2_red[,6:ncol(wide2_red)]+1), 
                   as.factor(wide2_red$Treatment), 
                     as.factor(wide2_red$yse))
prc.out.sub2 # inertia very low - ignoring locations and plot level variation here
sum_mod_prc_sub2 <- summary(prc.out.sub2, scaling = 2)
#species <- colSums(rda.res)
par(mar=c(5,5,10,10))

plot(prc.out.sub2, select = abs(sum_mod_prc_sub2$sp) > 0.12, scaling = 2, 
     legpos = NULL, xlab = "Year since exclosure")
# results comparable



## Locations are randomized, we have a time series, and are only
## interested in the first axis
ctrl <- how(plots = Plots(wide2$LocalityName),
            within = Within(type = "series"), nperm = 1000    )
anova(prc.out.sub, permutations = ctrl, first=TRUE)



# partial PRC method on aggregated data:
rda.out <- rda(log(wide[,5:ncol(wide)]+1) ~ wide$LocalityName) # equivalent of "rda.out = rda(X ~ Condition(Z))"
rda.out # location explains 70%
# unconstrained axes  all have equally high eigenvalues!
rda.res <- residuals(rda.out)    # does this carry only the residuals from the first axis?
rda.resX <- as.data.frame(rda.res)
wideX <- cbind(wide[,1:4], rda.resX)
head(wideX)
prc.out2 <- prc(wideX[,5:ncol(wideX)], as.factor(wideX$Treatment), 
               as.factor(wideX$yse))

ctrl <- how(plots = Plots(wideX$LocalityName),
            within = Within(type = "series"), nperm = 1000    )
anova(prc.out2,permutations = ctrl)

sum_mod_prc2 <- summary(prc.out2, scaling = 2)
#species <- colSums(rda.res)
par(mar=c(5,5,10,10))
prc.out2
plot(prc.out2, select = abs(sum_mod_prc2$sp) > 0.1, scaling = 2)








# plot time series ####

#long$cyse <- as.character(long$yse)
# get means per plot and year
timeS <- aggregate(data = long, 
                   biomass~Region+yse+LocalityName+Treatment+taxa, FUN = mean)
timeS$biomass_l <- log(timeS$biomass+1)
#timeS2 <- aggregate(data = timeS, 
#                   biomass~cyse+Treatment+taxa, FUN = mean)
timeS3 <- dcast(data = timeS, yse+taxa+LocalityName~Treatment, value.var = "biomass_l", fun.aggregate = mean)
timeS3$diff <- timeS3$UB-timeS3$B 
timeS3 <- timeS3[timeS3$diff!=0,]

pos_sp <- c(#"Calluna vulgaris",
        "Epilobium angustifolium",
        "Filipendula ulmaria",
        "Dryopteris expansa")
        #"Rubus idaeus",
        #"Gymnocarpium dryopteris",
        #"Vaccinium uliginosum",
        #"Pteridium aquilinum")
neg_sp <- c(#"Vaccinium vitis-idaea",
            "Vaccinium myrtillus",
            "Potentilla erecta",
            "Avenella flexuosa")
            #"Chamaepericlymenum suecicum",
            #"Melampyrum sylvaticum")
library(gridExtra)
grid.arrange(
ggplot(data = timeS3[timeS3$taxa %in% pos_sp,], aes(x=yse, y=diff))+
  geom_point()+
  geom_smooth(method = "loess")+
  theme_bw()+
  geom_hline(yintercept = 0, size = 2)+
  facet_grid(. ~ taxa, scales = "fixed"),
ggplot(data = timeS3[timeS3$taxa %in% neg_sp,], aes(x=yse, y=diff))+
  geom_point()+
  geom_smooth(method = "loess")+
  theme_bw()+
  geom_hline(yintercept = 0, size = 2)+
  facet_grid(. ~ taxa, scales = "fixed"), 
nrow=2)

ggplot(data = timeS[timeS$taxa=="Vaccinium myrtillus",], aes(x=yse, y=biomass, group = LocalityName))+
  geom_line()+
  theme_classic()+
  facet_grid(. ~ Treatment+Region, scales = "fixed")

ggplot(data = timeS[timeS$taxa=="Vaccinium vitis-idaea",], aes(x=yse, y=biomass))+
  theme_bw()+
  geom_smooth()+
  geom_smooth(method = "lm")+
  geom_point()+
  facet_grid(. ~ Treatment, scales = "fixed")
ggplot(data = timeS[timeS$taxa=="Calluna vulgaris",], aes(x=yse, y=biomass))+
  theme_bw()+
  geom_smooth()+
  geom_smooth(method = "lm")+
  geom_point()+
  facet_grid(. ~ Treatment, scales = "fixed")
ggplot(data = timeS[timeS$taxa=="Vaccinium myrtillus",], aes(x=yse, y=biomass))+
  theme_classic()+
  geom_smooth()+
  geom_smooth(method = "lm")+
  geom_point()+
  facet_grid(. ~ Treatment, scales = "fixed")



#outlier in year4? - Vac.myr
#plot(timeS2$biomass)
#identify(timeS2$biomass)
#head(timeS2)
ggplot(data = timeS[timeS$taxa=="Vaccinium myrtillus",], aes(x=yse, y=biomass, group = LocalityName))+
  geom_line()+
  theme_classic()+
  facet_grid(. ~ Treatment+Region, scales = "fixed")
# I don't think it a mistake - same pattern for both regions and treatments

ggplot(data = timeS[timeS$taxa=="Calluna vulgaris",], aes(x=yse, y=biomass, group = LocalityName))+
  geom_line()+
  theme_classic()+
  facet_grid(. ~ Region+Treatment, scales = "fixed")
# Calluna increases all over - more common in Telemark







# ***************************************#
# ***************************************#
# Betadiversity ####
# ***************************************#
# ***************************************#
library(vegan)



#***********************************
# VEGDIST

# betadiversity and how it changes with time since exlosure

head(wide)
head(wide2)


pairs <- interaction(wide2$LocalityName, wide2$yse, wide2$Treatment)
dist_out <- cbind(pairs, wide2)
dist_out2 <- NULL

for(i in unique(dist_out$pairs)){
  temp <- vegdist(x = subset(dist_out[,7:ncol(dist_out)], pairs == i), method="jaccard")     # dissimilarity
  temp2 <- cbind( 
                 unique(dist_out$Region[dist_out$pairs == i]),             
                 unique(dist_out$LocalityName[dist_out$pairs == i]), 
                 unique(dist_out$yse[dist_out$pairs == i]),
                 unique(dist_out$Treatment[dist_out$pairs == i]),
                 mean(as.numeric(temp))
                 )
  dist_out2 <- rbind(dist_out2, temp2)
}

class(dist_out2)
dist_out3 <- data.frame(dist_out2)
names(dist_out3) <- c("Region", "LocalityName", "yse", "Treatment", "Jaccard")
dist_out3$nJaccard <- as.numeric(as.character(dist_out3$Jaccard))
levels(dist_out3$Treatment)[levels(dist_out3$Treatment) == "B"] <- "Open plots"
levels(dist_out3$Treatment)[levels(dist_out3$Treatment) == "UB"] <- "Exclosures"



# get mean Jaccard distance per location and year
Jacc <- aggregate(data = dist_out3, nJaccard~yse+Treatment, FUN = mean)
Jacc$SE <- aggregate(data = dist_out3, nJaccard~yse+Treatment, FUN = std)[,3]
summary(Jacc)

Jacc$nyse <- as.numeric(as.character(Jacc$yse))


jacc <- ggplot(data = Jacc, aes(x=nyse, y=nJaccard, group=Treatment, linetype=Treatment))+
  theme_bw()+
  xlab("Years since exclusion")+
  ylab("Mean within-plot\nJaccard dissimilarity")+
  geom_line(size=1, position=pd)+ 
  geom_point(size =3, position=pd, aes(shape=Treatment))+
  geom_errorbar(aes(ymax=nJaccard+SE, ymin=nJaccard-SE, linetype = NULL), size=1.1, width=0.5, position=pd)+
  guides(linetype = FALSE, shape=FALSE)+
  scale_linetype_manual(breaks = c("B", "UB"), labels = c("Open plots", "Exclosures"), values=c(1,2))
  




# plot treatment difference against site productivity:
Jacc <- aggregate(data = dist_out3, nJaccard~yse+Treatment+LocalityName, FUN = mean)
Jacc2 <- Jacc[Jacc$yse=="8",]
Jacc$nyse <- as.numeric(as.character(Jacc$yse))

Jacc3 <- aggregate(data = Jacc2,
                     nJaccard~Treatment+LocalityName,
                     FUN = mean)

Jacc4 <- dcast(data = Jacc3, 
                 LocalityName~Treatment,
                 value.var = "nJaccard")
Jacc4$diff <- Jacc4$`Open plots` - Jacc4$Exclosures
Jacc4$productivity <- productivity$productivity[match(Jacc4$LocalityName, productivity$LocalityName)]


jacc2 <- ggplot(data = Jacc4, aes(x = productivity, y = diff))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=0),
            fill="lightgreen")+
  #theme(text = element_text(size=15))+
  geom_point(size = 3, stroke = 2, shape =1)+
  #geom_smooth(method = "lm", se = T, size = 2, colour = "black")+
  ylab("")+
  xlab("")
#ggtitle("Grasses")



#****************************************
# LMM Jaccard ####

library(lmerTest)

# prepare data:
JaccMod <- aggregate(data = dist_out3, nJaccard~yse+Treatment+LocalityName+Region, FUN = mean)
JaccModDat <- JaccMod[JaccMod$yse == "8",]
JaccModDat$productivity <- productivity$productivity[match(JaccModDat$LocalityName, productivity$LocalityName)]

# run full model:
modJacc <- lmerTest::lmer(nJaccard~Treatment*productivity + (1|Region/LocalityName), 
                        data = JaccModDat, REML = F)
plot(modJacc) # tja
qqnorm(resid(modJacc))
plot(JaccModDat$productivity, resid(modJacc))   # ok
plot(JaccModDat$Treatment, resid(modJacc))      # ok
summary(modJacc)
modJacc_add <- update(modJacc, .~. -Treatment:productivity)
modJacc_pi <- update(modJacc_add, .~. -Treatment)
modJacc_exc  <- update(modJacc_add, .~. -productivity)
AIC(modJacc, modJacc_add, modJacc_exc, modJacc_pi)
BIC(modJacc, modJacc_add, modJacc_exc, modJacc_pi)

#anova(modFS, modFS2)
AIC(modJacc_exc)-AIC(modJacc)
AIC(modJacc_exc)-AIC(modJacc_add)
AIC(modJacc_exc)-AIC(modJacc_pi)

modJacc_exc <- lmerTest::lmer(nJaccard~Treatment + (1|Region/LocalityName), 
                          data = JaccModDat, REML = T)
summary(modJacc_exc)
ICCr <- 0
ICCl <- 0.003718/(0.003718+0.004920)








# Jaccard and CCI ####
# Is it related to canopy cover?
# first we see is there's a treatment effect on the variation in CCI
# by analysing standrad deviation. Then we do the same using actauall euclidian disntances


# data from Trøndelag sites, in year 2016
CCI <- read_excel("CanopyCover_SUSTHERB_export.xlsx", 
                                          sheet = "SUSTHERB_export")
# remove thinned sites
unique(CCI$LocalityName)
thinned <- c("Selbu_Flub" , "Hi_tydal", "Malvik" )
CCI2 <- CCI[!CCI$LocalityName %in% thinned,]

uniquePlot <- interaction(CCI2$LocalityName, CCI2$Treatment, CCI2$Plot)
CCI3 <- cbind(uniquePlot, CCI2)
CCI4 <- CCI3[CCI3$Plot != "Experiment area",]
CCI5 <- aggregate(data = CCI4, CanopyCoverIndex~uniquePlot+LocalityName+Treatment+Plot, FUN =mean, na.rm=T)

wide3 <- wide2[wide2$Region=="Trøndelag",]
wide3 <- wide3[!wide3$LocalityName %in% thinned,]
#unique(wide3$LocalityName)
#unique(CCI4$LocalityName)
wide3$CCI <- CCI5$CanopyCoverIndex[match(paste0(wide3$LocalityName, wide3$Treatment, wide3$Plot),
                                         paste0(CCI5$LocalityName, CCI5$Treatment, CCI5$Plot))]

# change order
wide4 <- wide3[,c(ncol(wide3), 1:(ncol(wide3)-1))]


loc_trt <- interaction(wide4$LocalityName, wide4$Treatment)
wide5 <- cbind(loc_trt, wide4)
wide6 <- wide5[!is.na(wide5$CCI),]
# Euclidian distances

# IM STUCK!

EucD <- NULL
for(i in unique(wide6$loc_trt)){
  temp_dat <- subset(wide6, loc_trt ==i)
  temp <- vegdist(temp_dat$CCI, method = "euclidean")
  temp2 <- cbind(
    unique(temp_dat$LocalityName),
    unique(temp_dat$Treatment),
    temp
  )
  EucD <- rbind(EucD, temp2)
}

EucD <- data.frame(EucD)
names(EucD) <- c("LocalityName", "Treatment", "CCI_EucD")
EucD$CCI_EucD <- as.numeric(as.character(EucD$CCI_EucD))
boxplot(EucD$CCI_EucD~EucD$Treatment)






library(nlme)
mod_eucCCI <- lme(data = EucD, log(CCI_EucD+1)~Treatment, random = ~1|LocalityName)
plot(mod_eucCCI)
summary(mod_eucCCI)
# gamma glmm ...






SDs <- NULL
for(i in unique(CCI4$loc_trt)){
  temp_dat <- subset(CCI4, loc_trt ==i)
  tempSD <- sd(temp_dat$CanopyCoverIndex, na.rm=T)
  tempSD2 <- cbind(
    unique(temp_dat$Region),
    unique(temp_dat$LocalityName),
    unique(temp_dat$Treatment),
    tempSD
  )
  SDs <- rbind(SDs, tempSD2)
}

SDs <- data.frame(SDs)
names(SDs) <- c("Region", "LocalityName", "Treatment", "CCI_SD")
SDs$CCI_SD <- as.numeric(as.character(SDs$CCI_SD))
boxplot(SDs$CCI_SD~SDs$Treatment)
ModCCISD <- lme(data=SDs, CCI_SD~Treatment, random = ~1|LocalityName)
plot(ModCCISD)
summary(ModCCISD)
# no effect - also low sample size

# add productvity index...
productivity_index_sustherbSites <- read.csv("M:/Anders L Kolstad/systherb data/exported cvs/productivity_index_sustherbSites.csv")
SDs$PI <- productivity_index_sustherbSites$PI
# ++++







# ******************************#
# ******************************#
# Diveristy index ####
# ******************************#
# ******************************#

setwd("M:\\Anders L Kolstad\\R\\R_projects\\succession_paper")
cdat <- read_excel("community_data.xlsx", 
                   sheet = "SUSTHERB_export")



# remove regions
cdat <- cdat[cdat$Region=="Trøndelag" | cdat$Region=="Telemark",]

# set data and year
tail(cdat$'_Date')
Date <- as.Date(cdat$`_Date`, format = c("%Y-%m-%d"))
year <- format(Date, "%Y")
cdat <- cbind(year, Date, cdat)
head(cdat$year)


# calculating year since exclosure (yse)
yse <- ifelse(cdat$Region == "Trøndelag", as.numeric(as.character(cdat$year))-2008, 
              as.numeric(as.character(cdat$year))-2009)
cdat <- cbind(yse, cdat)
table(cdat$Region, cdat$yse)
table(cdat$LocalityName, cdat$Plot)
cdat$Plot <- ifelse(cdat$Plot > 10, cdat$Plot-10, cdat$Plot)


# remove some columns that are not vascular plant species
other_types <- c("Bare ground",
                 "Bare ground/branch", 
                 "Bare ground-stone",
                 "Bryophyta",
                 "Cow shit",
                 "Lichens",
                 "Litter",
                 "No occurrence",
                 "Sphagnum sp",
                 "Stone",
                 "Not identified")
trees <- c("Alnus incana",
           "Betula pubescens",
           "Quercus sp_",
           "Salix caprea",
           "Betula pendula",
           "Corylus avellana",
           "Picea abies",
           "Pinus sylvestris",
           "Populus tremula",
           "Prunus padus",
           "Sorbus aucuparia",
           "Juniperus communis",
           "Sambucus sp_")

cdat <- cdat[,!colnames(cdat) %in% c(other_types, trees)]

mosses <- c("Cirriphyllum piliferum",
            "Dicranum sp",
            "Hylocomiastrum umbratum",
            "Hylocomium splendens",
            "Marchantiophyta",
            "Plagiochila asplenioides",
            "Plagiomnium ellipticum",
            "Plagiomnium undulatum",
            "Plagiothecium laetum/curvifolium",
            "Plagiothecium undulatum",
            "Pleurozium schreberi",
            "Polytrichum/-astrum",
            "Ptilidium ciliare",
            "Ptilium crista-castrensis",
            "Rhodobryum roseum",
            "Rhytidiadelphus loreus",
            "Rhytidiadelphus squarrosus/subpinnatus",
            "Rhytidiadelphus triquetrus",
            "Sciuro-hypnum reflexum",
            "Sciuro-hypnum starkei")






# Turning all species into numeric
to_be_numbers <- c(16:ncol(cdat))
cdat[,to_be_numbers] <- as.numeric(as.character(unlist(cdat[,to_be_numbers])))
summary(colSums(cdat[,16:ncol(cdat)], na.rm = T))


# removing species with no records
cdat <- cdat[,c(rep(TRUE, times = 15),
                 colSums(cdat[,16:ncol(cdat)], na.rm = T) > 0)]


# look for empty plots
summary(rowSums(cdat[,16:ncol(cdat)], na.rm = T))  # obs - note some non-species at the end of df
#View(cdat[rowSums(cdat[,9:ncol(cdat)], na.rm = T) == 0,])
# Bjøllåa B plot 5 and Slindsvann UB plot 7 were not found in 2016 so data exists
# assuming the same is tru for the rest
cdat <- cdat[rowSums(cdat[,16:ncol(cdat)], na.rm = T) > 0,]


# Balance? OK
table(cdat$LocalityName, cdat$Treatment)
cdat <- cdat[!(cdat$LocalityCode == "1RB" | cdat$LocalityCode == "1RUB"),]


# removing 'observed' species and retaining only measured species
# (more species are recorded for Trøndelag in 2016)
cdat <- cdat[cdat$Method=="Point_Intercept",]



# calculating biomass
# run 'Species groups'

cdatBM <- cdat   
#cdatBM[,16:ncol(cdatBM)] <- NULL

cdat[,16:ncol(cdat)] <- cdat[,16:ncol(cdat)]/16   # standardising the intercept frequency

cdatBM[, BLS_taxa] <- cdat[, BLS_taxa]  * 74.4101   + 1.2857
cdatBM[, NLS_taxa] <- cdat[, NLS_taxa]  * 74.184    + 9.2471
cdatBM[, TH_taxa]  <- cdat[, TH_taxa]   * 36.4849   + 4.0373
cdatBM[, SH_taxa]  <- cdat[, SH_taxa]   * 15.1209   + 0.7713
cdatBM[, BLG_taxa] <- cdat[, BLG_taxa]  * 23.3885   + 0.6384
cdatBM[, NLG_taxa] <- cdat[, NLG_taxa]  * 5.9653    + 0.8747



# Vascular plant species 
vasc_names <- names(cdatBM[,16:ncol(cdatBM)])[!names(cdatBM[,16:ncol(cdatBM)]) %in% mosses]


# get species lists with percentage occurence at location level in 
spListDat <- cdatBM[cdatBM$yse=="8",16:ncol(cdatBM)]
spList <- colSums(spListDat>0, na.rm=T)
spList <- as.data.frame(spList)
spList$Functional_type <- NULL
spList$Taxa <- rownames(spList)
spList$Functional_type[spList$Taxa %in% mosses] <- "Bryophyte"
spList$Functional_type[spList$Taxa %in% shrubs_fg] <- "Dwarf shrub"
spList$Functional_type[spList$Taxa %in% ferns_fg] <- "Fern"
spList$Functional_type[spList$Taxa %in% graminoids_fg] <- "Graminoid"
spList$Functional_type[spList$Taxa %in% small_herbs] <- "Small herb"
spList$Functional_type[spList$Taxa %in% TH_taxa] <- "Tall herb"
spList$Functional_type[spList$Taxa== "Luzula pilosa"] <- "Graminoid"
colnames(spList)[colnames(spList) == "spList"] <- "Frequency"
spList <- spList[order(spList$Frequency, decreasing = T),]
spList <- spList[spList$Frequency >2,]
spList$relativeFrequency <- spList$Frequency/nrow(spListDat)
datX <- cdatBM[cdatBM$yse=="8" & cdatBM$Region == "Trøndelag",]
spList$relativeFrequency[spList$Functional_type == "Bryophyte"] <- spList$Frequency[spList$Functional_type == "Bryophyte"]/nrow(datX)
spList <- spList[order(spList$relativeFrequency, decreasing = T),]

#write.csv(spList, file = "speciesList.csv", row.names = F)


# Total species richness
SR <- rowSums(cdatBM[,16:ncol(cdatBM)]>0, na.rm=T)

# TOTAL BIOMASS
biom <- rowSums(cdatBM[,names(cdatBM) %in% vasc_names], na.rm=T)
summary(biom)
plot(biom)
# Moss species richness (Trøndelag 2016 only)
MSR <- rowSums(cdatBM[,names(cdatBM) %in% mosses]>0, na.rm=T)

# Vascular plant species richness
VSR <- rowSums(cdatBM[,names(cdatBM) %in% vasc_names]>0, na.rm=T)



cdatBM <- cbind(SR, biom, MSR, VSR, cdatBM)

library(vegan)
library(gridExtra)
cdatBM[is.na(cdatBM)] <- 0
Shannon_moss <- diversity(cdatBM[,mosses], index = "shannon")
Shannon_vasc <- diversity(cdatBM[,vasc_names], index = "shannon")
cdatBM <- cbind(Shannon_moss, Shannon_vasc, cdatBM)


cdatBM_site <- aggregate(data = cdatBM, cbind(SR,biom,MSR,VSR, Shannon_vasc, Shannon_moss)~yse+Treatment+LocalityName, FUN = mean)
cdatBM_trt <- aggregate(data = cdatBM_site, cbind(SR,biom,MSR,VSR, Shannon_vasc, Shannon_moss)~yse+Treatment, FUN = mean)
cdatBM_trt$SE_VSR <- aggregate(data = cdatBM_site, cbind(VSR)~yse+Treatment, FUN = std)[,"VSR"]
cdatBM_trt$SE_ShannonVasc <- aggregate(data = cdatBM_site, cbind(Shannon_vasc)~yse+Treatment, FUN = std)[,"Shannon_vasc"]



#tiff("Vasc_SR_and_shannon.tiff", height = 15, width = 10, res = 300, units = "cm")
div1 <- ggplot(data = cdatBM_trt, aes(x = yse, y= VSR, group = Treatment, linetype = Treatment))+
  theme_bw()+
  ylab("Mean vascular plant\nspecies richness") +
  xlab("")+
  geom_line(size=1, position=pd)+ 
  geom_point(size =3, position=pd, aes(shape=Treatment))+
  geom_errorbar(aes(ymax=VSR+SE_VSR, ymin=VSR-SE_VSR, linetype = NULL), size=1.1, width=0.5, position=pd)+
  theme(legend.justification = c(0.99,0.01), 
        legend.position = c(0.99,0.01),
        #legend.background = element_blank(),
        legend.key.width = unit(3,"line"),
        legend.text=element_text(size=12),
        legend.title = element_blank())+
  scale_linetype_manual(breaks = c("B", "UB"), labels = c("Open plots", "Exclosures"), values=c(1,2))+
  scale_shape_manual(breaks = c("B", "UB"), labels = c("Open plots", "Exclosures"), values=c(16,17))

                  
div2 <- ggplot(data = cdatBM_trt, aes(x = yse, y=Shannon_vasc,group = Treatment, linetype = Treatment))+
  theme_bw()+
  ylab("Mean vascular plant\nShannon entropy") +
  xlab("")+
  guides(linetype=FALSE, shape = FALSE)+
  geom_line(size=1, position=pd)+ 
  geom_point(size =3, position=pd, aes(shape=Treatment))+
  scale_linetype_manual(breaks = c("B", "UB"), labels = c("Open plots", "Exclosures"), values=c(1,2))+
  geom_errorbar(aes(ymax=Shannon_vasc+SE_ShannonVasc, ymin=Shannon_vasc-SE_ShannonVasc, linetype = NULL), size=1.1, width=0.5, position=pd)





tiff("alpha2_beta.tiff", height = 20, width = 10, units = "cm", res=300)
plot_grid(div1, div2, jacc,
          ncol=1, align = "hv")
dev.off()








# VSR Interactio plot
cdatBM_site2 <- aggregate(data = cdatBM_site,
                     VSR~Treatment+LocalityName,
                     FUN = mean)
cdatBM_site3 <- dcast(data = cdatBM_site2, 
                 LocalityName~Treatment,
                 value.var = "VSR")
cdatBM_site3$diff <- cdatBM_site3$UB - cdatBM_site3$B
cdatBM_site3$productivity <- productivity$productivity[match(cdatBM_site3$LocalityName, productivity$LocalityName)]




div11 <- ggplot(data = cdatBM_site3, aes(x = productivity, y = diff))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=0),
            fill="lightgreen")+
  geom_point(size = 3, stroke = 2, shape =1)+
  #geom_smooth(method = "lm", se = T, size = 2, colour = "black")+
  ylab("")+
  xlab("Site productivity")
#  ggtitle("Large herbs")


# Moss interaction plot
cdatBM_plotmoss <- cdatBM_plot[cdatBM_plot$Region == "Trøndelag",]



cdatBM_plotmoss2 <- aggregate(data = cdatBM_plotmoss,
                          MSR~Treatment+LocalityName,
                          FUN = mean)
cdatBM_plotmoss3 <- dcast(data = cdatBM_plotmoss2, 
                      LocalityName~Treatment,
                      value.var = "MSR")
cdatBM_plotmoss3$diff <- cdatBM_plotmoss3$UB - cdatBM_plotmoss3$B
cdatBM_plotmoss3$productivity <- productivity$productivity[match(cdatBM_plotmoss3$LocalityName, productivity$LocalityName)]

moss2 <- ggplot(data = cdatBM_plotmoss3, aes(x = productivity, y = diff))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=0),
            fill="lightgreen")+
  geom_point(size = 3, stroke = 2, shape =1)+
  geom_smooth(method = "lm", se = T, size = 2, colour = "black")+
  ylab("Relativ shift in bryophyte species richness")+
  xlab("Site productivity")
#  ggtitle("Large herbs")



# LMM 
# VSR

cdatBM$productivity <- productivity$productivity[match(cdatBM$LocalityName, productivity$LocalityName)]
cdatBMSR <- cdatBM[cdatBM$yse==8,]

table(cdatBMSR$Region)
table(cdatBMSR$Region, cdatBMSR$yse)

modSR <- lmerTest::lmer(log(VSR+1)~Treatment*productivity + (1|Region/LocalityName), 
                        data = cdatBMSR, REML = F)
plot(modSR) # tja
plot(cdatBMSR$productivity, resid(modSR))   # ok
plot(as.factor(cdatBMSR$Treatment), resid(modSR))   # ok
summary(modSR)
modSR_add <- update(modSR, .~. -Treatment:productivity)
modSR_pi <- update(modSR_add, .~. -Treatment)
modSR_exc <- update(modSR_add, .~. -productivity)
AIC(modSR, modSR_add, modSR_exc, modSR_pi)
#anova(modFS, modFS2)
AIC(modSR_pi)-AIC(modSR)
AIC(modSR_pi)-AIC(modSR_add)
AIC(modSR_pi)-AIC(modSR_exc)
modSR_pi <- lmerTest::lmer(log(VSR+1)~productivity + (1|Region/LocalityName), 
                        data = cdatBMSR, REML = T)
summary(modSR_pi)




modFS <- lmerTest::lmer(log(biomass+1)~Treatment*productivity + (1|Region/LocalityName), 
                        data = modDat[modDat$group == "large_herbs",], REML = T)
ICCr <- 0.03107/(0.03107+0.02500+0.09227)
ICCl <- 0.02500/(0.03107+0.02500+0.09227)


#MSR
cdatBMSR2 <- cdatBMSR[cdatBMSR$Region== "Trøndelag",]

modMSR <- lmerTest::lmer(MSR~Treatment*productivity + (1|LocalityName), 
                        data = cdatBMSR2, REML = F)
plot(modMSR) # tja
plot(cdatBMSR2$productivity, resid(modMSR))   # ok
plot(as.factor(cdatBMSR2$Treatment), resid(modMSR))   # ok
summary(modMSR)
modMSR_add <- update(modMSR, .~. -Treatment:productivity)
modMSR_pi <- update(modMSR_add, .~. -Treatment)
modMSR_exc <- update(modMSR_add, .~. -productivity)
AIC(modMSR, modMSR_add, modMSR_exc, modMSR_pi)
#anova(modFS, modFS2)
AIC(modMSR_pi)-AIC(modMSR)
AIC(modMSR_pi)-AIC(modMSR_add)
AIC(modMSR_pi)-AIC(modMSR_exc)
modMSR_pi <- lmerTest::lmer(MSR~productivity + (1|LocalityName), 
                            data = cdatBMSR2, REML = T)
summary(modMSR_pi)




modFS <- lmerTest::lmer(log(biomass+1)~Treatment*productivity + (1|Region/LocalityName), 
                        data = modDat[modDat$group == "large_herbs",], REML = T)
ICCl <- 0.7132/(0.7132+3.3690)

cdatBM$fTreatment <- factor(cdatBM$Treatment)
levels(cdatBM$fTreatment)[levels(cdatBM$fTreatment)=="B"] <- "Open plots"
levels(cdatBM$fTreatment)[levels(cdatBM$fTreatment)=="UB"] <- "Exclosure"


#tiff("Moss_SR_and_shannon.tiff", height = 15, width = 10, res = 300, units = "cm")

grid.arrange(
ggplot(data = subset(cdatBM, yse == 8 & Region =="Trøndelag"))+
  theme_classic()+xlab("")+ylab("Bryophyte species richness")+
  geom_boxplot(aes(x=fTreatment, y=MSR)),
ggplot(data = subset(cdatBM, yse == 8 & Region =="Trøndelag"))+
  theme_classic()+xlab("")+ylab("Bryophyte shannon entrophy")+
  geom_boxplot(aes(x=fTreatment, y=Shannon_moss)))
#dev.off()

#ggplot(data = subset(cdatBM, yse == 8 & Region =="Trøndelag"))+
#  theme_classic()+xlab("")+ylab("Bryophyte shannon entrophy")+
#  geom_line(aes(x=fTreatment, y=Shannon_moss, group = LocalityName))


cdatBM$Region <- factor(cdat$Region)
cdatBM$Treatment <- factor(cdat$Treatment)
cdatBM$fLocalityName <- factor(cdat$LocalityName)
table(cdatBM$fLocalityName, cdat$yse)
table(cdatBM$fLocalityName, cdat$Plot)





# OLD ####
# standardizing time effect from year 0
wide3 <- dcast(data = long, Region + LocalityName + Treatment+Plot+taxa ~ yse, 
               value.var = "biomass", fun.aggregate = mean)
yrs <- c("2", "4", "6", "8")
wide3.2 <- wide3
for(i in yrs){
  wide3[,i] <- wide3[,i]-wide3$`0`
}

wide3 <- wide3[ , !(names(wide3) %in% c("0"))]

wide3 <- melt(data = wide3, 
              id.vars = c("Region", "LocalityName", "Treatment", "Plot", "taxa"),
              measure.vars = yrs)
names(wide3)[names(wide3) == "variable"] <- "yse"
names(wide3)[names(wide3) == "value"]    <- "biomass"
wide3 <- dcast(data = wide3, yse + Region + LocalityName + Treatment+Plot ~ taxa, 
               value.var = "biomass", fun.aggregate = mean)

wide3[is.na(wide3)] <- 0
