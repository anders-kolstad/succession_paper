
############ PRC ###############

# Three analyses:....

# NR1
# Field layer species change over time
# The change in the abundance of species over time
# Goal - describe the plant succession in absolute numbers (biomass)

# Nr2
# Plot diversity indices over time.
# A univariate comunity response.

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
             "Fragaria vesca"                  ,  
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
             "Salix sp_"      ,                   
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
# estimate biomass


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

# removing singletons and species rarer than 10%
# total nr location:year = 155 (-3)
#length(unique(cdatBM$loc_year))
# define cut of at 15

cdatBM2 <- cdatBM
cdatBM <- cdatBM[,c(rep(TRUE, times=15), colSums(cdatBM[,16:ncol(cdatBM)] >1, na.rm=T) >  15)]
rm(cdatBM2)

# Functional groups ####
FG <- cdatBM
grasses <- c("other_graminoids",
             "Carex",
             "Avenella flexuosa",
             "Deschampsia cespitosa")
FG$grasses <- rowSums(FG[,names(FG) %in% grasses], na.rm=TRUE)
large_herbs <- TH_taxa[TH_taxa != ("Veronica officinalis")]
large_herbs <- large_herbs[!large_herbs %in% ferns]
FG$large_herbs <- rowSums(FG[,names(FG) %in% large_herbs], na.rm=TRUE)
small_herbs <- c(SH_taxa[SH_taxa %in% names(cdatBM)], "Epilobium", "Alchemilla", "Galeopsis")
FG$small_herbs <- rowSums(FG[,names(FG) %in% small_herbs], na.rm=TRUE)
ferns <- ferns_fg[ferns_fg %in% names(cdatBM)]
FG$ferns <- rowSums(FG[,names(FG) %in% ferns], na.rm=TRUE)
shrubs_fg <- shrubs_fg[shrubs_fg %in% names(cdatBM)]
FG$shrubs <- rowSums(FG[,names(FG) %in% shrubs_fg], na.rm=TRUE)
library(reshape2)
FG2 <- select(FG, yse, Region, LocalityName, Treatment, Plot,
              grasses, large_herbs, small_herbs,
              ferns, shrubs)
FG3 <- melt(data = FG2, id.vars = names(FG2[,1:5]), measure.vars = names(FG2[,6:ncol(FG2)]))
names(FG3)[names(FG3) == "variable"] <- "group"
names(FG3)[names(FG3) == "value"]    <- "biomass"

FG4 <- aggregate(data=FG3, biomass~yse+Treatment+LocalityName+group, FUN = mean)
table(FG4$LocalityName, FG4$yse, FG4$group)

library(gridExtra)
tiff("Functional_groups_gridarrange.tiff", height = 40, width = 12, units = "cm", res=300)
grid.arrange(
ggplot(data = FG4[FG4$group=="grasses",], aes(x=yse, y= biomass, group=Treatment, linetype=Treatment))+
  geom_smooth(method = "lm", size = 2, alpha = 0.2, colour = "black")+ #geom_point(aes(shape=Treatment))+
  theme_bw()+ggtitle("Grasses")+guides(linetype = FALSE)+xlab("")+ylab(expression(paste("Biomass (g m"^"-2", ")"))),
ggplot(data = FG4[FG4$group=="large_herbs",], aes(x=yse, y= biomass, group=Treatment, linetype=Treatment))+
  geom_smooth(method = "lm", size = 2, alpha = 0.2, colour = "black")+
  theme_bw()+ggtitle("Large herbs")+guides(linetype = FALSE)+xlab("")+ylab(expression(paste("Biomass (g m"^"-2", ")"))),
ggplot(data = FG4[FG4$group=="small_herbs",], aes(x=yse, y= biomass, group=Treatment, linetype=Treatment))+
  geom_smooth(method = "lm", size = 2, alpha = 0.2, colour = "black")+
  theme_bw()+ggtitle("Small herbs")+guides(linetype = FALSE)+xlab("")+ylab(expression(paste("Biomass (g m"^"-2", ")"))),
ggplot(data = FG4[FG4$group=="ferns",], aes(x=yse, y= biomass, group=Treatment, linetype=Treatment))+
  geom_smooth(method = "lm", size = 2, alpha = 0.2, colour = "black")+
  theme_bw()+ggtitle("Ferns")+guides(linetype = FALSE)+xlab("")+ylab(expression(paste("Biomass (g m"^"-2", ")"))),
ggplot(data = FG4[FG4$group=="shrubs",], aes(x=yse, y= biomass, group=Treatment, linetype=Treatment))+
  geom_smooth(method = "lm", size = 2, alpha = 0.2, colour = "black")+xlab("Years since exclusion")+
  theme_bw()+ggtitle("Dwarf shrubs")+guides(linetype = FALSE)+ylab(expression(paste("Biomass (g m"^"-2", ")"))),
ncol=1
)
dev.off()



setwd("M:\\Anders L Kolstad\\R\\R_projects\\succession_paper")
load("prod_index_telemark_and_trondelag.RData")
#productivity <- read.csv("M:/Anders L Kolstad/R/R_projects/succession_paper/prod_index_ID_name_converter.csv", sep=";")

FG3$productivity <- productivity$productivity[match(FG3$LocalityName, productivity$LocalityName)]
summary(FG3$productivity) # no NA's
FG3$uniquePlot <- paste(FG3$LocalityName, FG3$Treatment, FG3$Plot, sep = "_")
modDat <- FG3[FG3$yse==8,]
yearZ <- FG3[FG3$yse == 0,]
modDat$start_biomass <- yearZ$biomass[match(modDat$uniquePlot, yearZ$uniquePlot)]

modDat$Treatment <- as.factor(modDat$Treatment)
modDat$biomass2 <- modDat$biomass-modDat$start_biomass

library(lmerTest)
modFS <- lmerTest::lmer(log(biomass+1)~Treatment*productivity + (1|Region/LocalityName), 
                         data = modDat[modDat$group == "large_herbs",])
plot(modFS) # tja
plot(modDat$productivity[modDat$group == "large_herbs"], resid(modFS))   # ok
plot(modDat$Treatment[modDat$group == "large_herbs"], resid(modFS))      # ok
summary(modFS)
modFS <- lmerTest::lmer(log(biomass+1)~Treatment+productivity + (1|Region/LocalityName), 
                        data = modDat[modDat$group == "large_herbs",])
anova(modFS)

library(glmmTMB)
modFS2 <- glmmTMB(biomass+1~Treatment*productivity + (1|Region/LocalityName), 
                        data = modDat[modDat$group == "large_herbs",], family = list(family = "Gamma", link = "log"))

plot(fitted(modFS2), resid(modFS2)) # funell, but with Gamma i'm not sure thats a problem. It should be taken care of by the dispersion parameter
plot(modDat$productivity[modDat$group == "large_herbs"], resid(modFS2)) # this one is worse
plot(modDat$Treatment[modDat$group == "large_herbs"], resid(modFS2)) # and this is not good
# I think the log transformation was best in this case



modDat2 <- aggregate(data = modDat,
                biomass~Treatment+LocalityName+group,
                FUN = mean)
modDat3 <- dcast(data = modDat2, 
             LocalityName+group~Treatment,
             value.var = "biomass")
modDat3$diff <- modDat3$UB - modDat3$B
modDat3$productivity <- modDat$productivity[match(modDat3$LocalityName, modDat$LocalityName)]


ggplot(data = modDat3, aes(x = productivity, y = diff, group = group, colour = group))+
  #geom_point()+
  geom_smooth(method = "lm", se = F)+
  ylab("Relative difference in biomass\n(Exclosure minus Open plots)")+
  xlab("Site productivity")
# expect the larges interaction effects for large herbs, shrubs, and ferns

ggplot(data = modDat3[modDat3$group == "large_herbs",], aes(x = productivity, y = diff))+
  theme_bw()+theme(text = element_text(size=15))+
  geom_point(size = 3, stroke = 2, shape =1)+
  geom_smooth(method = "lm", se = F, size = 2, colour = "black")+
  ylab("Relative difference in biomass\n(Exclosure minus Open plots)")+
  xlab("Site productivity")+
  ggtitle("Large herbs")




# other functional group plots ....
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




# PRC   ###
library(vegan)

cdatBM[is.na(cdatBM)] <- 0
cdatx <- cdatBM
library(reshape2)
long <- melt(data = cdatx, id.vars = c("yse", "Region", "LocalityName", "Treatment", "Plot") , 
             measure.vars = names(cdatx[,16:ncol(cdatx)]))
names(long)[names(long) == "variable"] <- "taxa"
names(long)[names(long) == "value"]    <- "biomass"
wide <- dcast(data = long, yse + Region + LocalityName + Treatment ~ taxa, 
              value.var = "biomass", fun.aggregate = mean)
wide2 <- dcast(data = long, yse + Region + LocalityName + Treatment+Plot ~ taxa, 
              value.var = "biomass", fun.aggregate = mean)
thinned <- c("Selbu_Flub", "Hi_tydal", "Malvik")
unique(wide2$LocalityName)
wide2_red <- wide2[!wide2$LocalityName %in% thinned,]





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

# compare # PRC on SUBplot level data
prc.out.sub <- prc(log(wide2[,6:ncol(wide2)]+1), 
               as.factor(wide2$Treatment), 
               as.factor(wide2$yse))
prc.out.sub # inertia very low - ignoring locations and plot level variation here
sum_mod_prc_sub <- summary(prc.out.sub, scaling = 2)
#species <- colSums(rda.res)
par(mar=c(5,5,10,10))


#tiff("PRC.tiff", height = 10, width = 10, units = "cm", res=300)
plot(prc.out.sub, select = abs(sum_mod_prc_sub$sp) > 0.12, scaling = 2, 
     legpos = NULL, xlab = "Year since exclosure")
dev.off()
# results comparable


# compare removing thinned sites
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
            within = Within(type = "series"), nperm = 500    )
anova(prc.out.sub, permutations = ctrl, first=TRUE)

# compare with partial PRC method on aggregated data:
rda.out <- rda(log(wide[,5:ncol(wide)]+1) ~ wide$LocalityName) # equivalent of "rda.out = rda(X ~ Condition(Z))"
rda.out # location explains 70%
# unconstrained axes  all have equally high eigenvalues!
rda.res <- residuals(rda.out)    # does this carry only the residuals from the first axis?

prc.out2 <- prc(rda.res, as.factor(wide$Treatment), 
               as.factor(wide$yse))
anova(prc.out2)
sum_mod_prc2 <- summary(prc.out, scaling = 2)
#species <- colSums(rda.res)
par(mar=c(5,5,10,10))
prc.out2
plot(prc.out2, select = abs(sum_mod_prc2$sp) > 0.1, scaling = 2)


# compare with partial PRC method on plot level data:
rda.out2 <- rda(log(wide2[,6:ncol(wide2)]+1) ~ wide2$LocalityName) # equivalent of "rda.out = rda(X ~ Condition(Z))"
rda.out2 # location explains 34%
plot(rda.out2)
rda.res2 <- residuals(rda.out2)    # does this carry only the residuals from the first axis?

prc.out3 <- prc(rda.res2, as.factor(wide2$Treatment), 
                as.factor(wide2$yse))
sum_mod_prc3 <- summary(prc.out3, scaling = 2)
#species <- colSums(rda.res)
par(mar=c(5,5,10,10))
prc.out2 # time: 11%; treatment = 2%; residual variance = 87%
plot(prc.out3, select = abs(sum_mod_prc3$sp) > 0.05, scaling = 2)
# all methods look similar





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
  facet_grid(. ~ taxa, scales = "fixed"), nrow=2)

ggplot(data = timeS[timeS$taxa=="Vaccinium myrtillus",], aes(x=cyse, y=biomass, group = LocalityName))+
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


ggplot(data = timeS2, aes(x=cyse, y=biomass, group = taxa))+
  geom_line()+
  theme_classic()+
  facet_grid(. ~ Treatment, scales = "fixed")
#outlier in year4? - Vac.myr
#plot(timeS2$biomass)
#identify(timeS2$biomass)
#head(timeS2)
ggplot(data = timeS[timeS$taxa=="Vaccinium myrtillus",], aes(x=cyse, y=biomass, group = LocalityName))+
  geom_line()+
  theme_classic()+
  facet_grid(. ~ Treatment+Region, scales = "fixed")
# I don't think it a mistake - same pattern for both regions and treatments

ggplot(data = timeS[timeS$taxa=="Calluna vulgaris",], aes(x=cyse, y=biomass, group = LocalityName))+
  geom_line()+
  theme_classic()+
  facet_grid(. ~ Region+Treatment, scales = "fixed")
# Calluna increases all over - more common in Telemark



library(ggplot2)
ggplot(data = timeS3, aes(x=cyse, y=diff, group = taxa))+
         geom_line()+
  theme_classic()
ggplot(data = timeS3[timeS3$taxa=="Vaccinium myrtillus",], aes(x=cyse, y=diff, group = taxa))+
  geom_line()+
  theme_classic()



#  the most extreme species
diff_sp <- unique(timeS3$taxa[timeS3$diff >0.05])
# calluna is extreme - investigate! 

timeS3[which.max(timeS3$diff),]



# taking out the localitions effect
PRCdat <- cdatBM
PRCdat2 <- PRCdat
PRCdat[,16:ncol(PRCdat)] <- scale(PRCdat[,16:ncol(PRCdat)])
summary(PRCdat2[,16:30])
summary(PRCdat[,16:30])


rda.out <- rda(PRCdat[,16:ncol(PRCdat)] ~ PRCdat$LocalityName) # equivalent of "rda.out = rda(X ~ Condition(Z))"
rda.out # location explains 31%
rda.res <- residuals(rda.out)    # does this carry only the residuals from the first axis?
plot(rda.out)

prc.out <- prc(rda.res, as.factor(PRCdat$Treatment), 
               as.factor(PRCdat$yse))
sum_mod_prc <- summary(prc.out, scaling = 2)
#species <- colSums(rda.res)
par(mar=c(5,5,10,10))
prc.out
plot(prc.out, select = abs(sum_mod_prc$sp) > 0.03, scaling = 2)


prc.out.region <- prc(PRCdat[,16:ncol(PRCdat)], as.factor(PRCdat$Region), 
               as.factor(PRCdat$yse))
sum_mod_prc2 <- summary(prc.out.region, scaling = 2)
plot(prc.out.region, select = abs(sum_mod_prc$sp)>0.1, scaling = 2)

prc.out.location <- prc(PRCdat[,16:ncol(PRCdat)], as.factor(PRCdat$LocalityName), 
                      as.factor(PRCdat$yse))
sum_mod_prc3 <- summary(prc.out.location, scaling = 2)
plot(prc.out.location, select = abs(sum_mod_prc3$sp)>0.1, scaling = 2, legpos = NA)




# VEGDIST ####

# betadiversity and how it changes with time since exlosure

head(wide)
library(vegan)

pairs <- interaction(wide2$LocalityName, wide2$yse, wide2$Treatment)
dist_out <- cbind(pairs, wide2)
dist_out2 <- NULL

for(i in unique(dist_out$pairs)){
  temp <- vegdist(x = subset(dist_out[,6:ncol(dist_out)], pairs == i), method="jaccard")  
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


#dist_out3$dups <- interaction(dist_out3$LocalityName, dist_out3$yse)
#dist_out3.2 <- dist_out3[!duplicated(dist_out3$dups),]

#tiff("Jaccard_interaction.tiff", width = 15, height = 15, res=300, units="cm")  
ggplot(data = dist_out3, aes(x=as.numeric(yse), y=nJaccard,
                             group = Treatment, colour = Treatment))+
  theme_bw()+
  #geom_point()+
  geom_smooth(method = "lm", size = 1.5, fill = "grey70")+
  xlab("Year since exclosure")+
  ylab("Jaccard dissimilarity")+
  theme(legend.justification=c(0.01,0.01), legend.position=c(0.01,0.01))
#dev.off()
#tiff("Jaccard_interaction_points.tiff", width = 15, height = 15, res=300, units="cm")  
ggplot(data = dist_out3, aes(x=as.numeric(yse), y=nJaccard,
                             group = Treatment, colour = Treatment))+
  theme_bw()+
  geom_point()+
  geom_smooth(method = "lm", size = 1.5, fill = "grey70")+
  xlab("Year since exclosure")+
  ylab("Jaccard dissimilarity")+
  theme(legend.justification=c(0.01,0.01), legend.position=c(0.01,0.01))
#dev.off()


# beta diversity is similar between treatments, but
# possibly diverging....
library(nlme)
Jmod <- lme(data = dist_out3, nJaccard~as.numeric(yse)*Treatment,
            random = ~1|Region/LocalityName)

library(lme4)
Jmod2 <- lmer(data = dist_out3, nJaccard~as.numeric(yse)*Treatment+
            (1|Region/LocalityName))

plot(Jmod)
plot(dist_out3$yse, resid(Jmod))
plot(dist_out3$Treatment, resid(Jmod))
plot(dist_out3$LocalityName, resid(Jmod))
plot(dist_out3$Region, resid(Jmod))


summary(Jmod) # yes, significant interaction effect!
summary(Jmod2) # this is better at showing random effects and calculating ICC



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





# Diveristy index ####
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
cdatBM[, NLS_taxa] <- cdat[, NLS_taxa]  * 74.184  + 9.2471
cdatBM[, TH_taxa]  <- cdat[, TH_taxa]   * 36.4849 + 4.0373
cdatBM[, SH_taxa]  <- cdat[, SH_taxa]   * 15.1209 + 0.7713
cdatBM[, BLG_taxa] <- cdat[, BLG_taxa]  * 23.3885   + 0.6384
cdatBM[, NLG_taxa] <- cdat[, NLG_taxa]  * 5.9653  + 0.8747



# Vascular plant species 
vasc_names <- names(cdatBM[,16:ncol(cdatBM)])[!names(cdatBM[,16:ncol(cdatBM)]) %in% mosses]

# Total species richness
SR <- rowSums(cdatBM[,16:ncol(cdatBM)]>0, na.rm=T)

# Moss species richness (Trøndelag 2016 only)
MSR <- rowSums(cdatBM[,names(cdatBM) %in% mosses]>0, na.rm=T)

# Vascular plant species richness
VSR <- rowSums(cdatBM[,names(cdatBM) %in% vasc_names]>0, na.rm=T)

cdatBM <- cbind(SR, MSR, VSR, cdatBM)

library(vegan)

cdatBM[is.na(cdatBM)] <- 0
Shannon_moss <- diversity(cdatBM[,mosses], index = "shannon")
Shannon_vasc <- diversity(cdatBM[,vasc_names], index = "shannon")
cdatBM <- cbind(Shannon_moss, Shannon_vasc, cdatBM)

cdatBM_plot <- aggregate(data = cdatBM, cbind(SR,MSR,VSR, Shannon_vasc, Shannon_moss)~LocalityName+yse+Treatment, FUN = mean)

tiff("Vasc_SR_and_shannon.tiff", height = 15, width = 10, res = 300, units = "cm")
grid.arrange(
ggplot(data = cdatBM_plot, aes(x = yse, group = Treatment, linetype = Treatment))+
  #geom_point(aes(y=VSR, shape = Treatment))+
  geom_smooth(aes(y=VSR), method = "lm", colour = "black")+
  ylab("Vascular plant\nspecies richness") +xlab("")+
  guides(linetype=FALSE, shape = FALSE),
ggplot(data = cdatBM_plot, aes(x = yse, group = Treatment, linetype = Treatment))+
  #geom_point(aes(y=Shannon_vasc, shape = Treatment))+
  geom_smooth(aes(y=Shannon_vasc), method = "lm", colour = "black")+
  ylab("Vascular plant\nShannon entropy") +xlab("Years since exclosure")+
  guides(linetype=FALSE, shape = FALSE)
, nrow=2)
dev.off()

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