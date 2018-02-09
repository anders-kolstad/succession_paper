

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

FG4 <- aggregate(data=FG3, biomass~yse+Treatment+group, FUN = mean)

ggplot(data = FG4, aes(x=yse, y= biomass, group=group, colour=group))+
  geom_line()+theme_bw()+
  facet_grid(. ~ Treatment, scales = "fixed")

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


# Analyse functional groups   ####
library(glmmTMB)
TH_glm <- glmmTMB(biomass+0.1 ~ Treatment * yse + (1|Region/LocalityName/Plot), 
                  data = subset(FG3, group == "large_herbs"), family = Gamma(link = "log"))
library(lme4)
TH_glm <- lmer(biomass ~ Treatment * yse + (1|Region/LocalityName/Plot), 
                  data = subset(FG3, group == "large_herbs"))
library(nlme)
TH_glm <- lme(log(biomass+1) ~ Treatment * yse, random = ~1|Region/LocalityName/Plot, 
               data = subset(FG3, group == "large_herbs"))

summary(TH_glm)
plot(TH_glm)
qqnorm(resid(TH_glm))

#not working well
# try ignoring time (drawback is you cant standarize by year 0, 
# unless you subtract the biomass in year 0, but year 0 is still a few months after the fence can up,
# which is important for short lived species.
THdat <- subset(FG3, group == "large_herbs" & yse %in% c(6,8))
THdat2 <- subset(FG3, group == "large_herbs")

sum(THdat$biomass==0)/nrow(THdat)
# way too many zero - aggregating to plot level
THdat <- aggregate(data=THdat, biomass~Region+LocalityName+Treatment+group, FUN = mean)
THdat2 <- aggregate(data=THdat2, biomass~Region+LocalityName+Treatment+group+yse, FUN = mean)
sum(THdat$biomass==0)/nrow(THdat) #ok

TH_glm <- lme(log(biomass+1) ~ Treatment, random = ~1|Region/LocalityName, 
              data = THdat)
TH_glm <- lme(log(biomass+1) ~ Treatment*yse, random = ~1|Region/LocalityName, 
              data = THdat2) # negative fitted values

TH_glm <- glmmTMB(biomass+0.1 ~ Treatment*yse + (1|Region/LocalityName), 
                  data = THdat2,
                  family = Gamma(link = "log"))

TH_glm <- glmer(biomass+0.1 ~ Treatment*scale(yse, scale=F) +(1|Region/LocalityName), 
                  data = THdat2,
                  family = Gamma)

summary(TH_glm)
plot(TH_glm)
R <- resid(TH_glm, type = "pearson")
plot(fitted(TH_glm), R)
boxplot(log(THdat$biomass+1)~as.factor(THdat$Treatment))
boxplot(R~as.factor(THdat2$Treatment))
boxplot(R~as.factor(THdat$Treatment))


# tHIS WORKS BEST, EVENT WITH A FEW NEGATIVE FITTED VALUES
TH_glm <- lme(log(biomass+1) ~ Treatment*yse, random = ~1|Region/LocalityName, 
              data = THdat2) # negative fitted values




# PRC
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