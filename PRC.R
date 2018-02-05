

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
unique(cdat$loc_year)
thinned <- c("Selbu_Flub.8", "Malvik.8", "Hi_tydal.8")
cdat2 <- cdat[!cdat$loc_year %in% thinned,]
cdat <- cdat2;rm(cdat2)



# Species groups ####
# group grasses that proved difficult to identify in the field
# Keeping Avenella and Des Cea as they are. 
# Carex is grouped together. 

# Luzula is grouped


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
              carex, 
              luzula )
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
# check for zeros. They should be NAs:
#cdatBM[cdatBM == 0] <- NA

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




# PRC
library(vegan)
# combining species that are hard to identify in the field
PRCdat <- cdatBM
PRCdat$other_graminoids <- rowSums(PRCdat[, sub_other_graminoids], na.rm=T)
PRCdat$Carex <- rowSums(PRCdat[, carex], na.rm=T)
PRCdat$Epilobium <-  rowSums(PRCdat[, epilobium], na.rm=T)
PRCdat$Alchemilla <- rowSums(PRCdat[, alchemilla], na.rm=T)
PRCdat$Galeopsis <-  rowSums(PRCdat[, galeopsis], na.rm=T)
PRCdat2 <- PRCdat[,!colnames(PRCdat) %in% c(carex, epilobium, alchemilla, galeopsis)]
PRCdat <- PRCdat2;rm(PRCdat2)

PRCdat2 <- PRCdat
PRCdat[is.na(PRCdat)] <- 0
rm(PRCdat2)



# plot time series ####
library(reshape2)
long <- melt(data = PRCdat, id.vars = c("yse", "Region", "LocalityName", "Treatment", "Plot") , measure.vars = names(PRCdat[,16:ncol(PRCdat)]))
names(long)[names(long) == "variable"] <- "taxa"
names(long)[names(long) == "value"]    <- "biomass"

long$cyse <- as.character(long$yse)
# get means per plot and year
timeS <- aggregate(data = long, 
                   biomass~Region+cyse+LocalityName+Treatment+taxa, FUN = mean)
timeS2 <- aggregate(data = timeS, 
                   biomass~cyse+Treatment+taxa, FUN = mean)
ggplot(data = timeS2, aes(x=cyse, y=biomass, group = taxa))+
  geom_line()+
  theme_classic()+
  facet_grid(. ~ Treatment, scales = "fixed")
#outlier in year4?



timeS3 <- dcast(data = timeS2, cyse+taxa~Treatment, value.var = "biomass", fun.aggregate = mean)
timeS3$diff <- timeS3$UB-timeS3$B 

library(ggplot2)
ggplot(data = timeS3, aes(x=cyse, y=diff, group = taxa))+
         geom_line()+
  theme_classic()

# label the most extreme species
diff_sp <- unique(timeS3$taxa[timeS3$diff >0.2])
# calluna is extreme - investigate!






# taking out the localitions effect
rda.out <- rda(PRCdat[,16:ncol(PRCdat)] ~ PRCdat$LocalityName) # equivalent of "rda.out = rda(X ~ Condition(Z))"
rda.out # location explains 31%
rda.res <- residuals(rda.out)
plot(rda.out)

prc.out <- prc(rda.res, as.factor(PRCdat$Treatment), 
               as.factor(PRCdat$yse))
sum_mod_prc <- summary(prc.out, scaling = 2)
#species <- colSums(rda.res)
par(mar=c(5,5,10,10))
plot(prc.out, select = abs(sum_mod_prc$sp) > 0.1, scaling = 2)


prc.out.region <- prc(PRCdat[,16:ncol(PRCdat)], as.factor(PRCdat$Region), 
               as.factor(PRCdat$yse))
sum_mod_prc2 <- summary(prc.out.region, scaling = 2)
plot(prc.out.region, select = abs(sum_mod_prc$sp)>0.1, scaling = 2)

prc.out.location <- prc(PRCdat[,16:ncol(PRCdat)], as.factor(PRCdat$LocalityName), 
                      as.factor(PRCdat$yse))
sum_mod_prc3 <- summary(prc.out.location, scaling = 2)
plot(prc.out.location, select = abs(sum_mod_prc3$sp)>0.1, scaling = 2, legpos = NA)
