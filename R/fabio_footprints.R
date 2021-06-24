##############################################################################################
##  FABIO Footprints
##############################################################################################

library(Matrix)
library(tidyverse)
library(data.table)

rm(list=ls()); gc()

is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }


#-------------------------------------------------------------------------
# Make intitial settings
#-------------------------------------------------------------------------
# read region classification
regions <- fread(file="/mnt/nfs_fineprint/tmp/fabio/v2/regions.csv")
# read commodity classification
items <- fread(file="/mnt/nfs_fineprint/tmp/fabio/v2/items.csv")
items_ghg <- read.csv("./input/items.csv")
items_ghg <- rep(items_ghg$item_code, 192)
nrreg <- nrow(regions)
nrcom <- nrow(items)
index <- data.table(code = rep(regions$code, each = nrcom),
  iso3c = rep(regions$iso3c, each = nrcom),
  country = rep(regions$name, each = nrcom),
  item = rep(items$item, nrreg),
  group = c(rep("veg", 52), rep("nonfood",11), rep("veg", 27), rep("alc",4), rep("nonfood",15), 
  "dairy", "dairy", "eggs", "nonfood", "bovine meat", "mutton & goat meat", "pigmeat", 
  "poultry meat", "meat, other", "offals, edible", "fats, animals", rep("nonfood", 4), "fish, seafood"))

X <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v2/X.rds"))
Y <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v2/Y.rds"))
E <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v2/E.rds"))


# country = "EU27"
# extension = "landuse"
# consumption = "food"
# allocation = "value"

footprint <- function(country = "EU27", extension = "landuse", allocation = "value"){
  # Prepare Multipliers
  ext <- as.vector(as.matrix(as.vector(Ei[, ..extension]) / as.vector(Xi)))
  ext[!is.finite(ext)] <- 0
  MP <- ext * L
  # Calculate detailed footprints
  FP <- data.frame(index, value = (colSums(MP) * as.vector(as.matrix(Yi[,country]))))
  results <- FP %>% 
    as_tibble() %>% 
    group_by(item) %>% 
    summarize(value = round(sum(value)))
  
  return(results)
}



#-------------------------------------------------------------------------
# Calculate detailed footprints
#-------------------------------------------------------------------------
allocations = c("mass","value")
years <- c(1987, 2012)
year <- 2012

# Read data
Xi <- as.vector(X[, as.character(year-1)] + X[, as.character(year)] + X[, as.character(year+1)]) / 3
Yi <- (as.matrix(Y[[as.character(year-1)]]) + as.matrix(Y[[as.character(year)]]) + as.matrix(Y[[as.character(year+1)]])) / 3
Ei <- E[[as.character(year)]]
Eiminus1 <- E[[as.character(year-1)]]
Eiplus1 <- E[[as.character(year+1)]]
Ei$landuse <- (Ei$landuse + Eiminus1$landuse + Eiplus1$landuse) / 3
Ei$biomass <- (Ei$biomass + Eiminus1$biomass + Eiplus1$biomass) / 3
Ei$blue <- (Ei$blue + Eiminus1$blue + Eiplus1$blue) / 3
Ei$green <- (Ei$green + Eiminus1$green + Eiplus1$green) / 3

load(paste0("/mnt/nfs_fineprint/tmp/fabio/ghg/E_ghg_",year-1,".RData"))
emissions <- rowSums(t(E_ghg[,-1]))
load(paste0("/mnt/nfs_fineprint/tmp/fabio/ghg/E_ghg_",year,".RData"))
emissions <- emissions + rowSums(t(E_ghg[,-1]))
load(paste0("/mnt/nfs_fineprint/tmp/fabio/ghg/E_ghg_",year+1,".RData"))
emissions <- emissions + rowSums(t(E_ghg[,-1]))
emissions <- emissions / 3

load(paste0("/mnt/nfs_fineprint/tmp/fabio/ghg/E_luh2_",year-1,".RData"))
emissions_luc <- rowSums(t(E_luh2[c(2,7),-1]))
load(paste0("/mnt/nfs_fineprint/tmp/fabio/ghg/E_luh2_",year,".RData"))
emissions_luc <- emissions_luc + rowSums(t(E_luh2[c(2,7),-1]))
load(paste0("/mnt/nfs_fineprint/tmp/fabio/ghg/E_luh2_",year+1,".RData"))
emissions_luc <- emissions_luc + rowSums(t(E_luh2[c(2,7),-1]))
emissions_luc <- emissions_luc / 3

Ei$ghg <- (emissions + emissions_luc)[items_ghg %in% items$item_code]
Ei$water <- (Ei$blue + Ei$green)

Y_codes <- data.frame(code = substr(colnames(Yi), 1, str_locate(colnames(Yi), "_")[,1]-1))
Y_codes$iso3c = regions$iso3c[match(Y_codes$code,regions$code)]
Y_codes$continent = regions$continent[match(Y_codes$iso3c,regions$iso3c)]
Y_codes$fd <- substr(colnames(Yi), str_locate(colnames(Yi), "_")[,1]+1, 100)

extensions <- colnames(Ei)[c(8,12,13)]
colnames(Yi) <- Y_codes$iso3c
Yi <- Yi[, Y_codes$fd != "other"]
Yi <- agg(Yi)

allocation = "mass"
L <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v2/",year-1,"_L_",allocation,".rds"))
L <- L + readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v2/",year,"_L_",allocation,".rds"))
L <- L + readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v2/",year+1,"_L_",allocation,".rds"))
L <- L / 3

countries <- colnames(Yi)
country <- "ITA"
extension = "landuse"

results <- data.frame(index, landuse = 0, ghg = 0, water = 0)

for(country in countries){
  for(extension in extensions){
    # calculate footprints
    data <- footprint(country = country, extension = extension, allocation = allocation)
    results[results$iso3c==country, extension] <- data$value[match(results$item[results$iso3c==country], data$item)]
  }
}

results$consumption <- round(as.vector(t(agg(t(Yi)))))

write_csv(results, "output/results_full.csv")

results <- results %>% 
  group_by(code, iso3c, country, group) %>% 
  summarize(landuse = sum(landuse), ghg = sum(ghg), water = sum(water), consumption = sum(consumption))

write_csv(results, "output/results.csv")


