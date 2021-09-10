##############################################################################################
##  FABIO Footprints
##############################################################################################

library(Matrix)
library(tidyverse)
library(data.table)
library(wbstats)

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
others <- "Vegetables, fruit, nuts, pulses, oilseeds, others"
index <- data.table(code = rep(regions$code, each = nrcom),
  iso3c = rep(regions$iso3c, each = nrcom),
  country = rep(regions$name, each = nrcom),
  item = rep(items$item, nrreg),
  # group = c(rep("veg", 52), rep("nonfood",11), rep("veg", 27), rep("alc",4), rep("nonfood",15),
  group = c(items$comm_group[1:14], rep(others, 30), items$comm_group[45:47], rep(others, 5), 
            rep("nonfood",10), rep(others, 2), items$comm_group[65:81], rep("nonfood",9), 
            rep("alc",4), rep("nonfood",15), "dairy", "dairy", "eggs", "nonfood", 
            rep("meat", 7), rep("nonfood", 4), "fish, seafood"))

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
year <- years[2]

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

# items_ghg <- read_csv("/mnt/nfs_fineprint/tmp/fabio/ghg/items_ghg.csv")
# items_luh2 <- read_csv("/mnt/nfs_fineprint/tmp/fabio/ghg/items_luh2.csv")
ghg <- readRDS("/mnt/nfs_fineprint/tmp/fabio/ghg/E_ghg_mass.rds")
luh2 <- readRDS("/mnt/nfs_fineprint/tmp/fabio/ghg/E_luh2_mass.rds")
Ei$ghg <- (colSums(ghg[[as.character(year-1)]]) + 
  colSums(ghg[[as.character(year)]]) + colSums(ghg[[as.character(year-1)]])) / 3
Ei$luh2 <- (colSums(luh2[[as.character(year-1)]][1:2,]) + 
  colSums(luh2[[as.character(year)]][1:2,]) + colSums(luh2[[as.character(year-1)]][1:2,])) / 3
Ei$water <- (Ei$blue + Ei$green)
Ei[, cropland := ifelse(item == "Grazing", 0, landuse)]
Ei[, grazing := ifelse(item == "Grazing", landuse, 0)]

Y_codes <- data.frame(code = substr(colnames(Yi), 1, str_locate(colnames(Yi), "_")[,1]-1))
Y_codes$iso3c = regions$iso3c[match(Y_codes$code,regions$code)]
Y_codes$continent = regions$continent[match(Y_codes$iso3c,regions$iso3c)]
Y_codes$fd <- substr(colnames(Yi), str_locate(colnames(Yi), "_")[,1]+1, 100)

extensions <- colnames(Ei)[12:16]
colnames(Yi) <- Y_codes$iso3c
Yi <- Yi[, Y_codes$fd %in% c("food", "losses")]
Yi <- agg(Yi)

allocation = "mass"
L <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v2/",year-1,"_L_",allocation,".rds"))
L <- L + readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v2/",year,"_L_",allocation,".rds"))
L <- L + readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v2/",year+1,"_L_",allocation,".rds"))
L <- L / 3

countries <- colnames(Yi)
country <- "ITA"
extension = "landuse"

cbs <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/data/tidy/cbs_tidy.rds")
countries <- regions[code %in% unique(cbs$area_code[cbs$year==1986]) & 
  code %in% unique(cbs$area_code[cbs$year==1987]) & 
  code %in% unique(cbs$area_code[cbs$year==1988]) & 
  code %in% unique(cbs$area_code[cbs$year==2011]) & 
  code %in% unique(cbs$area_code[cbs$year==2012]) & 
  code %in% unique(cbs$area_code[cbs$year==2013]) &
  countries %in% iso3c & iso3c != "TWN", iso3c]

results <- data.frame(index, ghg = 0, luh2 = 0, water = 0, cropland = 0, grazing = 0)

for(country in countries){
  print(paste(match(country, countries), "/", length(countries)))
  for(extension in extensions){
    # calculate footprints
    data <- footprint(country = country, extension = extension, allocation = allocation)
    results[results$iso3c==country, extension] <- data$value[match(results$item[results$iso3c==country], data$item)]
  }
}

results$consumption <- round(as.vector(t(agg(t(Yi)))))

write_csv(results, paste0("output/results_full_",year,".csv"))
# results <- read_csv(paste0("output/results_full_",year,".csv"))

results <- results %>% 
  group_by(code, iso3c, country, group) %>% 
  summarize(ghg = sum(ghg), luh2 = sum(luh2), water = sum(water), 
            cropland = sum(cropland), grazing = sum(grazing), consumption = sum(consumption))

data <- wbstats::wb(indicator = c("SP.POP.TOTL", "SP.URB.TOTL.IN.ZS", "NY.GDP.MKTP.CD", "NY.GDP.MKTP.PP.CD"), 
                    startdate = year-1, enddate = year+1) %>% 
  group_by(iso3c, indicator) %>% 
  summarize(value = mean(value)) %>% 
  spread(indicator, value)

results <- bind_cols(results, data[match(results$iso3c, data$iso3c),-1])

results <- results[results$iso3c %in% countries,]

write_csv(results, paste0("output/results_",year,".csv"))
# results <- read_csv(paste0("output/results_",year,".csv"))

