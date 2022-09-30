#for bringing in points, starting with just CK's ND data, but could expand to include gbif at a later date.

library(rnaturalearthdata)
library(rnaturalearth)
library(dismo)
library(maps)
library(mapproj)
library(ggthemes)
library(sf)
library(tidyverse)
library(conflicted)
library(readxl)

#read in net and traps and then combine after adding row for collection type
bee_netting <- read.csv("data/XYBeeData_27_Sept_2022_netting.csv")
bee_passive <- read.csv("data/XYBeeData_27_Sept_2022_passive.csv")

#need to remove males
bee_netting <- bee_netting %>% filter(Male. != 'x')
bee_passive <- bee_passive %>% filter(Male. != 'x')


#note only need a few columns
bee_netting <- as.data.frame(cbind('X' = bee_netting$X_Site, 'Y' = bee_netting$Y_Site, 'Species' = bee_netting$Bee.Species, 'Count' = bee_netting$Bee.Count))
bee_passive <- as.data.frame(cbind('X' = bee_passive$X_Site, 'Y'= bee_passive$Y_Site, 'Species' = bee_passive$Bee.Species, 'Count' = c('1')))

bee_netting$Method <- c('netting')
bee_passive$Method <- c('passive')

bee_data <- rbind(bee_netting, bee_passive)

#turn individual lines to count data
#loses the count and methods, but keeping in mind here that anything at the same site is going to be lost anyway
bee_sum_loc <- bee_data %>%
  dplyr::filter(Species != 0 & Species != ' ' & Species != '\n') %>%
  group_by(Species, X, Y) %>%
  tally()

#this can then be subset for later pieces, but also getting total n
bee_sum <- bee_data %>%
  dplyr::filter(Species != 0 & Species != ' ' & Species != '\n') %>%
  group_by(Species) %>%
  tally() %>%
  arrange(desc(n))

#take top 10
bees_common <- bee_sum[1:2,]
bees_common <- bees_common$Species

#sort the location data for just those bees
bee_occ <- bee_sum_loc %>%
  dplyr::filter(Species %in% bees_common)

bee_occ$Species <- as.factor(bee_occ$Species)
bee_occ$X <- as.numeric(bee_occ$X)
bee_occ$Y <- as.numeric(bee_occ$Y)

write.csv(bee_occ, './outputs/bee_occ.csv', row.names = FALSE)