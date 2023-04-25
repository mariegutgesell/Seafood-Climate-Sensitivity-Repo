###Project:Seafood Climate Sensitivity
###Code: Joining databases to generate dataframe for analysis
###Date Created: 25/04/2023
###By: Marie Gutgesell

library(tidyverse)
library(readxl)
library(taxize)


##Import list of commercial species in Great Lakes
##source: Great Lakes Fishery Commission: http://www.glfc.org/commercial/commerc.php
gl_commercial_specieslist <- read.csv("data/GL_Commercial Catch_Species List.csv") %>%
  dplyr::rename(Species = "Species.Name")

##obtain family using taxize
gl_commercial_famlist <- gl_commercial_specieslist%>%
  separate(Species, into = c("Genus", "Species")) %>%
  select(-Species) %>%
  left_join(gl_commercial_specieslist, by = "Common.Name") 

gl_commercial_fams <- tax_name(gl_commercial_famlist$Genus, get = "family", db = "itis") 

##join family back to original species list
gl_commercial_specieslist <- cbind(gl_commercial_famlist, gl_commercial_fams) %>%
  select(-db, -query)

rm(gl_commercial_famlist, gl_commercial_fams)

##Import thermal data
##source: 
thermal_data_orig <- read_excel("data/Comte_Olden_Data.xlsx", sheet = "Original_data")
thermal_data_orig <- thermal_data_orig %>%
  separate(Species, into = c("Genus", "Species")) %>%
  select(-Species) %>%
  cbind(thermal_data_orig)
thermal_data_orig <- thermal_data_orig[,c(1,2, 26, 3:24)]

thermal_data_imputed <- read_excel("data/Comte_Olden_Data.xlsx", sheet = "Imputed_data")
thermal_data_imputed <- thermal_data_imputed %>%
  separate(Species, into = c("Genus", "Species")) %>%
  select(-Species) %>%
  cbind(thermal_data_imputed)
thermal_data_imputed <- thermal_data_imputed[,c(1,2,7:10)]

##Join original temperature data to great lakes species list
gl_sp_therm_df_orig <- left_join(gl_commercial_specieslist, thermal_data_orig, by = c("Species", "Genus"), multiple = "all")
#gl_sp_therm_df_imputed <- left_join(gl_commercial_specieslist, thermal_data_imputed, by = c("Genus"), multiple = "all")

##Determine which "species" from the Great Lakes are missing in the original thermal data
sp_missing_tempdata <- gl_sp_therm_df_orig   %>%
  filter(is.na(`Thermal limit (°C)`))
##12 missing -- 4 species, 7 genus, 1 family

##Potential approach (approach taken here):
##For missing species, see if there is imputed CTmax data from Compte and Odum -- if not, look in GlobTherm or Hasnian et al., 2018 to see if can fill in
##For genuses: take mean of genus (or family) in first: original data if present, imputed data, then GlobTherm and then Hasnian... (maybe Hasnian before GlobTherm, also see how different values are)

##All original CTmax data for SPECIES level ID in Great Lakes 
thermal_data_orig_sp <- gl_sp_therm_df_orig %>%
  filter(!is.na(`Thermal limit (°C)`)) %>%
  rename(CTmax = "Thermal limit (°C)", SD_CTmax = "SD Thermal limit") %>%
  select(Species, Genus, family, `Life stage`, Methodology, Endpoint, Acclimation, `Temperature of acclimation (°C)`, CTmax, SD_CTmax) %>%
  #summarise(mean_ctmax = mean(`Thermal limit (°C)`), sd_ctmax = sd(`Thermal limit (°C)`)) %>%
  mutate(Source = "Compte_and_Odum_OriginalData") %>%
  rename(Species_GLFC = "Species", Family = "family") %>%
  select(Species_GLFC, Genus, Family, `Life stage`, Methodology, Endpoint, Acclimation, `Temperature of acclimation (°C)`, CTmax, SD_CTmax, Source)

##Getting imputed CTmax for SPECIES missing original data from Compte and Odum
thermal_data_imputed_sp <- gl_sp_therm_df_orig %>%
  filter(is.na(`Thermal limit (°C)`)) %>%
  left_join(thermal_data_imputed, by = c("Species", "Genus")) %>%
  select(Species, Genus, family, `Life stage`, Methodology, Endpoint, Acclimation, `Temperature of acclimation (°C)`, CTmax, `SD CTmax`) %>%
  rename(SD_CTmax = "SD CTmax", Species_GLFC = "Species", Family = "family") %>% 
  mutate(Source = "Compte_and_Odum_ImputedData") %>%
  filter(!is.na(CTmax)) %>%
  select(Species_GLFC, Genus, Family, `Life stage`, Methodology, Endpoint, Acclimation, `Temperature of acclimation (°C)`, CTmax, SD_CTmax, Source)
##have imputed ctmax for 2/4 species

##looking for 2 more missing species in GlobTherm (Sander vitreus glaucum, Prosopium cylindraceum) ##note: only Sander vitreus glaucum missing from Hasnian et al., 2018
##From Hasnian: Prosopium is estimated (need to look into sources/how was estimated)

##Original CTmax data for GENUS level ID in Great Lakes (7 genus')
##Approach: use all original data from specified genus -- calculate means later
thermal_data_orig_genus <- thermal_data_orig %>%
  filter(Genus %in% c("Catostomus", "Coregonus", "Ictalurus", "Ictiobus", "Lepomis", "Moxostoma", "Oncorhynchus")) %>%
  rename(CTmax = "Thermal limit (°C)", SD_CTmax = "SD Thermal limit") %>%
  select(Species, Genus, Family, `Life stage`, Methodology, Endpoint, Acclimation, `Temperature of acclimation (°C)`, CTmax, SD_CTmax) %>%
  #summarise(mean_ctmax = mean(`Thermal limit (°C)`), sd_ctmax = sd(`Thermal limit (°C)`)) %>%
  mutate(Source = "Compte_and_Odum_OriginalData") %>%
  mutate(Species_GLFC = case_when(
    startsWith(Genus, "Catost") ~ "Catostomus spp.",
    startsWith(Genus, "Coreg") ~ "Coregonus spp.",
    startsWith(Genus, "Ictal") ~ "Ictalurus spp.",
    startsWith(Genus, "Ictio") ~ "Ictiobus spp.",
    startsWith(Genus, "Lepo") ~ "Lepomis spp.",
    startsWith(Genus, "Moxo") ~ "Moxostoma spp.",
    startsWith(Genus, "Oncor") ~ "Oncorhynchus spp.",
  )) %>%
  select(Species_GLFC, Genus, Family, `Life stage`, Methodology, Endpoint, Acclimation, `Temperature of acclimation (°C)`, CTmax, SD_CTmax, Source) ##don't run this is want to retain what all the different species are within the genus that were used to calculate genus level CTmax

##quick test to make sure have data for all genus'
thermal_data_orig_genus_summary <- thermal_data_orig_genus %>%
  group_by(Genus) %>%
  summarise(mean_ctmax_genus = mean(CTmax), sd_ctmax_genus = sd(CTmax))
##yes, have genus level CTmax in original data for genus'

##Original CTmax data for FAMILY level ID in Great Lakes (1 family)
##Approach: use all original data from specified family -- calculate means later
##Potential option: don't include carp, as they are separate within the GLFC data, but I guess can't guarentee they weren't sampled as minnows?
thermal_data_orig_fam <- thermal_data_orig %>%
  filter(Family == "Cyprinidae") %>%
  #filter(Species != "Cyprinus carpio") %>% ##removes carp, but can stop this if don't want to do
  rename(CTmax = "Thermal limit (°C)", SD_CTmax = "SD Thermal limit") %>%
  select(Species, Genus, Family, `Life stage`, Methodology, Endpoint, Acclimation, `Temperature of acclimation (°C)`, CTmax, SD_CTmax) %>%
  mutate(Source = "Compte_and_Odum_OriginalData", Species_GLFC = "Cyprinidae spp.") %>%
  select(Species_GLFC, Genus, Family, `Life stage`, Methodology, Endpoint, Acclimation, `Temperature of acclimation (°C)`, CTmax, SD_CTmax, Source)


##Join all data together
thermal_data_all <- rbind(thermal_data_orig_sp, thermal_data_imputed_sp, thermal_data_orig_genus, thermal_data_orig_fam)

##remove all df other than the one you will continue analysis with
rm(list=setdiff(ls(), c("thermal_data_all")))


##So all together, only 2 species are missing from Compte and Odum b/w the original and the imputed data
##looking for 2 more missing species in GlobTherm (Sander vitreus glaucum, Prosopium cylindraceum) ##note: only Sander vitreus glaucum missing from Hasnian et al., 2018
##From Hasnian: Prosopium is estimated (need to look into sources/how was estimated)
##Now to find CTmax from GlobTherm for two missing species: Sander vitreus glaucum, Prosopium cylindraceum

globtherm_temp <- read.csv("data/GlobalTherm_upload_02_11_17.csv") %>%
  unite(Species_GLFC, Genus, Species, sep = " ")

##see if two missing species exist in GlobTherm
globtherm_temp_missingsp <- globtherm_temp %>%
  filter(Species_GLFC %in% c("Sander vitreus glaucum", "Prosopium cylindraceum"))
##Neither species exists in GlobTherm

hasnian_temp <- read_excel("data/Hasnian et al., 2018_Table S1.xlsx", sheet = "Sheet1", skip = 3)
hasnian_temp_missingsp <- hasnian_temp %>%
  filter(`Scientific Name` %in% c("Sander vitreus glaucum", "Prosopium cylindraceum")) %>%
  dplyr::rename(Species_GLFC = "Scientific Name", CTmax = "CTMax") %>%
  mutate(Source = "Hasnian_Estimated_Data") %>%
  select(Species_GLFC, CTmax, Source) %>%
  separate(CTmax, into = c("CTmax", "SE_CTmax"), sep = " ")

library(plyr)
thermal_data_all <- rbind.fill(thermal_data_all, hasnian_temp_missingsp)
detach("package:plyr", unload = TRUE)
str(thermal_data_all)

##have Prosopium -- need to extract CTmax and add to rest of thermal data -- also note, this data is estimated based on analysis in Hasnian et al., 2018 (need to double check how similar methods are to Compte and Odum)
##Also has standard error but not standard deviation -- 
##Blue pike was declared extinct in 1983 by US Fish and Wildlife Service (also considered sub-species of Walleye)

##Summary Statistics and Data Distribution
thermal_data_all$CTmax <- as.numeric(thermal_data_all$CTmax)

thermal_data_all_summary <- thermal_data_all %>%
  dplyr::group_by(Species_GLFC, Source) %>%
  dplyr::summarise(mean_ctmax = mean(CTmax), sd_ctmax = sd(CTmax))


hist(thermal_data_all_summary$mean_ctmax)
hist(thermal_data_all$CTmax)

rm(list=setdiff(ls(), c("thermal_data_all", "thermal_data_all_summary")))
##Questions:
##Use average CTmax per species? what if collected different ways? 
##how different are imputed vs. original values? -- compare for species where this is available

##comparing imputed vs. avg. original data
thermal_data_orig_summary_3 <- gl_sp_therm_df %>%
  group_by(Species) %>%
  summarise(mean_ctmax = mean(`Thermal limit (°C)`), sd_ctmax = sd(`Thermal limit (°C)`))

thermal_data_comparison <- left_join(thermal_data_orig_summary_3, thermal_data_imputed, by = "Species")
thermal_data_comparison_plot <- ggplot(thermal_data_comparison, aes(x = mean_ctmax, y = CTmax)) +
  geom_point()+
  theme_classic() +
  xlab("Mean CTmax based on Original Data") +
  ylab("Phylogenetic Imputed CTmax")
thermal_data_comparison_plot
##mean ctmax from original data is not the same as the imputed data, close, but not the exact same

##All imputed CTmax data for GENUS level ID in Great Lakes -- DONT NEED THIS -- but could use to compare original vs. imputed at genus level
##Approach: use all original data from specified genus -- calculate means later
thermal_data_imputed_genus <- gl_sp_therm_df %>%
  filter(is.na(`Thermal limit (°C)`)) %>%
  filter(!Genus %in% c("Acipenser", "Prosopium", "Sander", "Cyprinidae", "Anguilla")) %>% ##filter out SPECIES or FAMILY level IDs (only want genus')
  left_join(thermal_data_imputed, by = "Genus", multiple = "all") %>%
  select(Species.x, Species.y, Genus, family, `Life stage`, Methodology, Endpoint, Acclimation, `Temperature of acclimation (°C)`, CTmax, `SD CTmax`) %>%
  rename(Species_GLFC = "Species.x", Species_CO = "Species.y") %>%
  mutate(Source = "Compte_and_Odum_OriginalData")


