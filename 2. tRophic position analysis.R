# TP & Alpha analysis: Rangitāhua terrestrial, n >= 5 -----------

selectedPeriod4RangitāhuaT <- selectedPeriod4Rangitāhua %>%
  # keep all terrestrial rows, but marine rows only if they are baseline species
  filter(
    (Terres.Marine == "Terrestrial" & Species %in% keepSpeciesRT) |
      (Terres.Marine == "Marine" & Species %in% macroalgaeR))  %>%
  mutate(FunctionalGroup = case_when(
    # Terrestrial primary producers as baseline
    Species %in% terrestrialPlantsR ~ "Terrestrial_BL",
    # Marine primary producers as baseline  
    Species %in% macroalgaeR ~ "Marine_BL",
    # All other species remain as consumers (keep their species names)
    TRUE ~ Species
  ))

# Check the results
table(selectedPeriod4RangitāhuaT$FunctionalGroup)

# Verify which species are in each baseline group
selectedPeriod4RangitāhuaT %>%
  filter(FunctionalGroup == "Terrestrial_BL") %>%
  pull(Species) %>%
  unique()

selectedPeriod4RangitāhuaT %>%
  filter(FunctionalGroup == "Marine_BL") %>%
  pull(Species) %>%
  unique()


table(selectedPeriod4RangitāhuaT$CommonGroupName)
table(selectedPeriod4RangitāhuaT$Species)

#selectedPeriod4Rangitāhua <- selectedPeriod4Rangitāhua %>%
# mutate(GroupGuild = paste(Group, GuildCapitalized, sep = "-"))

# Then extract with BOTH baselines (like the Bilagay example)
selectedPeriod4RangitāhuaTList <- extractIsotopeData(
  selectedPeriod4RangitāhuaT,
  b1 = "Terrestrial_BL",           # baselineColumn argument
  b2 = "Marine_BL",                 # baselineColumn argument  
  baselineColumn = "FunctionalGroup", # Column containing baseline labels
  consumersColumn = "Species",        # Column with consumer species names
  #groupsColumn = "Terres.Marine",    # Groups communities by location
  d13C = "d13CSeussCorrected",       # The carbon column
  d15N = "d15N"                       # The nitrogen column
)



str(selectedPeriod4RangitāhuaTList)
str(selectedPeriod4RangitāhuaTList[1:1])

length(selectedPeriod4RangitāhuaTList)
names(selectedPeriod4RangitāhuaTList)



selectedPeriod4RangitāhuaT_models <- multiSpeciesTP(selectedPeriod4RangitāhuaTList, model = "twoBaselinesFull",
                                                    n.adapt = 10000, n.iter = 10000,
                                                    burnin = 10000, n.chains = 5, print = FALSE)

#a <- multiSpeciesTP(selectedPeriod4RangitāhuaTList[1], model = "twoBaselinesFull",
#                   n.adapt = 100, n.iter = 100,
#                    burnin = 100, n.chains = 5, print = FALSE)
head(selectedPeriod4RangitāhuaT_models$df)
names(selectedPeriod4RangitāhuaT_models$df)
table(selectedPeriod4RangitāhuaT_models$df$consumer)

# By default the mode is used in both trophic position and alpha plots
credibilityIntervals(selectedPeriod4RangitāhuaT_models$df, x = "consumer", xlab ="Consumer group") #this plots the mode

# If you want to use the median instead of the mode,
# just add y1 and y2 as arguments
credibilityIntervals(selectedPeriod4RangitāhuaT_models$df, x = "consumer", xlab ="Consumer group", #this plots the median
                     y1 = "median", y2 = "alpha.median")

credibilityIntervals(selectedPeriod4RangitāhuaT_models$df, x = "consumer", xlab = "Consumer Group")

# To get a numerical summary
sapply(selectedPeriod4RangitāhuaT_models$"TPs", quantile, probs = c(0.025, 0.5, 0.975)) %>% round(3)

getPosteriorMode(selectedPeriod4RangitāhuaT_models$"TPs")


# To get a numerical summary
sapply(selectedPeriod4RangitāhuaT_models$"TPs", quantile, probs = c(0.025, 0.5, 0.975)) %>% round(3)

# To get the mode
getPosteriorMode(selectedPeriod4RangitāhuaT_models$"TPs")

# First, we compare selectedPeriod4Rangitāhua posterior trophic position estimates
pairwiseTP <- pairwiseComparisons(selectedPeriod4RangitāhuaT_models$TPs, print = TRUE)

# And then, we compare their posterior alpha estimates
pairwiseAlpha <- pairwiseComparisons(selectedPeriod4RangitāhuaT_models$Alphas, print = TRUE)


# First, we compare selectedPeriod4Rangitāhua posterior trophic position estimates
TPRangitāhuaT <- pairwiseTP <- pairwiseComparisons(selectedPeriod4RangitāhuaT_models$TPs, print = TRUE)

# And then, we compare their posterior alpha estimates
AlphaRangitāhuaT <- pairwiseAlpha <- pairwiseComparisons(selectedPeriod4RangitāhuaT_models$Alphas, print = TRUE)


# Numerical summary (95% CI and median)
sapply(selectedPeriod4RangitāhuaT_models$Alphas, quantile, probs = c(0.025, 0.5, 0.975)) %>% round(3)

# Posterior mode
getPosteriorMode(selectedPeriod4RangitāhuaT_models$Alphas)

# Plot of α credibility intervals by consumer
credibilityIntervals(selectedPeriod4RangitāhuaT_models$df, x = "consumer",
                     y1 = "alpha.median", y2 = "alpha.median")

### IMPROVEMENT (pasted from Meyers terrestrial)

desired_order <- c("Philanisus fasciatus", "Diarsia intermixa", "Hydrillodes surata", 
                   "Cryptamorpha desjardinsi", "Phrissogonus sp.", "Geograpsus grayi", 
                   "Cyanoramphus novaezelandiae", "Prosthemadera novaeseelandiae", 
                   "Turdus merula", "Turdus philomelos", "Todiramphus sanctus", 
                   "Rattus norvegicus")

# Sort the dataframe by the factor levels
selectedPeriod4RangitāhuaT_models$df <- selectedPeriod4RangitāhuaT_models$df %>%
  arrange(consumer)

# Filter to only species in desired_order
selectedPeriod4RangitāhuaT_filtered <- selectedPeriod4RangitāhuaT[selectedPeriod4RangitāhuaT$Species %in% desired_order, ]

# Calculate sample sizes
sample_sizes <- table(selectedPeriod4RangitāhuaT_filtered$Species)

# IMPORTANT: Reorder sample_sizes to match desired_order
sample_sizes_ordered <- sample_sizes[desired_order]

# Create labels with species name and n in the correct order
labels_with_n <- paste0(desired_order, "\n(n=", sample_sizes_ordered, ")")

# Update the factor levels to include n
selectedPeriod4RangitāhuaT_models$df$consumer <- factor(
  selectedPeriod4RangitāhuaT_models$df$consumer,
  levels = desired_order,
  labels = labels_with_n
)

credibilityIntervals(selectedPeriod4RangitāhuaT_models$df, x = "consumer", xlab = "Consumer group - Rangitāhua Terrestrial, (Mode, n ≥ 5)",
                     y1 = "median", y2 = "alpha.median")




# TP & Alpha analysis: Rangitāhua marine,  all spp with n >=5, all algae baseline  ---------------------------------------------


table(selectedPeriod4RangitāhuaM$Species)
table(selectedPeriod4RangitāhuaM$Period)


## Keep selected columns only

selectedPeriod4RangitāhuaM <- selectedPeriod4RangitāhuaM %>%
  dplyr::select(IdentifierEditedMK, Species,Terres.Marine, Group, GuildCapitalized, 
                CommonGroupName, Feeding.habit.Niche, Terres.Marine,
                CollectionDate, CollectionYear, Locality.Main2, Locality.sub,
                SampleTypeDetails, d15N, d13CSeussCorrected, Period)


selectedPeriod4RangitāhuaM <- selectedPeriod4RangitāhuaM %>%
  mutate(FunctionalGroup = case_when(
    Species %in% macroalgaeR ~ "Marine_BL",
    # All other species remain as consumers (keep their species names)
    TRUE ~ Species
  ))
table(selectedPeriod4RangitāhuaM$FunctionalGroup, selectedPeriod4RangitāhuaM$CommonGroupName)
table(selectedPeriod4RangitāhuaM$Species)


#selectedPeriod4 <- selectedPeriod4 %>%
# mutate(GroupGuild = paste(Group, GuildCapitalized, sep = "-"))

# Then extract with BOTH baselines (like the Bilagay example)
selectedPeriod4RangitāhuaMList <- extractIsotopeData(
  selectedPeriod4RangitāhuaM,
  b1 = "Marine_BL",           # baselineColumn argument
  b2 = NULL,                 # baselineColumn argument  
  baselineColumn = "FunctionalGroup", # Column containing baseline labels
  consumersColumn = "Species",        # Column with consumer species names
  #groupsColumn = "Terres.Marine",    # Groups communities by location
  d13C = "d13CSeussCorrected",       # The carbon column
  d15N = "d15N"                       # The nitrogen column
)



str(selectedPeriod4RangitāhuaMList)
str(selectedPeriod4RangitāhuaMList[1:1])

length(selectedPeriod4RangitāhuaMList)
names(selectedPeriod4RangitāhuaMList)



desired_order <- c(
  "Guano",
  "Euphausiids",  
  "Zooplankton",
  "Modiolus auriculatus",
  "Scutellastra kermadecensis",
  "Tectus royanus",
  "Neothais smithi",
  "Annaperenna verrucosa",
  "Dardanus hessii",
  "Leptograsus variegatus",
  "Tesseropora rosea",
  "Tripneustes australiae",
  "Heliocidaris tuberculata",
  "Astrostole rodolphi",
  "Acanthaster planci",
  "Pateriella oliveri",
  "Gymnothorax nubilus",
  "Engyprosopon raoulense",
  "Upeneus francisi",
  "Scorpaena cardinalis",
  "Seriola lalandi",
  "Arripis xylabion",
  "Acanthistius cinctus",
  "Kyphosus bigibbus",
  "Pseudolabrus luculentus",
  "Chromis dispilus",
  "Chrysiptera rapanui",
  "Stegastes fasciolatus")

# Then run the model as before
selectedPeriod4RangitāhuaM_models <- multiSpeciesTP(selectedPeriod4RangitāhuaMList, model = "oneBaseline",
                                                    n.adapt = 10000, n.iter = 10000,
                                                    burnin = 10000, n.chains = 5, print = FALSE)




#a <- multiSpeciesTP(selectedPeriod4RangitāhuaMList[1], model = "oneBaseline",
#                   n.adapt = 100, n.iter = 100,
#                  burnin = 100, n.chains = 5, print = FALSE)
head(selectedPeriod4RangitāhuaM_models$df)
names(selectedPeriod4RangitāhuaM_models$df)
table(selectedPeriod4RangitāhuaM_models$df$consumer)

selectedPeriod4RangitāhuaM_models$df$consumer <- factor(selectedPeriod4RangitāhuaM_models$df$consumer, 
                                                        levels = desired_order)

# By default the mode is used in both trophic position and alpha plots
credibilityIntervals(selectedPeriod4RangitāhuaM_models$df, x = "consumer", xlab ="Consumer group") #this plots the mode

# If you want to use the median instead of the mode,
# just add y1 and y2 as arguments
credibilityIntervals(selectedPeriod4RangitāhuaM_models$df, x = "consumer", xlab ="Consumer group", #this plots the median
                     y1 = "median", y2 = "alpha.median")

credibilityIntervals(selectedPeriod4RangitāhuaM_models$df, x = "consumer", xlab = "Consumer Group")

# To get a numerical summary
sapply(selectedPeriod4RangitāhuaM_models$"TPs", quantile, probs = c(0.025, 0.5, 0.975)) %>% round(3)

getPosteriorMode(selectedPeriod4RangitāhuaM_models$"TPs")


# To get a numerical summary
sapply(selectedPeriod4RangitāhuaM_models$"TPs", quantile, probs = c(0.025, 0.5, 0.975)) %>% round(3)

# To get the mode
getPosteriorMode(selectedPeriod4RangitāhuaM_models$"TPs")
posteriorModeRangitāhuaMarineAllAlgaeN5 <- getPosteriorMode(selectedPeriod4RangitāhuaM_models$"TPs")

# First, we compare selectedPeriod4 posterior trophic position estimates
pairwiseTPRangitāhuaMarineGuanoN5 <- pairwiseComparisons(selectedPeriod4RangitāhuaM_models$TPs, print = TRUE)

# And then, we compare their posterior alpha estimates
#pairwiseAlphaRangitāhuaMarineGuanoN5 <- pairwiseComparisons(selectedPeriod4RangitāhuaM_models$Alphas, print = TRUE)


# First, we compare selectedPeriod4 posterior trophic position estimates
pairwiseTP <- pairwiseComparisons(selectedPeriod4RangitāhuaM_models$TPs, print = TRUE)

# And then, we compare their posterior alpha estimates
#pairwiseAlpha <- pairwiseComparisons(selectedPeriod4RangitāhuaM_models$Alphas, print = TRUE)


## IMPROVEMENT

# Sort the dataframe by the factor levels
selectedPeriod4RangitāhuaM_models$df <- selectedPeriod4RangitāhuaM_models$df %>%
  arrange(consumer)

# Filter to only species in desired_order
selectedPeriod4RangitāhuaM_filtered <- selectedPeriod4RangitāhuaM[selectedPeriod4RangitāhuaM$Species %in% desired_order, ]

# Calculate sample sizes
sample_sizes <- table(selectedPeriod4RangitāhuaM_filtered$Species)

# IMPORTANT: Reorder sample_sizes to match desired_order
sample_sizes_ordered <- sample_sizes[desired_order]

# Create labels with species name and n in the correct order
labels_with_n <- paste0(desired_order, "\n(n=", sample_sizes_ordered, ")")

# Update the factor levels to include n
selectedPeriod4RangitāhuaM_models$df$consumer <- factor(
  selectedPeriod4RangitāhuaM_models$df$consumer,
  levels = desired_order,
  labels = labels_with_n
)

credibilityIntervals(selectedPeriod4RangitāhuaM_models$df, x = "consumer", xlab = "Consumer group - Rangitāhua Marine, n≥5",
                     main = "Rangitāhua Marine (Mode, n ≥ 5)",
                     y1 = "median", y2 = "alpha.median")


# TP & Alpha analysis: Meyer Islands marine,  all spp with n >=5, all algae baseline  ---------------------------------------------


table(selectedPeriod4MeyersM$Species)
table(selectedPeriod4Meyers$Period)
table(selectedPeriod4Meyers$SampleType)

## Keep selected columns only

selectedPeriod4Meyers <- selectedPeriod4Meyers %>%
  dplyr::select(IdentifierEditedMK, Species,Terres.Marine, Group, GuildCapitalized, 
                CommonGroupName, Feeding.habit.Niche, Terres.Marine,
                CollectionDate, CollectionYear,  Locality.Main2, Locality.sub,
                SampleTypeDetails, d15N, d13CSeussCorrected, Period)


selectedPeriod4MeyersM <- selectedPeriod4MeyersM %>%
  mutate(FunctionalGroup = case_when(
    Species %in% macroalgaeM ~ "Marine_BL",
    # All other species remain as consumers (keep their species names)
    TRUE ~ Species
  ))
table(selectedPeriod4MeyersM$FunctionalGroup, selectedPeriod4MeyersM$CommonGroupName)
table(selectedPeriod4MeyersM$Species)


#selectedPeriod4 <- selectedPeriod4 %>%
# mutate(GroupGuild = paste(Group, GuildCapitalized, sep = "-"))

# Then extract with BOTH baselines (like the Bilagay example)
selectedPeriod4MeyersList <- extractIsotopeData(
  selectedPeriod4MeyersM,
  b1 = "Marine_BL",           # baselineColumn argument
  b2 = NULL,                 # baselineColumn argument  
  baselineColumn = "FunctionalGroup", # Column containing baseline labels
  consumersColumn = "Species",        # Column with consumer species names
  #groupsColumn = "Terres.Marine",    # Groups communities by location
  d13C = "d13CSeussCorrected",       # The carbon column
  d15N = "d15N"                       # The nitrogen column
)



str(selectedPeriod4MeyersList)
str(selectedPeriod4MeyersList[1:1])

length(selectedPeriod4MeyersList)
names(selectedPeriod4MeyersList)



desired_order <- c(
  "Guano",
  "Hydnophora pilosa",         
  "Montipora spongoides",        
  "Pocillopora damicornis",      
  "Turbinaria frondens",         
  "Actinia tenebrosa",           
  "Modiolus auriculatus",        
  "Hinea brasiliana",            
  "Scutellastra kermadecensis",  
  "Tectus royanus",              
  "Annaperenna verrucosa",      
  "Acanthaster planci",       
  "Scorpaena cardinalis",        
  "Pseudolabrus luculentus",    
  "Chrysiptera rapanui"     
)

# Then run the model as before
selectedPeriod4Meyers_models <- multiSpeciesTP(selectedPeriod4MeyersList, model = "oneBaseline",
                                               n.adapt = 10000, n.iter = 10000,
                                               burnin = 10000, n.chains = 5, print = FALSE)




#a <- multiSpeciesTP(selectedPeriod4MeyersList[1], model = "oneBaseline",
#                   n.adapt = 100, n.iter = 100,
#                  burnin = 100, n.chains = 5, print = FALSE)
head(selectedPeriod4Meyers_models$df)
names(selectedPeriod4Meyers_models$df)
table(selectedPeriod4Meyers_models$df$consumer)

selectedPeriod4Meyers_models$df$consumer <- factor(selectedPeriod4Meyers_models$df$consumer, 
                                                   levels = desired_order)

# By default the mode is used in both trophic position and alpha plots
credibilityIntervals(selectedPeriod4Meyers_models$df, x = "consumer", xlab ="Consumer group") #this plots the mode

# If you want to use the median instead of the mode,
# just add y1 and y2 as arguments
credibilityIntervals(selectedPeriod4Meyers_models$df, x = "consumer", xlab ="Consumer group", #this plots the median
                     y1 = "median", y2 = "alpha.median")

credibilityIntervals(selectedPeriod4Meyers_models$df, x = "consumer", xlab = "Consumer Group")

# To get a numerical summary
sapply(selectedPeriod4Meyers_models$"TPs", quantile, probs = c(0.025, 0.5, 0.975)) %>% round(3)

getPosteriorMode(selectedPeriod4Meyers_models$"TPs")


# To get a numerical summary
sapply(selectedPeriod4Meyers_models$"TPs", quantile, probs = c(0.025, 0.5, 0.975)) %>% round(3)

# To get the mode
getPosteriorMode(selectedPeriod4Meyers_models$"TPs")
posteriorModeMeyersMarineAllAlgaeN5 <- getPosteriorMode(selectedPeriod4Meyers_models$"TPs")

# First, we compare selectedPeriod4 posterior trophic position estimates
pairwiseTPMeyersMarineGuanoN5 <- pairwiseComparisons(selectedPeriod4Meyers_models$TPs, print = TRUE)

# And then, we compare their posterior alpha estimates
#pairwiseAlpha <- pairwiseComparisons(selectedPeriod4Meyers_models$Alphas, print = TRUE)


# First, we compare selectedPeriod4 posterior trophic position estimates
pairwiseTP <- pairwiseComparisons(selectedPeriod4Meyers_models$TPs, print = TRUE)

# And then, we compare their posterior alpha estimates
#pairwiseAlpha <- pairwiseComparisons(selectedPeriod4Meyers_models$Alphas, print = TRUE)


## IMPROVEMENT

# Sort the dataframe by the factor levels
selectedPeriod4Meyers_models$df <- selectedPeriod4Meyers_models$df %>%
  arrange(consumer)

# Filter to only species in desired_order
selectedPeriod4Meyers_filtered <- selectedPeriod4Meyers[selectedPeriod4Meyers$Species %in% desired_order, ]

# Calculate sample sizes
sample_sizes <- table(selectedPeriod4Meyers_filtered$Species)

# IMPORTANT: Reorder sample_sizes to match desired_order
sample_sizes_ordered <- sample_sizes[desired_order]

# Create labels with species name and n in the correct order
labels_with_n <- paste0(desired_order, "\n(n=", sample_sizes_ordered, ")")

# Update the factor levels to include n
selectedPeriod4Meyers_models$df$consumer <- factor(
  selectedPeriod4Meyers_models$df$consumer,
  levels = desired_order,
  labels = labels_with_n
)

credibilityIntervals(selectedPeriod4Meyers_models$df, x = "consumer", xlab = "Consumer group - Meyers Marine, n≥5",
                     main = "Meyers Marine (Mode, n ≥ 5)",
                     y1 = "median", y2 = "alpha.median")






