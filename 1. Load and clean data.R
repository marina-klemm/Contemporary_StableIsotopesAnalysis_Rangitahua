# Load packages -----------------------------------------------------------
require(box, quietly = TRUE)
require(car, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(ggbeeswarm, quietly = TRUE)
require(ggimage, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(ggpubr, quietly = TRUE)
require(ggstatsplot, quietly = TRUE)
require(grid, quietly = TRUE)
require(gridExtra, quietly = TRUE)
require(htmlwidgets, quietly = TRUE)
require(kableExtra, quietly = TRUE)
require(nortest, quietly = TRUE)
require(openxlsx, quietly = TRUE)
require(patchwork, quietly = TRUE)
require(plotly, quietly = TRUE)
require(purrr, quietly = TRUE)
require(readr, quietly = TRUE)
require(readxl, quietly = TRUE)
require(SIBER, quietly = TRUE)
require(tibble, quietly = TRUE)
require(tidyverse, quietly = TRUE)
library(dplyr)
library(rjags)
library(tidyr)
library(tRophicPosition)
library(broom)
library(car)
require(emmeans)

# Load and clean stable isotopes data -----------------------------------------------

stableIsotopesSpecimens <- read.delim("~/Documents/TMoR/4. Stable Isotopes/2. Results/EditedMK_COMPLETE_Master_SI_Rangitahua_July2025.txt", 
                                      header=TRUE)

# Explore data structure and unique values
str(stableIsotopesSpecimens)
sapply(stableIsotopesSpecimens[c("CollectionYear", "Species", "SpeciesAlgaeCorrectedSafePaste", 
                                 "Locality.main", "SampleType", "Feeding.habit.Niche", 
                                 "GuildCapitalized")], function(x) length(unique(x)))

# Filter and count Kermadec Islands records
stableIsotopesSpecimens %>%
  filter(Locality.main == "Kermadec Islands") %>%
  count(CommonGroupName, CollectionYear)



# Locality clean up

# Define locality standardization mapping
localityNewNames <- list(
  "Meyer" = "Meyer Islands",
  "Raoul" = "Rangitāhua", 
  "Herald" = "Herald Island",
  "Macauley" = "Macauley Island",
  "Kermadec Islands" = "Rangitāhua"  # Early collections likely from Rangitāhua
)

# Apply locality standardization
for (pattern in names(localityNewNames)) {
  stableIsotopesSpecimens <- stableIsotopesSpecimens %>%
    mutate(Locality.main = ifelse(grepl(pattern, Locality.main, ignore.case = TRUE), 
                                  localityNewNames[[pattern]], Locality.main))
}

# done with it, can delete it
rm(localityNewNames)

# Clean sub-locality
stableIsotopesSpecimens$Locality.sub <- ifelse(stableIsotopesSpecimens$Locality.sub == 0, 
                                               "Unknown", stableIsotopesSpecimens$Locality.sub)

## Correct locality for the three guano samples mistakenly assigned to Rangitāhua

stableIsotopesSpecimens %>%
  filter(SampleType == "Guano") %>%
  pull(Locality.main) %>%
  table()

stableIsotopesSpecimens %>%
  filter(SampleType == "Guano") %>%
  pull(Sample.ID) %>%
  table()

stableIsotopesSpecimens <- stableIsotopesSpecimens %>%
  mutate(Locality.main = case_when(
    Sample.ID %in% c("RS1564", "RS1565", "RS1566") ~ "Meyer Islands",
    TRUE ~ Locality.main
  ))



# Feeding guild clean up

# Standardize feeding guilds
stableIsotopesSpecimens <- stableIsotopesSpecimens %>%
  mutate(
    GuildCapitalized = case_when(
      grepl("Omnivore|Detritivore", GuildCapitalized, ignore.case = TRUE) ~ "Omnivore/Detritivore",
      grepl("Predator", GuildCapitalized, ignore.case = TRUE) ~ "Carnivore",
      grepl("Can be scavenger, predator or algae feeders", Ecological.metadata, ignore.case = TRUE) ~ "Omnivore/Detritivore",
      TRUE ~ GuildCapitalized
    ),
    # Standardize group names
    Group = case_when(
      grepl("Terrestrial invertebrates", Group, ignore.case = TRUE) ~ "Terrestrial Invertebrates",
      grepl("Terrestrial/Marine invertebrate", Group, ignore.case = TRUE) ~ "Terrestrial/Marine Invertebrate",
      TRUE ~ Group
    )
  )



# Species names corrections

unique(stableIsotopesSpecimens$Species)

# Define species name corrections
speciesNewNames <- list(
  "Cyanoramphus novaezelandie" = "Cyanoramphus novaezelandiae",
  "Turdus philomenos" = "Turdus philomelos",
  "Turdus philomelos " = "Turdus philomelos",
  "Pheloide megacephala" = "Pheidole megacephala", 
  "Heliocidaris tuberculata " = "Heliocidaris tuberculata",
  "Tripneustes gratilla" = "Tripneustes australiae",
  "Puweto" = "Zapornia tabuensis plumbea",
  "Caulerpa webbiaana" = "Caulerpa webbiana",
  "Caulerpa racemosa/chemitzia" = "Caulerpa chemnitzia",
  "Caulerpa chemitzia" = "Caulerpa chemnitzia",
  "Delisea sp. nov." = "Delisea aweawetai",
  "Codium sp." = "Codium tohuoratai",
  "Chrysiptera rapanui" = "Chrysiptera sp. nov.",
  "Diarsia intermixa" = "Diarsia intermixta",
  
  "Myoporum rapense subsp. kermadecense" = "Myoporum rapense subsp. kermadecense",
  "Pyrrosia eleagnifolia" = "Pyrrosia elaeagnifolia",
  "Melicytus ramiflorus ramiflorus" = "Melicytus ramiflorus",
  "Ugyops (Paracona) raouli" =  "Ugyops sp.",
  "Ugyops" = "Ugyops sp."
)

# Remove Pyrrosia serpens 
stableIsotopesSpecimens <- stableIsotopesSpecimens %>%
  filter(Species != "Pyrrosia serpens")

# Apply species corrections
for (old_name in names(speciesNewNames)) {
  stableIsotopesSpecimens <- stableIsotopesSpecimens %>%
    mutate(Species = ifelse(grepl(old_name, Species, ignore.case = TRUE), 
                            speciesNewNames[[old_name]], Species))
}

table(stableIsotopesSpecimens$Species)

# done with it, can remove it
unique(stableIsotopesSpecimens$Species)
rm(speciesNewNames)


# Remove phytoplankton records
stableIsotopesSpecimens <- stableIsotopesSpecimens %>%
  filter(Species != "Phytoplankton")

# Remove guano
stableIsotopesSpecimens <- stableIsotopesSpecimens %>%
  filter(SampleType != "Guano")




# Other last corrections

# Update specific species classifications
stableIsotopesSpecimens <- stableIsotopesSpecimens %>%
  mutate(
    CommonGroupName = case_when(
      Species == "Euphausiids" ~ "Krill",
      TRUE ~ CommonGroupName
    ),
    GuildCapitalized = case_when(
      Species == "Zooplankton" ~ "Omnivore/Detritivore",
      CommonGroupName == "Shrimp" ~ "Omnivore/Detritivore",
      TRUE ~ GuildCapitalized
    ),
    CommonGroupName = case_when(
      CommonGroupName == "Lacewig" ~ "Lacewing",
      TRUE ~ CommonGroupName
    )
  )

table(stableIsotopesSpecimens$CommonGroupName)

# Fish mobility changes

# Create Locality.Main2 column for mobile fish species
stableIsotopesSpecimens$Locality.Main2 <- stableIsotopesSpecimens$Locality.main

# Update locality for highly mobile fish species (likely moved between islands)
mobileFish <- c("Arripis xylabion", "Seriola lalandi", "Chromis dispilus", "Gymnothorax nubilus")
stableIsotopesSpecimens$Locality.Main2[stableIsotopesSpecimens$Species %in% mobileFish] <- "Rangitāhua"

# Update fish feeding guilds with specific ecological data
newFishGuilds <- data.frame(
  Species = c("Arripis xylabion", "Chrysiptera rapanui", "Engyprosopon raoulense", 
              "Pseudocaranx sp.", "Scorpis violacea"),
  NewGuild = c("Carnivore/Planktivore", "Omnivore/Planktivore", "Carnivore", 
               "Carnivore/Planktivore", "Planktivore"),
  stringsAsFactors = FALSE
)

# Apply fish guild updates
for (i in 1:nrow(newFishGuilds)) {
  stableIsotopesSpecimens$GuildCapitalized[stableIsotopesSpecimens$Species == newFishGuilds$Species[i]] <- 
    newFishGuilds$NewGuild[i]
}

# done with it, can remove it
rm(newFishGuilds)

# Final data processing

# Clean C:N mass ratio data
stableIsotopesSpecimens$C.N.mass.ratio[stableIsotopesSpecimens$C.N.mass.ratio %in% c("ND", "#DIV/0!")] <- NA

# Add temporal period classification
stableIsotopesSpecimens$Period <- case_when(
  #stableIsotopesSpecimens$CollectionYear <= 1870 ~ "Period 1\n(1854-1920)",
  stableIsotopesSpecimens$CollectionYear <= 1920 ~ "Period 1\n(1854-1920)",
  stableIsotopesSpecimens$CollectionYear >= 1921 & stableIsotopesSpecimens$CollectionYear <= 1983 ~ "Period 2\n(1921-1983)",
  stableIsotopesSpecimens$CollectionYear >= 1984 & stableIsotopesSpecimens$CollectionYear <= 2001 ~ "Period 3\n(1984-2001)",
  stableIsotopesSpecimens$CollectionYear >= 2002 ~ "Period 4\n(2002-2023)"
)

# Create backup of processed data
#stableIsotopesSpecimensBackup <- stableIsotopesSpecimens

# Summary of final dataset
cat("Final dataset summary:\n")
cat("Total records:", nrow(stableIsotopesSpecimens), "\n")
cat("Unique species:", length(unique(stableIsotopesSpecimens$Species)), "\n")
cat("Unique localities:", length(unique(stableIsotopesSpecimens$Locality.main)), "\n")
cat("Unique guilds:", length(unique(stableIsotopesSpecimens$GuildCapitalized)), "\n")
table(stableIsotopesSpecimens$Period)

table(stableIsotopesSpecimens$Species)

# Lipid correction and seuss effect adjustment ----------------------------

## Correct arthropods lipid content first:

stableIsotopesSpecimens$C.N.mass.ratio <- as.numeric(stableIsotopesSpecimens$C.N.mass.ratio)

stableIsotopesSpecimens <- stableIsotopesSpecimens %>%
  mutate(
    # Calculate lipid content estimate (L)
    L = -20.54 + (7.24 * C.N.mass.ratio),
    
    # Apply lipid correction formula for arthropods with C:N > 7
    lipid_corrected_d13C_arthropods = case_when(
      # For arthropods with C:N ratio > 7
      CommonGroupName %in% c("Ant", "Fungus gnats", "Fly", "Beetle", "Bee", "Spider",
                             "Millipede", "Cricket", "Collembola", "Dragonfly", 
                             "Katydid", "Moth", "Marine caddisfly", "Planthoppers",
                             "Land crab", "Shrimp", "Porcelain crab", "Hermit crab", 
                             "Rock crab", "Barnacle", "Damsel bug", "Book lice", 
                             "Lacewing", "Krill") & 
        C.N.mass.ratio > 7 ~ 
        normalised.d13C + 7.018 * (0.048 + 3.90 / (1 + 287 / L)),
      
      # For all other cases, keep original value
      TRUE ~ normalised.d13C
    )
  )

# Check which arthropods had correction applied
correctedArthropods <- stableIsotopesSpecimens %>%
  filter(CommonGroupName %in% c("Ant", "Fungus gnats", "Fly", "Beetle", "Bee", "Spider",
                                "Millipede", "Cricket", "Collembola", "Dragonfly", 
                                "Katydid", "Moth", "Marine caddisfly", "Planthoppers",
                                "Land crab", "Shrimp", "Porcelain crab", "Hermit crab", 
                                "Rock crab", "Barnacle", "Damsel bug", "Book lice", 
                                "Lacewing", "Krill")) %>%
  dplyr::select(Sample.ID, CommonGroupName, C.N.mass.ratio, normalised.d13C, 
                lipid_corrected_d13C_arthropods) %>%
  mutate(corrected = C.N.mass.ratio > 7)


# Initialize dataset and rename columns
lipidCorrectedData <- stableIsotopesSpecimens %>%
  rename(
    d15N = normalised.d15N,
    d13C = lipid_corrected_d13C_arthropods
  )

# Apply lipid correction where available (replace d13C with lipid-corrected values)
lipidCorrectedData$d13C <- ifelse(
  is.na(lipidCorrectedData$Lipid.corrected.d13C.where.mass.C.N.ratio..3.5), 
  lipidCorrectedData$d13C, 
  lipidCorrectedData$Lipid.corrected.d13C.where.mass.C.N.ratio..3.5
)

# Remove rows with missing isotope values and ensure numeric format
seussCorrectedData <- lipidCorrectedData %>%
  filter(!is.na(d13C), !is.na(d15N)) %>%
  mutate(
    d13C = as.numeric(d13C),
    d15N = as.numeric(d15N),
    # Apply Seuss Effect Correction: d13C - 1 + 1.1 * (2024 - CollectionYear) * 0.027
    d13CSeussCorrected = d13C - 1 + 1.1 * (2024 - CollectionYear) * 0.027
  )

# Summary of corrections applied
cat("Data processing summary:\n")
cat("Original records:", nrow(stableIsotopesSpecimens), "\n")
cat("Records after lipid correction:", nrow(lipidCorrectedData), "\n")
cat("Records after removing NA values:", nrow(seussCorrectedData), "\n")
cat("Lipid corrections applied:", sum(!is.na(stableIsotopesSpecimens$Lipid.corrected.d13C.where.mass.C.N.ratio..3.5)), "\n")

# Display sample of corrections (first 5 rows)
sample_data <- seussCorrectedData %>%
  dplyr::select(d13C, d15N, CollectionYear, d13CSeussCorrected) %>%
  slice_head(n = 5)

cat("\nSample of corrected values:\n")
print(sample_data)

seussCorrectedData <- seussCorrectedData %>%
  rename(
    percentageCarbon = `All.Correct.Amt..C.Flash.TCD`,
    percentageNitrogen = `All.Correct.Amt..N.Flash.TCD`
  )


# Tissue comparison for arthropods ------------------

# Data preparation

# Filter data for batch test samples and create tissue type indicators
prepare_batch_data <- function(data) {
  data %>%
    filter(Box.Name == "Batch test - invertebrates") %>%
    mutate(
      PartType = ifelse(grepl(" t$", Sample.ID), "rest", "muscle"),
      SampleBase = sub(" t$", "", Sample.ID),
      Subphyla = case_when(
        CommonGroupName %in% c("Shrimp", "Land crab") ~ "Crustacea",
        CommonGroupName == "Spider" ~ "Arachnid",
        CommonGroupName %in% c("Planthoppers", "Marine caddisfly") ~ "Insecta",
        TRUE ~ NA_character_
      )
    )
}

# Create paired data for statistical testing
create_paired_data <- function(samples, exclude_groups = "Marine caddisfly") {
  samples %>%
    dplyr::select(CommonGroupName, Subphyla, SampleBase, PartType, d15N, d13CSeussCorrected) %>%
    filter(CommonGroupName != exclude_groups) %>%
    pivot_wider(names_from = PartType, values_from = c(d15N, d13CSeussCorrected)) %>%
    filter(
      !is.na(d15N_rest) & !is.na(d15N_muscle) &
        !is.na(d13CSeussCorrected_rest) & !is.na(d13CSeussCorrected_muscle)
    )
}

# Stats analysis functions

# Perform paired t-tests for each group
perform_t_tests <- function(paired_data, group_var) {
  paired_data %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      tTest_N = list(t.test(d15N_rest, d15N_muscle, paired = TRUE)),
      tTest_C = list(t.test(d13CSeussCorrected_rest, d13CSeussCorrected_muscle, paired = TRUE)),
      .groups = "drop"
    )
}

# Extract summary statistics from t-tests
summarize_t_tests <- function(t_test_results, group_var) {
  t_test_results %>%
    mutate(
      p_value_N = map_dbl(tTest_N, ~ .x$p.value),
      p_value_C = map_dbl(tTest_C, ~ .x$p.value),
      mean_diff_N = map_dbl(tTest_N, ~ .x$estimate),
      mean_diff_C = map_dbl(tTest_C, ~ .x$estimate)
    ) %>%
    dplyr::select(!!sym(group_var), p_value_N, p_value_C, mean_diff_N, mean_diff_C)
}

# Plotting functions

# Create violin/box/scatter plot for isotope values
create_isotope_plot <- function(data, x_var, y_var, title, y_label) {
  # Filter for valid groups (at least 2 samples per tissue type)
  valid_groups <- data %>%
    group_by(!!sym(x_var), PartType) %>%
    summarize(n = n(), .groups = "drop") %>%
    pivot_wider(names_from = PartType, values_from = n) %>%
    filter(!is.na(muscle) & !is.na(rest) & muscle >= 2 & rest >= 2) %>%
    pull(!!sym(x_var))
  
  filtered_data <- data %>%
    filter(!!sym(x_var) %in% valid_groups)
  
  ggplot(filtered_data, 
         aes(x = reorder(!!sym(x_var), -!!sym(y_var)), 
             y = !!sym(y_var), 
             fill = PartType)) +
    geom_boxplot(width = 0.1, position = position_dodge(width = 0.9), alpha = 0.3) +
    geom_violin(trim = FALSE, alpha = 0.6, position = position_dodge(width = 0.9)) +
    geom_point(aes(color = PartType), position = position_dodge(width = 0.9), 
               size = 2, alpha = 0.9) +
    stat_compare_means(
      aes(group = PartType),
      method = "t.test",
      label = "p.signif",
      label.y = max(filtered_data[[y_var]]) + 1,
      hide.ns = TRUE
    ) +
    labs(
      title = title,
      x = "",
      y = y_label,
      fill = "",
      color = "Tissue Type"
    ) +
    scale_fill_manual(
      values = c("muscle" = "#919c4c", "rest" = "#fd8f24"),
      labels = c("muscle" = "Muscle", "rest" = "Rest of the body"),
      guide = "none"
    ) +
    scale_color_manual(
      values = c("muscle" = "#919c4c", "rest" = "#fd8f24"),
      labels = c("muscle" = "Muscle", "rest" = "Rest of the body")
    ) +
    guides(color = guide_legend(override.aes = list(shape = 15, size = 5))) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      plot.title = element_text(hjust = 0.5),
      axis.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12)
    )
}

# Add significance annotations to plot
add_significance_annotations <- function(plot, data, x_var, y_var, t_test_summary, p_col) {
  # Get ordered levels as they appear in the plot
  ordered_levels <- levels(reorder(data[[x_var]], -data[[y_var]]))
  
  # Create annotation data frame
  annotations <- t_test_summary %>%
    filter(!!sym(p_col) < 0.05) %>%
    mutate(
      x = match(!!sym(x_var), ordered_levels),
      y = max(data[[y_var]]) + 1,
      p_label = paste0("p = ", format(round(!!sym(p_col), 3), nsmall = 3)),
      label = paste0("*\np = ", format(round(!!sym(p_col), 3), nsmall = 3))
    )
  
  plot +
    geom_text(
      data = annotations,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      size = 4,
      vjust = -0.5
    )
}

# Data cleaning functions

# Remove duplicate samples randomly
remove_duplicate_samples <- function(data, seed = 123) {
  set.seed(seed)
  
  # Get samples to remove
  samples_to_remove <- data %>%
    filter(Box.Name == "Batch test - invertebrates") %>%
    mutate(BaseID = gsub(" t$", "", Sample.ID)) %>%
    group_by(BaseID) %>%
    mutate(group_size = n()) %>%
    ungroup() %>%
    group_by(BaseID) %>%
    filter(group_size > 1 & row_number() != sample(1:n(), 1)) %>%
    ungroup()
  
  # Remove duplicates from main dataset
  data %>%
    anti_join(samples_to_remove, by = "Sample.ID")
}

# Filter data to specific localities
filter_localities <- function(data, localities = c("Rangitāhua", "Meyer Islands")) {
  data %>%
    filter(
      Locality.Main2 %in% localities,
      LostSample == "0"
    )
}





run_arthropod_analysis <- function(seussCorrectedData) {
  cat("Starting arthropod tissue comparison analysis...\n\n")
  
  # 1. Prepare batch test data
  cat("1. Preparing batch test data...\n")
  box11Samples <- prepare_batch_data(seussCorrectedData)
  
  # 2. Create paired data for statistical testing
  cat("2. Creating paired data for statistical testing...\n")
  box11PairedData <- create_paired_data(box11Samples)
  
  # 3. Statistical analysis by CommonGroupName
  cat("3. Running statistical tests by CommonGroupName...\n")
  tTestResults_group <- perform_t_tests(box11PairedData, "CommonGroupName")
  tTestSummary_group <- summarize_t_tests(tTestResults_group, "CommonGroupName")
  
  cat("T-test results by CommonGroupName:\n")
  print(tTestSummary_group)
  cat("\n")
  
  # 4. Statistical analysis by Subphyla
  cat("4. Running statistical tests by Subphyla...\n")
  tTestResults_subphyla <- perform_t_tests(box11PairedData, "Subphyla")
  tTestSummary_subphyla <- summarize_t_tests(tTestResults_subphyla, "Subphyla")
  
  cat("T-test results by Subphyla:\n")
  print(tTestSummary_subphyla)
  cat("\n")
  
  # 5. Create plots
  cat("5. Creating visualization plots...\n")
  
  # δ13C plots
  plot_d13C_group <- create_isotope_plot(
    box11Samples, "CommonGroupName", "d13CSeussCorrected",
    expression(paste("Arthropods ", delta^13, "C (Corrected for Seuss effect) Values by Common Group Name")),
    expression(delta^13*C)
  )
  
  plot_d15N_group <- create_isotope_plot(
    box11Samples, "CommonGroupName", "d15N",
    expression(paste("Arthropods ", delta^15, "N Values by Common Group Name")),
    expression(delta^15*N)
  )
  
  plot_d13C_subphyla <- create_isotope_plot(
    box11Samples, "Subphyla", "d13CSeussCorrected",
    expression(paste("Arthropods ", delta^13, "C Values (corrected for Seuss effect) by Subphyla")),
    expression(paste(delta^13, "C (‰)"))
  )
  
  plot_d15N_subphyla <- create_isotope_plot(
    box11Samples, "Subphyla", "d15N",
    expression(paste("Arthropods ", delta^15, "N Values by Subphyla")),
    expression(paste(delta^15,"N (‰)"))
  )
  
  # Add significance annotations to subphyla plots
  plot_d13C_subphyla_annotated <- add_significance_annotations(
    plot_d13C_subphyla, box11Samples, "Subphyla", "d13CSeussCorrected", 
    tTestSummary_subphyla, "p_value_C"
  )
  
  # 6. Clean main dataset
  cat("6. Cleaning main dataset...\n")
  cleaned_data <- seussCorrectedData %>%
    filter_localities() %>%
    remove_duplicate_samples()
  
  cat("Data cleaning complete!\n")
  cat("Original samples in batch test:", nrow(filter(seussCorrectedData, Box.Name == "Batch test - invertebrates")), "\n")
  cat("Samples after cleaning:", nrow(filter(cleaned_data, Box.Name == "Batch test - invertebrates")), "\n\n")
  
  # 7. Summary and conclusions
  cat("SUMMARY OF FINDINGS:\n")
  cat("===================\n")
  cat("For Arachnida and Insecta: Muscle and rest tissues appear interchangeable\n")
  cat("for stable isotope analysis, for both nitrogen and carbon.\n\n")
  cat("For Crustacea, particularly δ13C, consider:\n")
  cat("1. Using consistent tissue types within these groups\n")
  cat("2. Conducting further investigation with larger sample sizes\n")
  cat("3. Acknowledging potential tissue-specific variations in methods\n\n")
  
  # Return results
  list(
    batch_samples = box11Samples,
    paired_data = box11PairedData,
    t_test_group = tTestSummary_group,
    t_test_subphyla = tTestSummary_subphyla,
    plots = list(
      d13C_group = plot_d13C_group,
      d15N_group = plot_d15N_group,
      d13C_subphyla = plot_d13C_subphyla_annotated,
      d15N_subphyla = plot_d15N_subphyla
    ),
    cleaned_data = cleaned_data
  )
}

# Run the complete analysis
results <- run_arthropod_analysis(seussCorrectedData)

# Display individual plots
print(results$plots$d13C_group)
print(results$plots$d15N_group)
print(results$plots$d13C_subphyla)
print(results$plots$d15N_subphyla)

# Access cleaned data
final_data <- results$cleaned_data

# Save clean dataset
#write_csv(results$cleaned_data, "Stable_Isotopes_Results_Cleaned_July3rd2025.csv")


rm(final_data)
rm(lipidCorrectedData)
rm(results)
rm(sample_data)

# Remove localities that are not Rangitāhua or Meyers for rest of the analysis --------

## For the other analysis, drop the rows where LocalityTerrestrial is not Rangitāhua or Meyers:
unique(seussCorrectedData$Locality.Main2)

seussCorrectedData <- seussCorrectedData %>%
  filter(Locality.Main2 %in% c("Rangitāhua", "Meyer Islands"))

unique(seussCorrectedData$LostSample)

seussCorrectedData <- seussCorrectedData %>%
  filter(LostSample %in% c("0"))

unique(seussCorrectedData$GuildCapitalized)
unique(seussCorrectedData$CollectionYear)
unique(seussCorrectedData$Locality.Main2)
unique(seussCorrectedData$Sample.ID)


# Remove the arthropods duplicated samples --------------------------------

seussCorrectedData %>%
  filter(Box.Name == "Batch test - invertebrates") %>% 
  pull(Sample.ID) %>%      # Extract the SampleID column
  unique()                # Get unique SampleIDs

set.seed(123)  # Set a seed for reproducibility

# Ensure all BaseIDs are accounted for
filteredBox11RandomSelection <- seussCorrectedData %>%
  filter(Box.Name == "Batch test - invertebrates") %>%  # Filter rows by Box.Name
  mutate(BaseID = gsub(" t$", "", Sample.ID)) %>%       # Create BaseID by removing " t"
  group_by(BaseID) %>%
  mutate(group_size = n()) %>%                         # Count rows in each group
  ungroup() %>%
  group_by(BaseID) %>%
  # Randomly sample one row per group, but keep all rows for single-value groups
  filter(group_size == 1 | row_number() == sample(1:n(), 1)) %>%
  ungroup() %>%
  dplyr::select(-group_size)  # Remove temporary column

print(filteredBox11RandomSelection$Sample.ID)

# Create a dataset with the values left out from the filtering
box11RemainingRows <- seussCorrectedData %>%
  filter(Box.Name == "Batch test - invertebrates") %>% # Filter rows by Box.Name
  anti_join(filteredBox11RandomSelection, by = "Sample.ID") # Exclude selected rows


print(box11RemainingRows$Sample.ID)


seussCorrectedDataBox11Removed <- seussCorrectedData %>%
  anti_join(box11RemainingRows, by = "Sample.ID")


## Check before going on with the analysis
seussCorrectedDataBox11Removed %>%
  filter(Box.Name == "Batch test - invertebrates") %>% 
  pull(Sample.ID) %>%      # Extract the SampleID column
  unique()   


seussCorrectedData <- seussCorrectedDataBox11Removed

seussCorrectedDataBackup <- seussCorrectedData


#write_csv(seussCorrectedData, "Stable_Isotopes_Results_Full_List_Removed_Duplicated_OtherLocalities_July3rd2025.csv")

rm(box11RemainingRows)
rm(correctedArthropods)
rm(filteredBox11RandomSelection)
rm(seussCorrectedDataBox11Removed)
rm(seussCorrectedDataBackup)

# A few more subsets of data ----------------------------------------------

#  Specimens from each motu
specimensMeyers <- seussCorrectedData %>%
  filter(Locality.Main2 == "Meyer Islands")

specimensRangitāhua <- seussCorrectedData %>%
  filter(Locality.Main2 == "Rangitāhua")



# Filter for each period:

# Function to create all period-island dataset combinations
create_period_island_datasets <- function(data) {
  
  # Clean period names for dataset naming
  period_mapping <- c(
    "Period 1\n(1854-1920)" = "Period1", 
    "Period 2\n(1921-1983)" = "Period2",
    "Period 3\n(1984-2001)" = "Period3",
    "Period 4\n(2002-2023)" = "Period4"
  )
  
  # Clean island names for dataset naming
  island_mapping <- c(
    "Rangitāhua" = "Rangitāhua",
    "Meyer Islands" = "Meyers"
  )
  
  # Get unique periods and islands
  periods <- unique(data$Period)
  islands <- unique(data$Locality.Main2)
  
  # Initialize list to store all datasets
  datasets <- list()
  
  # Create datasets for each period (all localities combined)
  cat("Creating period datasets (all localities):\n")
  for (period in periods) {
    clean_period <- period_mapping[period]
    dataset_name <- clean_period
    
    datasets[[dataset_name]] <- data %>%
      filter(Period == period)
    
    cat(sprintf("  %s: %d rows\n", dataset_name, nrow(datasets[[dataset_name]])))
  }
  
  cat("\n")
  
  # Create datasets for each period-island combination
  cat("Creating period-island combination datasets:\n")
  for (period in periods) {
    for (island in islands) {
      clean_period <- period_mapping[period]
      clean_island <- island_mapping[island]
      dataset_name <- paste0(clean_period, clean_island)
      
      datasets[[dataset_name]] <- data %>%
        filter(Period == period, Locality.Main2 == island)
      
      cat(sprintf("  %s: %d rows\n", dataset_name, nrow(datasets[[dataset_name]])))
    }
  }
  
  cat("\n")
  
  # Summary
  cat("SUMMARY:\n")
  cat("========\n")
  cat(sprintf("Total datasets created: %d\n", length(datasets)))
  cat(sprintf("Period-only datasets: %d\n", length(periods)))
  cat(sprintf("Period-island datasets: %d\n", length(periods) * length(islands)))
  
  cat("\nDataset names:\n")
  for (name in names(datasets)) {
    cat(sprintf("  %s\n", name))
  }
  
  return(datasets)
}

# Function to assign datasets to global environment
#assign_datasets_globally <- function(dataset_list) {
#  cat("\nAssigning datasets to global environment:\n")
#  for (name in names(dataset_list)) {
#    assign(name, dataset_list[[name]], envir = .GlobalEnv)
#    cat(sprintf("  %s assigned\n", name))
#  }
#  cat("All datasets are now available in the workspace!\n")
#}

# Function to get dataset summary
#get_datasets_summary <- function(dataset_list) {
#  summary_df <- data.frame(
#    Dataset = names(dataset_list),
#    Rows = sapply(dataset_list, nrow),
#    Periods = sapply(dataset_list, function(x) length(unique(x$Period))),
#    Islands = sapply(dataset_list, function(x) length(unique(x$Locality.Main2))),
#    stringsAsFactors = FALSE
#  )

# return(summary_df)
#}

# Create all datasets
#all_datasets <- create_period_island_datasets(seussCorrectedData)

# Assign them to global environment so we can use them directly
#assign_datasets_globally(all_datasets)

# Get summary table
#summary_table <- get_datasets_summary(all_datasets)

#print(summary_table)

#  Groups subsamples:

#birds <- seussCorrectedData[seussCorrectedData$CommonGroupName == "Bird",] %>%
#  filter(!(Species == "Prosthemadera novaeseelandiae" & percentageNitrogen < 10))

#birdsForest <- birds[birds$Sample.Type == "Animal - feather",] %>%
# filter(!(Species == "Cyanoramphus novaezelandiae" & percentageNitrogen < 10))

#plants <- seussCorrectedData[seussCorrectedData$CommonGroupName == "Plant",]



# Are kōtare values different across time? --------------------------------

kōtare <- seussCorrectedData %>%
  filter(Species == "Todiramphus sanctus")

colnames(kōtare)
table(kōtare$Period)
table(kōtare$CollectionYear)

ggbetweenstats(kōtare, x=CollectionYear, y = d15N,
               pairwise.display = "significant")
ggbetweenstats(kōtare, x = Period, y = d15N,
               pairwise.display = "significant")

ggbetweenstats(kōtare, x=CollectionYear, y = d13CSeussCorrected,
               pairwise.display = "significant")
ggbetweenstats(kōtare, x = Period, y = d13CSeussCorrected,
               pairwise.display = "significant")


model_d15N_year <- lm(d15N ~ CollectionYear, data = kōtare)
summary(model_d15N_year)

model_d13C_year <- lm(d13CSeussCorrected ~ CollectionYear, data = kōtare)
summary(model_d13C_year)


model_d15N_period <- lm(d15N ~ Period, data = kōtare)
summary(model_d15N_year)

model_d13C_period <- lm(d13CSeussCorrected ~ Period, data = kōtare)
summary(model_d13C_year)

## So, use kōtare from period 2 onwards!

rm(model_d15N_year)
rm(model_d15N_period)
rm(model_d13C_year)
rm(model_d13C_period)



# Are land crabs values changing over time? ------------------------------

landCrabs <- seussCorrectedData %>%
  filter(Species == "Geograpsus grayi")

colnames(landCrabs)
table(landCrabs$Period)
table(landCrabs$CollectionYear)

ggbetweenstats(landCrabs, x=CollectionYear, y = d15N,
               pairwise.display = "significant")
ggbetweenstats(landCrabs, x = Period, y = d15N,
               pairwise.display = "significant")

ggbetweenstats(landCrabs, x=CollectionYear, y = d13CSeussCorrected,
               pairwise.display = "significant")
ggbetweenstats(landCrabs, x = Period, y = d13CSeussCorrected,
               pairwise.display = "significant")


model_d15N_year <- lm(d15N ~ CollectionYear, data = landCrabs)
summary(model_d15N_year)

model_d13C_year <- lm(d13CSeussCorrected ~ CollectionYear, data = landCrabs)
summary(model_d13C_year)


model_d15N_period <- lm(d15N ~ Period, data = landCrabs)
summary(model_d15N_year)

model_d13C_period <- lm(d13CSeussCorrected ~ Period, data = landCrabs)
summary(model_d13C_year)

# No, cannot use land crabs' older data as their d13C has changed over time.

rm(model_d15N_year)
rm(model_d15N_period)
rm(model_d13C_year)
rm(model_d13C_period)
rm(landCrabs)


# Create the datasets for the tRophic position analysis - Rangitāhua ----------------------

# Add rats from period 2 and all kōtare 
rats <- specimensRangitāhua[specimensRangitāhua$CommonGroupName == "Rat", ]
unique(rats$Period)
#kōtarePeriod2 <- Period2Rangitāhua[Period2Rangitāhua$Species == "Todiramphus sanctus",]

combinedData <- rbind(
  rats, kōtare #, kōtarePeriod2
)
table(combinedData$Species)
table(combinedData$Period)
table(combinedData$Species, combinedData$CollectionYear)

unique(specimensRangitāhua$Period)
Period4Rangitāhua <- specimensRangitāhua %>%
  filter(Period == "Period 4\n(2002-2023)")
unique(Period4Rangitāhua$Species)
table(Period4Rangitāhua$Species)

selectedPeriod4Rangitāhua <- rbind(combinedData, Period4Rangitāhua)
selectedPeriod4Rangitāhua <- selectedPeriod4Rangitāhua %>%
  distinct()


## Keep selected columns only

selectedPeriod4Rangitāhua <- selectedPeriod4Rangitāhua %>%
  dplyr::select(IdentifierEditedMK, Species,Terres.Marine, Group, GuildCapitalized, 
                CommonGroupName, Feeding.habit.Niche, Terres.Marine,
                CollectionDate, CollectionYear,  Locality.Main2, Locality.sub,
                SampleTypeDetails, d15N, d13CSeussCorrected, Period)


## Then, keep baseline species for the b1 and b2 categories

terrestrialPlantsR <- selectedPeriod4Rangitāhua %>% 
  filter(GuildCapitalized == "Primary producer") %>% 
  filter(Terres.Marine == "Terrestrial") %>%
  distinct(Species) %>%
  pull(Species)

macroalgaeR <- selectedPeriod4Rangitāhua %>%
  filter(CommonGroupName %in% c("Macroalga - brown",
                                "Macroalga - green",
                                "Macroalga - red")) %>%
  distinct(Species) %>%
  pull(Species)

terrestrialAbundantR <- selectedPeriod4Rangitāhua %>%
  filter(Terres.Marine == "Terrestrial") %>%
  count(Species) %>%
  filter(n >= 5) %>%
  pull(Species)

marineAbundantR <- selectedPeriod4Rangitāhua %>%
  filter(Terres.Marine == "Marine") %>%
  count(Species) %>%
  filter(n >= 5) %>%
  pull(Species)

keepSpeciesRT <- unique(c(terrestrialPlantsR, terrestrialAbundantR))
keepSpeciesRM <- unique(c(macroalgaeR, marineAbundantR))

## Now, keep them:

#Rangitāhua terrestrial
selectedPeriod4RangitāhuaT <- selectedPeriod4Rangitāhua %>%
  filter(Species %in% keepSpeciesRT)

table(selectedPeriod4RangitāhuaT$Species)


#Rangitāhua marine
## Now, keep them:

selectedPeriod4RangitāhuaM <- selectedPeriod4Rangitāhua %>%
  filter(Species %in% keepSpeciesRM)

table(selectedPeriod4RangitāhuaM$Species)

# Create the datasets for the tRophic position analysis - Meyers ----------------------


Period4Meyers <- specimensMeyers %>%
  filter(Period == "Period 4\n(2002-2023)")
unique(Period4Meyers$Species)
table(Period4Meyers$Species)

## Keep selected columns only

selectedPeriod4Meyers <- Period4Meyers %>%
  dplyr::select(IdentifierEditedMK, Species,Terres.Marine, Group, GuildCapitalized, 
                CommonGroupName, Feeding.habit.Niche, Terres.Marine,
                CollectionDate, CollectionYear,  Locality.Main2, Locality.sub,
                SampleTypeDetails, d15N, d13CSeussCorrected, Period)


## Then, keep baseline species for the b1 and b2 categories

terrestrialPlantsM <- selectedPeriod4Meyers %>% 
  filter(GuildCapitalized == "Primary producer") %>% 
  filter(Terres.Marine == "Terrestrial") %>%
  distinct(Species) %>%
  pull(Species)

macroalgaeM <- selectedPeriod4Meyers %>%
  filter(CommonGroupName %in% c("Macroalga - brown",
                                "Macroalga - green",
                                "Macroalga - red")) %>%
  distinct(Species) %>%
  pull(Species)

terrestrialAbundantM <- selectedPeriod4Meyers %>%
  filter(Terres.Marine == "Terrestrial") %>%
  count(Species) %>%
  filter(n >= 5) %>%
  pull(Species)

marineAbundantM <- selectedPeriod4Meyers %>%
  filter(Terres.Marine == "Marine") %>%
  count(Species) %>%
  filter(n >= 5) %>%
  pull(Species)

keepSpeciesMT <- unique(c(terrestrialPlantsM, terrestrialAbundantM))
keepSpeciesMM <- unique(c(macroalgaeM, marineAbundantM))

## Now, keep them:

#Meyers terrestrial
selectedPeriod4MeyersT <- selectedPeriod4Meyers %>%
  filter(Species %in% keepSpeciesMT)

table(selectedPeriod4MeyersT$Species)


#Meyers marine
## Now, keep them:

selectedPeriod4MeyersM <- selectedPeriod4Meyers %>%
  filter(Species %in% keepSpeciesMM)

table(selectedPeriod4MeyersM$Species)


