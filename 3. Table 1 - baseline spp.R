# Table 1 for the paper with all baseline spp used ---------------------------------

# combine both islands, tagging each with its locality


baselineData <- bind_rows(
  Period4Rangitāhua %>% mutate(island = "Rangitāhua"),
  Period4Meyers %>% mutate(island = "Meyer Islands")
) %>%
  filter(GuildCapitalized == "Primary producer")  

unique(baselineData$Species)
unique(terrestrialPlantsM)
unique(terrestrialPlantsR)
unique(macroalgaeM)
unique(macroalgaeR)

Period4Rangitāhua %>% 
  filter(GuildCapitalized == "Primary producer") %>%
  pull(unique(Species))

specimensRangitāhua %>%
  filter(GuildCapitalized == "Primary producer") %>%
  filter(Period == "Period 4\n(2002-2023)") %>%
  pull(Species)

specimensMeyers %>%
  filter(GuildCapitalized == "Primary producer") %>%
  filter(Period == "Period 4\n(2002-2023)") %>%
  pull(Species)

# one row per island x terres/marine x species
speciesSummary <- baselineData %>%
  group_by(island, Terres.Marine, Species) %>%
  summarise(
    sampleSize   = n(),
    meanD15N     = mean(d15N, na.rm = TRUE),
    sdD15N       = sd(d15N, na.rm = TRUE),
    seD15N       = sdD15N / sqrt(sampleSize),
    ciLowerD15N  = meanD15N - qt(0.975, df = sampleSize - 1) * seD15N,
    ciUpperD15N  = meanD15N + qt(0.975, df = sampleSize - 1) * seD15N,
    collectionYears = paste(sort(unique(CollectionYear)), collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(
    d15nCi = case_when(
      sampleSize > 1 ~ sprintf("%.2f ± %.2f", meanD15N, ciUpperD15N - meanD15N),
      TRUE ~ sprintf("%.2f (n=1)", meanD15N)
    )
  ) %>%
  arrange(island, Terres.Marine, desc(sampleSize)) %>%
  dplyr::select(island, Terres.Marine, Species, sampleSize, d15nCi, collectionYears)

print(speciesSummary, n = 28)


speciesLong <- speciesSummary %>%
  mutate(
    islandLabel  = if_else(island == "Rangitāhua", "R", "M"),
    islandN      = paste0(islandLabel, " (n = ", sampleSize, ")"),
    islandOrder  = if_else(island == "Rangitāhua", 1L, 2L),
    groupOrder   = if_else(Terres.Marine == "Terrestrial", 1L, 2L)
  ) %>%
  arrange(groupOrder, Species, islandOrder) %>%
  group_by(Species) %>%
  mutate(speciesLabel = if_else(row_number() == 1, Species, "")) %>%
  ungroup() %>%
  dplyr::select(speciesLabel, islandN, collectionYears, d15nCi)

speciesLong %>%
  kable(
    col.names = c("Species", "Island (sample size)",
                  "Collection years", "δ15N (95% CI)"),
    align = "llll"
  )

write.table(speciesLong,
            file = "table1.csv",
            sep = "\t", quote = FALSE, row.names = FALSE,
            col.names = c("Species", "Island (sample size)",
                          "Collection years", "δ15N (95% CI)"))







## New table, with d13C too


baselineData <- bind_rows(
  Period4Rangitāhua %>% mutate(island = "Rangitāhua"),
  Period4Meyers %>% mutate(island = "Meyer Islands")
) %>%
  filter(GuildCapitalized == "Primary producer")

# formats mean ± 95% CI half-width (t-based); reports n=1 cases without a CI
formatMeanCi <- function(x, digits = 2) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0) return(NA_character_)
  if (n == 1) return(sprintf(paste0("%.", digits, "f (n=1)"), x))
  halfWidth <- qt(0.975, df = n - 1) * sd(x) / sqrt(n)
  sprintf(paste0("%.", digits, "f ± %.", digits, "f"), mean(x), halfWidth)
}

# one row per island x terrestrial/marine x species
speciesSummary <- baselineData %>%
  group_by(island, Terres.Marine, Species) %>%
  summarise(
    sampleSize      = n(),
    d15nCi          = formatMeanCi(d15N),
    d13cCi          = formatMeanCi(d13CSeussCorrected),
    collectionYears = paste(sort(unique(CollectionYear)), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(island, Terres.Marine, desc(sampleSize))

print(speciesSummary, n = Inf)

speciesLong <- speciesSummary %>%
  mutate(
    islandLabel = if_else(island == "Rangitāhua", "R", "M"),
    islandN     = paste0(islandLabel, " (n = ", sampleSize, ")"),
    islandOrder = if_else(island == "Rangitāhua", 1L, 2L),
    groupOrder  = if_else(Terres.Marine == "Terrestrial", 1L, 2L)
  ) %>%
  arrange(groupOrder, Species, islandOrder) %>%
  group_by(Species) %>%
  mutate(speciesLabel = if_else(row_number() == 1, Species, "")) %>%
  ungroup() %>%
  dplyr::select(speciesLabel, islandN, collectionYears, d15nCi, d13cCi)

table1ColNames <- c("Species", "Island (sample size)", "Collection years",
                    "δ15N (mean ± 95% CI)", "δ13C (mean ± 95% CI)")

speciesLong %>%
  kable(col.names = table1ColNames, align = "lllll")

write.table(speciesLong,
            file = "table1_withd13C.csv",
            sep = "\t", quote = FALSE, row.names = FALSE,
            col.names = table1ColNames)
