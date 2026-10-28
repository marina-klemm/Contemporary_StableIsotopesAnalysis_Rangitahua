# What's the d13C and d15N of algae for each island --------

macroalgaeRangitāhua <- Period4Rangitāhua %>%
  filter(CommonGroupName %in% c("Macroalga - red", "Macroalga - green", "Macroalga | brown"))
mean(macroalgaeRangitāhua$d15N)
sd(macroalgaeRangitāhua$d15N)
mean(macroalgaeRangitāhua$d13CSeussCorrected)
sd(macroalgaeRangitāhua$d13CSeussCorrected)

macroalgaeMeyers <- Period4Meyers %>%
  filter(CommonGroupName %in% c("Macroalga - red", "Macroalga - green", "Macroalga | brown"))
mean(macroalgaeMeyers$d15N)
sd(macroalgaeMeyers$d15N)
mean(macroalgaeMeyers$d13CSeussCorrected)
sd(macroalgaeMeyers$d13CSeussCorrected)


# Compare baseline spp with linear models ---------------

colnames(marineCombinedn5Filtered)

algaeCombined <- marineCombinedn5Filtered %>%
  filter(GuildCapitalized == "Primary producer")

algaeModeld15NLocality <- lm(d15N ~ Locality.main, data = algaeCombined)
emmeans(algaeModeld15NLocality, pairwise ~ Locality.main)

algaeModeld13CLocality <- lm(d13CSeussCorrected ~ Locality.main, data = algaeCombined)
emmeans(algaeModeld13CLocality, pairwise ~ Locality.main)


algaeModeld15N <- lm(d15N ~ CommonGroupName * Locality.main, data = algaeCombined)
emmeans(algaeModeld15N, pairwise ~ Locality.main | CommonGroupName)

algaeModeld13C <- lm(d13CSeussCorrected ~ CommonGroupName * Locality.main, data = algaeCombined)
emmeans(algaeModeld13C, pairwise ~ Locality.main | CommonGroupName)





## and then also compare terrestrial plants in a group


terrestrialPlantsMeyers <- Period4Meyers %>%
  filter(CommonGroupName == "Plant")

table(terrestrialPlantsMeyers$Species)


terrestrialPlantsRangitāhua <- Period4Rangitāhua %>%
  filter(CommonGroupName == "Plant")
table(terrestrialPlantsRangitāhua$Species)





terrestrialCombined <- rbind(terrestrialPlantsRangitāhua, terrestrialPlantsMeyers)

# Report the overall d13C and d15N differences first
terrestrialmodeld15NLocality <- lm(d15N ~ Locality.main, data = terrestrialCombined)
emmeans(terrestrialmodeld15NLocality, pairwise ~ Locality.main)

terrestrialmodeld13CLocality <- lm(d13CSeussCorrected ~ Locality.main, data = terrestrialCombined)
emmeans(terrestrialmodeld13CLocality, pairwise ~ Locality.main)

#And then report for individual species differences, filter only the ones present in both islands
commonSpp <- terrestrialCombined %>%
  group_by(Species) %>%
  summarise(n_islands = n_distinct(Locality.main)) %>%
  filter(n_islands == 2) %>%
  pull(Species)

terrestrialCombinedFiltered <- terrestrialCombined %>%
  filter(Species %in% commonSpp)

terrestrialmodeld15N <- lm(d15N ~ Species * Locality.main, data = terrestrialCombinedFiltered)
emmeans(terrestrialmodeld15N, pairwise ~ Locality.main | Species)

terrestrialmodeld13C <- lm(d13CSeussCorrected ~ Species * Locality.main, data = terrestrialCombinedFiltered)
emmeans(terrestrialmodeld13C, pairwise ~ Locality.main | Species)


### Or, make a single dataset with all the baselines

baselinesCombined <- Period4 %>%
  filter(GuildCapitalized == "Primary producer")

table(baselinesCombined$Species, baselinesCombined$Locality.main)

baselinesCombinedModeld15N <- lm(d15N ~ CommonGroupName * Locality.main, data = baselinesCombined)
emmeans(baselinesCombinedModeld15N, pairwise ~ Locality.main | CommonGroupName)

baselinesCombinedModeld13C <- lm(d13CSeussCorrected ~ CommonGroupName * Locality.main, data = baselinesCombined)
emmeans(baselinesCombinedModeld13C, pairwise ~ Locality.main | CommonGroupName)

baselinesCombinedModelSpeciesd15N <- lm(d15N ~ Species * Locality.main, data = baselinesCombined)
emmeans(baselinesCombinedModelSpeciesd15N, pairwise ~ Locality.main | Species)

baselinesCombinedModelSpeciesd13C <- lm(d13CSeussCorrected ~ Species * Locality.main, data = baselinesCombined)
emmeans(baselinesCombinedModelSpeciesd13C, pairwise ~ Locality.main | Species)

#


# Compare baseline spp with linear models ---------------

colnames(marineCombinedn5Filtered)

algaeCombined <- marineCombinedn5Filtered %>%
  filter(GuildCapitalized == "Primary producer")

algaeModeld15NLocality <- lm(d15N ~ Locality.main, data = algaeCombined)
emmeans(algaeModeld15NLocality, pairwise ~ Locality.main)

algaeModeld13CLocality <- lm(d13CSeussCorrected ~ Locality.main, data = algaeCombined)
emmeans(algaeModeld13CLocality, pairwise ~ Locality.main)


algaeModeld15N <- lm(d15N ~ CommonGroupName * Locality.main, data = algaeCombined)
emmeans(algaeModeld15N, pairwise ~ Locality.main | CommonGroupName)

algaeModeld13C <- lm(d13CSeussCorrected ~ CommonGroupName * Locality.main, data = algaeCombined)
emmeans(algaeModeld13C, pairwise ~ Locality.main | CommonGroupName)





## and then also compare terrestrial plants in a group


terrestrialPlantsMeyers <- Period4Meyers %>%
  filter(CommonGroupName == "Plant")

table(terrestrialPlantsMeyers$Species)


terrestrialPlantsRangitāhua <- Period4Rangitāhua %>%
  filter(CommonGroupName == "Plant")
table(terrestrialPlantsRangitāhua$Species)





terrestrialCombined <- rbind(terrestrialPlantsRangitāhua, terrestrialPlantsMeyers)

# Report the overall d13C and d15N differences first
terrestrialmodeld15NLocality <- lm(d15N ~ Locality.main, data = terrestrialCombined)
emmeans(terrestrialmodeld15NLocality, pairwise ~ Locality.main)

terrestrialmodeld13CLocality <- lm(d13CSeussCorrected ~ Locality.main, data = terrestrialCombined)
emmeans(terrestrialmodeld13CLocality, pairwise ~ Locality.main)

#And then report for individual species differences, filter only the ones present in both islands
commonSpp <- terrestrialCombined %>%
  group_by(Species) %>%
  summarise(n_islands = n_distinct(Locality.main)) %>%
  filter(n_islands == 2) %>%
  pull(Species)

terrestrialCombinedFiltered <- terrestrialCombined %>%
  filter(Species %in% commonSpp)

terrestrialmodeld15N <- lm(d15N ~ Species * Locality.main, data = terrestrialCombinedFiltered)
emmeans(terrestrialmodeld15N, pairwise ~ Locality.main | Species)

terrestrialmodeld13C <- lm(d13CSeussCorrected ~ Species * Locality.main, data = terrestrialCombinedFiltered)
emmeans(terrestrialmodeld13C, pairwise ~ Locality.main | Species)


### Or, make a single dataset with all the baselines

baselinesCombined <- Period4 %>%
  filter(GuildCapitalized == "Primary producer")

table(baselinesCombined$Species, baselinesCombined$Locality.main)

baselinesCombinedModeld15N <- lm(d15N ~ CommonGroupName * Locality.main, data = baselinesCombined)
emmeans(baselinesCombinedModeld15N, pairwise ~ Locality.main | CommonGroupName)

baselinesCombinedModeld13C <- lm(d13CSeussCorrected ~ CommonGroupName * Locality.main, data = baselinesCombined)
emmeans(baselinesCombinedModeld13C, pairwise ~ Locality.main | CommonGroupName)

baselinesCombinedModelSpeciesd15N <- lm(d15N ~ Species * Locality.main, data = baselinesCombined)
emmeans(baselinesCombinedModelSpeciesd15N, pairwise ~ Locality.main | Species)

baselinesCombinedModelSpeciesd13C <- lm(d13CSeussCorrected ~ Species * Locality.main, data = baselinesCombined)
emmeans(baselinesCombinedModelSpeciesd13C, pairwise ~ Locality.main | Species)

#

# Or, actually: compare TPs and not raw d13C and d15N ---------------------

posteriorModeRangitāhuaMarineAllAlgaeN5 <- getPosteriorMode(selectedPeriod4RangitāhuaM_models$"TPs")
sapply(selectedPeriod4RangitāhuaM_models$"TPs", quantile, probs = c(0.025, 0.5, 0.975))

posteriorModeMeyersMarineAllAlgaeN5 <- getPosteriorMode(selectedPeriod4Meyers_models$"TPs")
sapply(selectedPeriod4Meyers_models$"TPs", quantile, probs = c(0.025, 0.5, 0.975))


# Species present at both islands
shared_species <- c("Chrysiptera rapanui", "Pseudolabrus luculentus",
                    "Modiolus auriculatus", "Scutellastra kermadecensis",
                    "Tectus royanus", "Scorpaena cardinalis", "Acanthaster planci")

# Fix names properly - remove .1b AND convert dots back to spaces
clean_tp_names <- function(model) {
  nms <- names(model$TPs)
  nms <- gsub("\\.1b$", "", nms)        # remove .1b suffix
  nms <- gsub("\\.", " ", nms)           # convert dots to spaces
  names(model$TPs) <- nms
  return(model)
}

selectedPeriod4RangitāhuaM_models <- clean_tp_names(selectedPeriod4RangitāhuaM_models)
selectedPeriod4Meyers_models <- clean_tp_names(selectedPeriod4Meyers_models)

# Check exactly what names look like after cleaning
print(names(selectedPeriod4RangitāhuaM_models$TPs))
print(names(selectedPeriod4Meyers_models$TPs))

# Check which shared_species are actually found
for (sp in shared_species) {
  r_found <- sp %in% names(selectedPeriod4RangitāhuaM_models$TPs)
  m_found <- sp %in% names(selectedPeriod4Meyers_models$TPs)
  cat(sp, "| Rangitāhua:", r_found, "| Meyers:", m_found, "\n")
}

# Rebuild results dataframe
tp_comparison <- data.frame(
  Species = shared_species,
  p_Meyers_higher = NA_real_,
  Rangitāhua_mode = NA_real_,
  Meyers_mode = NA_real_
)

for (sp in shared_species) {
  r_draws <- selectedPeriod4RangitāhuaM_models$TPs[[sp]]
  m_draws <- selectedPeriod4Meyers_models$TPs[[sp]]
  
  if (is.null(r_draws) || is.null(m_draws)) {
    message(paste("Species not found in one or both models:", sp))
    next
  }
  
  n <- min(length(r_draws), length(m_draws))
  
  tp_comparison$p_Meyers_higher[tp_comparison$Species == sp] <-
    sum(m_draws[1:n] > r_draws[1:n]) / n
  
  # Use [[1]] instead of [[sp]] to avoid spaces issue
  tp_comparison$Rangitāhua_mode[tp_comparison$Species == sp] <-
    getPosteriorMode(setNames(list(r_draws), sp))[[1]]
  
  tp_comparison$Meyers_mode[tp_comparison$Species == sp] <-
    getPosteriorMode(setNames(list(m_draws), sp))[[1]]
}

print(tp_comparison)




# Table for Pr of compared TPs and alpha values ---------------------------
# 1. Inputs 

analysisList <- list(
  "Rangitāhua terrestrial (two baselines)" = selectedPeriod4RangitāhuaT_models,
  "Rangitāhua marine (one baseline)"       = selectedPeriod4RangitāhuaM_models,
  "Meyer Islands marine (one baseline)"    = selectedPeriod4Meyers_models
)

sharedSpecies <- c("Chrysiptera rapanui", "Pseudolabrus luculentus",
                   "Modiolus auriculatus", "Scutellastra kermadecensis",
                   "Tectus royanus", "Scorpaena cardinalis", "Acanthaster planci")

supportThreshold <- 0.95   # posterior probability used to flag a "supported" direction


# 2. Helper functions

# Remove ".1b" suffix, turn dots into spaces (does not modify the original models)
cleanPosteriorNames <- function(draws) {
  names(draws) <- names(draws) %>%
    gsub("\\.1b$", "", .) %>%
    gsub("\\.", " ", .) %>%
    trimws()
  lapply(draws, as.numeric)
}

# Mode, median and 95% CI per consumer, ordered from highest to lowest median
summarisePosterior <- function(draws) {
  tibble(
    consumer = names(draws),
    mode     = vapply(draws, function(x) as.numeric(getPosteriorMode(list(x))[[1]]), numeric(1)),
    median   = vapply(draws, median, numeric(1)),
    ciLower  = vapply(draws, function(x) unname(quantile(x, 0.025)), numeric(1)),
    ciUpper  = vapply(draws, function(x) unname(quantile(x, 0.975)), numeric(1))
  ) %>%
    arrange(desc(median))
}

# All pairwise comparisons: pAGreater = P(A > B) from paired posterior draws
pairwisePosterior <- function(draws, threshold = supportThreshold) {
  pairList <- utils::combn(names(draws), 2, simplify = FALSE)
  
  lapply(pairList, function(p) {
    a <- draws[[p[1]]]
    b <- draws[[p[2]]]
    n <- min(length(a), length(b))
    d <- a[1:n] - b[1:n]
    tibble(
      speciesA   = p[1],
      speciesB   = p[2],
      pAGreater  = mean(d > 0),
      medianDiff = median(d),
      ciLower    = unname(quantile(d, 0.025)),
      ciUpper    = unname(quantile(d, 0.975))
    )
  }) %>%
    bind_rows() %>%
    mutate(direction = case_when(
      pAGreater >= threshold     ~ "A > B",
      pAGreater <= 1 - threshold ~ "B > A",
      TRUE                       ~ "unresolved"
    ))
}

# One report section (summary + pairwise table) for a set of draws
buildSection <- function(rawDraws, heading) {
  draws   <- cleanPosteriorNames(rawDraws)
  summ    <- summarisePosterior(draws)
  pairs   <- pairwisePosterior(draws[summ$consumer])   # A is the higher-median consumer
  
  c(paste0("### ", heading), "",
    "**Posterior summary**", "",
    kable(summ, format = "pipe", digits = 3), "",
    "**Pairwise comparisons** (A > B probabilities)", "",
    kable(pairs, format = "pipe", digits = 3), "")
}


# 3. Between-island comparison of shared species 

compareIslands <- function(rangitahuaModel, meyersModel, species) {
  rDraws <- cleanPosteriorNames(rangitahuaModel$TPs)
  mDraws <- cleanPosteriorNames(meyersModel$TPs)
  
  lapply(species, function(sp) {
    if (!(sp %in% names(rDraws)) || !(sp %in% names(mDraws))) {
      warning("Species not found in one or both models: ", sp)
      return(NULL)
    }
    r <- rDraws[[sp]]
    m <- mDraws[[sp]]
    n <- min(length(r), length(m))
    d <- m[1:n] - r[1:n]                      # Meyers minus Rangitāhua
    
    tibble(
      species          = sp,
      modeRangitahua   = as.numeric(getPosteriorMode(list(r))[[1]]),
      modeMeyers       = as.numeric(getPosteriorMode(list(m))[[1]]),
      medianRangitahua = median(r),
      medianMeyers     = median(m),
      medianDiff       = median(d),
      ciLower          = unname(quantile(d, 0.025)),
      ciUpper          = unname(quantile(d, 0.975)),
      pMeyersHigher    = mean(d > 0)
    )
  }) %>%
    bind_rows()
}

islandComparison <- compareIslands(
  selectedPeriod4RangitāhuaM_models,
  selectedPeriod4Meyers_models,
  sharedSpecies
)
print(islandComparison)


# 4. Assemble the report 

reportLines <- c(
  "# Trophic position (TP) and alpha comparisons", "",
  paste0("Pairwise probabilities are posterior probabilities that A exceeds B ",
         "(not p-values). Direction is flagged when the probability is ≥ ",
         supportThreshold, " or ≤ ", 1 - supportThreshold, ". ",
         "Within each table, A is the consumer with the higher median."), ""
)

for (analysisName in names(analysisList)) {
  model <- analysisList[[analysisName]]
  
  reportLines <- c(reportLines,
                   paste0("## ", analysisName), "",
                   buildSection(model$TPs, "Trophic position (TP)"))
  
  # Alphas only exist for the two-baseline model
  if (length(model$Alphas) > 0) {
    reportLines <- c(reportLines, buildSection(model$Alphas, "Alpha (proportion of baseline 1)"))
  } else {
    reportLines <- c(reportLines, "*No alpha estimates (one-baseline model).*", "")
  }
}

reportLines <- c(
  reportLines,
  "## Shared species: Meyer Islands vs Rangitāhua (marine, TP)", "",
  "Differences are Meyers minus Rangitāhua; pMeyersHigher = P(TP Meyers > TP Rangitāhua).", "",
  kable(islandComparison, format = "pipe", digits = 3), ""
)


# 5. Write out 

con <- file("tpAlphaComparisons.md", encoding = "UTF-8")
writeLines(reportLines, con)
close(con)

# Optional Word version (needs pandoc, which RStudio bundles)
rmarkdown::pandoc_convert("tpAlphaComparisons.md", to = "docx",
                          output = "tpAlphaComparisons.docx")
