# SIBER: Load packages -----------------------------------------------------------

library(viridis)
palette(viridis(3))
library(dplyr)
library(SIBER)
library(MASS)


# SIBER: Load datasets (from forMethodsPaper.R) ----------------------------------

## Use the same datasets as the beautiful figure from "forMethodsPaper.R" script:

table(Period4Meyers$Species)
table(Period4Meyers$CommonGroupName)
table(Period4Meyers$Period)
table(Period4Meyers$CollectionYear)
table(Period4Meyers$Locality.main)

Period4RangitāhuaFinal <- selectedPeriod4Rangitāhua

table(Period4RangitāhuaFinal$Species)
table(Period4RangitāhuaFinal$CommonGroupName)
table(Period4RangitāhuaFinal$Period)
table(Period4RangitāhuaFinal$CollectionYear)
table(Period4RangitāhuaFinal$Locality.main)


# SIBER: Process the data: Rangitāhua -----------------------------------------------


## Rangitāhua Island, ALL DATA

# 1. Prepare data for SIBER
prepareDataForSiber <- function(data) {
  siberDataRangitāhua <- data %>%
    dplyr::select(Locality.Main2, Terres.Marine, d13CSeussCorrected, d15N) %>%
    filter(!is.na(d13CSeussCorrected) & !is.na(d15N)) %>%
    rename(
      group = Terres.Marine,      # Marine/Terrestrial as groups (for colors)
      community = Locality.Main2,  # Rangitāhua/Meyer as communities
      iso1 = d13CSeussCorrected,
      iso2 = d15N
    ) %>%
    mutate(
      group = factor(group),
      community = factor(community)
    ) %>%
    dplyr::select(iso1, iso2, group, community)
  
  return(as.data.frame(siberDataRangitāhua))
}

# 2. Process the data
siberDataRangitāhua <- prepareDataForSiber(Period4RangitāhuaFinal)
# Relevel so Terrestrial = 1st group, Marine = 2nd
siberDataRangitāhua <- siberDataRangitāhua %>%
  mutate(group = factor(group, levels = c("Terrestrial", "Marine")))

# 3. Create SIBER object
siberExampleRangitāhua <- createSiberObject(siberDataRangitāhua)

# 4. Calculate ellipses using maximum likelihood
ellipses <- lapply(split(siberDataRangitāhua, siberDataRangitāhua$group), function(group_data) {
  cov.trob(cbind(group_data$iso1, group_data$iso2))
})

# 5. Set up colors from viridis
n_colors <- 10
viridis_full <- viridis(n_colors)
# Select blue for Marine and green for Terrestrial
selected_colors <- viridis_full[c(7, 3)]  # Index 3 for blue, 7 for green
palette(selected_colors)

# Create the plot with basic elements first
par(mfrow = c(1,1), mar = c(5, 5, 4, 2))

# Basic plot without ellipses
plotSiberObject(siberExampleRangitāhua,
                ax.pad = 2,
                hulls = TRUE,
                ellipses = TRUE,  # We'll add these manually
                group.hulls = TRUE,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'(‰)'),
                ylab = expression({delta}^15*N~'(‰)'),
                points.order = c(16, 17),
                cex = 1.2,
                cex.lab = 1.3,
                cex.axis = 1.1
)


# Add ellipses for each community and group combination
# Get unique combinations
communities <- unique(siberDataRangitāhua$community)
groups <- unique(siberDataRangitāhua$group)



# Add two separate legends
# Communities legend (colors)
legend("topright",
       legend = levels(siberDataRangitāhua$group),
       title = "Communities",
       pch = 16,
       col = selected_colors,
       bty = "n",
       cex = 1.1)

title(
  main = "Rangitāhua Island, all data",
  line = 2,  # Adjusts the distance of the title from the plot
  cex.main = 1.5  # Adjusts the font size of the title
)

selected_colors_ellipse <- viridis_full[c(3, 7)]

# Add ellipses with appropriate colors
for(c.id in 1:length(communities)) {
  for(g.id in 1:length(groups)) {
    if(!is.null(siberExampleRangitāhua$ML.mu[[c.id]]) && 
       !is.null(siberExampleRangitāhua$ML.cov[[c.id]]) &&
       !is.null(siberExampleRangitāhua$ML.mu[[c.id]][,,g.id]) && 
       !is.null(siberExampleRangitāhua$ML.cov[[c.id]][,,g.id])) {
      
      coords <- addEllipse(siberExampleRangitāhua$ML.mu[[c.id]][,,g.id],
                           siberExampleRangitāhua$ML.cov[[c.id]][,,g.id],
                           m = NULL,
                           n = 100,
                           p.interval = 0.95,
                           ci.mean = FALSE,
                           col = selected_colors_ellipse[g.id],  # Use our viridis colors
                           lty = 1,
                           lwd = 2)
    }
  }
}

# Groups legend (point types)
#legend("right",
#       legend = levels(siberDataRangitāhua$community),
#       title = "Locations",
#       pch = c(16, 17),
#       col = "black",
#       bty = "n",
#       cex = 1.1)

# Print summary statistics for verification
cat("\nEllipse Statistics:\n")
for(c.id in 1:length(communities)) {
  for(g.id in 1:length(groups)) {
    if(!is.null(siberExampleRangitāhua$ML.mu[[c.id]]) && !is.null(siberExampleRangitāhua$ML.mu[[c.id]][,,g.id])) {
      cat(sprintf("Community %s, Group %s:\n", communities[c.id], groups[g.id]))
      cat("Mean values:\n")
      print(siberExampleRangitāhua$ML.mu[[c.id]][,,g.id])
      cat("\n")
    }
  }
}


# Calculate the ML-based ellipse metrics for all groups/communities
group.ML <- groupMetricsML(siberExampleRangitāhua)

# group.ML returns a matrix with rows: TA, SEA, SEAc
# and columns named as "community.group"
print(group.ML)

# --- Basic barplot of SEAc (corrected SEA) ---
# SEAc is the 3rd row
barplot(group.ML["SEAc", ],
        xlab = "Group",
        ylab = expression("SEA"[c]~"(‰"^2*")"),
        main = "Standard Ellipse Area (corrected) — Rangitāhua Island",
        col = selected_colors,   # reuse the viridis colors
        las = 2,                 # rotate x-axis labels
        cex.names = 0.85)


# --- Bayesian ellipses ---
parms <- list()
parms$n.iter <- 2 * 10^4
parms$n.burnin <- 1 * 10^3
parms$n.thin <- 10
parms$n.chains <- 2

priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

# Fit the Bayesian model
ellipses.posterior <- siberMVN(siberExampleRangitāhua, parms, priors)

# Extract SEA posterior for each group
SEA.B <- siberEllipses(ellipses.posterior)

group.ML <- groupMetricsML(siberExampleRangitāhua)
plot_colors <- viridis_full[c(7, 3)]
# The column names come from the community.group structure
# Use siberDensityPlot for a violin/density style plot
siberDensityPlot(SEA.B,
                 xticklabels = colnames(group.ML),
                 xlab = "Group",
                 ylab = expression("SEA"[B]~"(‰"^2*")"),
                 main = "Bayesian SEA — Rangitāhua Island",
                 bty = "L",
                 las = 2,
                 ylim = c(20, 90),
                 col = plot_colors)

# Overlay the ML SEAc estimates as points for comparison
points(1:ncol(group.ML),
       group.ML["SEAc", ],
       pch = 24,          # filled triangle
       col = "black",
       bg = selected_colors,
       cex = 1.5)

# Add a legend to clarify
legend("topright",
       legend = c("SEAc (ML)"),
       pch = 24,
       pt.bg = "grey50",
       bty = "n")





### Trying to reorder SEA plots








# Assign names based on group.ML column order
colnames(SEA.B) <- colnames(group.ML)  
# Now SEA.B has: "Rangitāhua.Marine", "Rangitāhua.Terrestrial"

# Reorder so Terrestrial is first (left), Marine second (right)
SEA.B_reordered <- SEA.B[, c("Rangitāhua.Terrestrial", "Rangitāhua.Marine")]

# Convert to long format
sea_long <- as.data.frame(SEA.B_reordered) %>%
  pivot_longer(everything(), names_to = "Group", values_to = "SEA_B") %>%
  mutate(Group = factor(Group, levels = c("Rangitāhua.Terrestrial", "Rangitāhua.Marine")))

# SEAc ML points in matching order
seac_df <- data.frame(
  Group = factor(c("Rangitāhua.Terrestrial", "Rangitāhua.Marine"),
                 levels = c("Rangitāhua.Terrestrial", "Rangitāhua.Marine")),
  SEAc  = group.ML["SEAc", c("Rangitāhua.Terrestrial", "Rangitāhua.Marine")]
)

# Colors: Terrestrial = green, Marine = blue
terr_color   <- viridis_full[7]
marine_color <- viridis_full[3]

ggplot(sea_long, aes(x = Group, y = SEA_B, fill = Group)) +
  geom_violin(alpha = 0.7, color = NA) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.8, color = "white") +
  geom_point(data = seac_df, aes(x = Group, y = SEAc),
             shape = 24, size = 3, 
             fill  = c(terr_color, marine_color), 
             color = "black") +
  scale_fill_manual(values = c("Rangitāhua.Terrestrial" = terr_color,
                               "Rangitāhua.Marine"      = marine_color)) +
  scale_x_discrete(labels = c("Rangitāhua.Terrestrial" = "Terrestrial",
                              "Rangitāhua.Marine"      = "Marine")) +
  labs(
    title = "Bayesian SEA — Rangitāhua Island",
    x     = "Group",
    y     = expression("SEA"[B]~"(‰"^2*")")
  ) +
  ylim(20, 90) +
  theme_classic(base_size = 13) +
  theme(legend.position = "none")









# Assign column names and reorder
colnames(SEA.B) <- colnames(group.ML)
SEA.B_reordered <- SEA.B[, c("Rangitāhua.Terrestrial", "Rangitāhua.Marine")]
group.ML_reordered <- group.ML[, c("Rangitāhua.Terrestrial", "Rangitāhua.Marine")]

# Plot
siberDensityPlot(SEA.B_reordered,
                 xticklabels = c("Terrestrial", "Marine"),
                 xlab        = "Group",
                 ylab        = expression("SEA"[B]~"(‰"^2*")"),
                 main        = "Bayesian SEA — Rangitāhua Island",
                 bty         = "L",
                 las         = 2,
                 ylim        = c(20, 100),
                 col         = c(viridis_full[7], viridis_full[3]))  # green, blue

# Overlay SEAc ML points
points(1:2,
       group.ML_reordered["SEAc", ],
       pch = 24,
       col = "black",
       bg  = c(viridis_full[7], viridis_full[3]),
       cex = 1.5)

legend("topright",
       legend = "SEAc (ML)",
       pch    = 24,
       pt.bg  = "grey50",
       bty    = "n")



# SIBER: Process the data: Meyer Islands -----------------------------------------

## Meyers Islands, ALL DATA

# 1. Prepare data for SIBER
prepareDataForSiber <- function(data) {
  siberDataMeyers <- data %>%
    dplyr::select(Locality.main, Terres.Marine, d13CSeussCorrected, d15N) %>%
    filter(!is.na(d13CSeussCorrected) & !is.na(d15N)) %>%
    rename(
      group = Terres.Marine,      # Marine/Terrestrial as groups (for colors)
      community = Locality.main,  # Meyers/Meyer as communities
      iso1 = d13CSeussCorrected,
      iso2 = d15N
    ) %>%
    mutate(
      group = factor(group),
      community = factor(community)
    ) %>%
    dplyr::select(iso1, iso2, group, community)
  
  return(as.data.frame(siberDataMeyers))
}

# 2. Process the data
siberDataMeyers <- prepareDataForSiber(Period4Meyers)

# 3. Create SIBER object
siberExampleMeyers <- createSiberObject(siberDataMeyers)

# 4. Calculate ellipses using maximum likelihood
ellipses <- lapply(split(siberDataMeyers, siberDataMeyers$group), function(group_data) {
  cov.trob(cbind(group_data$iso1, group_data$iso2))
})

# 5. Set up colors from viridis
n_colors <- 10
viridis_full <- viridis(n_colors)
# Select blue for Marine and green for Terrestrial
selected_colors <- viridis_full[c(3, 7)]  # Index 3 for blue, 7 for green
palette(selected_colors)

# Create the plot with basic elements first
par(mfrow = c(1,1), mar = c(5, 5, 4, 2))

# Basic plot without ellipses
plotSiberObject(siberExampleMeyers,
                ax.pad = 2,
                hulls = TRUE,
                ellipses = TRUE,  # We'll add these manually
                group.hulls = TRUE,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'(‰)'),
                ylab = expression({delta}^15*N~'(‰)'),
                points.order = c(16, 17),
                cex = 1.2,
                cex.lab = 1.3,
                cex.axis = 1.1
)


# Add ellipses for each community and group combination
# Get unique combinations
communities <- unique(siberDataMeyers$community)
groups <- unique(siberDataMeyers$group)



# Add two separate legends
# Communities legend (colors)
legend("topright",
       legend = levels(siberDataMeyers$group),
       title = "Communities",
       pch = 16,
       col = selected_colors,
       bty = "n",
       cex = 1.1)

title(
  main = "Meyers Islands, all data",
  line = 2,  # Adjusts the distance of the title from the plot
  cex.main = 1.5  # Adjusts the font size of the title
)

selected_colors_ellipse <- viridis_full[c(3, 7)]

# Add ellipses with appropriate colors
for(c.id in 1:length(communities)) {
  for(g.id in 1:length(groups)) {
    if(!is.null(siberExampleMeyers$ML.mu[[c.id]]) && 
       !is.null(siberExampleMeyers$ML.cov[[c.id]]) &&
       !is.null(siberExampleMeyers$ML.mu[[c.id]][,,g.id]) && 
       !is.null(siberExampleMeyers$ML.cov[[c.id]][,,g.id])) {
      
      coords <- addEllipse(siberExampleMeyers$ML.mu[[c.id]][,,g.id],
                           siberExampleMeyers$ML.cov[[c.id]][,,g.id],
                           m = NULL,
                           n = 100,
                           p.interval = 0.95,
                           ci.mean = FALSE,
                           col = selected_colors_ellipse[g.id],  # Use our viridis colors
                           lty = 1,
                           lwd = 2)
    }
  }
}

# Groups legend (point types)
#legend("right",
#       legend = levels(siberDataMeyers$community),
#       title = "Locations",
#       pch = c(16, 17),
#       col = "black",
#       bty = "n",
#       cex = 1.1)

# Print summary statistics for verification
cat("\nEllipse Statistics:\n")
for(c.id in 1:length(communities)) {
  for(g.id in 1:length(groups)) {
    if(!is.null(siberExampleMeyers$ML.mu[[c.id]]) && !is.null(siberExampleMeyers$ML.mu[[c.id]][,,g.id])) {
      cat(sprintf("Community %s, Group %s:\n", communities[c.id], groups[g.id]))
      cat("Mean values:\n")
      print(siberExampleMeyers$ML.mu[[c.id]][,,g.id])
      cat("\n")
    }
  }
}


# Calculate the ML-based ellipse metrics for all groups/communities
group.ML <- groupMetricsML(siberExampleMeyers)

# group.ML returns a matrix with rows: TA, SEA, SEAc
# and columns named as "community.group"
print(group.ML)

# --- Basic barplot of SEAc (corrected SEA) ---
# SEAc is the 3rd row
barplot(group.ML["SEAc", ],
        xlab = "Group",
        ylab = expression("SEA"[c]~"(‰"^2*")"),
        main = "Standard Ellipse Area (corrected) — Meyers Islands",
        col = selected_colors,   # reuse the viridis colors
        las = 2,                 # rotate x-axis labels
        cex.names = 0.85)


# --- Bayesian ellipses ---
parms <- list()
parms$n.iter <- 2 * 10^4
parms$n.burnin <- 1 * 10^3
parms$n.thin <- 10
parms$n.chains <- 2

priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

# Fit the Bayesian model
ellipses.posterior <- siberMVN(siberExampleMeyers, parms, priors)

# Extract SEA posterior for each group
SEA.B <- siberEllipses(ellipses.posterior)

# The column names come from the community.group structure
# Use siberDensityPlot for a violin/density style plot
siberDensityPlot(SEA.B,
                 xticklabels = colnames(group.ML),
                 xlab = "Group",
                 ylab = expression("SEA"[B]~"(‰"^2*")"),
                 main = "Bayesian SEA — Meyers Islands",
                 bty = "L",
                 las = 2,
                 ylim = c(20, 100))

# Overlay the ML SEAc estimates as points for comparison
points(1:ncol(group.ML),
       group.ML["SEAc", ],
       pch = 24,          # filled triangle
       col = "black",
       bg = selected_colors,
       cex = 1.5)

# Add a legend to clarify
legend("topright",
       legend = c("SEAc (ML)"),
       pch = 24,
       pt.bg = "grey50",
       bty = "n")
















### Trying to reorder SEA plots








# Assign names based on group.ML column order
colnames(SEA.B) <- colnames(group.ML)  
# Now SEA.B has: "Meyers.Marine", "Meyers.Terrestrial"

# Reorder so Terrestrial is first (left), Marine second (right)
SEA.B_reordered <- SEA.B[, c("Meyer Islands.Terrestrial", "Meyer Islands.Marine")]

# Convert to long format
sea_long <- as.data.frame(SEA.B_reordered) %>%
  pivot_longer(everything(), names_to = "Group", values_to = "SEA_B") %>%
  mutate(Group = factor(Group, levels = c("Meyer Islands.Terrestrial", "Meyer Islands.Marine")))

# SEAc ML points in matching order
seac_df <- data.frame(
  Group = factor(c("Meyer Islands.Terrestrial", "Meyer Islands.Marine"),
                 levels = c("Meyer Islands.Terrestrial", "Meyer Islands.Marine")),
  SEAc  = group.ML["SEAc", c("Meyer Islands.Terrestrial", "Meyer Islands.Marine")]
)

# Colors: Terrestrial = green, Marine = blue
terr_color   <- viridis_full[7]
marine_color <- viridis_full[3]

ggplot(sea_long, aes(x = Group, y = SEA_B, fill = Group)) +
  geom_violin(alpha = 0.7, color = NA) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.8, color = "white") +
  geom_point(data = seac_df, aes(x = Group, y = SEAc),
             shape = 24, size = 3, 
             fill  = c(terr_color, marine_color), 
             color = "black") +
  scale_fill_manual(values = c("Meyer Islands.Terrestrial" = terr_color,
                               "Meyer Islands.Marine"      = marine_color)) +
  scale_x_discrete(labels = c("Meyer Islands.Terrestrial" = "Terrestrial",
                              "Meyer Islands.Marine"      = "Marine")) +
  labs(
    title = "Bayesian SEA — Meyer Islands",
    x     = "Group",
    y     = expression("SEA"[B]~"(‰"^2*")")
  ) +
  ylim(20, 90) +
  theme_classic(base_size = 13) +
  theme(legend.position = "none")









# Assign column names and reorder
colnames(SEA.B) <- colnames(group.ML)
SEA.B_reordered <- SEA.B[, c("Meyer Islands.Terrestrial", "Meyer Islands.Marine")]
group.ML_reordered <- group.ML[, c("Meyer Islands.Terrestrial", "Meyer Islands.Marine")]

# Plot
siberDensityPlot(SEA.B_reordered,
                 xticklabels = c("Terrestrial", "Marine"),
                 xlab        = "Group",
                 ylab        = expression("SEA"[B]~"(‰"^2*")"),
                 main        = "Bayesian SEA — Meyer Islands",
                 bty         = "L",
                 las         = 2,
                 ylim        = c(20, 100),
                 col         = c(viridis_full[7], viridis_full[3]))  # green, blue

# Overlay SEAc ML points
points(1:2,
       group.ML_reordered["SEAc", ],
       pch = 24,
       col = "black",
       bg  = c(viridis_full[7], viridis_full[3]),
       cex = 1.5)

legend("topright",
       legend = "SEAc (ML)",
       pch    = 24,
       pt.bg  = "grey50",
       bty    = "n")






# SIBER: Calculate posterior distributions too -----------------------------------

# Define MCMC parameters
parms <- list(
  n.iter = 10000,
  n.burnin = 1000,
  n.thin = 10,
  n.chains = 2
)

# Define priors
priors <- list(
  R = matrix(c(1, 0, 0, 1), 2, 2),
  k = 2,
  tau.mu = 0.001
)

# Fit Bayesian ellipses to each object
ellipses.meyers <- siberMVN(siberExampleMeyers, parms, priors)
ellipses.rangitahua <- siberMVN(siberExampleRangitāhua, parms, priors)

# Extract SEA posteriors - columns 3:6 are covariance, SEA derived from these
SEA.B.meyers <- sapply(ellipses.meyers, function(x) {
  apply(x[,3:6], 1, function(v) {
    Sigma <- matrix(v, 2, 2)
    pi * sqrt(det(Sigma))
  })
})

SEA.B.rangitahua <- sapply(ellipses.rangitahua, function(x) {
  apply(x[,3:6], 1, function(v) {
    Sigma <- matrix(v, 2, 2)
    pi * sqrt(det(Sigma))
  })
})

# Name columns for clarity - order matches 1.1, 1.2 naming convention
colnames(SEA.B.meyers) <- c("Meyer.Marine", "Meyer.Terrestrial")
colnames(SEA.B.rangitahua) <- c("Rangitahua.Marine", "Rangitahua.Terrestrial")

# Calculate overlap probabilities
p.marine <- sum(SEA.B.meyers[,"Meyer.Marine"] > 
                  SEA.B.rangitahua[,"Rangitahua.Marine"]) / nrow(SEA.B.meyers)

p.terrestrial <- sum(SEA.B.meyers[,"Meyer.Terrestrial"] > 
                       SEA.B.rangitahua[,"Rangitahua.Terrestrial"]) / nrow(SEA.B.meyers)

p.meyers.internal <- sum(SEA.B.meyers[,"Meyer.Terrestrial"] > 
                           SEA.B.meyers[,"Meyer.Marine"]) / nrow(SEA.B.meyers)

p.rangitahua.internal <- sum(SEA.B.rangitahua[,"Rangitahua.Terrestrial"] > 
                               SEA.B.rangitahua[,"Rangitahua.Marine"]) / nrow(SEA.B.rangitahua)

print(data.frame(
  Comparison = c("Meyer Marine > Rangitahua Marine",
                 "Meyer Terrestrial > Rangitahua Terrestrial",
                 "Meyer Terrestrial > Meyer Marine",
                 "Rangitahua Terrestrial > Rangitahua Marine"),
  Probability = c(p.marine, p.terrestrial, p.meyers.internal, p.rangitahua.internal)
))











## Reporting the stats


# Posterior SEA draws (rows = draws, columns = groups)
seaMeyers <- siberEllipses(ellipses.meyers)
seaRangitahua <- siberEllipses(ellipses.rangitahua)

# Check group order first: 1.1 = marine, 1.2 = terrestrial in the setup
colnames(seaMeyers) <- c("meyerMarine", "meyerTerrestrial")
colnames(seaRangitahua) <- c("rangitahuaTerrestrial", "rangitahuaMarine")

# Combine into one data frame (the two fits are independent, so pairing by row is arbitrary but valid)
nDraws <- min(nrow(seaMeyers), nrow(seaRangitahua))
seaAll <- data.frame(seaMeyers[1:nDraws, ], seaRangitahua[1:nDraws, ])

# --- 1. Point estimates and credible intervals for each SEA 
postMode <- function(x) {
  d <- density(x)
  d$x[which.max(d$y)]
}

summariseDraws <- function(x) {
  c(mode   = postMode(x),
    median = median(x),
    mean   = mean(x),
    lower  = quantile(x, 0.025, names = FALSE),
    upper  = quantile(x, 0.975, names = FALSE))
}

seaSummary <- t(sapply(seaAll, summariseDraws))
print(round(seaSummary, 2))
# Compare these with 51.43, 25.49, 70.87 and 38.9 to see which statistic you quoted

# --- 2. Terrestrial:marine ratios, calculated per draw 
ratioMeyers <- seaAll$meyerTerrestrial / seaAll$meyerMarine
ratioRangitahua <- seaAll$rangitahuaTerrestrial / seaAll$rangitahuaMarine

ratioSummary <- rbind(
  meyers = summariseDraws(ratioMeyers),
  rangitahua = summariseDraws(ratioRangitahua)
)
print(round(ratioSummary, 2))

# --- 3. Posterior probabilities, with the number of supporting draws 
probTable <- data.frame(
  comparison = c("Meyer terrestrial > Meyer marine",
                 "Rangitahua terrestrial > Rangitahua marine",
                 "Meyer marine > Rangitahua marine",
                 "Meyer terrestrial > Rangitahua terrestrial"),
  nSupporting = c(sum(seaAll$meyerTerrestrial > seaAll$meyerMarine),
                  sum(seaAll$rangitahuaTerrestrial > seaAll$rangitahuaMarine),
                  sum(seaAll$meyerMarine > seaAll$rangitahuaMarine),
                  sum(seaAll$meyerTerrestrial > seaAll$rangitahuaTerrestrial))
)
probTable$nDraws <- nDraws
probTable$probability <- round(probTable$nSupporting / probTable$nDraws, 3)
print(probTable)

# --- 4. Between-island SEA differences (Meyer minus Rangitahua)
diffMarine <- seaAll$meyerMarine - seaAll$rangitahuaMarine
diffTerrestrial <- seaAll$meyerTerrestrial - seaAll$rangitahuaTerrestrial

diffSummary <- rbind(
  marine = summariseDraws(diffMarine),
  terrestrial = summariseDraws(diffTerrestrial)
)
print(round(diffSummary, 2))
