# Plots synthax -----------------------------------------------------------

unique(seussCorrectedData$GuildCapitalized)

# Create standardized color and shape scales that I can reuse
guild_shapes <- c(
  "Guano sample" = 8,                  # Star
  "Herbivore" = 17,                    # Filled triangle
  "Omnivore/Detritivore" = 15,         # Filled square
  "Carnivore" = 16,                    # Filled circle
  "Primary producer" = 11,             # Empty circle
  "Carnivore/Planktivore" = 23,        # Filled diamond (point up)
  "Omnivore/Planktivore" = 18,         # Filled diamond
  "Planktivore" = 25                  # Filled triangle (point down)
)

guildColors <- c(
  "Carnivore"             = "#8E2043",  # dark maroon
  "Carnivore/Planktivore" = "#C8402F",  # red
  "Herbivore"             = "#E69F00",  # amber (replaces yellow)
  "Omnivore/Detritivore"  = "#7B68C8",  # lavender
  "Omnivore/Planktivore"  = "#9FA8DA",  # pale periwinkle
  "Planktivore"           = "#1F2A8C",  # navy
  "Primary producer"      = "#1E9E5A",  # green
  #"Guano sample"          = "#F29DBF",  # pink
  "Detritivore"           = "#009292",
  "Omnivore"              = "#882255",
  "Predator"              = "#FF0000",
  "Unknown"               = "#999999"
)

environment_colors  <- c(
  "Terrestrial" = "#5BAA5B",
  "Marine"      = "#7F78C9"
)
unique(stableIsotopesSpecimens$Group)

group_colors <- c(
  "Birds" = "darkorchid2",                          
  "Fish" = "blue4",                           
  "Marine Invertebrates" = "cornflowerblue",           
  "Terrestrial Invertebrates" = "brown2",      
  "Terrestrial/Marine Invertebrate" = "darkorange2", 
  "Algae" = "chartreuse3",               
  "Zooplankton" = "cadetblue3",
  "Plants" = "darkgreen",
  "Rats" = "maroon4"
)



# FINAL: Figure 4B trophic position figure with drawings - Rangitāhua ---------------------------


# Combine datasets to add rats to neodata (Period 4)


groupMeansselectedPeriod4Rangitāhua <- selectedPeriod4Rangitāhua %>%
  dplyr::select(GuildCapitalized, CommonGroupName, d13CSeussCorrected, d15N, Terres.Marine) %>%
  group_by(CommonGroupName, GuildCapitalized, Terres.Marine) %>%
  summarise(
    mean_d13CSeussCorrected = mean(d13CSeussCorrected),
    mean_d15N = mean(d15N),
    sd_d13CSeussCorrected = sd(d13CSeussCorrected),
    sd_d15N = sd(d15N),
    .groups = 'drop'
  )

# Then create the sanitize function to load images
sanitize_filename <- function(x) {
  x <- gsub("/", "-", x)
  #x <- gsub(" ", "_", x)
  #x <- gsub("[^a-zA-Z0-9_-]", "", x)
  #return(tolower(x))
  return(x)
}

# Now create the group_guild_id and image columns
groupMeansselectedPeriod4Rangitāhua$group_guild_id <- paste(
  groupMeansselectedPeriod4Rangitāhua$CommonGroupName, 
  groupMeansselectedPeriod4Rangitāhua$GuildCapitalized, 
  sep = "_"
)

groupMeansselectedPeriod4Rangitāhua$image <- paste0(
  '/Users/marinaklemm/Documents/TMoR/4. Stable Isotopes/5. Scripts/GroupIcons50Transparency1.5pt/', 
  sanitize_filename(groupMeansselectedPeriod4Rangitāhua$group_guild_id),
  ".png"
)

custom_positions <- data.frame(
  group_guild_id = c("Macroalga - red_Primary producer", "Plant_Primary producer", "Fish_Carnivore",  
                     "Hermit crab_Omnivore/Detritivore", "Fish_Herbivore", "Rat_Omnivore/Detritivore",
                     "Bird_Carnivore", "Bird_Herbivore", "Sea urchin_Omnivore/Detritivore", "Fungus gnats_Omnivore/Detritivore",
                     "Zooplankton_Omnivore/Detritivore", "Macroalga - green_Primary producer",
                     "Sea snail_Carnivore", "Spider_Carnivore", "Marine caddisfly_Herbivore"),
  x_offset = c(0.3, -0.2, -0.1,
               0.5, 0.7, -0.5, 
               0, 0.1, -0.5, 0, 
               0.2, -0.1, 
               -0.6, -0.2, 0.2),  # Horizontal offset
  y_offset = c(-0.2, 0.1, 0.9, 
               -0.3, -0.2, -0.7, 
               0.5, 0.1, 0, -0.2, 
               -0.5, -0.1, 
               -1, 0.9, -0.2)   # Vertical offset
)

# Create a modified version of my dataframe with these custom offsets
groupMeansselectedPeriod4Rangitāhua <- groupMeansselectedPeriod4Rangitāhua %>%
  left_join(custom_positions, by = "group_guild_id") %>%
  mutate(
    # Replace NAs with 0 for points without custom offsets
    x_offset = replace_na(x_offset, 0),
    y_offset = replace_na(y_offset, 0),
    # Apply the offsets to create adjusted coordinates
    adj_x = mean_d13CSeussCorrected + x_offset,
    adj_y = mean_d15N + y_offset
  )

main_plotTMRangitāhua <- ggplot() +
  # Add shaded ellipses for terrestrial and marine environments
  stat_ellipse(data = groupMeansselectedPeriod4Rangitāhua,
               aes(x = mean_d13CSeussCorrected, 
                   y = mean_d15N,
                   fill = Terres.Marine), 
               geom = "polygon",
               alpha = 0.25,
               level = 0.95,
               colour = NA) +
  
  # We need to separate the environment lines from the color legend
  # One option: Draw the ellipses once for each environment type manually
  # This avoids using the colour aesthetic for the ellipses
  #stat_ellipse(data = subset(groupMeansselectedPeriod4Rangitāhua, Terres.Marine == "Terrestrial"),
  #            aes(x = mean_d13CSeussCorrected, 
  #               y = mean_d15N),
  #          geom = "path",
  #         color = "#006400",
  #linewidth = 0.8,
  #        level = 0.99) +
  
  # stat_ellipse(data = subset(groupMeansselectedPeriod4Rangitāhua, Terres.Marine == "Marine"),
  #             aes(x = mean_d13CSeussCorrected, 
  #                y = mean_d15N),
  #           geom = "path",
  #          color = "#00008B",
  #         linewidth = 0.8, 
  #        level = 0.99) +
  
  
  # Use regular position jittering for most points (points NOT in custom_positions)
  geom_image(
    data = filter(groupMeansselectedPeriod4Rangitāhua, 
                  is.na(x_offset) | x_offset == 0), 
    aes(x = mean_d13CSeussCorrected, 
        y = mean_d15N,
        image = image),
    size = 0.09,
    position = position_jitter(width = 0.5, height = 0.2, seed = 123)
  ) +  
  
  # Use custom positions for the specific points I want to dodge
  geom_image(
    data = filter(groupMeansselectedPeriod4Rangitāhua, 
                  !is.na(x_offset) & x_offset != 0), 
    aes(x = adj_x, 
        y = adj_y,
        image = image),
    size = 0.09
  ) +
  
  # Add arrows pointing to the original positions for the moved points
  geom_segment(
    data = filter(groupMeansselectedPeriod4Rangitāhua, 
                  !is.na(x_offset) & x_offset != 0),
    aes(x = mean_d13CSeussCorrected, y = mean_d15N, 
        xend = adj_x, yend = adj_y),
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = "black", linewidth = 0.3
  ) +
  
  # Black error bars
  geom_errorbar(data = groupMeansselectedPeriod4Rangitāhua,
                aes(x = mean_d13CSeussCorrected, 
                    y = mean_d15N,
                    xmin = mean_d13CSeussCorrected - sd_d13CSeussCorrected, 
                    xmax = mean_d13CSeussCorrected + sd_d13CSeussCorrected),
                color = "black",
                width = .2) +
  geom_errorbar(data = groupMeansselectedPeriod4Rangitāhua,
                aes(x = mean_d13CSeussCorrected, 
                    y = mean_d15N,
                    ymin = mean_d15N - sd_d15N, 
                    ymax = mean_d15N + sd_d15N),
                color = "black",
                width = .2) +
  
  # Add colored points based on guild - this is for the feeding guild legend
  geom_point(data = groupMeansselectedPeriod4Rangitāhua,
             aes(x = mean_d13CSeussCorrected, 
                 y = mean_d15N,
                 colour = GuildCapitalized),
             shape = 16,  
             size = 3) +
  
  # Rest of my styling code
  theme_bw() +
  labs(title = "Neodata (Rangitāhua), coloured by feeding guild, ellipses represent the habitat",
       x = expression(delta^13*C), 
       y = expression(delta^15*N)) +
  
  # Guild colors only      
  scale_colour_manual(values = guildColors, name = "Feeding guild") +
  
  # Environment colors for FILL
  scale_fill_manual(values = environment_colors, name = "Environment") +
  
  # Carefully set up the guides to handle both color aesthetics
  guides(
    image = guide_legend(title = "Organisms", order = 1),
    colour = guide_legend(title = "Feeding guild", order = 2,
                          override.aes = list(shape = 16, size = 5)),
    fill = guide_legend(title = "Environment", order = 3, 
                        override.aes = list(alpha = 0.8, size = 5))
  ) +
  
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",  # Stack legends horizontally
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(size = 14, hjust = 0.5))

main_plotTMRangitāhua


# Now, to create the beautiful legend box with the images:

# Redesign the organism guide to be more horizontal
# We'll create multiple columns to fit at the bottom
legend_data <- groupMeansselectedPeriod4Rangitāhua %>%
  dplyr::select(image, CommonGroupName) %>%
  distinct() %>%
  arrange(CommonGroupName)


# Calculate appropriate number of columns based on the number of organisms
total_items <- nrow(legend_data)
num_columns <- 10  # Adjust this based on how many columns look good
items_per_column <- ceiling(total_items / num_columns)

# When creating the legend_data dataframe, adjust the text_x value to be closer to the image
legend_data <- legend_data %>%
  mutate(
    column = rep(1:num_columns, each = items_per_column)[1:total_items],
    row = rep(rev(seq_len(items_per_column)), num_columns)[1:total_items],
    text_x = column + 0.12  # Reduced from 0.4 to 0.2 to move text closer to images
  )

# Create a more horizontal organism guide
organism_guide <- ggplot(legend_data) +
  geom_image(aes(x = column, y = row, image = image), size = 0.25) + ##Adjust the size of the image here!
  geom_text(aes(x = text_x, y = row, label = CommonGroupName), 
            hjust = 0, size = 4) +  # Smaller text to fit horizontally
  scale_x_continuous(limits = c(0.5, max(legend_data$column) + 3)) +
  scale_y_continuous(limits = c(0.5, items_per_column + 0.5)) +
  theme_void() +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  labs(title = "Organisms") +
  theme(
    plot.title = element_text(size = 12, hjust = 0, margin = margin(b = 5, l = 5))
  )

# Create a layout that stacks the organism guide below the main plot
combined_plotRangitāhuaTM <- main_plotTMRangitāhua / organism_guide +
  plot_layout(heights = c(4, 1)) +  # Adjust the height ratio as needed
  plot_annotation(
    theme = theme(
      plot.margin = margin(5, 5, 5, 5)
    )
  )

# Display the combined plot
combined_plotRangitāhuaTM



# Combining above with below ----------------------------------------------

# Key changes to add to the Rangitāhua code:

# 1. Update the error bars to be colored by guild (add color inside aes())
main_plotTMRangitāhua <- ggplot() +
  # Add shaded ellipses for terrestrial and marine environments
  stat_ellipse(data = groupMeansselectedPeriod4Rangitāhua,
               aes(x = mean_d13CSeussCorrected, 
                   y = mean_d15N,
                   fill = Terres.Marine), 
               geom = "polygon",
               alpha = 0.25,
               level = 0.95) +
  
  #stat_ellipse(data = subset(groupMeansselectedPeriod4Rangitāhua, Terres.Marine == "Terrestrial"),
  #             aes(x = mean_d13CSeussCorrected, 
  #                 y = mean_d15N),
  #             geom = "path",
  #             color = "#006400",
  #             linewidth = 0.8,
  #             level = 0.99) +
  
  #stat_ellipse(data = subset(groupMeansselectedPeriod4Rangitāhua, Terres.Marine == "Marine"),
  #             aes(x = mean_d13CSeussCorrected, 
  #                 y = mean_d15N),
  #             geom = "path",
  #             color = "#00008B",
  #             linewidth = 0.8, 
  #             level = 0.99) +
  
  # 2. CHANGE: Add colored error bars (move color inside aes())
  geom_errorbar(data = groupMeansselectedPeriod4Rangitāhua,
                aes(x = mean_d13CSeussCorrected, 
                    y = mean_d15N,
                    xmin = mean_d13CSeussCorrected - sd_d13CSeussCorrected, 
                    xmax = mean_d13CSeussCorrected + sd_d13CSeussCorrected,
                    color = GuildCapitalized),  # MOVED INSIDE aes()
                width = 0.2, linewidth = 0.4, alpha = 0.7) +
  
  geom_errorbar(data = groupMeansselectedPeriod4Rangitāhua,
                aes(x = mean_d13CSeussCorrected, 
                    y = mean_d15N,
                    ymin = mean_d15N - sd_d15N, 
                    ymax = mean_d15N + sd_d15N,
                    color = GuildCapitalized),  # MOVED INSIDE aes()
                width = 0.2, linewidth = 0.4, alpha = 0.7) +
  
  # 3. CHANGE: Add colored arrows (move color inside aes())
  geom_segment(
    data = filter(groupMeansselectedPeriod4Rangitāhua, 
                  !is.na(x_offset) & x_offset != 0),
    aes(x = mean_d13CSeussCorrected, y = mean_d15N, 
        xend = adj_x, yend = adj_y,
        color = GuildCapitalized),  # ADDED THIS LINE
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    linewidth = 0.3
  ) +
  
  # Add colored points based on guild
  geom_point(data = groupMeansselectedPeriod4Rangitāhua,
             aes(x = mean_d13CSeussCorrected, 
                 y = mean_d15N,
                 colour = GuildCapitalized),
             shape = 16,  
             size = 3) +
  
  # 4. CHANGE: Add colour aesthetic to jittered images
  geom_image(
    data = filter(groupMeansselectedPeriod4Rangitāhua, 
                  is.na(x_offset) | x_offset == 0), 
    aes(x = mean_d13CSeussCorrected, 
        y = mean_d15N,
        image = image,
        colour = GuildCapitalized),  # ADDED THIS LINE
    size = 0.09,
    position = position_jitter(width = 0.5, height = 0.2, seed = 123)
  ) +  
  
  # 5. CHANGE: Add colour aesthetic to custom positioned images
  geom_image(
    data = filter(groupMeansselectedPeriod4Rangitāhua, 
                  !is.na(x_offset) & x_offset != 0), 
    aes(x = adj_x, 
        y = adj_y,
        image = image,
        colour = GuildCapitalized),  # ADDED THIS LINE
    size = 0.09
  ) +
  
  # Rest of styling remains the same
  theme_bw() +
  labs(#title = "Neodata (Rangitāhua), coloured by feeding guild, ellipses represent the habitat,\nkōtare & rats projections",
    x = expression(delta^13*C), 
    y = expression(delta^15*N)) +
  
  scale_colour_manual(values = guildColors, name = "Feeding guild") +
  scale_fill_manual(values = environment_colors, name = "Environment") +
  scale_y_continuous(limits = c(-3, 25)) +
  guides(
    colour = guide_legend(title = "Feeding guild", order = 1,
                          nrow = 2, byrow = FALSE,
                          title.position = "top", title.hjust = 0,
                          override.aes = list(shape = 16, size = 4, alpha = 1)),
    fill   = guide_legend(title = "Environment", order = 2,
                          nrow = 2, byrow = FALSE,
                          title.position = "top", title.hjust = 0,
                          override.aes = list(alpha = 1))
  ) +
  
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.box.just = "top",       # align the tops of the two legends
        legend.box.spacing = unit(0.3, "cm"),
        legend.spacing.x = unit(0.8, "cm"),   # gap between the two legends
        legend.margin = margin(0, 0, 0, 0))
# guides(
#  image = guide_legend(title = "Organisms", order = 1),
#  colour = guide_legend(title = "Feeding guild", order = 2,
#                        override.aes = list(shape = 16, size = 5)),
#  fill = guide_legend(title = "Environment", order = 3, 
#                      override.aes = list(alpha = 0.8, size = 5))
#) +

#theme(panel.grid.minor = element_blank(),
#      legend.position = "bottom",
#      legend.box = "horizontal",
#      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
#      axis.title = element_text(size = 12),
#      axis.text = element_text(size = 12),
#      legend.text = element_text(size = 10),
#      legend.title = element_text(size = 10),
#      plot.title = element_text(size = 12, hjust = 0.5))

main_plotTMRangitāhua

# 6. CHANGE: Update organism_guide to include colored images
legend_data <- groupMeansselectedPeriod4Rangitāhua %>%  
  dplyr::select(image, CommonGroupName, GuildCapitalized) %>%
  distinct() %>%
  arrange(GuildCapitalized, CommonGroupName) %>%
  mutate(
    displayName = sub("^(.*) - (.*)$", "\\1 (\\2)", CommonGroupName),
    displayName = case_when(
      grepl("^Macroalga \\(brown\\)$", displayName, ignore.case = TRUE) ~ "Brown algae",
      grepl("^Macroalga \\(green\\)$", displayName, ignore.case = TRUE) ~ "Green algae",
      grepl("^Macroalga \\(red\\)$", displayName, ignore.case = TRUE) ~ "Red algae",
      grepl("Porcelain crab", CommonGroupName, ignore.case = TRUE) ~ "Crab",
      grepl("Marine caddisfly", CommonGroupName, ignore.case = TRUE) ~ "Caddisfly",
      grepl("Fungus gnats", CommonGroupName, ignore.case = TRUE) ~ "Fungus gnat",
      grepl("Planthoppers", CommonGroupName, ignore.case = TRUE) ~ "Planthopper",
      TRUE ~ displayName
    )
  )
total_items <- nrow(legend_data)
num_columns <- 13
items_per_column <- ceiling(total_items / num_columns)

legend_data <- legend_data %>%
  mutate(
    column = rep(1:num_columns, each = items_per_column)[1:total_items],
    row = rep(rev(seq_len(items_per_column)), num_columns)[1:total_items],
    text_x = column + 0.12,
    guild_color = guildColors[GuildCapitalized]  # ADDED guild color mapping
  )

# 7. CHANGE: Add colour aesthetic to organism guide images
organism_guide <- ggplot(legend_data) +
  geom_image(aes(x = column, y = row, image = image, 
                 colour = GuildCapitalized), size = 0.25) +  # ADDED colour
  geom_text(aes(x = text_x, y = row, label = displayName), 
            hjust = 0, size = 3, colour = "black") +  # Keep text black
  scale_x_continuous(limits = c(1, max(legend_data$column) + 1)) +
  scale_y_continuous(limits = c(0, items_per_column + 0.5)) +
  scale_colour_manual(values = guildColors) +  # ADDED scale for colors
  theme_void() +
  theme(
    #plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"  # ADDED to hide duplicate legend
  ) +
  labs(title = "Organisms") +
  theme(
    plot.title = element_text(size = 11, hjust = 0)#, margin = margin(b = 5, l = 5))
  )

combined_plotRangitāhuaTM <- main_plotTMRangitāhua / organism_guide +
  plot_layout(heights = c(4, 1)) #+
#plot_annotation(
#theme = theme(
#  plot.margin = margin(5, 5, 5, 5)
#)
#)

combined_plotRangitāhuaTM




# FINAL: Figure 4A - Now, do the same but for Meyers -----------------------------------------


groupMeansPeriod4Meyers <- Period4Meyers %>%
  dplyr::select(GuildCapitalized, CommonGroupName, d13CSeussCorrected, d15N, Terres.Marine) %>%
  group_by(CommonGroupName, GuildCapitalized, Terres.Marine) %>%
  summarise(
    mean_d13CSeussCorrected = mean(d13CSeussCorrected),
    mean_d15N = mean(d15N),
    sd_d13CSeussCorrected = sd(d13CSeussCorrected),
    sd_d15N = sd(d15N),
    .groups = 'drop'
  )

# Then create the sanitize function to load images
sanitize_filename <- function(x) {
  x <- gsub("/", "-", x)
  #x <- gsub(" ", "_", x)
  #x <- gsub("[^a-zA-Z0-9_-]", "", x)
  #return(tolower(x))
  return(x)
}

# Now create the group_guild_id and image columns
groupMeansPeriod4Meyers$group_guild_id <- paste(
  groupMeansPeriod4Meyers$CommonGroupName, 
  groupMeansPeriod4Meyers$GuildCapitalized, 
  sep = "_"
)

groupMeansPeriod4Meyers$image <- paste0(
  '/Users/marinaklemm/Documents/TMoR/4. Stable Isotopes/5. Scripts/GroupIcons50Transparency1.5pt/', 
  sanitize_filename(groupMeansPeriod4Meyers$group_guild_id),
  ".png"
)

custom_positions <- data.frame(
  group_guild_id = c("Lacewing_Carnivore", "Fish_Omnivore/Planktivore", "Fish_Carnivore",  
                     "Starfish_Carnivore", "Starfish_Omnivore/Detritivore", "Sea urchin_Omnivore/Detritivore",
                     "Anemone_Carnivore", "Bird_Guano sample", "Fish_Planktivore", "Barnacle_Omnivore/Detritivore",
                     "Sea snail_Carnivore", "Sea snail_Herbivore"),
  x_offset = c(0.001, 0.001, 0.8,
               0.2, -0.5, 0.001, 
               -0.1, 0.5, -0.1, -0.5, 
               0.2, 0.5),  # Horizontal offset
  y_offset = c(0.5, 0.8, 0, 
               -1.5, 1, -0.3, 
               -2.5, -0.4, 0.3, 0.3, 
               0.8, 0.2)   # Vertical offset
)
# Create a modified version of my dataframe with these custom offsets
groupMeansPeriod4Meyers <- groupMeansPeriod4Meyers %>%
  left_join(custom_positions, by = "group_guild_id") %>%
  mutate(
    # Replace NAs with 0 for points without custom offsets
    x_offset = replace_na(x_offset, 0),
    y_offset = replace_na(y_offset, 0),
    # Apply the offsets to create adjusted coordinates
    adj_x = mean_d13CSeussCorrected + x_offset,
    adj_y = mean_d15N + y_offset
  )

main_plotTMMeyers <- ggplot() +
  # Add shaded ellipses for terrestrial and marine environments
  stat_ellipse(data = groupMeansPeriod4Meyers,
               aes(x = mean_d13CSeussCorrected, 
                   y = mean_d15N,
                   fill = Terres.Marine), 
               geom = "polygon",
               alpha = 0.25,
               level = 0.95) +
  
  # We need to separate the environment lines from the color legend
  # One option: Draw the ellipses once for each environment type manually
  # This avoids using the colour aesthetic for the ellipses
  #stat_ellipse(data = subset(groupMeansPeriod4Meyers, Terres.Marine == "Terrestrial"),
  #             aes(x = mean_d13CSeussCorrected, 
  #                 y = mean_d15N),
  #             geom = "path",
  #             color = "#006400",
  #             linewidth = 0.8,
  #             level = 0.99) +
  
  #stat_ellipse(data = subset(groupMeansPeriod4Meyers, Terres.Marine == "Marine"),
  #             aes(x = mean_d13CSeussCorrected, 
  #                 y = mean_d15N),
  #             geom = "path",
  #             color = "#00008B",
  #             linewidth = 0.8, 
  #             level = 0.99) +
  
  
  # Use regular position jittering for most points (points NOT in custom_positions)
  geom_image(
    data = filter(groupMeansPeriod4Meyers, 
                  is.na(x_offset) | x_offset == 0), 
    aes(x = mean_d13CSeussCorrected, 
        y = mean_d15N,
        image = image),
    size = 0.09,
    position = position_jitter(width = 0.5, height = 0.2, seed = 123)
  ) +  
  
  # Use custom positions for the specific points I want to dodge
  geom_image(
    data = filter(groupMeansPeriod4Meyers, 
                  !is.na(x_offset) & x_offset != 0), 
    aes(x = adj_x, 
        y = adj_y,
        image = image),
    size = 0.09
  ) +
  
  # Add arrows pointing to the original positions for the moved points
  geom_segment(
    data = filter(groupMeansPeriod4Meyers, 
                  !is.na(x_offset) & x_offset != 0),
    aes(x = mean_d13CSeussCorrected, y = mean_d15N, 
        xend = adj_x, yend = adj_y),
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = "black", linewidth = 0.3
  ) +
  
  # Black error bars
  geom_errorbar(data = groupMeansPeriod4Meyers,
                aes(x = mean_d13CSeussCorrected, 
                    y = mean_d15N,
                    xmin = mean_d13CSeussCorrected - sd_d13CSeussCorrected, 
                    xmax = mean_d13CSeussCorrected + sd_d13CSeussCorrected),
                color = "black",
                width = .2) +
  geom_errorbar(data = groupMeansPeriod4Meyers,
                aes(x = mean_d13CSeussCorrected, 
                    y = mean_d15N,
                    ymin = mean_d15N - sd_d15N, 
                    ymax = mean_d15N + sd_d15N),
                color = "black",
                width = .2) +
  
  # Add colored points based on guild - this is for the feeding guild legend
  geom_point(data = groupMeansPeriod4Meyers,
             aes(x = mean_d13CSeussCorrected, 
                 y = mean_d15N,
                 colour = GuildCapitalized),
             shape = 16,  
             size = 3) +
  
  # Rest of my styling code
  theme_bw() +
  labs(title = "Neodata (Meyers), coloured by feeding guild, ellipses represent the habitat",
       x = expression(delta^13*C), 
       y = expression(delta^15*N)) +
  
  # Guild colors only      
  scale_colour_manual(values = guildColors, name = "Feeding guild") +
  
  # Environment colors for FILL
  scale_fill_manual(values = environment_colors, name = "Environment") +
  
  # Carefully set up the guides to handle both color aesthetics
  guides(
    image = guide_legend(title = "Organisms", order = 1),
    colour = guide_legend(title = "Feeding guild", order = 2,
                          override.aes = list(shape = 16, size = 5)),
    fill = guide_legend(title = "Environment", order = 3, 
                        override.aes = list(alpha = 0.8, size = 5))
  ) +
  
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",  # Stack legends horizontally
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(size = 14, hjust = 0.5))

main_plotTMMeyers


# Now, to create the beautiful legend box with the images:

# Redesign the organism guide to be more horizontal
# We'll create multiple columns to fit at the bottom
legend_data <- groupMeansPeriod4Meyers %>%
  dplyr::select(image, CommonGroupName, GuildCapitalized) %>%
  distinct() %>%
  arrange(GuildCapitalized, CommonGroupName) %>%
  mutate(
    displayName = sub("^(.*) - (.*)$", "\\1 (\\2)", CommonGroupName),
    displayName = case_when(
      grepl("^Macroalga \\(brown\\)$", displayName, ignore.case = TRUE) ~ "Brown algae",
      grepl("^Macroalga \\(green\\)$", displayName, ignore.case = TRUE) ~ "Green algae",
      grepl("^Macroalga \\(red\\)$", displayName, ignore.case = TRUE) ~ "Red algae",
      TRUE ~ displayName
    )
  )


# Calculate appropriate number of columns based on the number of organisms
total_items <- nrow(legend_data)
num_columns <- 13  # Adjust this based on how many columns look good
items_per_column <- ceiling(total_items / num_columns)

# When creating the legend_data dataframe, adjust the text_x value to be closer to the image
legend_data <- legend_data %>%
  mutate(
    column = rep(1:num_columns, each = items_per_column)[1:total_items],
    row = rep(rev(seq_len(items_per_column)), num_columns)[1:total_items],
    text_x = column + 0.12  # Reduced from 0.4 to 0.2 to move text closer to images
  )

# Create a more horizontal organism guide
organism_guide <- ggplot(legend_data) +
  geom_image(aes(x = column, y = row, image = image), size = 0.25) + ##Adjust the size of the image here!
  geom_text(aes(x = text_x, y = row, label = displayName), 
            hjust = 0, size = 3) +  # Smaller text to fit horizontally
  scale_x_continuous(limits = c(0.5, max(legend_data$column) + 3)) +
  scale_y_continuous(limits = c(0.5, items_per_column + 0.5)) +
  theme_void() +
  theme(
    #plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  labs(title = "Organisms") +
  theme(
    plot.title = element_text(size = 11, hjust = 0, margin = margin(b = 5, l = ))
  )

# Create a layout that stacks the organism guide below the main plot
combined_plotMeyersTM <- main_plotTMMeyers / organism_guide +
  plot_layout(heights = c(4, 1)) +  # Adjust the height ratio as needed
  plot_annotation(
    theme = theme(
      plot.margin = margin(5, 5, 5, 5)
    )
  )

# Display the combined plot
combined_plotMeyersTM



# Combining above with below ----------------------------------------------

# Key changes to add to the Meyers code:

# 1. Update the error bars to be colored by guild (add color inside aes())
main_plotTMMeyers <- ggplot() +
  # Add shaded ellipses for terrestrial and marine environments
  stat_ellipse(data = groupMeansPeriod4Meyers,
               aes(x = mean_d13CSeussCorrected, 
                   y = mean_d15N,
                   fill = Terres.Marine), 
               geom = "polygon",
               alpha = 0.25,
               level = 0.95) +
  
  #stat_ellipse(data = subset(groupMeansPeriod4Meyers, Terres.Marine == "Terrestrial"),
  #             aes(x = mean_d13CSeussCorrected, 
  #                 y = mean_d15N),
  #             geom = "path",
  #             color = "#006400",
  #             linewidth = 0.8,
  #             level = 0.99) +
  
  #stat_ellipse(data = subset(groupMeansPeriod4Meyers, Terres.Marine == "Marine"),
  #             aes(x = mean_d13CSeussCorrected, 
  #                 y = mean_d15N),
  #             geom = "path",
  #             color = "#00008B",
  #             linewidth = 0.8, 
  #             level = 0.99) +
  
  # 2. CHANGE: Add colored error bars (move color inside aes())
  geom_errorbar(data = groupMeansPeriod4Meyers,
                aes(x = mean_d13CSeussCorrected, 
                    y = mean_d15N,
                    xmin = mean_d13CSeussCorrected - sd_d13CSeussCorrected, 
                    xmax = mean_d13CSeussCorrected + sd_d13CSeussCorrected,
                    color = GuildCapitalized),  # MOVED INSIDE aes()
                width = 0.2, linewidth = 0.4, alpha = 0.7) +
  
  geom_errorbar(data = groupMeansPeriod4Meyers,
                aes(x = mean_d13CSeussCorrected, 
                    y = mean_d15N,
                    ymin = mean_d15N - sd_d15N, 
                    ymax = mean_d15N + sd_d15N,
                    color = GuildCapitalized),  # MOVED INSIDE aes()
                width = 0.2, linewidth = 0.4, alpha = 0.7) +
  
  # 3. CHANGE: Add colored arrows (move color inside aes())
  geom_segment(
    data = filter(groupMeansPeriod4Meyers, 
                  !is.na(x_offset) & x_offset != 0),
    aes(x = mean_d13CSeussCorrected, y = mean_d15N, 
        xend = adj_x, yend = adj_y,
        color = GuildCapitalized),  # ADDED THIS LINE
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    linewidth = 0.3
  ) +
  
  # Add colored points based on guild
  geom_point(data = groupMeansPeriod4Meyers,
             aes(x = mean_d13CSeussCorrected, 
                 y = mean_d15N,
                 colour = GuildCapitalized),
             shape = 16,  
             size = 3) +
  
  # 4. CHANGE: Add colour aesthetic to jittered images
  geom_image(
    data = filter(groupMeansPeriod4Meyers, 
                  is.na(x_offset) | x_offset == 0), 
    aes(x = mean_d13CSeussCorrected, 
        y = mean_d15N,
        image = image,
        colour = GuildCapitalized),  # ADDED THIS LINE
    size = 0.09,
    position = position_jitter(width = 0.5, height = 0.2, seed = 123)
  ) +  
  
  # 5. CHANGE: Add colour aesthetic to custom positioned images
  geom_image(
    data = filter(groupMeansPeriod4Meyers, 
                  !is.na(x_offset) & x_offset != 0), 
    aes(x = adj_x, 
        y = adj_y,
        image = image,
        colour = GuildCapitalized),  # ADDED THIS LINE
    size = 0.09
  ) +
  
  # Rest of styling remains the same
  theme_bw() +
  labs(#title = "Neodata (Meyer Islands), coloured by feeding guild, ellipses represent the habitat",
    x = expression(delta^13*C), 
    y = expression(delta^15*N)) +
  
  scale_colour_manual(values = guildColors, name = "Feeding guild") +
  scale_fill_manual(values = environment_colors, name = "Environment") +
  scale_y_continuous(limits = c(-3, 24)) +
  # guides(
  #  image = guide_legend(title = "Organisms", order = 1),
  #  colour = guide_legend(title = "Feeding guild", order = 2,
  #                        override.aes = list(shape = 16, size = 5)),
  #  fill = guide_legend(title = "Environment", order = 3, 
  #                      override.aes = list(alpha = 0.8, size = 5))
  #) +
  
  #theme(panel.grid.minor = element_blank(),
  #      legend.position = "bottom",
  #      legend.box = "horizontal",
  #      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
  #      axis.title = element_text(size = 12),
  #      axis.text = element_text(size = 12),
  #      legend.text = element_text(size = 10),
  #      legend.title = element_text(size = 10),
  #     plot.title = element_text(size = 12, hjust = 0.5))
  guides(
    colour = guide_legend(title = "Feeding guild", order = 1,
                          nrow = 2, byrow = FALSE,
                          title.position = "top", title.hjust = 0,
                          override.aes = list(shape = 16, size = 4, alpha = 1)),
    fill   = guide_legend(title = "Environment", order = 2,
                          nrow = 2, byrow = FALSE,
                          title.position = "top", title.hjust = 0,
                          override.aes = list(alpha = 1))
  ) +
  
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.box.just = "top",       # align the tops of the two legends
        legend.box.spacing = unit(0.3, "cm"),
        legend.spacing.x = unit(0.8, "cm"),   # gap between the two legends
        legend.margin = margin(0, 0, 0, 0))

main_plotTMMeyers

# 6. CHANGE: Update organism_guide to include colored images
legend_data <- groupMeansPeriod4Meyers %>%
  dplyr::select(image, CommonGroupName, GuildCapitalized) %>%  # ADDED GuildCapitalized
  distinct() %>%
  arrange(GuildCapitalized, CommonGroupName) %>%
  mutate(
    displayName = sub("^(.*) - (.*)$", "\\1 (\\2)", CommonGroupName),
    displayName = case_when(
      grepl("^Macroalga \\(brown\\)$", displayName, ignore.case = TRUE) ~ "Brown algae",
      grepl("^Macroalga \\(green\\)$", displayName, ignore.case = TRUE) ~ "Green algae",
      grepl("^Macroalga \\(red\\)$", displayName, ignore.case = TRUE) ~ "Red algae",
      TRUE ~ displayName
    )
  )

total_items <- nrow(legend_data)
num_columns <- 13
items_per_column <- ceiling(total_items / num_columns)

legend_data <- legend_data %>%
  mutate(
    column = rep(1:num_columns, each = items_per_column)[1:total_items],
    row = rep(rev(seq_len(items_per_column)), num_columns)[1:total_items],
    text_x = column + 0.12,
    guild_color = guildColors[GuildCapitalized]  # ADDED guild color mapping
  )

# 7. CHANGE: Add colour aesthetic to organism guide images
organism_guide <- ggplot(legend_data) +
  geom_image(aes(x = column, y = row, image = image, 
                 colour = GuildCapitalized), size = 0.25) +  # ADDED colour
  geom_text(aes(x = text_x, y = row, label = displayName), 
            hjust = 0, size = 3, colour = "black") +  # Keep text black
  scale_x_continuous(limits = c(1, max(legend_data$column) + 1)) +
  scale_y_continuous(limits = c(0, items_per_column + 0.5)) +
  scale_colour_manual(values = guildColors) +  # ADDED scale for colors
  theme_void() +
  theme(
    #plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"  # ADDED to hide duplicate legend
  ) +
  labs(title = "Organisms") +
  theme(
    plot.title = element_text(size = 11, hjust = 0)#, margin = margin(b = 5, l = 5))
  )

combined_plotMeyersTM <- main_plotTMMeyers / organism_guide +
  plot_layout(heights = c(4, 1)) #+
#plot_annotation(
#theme = theme(
#  plot.margin = margin(5, 5, 5, 5)
#)
#)

combined_plotMeyersTM
combined_plotRangitāhuaTM


