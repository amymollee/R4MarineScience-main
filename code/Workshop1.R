

# House Keeping
objects()

# Purge global environment
rm(list = ls())

# Confirm that global session memory is now completely vacant 
objects()

# Install here package
# install.packages("here")

# Load here package
library(here)

# Load the primary data science framework and Excel import library
library(tidyverse)
library(readxl)

# Practice Import A: Loading a standard comma-separated plain text file
benthic_cover <- read_csv(here::here("data/reef_cover_log.csv"))

# Practice Import B: Parsing a tab-separated telemetry instrument array string
acoustic_stream <- read_tsv(here::here("data/acoustic_telemetry_stream.txt"))

# Practice Import C: Targeting a specific sheet in a multi-tab Excel spreadsheet
fisheries_annual <- read_excel(here::here("data/fish_catch_data.xlsx"), sheet = "Commercial_2026")

# Read in mangrove_data
mangrove_data <- read_csv(file = here::here("data/mangrove_survey_raw.csv"))

# Use args within read_csv to skip headers and declare missing flags
mangrove_data <- read_csv(
  here::here("data/mangrove_survey_raw.csv"),
  skip = 5,   # Skip the first 5 lines of field notes
  na = c(".", "NA", "9999", "ND", "blank"))  # Convert known text alts to true NA

# Force a modern tibble to degrade into a legacy base R data frame structure
benthic_cover_df <- as.data.frame(benthic_cover)

# Print the old-style dataframe structure to view
print(benthic_cover_df)
# And compare with tibble alternative
print(benthic_cover)


# Install the data package (execute this command once in your console pane and then delete!)
# install.packages("palmerpenguins")

# Load the package data into active memory
library(palmerpenguins)
data("penguins")

# Examine the structure of the dataset - always do this when loading a new dataset!
glimpse(penguins) # tidyverse version (from dplyr package)
str(penguins) # base R version

# Generate an exploratory summary matrix
summary(penguins)

# Vertically slice specific morphometric variables by explicit name
morphology_metrics <- select(penguins, species, bill_length_mm, bill_depth_mm, body_mass_g)
glimpse(morphology_metrics)

# Retain a continuous block of attributes using the colon operator
spatial_block <- select(penguins, species:island)

# Discard logistics tracking attributes while preserving everything else using the minus sign
clean_scientific_fields <- select(penguins, -year)

# Isolate observations belonging to a single categorical target group
adelie_cohort <- filter(penguins, species == "Adelie")

# Sift out individuals using continuous numerical boundary thresholds
# Preserves only large penguins whose mass exceeds 4500 grams
heavy_penguins <- filter(penguins, body_mass_g > 4500)

# Combine multiple conditional parameters across separate attributes
# Preserves records matching Gentoo penguins sampled explicitly on Biscoe Island
biscoe_gentoo <- filter(penguins, species == "Gentoo" & island == "Biscoe")

# Sift records matching multiple targeting flags within an explicit set
sub_islands <- filter(penguins, island %in% c("Dream", "Torgersen"))

# Sort penguins by ascending body mass (Default setting: Smallest mass first)
lightest_first <- arrange(penguins, body_mass_g)

# Sort penguins in descending sequence using the desc() layout wrapper
heaviest_first <- arrange(penguins, desc(body_mass_g))

# Execute nested sorting criteria: Group by species, then sort by descending bill length
stratified_morphology <- arrange(penguins, species, desc(bill_length_mm))


# Pipe: best way
penguins_final <- penguins |>
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) |>
  filter(species == "Adelie")

# Calculate a new morphological ratio in our environment
penguin_ratios <- penguins  |> 
  mutate(body_mass_kg = body_mass_g / 1000,   # Convert grams to kilograms
         bill_ratio = bill_length_mm / bill_depth_mm  # Bill ratio
  )

# View your newly engineered variables appended to the far-right columns
glimpse(penguin_ratios)

# Grouping our active memory penguins by species
grouped_penguins <- group_by(penguins, species)

# Notice that the table looks identical, but metadata notes 'Groups: species [3]'
print(grouped_penguins)

# Collapsing the buckets into explicit summary metrics
species_mass_summary <- summarise(grouped_penguins,
                                  mean_mass_g = mean(body_mass_g)
)

print(species_mass_summary)


# Overcoming the missing value trap using na.rm = TRUE
biological_signal <- penguins %>%
  group_by(species, sex) %>%
  summarise(
    sample_size = n(),                                     # Count total individuals per category
    mean_mass_g = mean(body_mass_g, na.rm = TRUE),         # Calculate mean ignoring missing cells
    sd_mass_g   = sd(body_mass_g, na.rm = TRUE)            # Standard deviation calculation
  )

print(biological_signal)


# Pipe directly from aggregation to plotting with error bars

# Save the summary table
mass_compare_plot <- penguins |>
  group_by(species, island) |>
  summarise(
    mean_mass = mean(body_mass_g, na.rm = TRUE),
    sd_mass = sd(body_mass_g, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) 

# Print the summary table
print(mass_compare_plot)

# Plot from the saved summary object
ggplot(mass_compare_plot, aes(x = species, y = mean_mass, colour = island)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_mass - sd_mass, 
                    ymax = mean_mass + sd_mass), 
                width = 0.2) +
  facet_wrap(~island) +
  labs(title = "Mean Body Mass by Species and Island",
       subtitle = "Error bars represent standard deviation",
       y = "Mean Body Mass (g)",
       x = "Species") +
  theme_minimal()

mass_compare_plot



# Change from standard deviation to standard error for mass

# Save the summary table
mass_compare_plot <- penguins |>
  group_by(species, island) |>
  summarise(
    mean_mass = mean(body_mass_g, na.rm = TRUE),
    se_mass   = sd(body_mass_g, na.rm = TRUE) / sqrt(n()),
    n         = n(),
    .groups   = "drop"
  ) 

# Print the summary table
print(mass_compare_plot)


# Plot from the saved summary object
ggplot(mass_compare_plot, aes(x = species, y = mean_mass, colour = island)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_mass - se_mass,
                    ymax = mean_mass + se_mass),
                width = 0.2) +
  facet_wrap(~island) +
  labs(title    = "Mean Body Mass by Species and Island",
       subtitle = "Error bars represent standard error",
       y        = "Mean Body Mass (g)",
       x        = "Species") +
  theme_minimal()

mass_compare_plot


# Make plots prettier

# Standard deviation
mass_compare_plot <- penguins |>
  group_by(species, island) |>
    summarise(
      mean_mass = mean(body_mass_g, na.rm = TRUE),
      sd_mass   = sd(body_mass_g, na.rm = TRUE),
      n         = n(),
      .groups   = "drop"
      ) 

# Print the summary table
print(mass_compare_plot)

# Plot from the saved summary object
ggplot(mass_compare_plot, aes(x = species, y = mean_mass, colour = island)) +
  geom_errorbar(aes(ymin = mean_mass - sd_mass,
                    ymax = mean_mass + sd_mass),
                width = 0.2, linewidth = 0.7, alpha = 0.7) +
  facet_wrap(~island) +
  geom_point(size = 4, alpha = 0.9) +
  scale_colour_manual(values = c(
    "Biscoe"    = "deeppink",
    "Dream"     = "blue",
    "Torgersen" = "aquamarine"
  )) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Mean Body Mass by Species and Island",
    subtitle = "Error bars represent ± 1 standard deviation",
    y        = "Mean Body Mass (g)",
    x        = NULL,
    colour   = "Island"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title       = element_text(face = "bold", margin = margin(b = 4)),
    plot.subtitle    = element_text(colour = "grey50", margin = margin(b = 12)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position  = "top",
    legend.title     = element_text(face = "bold"),
    axis.text        = element_text(colour = "grey30")
  )

mass_compare_plot



# Standard deviation changed to standard error

# Save the summary table
mass_compare_plot <- penguins |>
  group_by(species, island) |>
  summarise(
    mean_mass = mean(body_mass_g, na.rm = TRUE),
    se_mass   = sd(body_mass_g, na.rm = TRUE) / sqrt(n()),
    n         = n(),
    .groups   = "drop"
  ) 

# Print the summary table
print(mass_compare_plot)


# Plot from the saved summary object
ggplot(mass_compare_plot, aes(x = species, y = mean_mass, colour = island)) +
  geom_errorbar(aes(ymin = mean_mass - se_mass,
                    ymax = mean_mass + se_mass),
                width = 0.3, linewidth = 0.9, alpha = 0.7) +
  facet_wrap(~island) +
  geom_point(size = 5, alpha = 0.9) +
  scale_colour_manual(values = c(
    "Biscoe"    = "deeppink",
    "Dream"     = "blue",
    "Torgersen" = "aquamarine"
  )) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Mean Body Mass by Species and Island",
    subtitle = "Error bars represent ± 1 standard error",
    y        = "Mean Body Mass (g)",
    x        = NULL,
    colour   = "Island"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title         = element_text(face = "bold", margin = margin(b = 4)),
    plot.subtitle      = element_text(colour = "grey50", margin = margin(b = 12)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "top",
    legend.title       = element_text(face = "bold"),
    axis.text          = element_text(colour = "grey30")
  )
mass_compare_plot





# The data reveals a clear difference in body mass both between and within species. 
# 


