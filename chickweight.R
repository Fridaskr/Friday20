library(tidyverse)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop(
    "Please provide a measurement day.\n",
    "Example: Rscript chick_diet_bars.R 14"
  )
}

# Argument is the day whose chicken weights you want to see
selected_day <- as.numeric(args[1])

# Load the complete ChickWeight dataset
Chick_data <- datasets::ChickWeight

# Find valid measurement days
available_days <- sort(unique(Chick_data$Time))

# Give a helpful message if the day is invalid
if (!selected_day %in% available_days) {
  stop(
    "Please choose one of these measurement days: ",
    paste(available_days, collapse = ", ")
  )
}

# Keep only chicken weights from the selected day
Chick_data <- Chick_data |>
  filter(Time == selected_day)

# Name the output image
outputfile <- paste0(
  "figure/chickens_weight_day_",
  selected_day,
  ".png"
)

# Create the folder for figures if necessary
dir.create("figure", showWarnings = FALSE)

# Calculate median weight and number of chickens per diet
diet_summary <- Chick_data |>
  group_by(Diet) |>
  summarise(
    median_weight = median(weight),
    n_chicks = n(),
    .groups = "drop"
  )

# Leave room above dots and labels
y_max <- max(Chick_data$weight) * 1.2

plot <- ggplot() +
  # One median bar in each diet panel
  geom_col(
    data = diet_summary,
    aes(x = "", y = median_weight, fill = Diet),
    width = 0.55,
    alpha = 0.85,
    show.legend = FALSE
  ) +
  # Every chicken measured on the selected day
  geom_point(
    data = Chick_data,
    aes(x = "", y = weight, color = Diet),
    position = position_jitter(
      width = 0.12,
      height = 0,
      seed = 20261120
    ),
    size = 2.8,
    alpha = 0.8,
    show.legend = FALSE
  ) +
  # Label the median in each panel
  geom_text(
    data = diet_summary,
    aes(
      x = "",
      y = median_weight,
      label = paste0(
        "Median: ", round(median_weight), " g",
        "\n",
        "n = ", n_chicks
      )
    ),
    vjust = -0.6,
    fontface = "bold",
    size = 3.5
  ) +
  # Make four different diet panels
  facet_wrap(~ Diet, nrow = 1) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(
    limits = c(0, y_max),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = paste("Chick body weight at day", selected_day),
    subtitle = "Each dot is one chick. The bar represents the median weight within each diet.",
    x = NULL,
    y = "Body weight (grams)",
    caption = "Source: R's built-in ChickWeight dataset."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(color = "grey30"),
    strip.text = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.caption = element_text(color = "grey35", hjust = 0),
    plot.margin = margin(12, 18, 12, 12)
  )

ggsave(
  filename = outputfile,
  plot = plot,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
)

message("Saved figure to: ", outputfile)


