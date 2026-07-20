library(tidyverse)
library(ggplot2)
library(ggthemes)

setwd("C:/Users/u0090833/OneDrive - KU Leuven/Writing/Tribunal Constitucional/R scripts")

data1 <- read.csv("info_judges_tc.csv", sep = ";", header = T) %>% 
  select(Juez_Voto, id_juez, Appointment_date, End_in_court, Party_manifesto)

data1 <- data1 %>% 
  mutate(appointment_year = lubridate::year(lubridate::dmy(Appointment_date)))
data1 <- data1 %>% 
  mutate(end_tenure_year = lubridate::year(lubridate::dmy(End_in_court)))

theta <-read.csv("indifference_estimates_full.csv", sep=",", header = T)

theta <- left_join(theta, data1, by = "id_juez")




indifferencepoints <- theta %>% 
  mutate(
    lower = ideal - 1.96*abs(SD),
    upper = ideal + 1.96*abs(SD)) 


# Determine unique years and create a color mapping
years <- sort(unique(indifferencepoints$appointment_year))
min_year <- min(years)
max_year <- max(years)

# Define a color gradient from pale blue to dark red
color_palette <- colorRampPalette(c("lightblue", "darkred"))(100)

# Plotting the graph
pdf("Judge_indifference_points.pdf", width = 6.5, height = 6.5)
print(
  ggplot(indifferencepoints, aes(x = ideal, y = reorder(Juez_Voto, ideal), 
                                 xmin = lower, xmax = upper, 
                                 color = as.numeric(appointment_year))) +
    geom_point(aes(shape = Ideology)) +
    geom_linerange() +
    scale_shape_manual(values = c(16, 17, 15),
                       labels = c("Conservative", "Ambiguous", "Progressive"),  # Custom labels for the shapes
                       name = "Partisan \nLabel" ) +  # Adjust as necessary for different ideologies
    scale_color_gradientn(colors = color_palette, limits = c(min_year, max_year), 
                          guide = guide_colorbar(title = "Year \n Appointed", title.position = "top", barwidth = 0.7, barheight = 5)) +
    theme_classic() +
    ylab("") +
    geom_vline(xintercept = -1:1, linetype = 3) +
    theme(axis.text = element_text(size = 7), 
          legend.key.height = unit(1, "cm")) +
    xlab("Left-Right")
)
dev.off()










indifferencepoints_ts <- indifferencepoints %>%
  mutate(
    end_tenure_year = ifelse(is.na(end_tenure_year), 2023, end_tenure_year)  # Replace NA with 2023
  ) %>%
  rowwise() %>%
  mutate(years = list(seq(appointment_year, end_tenure_year))) %>%  # Create a list of years for each judge
  unnest(years)  # Expand the list of years into rows




# Plot the "ideal" values over time for each judge
pdf("Indifference_points_over_time.pdf", width = 6.5, height = 6.5)
print(ggplot(indifferencepoints_ts, aes(x = years, y = ideal, group = Juez_Voto, color = Juez_Voto, linetype = Ideology)) +
  geom_line(size = 1.5) +  # Add lines for each judge
  # Add judge names at the end of their lines in small black font
  geom_text(
    data = indifferencepoints_ts %>%
      group_by(Juez_Voto) %>%
      filter(years == max(years)),  # Select the last year for each judge
    aes(label = Juez_Voto),  # Label with judge's name
    color = "black",  # Set font color to black
    hjust = 0.7,  # Align text slightly to the right of the end of the line
    nudge_x = 0.05,  # Add a small horizontal offset
    size = 1.9,  # Adjust text size for smaller names
    check_overlap = FALSE  # Prevent overlapping labels
  ) +
  scale_color_manual(values = rainbow(length(unique(indifferencepoints_ts$Juez_Voto)))) +  # Unique colors for each judge
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted"),  # Assign line types for Ideology
    labels = c("Conservative", "Ambiguous", "Progressive")  # Rename the Ideology levels
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.13))) + 
  theme_minimal() +
  labs(
    #title = "Ideological Ideal Points of Judges Over Time",
    x = NULL,  # Remove x-axis title
    y = "Latent Ideological Indifference Point",
    linetype = "Partisan \nLabel"  # Legend only for linetype
  ) +
  theme(
    legend.position = "bottom",  # Place legend on the right
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.key.width = unit(1.5, "lines"),  # Adjust key width for clarity
    legend.key.height = unit(0.8, "lines"),  # Adjust key height for clarity
    axis.title.x = element_blank(),  # Explicitly ensure no x-axis title
    axis.title.y = element_text(size = 12)
  ) +
  guides(color = "none")  # Remove color legend for judges
)
dev.off()

