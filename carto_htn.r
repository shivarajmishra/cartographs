library(dplyr) # data wrangling
library(cartogram) # for the cartogram
library(ggplot2) # to realize the plots
library(transformr) # needed in order to make gganimate works with sf
library(gganimate) # To realize the animation
library(sf) # read shapefiles

# install.packages("readxl")
library(readxl)

# Load the world shapefile (this includes all countries)
afr <- read_sf("data_for_map.shp") /#you need to have the required shape file to load the map#/
unique_countries <- unique(afr$REGION)  # Replace NAME with the actual column name
print(unique_countries)
variable_names <- names(afr)
print(variable_names)

afr <- afr[afr$REGION == 142, ]

# We can visualize all country boundaries with the plot function
plot(st_geometry(afr))

# Change the projection of all countries to Mercator (EPSG: 3857)
afr <- st_transform(afr, 3857)

# Construct a cartogram using the population in 2005 for all countries
afr_cartogram <- cartogram_cont(afr, "pop", itermax = 7)

# A basic representation
plot(st_geometry(afr_cartogram))

# Using the advice of chart #331 we can custom it to get a better result:
# You can do the same for afr_cartogram

# Loop to create states
afr$id <- seq(1, nrow(afr))
afr$.frame <- 0

# Store the loop on this object
dt1 <- afr
afr_cartogram <- afr

for (i in 1:15) {
  afr_cartogram <- cartogram_cont(afr_cartogram, "pop", itermax = 1)
  afr_cartogram$.frame <- i

  dt1 <- rbind(dt1, afr_cartogram)
}

# Arrange in the inverse order now to go back to the initial state
dt2 <- dt1 %>%
  arrange(desc(.frame), id) %>%
  mutate(.frame = -1 * .frame + 31)

dt <- bind_rows(dt1, dt2) %>% arrange(.frame, id)

# check a few frames
ggplot() +
  geom_sf(data = dt %>% filter(.frame == 0), aes(fill = pop), linewidth = 0)

ggplot() +
  geom_sf(
    data = dt %>% filter(.frame == 5), aes(fill = pop),
    linewidth = 0
  )

ggplot() +
  geom_sf(
    data = dt %>% filter(.frame == 15), aes(fill = pop),
    linewidth = 0
  )

  # Remove CRS due to a bug on gganimate
dt <- st_set_crs(dt, NA)

p <- ggplot(dt) +
  geom_sf(aes(fill = pop / 1000000, group = id), linewidth = 0, alpha = 0.9) +
  theme_void() +
  scale_fill_viridis_c(
    name = "Total population with hypertension (M)",
    breaks = c(1, 10, 110, 1100),
    limits = c(0, 1100), 
    guide = guide_legend(
      keyheight = unit(3, units = "mm"),
      keywidth = unit(12, units = "mm"),
      label.position = "top",
      title.position = "top", nrow = 1
    )
  ) +
  labs(title = "", subtitle = "Total population with hypertension \n relative to country size") +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f4", color = NA),
    panel.background = element_rect(fill = "#f5f5f4", color = NA),
    legend.background = element_rect(fill = "#f5f5f4", color = NA),
    plot.title = element_text(
      size = 22, hjust = 0.5, color = "#4e4d47",
      margin = margin(
        b = -0.1, t = 0.4, l = 2,
        unit = "cm"
      )
    ),
    plot.subtitle = element_text(
      size = 13, hjust = 0.5, color = "#4e4d47",
      margin = margin(
        b = -0.1, t = 0.4, l = 2,
        unit = "cm"
      )
    ),
    legend.position = c(0.2, 0.26)
  ) +
  # from gganimate
  transition_states(.frame) +
  ease_aes("cubic-in-out")

# Make the animation
animate(p, duration = 5)

anim_save("Animated_Cartogram_Asia.gif", p, duration = 5)
# Save to a more simple path without spaces or special characters
# Extract and save the 100th frame as PNG
ggsave("frame_100.png", plot = p + transition_states(.frame) + view_follow(fixed = TRUE) + ggtitle("Frame 100"), width = 10, height = 7)