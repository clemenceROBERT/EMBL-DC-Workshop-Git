
plt <- ggplot(
  data = surveys_complete,
  mapping = aes(x = weight, y = hindfoot_length)
  )
plt
str(plt)

plt +
  geom_point()

plt + 
  geom_point() +
  ggtitle("My firstplot!")

plt + 
  geom_point()
  ggtitle("Weight vs hindfoot length")

# create heatmap
install.packages("hexbin")
library(hexbin)
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_hex()

ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1)
# ?geom_point in console --> tweaking some values

ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, color = "blue")

ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.25, aes(color = species_id))

ggplot(
  data = surveys_complete,
  mapping = aes(
    x = weight, 
    y = hindfoot_length,
    color = species_id
  )
) + 
  geom_point(alpha = 0.25)


# CHALLENGE: scatterplot weight vs species_id color by plot_type
ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = weight,
    color = plot_type)
) +
  geom_point()

# but dots visual not really adapted to this data --> use _boxplot instead of _point
ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = weight)
) +
  geom_boxplot()

# still not the best, so we layer
ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = weight)
) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, color = "salmon") #adds little value for each "x" coord

ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = weight)
) +
  geom_jitter(alpha = 0.3, color = "salmon") +
  geom_boxplot(outlier.shape = NA, fill = NA)


# CHALLENGE: produce a violin plot of weight by species_id
ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = weight)
) +
  geom_violin()+
  scale_y_log10()+
  ylab("Weight (log10)")

# CHALLENGE: make a boxplot + jittered scatterplot of hindfoot_length by species_id. 
# Boxplot should be in front of the dots and filled with white

ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = hindfoot_length)
) +
  geom_jitter(aes(color = factor(plot_id))) +
  geom_boxplot()



yearly_count <- surveys_complete %>%
  count(year, genus)

ggplot(
  data = yearly_count,
  mapping = aes(
    x = year,
    y = n,
    color = genus)
) +
  geom_line()
# other writing w/o data, now that yearly_count is created (cf lines 111-112)
yearly_count %>%
  ggplot(mapping = aes(
    x = year,
    y = n,
    color = genus)) + 
  geom_line()


ggplot(
  data = yearly_count,
  mapping = aes(
    x = year,
    y = n))+
  geom_line() +
  facet_wrap(facets = vars(genus))

surveys_complete %>%
  count(year, genus, sex) %>%
  ggplot(
    mapping = aes(
    x = year,
    y = n,
    color = sex))+
  geom_line() +
  facet_wrap(facets = vars(genus))+
  theme_bw(base_size = 18)
ggsave(filename = "data/plot.pdf",
       plot = plt,
       width = 20,
       height = 20)

surveys_complete %>%
  count(year, genus, sex) %>%
  ggplot(
    mapping = aes(
      x = year,
      y = n,
      color = sex))+
  geom_line() +
  facet_grid(
    rows = vars(sex),
    cols = vars(genus))




surveys_complete %>%
  count(year, genus, sex) %>%
  ggplot(
    mapping = aes(
      x = year,
      y = n,
      color = sex))+
  geom_line() +
  facet_wrap(facet= vars(genus),
             scales = "free"
             ) +
  scale_color_manual(
    values = c("tomato", "dodgerblue"),
    labels = c("female", "male"),
    name = "Sex") +
  xlab("Year of observation") +
  ylab("Number of individuals") +
  ggtitle("observed genera over time") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    aspect.ratio = 0.5,
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
      ),
    plot.title = element_text(hjust = 0),
    panel.grid = element_blank(),
    strip.background = element_blank()
  )
ggsave(filename = "fig/plot3.pdf",
       width = 20,
       height = 20)
