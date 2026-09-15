library(tidyverse)
library(ggalluvial)
library(ggplot2)

# businesses' longitudinal response patterns
dat <- tribble(
  ~pattern, ~wave1, ~wave2, ~wave3, ~wave4, ~wave5, ~N,
  1, 1, 1, 1, 1, 1, 59,
  2, 1, 1, 1, 1, 0, 27,
  3, 1, 1, 1, 0, 0, 110,
  4, 1, 1, 0, 0, 0, 239,
  5, 0, 1, 1, 1, 1, 20,
  6,  0, 1, 1, 1, 0, 10,
  7,  0, 1, 1, 0, 0, 51,
  8,  0, 0, 1, 1, 1, 37,
  9,  0, 0, 1, 1, 0, 39,
  10,  0, 0, 0, 1, 1, 176
)

long <- dat %>%
  pivot_longer(
    cols = starts_with("wave"),
    names_to = "wave",
    values_to = "response"
  ) %>%
  mutate(
    wave = as.integer(str_remove(wave, "wave"))
  )

dots <- long %>%
  filter(response == 1)

segments <- long %>%
  arrange(pattern, wave) %>%
  group_by(pattern) %>%
  mutate(
    next_wave = lead(wave),
    next_response = lead(response)
  ) %>%
  filter(
    response == 1,
    next_response == 1,
    next_wave == wave + 1
  ) %>%
  ungroup()

p <- ggplot() +
  geom_segment(
    data = segments,
    aes(
      x = wave,
      xend = next_wave,
      y = pattern,
      yend = pattern
    ),
    linewidth = 1
  ) +
  geom_point(
    data = dots,
    aes(x = wave, y = pattern),
    size = 4.5
  ) +
  geom_text(
    data = dat,
    aes(
      x = 5.45,
      y = pattern,
      label = N
    ),
    hjust = 0,
    size = 6
  ) +
  annotate(
    "text",
    x = 5.45,
    y = 0.2,
    label = "N",
    hjust = 0,
    fontface = "bold",
    size = 6.2
  ) +
  scale_x_continuous(
    breaks = 1:5,
    labels = paste("Wave", 1:5),
    limits = c(0.8, 6.1)
  ) +
  scale_y_reverse(
    breaks = 1:10,
    labels = 1:10
  ) +
  labs(
    x = NULL,
    y = "Participation pattern"
  ) +
  theme_classic(base_size = 18) +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 19)
  )

p




