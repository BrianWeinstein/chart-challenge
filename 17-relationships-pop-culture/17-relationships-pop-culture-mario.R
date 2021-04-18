
# setup #####################################################################

library(tidyverse)
library(janitor)
library(patchwork)
library(png)




# get data #####################################################################

# https://www.nintendolife.com/news/2017/05/guide_mario_kart_8_deluxe_fastest_kart_-_how_to_build_the_best_kart
characters_raw <- read_csv(file = "data/characters.csv")

characters <- characters_raw %>%
  clean_names() %>%
  separate_rows(characters, sep = ", ") %>%
  rename(character = characters)

# https://mkwrs.com/mk8dx/
records_raw <- read_csv(file = "data/records.csv")
records <- records_raw %>%
  clean_names() %>%
  mutate(t_minutes = str_match(string = time, pattern = "^(.*?)'")[,2],
         t_seconds = str_match(string = time, pattern = "'(.*?)\"")[,2],
         t_milliseconds = str_match(string = time, pattern = "\"(.*?)$")[,2],
         time_seconds = (as.numeric(t_minutes)*60) +
           (as.numeric(t_seconds)) +
           (as.numeric(t_milliseconds)/1000)) %>%
  mutate(t_minutes = NULL, t_seconds = NULL, t_milliseconds = NULL, time = NULL)




# clean data #####################################################################

# join datasets
stats <- left_join(x = records %>% select(character, track_name, track_cc, time_seconds),
                   y = characters %>% select(character, total, weight, speed),
                   by = "character")

# calculate # of records per character
stats <- stats %>%
  group_by(character) %>%
  mutate(character_records = n()) %>%
  group_by(character, track_cc) %>%
  mutate(character_track_cc_records = n()) %>%
  ungroup()

# create dataset for plotting
stats_plot <- stats %>%
  filter(track_cc == 150) %>%
  group_by(character, track_cc) %>%
  mutate(time_seconds_mean = mean(time_seconds),
         time_seconds_me_mean =  sd(time_seconds)/sqrt(n())) %>%
  mutate(min_time = if_else(time_seconds == min(time_seconds), min(time_seconds), as.numeric(NA)),
         max_time = if_else(time_seconds == max(time_seconds), max(time_seconds), as.numeric(NA))) %>%
  ungroup() %>%
  mutate(character = fct_reorder(character, -time_seconds, .fun = mean))




# create base plot ##################################################################

# baseplot
p1 <- ggplot(data = stats_plot,
             aes(x = time_seconds,
                 y = character)) +
  
  # avg finish time for each character
  geom_pointrange(aes(x = time_seconds_mean,
                      xmin = time_seconds_mean - time_seconds_me_mean,
                      xmax = time_seconds_mean + time_seconds_me_mean),
                  color = "gray80",
                  size = 0.3,
                  shape = 10,
                  size = 5) +
  
  # avg finish time for each character - label
  geom_text(aes(x = time_seconds_mean,
                label = round(time_seconds_mean)),
            nudge_y = 0.3,
            family = "mono",
            size = 4,
            color = "gray80") +
  
  # min finish time for each character - label
  geom_text(aes(x = min_time,
                label = round(min_time), color = character),
            nudge_y = 0.3,
            family = "mono",
            size = 4) +
  
  # points for finish times of each character
  geom_jitter(width = 0,
              height = 0.05,
              alpha = 0.9,
              size = 2,
              aes(color = character)) +
  
  # scales
  scale_x_continuous(n.breaks = 10) +
  scale_color_manual(values = rev(c("#C31E04", "#CF688B","#360027", "#F17426","#400097", "#D5A80A","#D49C5B", "#20BD17","#611B0C"))) +
  
  # theme
  theme_minimal() +
  theme(plot.title = element_text(family = "mono",
                                  size = 19,
                                  face = "bold",
                                  # hjust = -0.1),
                                  hjust = 0),
        plot.subtitle = element_text(family = "mono",
                                     colour = "gray23",
                                     # hjust = -0.24),
                                     hjust = 0),
        plot.caption = element_text(hjust = 0,
                                    family = "mono",
                                    colour = "gray23",
                                    size = 7),
        plot.background = element_rect(colour = NA),
        
        axis.text = element_text(family = "Karla",
                                 size = 12,
                                 face = "bold"),
        axis.text.x = element_text(family = "mono"),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title = element_text(family = "Karla", 
                                  face = "bold"),
        panel.grid.major = element_line(colour = "gray93",
                                        size = 0.3),
        panel.border = element_blank()) +
  
  # labels
  labs(x = "Finish time (seconds)",
       y = NULL) +
  
  # legends
  guides(color = FALSE) +
  
  ggsave(filename = "plots/race_records_by_character.png", width = 10, height = 6, units = "in")




# annotate the plot  ##################################################################

p2 <- p1 +
  
  # world record arrow
  geom_curve(
    data = data.frame(x1 = 65, y1 = 4.55,
                      x2 = 63, y2 = 4.95),
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.015, "npc"),
                  type = "closed"),
    curvature = 0.2,
    color = "#400097") +
  
  # world record annotation
  annotate(geom = "text",
           x = 62, y = 3.6,
           label = str_wrap('Player "alpha" set the world record in Feb 2020, finishing the "Baby Park" course — the shortest course in the series — in 62.2 seconds.',
                            width = 35),
           size = 3.5,
           hjust = 0,
           family="Karla",
           color = "#400097") +
  
  ggsave(filename = "plots/race_records_by_character_2.png", width = 10, height = 6, units = "in")

p2




# create a legend  ##################################################################

# baseplot
p3 <- ggplot(data = stats_plot %>% filter(character == "Baby Daisy" & time_seconds <= 110),
             aes(x = time_seconds,
                 y = character)) +
  
  # avg finish time for each character
  geom_pointrange(aes(x = time_seconds_mean,
                      xmin = time_seconds_mean - time_seconds_me_mean,
                      xmax = time_seconds_mean + time_seconds_me_mean),
                  color = "gray80",
                  size = 0.3,
                  shape = 10) +
  
  # avg finish time for each character - label
  geom_text(aes(x = time_seconds_mean,
                label = round(time_seconds_mean)),
            nudge_y = 0.2,
            family = "mono",
            size = 2.8,
            color = "gray80") +
  
  # min finish time for each character - label
  geom_text(aes(x = min_time,
                label = round(min_time), color = character),
            nudge_y = 0.2,
            family = "mono",
            size = 2.8) +
  
  # points for finish times of each character
  geom_jitter(width = 0,
              height = 0.05,
              alpha = 0.9,
              aes(color = character)) +
  
  # scales
  scale_color_manual(values = rev(c("#F17426"))) +
  
  # theme
  theme_void() +
  
  # labels
  labs(x = NULL,
       y = NULL) +
  
  # legends
  guides(color = FALSE) +
  
  # world record arrow
  geom_curve(
    data = data.frame(x1 = 98.1, y1 = 2.15,
                      x2 = 97.2, y2 = 1.4),
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.035, "npc"),
                  type = "closed"),
    curvature = 0.5,
    color = "#F17426") +
  
  # world record annotation
  annotate(geom = "text",
           x = 98.3, y = 2,
           label = str_wrap("Character's fastest world record finish time",
                            width = 30),
           size = 2.5,
           hjust = 0,
           family="Karla",
           color = "#F17426") +
  
  # avg arrow
  geom_curve(
    data = data.frame(x1 = 106.2, y1 = 1.9,
                      x2 = 105.6, y2 = 1.4),
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.035, "npc"),
                  type = "closed"),
    curvature = 0.3,
    color = "gray60") +
  
  # avg annotation
  annotate(geom = "text",
           x = 106.3, y = 1.8,
           label = str_wrap("Character's average finish time, among world records",
                            width = 30),
           size = 2.5,
           hjust = 0,
           family="Karla",
           color = "gray40") +
  
  # std mean arrow
  geom_curve(
    data = data.frame(x1 = 105.0, y1 = 0.6,
                      x2 = 104.3, y2 = 0.93),
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.035, "npc"),
                  type = "closed"),
    curvature = -0.3,
    color = "gray60") +
  
  # std mean annotation
  annotate(geom = "text",
           x = 105.1, y = 0.6,
           label = str_wrap("Standard deviation of the mean",
                            width = 30),
           size = 2.5,
           hjust = 0,
           family="Karla",
           color = "gray40") +
  
  coord_cartesian(xlim = c(NA, 113.2), ylim = c(0.7, 2)) +
  
  theme(panel.border = element_rect(colour = "black", fill = NA, linetype = "dotted")) +
  
  ggsave(filename = "plots/race_records_by_character_3.png", width = 3, height = 1, units = "in")




# add the legend to the plot  ##################################################################

# annotated plot
p4 <- p2 +
  
  # legend
  inset_element(p = readPNG(source = "plots/race_records_by_character_3.png", native = TRUE),
                left = 0.6,
                bottom = 0.93,
                right = 0.97,
                top = 1.24) +
  
  # titles
  plot_annotation(
    title = "\nMario Kart world records",
    subtitle = "World record race times by character in Mario Kart 8 Deluxe\n",
    caption = '\nDataset source: "Mario Kart 8 Deluxe World Records" 150cc tracks, mkwrs.com/mk8dx/\n\n',
    theme =   theme(plot.title = element_text(family = "mono",
                                              size = 22,
                                              face = "bold",
                                              hjust = 0),
                    plot.subtitle = element_text(family = "mono",
                                                 colour = "gray23",
                                                 size = 12,
                                                 hjust = 0),
                    plot.caption = element_text(hjust = 0,
                                                family = "mono",
                                                colour = "gray23",
                                                size = 7))) +
  
  ggsave(filename = "plots/race_records_by_character_4.png", width = 10, height = 6, units = "in")

p4
