data <- read.csv("prac.csv")

(cleaned_data <- data %>% 
    mutate(
      diff = weight6weeks - pre.weight,
      Phenotype = Phenotype %>% as.factor,
      Temp = Temp %>% as.factor
    ) %>% 
    group_by(Temp, Phenotype) %>% 
    summarise(
      mean_diff = mean(diff, na.rm = TRUE)
    ) %>% 
    drop_na())

cleaned_data %>% 
  ggplot(aes(Temperature, mean_diff)) +
  geom_line(size = 1.2, aes(group = Phenotype, color = Phenotype)) +
  geom_point(size = 2.6, aes(color = Phenotype), shape = 15) +
  geom_text(size = 6, aes(label = label, 
                          color = Phenotype),
            nudge_x = 0.04, hjust = 0,
            data = labels) +
  guides(color = FALSE) +
  labs(
    title = "Interaction between temperature and Phenotype",
    subtitle = paste0("Women benefit most from diet 3 compared to men.\n",
                      "There are no significant differences between women and\n",
                      "men in diet 1 and diet 2."),
    caption = "Source: https://www.sheffield.ac.uk/mash/statistics/datasets",
    x = "Temperature",
    y = "Growth in 6 weeks"
  ) +
  scale_color_manual(values = c("#d192b4", "#80ac54"))