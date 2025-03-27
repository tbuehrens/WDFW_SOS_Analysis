# Load the necessary libraries
library(dplyr)
library(ggplot2)
library(scales)  # This loads the label_percent function

# Apply transformation only to the numeric columns (Q2.5., Q50., Q75., Q97.5.)
ggplot(dat3 %>% 
         mutate(across(c(Q2.5., Q50., Q75., Q97.5.), ~ exp(.) - 1)), aes(x = COMMON_POPULATION_NAME)) +
  # Add the lower whisker
  geom_segment(aes(y = Q2.5., yend = Q25., xend = COMMON_POPULATION_NAME), size = 1) +
  # Add the upper whisker
  geom_segment(aes(y = Q75., yend = Q97.5., xend = COMMON_POPULATION_NAME), size = 1) +
  # Add the box (IQR: Q25 to Q75)
  geom_rect(aes(xmin = as.numeric(COMMON_POPULATION_NAME) - 0.25,
                xmax = as.numeric(COMMON_POPULATION_NAME) + 0.25,
                ymin = Q25., ymax = Q75.),
            fill = "lightblue", color = "black") +
  # Add the median line (Q50) within the box using geom_segment
  geom_segment(aes(x = as.numeric(COMMON_POPULATION_NAME) - 0.25, 
                   xend = as.numeric(COMMON_POPULATION_NAME) + 0.25, 
                   y = Q50., 
                   yend = Q50.), color = "red", size = 1) +
  # Customize theme and labels
  theme_minimal() +
  labs(
    x=NULL,
    y = "Population-Specific Trend Slopes"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept = 0) +  # Add horizontal line at y = 0
  coord_flip() +  #_
  scale_y_continuous(labels = scales::label_percent(scale = 1))  #
