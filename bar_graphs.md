Bar graphs for observed lessons
================
Saurabh Khanna
2020-04-14

  - [Bar graphs](#bar-graphs)
      - [Percentage](#percentage)

``` r
# Libraries
library(tidyverse)
```

# Bar graphs

## Percentage

``` r
tribble(
  ~school, ~teacher, ~act,                   ~time,
  "S2",    "T1",     "Warm up",              9,
  "S2",    "T1",     "Lesson launch",        6,
  "S2",    "T1",     "Transition to tables", 3,
  "S2",    "T1",     "Student group work",   15,
  "S2",    "T1",     "Transition to rug",    1,
  "S2",    "T1",     "Discussion",           5
) %>%
  group_by(school, teacher) %>% 
  mutate(
   time = time / sum(time)
  ) %>%
  ungroup %>% 
  mutate(
    act = act %>% fct_inorder() %>% fct_rev()
  ) %>% 
  ggplot(aes(teacher, time, fill = act)) +
  geom_col(position = "stack", color = "black") +
  scale_y_continuous(
    breaks = seq(0, 1, 0.2),
    minor_breaks = NULL,
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_fill_viridis_d() +
  guides(fill = guide_legend(reverse = T)) +
  coord_flip() +
  labs(
    x = "Teacher",
    y = "Duration (%)",
    fill = "Activity"
  )
```

![](bar_graphs_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
