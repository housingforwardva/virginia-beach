library(tidyverse)

mgr <- c(1125, 1147, 1167, 1199, 1269, 1421, 1486, 1522)
msp <- c(250000, 255000, 262000, 274500, 294991, 309995, 334639, 339900)

vb <- tibble(mgr, msp) |> 
  mutate(
    across(1:2, ~ (.x - first(.x))/first(.x), .names = "{.col}_pct")
  )

ggplot(vb, aes(y = -1:-8)) +
  geom_line(aes(x = mgr), color = "#1b99d6", linewidth = 1.5, alpha = 0.2) +
  geom_line(aes(x = msp), color = "#0b3b60", linewidth = 1.5, alpha = 0.2) +
  #geom_point(color = "#0b3b60", size = 3) +
  scale_x_continuous(expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  theme_void()

ggplot(vb, aes(x = msp, y = -1:-8)) +
  geom_line(color = "#0b3b60", linewidth = 1.5, alpha = 0.2) +
  #geom_point(color = "#0b3b60", size = 3) +
  scale_x_continuous(expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  theme_void()


ggsave("vb-msp-mgr.svg", width = 2.75, height = 7.5, units = "in", dpi = 300)


mutate(chg = x - first(x), # Cumulative change
       pct_chg = chg / first(x)) # Percent change