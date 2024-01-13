library(tidyverse)
library(scales)
library(readxl)
library(ggforce)
library(ggpol)

# Colors --------------------

vb <- c("#003764", "#199ad6", "#39b54a", "#ffb81d", "#f4913e")

vb_pal_discrete <- function(direction = 1, repeat_pal = FALSE) {
  
  pal <- vb
  
  function(n) {
    
    if (repeat_pal) {
      if (n > length(pal)) {
        times <- ceiling(n / length(pal))
        pal <- rep(pal, times)
      }
    }
    
    pal_n <- pal[1:n]
    
    if (direction == -1) {
      return(rev(pal_n))
    } else {
      return(pal_n)
    }
    
  }
  
}

scale_color_vb <- function(direction = 1, repeat_pal = FALSE, ...) {
  ggplot2::discrete_scale(
    "colour", "vb", palette = vb_pal_discrete(direction = direction,
                                                repeat_pal = repeat_pal),
    ...
  )
}

scale_fill_vb <- function(direction = 1, repeat_pal = FALSE, ...) {
  ggplot2::discrete_scale(
    "fill", "vb", palette = vb_pal_discrete(direction = direction,
                                              repeat_pal = repeat_pal),
    ...
  )
}

# Timeline ------------------

mgr <- c(1125, 1147, 1167, 1199, 1269, 1421, 1486, 1522)
msp <- c(250000, 255000, 262000, 274500, 294991, 309995, 334639, 339900)

timeline <- tibble(mgr, msp) |> 
  mutate(
    across(1:2, ~ (.x - first(.x))/first(.x), .names = "{.col}_pct")
  )

ggplot(timeline, aes(y = -1:-8)) +
  geom_line(aes(x = mgr_pct), color = "#1b99d6", linewidth = 1.5, alpha = 0.8) +
  geom_line(aes(x = msp_pct), color = "#0b3b60", linewidth = 1.5, alpha = 0.8) +
  #geom_point(color = "#0b3b60", size = 3) +
  scale_x_continuous(expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  theme_void()

ggplot(timeline, aes(x = msp, y = -1:-8)) +
  geom_line(color = "#0b3b60", linewidth = 1.5, alpha = 0.2) +
  #geom_point(color = "#0b3b60", size = 3) +
  scale_x_continuous(expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  theme_void()


ggsave("vb-msp-mgr.svg", width = 2.75, height = 7.5, units = "in", dpi = 300)


mutate(chg = x - first(x), # Cumulative change
       pct_chg = chg / first(x)) # Percent change

# HH size / bedrooms --------

hhsize <- tibble(
  var = "hhsize",
  val = c("1", "2", "3", "4", "5+"),
  pct = c(0.250461789, 0.354376966, 0.170237645, 0.137050992, 0.087872608)
)

br <- tibble(
  var = "br",
  val = c("1", "2", "3", "4", "5+"),
  pct = c(0.071437255, 0.222478289, 0.393642269, 0.247173432, 0.065268755)
)

hhbr <- hhsize |> 
  bind_rows(br) |> 
  mutate(pct = 100*round(pct, 2))

ggplot(hhbr, aes(x = rev(var), y = pct, fill = val)) +
  geom_col(show.legend = F) +
  #geom_text(
  #  aes(label = paste0(pct, "%")),
  #  color = "white",
  #  size = 3,
  #  position = position_stack(vjust = 0.5),
  #  ) +
  scale_x_discrete(expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  scale_fill_vb() +
  theme_void()

ggsave("vb-hhbr.svg", width = 2, height = 2.5, units = "in", dpi = 300)

# Generation tenure ---------

boomer <- tibble(
  var = "Boomer",
  val = c("Rent", "Own with mortgage", "Own free and clear"),
  n = c(10979, 29051, 13399)
  ) |> 
  mutate(pct = n/sum(n))

mill <- tibble(
  var = "Millennial",
  val = c("Rent", "Own with mortgage", "Own free and clear"),
  n = c(27289, 22625, 2217)
) |> 
  mutate(pct = n/sum(n))

genten <- bind_rows(boomer, mill) |> 
  mutate(pct = 100*round(pct, 2))

ggplot(genten, aes(x = var, y = pct, group = val, color = val)) +
  geom_line(linewidth = 1.5, lineend = "round", alpha = 0.9, show.legend = F) +
  geom_text(
    data = filter(genten, var == "Boomer"),
    aes(label = paste0(pct, "%")),
    size = 4.5,
    show.legend = F,
    nudge_x = -0.15
    ) +
  geom_text(
    data = filter(genten, var == "Millennial"),
    aes(label = paste0(pct, "%")),
    size = 4.5,
    show.legend = F,
    nudge_x = 0.15
  ) +
  scale_x_discrete(expand = c(0.15,0.15)) +
  scale_y_continuous(limits = c(0, 60), expand = c(0.01,0.01)) +
  scale_color_vb() +
  theme_void()

ggsave("vb-genten.svg", width = 2.5, height = 3, units = "in", dpi = 300)

# Home sales ----------------

sales <- tibble(
  var = "msp",
  year = 2005:2023,
  n = c(
    226900, 250000, 260000, 250000, 240000, 244499, 226900, 234900, 237000,
    239950, 245910, 250000, 255000, 262000, 274500, 294991, 309995, 334639, 339900)
)

dom <- tibble(
  var = "dom",
  year = 2005:2023,
  n = c(
    45, 66, 81, 88, 88, 92, 99, 92, 82, 81, 79, 76, 69, 65, 59, 49, 42, 40, 45
  )
)

sales_dom <- bind_rows(sales, dom) |> 
  mutate(highlight =
           case_match(
             year,
             c(2011, 2023) ~ TRUE,
             .default = FALSE
           )
        )

ggplot(sales_dom, aes(x = year)) +
  geom_col(
    data = filter(sales_dom, var == "msp"),
    aes(y = n, alpha = highlight),
    fill = "#39b54a",
    show.legend = FALSE
    ) +
  geom_smooth(data = filter(sales_dom, var == "dom"), aes(y = n*3500), span = 0.4, color = "#003764", fill = NA, lineend = "round") +
  scale_alpha_discrete(range = c(0.5, 0.9)) +
  scale_y_continuous(name = "msp", expand = c(0.02, 0.01), sec.axis = sec_axis(trans = ~ ./3500, name = "dom")) +
  scale_x_continuous(breaks = seq(2006, 2022, 4), expand = c(0.01, 0.01)) +
  theme_void() +
  theme(
    axis.text.x = element_text(color = "#696969", size = 6)
  )

ggsave("vb-salesdom.svg", width = 3.5, height = 3, units = "in", dpi = 300)

# Rent ----------------------

rent <- read_excel("vchr_vb_data_clean.xlsx", "gross_rent")

ggplot(rent, aes(x = year, y = value)) +
  geom_smooth(data = filter(rent, rent == "Nominal Gross Rent"), color = "#199ad6", fill = NA, lineend = "round") +
  geom_smooth(data = filter(rent, rent == "Real Gross Rent (2021 Dollars)"), color = "#ffb81d", fill = NA, lineend = "round") +
  scale_y_continuous(limits = c(1000, 1550), breaks = seq(1000, 1500, 100), labels = label_dollar()) +
  scale_x_continuous(limits = c(2014, 2021.1), breaks = seq(2015, 2021, 2), expand = c(0.01, 0.03)) +
  #theme_void() +
  theme(
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.2, color = "#b2bbc6"),
    axis.text = element_text(family = "Proxima Novo", color = "#696969", size = 6)
  )

ggsave("vb-rent.svg", width = 3, height = 3.5, units = "in", dpi = 300)

# Cost burden ---------------

library(showtext)

font_add("proxima", "proxima.otf")

showtext_auto()

cb_cols = c("cb0" = "#e5e8ec", "cb30" = "#199ad6", "cb50" = "#003764")

cb <- tibble(
  var = c("hh", "renters", "owners", "ami30", "ami50", "ami80", "senior75", "senior1"),
  cb1_50 = c(24301, 13159, 11142, 9821, 7894, 4672,  3661,  5790),
  cb2_30 = c(35064, 16666, 18398, 1132, 3658, 14095, 2885,  4707),
  cb3_0 = c(117664, 30685, 85365, 2504, 2492, 8830,  10138, 8471)
  ) |> 
  pivot_longer(
    cols = 2:4, names_to = "level", values_to = "n"  
  ) |> 
  mutate(
    col_fill = case_match(
      level,
      "cb3_0" ~ "#e5e8ec",
      "cb2_30" ~ "#199ad6",
      "cb1_50" ~ "#003764"
    ),
    col_color = case_match(
      level,
      "cb3_0" ~ "grey10",
      "cb2_30" ~ "#003764",
      .default = "white"
    )
  ) |> 
  group_by(var) |> 
  mutate(
    pct = round(n/sum(n), 2),
    ymax = cumsum(pct),
    ymin = c(0, head(ymax, n = -1)),
    #pct_label = 1 - (cumsum(pct) - 0.5*pct),
    pct_label = (ymax + ymin)/2
    ) |> 
  ungroup() 

half_circle_plot <- function(data, x) {
  
  data |> 
    filter(var == x) |>
    ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = col_fill)) +
    geom_rect(alpha = 0.9) +
    geom_text(x = 3.6, aes(y = pct_label, label = label_percent()(pct), color = col_color), size = 9, family = "proxima") +
    coord_polar("y", start = -pi/2, direction = 1) +
    scale_y_continuous(limits = c(0, 2)) + 
    scale_fill_identity() +
    scale_color_identity() +
    xlim(c(2.25, 4)) +
    theme_void() +
    theme(
      plot.margin = unit(c(-0.5, -0.5, -3.5, -0.5), "in")
    )
  
}

half_circle_plot(cb, "hh")
ggsave("vb-cb-hh.svg", width = 5, height = 3, units = "in", dpi = 300)

half_circle_plot(cb, "renters")
ggsave("vb-cb-renters.svg", width = 5, height = 3, units = "in", dpi = 300)

half_circle_plot(cb, "owners")
ggsave("vb-cb-owners.svg", width = 5, height = 3, units = "in", dpi = 300)

half_circle_plot(cb, "ami30")
ggsave("vb-cb-ami30.svg", width = 5, height = 3, units = "in", dpi = 300)

half_circle_plot(cb, "ami50")
ggsave("vb-cb-ami50.svg", width = 5, height = 3, units = "in", dpi = 300)

half_circle_plot(cb, "ami80")
ggsave("vb-cb-ami80.svg", width = 5, height = 3, units = "in", dpi = 300)

half_circle_plot(cb, "senior75")
ggsave("vb-cb-senior75.svg", width = 5, height = 3, units = "in", dpi = 300)

half_circle_plot(cb, "senior1")
ggsave("vb-cb-senior1.svg", width = 5, height = 3, units = "in", dpi = 300)



## HTF map ------------------

library(tigris)
library(mapview)

va_counties <- counties(state = 51, cb = TRUE, resolution = "5m") |> 
  select(name = NAMELSAD)

htfs <- c(
  "Alexandria city",
  "Arlington County",
  #"Charlottesville city",
  "Montgomery County", "Radford city", "Pulaski County", "Floyd County", "Giles County",
  "Fairfax County",
  "Loudoun County"
  #"Richmond city"
  )

va_htfs <- va_counties |> 
  mutate(
    htf = case_match(
      name,
      htfs ~ "TRUE",
      .default = "FALSE"
    )
    )

ggplot(va_htfs, aes(fill = htf, color = htf)) +
  geom_sf() +
  scale_color_manual(values = c("TRUE" = "#74cb80", "FALSE" = "#eeeeee")) +
  scale_fill_manual(values = c("TRUE" = "#74cb80", "FALSE" = "#eeeeee")) +
  theme_void() +
  theme(legend.position = "none")

ggsave("vb-htf-map.svg", width = 3, height = 1.5, units = "in", dpi = 300)
