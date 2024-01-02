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
  geom_line(aes(x = mgr), color = "#1b99d6", linewidth = 1.5, alpha = 0.2) +
  geom_line(aes(x = msp), color = "#0b3b60", linewidth = 1.5, alpha = 0.2) +
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
  geom_text(
    aes(label = paste0(pct, "%")),
    color = "white",
    size = 3,
    position = position_stack(vjust = 0.5),
    ) +
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
    size = 3,
    show.legend = F,
    nudge_x = -0.1
    ) +
  geom_text(
    data = filter(genten, var == "Millennial"),
    aes(label = paste0(pct, "%")),
    size = 3,
    show.legend = F,
    nudge_x = 0.1
  ) +
  scale_x_discrete(expand = c(0.1,0.1)) +
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

cb_cols = c("cb0" = "#e5e8ec", "cb30" = "#199ad6", "cb50" = "#0b3b60")

cb <- tibble(
  var = c("hh", "renters", "owners", "ami30", "ami50", "ami80", "senior75", "senior1"),
  cb3_0 = c(117664, 30685, 85365, 2504, 2492, 8830,  10138, 8471),
  cb1_30 = c(35064, 16666, 18398, 1132, 3658, 14095, 2885,  4707),
  cb2_50 = c(24301, 13159, 11142, 9821, 7894, 4672,  3661,  5790)
  ) |> 
  pivot_longer(
    cols = 2:4, names_to = "level", values_to = "n"  
  ) |> 
  mutate(
    cols = case_match(
      level,
      "cb3_0" ~ "#e5e8ec",
      "cb1_30" ~ "#199ad6",
      "cb2_50" ~ "#0b3b60"
    )
  ) |> 
  group_by(var) |> 
  mutate(
    pct = round(n/sum(n), 2),
    ymax = cumsum(pct),
    ymin = c(0, head(ymax, n = -1))
    ) |> 
  #mutate(ypos = cumsum(pct) - 0.65*pct) |> 
  ungroup() 
  #mutate(level = fct_relevel(level, "cb30", "cb50", "cb0"))






ggplot(data = filter(cb, var == "hh"), aes(x = "", y = pct, fill = level, label = pct)) +
  geom_col(width = 1, show.legend = F) +
  coord_polar("y", start = 0) +
  geom_text(
    data = filter(cb, var == "hh" & level != "cb0"),
    aes(y = ypos, label = paste0(pct, "%")),
    color = "white",
    hjust = 0.8
    ) +
  scale_fill_manual(values = cb_cols) +
  theme_void()




ggplot(data = filter(cb, var == "hh"), aes(ymax = ymax, ymin = ymin, xmax = 2, xmin = 1, fill = level, label = pct)) +
  geom_rect() +
  coord_polar(theta = "y", start = -pi/2, direction = 1) +
  xlim(c(0, 2)) + ylim(c(0, 4)) +
  scale_fill_manual(values = cb_cols)
  theme_void()
  
  
  geom_col(width = 1, show.legend = F) +
  coord_polar("y", start = 0) +
  geom_text(
    data = filter(cb, var == "hh" & level != "cb0"),
    aes(y = ypos, label = paste0(pct, "%")),
    color = "white",
    hjust = 0.8
  ) +
  scale_fill_manual(values = cb_cols) +
  theme_void()

  
  
ggplot(data = filter(cb, var == "hh")) +
  geom_arcbar(aes(shares = pct, r0 = 2, r1 = 4, fill = level), show.legend = F, sep = 0, color = NA) +
  geom_text(aes(label = pct)) +
  scale_fill_manual(values = cb_cols) +
  coord_fixed() +
  theme_void()
  
  
  
plot_cb <- function(input, variable) {
  
  ggplot(data = filter(input, var == variable), aes(x = "", y = pct, fill = level, label = pct)) +
    geom_col(width = 1, show.legend = F) +
    coord_polar("y", start = 0) +
    geom_text(
      data = filter(input, var == variable & level != "cb0"),
      aes(y = ypos, label = paste0(pct, "%")),
      color = "white",
      hjust = 0.8
    ) +
    scale_fill_manual(values = cb_cols) +
    theme_void()
  
}

ggsave("vb-cb-hh.svg", width = 2, height = 2, units = "in", dpi = 300)


plot_cb(cb, "owners")



# data = cb
# filter = `var` column
# input = `var` selection ("hh")
# levels = `level` (cb0, cb30, cb50)  parties
# value = `pct`                       shares

cb_hh <- filter(cb, var == "hh") |> 
  mutate(level = fct_relevel(level, "cb1_30", "cb2_50", "cb3_0"))

cb_donut <- function(levels, values, cols = NULL, repr=c("absolute", "proportion")) {
  
  repr = match.arg(repr)
  stopifnot(length(levels) == length(values))
  if (repr == "proportion") {
    stopifnot(sum(values) == 1)
  }
  if (!is.null(cols)) {
    names(cols) <- levels
  }
  
  # arc start/end in rads, last one reset bc rounding errors
  cc <- cumsum(c(-pi/2, switch(repr, "absolute" = (values / sum(values)) * pi, "proportion" = values * pi)))
  cc[length(cc)] <- pi/2
  
  # get angle of arc midpoints
  meanAngles <- colMeans(rbind(cc[2:length(cc)], cc[1:length(cc)-1]))
  
  # unit circle
  labelX <- sin(meanAngles)
  labelY <- cos(meanAngles)
  
  # prevent bounding box < y=0
  labelY <- ifelse(labelY < 0.015, 0.015, labelY)
  
  p <- ggplot() + theme_no_axes() + coord_fixed() +
    #expand_limits(x = c(-1.3, 1.3), y = c(0, 1.3)) + 
    theme(panel.border = element_blank()) +
    theme(legend.position = "none") +
    
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.5, r = 1,
                     start = cc[1:length(values)], 
                     end = c(cc[2:length(values)], pi/2), fill = levels), color = NA) +
    
    switch(is.null(cols)+1, scale_fill_manual(values = cols), NULL) + 
    
    # for label and line positions, just scale sin & cos to get in and out of arc
    #geom_path(aes(x = c(0.9 * labelX, 1.15 * labelX), y = c(0.9 * labelY, 1.15 * labelY),
    #              group = rep(1:length(values), 2)), colour = "white", size = 2) +
    #geom_path(aes(x = c(0.9 * labelX, 1.15 * labelX), y = c(0.9 * labelY, 1.15 * labelY),
    #              group = rep(1:length(values), 2)), size = 1) +
    
    geom_text(
      aes(x = 0.75 * labelX, y = 0.75 * labelY, 
                   label = switch(repr,
                                  "absolute" = paste0(values),
                                  "proportion" = paste0(values*100, "%"))), color = "white", size = 5)
    
    #geom_point(aes(x = 0.9 * labelX, y = 0.9 * labelY), colour = "white", size = 2) +
    #geom_point(aes(x = 0.9 * labelX, y = 0.9 * labelY))
  
  return(p)
}

cb_hh <- data.frame(
  level = c("cb1_30", "cb2_50", "cb3_0"),
  pct = c(0.20, 0.14, 0.66),
  cols = c("#199ad6", "#0b3b60", "#e5e8ec")
)

cb_donut(cb_hh$level, cb_hh$pct, cb_hh$cols, repr = "proportion")


ggsave("vb-cb-hh.svg", width = 3, height = 2, units = "in", dpi = 300)
