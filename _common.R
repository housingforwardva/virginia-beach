library(tidyverse)
library(hdatools)

knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  error = FALSE,
  message = FALSE,
  fig.show = "hold",
  fig.asp = 0.618,
  fig.align = "left"
)

if (knitr::is_html_output()) {
  
  knitr::opts_chunk$set(
    out.width = "100%"
  )
  
  
  
  ggplot2::theme_set(
    hdatools::theme_hfv(base_size = 16)
  )

  } else {
  
    ggplot2::theme_set(
      hdatools::theme_hfv(base_size = 10)
    )
}