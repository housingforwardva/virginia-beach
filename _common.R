# Set global knitr chunk options

knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  error = FALSE,
  message = FALSE,
  fig.show = "hold",
  fig.asp = 0.618,
  fig.align = "left"
)

# Load core packages

library(tidyverse)
library(hdatools)

# Set plot rendering options

if (knitr::is_html_output()) {
  
  knitr::opts_chunk$set(
    out.width = "100%"
  )
  
  ggplot2::theme_set(
    hdatools::theme_hfv(base_size = 11)
  )

  } else {
    
    knitr::opts_chunk$set(
      dpi = 150
    )
  
    ggplot2::theme_set(
      hdatools::theme_hfv(base_size = 9)
    )
}