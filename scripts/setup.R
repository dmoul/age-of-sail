# setup.R

library(here)
library(tidyr)
library(readr)
library(readxl)
library(skimr) 
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(forcats)
library(glue)
library(gt)
library(purrr)
library(scales)

library(rnaturalearth) # for landmass boundaries
library(cowplot)       # for theme_map()
library(sf)
library(lwgeom)        # for st_distance()
library(measurements)  # for conv_unit()
library(units)

# library(tidygeocoder)

library(hrbrthemes)    # for plot layout
options(hrbrthemes.loadfonts = TRUE,
        warn = FALSE,
        fig.retina = 3
)
theme_set(theme_ipsum_ps() + # base_size = 10
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()
            )
)
my_caption <- glue("By Daniel Moul",
                   "\nSource: Data from logbooks digitized and recorded in the",
                   "\nClimatological Database for the World’s Oceans (CLIWOC)")

my_caption_map_extra <- glue("\nMap code following Simon Coulombe and Claus Wilke",
                             "\nhttps://www.simoncoulombe.com/2020/11/animated-ships/")

# map projection
# I wanted to use '+proj=wintri' (Winkel Tripel), but ggplot gives error after transforming with st_transform() or st_transform_proj()
##  Error in CPL_geos_op2(op, x, y) : 
##  Evaluation error: IllegalArgumentException: point array must contain 0 or >1 elements.
# my_proj <- "WGS84"
# my_proj <- '+proj=wink1'
my_proj <- '+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

sf_use_s2(FALSE)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

MIN_VOYAGE_DURATION <- 2 # days

define_flag_colors <- function() {
  ####### Define route colors
  flag_uk <- tibble(blue = "#00247d", red = "#cf142b", white = "#ffffff")
  flag_spain <- tibble(orange = "#ffc400", red = "#c60b1e")
  flag_france <- tibble(blue = "#002395", red = "#ed2939", white = "#ffffff")
  flag_netherlands <- tibble(blue = "#21468b", red = "#ae1c28", white = "cornsilk", orange = "#e19c41") #white = "#ffffff" "floral white" "ivory"
  flag_sweden <- tibble(blue = "#006aa7", yellow = "#fecc00")
  flag_usa <- tibble(blue = "#3c3b6e", red = "#b22234", white = "#ffffff")
  flag_hamburg <- tibble(red = "#da121a", black = "#000000", white = "#ffffff")
  flag_denmark <- tibble(red = "#c60c30", white = "#ffffff")
  
  # need more contrast (and brigher colors)
  # some colors from https://en.wikipedia.org/wiki/X11_color_names
  tribble(
    ~Nationality,         ~color_route,
    "BRITISH",            flag_uk$red,
    "SPANISH",            flag_spain$orange,
    "DUTCH",              "#FF00FF", #fuchsia",# flag_netherlands$white,
    "FRENCH",             "#00BFFF", # deep sky blue", # flag_france$blue,
    "SWEDISH",            flag_sweden$blue,
    "AMERICAN",           flag_usa$white, # "#FF00FF", #fuchsia",
    "HAMBURG",            "#ADFF2F", #green yellow, # "#006838", #green; from coat of arms (approx)
    "DANISH",             "#DA70D6" #orchid, #"#7FFFD4" #aquamarine # gold # from coat of arms
  )
}

color_routes <- define_flag_colors()
