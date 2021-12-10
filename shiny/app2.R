# Final Project Shiny app

# load packages
if (!require("tidyverse"))
  install.packages("tidyverse")
library(tidyverse)
if (!require("shiny"))
  install.packages("shiny")
library(shiny)
if (!require("ggpmisc"))
  install.packages("ggpmisc")
library(ggpmisc)
if (!require("rgdal"))
  install.packages("rgdal")
library(rgdal)
if (!require("sf"))
  install.packages("sf")
library(sf)
if (!require("sp"))
  install.packages("sp")
library(sp)
if (!require("tibble"))
  install.packages("tibble")
library(tibble)
if (!require("cowplot"))
  install.packages("cowplot")
library(cowplot)
if (!require("ggsn"))
  install.packages("ggsn")
library(ggsn)
if (!require("rgeos"))
  install.packages("rgeos")
library(rgeos)
if (!require("grid"))
  install.packages("grid")
library(grid)
if (!require("biscale"))
  install.packages("biscale")
library(biscale)
if (!require("openxlsx"))
  install.packages("openxlsx")
library(openxlsx)

# load data from data scraping section of project
dat <- readRDS("data/dat.rds") 

# prep dataset for shiny app tab 2
df <- dat %>%
  group_by(state) %>% 
  # create variables to get the mean value within state across counties
  summarise(m_le = mean(Life.Expectancy, na.rm=T), 
            m_od = mean(Drug.Overdose.Mortality.Rate, na.rm=T),
            m_food = mean(`%.Food.Insecure`, na.rm=T),
            m_insure = mean(`%.Uninsured`, na.rm=T),
            m_sleep = mean(`%.Insufficient.Sleep`, na.rm = T),
            m_income = mean(Median.Household.Income, na.rm=T)/1000) %>%
  mutate(rank_le=rank(-m_le),
         rank_od=rank(-m_od))

# prep map data for shiny app
#dat <- read.xlsx("https://www.countyhealthrankings.org/sites/default/files/media/document/2021%20County%20Health%20Rankings%20Data%20-%20v1.xlsx", sheet = 6, startRow = 2)
bivar_map <- readRDS("data/bivar_map.rds")
county_shp <- rgdal::readOGR("data", layer = "cb_2018_us_county_20m",
                             verbose = F)
county_sf <- as(county_shp, "sf")
state_shp <- rgdal::readOGR("data", layer = "cb_2018_us_state_20m",
                            verbose = F)
state_sf <- as(state_shp, "sf")
state_sf <- state_sf %>% filter(STATEFP != "72")
county_sf <- county_sf %>% mutate(fips = paste(STATEFP, COUNTYFP, sep = ""))
dat_subset <- dplyr::select(dat, c(fips, Drug.Overdose.Mortality.Rate, Life.Expectancy))
county_sf <- left_join(county_sf, dat_subset, by = c("fips" = "fips"))
county_sf <- county_sf %>% filter(STATEFP != "14" & STATEFP != "03" & STATEFP != "43" & STATEFP != "52" & STATEFP != "78" & STATEFP != "79" & STATEFP != "74" & STATEFP != "72" & STATEFP != "69" & STATEFP != "70" & STATEFP != "95" & STATEFP != "71" & STATEFP != "76" & STATEFP != "68" & STATEFP != "89" & STATEFP != "67" & STATEFP != "86" & STATEFP != "84" & STATEFP != "66" & STATEFP != "64" & STATEFP != "81" & STATEFP != "60" & COUNTYFP != "016")
county_sf_no_od_na <- county_sf %>% filter(is.na(Drug.Overdose.Mortality.Rate) == FALSE)
od_le_class <- bi_class(county_sf_no_od_na, x = Drug.Overdose.Mortality.Rate, y = Life.Expectancy, style = "quantile", dim = 3)
annotation <- ggplot() + 
  annotation_custom(grobTree(textGrob("Quantile Cutoffs\nOD Mortality (deaths/100,000): 3.82, 15.84, 24.92, 126.73\nLE (years): 64.41, 76.19, 78.70, 113.46", x = 0.38,  y = 0.05, hjust = 0, gp = gpar(col = "black", fontsize = 8)))) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA))

ui <- fluidPage(
  tabsetPanel(type="tabs",
    tabPanel("Life expectancy and Drug Overdose Mortality Rate", # first tab
             sidebarLayout(
               sidebarPanel(
      selectizeInput("topLE", # life expectancy state rankings
                               "Please specify states for US life expectancy (LE) ranking (highlighted in green):", 
                               c("Top 5", "Bottom 5", unique(df$state)), 
                               selected = "Top 5", multiple = TRUE),
             tableOutput(outputId = "tabLE"),
      selectizeInput("topOD", # drug overdose state rankings
                               "Please specify states for US drug overdose mortality (OD) rate per 100,000 population ranking (highlighted in orange):", 
                               c("Top 5", "Bottom 5", unique(df$state)), 
                               selected = "Top 5", multiple = TRUE),
            tableOutput(outputId = "tabOD")),
      mainPanel(
      plotOutput("le_plot"), # bivariate map
      plotOutput("od_plot")) # bivariate map
    )),
    
  tabPanel("What Are the Driving Factors?", # tab 2
           sidebarLayout(
             sidebarPanel(
           radioButtons("var1", "Please select a factor (x):", # choose variable 1
                                        choices = c("Drug Overdose Mortality Rate (per 100,000 people)" = "m_od",
                                                    "Life Expectancy (years)" = "m_le",
                                                    "% Food Insecure" = "m_food",
                                                    "% Uninsured" = "m_insure",
                                                    "% Insufficient Sleep" = "m_sleep",
                                                    "Median Household Income (thousand USD)" = "m_income"),
                                        selected = c("m_od")),
            radioButtons("var2", "Please select another factor (y):", # choose variable 2
                               choices = c("Drug Overdose Mortality Rate (per 100,000 people)" = "m_od",
                                           "Life Expectancy (years)" = "m_le",
                                           "% Food Insecure" = "m_food",
                                           "% Uninsured" = "m_insure",
                                           "% Insufficient Sleep" = "m_sleep",
                                           "Median Household Income (thousand USD)" = "m_income"),
                               selected = c("m_le"))),
           mainPanel(plotOutput("related_line")) # scatter plot between 2 variables
           )
  )
  )
)


server <- function(input, output) {
# output for le map
    output$le_plot <- renderPlot({
  if (input$topLE=="Top 5") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "06" | 
                                STATEFP == "08" | 
                                STATEFP == "25" | 
                                STATEFP == "27" | 
                                STATEFP == "53"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
  } else if (input$topLE=="Bottom 5") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "01" | 
                              STATEFP == "05" | 
                              STATEFP == "21" | 
                              STATEFP == "28" | 
                              STATEFP == "47"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Alabama") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "01"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Alaska") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "green", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Arizona") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "04"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Arkansas") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "05"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="California") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "06"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Colorado") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "08"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Connecticut") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "09"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Delaware") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "10"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Florida") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "12"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Georgia") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "13"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Hawaii") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "green", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Idaho") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "16"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Illinois") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "17"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Indiana") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "18"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Iowa") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "19"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Kansas") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "20"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Kentucky") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "21"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Louisiana") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "22"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Maine") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "23"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Maryland") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "24"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Massachusetts") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "25"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Michigan") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "26"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Minnesota") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "27"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Mississippi") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "28"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Missouri") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "29"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Montana") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "30"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Nebraska") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "31"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Nevada") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "32"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="New Hampshire") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "33"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="New Jersey") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "34"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="New Mexico") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "35"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="New York") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "36"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="North Carolina") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "37"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="North Dakota") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "38"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Ohio") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "39"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Oklahoma") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "40"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Oregon") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "41"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Pennsylvania") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "42"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Rhode Island") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "44"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="South Carolina") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "45"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="South Dakota") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "46"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Tennessee") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "47"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Texas") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "48"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Utah") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "49"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Vermont") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "50"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Virginia") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "51"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Washington") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "53"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="West Virginia") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "54"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Wisconsin") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "55"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
  } else if (input$topLE=="Wyoming") {
    map_od_le <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "56"), fill = NA, color = "green", size = 0.4, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_hi <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    map_od_le_ak <- ggplot() +
      geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
              color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = "DkViolet", dim = 3) +
      bi_theme() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            rect = element_blank()) + 
      geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) + 
      geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA))
    
    legend_od_le <- bi_legend(pal = "DkViolet",
                              dim = 3,
                              xlab = "Higher OD Rate",
                              ylab = "Higher LE",
                              size = 8)
    
    ggdraw() +
      draw_plot(map_od_le, 0, 0, 1, 1) +
      draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
      draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
      draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
      draw_plot(annotation)
  
      } else {bivar_map
      }  
})
  
  # output for drug overdose map
  output$od_plot <- renderPlot({
    if (input$topOD=="Top 5") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "10" | 
                                STATEFP == "24" | 
                                STATEFP == "34" | 
                                STATEFP == "39" | 
                                STATEFP == "54"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
    } else if (input$topOD=="Bottom 5") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "19" | 
                                STATEFP == "31" | 
                                STATEFP == "38" | 
                                STATEFP == "46" | 
                                STATEFP == "48"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Alabama") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "01"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Alaska") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "orange", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Arizona") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "04"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Arkansas") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "05"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="California") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "06"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Colorado") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "08"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Connecticut") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "09"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Delaware") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "10"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Florida") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "12"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Georgia") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "13"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Hawaii") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "orange", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Idaho") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "16"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Illinois") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "17"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Indiana") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "18"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Iowa") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "19"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Kansas") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "20"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Kentucky") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "21"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Louisiana") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "22"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Maine") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "23"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Maryland") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "24"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Massachusetts") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "25"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Michigan") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "26"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Minnesota") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "27"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Mississippi") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "28"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Missouri") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "29"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Montana") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "30"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Nebraska") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "31"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Nevada") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "32"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="New Hampshire") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "33"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="New Jersey") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "34"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="New Mexico") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "35"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="New York") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "36"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="North Carolina") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "37"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="North Dakota") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "38"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Ohio") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "39"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Oklahoma") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "40"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Oregon") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "41"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Pennsylvania") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "42"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Rhode Island") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "44"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="South Carolina") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "45"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="South Dakota") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "46"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Tennessee") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "47"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Texas") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "48"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Utah") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "49"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Vermont") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "50"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Virginia") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "51"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Washington") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "53"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="West Virginia") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "54"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Wisconsin") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "55"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else if (input$topOD=="Wyoming") {
      map_od_le <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP != "15" & STATEFP != "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP != "15" & STATEFP != "02"), fill = NA, color = "black",   size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "56"), fill = NA, color = "orange", size = 0.4, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_hi <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "15"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(county_sf, STATEFP == "15"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(state_sf, STATEFP == "15"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      map_od_le_ak <- ggplot() +
        geom_sf(data = filter(od_le_class, STATEFP == "02"), mapping = aes(fill = bi_class),
                color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkViolet", dim = 3) +
        bi_theme() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              rect = element_blank()) + 
        geom_sf(data = filter(state_sf, STATEFP == "02"), fill = NA, color = "black", size = 0.15, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA)) + 
        geom_sf(data = filter(county_sf, STATEFP == "02"), fill = NA, color = "gray50", size = 0.1, show.legend = FALSE) + 
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.title = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA))
      
      legend_od_le <- bi_legend(pal = "DkViolet",
                                dim = 3,
                                xlab = "Higher OD Rate",
                                ylab = "Higher LE",
                                size = 8)
      
      ggdraw() +
        draw_plot(map_od_le, 0, 0, 1, 1) +
        draw_plot(legend_od_le, 0.7, 0.01, 0.25, 0.25) +
        draw_plot(map_od_le_hi, 0.2, 0.01, 0.2, 0.2) + 
        draw_plot(map_od_le_ak, -0.07, -0.6, 1.6, 1.6) + 
        draw_plot(annotation)
      
    } else {bivar_map
    }  
  }) 

  
  # output for life expectancy table
  output$tabLE <- renderTable({
    
    # arrange data to rank
    top5_le <- df %>% arrange(desc(m_le)) %>% data.frame %>% slice(1:5) 
    bottom5_le <- df %>% arrange(m_le) %>% data.frame %>% slice(1:5)
    selected_le <- df %>% subset(state %in% input$topLE) %>% arrange(m_le) %>% data.frame
    
    # display rankings
    if (input$topLE=="Top 5") {
      data.frame(cbind("Ranking"=top5_le %>% dplyr::select(rank_le) %>% pull(),
                       "State"=top5_le %>% dplyr::select(state) %>% pull(),
                       "LE"=top5_le %>% dplyr::select(m_le) %>% pull() %>% round(digits = 0)))
    } else if (input$topLE=="Bottom 5") {
      data.frame(cbind("Ranking"=bottom5_le %>% dplyr::select(rank_le) %>% pull(),
                       "State"=bottom5_le %>% dplyr::select(state) %>% pull(),
                       "LE"=bottom5_le %>% dplyr::select(m_le) %>% pull() %>% round(digits = 0)))
    } else {
      data.frame(cbind("Ranking"=selected_le %>% dplyr::select(rank_le) %>% pull(),
                       "State"=selected_le %>% dplyr::select(state) %>% pull(),
                       "LE"=selected_le %>% dplyr::select(m_le) %>% pull() %>% round(digits = 0)))
    }
    
  })
  
  # output for drug overdose table
  output$tabOD <- renderTable({
    
    # arrange data to rank
    top5_od <- df %>% arrange(desc(m_od)) %>% data.frame %>% slice(1:5) 
    bottom5_od <- df %>% arrange(m_od) %>% data.frame %>% slice(1:5)
    selected_od <- df %>% subset(state %in% input$topod) %>% arrange(m_od) %>% data.frame
    
    # display rankings
    if (input$topOD=="Top 5") {
      data.frame(cbind("Ranking"=top5_od %>% dplyr::select(rank_od) %>% pull(),
                       "State"=top5_od %>% dplyr::select(state) %>% pull(),
                       "OD"=top5_od %>% dplyr::select(m_od) %>% pull() %>% round(digits = 0)))
    } else if (input$topOD=="Bottom 5") {
      data.frame(cbind("Ranking"=bottom5_od %>% dplyr::select(rank_od) %>% pull(),
                       "State"=bottom5_od %>% dplyr::select(state) %>% pull(),
                       "OD"=selected_od %>% dplyr::select(m_od) %>% pull() %>% round(digits = 0)))
    } else {
      data.frame(cbind("Ranking"=selected_od %>% dplyr::select(rank_od) %>% pull(),
                       "State"=selected_od %>% dplyr::select(state) %>% pull(),
                       "OD"=selected_od %>% dplyr::select(m_od) %>% pull() %>% round(digits = 0)))
    }
    
  })
  
  
  output$related_line <- renderPlot({
    # prep data and omit missing data
    df_b3 <- df %>%
      dplyr::select(state, input$var1, input$var2) %>%
      na.omit() %>% 
      data.frame()
    
    # specify formula for formula labeling later
    my.formula <- y ~ x
    
    # reactive x-axis label
    x_label <- reactive({
      req(input$var1)
      if(input$var1 == "m_od"){
        x_label <- "Drug Overdose Mortality Rate (per 100,000 people)"
      } else if(input$var1 == "m_le"){
        x_label <- "Life Expectancy (years)"
      } else if(input$var1 == "m_food"){
        x_label <- "% Food Insecure"
      } else if(input$var1 == "m_insure"){
        x_label <- "% Uninsured"
      } else if(input$var1 == "m_sleep"){
        x_label <- "% Insufficient Sleep"
      } else if(input$var1 == "m_income"){
        x_label <- "Median Household Income (thousand USD)"
      } else if(input$var1 == "m_cases"){
        x_label <- "Covid-19 case rate (per 100,000 people)"
      }
    })
    
    # reactive y-axis label
    y_label <- reactive({
      req(input$var2)
      if(input$var2 == "m_od"){
        y_label <- "Drug Overdose Mortality Rate (per 100,000 people)"
      } else if(input$var2 == "m_le"){
        y_label <- "Life Expectancy (years)"
      } else if(input$var2 == "m_food"){
        y_label <- "% Food Insecure"
      } else if(input$var2 == "m_insure"){
        y_label <- "% Uninsured"
      } else if(input$var2 == "m_sleep"){
        y_label <- "% Insufficient Sleep"
      } else if(input$var2 == "m_income"){
        y_label <- "Median Household Income (thousand USD)"
      } else if(input$var1 == "m_cases"){
        x_label <- "Covid-19 case rate (per 100,000 people)"
      }
    })
    
    # scattor plot between 2 variables
    ggplot(data=df_b3, aes(x = df_b3[,input$var1], y = df_b3[,input$var2])) +
      geom_point(size = 1) + 
      ggtitle("Are these variables related?") +
      theme(plot.title = element_text(size = 10)) + 
      xlab(paste(x_label(), "(x)")) + 
      ylab(paste(y_label(), "(y)")) + 
      theme_bw() +
      geom_smooth(method = "lm", formula = my.formula) +
      stat_poly_eq(formula = my.formula, 
                   aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                   parse = TRUE)
    
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
