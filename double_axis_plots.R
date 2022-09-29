rm( list = ls() )

library( tidyverse )
library( lubridate )

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Code to generate figures for blog post. Note only Case 1 was used.
## Published on Medium: https://medium.com/@dataforyou/a-case-for-a-dual-axe-s-attack-696400e1f8b9
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Output controls
display_plot <- F
save_plots   <- T 

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Case Study 1: Temperatures in New York, 1973 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_height_cm <- 9

# Formatting to create Date column
airquality_dt <- airquality %>% 
                 mutate( Date = dmy( str_c( Day, Month, "1973", sep = "/")) )

# Create a base plot that will later be added to
base_plot <- ggplot( airquality_dt, 
                     aes( x = Date, y = Temp )) +
             geom_line( alpha = .9 ) +
             coord_fixed( ratio =  1.78 ) +
             labs( x = "Date", 
                   y = "Fahrenheit", 
                   title = "New York Air Quality Measurements", 
                   subtitle = "Daily temperature during May - September, 1973.") +
             theme_bw()

if( display_plot) base_plot 
if( save_plots ) ggsave( base_plot, file = "case_1_base_plot.png", units = "cm", height = plot_height_cm, width = plot_height_cm * 1.78 )

# Specify break points in Fahrenheit and transform labels to Celsius 
y_breaks <- c(59, 68, 77, 86, 95, 104)
y_labels <- (y_breaks - 32) * 5 / 9

# Initialize the second axis
secondary_y_axis <- sec_axis(
  trans  = identity,
  name   = "Celsius",
  breaks = y_breaks,
  labels = y_labels
)

# Add second to base plot to produce final plot
final_plot <- base_plot + 
              scale_y_continuous( sec.axis = secondary_y_axis ) +
              theme( axis.title.x = element_text( size = 12 ), 
                     axis.title.y = element_text( size = 12 ))

if( display_plot) final_plot 
if( save_plots ) ggsave( final_plot, file = "case_1_final_plot.png", units = "cm", height = plot_height_cm, width = plot_height_cm * 1.78 )

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Case Study 2: Distribution of mammalian body weight
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_height_cm <- 9

# Raw plot
raw_plot <- ggplot( msleep, 
                    aes( bodywt, y = 1 )) +
            geom_jitter( height = .3, shape = 16, alpha = .7, size = 1 ) +
            scale_y_continuous( limits = c(.6, 1.4) ) +
            scale_x_continuous( limits = c(0, 7000)) +
           # coord_fixed( ratio = 1 ) +
            labs( x = "Body weight (Kg)", subtitle = "Distribution of Mammalian Body Weight" ) +
            theme( panel.grid = element_blank(), 
                   axis.text.y = element_blank(), 
                   axis.ticks.y = element_blank(), 
                   axis.title.y = element_blank(), 
                   axis.title.x = element_text( size = 12 ), 
                   axis.text.x = element_text( size = 10 ) )

if( display_plot) raw_plot
if( save_plots ) ggsave( raw_plot, file = "case_2_raw_plot.png", units = "cm", height = plot_height_cm, width = plot_height_cm * 3 )

# Base plot
base_plot <- ggplot( msleep,
                     aes( log10(bodywt), y = 1 )) +
             geom_jitter( height = .3, shape = 16, alpha = .7, size = 4 ) +
             scale_y_continuous( limits = c(.6, 1.4) ) +
             #scale_x_continuous( limits = c(-3, 4), breaks = -3:4 ) +
             coord_fixed( ratio = 1.78 ) +
             labs( x = "Body weight [log10(Kg)]", subtitle = "Distribution of Mammalian Body Weight") +
             theme( panel.grid = element_blank(), 
                    axis.text.y = element_blank(), 
                    axis.ticks.y = element_blank(), 
                    axis.title.y = element_blank(), 
                    axis.title.x = element_text( size = 12 ), 
                    axis.text.x = element_text( size = 10 ) )

if( display_plot) base_plot
if( save_plots ) ggsave( base_plot, file = "case_2_base_plot.png", units = "cm", height = plot_height_cm, width = plot_height_cm * 1.78 )

# Initialize the second axis
x_breaks <- -3:4
x_labels <- 10 ^ x_breaks

secondary_x_axis <- sec_axis(
  trans  = identity,
  name   = "Body weight (Kg)",
  breaks = x_breaks,
  labels = x_labels
)

# Add the second axis to the plot
final_plot <- base_plot + 
              scale_x_continuous( sec.axis = secondary_x_axis, limits = c(-3, 4), breaks = -3:4 ) +
              coord_fixed( ratio = 1.78, expand = F ) +
              annotation_logticks( sides = "t" )

if( display_plot) final_plot
if( save_plots ) ggsave( final_plot, file = "case_2_final_plot.png", units = "cm", height = plot_height_cm, width = plot_height_cm * 1.78 )

