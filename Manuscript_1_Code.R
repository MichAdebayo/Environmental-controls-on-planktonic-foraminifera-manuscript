##**********************************************************************************************************************************##
## Written by Michael B. Adebayo, PhD Student in de Garidel-Thoron lab, (2019 – 2023)                                               ##
## Centre européen de recherche et d'enseignement de géosciences de l'environnement (CEREGE), Aix-en-Provence                       ##
## Aix-Marseille University, France                                                                                                 ##
## Thesis Chapter 5 codes                                                                                                           ##
## Code related to the published manuscript in the journal: Geochemistry, Geophysics, Geosystems (G3)                               ##                                                                                            ##
##**********************************************************************************************************************************##

##**********************************************************************************************************************************##
## Manuscript Title: Environmental Controls of Size Distribution of Modern Planktonic Foraminifera in the tropical Indian Ocean     ##
##**********************************************************************************************************************************##

##**************************
## Load Packages -----------
##**************************

library("ggplot2")
library("ggpubr")
library("ggridges")
library("MASS")
library("vegan")
library("dplyr")
library("tidyverse")
library("hrbrthemes")
library("viridis")
library("forcats")
library("plotly")
library("ggrepel")
library("grid")
library("scales")
library("ggalluvial")
library("extrafont")
library("tseries")
library("LaplacesDemon")
library("robustbase")

##****************************************
## Test for Data Normality ----------
##****************************************

jarque.bera.test(All_species_unimod_test$size) #Test whether size data follows a normal distribution using the Jarque-Bera test method

##****************************************
## Figure 3b (Human vs Machine) ----------
##****************************************

## Load Data:: Human vs Machine Count (Total Individual and Mean Species Count)

Human_vs_Machine_1 <- read.csv("Human_vs_machine_total_individual_species_count.csv", header = TRUE, sep = ',')
Human_vs_Machine_2 <- read.csv("Human_vs_machine_total_mean_species_count.csv", header = TRUE, sep = ',')

## Plot Figure

Human_vs_Machine <- ggplot(NULL, 
                           aes(Total_Machine_Count, Total_Human_Count)) + # specify columns for x and y axes (individual species count)
  geom_point(data = Human_vs_Machine_1, size = 4, # plot the machine count vs the human expert count
             aes(color = Species, shape = Species)) + # add color and shape by species
  geom_abline(intercept = 0, slope = 1, color = "black", size = .4) + # add y = x line and specify the color and size of the line (i.e., the line that cuts across the figure)
  scale_color_viridis_d(aesthetics = "colour") + # add color scale (this was not used because it was override manually)
  scale_color_manual(aesthetics = "color", # specify the color representing each species manually
                     values = c("#dd77a3", "#67c675", "#c76dcf", "#a7bc45", 
                                "#6a70d7", "#6aa13f", "#523687", "#ce9534",
                                "#6d8bd6", "#cd6d3b", "#33d4d1", "#c9417e",
                                "#47bb8a", "#a54190", "#4a6e24", "#ca86ce",
                                "#af9e4d", "#802657", "#984627", "#dd5858",
                                "#b44555", "#8B4789")) +
  scale_shape_manual(values = c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, # specify the shape representing each species manually
                                14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25)) + 
  scale_y_log10() + scale_x_log10() + # convert values to logarithmic scale
  geom_point(data = Human_vs_Machine_2, # add a second plot for the human vs machine mean count per species (note that this is the second data object)
             aes(x = Mean_Machine_Count, y = Mean_Human_Count, # specify columns for second x and y axes (mean count per species)
                 alpha = 0.9, size = Mean)) + # specify transparency level and size for each bubble (here size is scaled to the mean values for each species)
  scale_size_continuous(range = c(4,28),  # restrict bubble size of species' mean count between 4 and 28
                        name = 'Mean Count (Upper Limit)') + # add title to the legend for the mean size bubbles
  theme_bw() + # set background to white
  theme(legend.position = "right") + # place legend to the right of the figure
  theme(axis.line  = element_line(colour = "black",size = 0), # set x and y axis line colors to black
        panel.border = element_rect(colour = "black", fill = NA, size = 1), # format panel border color to black and make size of the border 1
        panel.grid.minor = element_blank(), # remove minor grid lines in figure
        panel.grid.major = element_blank()) + # remove major grid lines in figure
  geom_text_repel(data = Human_vs_Machine_2,  # load the second data to specify labels for each species' bubble
                  aes(Mean_Machine_Count, Mean_Human_Count, label = Species), # specify x and y axes and column to be used as labels
                  segment.color = 'black', # specify color for the segment lines
                  fontface = 'italic', # format labels to italics
                  size = 7, # specify label text size
                  force = 9, # customize force of repulsion between overlapping text labels
                  point.padding = 0.9, # customize the amount of padding around labeled points
                  min.segment.length = 2.3, # omit short line segments with < 2.3 minimum length
                  box.padding = 1.4) + # customize the amount of padding around bounding box
  guides(alpha = "none", color = "none", shape = guide_legend(override.aes = list(size = 7))) + # override the initial size of the shapes shown in the legend section and specify new size for the shapes
  theme(legend.text = element_text(face = "italic", size = 24)) + # customize legend text size and format in 'italics'
  theme(legend.title = element_text(size = 25)) # customize legend title text size 

## Add and Customize Axis Titles

Human_vs_Machine_exp  <- Human_vs_Machine + # recall saved plot above (remember the plot above was saved as the object: Human_vs_Machine)
  theme(axis.text = element_text(size = 24, colour = "black"), # change the size and color of the axis texts 
  axis.title = element_text(size = 25, colour = "black")) + # change the size and color of the title texts 
  annotate("text", x = 90, y = 2700, size = 7, # specify position and size of text to be annotated
           label = "Total number of forams compared") + # add the text to be annotated
  annotate("text", x = 70, y = 2300, size = 7, # specify position and size of the second text to be annotated
           label = "italic(n)==127097", parse = TRUE) + # add the second text to be annotated
  annotate("text", x = 60, y = 1800, size = 7, # specify position and size of the third text to be annotated
            label = "italic(ρ)==0.97*','~italic(p)", parse = TRUE) + # add the third text to be annotated
  annotate("text", x = 145, y = 1800, size = 7, # specify position and size for the fourth text to be annotated
           label = "< 2.2e-16") + # add the fourth text to be annotated
  annotate("text", x = 1650, y = 2200, size = 7, # specify position and size for the fifth text to be annotated
           label = "italic(y)*'='~italic(x)", parse = TRUE) + # add the fifth text to be annotated
  labs(y = "Number Counted by Human Recognition", x = "Number Counted by CNN Recognition")  # add titles for x and y axes

## Export Figure as JPG

jpeg("~/Desktop/Figure3b.jpg", width = 10500, height = 6800, units = "px",res = 600, bg = "white", pointsize = 8)
Human_vs_Machine_exp # save the new figure object in jpeg format
dev.off()


##*********************************************
## Figure 4a (This study vs ForCenS) ----------
##*********************************************

## Load Data:: Relative Abundance (This study) vs Relative Abundance (ForCenS)

Relative_abundance_comparison_vs_ForCens <- read.csv("Relative_abundance_comparison_with_ForCens.csv", header = TRUE, sep = ',')

## Plot Figure

Relative_Abundance_Comparison <- ggplot(Relative_abundance_comparison_vs_ForCens, # specify data object
                                        aes(x = rel_abund_in_ForCens_database, y = rel_abund_from_machine_count)) + # specify columns for x and y axes
  geom_point(aes(color = Species, # give each species its own color
                 shape = Species), # give each species its own unique shape
             size = 5) + # set shape size
  scale_x_log10(limits = c(0.1,100)) + # convert values on x axis to logarithmic scale and restrict values between 0.1 and 100
  scale_y_log10(limits = c(0.1,100)) + # convert values on y axis to logarithmic scale and restrict values between 0.1 and 100
  scale_color_manual(aesthetics = "colour", # specify the color representing each species manually
                     values = c("#dd77a3", "#67c675", "#c76dcf","#a7bc45",
                                "#6a70d7", "#6aa13f","#523687","#ce9534",
                                "#6d8bd6","#cd6d3b", "#33d4d1","#c9417e",
                                "#47bb8a", "#a54190", "#4a6e24","#ca86ce",
                                "#af9e4d", "#802657","#984627","#dd5858",
                                "#b44555")) + 
  scale_shape_manual(values = c(4,5,6,7,8,9,10,11,12,13, # specify the shape representing each species manually
                                14,15,16,17,18,19,20,21,22,23,24)) + 
  theme_bw () + # set background to white
  theme(legend.position = "right") + # place legend to the right of the figure
  theme(axis.line  = element_line(colour = "black",size=0), # set x and y axis line colors to black
        panel.border = element_rect(colour = "black", fill = NA, size = 1), # format panel border color to black and make size of the border 1
        panel.grid.minor = element_blank(), # remove minor grid lines in figure
        panel.grid.major = element_blank()) +# remove major grid lines in figure
  theme(legend.text = element_text(face = "italic", size = 24)) + # customize legend text size and format in 'italics'
  guides(color = guide_legend(ncol = 1, bycol = FALSE)) + # set number of columns for the color guides in legend
  guides(shape = guide_legend(override.aes = list(size = 7))) +  # set a new size for the shapes of species in legend
  theme(legend.title = element_text(size = 25)) # customize legend title text size 

## Add y = x  Line

Relative_Abundance_abline <- Relative_Abundance_Comparison + geom_abline(intercept = 0, slope = 1, color = 'gray40', size = .6) # add y = x line and specify the color and size of the line (i.e., the line that cuts across the figure)

## Add and Customize Axis Titles

Relative_Abundance_abline_Exp <- Relative_Abundance_abline + # load plot object above
  theme(axis.text = element_text(size = 24, colour = "black"), # change the size and color of the axis texts 
        axis.title = element_text(size = 25, colour = "black")) + # change the size and color of the title texts 
  annotate("text", x = 0.3, y = 100, size = 10, # specify position and size of text to be annotated
           label = "italic(ρ)==0.77*','~italic(p)", parse = TRUE) + # add the text to be annotated
  annotate("text", x = 1.6, y = 100, size = 10, # specify position and size of second text to be annotated
           label = "< 2.2e-16") + # add the second text to be annotated
  annotate("text", x = 65, y = 100, size = 10, # specify position and size of third text to be annotated
           label = "italic(y)*' ='~italic(x)", parse = TRUE) + # add the third text to be annotated
  labs(y = "Relative Abundance ((This Study) %)", x = "Relative Abundance ((ForCenS) %)") # add titles for x and y axes


## Export Figure as JPG

jpeg("~/Desktop/Figure 4a.jpg", width = 6700, height = 6000, units = "px",res = 500, bg = "white", pointsize = 8)
Relative_Abundance_abline_Exp # save the new figure object in jpeg format
dev.off()

##********************************************************
## Figure 4b (This study vs Rillo et al. 2020) ----------
##********************************************************

## Load Data:: Comparison between CNN Size vs Rillo et al. size distribution with error bars (standard deviation)

this_study_size_vs_rillo <- read.csv("size_this_study_vs_rillo_et_al.csv", header = TRUE, sep = ',')

## Plot Figure

## Set standard deviation (sd) lower and upper limit for error bar 

ymin = this_study_size_vs_rillo$machine_size - this_study_size_vs_rillo$sd # calculate minimum values for species' size range based on sd
ymax = this_study_size_vs_rillo$machine_size + this_study_size_vs_rillo$sd # calculate maximum values for species' size range based on sd

## Preliminary Figure Plot

this_study_size_vs_rillo_plot <- ggplot(this_study_size_vs_rillo, # specify data object
                                        aes(rillo_size, machine_size)) + # specify columns for x and y axes
  geom_point(aes(color = Species, # give each species its own color
                 shape = Species), # give each species its own shape
             size = 8) + # format shape size
  scale_color_manual(aesthetics = "colour", # specify the color representing each species manually
                     values = c("#DC050C", "#67c675", "#a7bc45", "#6a70d7", "#6aa13f",
                                "#ce9534", "#6d8bd6", "#cd6d3b", "#33d4d1", "#c9417e",
                                "#47bb8a", "#a54190", "#4a6e24", "#ca86ce", "#af9e4d", 
                                "#802657", "#72190E", "#dd5858", "#42150A", "#b44555")) + 
  scale_shape_manual(values = c(1, 5, 7, 8, 9, 11, 12, 13, 14, # specify the shape representing each species manually
                                15, 16, 17, 18, 19, 20, 21, 22, 23, 3, 24)) + 
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) + # add error bars 
  theme_bw() + # set background to white
  theme(legend.position = "right") + # place legend to the right of the figure
  theme(axis.line  = element_line(colour = "black",size = 0), # set x and y axis line colors to black
        panel.border = element_rect(colour = "black", fill = NA, size = 1), # format panel border color to black and make size of the border 1
        panel.grid.minor = element_blank(), # remove minor grid lines in figure
        panel.grid.major = element_blank()) + # remove major grid lines in figure
  theme(legend.text = element_text(face = "italic", size = 24)) + # customize legend text size and format in 'italics'
  guides(shape = guide_legend(override.aes = list(size = 7))) + # customize size of species' shapes in legend
  theme(legend.title = element_text(size = 25)) # customize legend title text size 

## Set Fixed X and Y Coordinates

this_study_size_vs_rillo_plots <- this_study_size_vs_rillo_plot + 
  coord_fixed(ratio = 1, xlim = c(1,1240), ylim = c(1,1240), expand = T, clip = "on") # set limits for the values on the x and y axes; this is to ensure the same value ranges on both axes

## Add y = x Line

this_study_size_vs_rillo_plots_final <- this_study_size_vs_rillo_plots + 
  geom_abline(intercept = 0, slope = 1, color = 'black', size = .6) # add y = x line and specify the color and size of the line (i.e., the line that cuts across the figure)


## Add and Customize Axis Titles

this_study_size_vs_rillo_plots_final_exp <- this_study_size_vs_rillo_plots_final + # load plot object above
  theme(axis.text = element_text(size = 24), # change the size and color of the axis texts
        axis.title = element_text(size = 25, colour = "black")) + # change the size and color of the title texts
  annotate("text", x = 220, y = 1200, size = 8, # specify position and size of text to be annotated
           label = "italic(ρ)==0.94*','~italic(p)", parse = TRUE) + # add the text to be annotated
  annotate("text", x = 535, y = 1200, size = 8, # specify position and size of second text to be annotated
           label = "= 5.9e-06") + # add the second text to be annotated
  annotate("text", x = 1100, y = 1200, size = 8,# specify position and size of third text to be annotated
           label = "italic(y)*' ='~italic(x)", parse = TRUE) + # add the third text to be annotated
  labs(y = "CNN size (μm)", x = "Buckley collection + Resampled data size (μm)") # add titles for x and y axes

## Export Figure as JPG

jpeg("~/Desktop/Figure 4b.jpg", width = 6700, height = 6000, units = "px",res = 600, bg = "white")
this_study_size_vs_rillo_plots_final_exp # save the new figure object in jpeg format
dev.off()


##******************************************************************
## Figure 5 (Factor Analysis for Assemblage distribution) ----------
##******************************************************************

## Load Data:: Assemblage Distribution Factor Analysis (Groundtruthing Cayre et al. 1997)

All_core_data <- read.csv("All_core_site_data.csv", header = TRUE, sep = ',')

## (a) Assemblage F1 Scores vs Primary Productivity

F1assemblage_vs_pp <- ggplot(All_core_site_data, # specify data object
                             aes(logpp,F1assemblage)) + # specify columns for x and y axes
  geom_point(size = 3) + # Add point size
  geom_smooth(method = "rlm", # fit robust linear model
              se=T, # show confidence interval
              size = .5, # set size of regression line
              show.legend = F, # turn off legend
              colour = "black") + # set color of regression line
  theme_bw() +  # set background to white
  theme(panel.grid.major = element_blank(), # remove major grid lines in figure
        panel.grid.minor = element_blank()) + # remove minor grid lines in figure
  theme(axis.text = element_text(size = 12, colour = "black"), # set axis text size and color
        legend.position = "none", # don't show legend
        axis.title = element_text(size=14, colour = "black", face = "bold")) + # set axis title size, color, and format
  annotate("text", x = 3.17, y = 1.1, size = 5, colour ="black", # specify position, size, and color of text to be annotated
           label = "italic(R)^{2}==0.61*','~italic(p)==2.26e-06", parse = TRUE) + # add the text to be annotated
  labs(y = expression("F1 Scores (Assemblage)"), # add title of y axis
       x = expression(Log (Primary ~ Productivity ~ (mgC ~ m^{-2}~d^{-1})))) # add title of x axis


## (b) Assemblage F2 Scores vs SST

F2assemblage_vs_sst <- ggplot(All_core_site_data, # specify data object
                              aes(sst,F2assemblage)) + # specify columns for x and y axes
  geom_point(size = 3) + # Add point size
  geom_smooth(method = "rlm", # fit robust linear model
              se = T, # show confidence interval
              size = .5, # set size of regression line
              show.legend = F, # turn off legend
              colour = "black") + # set color of regression line
  theme_bw() + # set background to white
  theme(panel.grid.major = element_blank(), # remove major grid lines in figure
        panel.grid.minor = element_blank()) + # remove minor grid lines in figure
  theme(axis.text = element_text(size = 12, colour = "black"), # set axis text size and color
        legend.position = "none", # don't show legend 
        axis.title = element_text(size = 14, colour = "black", face = "bold")) + # set axis title size, color, and format
  annotate("text", x = 26, y = 3.8, size = 5, colour ="black", # specify position, size, and color of text to be annotated
           label = "italic(R)^{2}==0.46*','~italic(p)==6.52e-4", parse = TRUE) + # add the text to be annotated
  labs(y = expression("F2 Scores (Assemblage)"), x = expression("SST(°C)")) # add x and y axis titles

## Export Figure as JPG

jpeg("~/Desktop/Figure 5.jpg", width = 6000, height = 2500, units = "px",res = 600, bg = "white", pointsize = 8)
ggarrange(F1assemblage_vs_pp,F2assemblage_vs_sst, # save the figure object in jpeg format
          ncol = 2, nrow = 1) # set number of columns and row to arrange the figure
dev.off()

##****************************************
## Figure 6 (Dissolution Index) ----------
##****************************************

## Load Data:: Dissolution Index (Berger and Parker Index vs Depth, Size vs Depth, Size vs Carbonate at Core Depth, Size vs Fragmentation Rate)

All_core_data <- read.csv("All_core_site_data.csv", header = TRUE, sep = ';')

## Plot Figures

## (b) Size vs Depth

Size_vs_Depth <- ggplot(All_core_site_data, # specify data object
                        aes(Depth,size)) + # specify columns for x and y axes
  geom_point(aes(shape = Region, color = Region), # group points by region and set color and shape based on this grouping
             size = 6) + # set point size
  geom_smooth(method = "rlm", aes(colour="rlm"), # fit robust linear model
              se = T, # show confidence interval
              size = .5, # set size of regression line
              color = 'black') + # set color of regression line
  scale_shape_manual(values = c(15,16,17,18)) + # set shape for region manually
  scale_color_manual(values = c("#b067a3", "#9c954d", "#bc7d39", "#697ed5")) + # set color for region manually
  theme_bw() + # set background to white
  theme(panel.grid.major = element_blank(), # remove major grid lines in figure
        panel.grid.minor = element_blank()) + # remove minor grid lines in figure
  theme(axis.text = element_text(size = 24, colour = "black"), # set axis text size and color
        legend.position = "right", # show legend on the right
        axis.title = element_text(size = 25, colour = "black")) + # set axis title size and color
  theme(legend.title = element_text(size = 25)) + # set legend title size 
  theme(legend.text = element_text(size = 24)) + # set legend text size
  annotate("text", x = 3000, y = 850, size = 6, colour ="black", # specify position, size, and color of text to be annotated
           label = "italic(R)^{2}==0.10*','~italic(p)==0.03", parse = TRUE) + # add the text to be annotated
  labs(y = expression(Size[95]["/"][5]~(µm)), x = expression("Depth (m)")) # add x and y axis titles

## Export Figure as JPG

jpeg("~/Desktop/Figure 6b",width = 6000, height = 3500,units = "px", res = 600,bg = "white", pointsize = 8)
Size_vs_Depth # save the figure object in jpeg format
dev.off()

## (c) Size vs Carbonate at core depth

Size_vs_Delta_carbonate_at_core_depth <- ggplot(All_core_site_data, # specify data object
                                                aes(cd_delta_calc,size)) + # specify columns for x and y axes
  geom_point(aes(shape = Region, color = Region), # group points by region and set color and shape based on this grouping
             size = 6) + # set point size
  geom_smooth(method="rlm", aes(colour="rlm"), # fit robust linear model
              se = T, # show confidence interval
              size = .5, # set size of regression line
              color = 'black')  + # set color of regression line
  scale_shape_manual(values = c(15,16,17,18)) + # set shape for region manually
  scale_color_manual(values = c("#b067a3", "#9c954d", "#bc7d39", "#697ed5")) + # set color for region manually
  theme_bw() + # set background to white
  theme(panel.grid.major = element_blank(), # remove major grid lines in figure
        panel.grid.minor = element_blank()) + # remove minor grid lines in figure
  theme(axis.text = element_text(size = 24, colour = "black"), # set axis text size and color
        legend.position = "right", # show legend on the right
        axis.title = element_text(size = 25, colour = "black")) + # set axis title size and color
  theme(legend.title = element_text(size = 25)) + # set legend title size
  theme(legend.text = element_text(size = 24)) + # set legend text size
  annotate("text", x = 15, y = 850, size = 6, colour ="black", # specify position, size, and color of text to be annotated
           label = "italic(R)^{2}==0.11*','~italic(p)==0.01", parse = TRUE) + # add the text to be annotated
  labs(y = expression(Size[95]["/"][5]~(µm)), x = expression(Delta ~ Carbonate[Core~depth] ~ (µmol/kg))) # add x and y axis titles

## Export Figure as JPG

jpeg("~/Desktop/Figure 6c",width = 6000, height = 3500, units = "px", res = 600, bg = "white", pointsize = 8)
Size_vs_Delta_carbonate_at_core_depth # save the figure object in jpeg format
dev.off()

## (d) Size vs Fragmentation Rate

Size_vs_frag_rate <- ggplot(All_core_site_data, # specify data object
                            aes(fragmentation_rate,size)) + # specify columns for x and y axes
  geom_point(aes(shape = Region, color = Region), # group points by region and set color and shape based on this grouping
             size = 6) + # set point size
  geom_smooth(method="rlm", aes(colour="rlm"), # fit robust linear model
              se = T, # show confidence interval
              size = .5, # set size of regression line
              color = 'black')  + # set color of regression line
  scale_shape_manual(values = c(15,16,17,18)) + # set shape for region manually
  scale_color_manual(values = c("#b067a3", "#9c954d", "#bc7d39", "#697ed5")) + # set color for region manually
  theme_bw() + # set background to white
  theme(panel.grid.major = element_blank(), # remove major grid lines in figure
        panel.grid.minor = element_blank()) + # remove minor grid lines in figure
  theme(axis.text = element_text(size = 24, colour = "black"), # set axis text size and color
        legend.position = "right", # show legend on the right
        axis.title = element_text(size = 25, colour = "black")) + # set axis title size and color
  theme(legend.title = element_text(size = 25)) + # set legend title size
  theme(legend.text = element_text(size = 24)) + # set legend text size
  annotate("text", x = 30, y = 850, size = 6, colour ="black", # specify position, size, and color of text to be annotated
           label = "italic(R)^{2}==3.495e-07*','~italic(p)==0.99", parse = TRUE) + # add the text to be annotated
  labs(y = expression(Size[95]["/"][5]~(µm)), x = expression("Fragmentation Rate (%)")) # add x and y axis titles

## Export Figure as JPG

jpeg("~/Desktop/Figure 6c.jpg",width = 6000, height = 3500,units = "px",res = 600,bg = "white", pointsize = 8)
Size_vs_frag_rate # save the figure object in jpeg format
dev.off()

##***********************************
## Figure 7 (Density plot) ----------
##***********************************

## Load Data:: Density Plot of Species Size

size_density <- read.csv("size_density_plot.csv", header = TRUE, sep = ',')

## Plot Figure

Densityplot <- ggplot(size_density, # specify data object
                      aes(x = size, y = species, fill = species)) + # specify columns for x and y axes, and give each species its own color
  geom_density_ridges(alpha = 0.25) + # set transparency level of colors
  scale_x_continuous(name = "Size (μm)", # set x-axis title
                     limits = c(0, 1300), # set limits for the range of values on x-axis
                     expand = c(0,0)) + # make distribution plot start from O
  scale_y_discrete(name = "Species", # set y-axis title
                   expand = c(0, 0)) + # make labeling start from O mark
  theme_bw() + # set background to white
  theme(axis.text = element_text(size = 8), # set axis text size 
        axis.text.y = element_text(size = 8)) + # set axis text size on y-axis
  theme(legend.position = "none") + # show no legend
  theme(panel.grid.major = element_blank(), # remove major grid lines in figure
        panel.grid.minor = element_blank()) + # remove minor grid lines in figure
  theme(axis.title.x = element_text(size = 10, face = "bold"), # set x-axis title text size and format
        axis.title.y = element_text(size = 10, face = "bold")) # set y-axis title text size and format

## Italicize species names

Densityplot_italicized <- Densityplot + theme(axis.text.y = element_text(face = c(rep("italic", 4)))) # italicize species names on y-axis

## Export Figure as JPG

jpeg("~/Desktop/Figure 7.jpg",width = 1750, height = 2700, units = "px",res = 500, bg = "white",pointsize = 8)
Densityplot_italicized # save the figure object in jpeg format
dev.off()

##***********************************************************
## Test for modality of each species' distribution ----------
##***********************************************************

is.unimodal(B_digitata_unimod_test$size)

is.multimodal(C_nitida_unimod_test$size)
is.bimodal(C_nitida_unimod_test$size)

is.multimodal(G_vivans_unimod_test$size)

is.unimodal(B_bulloides_unimod_test$size)

is.unimodal(G_falconensis_unimod_test$size)

is.multimodal(G_adamsi_unimod_test$size)

is.multimodal(G_calida_unimod_test$size)

is.unimodal(G_siphonifera_unimod_test$size)

is.unimodal(G_glutinata_unimod_test$size)

is.unimodal(G_uvula_unimod_test$size)

is.unimodal(G_conglobatus_unimod_test$size)

is.unimodal(G_elongatus_unimod_test$size)

is.unimodal(G_ruber_unimod_test$size)

is.unimodal(G_conglomerata_unimod_test$size)

is.unimodal(G_crassaformis_unimod_test$size)

is.unimodal(G_hirsuta_unimod_test$size)

is.unimodal(G_inflata_unimod_test$size)

is.multimodal(G_menardii_unimod_test$size)

is.unimodal(G_scitula_unimod_test$size)

is.unimodal(G_trucatulinoides_unimod_test$size)

is.unimodal(G_tumida_unimod_test$size)

is.multimodal(G_ungulata_unimod_test$size)

is.multimodal(G_hexagonus_unimod_test$size)

is.unimodal(G_rubescens_unimod_test$size)

is.unimodal(G_tenella_unimod_test$size)

is.unimodal(H_pelagica_unimod_test$size)

is.multimodal(N_dutertrei_unimod_test$size)

is.unimodal(N_incompta_unimod_test$size)

is.multimodal(N_pachyderma_unimod_test$size)
is.bimodal(N_pachyderma_unimod_test$size)

is.multimodal(O_universa_unimod_test$size)

is.unimodal(P_obliquiloculata_unimod_test$size)

is.multimodal(S_dehiscens_unimod_test$size)

is.unimodal(T_iota_unimod_test$size)

is.unimodal(G_sacculifer_unimod_test$size)

is.unimodal(T_humilis_unimod_test$size)

is.multimodal(T_quinqueloba_unimod_test$size)


##************************************************************
## Figure 8 (Test for Optimum Size–Hypothesis, 0SH) ----------
##************************************************************

## Load Data::Optimum Size-Hypothesis

Opt_size_hyp_test <- read.csv("Opt_size_hyp_test_data.csv", header = TRUE, sep = ',')

## Plot Figure

## Single-species robust regression analyses with p-values corrected for multiple testing

OSH_Exp <- ggplot(Opt_size_hyp_test, # specify data object
                   aes(x = Sp_rel_abund, y = Size)) + # specify columns for x and y axes
  geom_point() + # Add data points
  facet_wrap(~Species, # create separate regression analysis on a per species basis (i.e., grouping by species)
             scales = "free_x", # base the x-axis value range of each plot on the relative abundance data for that species
             shrink = T, # shrink scales to fit output of statistics, not raw data
             as.table = T) + # permit facet layout as a table 
  stat_smooth(method = "rlm", # fit robust linear model 
              col = "#000000", # set regression line color
              se = T, # show confidence interval
              size = 0.7, # set size of the regression line
              fullrange = T) + # let the regression line go from end-to-end
  theme_bw() + # set background to white
  theme(strip.text.x = element_text(size = 20, face = "italic")) + # set text size and format of x-axis text 
  expand_limits(x = 0, y = 0) + # set plot to start from 0 (and not the lowest relative abundance or size values per species) on both x and y axes
  scale_x_continuous(limits = c(0, NA), # set x-axis values lower limit to 0 and max to be as present in the data per species
                     expand = expansion(mult = c(0, NA))) + # add some padding around the data to ensure that they are placed some distance away from the axes
  theme(panel.spacing = unit(2.3, "lines")) + # set panel spacing
  theme(panel.grid.major = element_blank(), # remove major grid lines in figure
        panel.grid.minor = element_blank()) + # remove minor grid lines in figure
  theme(axis.text = element_text(size = 20, colour = "black"), # set axis text size and color
        axis.title = element_text(size = 24, colour = "black", face = "bold")) + # set axis title size, color, and format
  guides(size = "none") + # do not set any value to the size scale
  labs(x = expression("Relative Abundance (%)"), y = expression("Size (µm)")) # add x and y axis titles

jpeg("~/Desktop/Figure 8.jpg", width = 15000, height = 10500, units = "px",res = 700, bg = "white", pointsize = 8)
OSH_Exp # save the figure object in jpeg format
dev.off()

## p-value correction for multiple testing

alpha <- (0.025) # Set Bonferroni alpha 

bonfer_test$bonferroni_sig_2 <- p.adjust(bonfer_test$pvalue, # load the initial p-values for each species' regression fit
                                         method = "bonferroni") < alpha # Determine whether p-value is significant at alpha level after bonferroni correction


##************************************************
## Figure 9 (Species-specific Response) ----------
##************************************************

## Load Data:: Species specific response to environmental parameters

species_specific_response <- read.csv("species_specific_response_data.csv", header = TRUE, sep = ',')

## Plot Figure

species_specific_response_plot <- ggplot(species_specific_response_primary_copy_2, # specify data object
                                         aes(y = r2rlm_1, axis1 = Parameter, axis2 = Species)) + # specify the column that determine the width of the curves, and the columns for the axes on the left and right
  geom_alluvium(aes(fill = Parameter), # set reference column to start the flow on the left
                aes.bind = "flow", # set connection type
                curve_type = "arctangent", # set curve type
                width = 1/4) + # set the width of each stratum, as a proportion of the distance between axes
  geom_stratum(width = 1/4, # plots rectangles for each stratum based on specified width
               fill = "black", # adds background color to the rectangle in each stratum
               color = "white") + # keeps a white color outline around each rectangle
  geom_text(stat = "stratum", # set statistical transformation to use on the data for this layer
            aes(label = after_stat(stratum)), # set aesthetic mapping. This means labels are to be added to each stratum
            size = 5, # set font size
            fontface = "italic", # set font format
            colour = "white", # set font color
            check_overlap = TRUE) + # checks for text overlaps, if present, text in the same layer will not be plotted
  scale_x_discrete(limits = c("Parameter", "Species"), # set vectors to be used to define possible values of the scale and their order
                   expand = c(.1, .1)) + # add some padding around the data to ensure that they are placed some distance away from the axes
  scale_fill_brewer(type = "qual", palette = "Set1") + # set color palette for the flows 
  theme_bw() + # set background to white
  theme(panel.grid.major = element_blank(), # remove major grid lines in figure
        panel.grid.minor = element_blank()) + # remove minor grid lines in figure
  theme(axis.text.y = element_blank()) + # remove axis texts on y-axis
  theme(axis.ticks.y = element_blank()) + # remove axis ticks on y-axis
  theme(axis.text.x = element_text(size = 22, colour = "black")) + # set x-axis text size and color
  theme(axis.title=element_blank()) + # remove axis title 
  theme(plot.title = element_text(size = 23)) + # set text size of plot title
  theme(legend.text = element_text(size = 22), # set text size of legend texts 
        legend.title = element_text(size = 23)) + # set text size of legend title 
  guides(fill = guide_legend(title = "Parameters")) + # add legend title
  ggtitle("Species-specific Response to Environmental Variables") # add plot title

## Export Figure as JPEG

jpeg("~/Desktop/Figure 9.jpg", width = 9500, height = 7500,units = "px",res = 700,bg = "white", pointsize = 8)
species_specific_response_plot # save the figure object in jpeg format
dev.off()

##*********************************************************************
## Figure 10 Size vs Species Richness and Species Diversity) ----------
##*********************************************************************

## Load Data::Size vs Species Richness, Size vs Species Diversity

All_core_data <- read.csv("All_core_site_data.csv", header = TRUE, sep = ',')

## Plot Figures

## (a) Size vs Species Richness

Size_vs_species_richness <- ggplot(All_core_site_data, # specify data object
                                     aes(species_richness,size)) + # specify columns for x and y axes
    geom_point(aes(color = Depth, shape = Region), size = 5) + # color points by depth and set shape based on region
    geom_smooth(method="rlm", aes(colour="rlm"), # fit robust linear model
                se=T,  # show confidence interval
                size = .5, # set size of regression line
                color = 'black')  + # set color of regression line
    scale_color_gradientn(colours = c("#d7191c", "#fdae61", "#abd9e9", "#004488")) + # set color gradient for depth manually
    scale_shape_manual(values = c(15,16,17,18)) + # set shape for region manually
    theme_bw() + # set background to white
    theme(panel.grid.major = element_blank(), # remove major grid lines in figure
          panel.grid.minor = element_blank()) + # remove minor grid lines in figure
    theme(axis.text=element_text(size = 20, colour = "black"), # set axis text size and color
          legend.position = "none", # show no legend 
          axis.title=element_text(size=22, colour = "black", face = "bold")) + # set axis title size, color, and format
    guides(shape = guide_legend(override.aes = list(size = 6))) + # override default legend text size for "region" and set preferred text size 
    annotate("text", x = 18, y = 850, size = 7, colour ="black", # specify position, size, and color of text to be annotated
             label = "italic(R)^{2}==0.013*','~italic(p)==0.546", parse = TRUE) + # add the text to be annotated
    labs(y = expression(Size[95]["/"][5]~(µm)), x = expression("Species Richness")) # add x and y axis titles

## (b) Size vs Species Diversity

  Size_vs_species_diversity <- ggplot(All_core_site_data, # specify data object
                                     aes(species_diversity,size)) + # specify columns for x and y axes
    geom_point(aes(color = Depth, shape = Region), size = 5) + # color points by depth and set shape based on region
    geom_smooth(method="rlm", aes(colour="rlm"), # fit robust linear model
                se=T, # show confidence interval
                size = .5, # set size of regression line
                color = 'black') + # set color of regression line
    scale_color_gradientn(colours = c("#d7191c", "#fdae61", "#abd9e9", "#004488"), # set color gradient for depth manually
                          name = "Depth (m)") + # set legend title 
    scale_shape_manual(values = c(15,16,17,18)) + # set shape for region manually
    theme_bw() + # set background to white
    theme(panel.grid.major = element_blank(), # remove major grid lines in figure
          panel.grid.minor = element_blank()) + # remove minor grid lines in figure
    theme(axis.text = element_text(size = 20, colour = "black"), # set axis text size and color
          axis.title=element_text(size=22, colour = "black", face = "bold")) + # set axis title size, color, and format
    theme(legend.position = "right", legend.text = element_text(size = 20), # set legend position and legend text size
          legend.title = element_text(size = 22)) + # set legend title size
    guides(shape = guide_legend(override.aes = list(size = 6))) + # override legend text size for "region" and set preferred text size 
    annotate("text", x = 1.65, y = 820, size = 7, colour ="black", # specify position, size, and color of text to be annotated
              label = "italic(R)^{2}==0.49*','~italic(p)==6.43e-09", parse = TRUE) + # add the text to be annotated
    labs(y = NULL, x = expression("Species Diversity (Shannon–Weiner Index)")) # add x-axis title
    
## Export Figure as JPG
  
jpeg("~/Desktop/Figure 10.jpg", width = 10000, height = 3700, units = "px", res = 600, bg = "white", pointsize = 8)
ggarrange(ggarrange(Size_vs_species_diversity,Size_vs_species_richness)) # save and arrange the figure object in jpeg format
dev.off() 

##*******************************************************************
## Figure 11 (Contribution of Large vs Small Size Species) ----------
##*******************************************************************

## Load Data:: Contribution of Large vs Small Size Species relative to the Regional Size 95

Proxy_for_large_vs_small_size_data <- read.csv("Proxy_for_large_vs_small_size.csv", header = TRUE, sep = ',') 

## Plot Figure

Perc_greater_than_reg_size95 <- ggplot(Proxy_for_large_vs_small_size, # specify data object
                                       aes(x = abundance, y = size, # specify columns for x and y axes
                                           size = Percentage_greater_than_regional_size95)) + # specify column to be used to scale bubble size
  geom_point(alpha = 0.93, # set transparency
             stat = "identity" , # set statistical transformation to use on the data for this layer
             aes(color = Region)) + # set column to be used to group by color 
  scale_shape_manual(values = c(15, 16, 17, 18)) + # set shape manually
  scale_color_manual(values = c("#b067a3", "#9c954d", "#bc7d39", "#697ed5")) + # set color for "region" manually
  theme_bw() + # set background to white
  theme(panel.grid.major = element_blank(), # remove major grid lines in figure
        panel.grid.minor = element_blank()) + # remove minor grid lines in figure
  theme(axis.text = element_text(size = 22, colour = "black"), # set axis text size and color
        axis.title = element_text(size = 24, colour = "black", face = "bold")) + # set axis title size, color, and format
  theme(legend.text = element_text(size = 22), # set legend text size
        legend.position = "right") + # set legend position
  theme(legend.title = element_text(size = 24), # set legend title size
        legend.position = "right") + # set legend position
  guides(color = guide_legend(override.aes = list(size = 8))) + # override legend text size for "region" and set preferred text size 
  scale_size(range = c(3, 18), # scale bubble size in legend  
             name = "Percentage (> Regional Size     )") + # add legend title for the bubble list in legend (note: space after "Regional Size" is intentional)
  labs(y = expression(Size[95]["/"][5]~(µm)), x = expression("Abundance (Total number of forams)")) # add x and y axis titles
  
## Export Figure as JPEG

png("~/Desktop/Figure 11.png", width = 7800, height = 4400, units = "px", res = 600, bg = "white", pointsize = 8)
Perc_greater_than_reg_size95 # save the figure object in jpeg format
dev.off()

##*****************************************************************
## Figure 12 (Size Factor 1 & 2 Scores vs CO32- and SST) ----------
##*****************************************************************

## Load Data(a & b):: Size F1 Scores vs Surface Carbonate concentration and Size F2 Scores vs 2nd Order Polynomial Fit with SST 

All_core_data <- read.csv("All_core_site_data.csv", header = TRUE, sep = ',')

## (a) Size F1 Scores vs surface carbonate concentration

F1size_vs_surf_carb <- ggplot(All_core_site_data, # specify data object
                              aes(x = surf_carb, y = F1size)) + # specify columns for x and y axes
  geom_point(aes(colour = Region, shape = Region), # set column to be used to group by color and shape
             size = 9) + # size point size
  stat_smooth(method = "rlm", formula = y~x, # fit robust linear model
              col = "#000000", # set regression line color
              se = FALSE, # do not show confidence interval
              size = 0.7, # set regression line size
              fullrange = T) + # make regression line go from end to end
  scale_shape_manual(values = c(15, 16, 17, 18)) + # set shape manually
  scale_color_manual(values = c("#b067a3", "#9c954d", "#bc7d39", "#697ed5"), # set color manually
                     guide = guide_legend(override.aes = list(size = 9))) + # override default legend shape size and set new shape size
  theme_bw() + # set background to white
  theme(panel.grid.major = element_blank(), # remove major grid lines in figure
        panel.grid.minor = element_blank()) + # remove minor grid lines in figure
  guides (size = "none") + # do not show any legend for size
  theme(axis.text=element_text(size = 22, colour = "black"), # set axis text size and color
        axis.title=element_text(size=24, colour = "black", face = "bold")) + # set axis title size, color, and format
  theme(legend.text = element_text(size = 24), # set legend text size
        legend.title = element_text(size = 24)) + # set legend title size
  annotate("text", x = 255, y = 1.3, size = 7, colour ="black", # specify position, size, and color of text to be annotated
           label = "italic(R)^{2}==0.41*','~italic(p)==1.18e-07", parse = TRUE) + # add the text to be annotated
  labs(x = expression(Carbonate~conc.[surface] ~ (µm/kg)), y = expression("F1 Axis Scores")) # add x and y axis titles


## (b) Size F2 Scores vs 2nd Order Polynomial Fit with SST

F2size_vs_SST <- ggplot(All_core_site_data, # specify data object
                        aes(x = sst, y = F2size)) + # specify columns for x and y axes
  geom_point(aes(colour = Region, shape = Region), # set column to be used to group by color and shape
             size = 9) + # size point size
  stat_smooth(method = "lm", formula = y~poly(x,2), # fit 2nd order polynomial model
              col = "#000000", # set regression line color
              se = FALSE, # do not show confidence interval
              size = 0.7, # set regression line size
              fullrange = T) + # make regression line go from end to end
  scale_shape_manual(values = c(15, 16, 17, 18)) + # set shape manually
  scale_color_manual(values = c("#b067a3", "#9c954d", "#bc7d39", "#697ed5"), # set color manually
                     guide = guide_legend(override.aes = list(size = 9))) + # override default legend shape size and set new shape size
  theme_bw() + # set background to white
  theme(panel.grid.major = element_blank(), # remove major grid lines in figure
        panel.grid.minor = element_blank()) + # remove minor grid lines in figure
  theme(axis.text=element_text(size = 22, colour = "black"), # set axis text size and color
        axis.title=element_text(size=24, colour = "black", face = "bold")) +  # set axis title text size, color, and format
  theme(legend.text = element_text(size = 22), # set legend text size
        legend.title = element_text(size = 24)) + # set legend title text size
  guides(size = "none") + # do not show any legend for size
  annotate("text", x = 23, y = 1.6, size = 7, colour = "black", # specify position, size, and color of text to be annotated
           label = "italic(R)^{2}==0.46*','~italic(p)==1.248e-08", parse = TRUE) + # add the text to be annotated
labs(x = expression("SST ("*degree*"C)"), y = expression("F2 Axis Scores")) # add x and y axis titles

## Export Figures as JPG

jpeg("~/Desktop/Figure 12.jpg", width = 15000, height = 5000, units = "px", res = 600, bg = "white", pointsize = 8)
ggarrange(ggarrange(F1size_vs_surf_carb,F2size_vs_SST, ncol = 2)) # save and arrange the figure object into 2 columns and in jpeg format
dev.off() 

#******************************************************************
#Figure S3 in Supplementary Information 1 (Data + Code) ---------
#******************************************************************

#Load Data:: Rillo et al Size for 9 Species (Equatorial Indian Ocean Samples only) vs SST 

pobliqrill <- read.csv("pobliqrillo.csv", header = TRUE, sep = ',')
gmenrill <- read.csv("gmenrillo.csv", header = TRUE, sep = ',')
ndurtrill <- read.csv("ndurtrillo.csv", header = TRUE, sep = ',')
grubrill <- read.csv("grubrillo.csv", header = TRUE, sep = ',')
gsacrill <- read.csv("gsacrillo.csv", header = TRUE, sep = ',')
gsiphonrill <- read.csv("gsiphonrillo.csv", header = TRUE, sep = ',')
gconglobarill <- read.csv("gconglobarillo.csv", header = TRUE, sep = ',')
gtruncrill <- read.csv("gtruncrillo.csv", header = TRUE, sep = ',')
ginflacrill <- read.csv("ginflarillo.csv", header = TRUE, sep = ',')

#Plot Figures

Rillo_PO <- ggplot(pobliqrill, aes(x=sst, y=size)) + 
  geom_point() +
  facet_wrap(~species, scales = "free_x") + 
  stat_smooth(method = "rlm",col = "#000000",se = T, size = 0.7, fullrange = T) + 
  theme_bw() + 
  theme(strip.text.x = element_text(size = 20, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size = 20, colour = "black"), 
        axis.title=element_text(size=22, colour = "black", face = "bold")) +
  labs(x = NULL, y = expression("Size (µm)")) 

Rillo_GMen <- ggplot(gmenrill, aes(x=sst, y=size)) + 
  geom_point() +
  facet_wrap(~species, scales = "free_x") + 
  stat_smooth(method = "rlm",col = "#000000",se = T, size = 0.7, fullrange = T) + 
  theme_bw() + 
  theme(strip.text.x = element_text(size = 20, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size = 20, colour = "black"), 
        axis.title=element_text(size=22, colour = "black", face = "bold")) +
  labs(x = NULL, y = NULL) 

Rillo_NDurt <- ggplot(ndurtrill, aes(x=sst, y=size)) + 
  geom_point() +
  facet_wrap(~species, scales = "free_x") + 
  stat_smooth(method = "rlm",col = "#000000",se = T, size = 0.7, fullrange = T) + 
  theme_bw() + 
  theme(strip.text.x = element_text(size = 20, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size = 20, colour = "black"), 
        axis.title=element_text(size=22, colour = "black", face = "bold")) +
  labs(x = NULL, y = NULL)

Rillo_GRub <- ggplot(grubrill, aes(x=sst, y=size)) + 
  geom_point() +
  facet_wrap(~species, scales = "free_x") + 
  stat_smooth(method = "rlm",col = "#000000",se = T, size = 0.7, fullrange = T) + 
  theme_bw() + 
  theme(strip.text.x = element_text(size = 20, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size = 20, colour = "black"), 
        axis.title=element_text(size=22, colour = "black", face = "bold")) +
  labs(x = NULL, y = expression("Size (µm)")) 

Rillo_GSac <- ggplot(gsacrill, aes(x=sst, y=size)) + 
  geom_point() +
  facet_wrap(~species, scales = "free_x") + 
  stat_smooth(method = "rlm",col = "#000000",se = T, size = 0.7, fullrange = T) + 
  theme_bw() + 
  theme(strip.text.x = element_text(size = 20, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size = 20, colour = "black"), 
        axis.title=element_text(size=22, colour = "black", face = "bold")) +
  labs(x = NULL, y = NULL) 

Rillo_GSiphon <- ggplot(gsiphonrill, aes(x=sst, y=size)) + 
  geom_point() +
  facet_wrap(~species, scales = "free_x") + 
  stat_smooth(method = "rlm",col = "#000000",se = T, size = 0.7, fullrange = T) + 
  theme_bw() + 
  theme(strip.text.x = element_text(size = 20, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size = 20, colour = "black"), 
        axis.title=element_text(size=22, colour = "black", face = "bold")) +
  labs(x = NULL, y = NULL) 

Rillo_GCongloba  <- ggplot(gconglobarill, aes(x=sst, y=size)) + 
  geom_point() +
  facet_wrap(~species, scales = "free_x") + 
  stat_smooth(method = "rlm",col = "#000000",se = T, size = 0.7, fullrange = T) + 
  theme_bw() + 
  theme(strip.text.x = element_text(size = 20, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size = 20, colour = "black"), 
        axis.title=element_text(size=22, colour = "black", face = "bold")) +
  labs(x = expression("SST (°C)"), y = expression("Size (µm)")) 

Rillo_GTrunc <- ggplot(gtruncrill, aes(x=sst, y=size)) + 
  geom_point() +
  facet_wrap(~species, scales = "free_x") + 
  stat_smooth(method = "rlm",col = "#000000",se = T, size = 0.7, fullrange = T) + 
  theme_bw() + 
  theme(strip.text.x = element_text(size = 20, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size = 20, colour = "black"), 
        axis.title=element_text(size=22, colour = "black", face = "bold")) +
  labs(x = expression("SST (°C)"), y = NULL) 

Rillo_GInfla <- ggplot(ginflacrill, aes(x=sst, y=size)) + 
  geom_point() +
  facet_wrap(~species, scales = "free_x") + 
  stat_smooth(method = "rlm",col = "#000000",se = T, size = 0.7, fullrange = T) + 
  theme_bw() + 
  theme(strip.text.x = element_text(size = 20, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size = 20, colour = "black"), 
        axis.title=element_text(size=22, colour = "black", face = "bold")) +
  labs(x = expression("SST (°C)"), y = NULL) 


#Arrange figures into columns and rows

Rillo_Analyzed <- ggarrange(Rillo_PO,Rillo_GMen,Rillo_NDurt,Rillo_GRub, Rillo_GSac, Rillo_GSiphon, Rillo_GCongloba,Rillo_GTrunc, Rillo_GInfla, ncol = 3, nrow = 3)

#Export Figure as EPS

cairo_ps(filename='Rillo Analyzed Plot.eps', width=18, height=16, family = "Arial",
         pointsize = 8, fallback_resolution = 500)
Rillo_Analyzed
dev.off()

#Export Figure as JPEG

jpeg("~/Desktop/Rillo Analyzed Plot18122022.jpg",width=6000,height=5000,units="px",res=500,bg="white", pointsize = 8)
Rillo_Analyzed
dev.off()

#******************************************************************
#Figure S4 in Supplementary information S4 (Data + Code) ---------
#******************************************************************

#Load Data:: Inflata size vs SST

infalata_vs_sst_data <- read.csv("infalata_vs_sst.csv", header = TRUE, sep = ',')

#Plot Figure

inflata_vs_sst_plot <- ggscatter(infalata_vs_sst_data, x = "infla", y = "sst", 
                                 add = "reg.line", conf.int = TRUE, size = 1,
                                 cor.coef = F, cor.coeff.args = list(method = "pearson", label.x.npc = "middle", label.y.npc = "top"), add.params = list(color = "black", fill = "lightgray", size = 0.5),
                                 xlab = ("Size (µm) "), ylab = "Sea surface temperature (°C)") +
  annotate("text", label = "R2 = – 0.34, p < 0.05", x = 350, y = 30, size = 4, colour ="black") 

#Export Figure as EPS

cairo_ps(filename='inflata_vs_sst_plot.eps', width=18, height=16, family = "Arial",
         pointsize = 8, fallback_resolution = 500)
inflata_vs_sst_plot
dev.off()

#Export Figure as PNG

png("~/Desktop/inflata_vs_sst_plot05052022.png",width=2000,height=1800,units="px",res=400,bg="white", pointsize = 8)
inflata_vs_sst_plot
dev.off()

#**************************************************************************************
# Supplementary Information S5a (Influence of assemblage distribution on size) --------
#**************************************************************************************

#Load Data::Influence of assemblage distribution on size (data only shown for the 13 most abundant species in the Equatorial Indian Ocean)

stacked_area_plot_ordered_13_species_data <- read.csv("stacked_area_plot_ordered_13_species.csv", header = TRUE, sep = ',') 

#Re-order the data according to core location (i.e. Arabian Sea, Bay of Bengal, Central Indian Ocean, and Mozambique Channel)

stacked_area_plot_ordered_13_species_data_one$Coreids <- factor(stacked_area_plot_ordered_13_species_data_one$Coreid, 
                                                            levels = c( "MD96-2045", "MD96-2044", "MD79-276","MD79-261","MD79-260", "MD76-011", 
                                                                        "MD90-0936","MD90-0938",  "MD90-0939", "MD90-0940", "MD96-2049", "MD79-257",
                                                                        "MD96-2051", "MD96-2053", "MD96-2054", "MD96-2055", "MD96-2056", "MD96-2067a", 
                                                                        "MD96-2067b", "MD96-2067b2","MD96-2058", "MD96-2059", "MD96-2066", "MD96-2065",
                                                                        "MD96-2061", "MD96-2063", "MD96-2064","MD96-2060", "MD98-2165", "BARP9442"))  
#Set color pallette

Paired <- colorRampPalette(brewer.pal('Paired',n=12), alpha = FALSE)

#Plot Figure

stacked_area_plot_ordered_13_species_plot <- ggplot(stacked_area_plot_ordered_13_species_data_one, 
                                                    aes(x= size, fill=Species)) +
  geom_area(aes(y = stat(width*density*25)),  stat = "bin") + 
  scale_fill_manual(values = setNames(Paired(13), levels(stacked_area_plot_ordered_13_species_data_one$Species))) + 
  facet_wrap(~Coreids, nrow = 6, ncol = 5) + 
  scale_x_continuous (limits=c(150,1300), breaks = seq(150, 1300, by = 250)) + 
  geom_vline(aes(xintercept = size95), col="black") +
  geom_vline(aes(xintercept = Norm_Mode), col="black", linetype = "longdash") +
  theme(legend.position = 'right') + 
  theme_bw() + 
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text.x = element_text(angle=90,hjust=1, size=15)) + 
  theme(axis.text.y = element_text(size=15)) + 
  theme(axis.title = element_text(size=18)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 
  theme(legend.text = element_text(face = c(rep("italic", 5)))) + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 18)) + 
  theme(legend.text = element_text(size = 15)) + 
  xlab("Size (μm)") +
  ylab("Density")


#Export Figure as JPEG

jpeg("~/Desktop/stacked_area_plot_ordered_exp_04012023.jpg",width=8500,height=7181,units="px",res=600,bg="white", pointsize = 8)
stacked_area_plot_ordered_13_species_plot
dev.off()

#**************************************************************************************
# Supplementary Information S5b (Influence of assemblage distribution on size) --------
#**************************************************************************************

#Load Data::Influence of assemblage distribution on size (data only shown for the 13 most abundant species in the Equatorial Indian Ocean)

stacked_area_plot_ordered_13_species_data <- read.csv("stacked_area_plot_ordered_13_species.csv", header = TRUE, sep = ',') 

#Re-order the data according to core location (i.e. Arabian Sea, Bay of Bengal, Central Indian Ocean, and Mozambique Channel)

stacked_area_plot_ordered_13_species_data_two$Coreids <- factor(stacked_area_plot_ordered_13_species_data_two$Coreid, 
                                                            levels = c("BARP9441",  "BARP9439",  "BARP9437", "MD12-3423",  "BARP9412", "BARP9411",
                                                                       "MD77-160 ","BARP9409", "BARP9430", "BARP9422", "BARP9426", "MD77-171 ",
                                                                       "MD77-185", "MD76-133",  "MD77-182 ", "MD90-0949",  "MD90-0955", "MD90-0963", 
                                                                       "MD90-0959", "MD90-0958","MD90-0961", "MD90-0960", "MD90-0956", "MD90-0957",
                                                                       "MD76-132", "MD77-205", "MD77-204","MD77-202", "MD04-2873", "MD04-2877", "MD04-2875B")) 

#Set color pallette

Paired <- colorRampPalette(brewer.pal('Paired',n=12), alpha = FALSE)

#Plot Figure

stacked_area_plot_ordered_13_species_plot_two <- ggplot(stacked_area_plot_ordered_13_species_data_two, 
                                                    aes(x= size, fill=Species)) +
  geom_area(aes(y = stat(width*density*25)),  stat = "bin") + 
  scale_fill_manual(values = setNames(Paired(13), levels(stacked_area_plot_ordered_13_species_data_two$Species))) + 
  facet_wrap(~Coreids, nrow = 7, ncol = 5) + 
  scale_x_continuous (limits=c(150,1300), breaks = seq(150, 1300, by = 250)) + 
  geom_vline(aes(xintercept = size95), col="black") +
  geom_vline(aes(xintercept = Norm_Mode), col="black", linetype = "longdash") +
  theme(legend.position = 'right') + 
  theme_bw() + 
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text.x = element_text(angle=90,hjust=1, size=15)) + 
  theme(axis.text.y = element_text(size=15)) + 
  theme(axis.title = element_text(size=18)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 
  theme(legend.text = element_text(face = c(rep("italic", 5)))) + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 18)) + 
  theme(legend.text = element_text(size = 15)) + 
  xlab("Size (μm)") +
  ylab("Density")

#Export Figure as JPEG

jpeg("~/Desktop/stacked_area_plot_ordered_exp_two_04012023.jpg",width=8500,height=7181,units="px",res=600,bg="white", pointsize = 8)
stacked_area_plot_ordered_13_species_plot_two
dev.off()

