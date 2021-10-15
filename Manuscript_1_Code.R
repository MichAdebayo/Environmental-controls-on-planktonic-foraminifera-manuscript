#**************************
# PhD First Manuscript Code 
#**************************
#
# Code related to the paper: 
#
#************************************************************************************************************************
# Manuscript Title: Environmental Controls on Size Distribution of Planktonic Foraminifera in the Equatorial Indian Ocean
#************************************************************************************************************************
#
# Author: Michael Adebayo
#
# Date: ... 14th April, 2021 
#*****************************

#***********************************************************************
# Packages -------------------------------------------------------------
#***********************************************************************

library("ggplot2")
library("ggpubr")
library("MASS")
library("vegan")
library("dplyr")
library("tidyverse")
library("hrbrthemes")
library("viridis")
library("forcats")
library("plotly")
library("ggrepel")
library("ggridges")
library("grid")
library("scales")
library("ggforce")
library("ggalluvial")
library("lintr")
library("cairo")
library("extrafonts")

#******************************************************************
# Figure 3b (Data + Code) -----------------------------------------
#******************************************************************

# Load Data:: Human vs Machine (Total Individual and Mean Species Count)

Human_vs_Machine_1 <- read.csv("Human_vs_machine_total_individual_species_count.csv", header = TRUE, sep = ',')
Human_vs_Machine_2 <- read.csv("Human_vs_machine_total_mean_species_count.csv", header = TRUE, sep = ',')

# Plot Figure

Human_vs_Machine <- ggplot(NULL, aes(Total_Machine_Count, Total_Human_Count)) + 
                    geom_point(data = Human_vs_Machine_1, 
                              aes(color = Species, shape = Species)) + 
                    geom_abline(intercept = 0, slope = 1, color = 'gray', size = .8) + 
                    scale_color_viridis_d(aesthetics = "colour") + scale_shape_manual(values = 1:21) + 
                    scale_y_log10() + scale_x_log10() + 
                    geom_point(data = Human_vs_Machine_2, 
                              aes(x=Mean_Machine_Count, y = Mean_Human_Count,alpha = 0.5, size = Mean, color = 'black')) +
                    scale_size_continuous(range = c(3,15), name = 'Mean Count (Upper Limit)') +
                    guides(alpha = FALSE, color = FALSE, size = guide_legend(override.aes = list(colour = "orchid4"))) + 
                    theme_bw() + 
                    theme(legend.position= "right") + 
                    theme(axis.line  = element_line(colour = "black",size=0), panel.border = element_rect(colour = "black", fill=NA, size=1),panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
                    geom_text_repel(data = Human_vs_Machine2,
                                    aes(MeanM, MeanH, label = Species),segment.color = 'black', fontface = 'italic', size = 6.2, force = 3.5,point.padding = 0.2, min.segment.length = 0.8, box.padding = 1) + 
                    theme(legend.text = element_text(face = "italic", size = 14)) + 
                    guides(shape = guide_legend(override.aes = list(size = 5))) + 
                    theme(legend.title = element_text(face = "bold", size = 14))

# Add and Customize Axis Titles

Human_vs_Machine_exp  <- Human_vs_Machine + theme(axis.text=element_text(size = 12, colour = "black"),
                         axis.title=element_text(size=14, colour = "black", face = "bold")) + 
                         annotate("text", label = "Total number of forams analysed (n= 127,097)", x =60, y = 2700, size = 5, colour ="grey40") + 
                         annotate("text", label = "1:1", x =2700, y = 2000, size = 5, colour ="grey40") + 
                         labs(y = "Number Counted by Human Recognition", x = "Number Counted by CNN Recognition")

# Export Figure as EPS

cairo_ps(filename='Human_vs_machine_total_individual_and_mean_species_count.eps', width=18, height=16, family = "Arial",
         pointsize = 8, fallback_resolution = 500)
Human_vs_Machine_exp
dev.off()

# Export Figure as PNG

png("~/Desktop/Human_vs_machine_total_individual_and_mean_species_count.png",width=5000,height=4000,units="px",res=600,bg="white", pointsize = 8)
Human_vs_Machine_exp
dev.off()


#******************************************************************
# Figure 4 (Data + Code) -----------------------------------------
#******************************************************************

# Load Data:: Density Plot of Species Size

size_density <- read.csv("Size_density_plot.csv", header = TRUE, sep = ',')

# Plot Figure

Densityplot <- ggplot(size_density, 
                      aes(x = size, y = species, fill = species)) + 
               geom_density_ridges(alpha=0.25) + 
               scale_x_continuous(name = "Size (μm)", limits = c(0, 1300), expand = c(0,0)) + 
               scale_y_discrete(name = "Species", expand = c(0, 0)) + 
               theme_bw()+
               theme(axis.text = element_text(size=8), axis.text.y = element_text(size = 8)) + 
               theme(legend.position = "none") +
               theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
               theme(axis.title.x = element_text(size = 10, face = "bold"), axis.title.y = element_text(size = 10, face = "bold"))

# Italicize species names

Densityplot_italicized <- Densityplot + theme(axis.text.y = element_text(face = c(rep("italic", 4))))

# Export Figure as EPS

cairo_ps(filename='Density Plot_2_v2.eps', width=18.75,height=29.2,family = "Arial",
         pointsize = 16, fallback_resolution = 500)
Densityplot_italicized
dev.off()

# Export Figure as PNG

png("~/Desktop/Density Plot_2_v2.png",width=1400,height=2600,units="px",res=350, bg="white",pointsize = 8)
Densityplot_italicized
dev.off()


#******************************************************************
# Figure 5a (Data + Code) -----------------------------------------
#******************************************************************

# Load Data:: Relative Abundance (This study) vs Relative Abundance (ForCens)

Relative_abundance_comparison_vs_ForCens <- read.csv("Relative_abundance_comparison_with_ForCens.csv", header = TRUE, sep = ',')

# Plot Figure

Relative_Abundance_Comparison <- ggplot(Relative_abundance_comparison_vs_ForCens, 
                                        aes(x=relative_abundance_in_ForCens_database,y=relative_abundance_from_machine_count)) + 
                                 geom_point(aes(color = Species, shape = Species), size = 3) + 
                                 scale_x_log10(limits= c(0.1,100)) + scale_y_log10(limits=c(0.1,100)) +
                                 scale_color_viridis_d(aesthetics = "colour") + 
                                 scale_shape_manual(values = 1:21) +
                                 theme_bw() + 
                                 theme(legend.position = "right") + 
                                 theme(axis.line  = element_line(colour = "black",size=0), 
                                       panel.border = element_rect(colour = "black", fill=NA, size=1),panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
                                 theme(legend.text = element_text(face = "italic", size = 14)) + 
                                 guides(color = guide_legend(ncol = 1, bycol = FALSE)) + 
                                 guides(shape = guide_legend(override.aes = list(size = 5))) + 
                                 theme(legend.title = element_text(face = "bold", size = 14))

# Add 1:1 Line

Relative_Abundance_Comparisons <- Relative_Abundance_Comparison + geom_abline(intercept = 0, slope = 1, color = 'gray40', size = .6)

# Add and Customize Axis Titles

Relative_Abundance_Comparisons_Exp <- Relative_Abundance_Comparisons + 
                                      theme(axis.text=element_text(size = 13, colour = "black"),
                                            axis.title=element_text(size=16, colour = "black")) + 
                                      annotate("text", label = "R = 0.60, p < 2.2e-16", x =0.3, y = 100, size = 6, colour ="black") + 
                                      annotate("text", label = "1:1", x =95, y = 83, size = 7, colour ="black") + 
                                      labs(y = "Relative Abundance ((This Study) %)", x = "Relative Abundance ((ForCens) %)")


# Export Figure as EPS

cairo_ps(filename='ForCens vs This study plot.eps', width=18, height=16, family = "Arial",
         pointsize = 8, fallback_resolution = 500)
Relative_Abundance_Comparisons_Exp
dev.off()

# Export Figure as PNG

png("~/Desktop/ForCens vs This study plot.png",width=6700,height=6000,units="px",res=500,bg="white", pointsize = 8)
Relative_Abundance_Comparisons_Exp
dev.off()

#******************************************************************
# Figure 5b (Data + Code) -----------------------------------------
#******************************************************************

# Load Data:: Comparison between CNN Size vs Rillo et al. size distribution with error bars (standard deviation)

this_study_size_vs_rillo <- read.csv("size_this_study_vs_rillo_et_al.csv", header = TRUE, sep = ',')

# Plot Figure

# Set sd (Standard Deviation) lower and upper limit for error bar 

ymin = this_study_size_vs_rillo$machine_size - this_study_size_vs_rillo$sd
ymax = this_study_size_vs_rillo$machine_size + this_study_size_vs_rillo$sd

# Preliminary Figure Plot

this_study_size_vs_rillo_plot <- ggplot(this_study_size_vs_rillo, 
                                        aes(rillo_size, machine_size)) + 
                                 geom_point(aes(color = Species, shape = Species), size = 4) + 
                                 scale_color_viridis_d(aesthetics = "colour") + 
                                 scale_shape_manual(values = 1:20) + 
                                 geom_errorbar(aes(ymin=ymin, ymax=ymax)) +
                                 theme_bw() + 
                                 theme(legend.position= "right") + 
                                 theme(axis.line  = element_line(colour = "black",size=0), 
                                       panel.border = element_rect(colour = "black", fill=NA, size=1),panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
                                 theme(legend.text = element_text(face = "italic", size = 13)) + 
                                 guides(shape = guide_legend(override.aes = list(size = 5))) + 
                                 theme(legend.title = element_text(face = "bold", size = 13)) 

# Set Fixed X and Y Coordinates

this_study_size_vs_rillo_plots <- this_study_size_vs_rillo_plot + coord_fixed(ratio = 1, xlim = c(1,1240), ylim = c(1,1240), expand = T, clip = "on")

# Add 1:1 Line

this_study_size_vs_rillo_plots_final <- this_study_size_vs_rillo_plots + geom_abline(intercept = 0, slope = 1, color = 'black', size = .6)

# Add and Customize Axis Titles

this_study_size_vs_rillo_plots_final_exp <- this_study_size_vs_rillo_plots_final + 
                                            theme(axis.text=element_text(size = 13, colour = "black"),
                                                  axis.title=element_text(size=16, colour = "black")) + 
                                            annotate("text", label = "R = 0.89, p < 4.7e-10", x =200, y = 1200, size = 6, colour ="black") + 
                                            annotate("text", label = "1:1", x =1150, y = 1100, size = 6, colour ="black") + 
                                            labs(y = "CNN size (μm)", x = "Buckley collection + Resampled data size (μm)")

# Export Figure as EPS

cairo_ps(filename='CNN vs Rillo with error bars.eps', width=18, height=16, family = "Arial",
         pointsize = 8, fallback_resolution = 500)
this_study_size_vs_rillo_plots_final_exp
dev.off()

# Export Figure as PNG

png("~/Desktop/CNN vs Rillo with error bars.png",width=6700,height=6000,units="px",res=600,bg="white")
this_study_size_vs_rillo_plots_final_exp
dev.off()

#******************************************************************
# Figure 7 (Data + Code) -----------------------------------------
#******************************************************************

# Load Data:: Dissolution Index (Berger and Parker Index, MTC-Index, Size vs Depth, Size vs Carbonate at Core Depth)

All_core_data <- read.csv("All_core_site_data.csv", header = TRUE, sep = ',')

# Plot Figures

# (a) Berger and Parker Dissolution Index (B_and_P_Index) vs Michael-Thibault-Clara Index (MTC_Index)

MTC_Index_vs_B_and_P_Index <- ggplot(All_core_data, 
                                     aes(x=mtc_index, y=bandp_index)) +
                              geom_point(aes(shape = Region, color = Depth), size = 6) +
                              scale_color_gradient(low = "blue", high = "green") + theme_ipsum() +
                              scale_shape_manual(values = c(15,16,17,18)) + 
                              theme(axis.title.y = element_text(size = 13), axis.title.x = element_text(size = 13)) +
                              theme(legend.title = element_text(face = c(rep("bold", 5)), size = 14)) + 
                              ylab("Berger & Parker Index") + 
                              xlab("MTC Index")

# Reverse legend color bar scale

MTC_Index_vs_B_and_P_Exp <- MTC_Index_vs_B_and_P_Index + guides(color = guide_colourbar(reverse = T))


# (b) MTC-Index vs Fragmentation Rate

MTC_Index_vs_Fragmentation_Rate <- ggplot(All_core_data,
                                          aes(mtc_index,fragmentation_rate)) + 
                                   geom_point(aes(shape = Region, color = Region), size = 4) +
                                   geom_smooth(method=lm,se=T, size = .5, color = 'black') +
                                   scale_fill_viridis(discrete=TRUE) + theme_ipsum() + 
                                   theme_bw() + 
                                   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
                                   theme(axis.text=element_text(size = 13, colour = "black"), legend.position = "right",
                                   axis.title=element_text(size=15, colour = "black", face = "bold")) +
                                   annotate("text", label = "R = –0.017, p = 0.89", x = 0.5, y = 45, size = 4.5, colour ="black") + 
                                   theme(legend.title = element_text(face = c(rep("bold", 5)), size = 14)) + 
                                   theme(legend.text = element_text(size = 12)) + 
                                   labs(y =  expression("Fragmentation Rate (%)"), x = expression("MTC-Index"))

# (c) Size vs Depth

Size_vs_Depth <- ggplot(All_core_data,
                        aes(Depth,size)) + 
                 geom_point(aes(shape = Region, color = Region), size = 4) +
                 geom_smooth(method=lm,se=T, size = .5, color = 'black') + 
                 scale_fill_viridis(discrete=TRUE) + theme_ipsum() + 
                 theme_bw() + 
                 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
                 theme(axis.text=element_text(size = 13, colour = "black"), legend.position = "right",
                      axis.title=element_text(size=15, colour = "black", face = "bold")) +
                 annotate("text", label = "R = 0.38, p = 0.0024", x =1500, y = 850, size = 4.5, colour ="black") +
                 theme(legend.title = element_text(face = c(rep("bold", 5)), size = 14)) + 
                 theme(legend.text = element_text(size = 12)) + 
                 labs(y = expression("Size (μm)"), x = expression("Depth (m)"))

# (d) Size vs Carbonate at core depth

Size_vs_Carbonate_at_Core_Depth <- ggplot(All_core_data,
                                          aes(carbonate_at_core_depth,size)) + 
                                   geom_point(aes(shape = Region, color = Region), size = 4) +
                                   geom_smooth(method=lm,se=T, size = .5, color = 'black') +
                                   scale_fill_viridis(discrete=TRUE) + theme_ipsum() + 
                                   theme_bw() + 
                                   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
                                   theme(axis.text=element_text(size = 13, colour = "black"), legend.position = "right",
                                        axis.title=element_text(size=15, colour = "black", face = "bold")) +
                                   annotate("text", label = "R = 0.33, p = 0.0087", x =69, y = 850, size = 4.5, colour ="black") + 
                                   theme(legend.title = element_text(face = c(rep("bold", 5)), size = 14)) + 
                                   theme(legend.text = element_text(size = 12)) + 
                                   labs(y =  expression(Size (μm)), x = expression(Carbonate[Core~depth] ~ (µmol/kg)))

# Export all 4 Figures as a single PNG file

png("~/Desktop/Dissolution Indices Latest_10102021.png",width=10073,height=5351,units="px",res=600,bg="white", pointsize = 8)
ggarrange(MTC_Index_vs_B_and_P_Exp,MTC_Index_vs_Fragmentation_Rate,Size_vs_Depth,Size_vs_Carbonate_at_Core_Depth, nrow= 2, ncol = 2, labels = c("(a)", "(b)", "(c)", "(d)"))
dev.off()

#******************************************************************
# Figure 8 (Data + Code) -----------------------------------------
#******************************************************************

#Load Data::Size vs Species Richness, Size vs Species Diversity, & Temperature at Maximum size vs Temperature at Maximum Relative Abundance

All_core_data <- read.csv("All_core_site_data.csv", header = TRUE, sep = ',')
Temperature_at_max_size_vs_temperature_at_max_relative_abundance <- read.csv("Temp_at_max_size_vs_Temp_at_max_rel_abund.csv", header = T, sep = ',')

#Plot Figures

# (a) Temp. at Maximum size vs Temp. at max relative abundance

Temp_max_size_vs_Temp_max_rel_abund <- ggplot(Temperature_at_max_size_vs_temperature_at_max_relative_abundance, 
                                              aes(y = Temp_at_max_size, x = Temp_at_max_rel_abund)) +
                                       geom_abline(intercept = 19.5, slope = 0.3, color = "grey40", linetype="dashed", size = .5) + 
                                       geom_point(color ="black", size = 1) + 
                                       theme_bw() + 
                                       geom_text_repel(aes(label = Species),segment.color = 'black', fontface = 'italic', size =3, force = .5,point.padding = 0.1, min.segment.length = 0) +      
                                       theme(axis.text=element_text(size = 12, colour = "black"), axis.title=element_text(size=12, colour = "black"),
                                             aspect.ratio = 1) +  annotate("text", label = "R = 0.19, P = 0.04", x =24, y = 23, size = 4, colour ="grey40") + 
                                       labs(y = expression(Temperature~at~Maximum~Size ~ (~degree~C)), 
                                            x = expression("Temperature at Maximum Relative Abundance ("*degree*"C)"))

# (b) Size vs Species Richness

Size_vs_species_richness <- ggplot(All_core_data,
                                   aes(species_richness,size)) + 
                            geom_point(aes(color = Depth), size = 2) +
                            scale_color_gradient(low = "orange", high = "navy") + 
                            geom_smooth(method=lm,se=T, size = .5, color = 'black')  + 
                            theme_bw() + 
                            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
                            theme(axis.text=element_text(size = 11, colour = "black"), legend.position = "none",
                                 axis.title=element_text(size=13, colour = "black", face = "bold")) +
                            annotate("text", label = "R = 0.093, p = 0.47", x = 19, y = 850, size = 4, colour ="black") + 
                            labs(y = expression("Size (μm)"), x = expression("Species Richness"))

# (c) Size vs Species Diversity

Size_vs_species_diversity <- ggplot(All_core_data,
                                    aes(species_diversity,size)) + 
                             geom_point(aes(color = Depth), size = 2) +
                             scale_color_gradient(low = "orange", high = "navy") + 
                             geom_smooth(method=lm,se=T, size = .5, color = 'black')  + 
                             scale_fill_viridis(discrete = T) +
                             theme_bw() + 
                             theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
                             theme(axis.text=element_text(size = 11, colour = "black"), legend.position = "right",
                                  axis.title=element_text(size=13, colour = "black", face = "bold")) +
                             annotate("text", label = "R = 0.65, p = 1.5e-08", x = 1.5, y = 850, size = 4, colour ="black") + 
                             labs(y = NULL, x = expression("Species Diversity (Shannon-Weiner Index)"))


# Export all 3 Figures as a single PNG file

png("~/Desktop/Size_vs_abundance_richness_and_diversity3_10102021.png",width=6000,height=5031,units="px",res=600,bg="white", pointsize = 8)
ggarrange(Temp_max_size_vs_Temp_max_rel_abund,  
          ggarrange(Size_vs_species_richness,Size_vs_species_diversity, ncol = 2, labels = c("(b)", "(c)")), nrow = 2, labels = "(a)")
dev.off() 


#******************************************************************
# Figure 9 (Data + Code) -----------------------------------------
#******************************************************************

# Load Data::Influence of assemblage distribution on size (data only shown for the 13 most abundant species in the Equatorial Indian Ocean)

stacked_area_plot_ordered_13_species_data <- read.csv("stacked_area_plot_ordered_13_species.csv", header = TRUE, sep = ',') 

# Re-order the data according to core location (i.e. Arabian Sea, Bay of Bengal, Central Indian Ocean, and Mozambique Channel)

stacked_area_plot_ordered_13_species_data$Coreids <- factor(stacked_area_plot_ordered_13_species_data$Coreid, 
                                                       levels = c( "MD96-2045", "MD96-2044", "MD79-276","MD79-261","MD79-260", "MD76-011", "MD90-0936", "MD90-0939", "MD90-0940", "MD90-0938",
                                                                   "MD96-2049", "MD79-257", "MD96-2051", "MD96-2053", "MD96-2054", "MD96-2055", "MD96-2056", "MD96-2067a", "MD96-2067b", "MD96-2067b2",
                                                                   "MD96-2058", "MD96-2059", "MD96-2066", "MD96-2065","MD96-2061", "MD96-2063", "MD96-2064","MD96-2060", "MD98-2165", "BARP9442", 
                                                                   "BARP9441",  "BARP9439",  "BARP9437", "MD12-3423",  "BARP9412", "BARP9411",  "MD77-160 ","BARP9409", "BARP9430", "BARP9422", 
                                                                   "BARP9426", "MD77-171 ", "MD77-185", "MD76-133",  "MD77-182 ", "MD90-0949",  "MD90-0955", "MD90-0963", "MD90-0959", "MD90-0958",
                                                                   "MD90-0961", "MD90-0960", "MD90-0956", "MD90-0957", "MD76-132", "MD77-205", "MD77-204","MD77-202", "MD04-2873", 
                                                                   "MD04-2877", "MD04-2875B"))  

# Plot Figure

stacked_area_plot_ordered_13_species_plot <- ggplot(stacked_area_plot_ordered_13_species_data, 
                                                    aes(x= logsize, fill=Species)) +
                                             geom_area(aes(y = stat(width*density*40)),  stat = "bin", alpha=0.9) +
                                             facet_wrap(~Coreids, nrow = 8, ncol = 8) + 
                                             scale_x_continuous (limits=c(2.2,3.1)) + 
                                             geom_vline(aes(xintercept = logsiz), col="black", linetype = "longdash") + 
                                             geom_vline(aes(xintercept = logmodsize), col="red", linetype = "longdash") + 
                                             scale_fill_viridis(discrete = T) +
                                             theme(legend.position = 'right') + 
                                             theme_ipsum() + 
                                             theme_bw() + 
                                             theme(axis.text.x = element_text(angle=90,hjust=1, size=10)) + 
                                             theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 
                                             theme(legend.text = element_text(face = c(rep("italic", 5)))) + 
                                             theme(legend.title = element_text(face = c(rep("bold", 5)), size = 14)) + 
                                             theme(legend.text = element_text(size = 12)) + 
                                             xlab("Log Size (μm)") +
                                             ylab("Density")

# Export Figure as EPS

cairo_ps(filename='stacked_area_plot_ordered_exp.eps', width=18, height=16, family = "Arial",
         pointsize = 8, fallback_resolution = 500)
stacked_area_plot_ordered_13_species_plot
dev.off()

# Export Figure as PNG

png("~/Desktop/stacked_area_plot_ordered_exp.png",width=8692,height=7181,units="px",res=600,bg="white", pointsize = 8)
stacked_area_plot_ordered_13_species_plot
dev.off()

#******************************************************************
# Figure 10 (Data + Code) -----------------------------------------
#******************************************************************

# Load Data:: Contribution of Large vs Small Size Species relative to the Regional Size 95

Proxy_for_large_vs_small_size_data <- read.csv("Proxy_for_large_vs_small_size.csv", header = TRUE, sep = ',') 

# Plot Figure

Perc_greater_than_reg_size95 <- ggplot(Proxy_for_large_vs_small_size_data, 
                                       aes(x=abundance, y=size, size = Percentage_greater_than_regional_size95, fill = Region)) +
                                geom_point(alpha=0.7, shape = 21) +  
                                scale_size(range = c(.1, 9), name="Percentage (> Regional Size 95/5)") + 
                                scale_fill_viridis(discrete=TRUE) + theme_ipsum() +
                                theme(axis.title.y = element_text(size = 13), axis.title.x = element_text(size = 13)) + 
                                ylab("Size 95/5 (μm)") + 
                                xlab("Abundance (Total Number of Forams)")
  


#******************************************************************
# Figure 11 (Data + Code) -----------------------------------------
#******************************************************************

#Load Data:: Species specific response to environmental parameters

species_specific_response_data <- read.csv("species_specific_response.csv", header = TRUE, sep = ',')

# Plot Figure

species_specific_response_plot <- ggplot(species_specific_response_data,
                                    aes(y = r_coefficient, axis1 = Parameter_most_correlated, axis2 = Species)) +
                             geom_alluvium(aes(fill = Parameter_most_correlated), width = 1/12) +
                             geom_stratum(width = 1/12, fill = "black", color = "grey") +
                             geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
                             scale_x_discrete(limits = c("Parameter", "Species"), expand = c(.05, .05)) +
                             scale_fill_brewer(type = "qual", palette = "Set1") +
                             ggtitle("Species-specific Response to Environmental Variables") + 
                             theme_bw() + 
                             theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
                             theme(axis.text=element_text(size = 12, colour = "black")) +
                             theme(axis.title=element_text(size=13, colour = "black", face = "bold")) + 
                             labs(y = expression("Correlation co-efficient (R)"))

# Export Figure as EPS

cairo_ps(filename='species_specific_response_plot_fig.eps', width=18, height=16, family = "Arial",
         pointsize = 8, fallback_resolution = 500)
species_specific_response_plot
dev.off()

# Export Figure as PNG

png("~/Desktop/species_specific_response_plot_fig.png",width=6000,height=4800,units="px",res=600,bg="white", pointsize = 8)
species_specific_response_plot
dev.off()


#******************************************************************
# Figure 12 (Data + Code) -----------------------------------------
#******************************************************************
# Plot of result after Factor Analysis

# Load Data:: Size F1 Scores vs Carbonate concentration at core depth and Size F2 Scores vs 2nd Order Polynomial Fit with SST &
#.............Assemblage F1 Scores vs Log (Primary Productivity) and Assemblage F2 Scores vs SST

All_core_data <- read.csv("All_core_site_data.csv", header = TRUE, sep = ',')

# (a) Size F1 Scores vs Carbonate concentration at core depth

# Create Base Plot

F1size_vs_Carbonate_at_core_depth_base_plot <- ggplot(All_core_data, 
                                                  aes(x=carbonate_at_core_depth, y=F1size)) + 
                                           geom_point(shape = 2, size = 3, colour = "blue") + 
                                           stat_smooth(method = "lm", col = "#C42126",se = FALSE, size = 1, fullrange = T)  +  
                                           stat_cor(method = "pearson", size = 5) + 
                                           theme_bw() + 
                                           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Add Titles and Customize Labels

F1size_vs_Carbonate_at_core_depth <- F1size_vs_Carbonate_at_core_depth_base_plot + 
                                     labs(x = expression(Carbonate~conc.[Core~depth] ~ (µm/kg)), 
                                     y = expression("F1 Axis Scores")) +
                                     theme(axis.text=element_text(size = 14, colour = "black"), 
                                     axis.title=element_text(size=16, colour = "black", face = "bold"))

# (b) Size F2 Scores vs 2nd Order Polynomial Fit with SST

# Create Base Plot

F2size_vs_SST_baseplot <- ggplot(All_core_data, 
                                 aes(x=sst, y=F2size)) + 
                          geom_point(shape = 7, size = 3, colour = "purple") + 
                          stat_smooth(method = "lm", formula = y~poly(x,2),col = "#C42126",se = FALSE, size = 1, fullrange = T) + 
                          theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

# Add Titles and Customize Labels

F2size_vs_SST <- F2size_vs_SST_baseplot + 
                 labs(x = expression("SST ("*degree*"C)"), 
                      y = expression("F2 Axis Scores")) +
                 annotate("text", label = "R = 0.80, p = 1.5e-13", x =23, y = 2.7, size = 5, colour ="black") + 
                 theme(axis.text=element_text(size = 14, colour = "black"), 
                       axis.title=element_text(size=16, colour = "black", face = "bold"))

# (c) Assemblage F1 Scores vs Primary Productivity

F1assemblagevspp <- ggplot(All_core_data,
                           aes(logpp,F1assemblage)) + 
                    geom_point(colour = "blue", shape = 2, size = 3) + 
                    geom_smooth(method=lm,se=T, size = .5, show.legend = F, colour = "red") + 
                    theme_bw() + 
                    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
                    theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "none",
                          axis.title=element_text(size=16, colour = "black", face = "bold")) +
                    annotate("text", label = "R = –0.76, p = 1.1e-12", x = 3.27, y = 1.1, size = 5, colour ="black") + 
                    labs(y = expression("F1 Scores (Assemblage)"), 
                         x = expression(Log (Primary ~ Productivity ~ (mgC ~ m^{-2}~d^{-1}))))

# (d) Assemblage F2 Scores vs SST

F2assemblagevssst <- ggplot(All_core_data,
                            aes(sst,F2assemblage)) + 
                     geom_point(colour = "purple", shape = 7, size = 3) +
                     geom_smooth(method=lm,se=T, size = .5, show.legend = F, colour = "red") + 
                     theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
                     theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "none",
                          axis.title=element_text(size=16, colour = "black", face = "bold")) +
                     annotate("text", label = "R = –0.81, p = 8.7e-16", x = 27, y = 4, size = 5, colour ="black") + 
                     labs(y = expression("F2 Scores (Assemblage)"), 
                          x = expression("SST(°C)"))

#Export Figures all 4 figures as PNG

png("~/Desktop/Factorial Analysis for size and Assemblage Data.png",width=8800,height=8300,units="px",res=800,bg="white", pointsize = 8)
ggarrange(F1size_vs_Carbonateatcoredepth, F2size_vs_SST, F1assemblagevspp,F2assemblagevssst, labels = c("(a)", "(b)","(c)","(d)"), ncol = 2, nrow = 2)
dev.off()


#******************************************************************
# Figure 13 (Data + Code) -----------------------------------------
#******************************************************************

# Load Data:: Mean fragment size vs Mean Foraminifera Size

All_core_data <- read.csv("All_core_site_data.csv", header = TRUE, sep = ',')

# Plot Figure

Average_foram_size_vs_Average_foram_size <- ggplot(All_core_data, 
                                                   aes(x=avg_frag_size, y=avg_foram_size)) + 
                                            geom_point(size = 2, show.legend = F) + geom_smooth(method = lm, se =T, show.legend = F, colour = "red")+
                                            theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
                                            theme(axis.text=element_text(size = 12, colour = "black"),
                                                  axis.title=element_text(size=14, colour = "black")) + 
                                            annotate("text", label = "R = 0.79, p = 2.7e-14", x =255, y = 420, size = 4, colour = "black")+
                                            labs(y = "Average Foram Size (μm)", x = "Average Fragment Size (μm)")

# Export Figure as EPS

cairo_ps(filename='Mean_Foram_Size_VS_Mean_Fragment_Size_Plot.eps', width=18, height=16, family = "Arial",
         pointsize = 8, fallback_resolution = 500)
Average_foram_size_vs_Average_foram_size
dev.off()

#Export Figure as PNG

png("~/Desktop/Mean_Foram_Size_VS_Mean_Fragment_Size_Plot.png",width=2800,height=2500,units="px",res=500,bg="white", pointsize = 8)
Average_foram_size_vs_Average_foram_size
dev.off()

#******************************************************************
# Supplementary File 3 (Data + Code) ------------------------------
#******************************************************************

# Load Data:: Inflata size vs SST

infalata_vs_sst_data <- read.csv("infalata_vs_sst.csv", header = TRUE, sep = ',')

# Plot Figure

inflata_vs_sst_plot <- ggscatter(infalata_vs_sst_data, x = "inflata_size", y = "sst", 
                            add = "reg.line", conf.int = TRUE, size = 1,
                            cor.coef = F, cor.coeff.args = list(method = "pearson", label.x.npc = "middle", label.y.npc = "top"), add.params = list(color = "black", fill = "lightgray", size = 0.5),
                            xlab = ("Size (µm) "), ylab = "Sea surface temperature (°C)") +
                            annotate("text", label = "R = – 0.58, p = 0.0013", x = 350, y = 30, size = 4, colour ="black") 

# Export Figure as EPS

cairo_ps(filename='inflata_vs_sst_plot.eps', width=18, height=16, family = "Arial",
         pointsize = 8, fallback_resolution = 500)
inflata_vs_sst_plot
dev.off()

#Export Figure as PNG

png("~/Desktop/inflata_vs_sst_plot.png",width=2000,height=1800,units="px",res=400,bg="white", pointsize = 8)
inflata_vs_sst_plot
dev.off()


#******************************************************************
# Supplementary File 4 (Data + Code) ------------------------------
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

# Plot Figures

Rillo_PO <- ggscatter(pobliqrill, x = "sst", y = "size", 
                      add = "reg.line", conf.int = TRUE, size = 1,
                      cor.coef = TRUE, cor.method = "pearson", add.params = list(color = "black", fill = "lightgray", size = 0.5), facet.by = "species",
                      xlab = F, ylab = "Size (μm)") + 
                      font("xy.text", size = 12) + # (Change axis text)
                      theme(strip.text.x = element_text(size = 12)) # (Change facet text size)

Rillo_GMen <- ggscatter(gmenrill, x = "sst", y = "size", 
                        add = "reg.line", conf.int = TRUE, size = 1,
                        cor.coef = TRUE, cor.method = "pearson", add.params = list(color = "black", fill = "lightgray", size = 0.5), facet.by = "species",
                        xlab = F, ylab = F) +
                        font("xy.text", size = 12) + 
                        theme(strip.text.x = element_text(size = 12)) 

Rillo_NDurt <- ggscatter(ndurtrill, x = "sst", y = "size", 
                         add = "reg.line", conf.int = TRUE, size = 1,
                         cor.coef = TRUE, cor.method = "pearson", add.params = list(color = "black", fill = "lightgray", size = 0.5), facet.by = "species",
                         xlab = F, ylab = F) +
                         font("xy.text", size = 12) + 
                         theme(strip.text.x = element_text(size = 12)) 

Rillo_GRub <- ggscatter(grubrill, x = "sst", y = "size", 
                        add = "reg.line", conf.int = TRUE, size = 1,
                        cor.coef = TRUE, cor.method = "pearson", add.params = list(color = "black", fill = "lightgray",size = 0.5), facet.by = "species",
                        xlab = F, ylab = "Size (μm)") +
                        font("xy.text", size = 12) + 
                        theme(strip.text.x = element_text(size = 12))

Rillo_GSac  <- ggscatter(gsacrill, x = "sst", y = "size", 
                         add = "reg.line", conf.int = TRUE, size = 1,
                         cor.coef = TRUE, cor.method = "pearson", add.params = list(color = "black", fill = "lightgray", size = 0.5), facet.by = "species",
                         xlab = F, ylab = F) + 
                         font("xy.text", size = 12) + 
                         theme(strip.text.x = element_text(size = 12)) 

Rillo_GSiphon <- ggscatter(gsiphonrill, x = "sst", y = "size", 
                           add = "reg.line", conf.int = TRUE, size = 1,
                           cor.coef = TRUE, cor.method = "pearson", add.params = list(color = "orange", fill = "lightgray", size = 0.5), facet.by = "species",
                           xlab = F, ylab = F) +
                           font("xy.text", size = 12) + 
                           theme(strip.text.x = element_text(size = 12)) 

Rillo_GCongloba <- ggscatter(gconglobarill, x = "sst", y = "size", 
                             add = "reg.line", conf.int = TRUE, size = 1,
                             cor.coef = TRUE, cor.method = "pearson", add.params = list(color = "orange", fill = "lightgray", size = 0.5), facet.by = "species",
                             xlab = "SST (°C)", ylab = "Size (μm)") + 
                             font("xy.text", size = 12) + # (Change axis text)
                             theme(strip.text.x = element_text(size = 12)) # (Change facet text size)

Rillo_GTrunc <- ggscatter(gtruncrill, x = "sst", y = "size", 
                          add = "reg.line", conf.int = TRUE, size = 1,
                          cor.coef = TRUE, cor.method = "pearson", add.params = list(color = "orange", fill = "lightgray", size = 0.5), facet.by = "species",
                          xlab = "SST (°C)", ylab = F) +
                          font("xy.text", size = 12) + # (Change axis text)
                          theme(strip.text.x = element_text(size = 12)) # (Change facet text size)

Rillo_GInfla <- ggscatter(ginflacrill, x = "sst", y = "size", 
                          add = "reg.line", conf.int = TRUE, size = 1,
                          cor.coef = TRUE, cor.method = "pearson", add.params = list(color = "orange", fill = "lightgray",size = 0.5), facet.by = "species",
                          xlab = "SST (°C)", ylab = F) + 
                          font("xy.text", size = 12) + 
                          theme(strip.text.x = element_text(size = 12))

# Arrange figures into columns and rows

Rillo_Analyzed <- ggarrange(Rillo_PO,Rillo_GMen,Rillo_NDurt,Rillo_GRub, Rillo_GSac, Rillo_GSiphon, Rillo_GCongloba,Rillo_GTrunc, Rillo_GInfla, ncol = 3, nrow = 3)

# Export Figure as EPS

cairo_ps(filename='Rillo Analyzed Plot.eps', width=18, height=16, family = "Arial",
      pointsize = 8, fallback_resolution = 500)
Rillo_Analyzed
dev.off()

# Export Figure as PNG

png("~/Desktop/Rillo Analyzed Plot.png",width=6000,height=5000,units="px",res=500,bg="white", pointsize = 8)
Rillo_Analyzed
dev.off()








