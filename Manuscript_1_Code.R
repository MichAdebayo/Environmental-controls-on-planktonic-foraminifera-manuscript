#**************************
#PhD First Manuscript Code 
#**************************
#
#Code related to the paper: 
#
#************************************************************************************************************************
#Manuscript Title: Environmental Controls on Size Distribution of Planktonic Foraminifera in the tropical Indian Ocean
#************************************************************************************************************************
#
#Author: Michael Adebayo
#
#Date: 14th April, 2021 
#*****************************

#***********************************************************************
#Packages -------------------------------------------------------------
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
library("Cairo")
library("extrafont")
library("Rfit")
library("tseries")
library("LaplacesDemon")
library("robustbase")
library("openxlsx")
library("dichromat")
library("RColorBrewer")
library("colorBlindness")

#******************************************************************
# Jarque-Bera Test for Data Normality Test -----------------------
#******************************************************************

jarque.bera.test(All_species_unimod_test$size) #Test size data for normality

#******************************************************************
# Kruskal-Wallis Test for Normality -----------------------
#******************************************************************
#*
kruskal.test(size ~ species, data = All_species_unimod_test)

#******************************************************************
#Figure 3b (Human vs Machine) -----------------------------------------
#******************************************************************

#Load Data:: Human vs Machine (Total Individual and Mean Species Count)

Human_vs_Machine_1 <- read.csv("Human_vs_machine_total_individual_species_count.csv", header = TRUE, sep = ',')
Human_vs_Machine_2 <- read.csv("Human_vs_machine_total_mean_species_count.csv", header = TRUE, sep = ',')

#Plot Figure

Human_vs_Machine <- ggplot(NULL, aes(Total_Machine_Count, Total_Human_Count)) + 
  geom_point(data = Human_vs_Machine_1, aes(color = Species, shape = Species)) + 
  geom_abline(intercept = 0, slope = 1, color = 'black', size = .4) + 
  scale_color_viridis_d(aesthetics = "colour") + 
  scale_color_manual(aesthetics = "color", values = c("#dd77a3", "#67c675", "#c76dcf","#a7bc45",
  "#6a70d7", "#6aa13f","#523687","#ce9534",
"#6d8bd6","#cd6d3b", "#33d4d1","#c9417e",
"#47bb8a", "#a54190", "#4a6e24","#ca86ce",
"#af9e4d", "#802657","#984627","#dd5858",
"#b44555","#8B4789")) +
  scale_shape_manual(values = c(4,5,6,7,8,9,10,11,12,13,
                                14,15,16,17,18,19,20,21,22,23,24,25)) + 
  scale_y_log10() + scale_x_log10() + 
  geom_point(data = Human_vs_Machine_2, 
             aes(x=Mean_Machine_Count, y = Mean_Human_Count,alpha = 0.9,size = Mean,  color = 'purple')) +
  scale_size_continuous(range = c(3,21), name = 'Mean Count (Upper Limit)') +
  theme_bw() + 
  theme(legend.position= "right") + 
  theme(axis.line  = element_line(colour = "black",size=0), panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_text_repel(data = Human_vs_Machine_2, aes(Mean_Machine_Count, 
                                                 Mean_Human_Count, label = Species),
                  segment.color = 'black', fontface = 'italic', size = 5.8, force = 4,
                  point.padding = 0.9, min.segment.length = 1.5, box.padding = 1.4) + 
  guides(alpha = "none", color = "none", shape = guide_legend(override.aes = list(size = 7))) + 
  guides(size = guide_legend(override.aes = list(color = 'orchid4'))) + 
  theme(legend.text = element_text(face = "italic", size = 24)) + 
  theme(legend.title = element_text(size = 25)) 

#Add and Customize Axis Titles

Human_vs_Machine_exp  <- Human_vs_Machine + theme(axis.text=element_text(size = 24, colour = "black"),
                                                  axis.title=element_text(size=25, colour = "black")) + 
  annotate("text", x =90, y = 2700, size = 7, label = "Total number of forams compared") + 
  annotate("text", x =70, y = 2300, size = 7, 
           label = "italic(n)==127097", parse = TRUE) +
  annotate("text", x = 60, y = 1800, size = 7, 
            label = "italic(ρ)==0.97*','~italic(p)", parse = TRUE) +
  annotate("text", x = 158, y = 1800, size = 7, 
           label = "< 2.2e-16") +
  annotate("text", x = 1650, y = 2200, size = 7, 
           label = "italic(y)*'='~italic(x)", parse = TRUE) +
  labs(y = "Number Counted by Human Recognition", x = "Number Counted by CNN Recognition")

#Export Figure as JPG

jpeg("~/Desktop/Human_vs_machine_total_individual_and_mean_species_count26082022.jpg",width=9500,height=6000,units="px",res=600,bg="white", pointsize = 8)
Human_vs_Machine_exp
dev.off()


#******************************************************************
#Figure 4a (This study vs ForCenS) -----------------------------------------
#******************************************************************

#Load Data:: Relative Abundance (This study) vs Relative Abundance (ForCenS)

Relative_abundance_comparison_vs_ForCens <- read.csv("Relative_abundance_comparison_with_ForCens.csv", header = TRUE, sep = ',')

#Plot Figure

Relative_Abundance_Comparison <- ggplot(Relative_abundance_comparison_vs_ForCens, 
                                        aes(x=relative_abundance_in_ForCens_database,y=relative_abundance_from_machine_count)) + 
  geom_point(aes(color = Species, shape = Species), size = 5) + 
  scale_x_log10(limits= c(0.1,100)) + scale_y_log10(limits=c(0.1,100)) +
  scale_color_manual(aesthetics = "colour", values = c("#dd77a3", "#67c675", "#c76dcf","#a7bc45",
                                                        "#6a70d7", "#6aa13f","#523687","#ce9534",
                                                        "#6d8bd6","#cd6d3b", "#33d4d1","#c9417e",
                                                        "#47bb8a", "#a54190", "#4a6e24","#ca86ce",
                                                        "#af9e4d", "#802657","#984627","#dd5858",
                                                        "#b44555")) + 
  scale_shape_manual(values = c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)) +
  theme_bw () + 
  theme(legend.position = "right") + 
  theme(axis.line  = element_line(colour = "black",size=0), 
        panel.border = element_rect(colour = "black", fill = NA, size = 1), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(legend.text = element_text(face = "italic", size = 24)) + 
  guides(color = guide_legend(ncol = 1, bycol = FALSE)) + 
  guides(shape = guide_legend(override.aes = list(size = 7))) + 
  theme(legend.title = element_text(size = 25))

#Add y = x  Line

Relative_Abundance_Comparisons <- Relative_Abundance_Comparison + geom_abline(intercept = 0, slope = 1, color = 'gray40', size = .6)

#Add and Customize Axis Titles

Relative_Abundance_Comparisons_Exp <- Relative_Abundance_Comparisons + 
  theme(axis.text=element_text(size = 24, colour = "black"),
        axis.title=element_text(size=25, colour = "black")) + 
  annotate("text", x = 0.3, y = 100, size = 10, 
           label = "italic(ρ)==0.77*','~italic(p)", parse = TRUE) +
  annotate("text", x = 1.6, y = 100, size = 10, 
           label = "< 2.2e-16") +
  annotate("text", x = 65, y = 100, size = 10, 
           label = "italic(y)*' ='~italic(x)", parse = TRUE) +
  labs(y = "Relative Abundance ((This Study) %)", x = "Relative Abundance ((ForCenS) %)")


#Export Figure as JPG

jpeg("~/Desktop/ForCens vs This study plot06092022.jpg",width=6700,height=6000,units="px",res=500,bg="white", pointsize = 8)
Relative_Abundance_Comparisons_Exp
dev.off()

#******************************************************************
#Figure 4b (This study vs Rillo et al. (2020) --------------------
#******************************************************************

#Load Data:: Comparison between CNN Size vs Rillo et al. size distribution with error bars (standard deviation)

this_study_size_vs_rillo <- read.csv("size_this_study_vs_rillo_et_al.csv", header = TRUE, sep = ',')

#Plot Figure

#Set sd (Standard Deviation) lower and upper limit for error bar 

ymin = this_study_size_vs_rillo$machine_size - this_study_size_vs_rillo$sd
ymax = this_study_size_vs_rillo$machine_size + this_study_size_vs_rillo$sd

#Preliminary Figure Plot

this_study_size_vs_rillo_plot <- ggplot(this_study_size_vs_rillo, 
                                        aes(rillo_size, machine_size)) + 
  geom_point(aes(color = Species, shape = Species), size = 8) + 
  scale_color_manual(aesthetics = "colour", values = c("#DC050C","#67c675", "#a7bc45", "#6a70d7", "#6aa13f",
                                                       "#ce9534","#6d8bd6","#cd6d3b", "#33d4d1","#c9417e",
                                                       "#47bb8a", "#a54190", "#4a6e24","#ca86ce", "#af9e4d", 
                                                       "#802657", "#72190E","#dd5858", "#b44555", "#42150A" )) + 
  scale_shape_manual(values = c(1,5,7,8,9,11,12,13,14,15,16,17,18,19,20,21,25,23,24,3)) + 
  geom_errorbar(aes(ymin=ymin, ymax=ymax)) +
  theme_bw() + 
  theme(legend.position= "right") + 
  theme(axis.line  = element_line(colour = "black",size=0), 
        panel.border = element_rect(colour = "black", fill=NA, size=1),panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  theme(legend.text = element_text(face = "italic", size = 24)) + 
  guides(shape = guide_legend(override.aes = list(size = 7))) + 
  theme(legend.title = element_text(size = 25)) 

#Set Fixed X and Y Coordinates

this_study_size_vs_rillo_plots <- this_study_size_vs_rillo_plot + 
  coord_fixed(ratio = 1, xlim = c(1,1240), ylim = c(1,1240), expand = T, clip = "on")

#Add y = x Line

this_study_size_vs_rillo_plots_final <- this_study_size_vs_rillo_plots + 
  geom_abline(intercept = 0, slope = 1, color = 'black', size = .6)


#Add and Customize Axis Titles

this_study_size_vs_rillo_plots_final_exp <- this_study_size_vs_rillo_plots_final + 
  theme(axis.text=element_text(size = 24),
        axis.title=element_text(size=25, colour = "black")) + 
  annotate("text", x = 220, y = 1200, size = 8, 
           label = "italic(ρ)==0.94*','~italic(p)", parse = TRUE) +
  annotate("text", x = 535, y = 1200, size = 8, 
           label = "= 5.9e-06") +
  annotate("text", x = 1100, y = 1200, size = 8, 
           label = "italic(y)*' ='~italic(x)", parse = TRUE) + 
  labs(y = "CNN size (μm)", x = "Buckley collection + Resampled data size (μm)")

#Export Figure as JPG

jpeg("~/Desktop/CNN vs Rillo with error bars06092022.jpg",width=6700,height=6000,units="px",res=600,bg="white")
this_study_size_vs_rillo_plots_final_exp
dev.off()


#******************************************************************
#Figure 6 (Factor Analysis for Assemblage distribution) ---------
#******************************************************************``

#Load Data:: Assemblage Distribution Factor Analysis (Groundtruthing Cayre et al. 1997)

All_core_data <- read.csv("All_core_site_data.csv", header = TRUE, sep = ',')

#(a) Assemblage F1 Scores vs Primary Productivity

F1assemblagevspp <- ggplot(All_core_site_data, aes(logpp,F1assemblage)) + 
  geom_point(colour = "#b067a3", size = 3) + 
  geom_smooth(method="rlm", aes(colour="rlm"),se=T, size = .5, show.legend = F, colour = "black") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 12, colour = "black"), legend.position = "none",
        axis.title=element_text(size=14, colour = "black", face = "bold")) +
  annotate("text", x = 3.17, y = 1.1, size = 5, colour ="black", label = "italic(R)^{2}==0.61*','~italic(p)==2.26e-06", parse = TRUE) +
  labs(y = expression("F1 Scores (Assemblage)"), 
       x = expression(Log (Primary ~ Productivity ~ (mgC ~ m^{-2}~d^{-1}))))

#(b) Assemblage F2 Scores vs SST

F2assemblagevssst <- ggplot(All_core_site_data,aes(sst,F2assemblage)) + 
  geom_point(colour = "#9c954d", size = 3) +
  geom_smooth(method=lm,se=T, size = .5, show.legend = F, colour = "black") + 
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 12, colour = "black"), legend.position = "none",
        axis.title=element_text(size=14, colour = "black", face = "bold")) +
  annotate("text", x = 26, y = 3.8, size = 5, colour ="black", label = "italic(R)^{2}==0.46*','~italic(p)==6.52e-4", parse = TRUE) +
  labs(y = expression("F2 Scores (Assemblage)"), x = expression("SST(°C)"))

#Export Figure as PNG

jpeg("~/Desktop/Factorial Analysis for size and Assemblage Data28082022.jpg",width=6000,height=2500,units="px",res=600,bg="white", pointsize = 8)
ggarrange(F1assemblagevspp,F2assemblagevssst, labels = c("(a)", "(b)"), ncol = 2, nrow = 1)
dev.off()

#******************************************************************
#Figure 7 (Dissolution Index) -------------------------------------
#******************************************************************

#Load Data:: Dissolution Index (Berger and Parker Index vs Depth, Size vs Depth, Size vs Carbonate at Core Depth, Size vs Fragmentation Rate)

All_core_data <- read.csv("All_core_site_data.csv", header = TRUE, sep = ';')

#Plot Figures

scale_color_manual(values = c("#b067a3", "#9c954d", "#bc7d39", "#697ed5")) + 
  
#Place axis title at the end of axis

B_and_P_Index_vs_Depth_axis_adj <- B_and_P_Index_vs_Depth + theme(axis.title.y = element_text(hjust = 1, size = 16), axis.title.x = element_text(size = 16))

#Reverse legend color bar scale

B_and_P_vs_Depth_Exp <- B_and_P_Index_vs_Depth_axis_adj + guides(color = guide_colourbar(reverse = T))

#scale_shape_manual(values = c(15,16,17,18))


#(b) Size vs Depth

Size_vs_Depth <- ggplot(All_core_site_data,aes(Depth,size)) + 
  geom_point(aes(shape = Region, color = Region), size = 6) +
  geom_smooth(method="rlm", aes(colour="rlm"),se=T, size = .5, color = 'black')  + 
  scale_shape_manual(values = c(15,16,17,18)) + 
  scale_color_manual(values = c("#b067a3", "#9c954d", "#bc7d39", "#697ed5")) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 24, colour = "black"), legend.position = "right",
        axis.title=element_text(size=25, colour = "black")) +
  theme(legend.title = element_text(size = 25)) + 
  theme(legend.text = element_text(size = 24)) + 
  annotate("text", x = 3000, y = 850, size = 6, colour ="black", 
           label = "italic(R)^{2}==0.10*','~italic(p)==0.03", parse = TRUE) +
  labs(y = expression(Size[95]["/"][5]~(µm)), x = expression("Depth (m)"))

jpeg("~/Desktop/Dissolution_depth05092022.jpg",width=6000,height=3500,units="px",res=600,bg="white", pointsize = 8)
Size_vs_Depth 
dev.off()

#(c) Size vs Carbonate at core depth

Size_vs_Delta_carbonate_at_core_depth <- ggplot(All_core_site_data,aes(cd_delta_calc,size)) + 
  geom_point(aes(shape = Region, color = Region), size = 6) +
  geom_smooth(method="rlm", aes(colour="rlm"),se=T, size = .5, color = 'black')  + 
  scale_shape_manual(values = c(15,16,17,18)) + 
  scale_color_manual(values = c("#b067a3", "#9c954d", "#bc7d39", "#697ed5")) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 24, colour = "black"), legend.position = "right",
        axis.title=element_text(size=25, colour = "black")) +
  theme(legend.title = element_text(size = 25)) + 
  theme(legend.text = element_text(size = 24)) + 
  annotate("text", x = 15, y = 850, size = 6, colour ="black", 
           label = "italic(R)^{2}==0.11*','~italic(p)==0.01", parse = TRUE) +
  labs(y = expression(Size[95]["/"][5]~(µm)), x = expression(Delta ~ Carbonate[Core~depth] ~ (µmol/kg)))

jpeg("~/Desktop/Dissolution_deltacarb05092022.jpg",width=6000,height=3500,units="px",res=600,bg="white", pointsize = 8)
Size_vs_Delta_carbonate_at_core_depth
dev.off()

#(d) Size vs Fragmentation Rate

Size_vs_frag_rate <- ggplot(All_core_site_data,aes(fragmentation_rate,size)) + 
  geom_point(aes(shape = Region, color = Region), size = 6) +
  geom_smooth(method="rlm", aes(colour="rlm"),se=T, size = .5, color = 'black')  + 
  scale_shape_manual(values = c(15,16,17,18)) + 
  scale_color_manual(values = c("#b067a3", "#9c954d", "#bc7d39", "#697ed5")) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 24, colour = "black"), legend.position = "right",
        axis.title=element_text(size=25, colour = "black")) +
  theme(legend.title = element_text(size = 25)) + 
  theme(legend.text = element_text(size = 24)) + 
  annotate("text", x = 30, y = 850, size = 6, colour ="black", 
           label = "italic(R)^{2}==3.495e-07*','~italic(p)==0.99", parse = TRUE) +
  labs(y = expression(Size[95]["/"][5]~(µm)), x = expression("Fragmentation Rate (%)"))


jpeg("~/Desktop/Dissolution_fragrate05092022.jpg",width=6000,height=3500,units="px",res=600,bg="white", pointsize = 8)
Size_vs_frag_rate
dev.off()

#Export all 4 Figures as a single PNG file

jpeg("~/Desktop/Dissolution Indices_B_29082022.jpg",width=7124,height=4424,units="px",res=600,bg="white", pointsize = 8)
ggarrange(Size_vs_Depth)
dev.off()

jpeg("~/Desktop/Dissolution Indices_C_29082022.jpg",width=7124,height=4424,units="px",res=600,bg="white", pointsize = 8)
ggarrange(Size_vs_Delta_carbonate_at_core_depth)
dev.off()

jpeg("~/Desktop/Dissolution Indices_D_29082022.jpg",width=7124,height=4424,units="px",res=600,bg="white", pointsize = 8)
ggarrange(Size_vs_frag_rate)
dev.off()

#******************************************************************
#Figure 8 (Density plot) -----------------------------------------
#******************************************************************

#Load Data:: Density Plot of Species Size

size_density <- read.csv("Size_density_plot.csv", header = TRUE, sep = ',')

#Plot Figure

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


# Test of unimodality, bimodality, and multimodality

is.unimodal(B_bulloides_unimod_test$size)

is.unimodal(B_digitata_unimod_test$size)

is.multimodal(G_adamsi_unimod_test$size)

is.multimodal(G_calida_unimod_test$size)

is.unimodal(G_conglobatus_unimod_test$size)

is.unimodal(G_conglomerata_unimod_test$size)

is.unimodal(G_crassaformis_unimod_test$size)

is.unimodal(G_elongatus_unimod_test$size)

is.unimodal(G_falconensis_unimod_test$size)

is.unimodal(G_glutinata_unimod_test$size)

is.multimodal(G_hexagonus_unimod_test$size)

is.unimodal(G_hirsuta_unimod_test$size)

is.unimodal(G_inflata_unimod_test$size)

is.unimodal(G_menardii_unimod_test$size)

is.unimodal(G_ruber_unimod_test$size)

is.unimodal(G_rubescens_unimod_test$size)

is.unimodal(G_sacculifer_unimod_test$size)

is.unimodal(G_scitula_unimod_test$size)

is.unimodal(G_siphonifera_unimod_test$size)

is.unimodal(G_tenella_unimod_test$size)

is.unimodal(G_trucatulinoides_unimod_test$size)

is.unimodal(G_tumida_unimod_test$size)

is.unimodal(G_ungulata_unimod_test$size)

is.unimodal(G_uvula_unimod_test$size)

is.unimodal(G_vivans_unimod_test$size)

is.unimodal(H_pelagica_unimod_test$size)

is.unimodal(N_dutertrei_unimod_test$size)

is.unimodal(N_incompta_unimod_test$size)

is.unimodal(O_universa_unimod_test$size)

is.unimodal(P_obliquiloculata_unimod_test$size)

is.unimodal(N_pachyderma_unimod_test$size)

is.bimodal(N_pachyderma_unimod_test$size)

is.unimodal(C_nitida_unimod_test$size)
is.bimodal(C_nitida_unimod_test$size)

is.unimodal(S_dehiscens_unimod_test$size)

is.unimodal(T_humilis_unimod_test$size)

is.unimodal(T_iota_unimod_test$size)

is.unimodal(T_quinqueloba_unimod_test$size)

# Export Figure as EPS

cairo_ps(filename='Density Plot.eps', width=18.75,height=29.2,family = "Arial",
         pointsize = 16, fallback_resolution = 500)
Densityplot_italicized
dev.off()

#Export Figure as JPG

jpeg("~/Desktop/Density Plot06092022.jpg",width=1750,height=2700,units="px",res=500, bg="white",pointsize = 8)
Densityplot_italicized
dev.off()

#*****************************************************************
#Figure 9 (Optimum size hypothesis + size vs SR and SD) ---------
#*****************************************************************

#Load Data::Size vs Species Richness, Size vs Species Diversity, & Temperature at Maximum size vs Temperature at Maximum Relative Abundance

All_core_data <- read.csv("All_core_site_data.csv", header = TRUE, sep = ',')
Temperature_at_max_size_vs_temperature_at_max_relative_abundance <- read.csv("Temp_at_max_size_vs_Temp_at_max_rel_abund.csv", header = T, sep = ',')

#Plot Figures

# (a) Single-species robust regression analyses with p-values corrected for multiple testing

robust.m <- ggplot(Opt_hyp_test, aes(x=Sp_rel_abund, y=Size)) + 
  geom_point() +
  facet_wrap(~Species, scales = "free_x", shrink = T, as.table = T) + 
  stat_smooth(method = "rlm",col = "#000000",se = T, size = 0.7, fullrange = T) + 
  #stat_regline_equation(aes(label = paste(..rr.label.., sep = "~~~~"))) +
  theme_bw() + 
  theme(strip.text.x = element_text(size = 20, face = "italic")) +
  expand_limits(x = 0, y = 0) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, NA))) +
  theme(panel.spacing = unit(2.3, "lines")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size = 20, colour = "black"), 
        axis.title=element_text(size=24, colour = "black", face = "bold")) +
  guides(size = "none") +
  labs(x = expression("Relative Abundance (%)"), y = expression("Size (µm)")) 

jpeg("~/Desktop/Opt_size_04092022_2.jpg",width=15000,height=10500,units="px",res=700,bg="white", pointsize = 8)
robust.m
dev.off()

# p-value correction for multiple testing

alpha <- (0.025) #Set Bonferroni alpha 

bonfer_test$bonferroni_sig_2 <- p.adjust(bonfer_test$pvalue, 
                                         method = "bonferroni") < alpha #Add True or False


# (b) Size vs Species Richness

  Size_vs_species_richness <- ggplot(All_core_site_data,
                                     aes(species_richness,size)) + 
    geom_point(aes(color = Depth), size = 4) +
    geom_smooth(method="rlm", aes(colour="rlm"),se=T, size = .5, color = 'black')  + 
    scale_color_gradientn(colours = c("#d7191c","#fdae61","#abd9e9","#004488")) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    theme(axis.text=element_text(size = 20, colour = "black"), legend.position = "none",
          axis.title=element_text(size=22, colour = "black", face = "bold")) +
    annotate("text", x = 18, y = 850, size = 5, colour ="black", label = "italic(R)^{2}==0.013*','~italic(p)==0.546", parse = TRUE) +
    labs(y = expression(Size[95]["/"][5]~(µm)), x = expression("Species Richness"))
  
  
  #450054","#FCE724","#DC267F","#FE6100"
  #FCE724","#7AD151", "#2A778F","#450054"
  #expression("Size (μm)")
  
#(c) Size vs Species Diversity

  Size_vs_species_diversity <- ggplot(All_core_site_data, #Add data (Data, x-variable, y-variable)
                                     aes(species_diversity,size)) + 
    geom_point(aes(color = Depth), size = 4) +    # color points by depth
    geom_smooth(method="rlm", aes(colour="rlm"),se=T, size = .5, color = 'black')  + 
    scale_color_gradientn(colours = c("#d7191c","#fdae61","#abd9e9","#004488"), name = "Depth (m)") +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    theme(axis.text=element_text(size = 20, colour = "black"), 
          axis.title=element_text(size=22, colour = "black", face = "bold")) +
    theme(legend.position = "right", legend.text = element_text(size = 20), 
          legend.title = element_text(size = 22)) +
    annotate("text", x = 1.5, y = 820, size = 5, 
             colour ="black", label = "italic(R)^{2}==0.49*','~italic(p)==6.43e-09", parse = TRUE) +
    labs(y = NULL, x = expression("Species Diversity (Shannon–Weiner Index)"))
    
  
#Export all Figures as single JPEG file

jpeg("~/Desktop/Size_vs_abundance_richness_and_diversity25042022_exp.jpg",width=9500,height=3700,units="px",res=600,bg="white", pointsize = 8)
ggarrange(ggarrange(Size_vs_species_richness,Size_vs_species_diversity, labels = c("(a)", "(b)")))
dev.off() 


#******************************************************************
#Figure 10 (Influence of assemblage distribution on size) --------
#******************************************************************

#Load Data::Influence of assemblage distribution on size (data only shown for the 13 most abundant species in the Equatorial Indian Ocean)

stacked_area_plot_ordered_13_species_data <- read.csv("stacked_area_plot_ordered_13_species.csv", header = TRUE, sep = ',') 

#Re-order the data according to core location (i.e. Arabian Sea, Bay of Bengal, Central Indian Ocean, and Mozambique Channel)

stacked_area_plot_ordered_13_species_data$Coreids <- factor(stacked_area_plot_ordered_13_species_data$Coreid, 
                                                            levels = c( "MD96-2045", "MD96-2044", "MD79-276","MD79-261","MD79-260", "MD76-011", "MD90-0936", "MD90-0939", "MD90-0940", "MD90-0938",
                                                                        "MD96-2049", "MD79-257", "MD96-2051", "MD96-2053", "MD96-2054", "MD96-2055", "MD96-2056", "MD96-2067a", "MD96-2067b", "MD96-2067b2",
                                                                        "MD96-2058", "MD96-2059", "MD96-2066", "MD96-2065","MD96-2061", "MD96-2063", "MD96-2064","MD96-2060", "MD98-2165", "BARP9442",
                                                                        "BARP9441",  "BARP9439",  "BARP9437", "MD12-3423",  "BARP9412", "BARP9411",  "MD77-160 ","BARP9409", "BARP9430", "BARP9422",
                                                                        "BARP9426", "MD77-171 ", "MD77-185", "MD76-133",  "MD77-182 ", "MD90-0949",  "MD90-0955", "MD90-0963", "MD90-0959", "MD90-0958",
                                                                        "MD90-0961", "MD90-0960", "MD90-0956", "MD90-0957", "MD76-132", "MD77-205", "MD77-204","MD77-202", "MD04-2873",
                                                                        "MD04-2877", "MD04-2875B"))  
#Set color pallette

Paired <- colorRampPalette(brewer.pal('Paired',n=12), alpha = FALSE)

#Plot Figure

stacked_area_plot_ordered_13_species_plot <- ggplot(stacked_area_plot_ordered_13_species_data, 
                                                    aes(x= size, fill=Species)) +
  geom_area(aes(y = stat(width*density*25)),  stat = "bin") + 
  scale_fill_manual(values = setNames(Paired(13), levels(stacked_area_plot_ordered_13_species_data$Species))) + 
  facet_wrap(~Coreids, nrow = 8, ncol = 8) + 
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
  
#Export Figure as EPS

cairo_ps(filename='stacked_area_plot_ordered_exp.eps', width=18, height=16, family = "Arial",
         pointsize = 8, fallback_resolution = 500)
stacked_area_plot_ordered_13_species_plot
dev.off()

#Export Figure as JPEG

jpeg("~/Desktop/stacked_area_plot_ordered_exp_06092022.jpg",width=10000,height=7181,units="px",res=600,bg="white", pointsize = 8)
stacked_area_plot_ordered_13_species_plot
dev.off()

#******************************************************************
#Figure 11 (Contribution of Large vs Small Size Species) ---------
#******************************************************************

#Load Data:: Contribution of Large vs Small Size Species relative to the Regional Size 95

Proxy_for_large_vs_small_size_data <- read.csv("Proxy_for_large_vs_small_size.csv", header = TRUE, sep = ',') 

#Plot Figure

Perc_greater_than_reg_size95 <- ggplot(Proxy_for_large_vs_small_size, 
                                       aes(x=abundance, y=size, 
                                           size = Percentage_greater_than_regional_size95)) +
  geom_point(alpha = 0.93, stat = "identity" ,aes(color = Region)) + 
  scale_shape_manual(values = c(15,16,17,18)) + 
  scale_color_manual(values = c("#b067a3", "#9c954d", "#bc7d39", "#697ed5")) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 22, colour = "black"),
        axis.title=element_text(size=24, colour = "black", face = "bold")) +
  theme(legend.text = element_text(size = 22), legend.position = "right") + 
  theme(legend.title = element_text(size = 24), legend.position = "right") + 
  guides(color = guide_legend(override.aes = list(size = 8))) +
  scale_size(range = c(3, 18), name="Percentage (> Regional Size     )") +
  labs(y = expression(Size[95]["/"][5]~(µm)), x = expression("Abundance (Total number of forams)"))
  
#Export Figure as JPEG

png("~/Desktop/Perc_greater_than_reg_size95_2.png",width=7800,height=4400,units="px",res=600,bg="white", pointsize = 8)
Perc_greater_than_reg_size95
dev.off()

#******************************************************************
#Figure 12 (Data + Code) -----------------------------------------
#******************************************************************

#Load Data (a):: Species specific response to environmental parameters

species_specific_response <- read.csv("species_specific_response_data.csv", header = TRUE, sep = ',')

#Plot Figure

species_specific_response_plot <- ggplot(species_specific_response_primary_copy_2, aes(y = r2rlm_1, axis1 = Parameter, axis2 = Species)) +
  geom_alluvium(aes(fill = Parameter),  aes.bind = "flow", curve_type = "arctangent", width = 1/4) +
  geom_stratum(width = 1/4, fill = "black", color = "white") +
  #geom_label(stat = "stratum", aes(label = after_stat(stratum)), face = "italic") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), 
            size = 5,fontface = "italic", colour = "white", check_overlap = TRUE) +
  scale_x_discrete(limits = c("Parameter", "Species"), expand = c(.1, .1)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 22, colour = "black")) +
  theme(axis.title=element_blank()) + 
  theme(plot.title = element_text(size = 23)) + 
  theme(legend.text = element_text(size = 22), legend.title = element_text(size = 23)) +
  labs(y = expression(Correlation ~ Co-efficient~(R^2))) + 
  guides(fill = guide_legend(title = "Parameters")) + 
  ggtitle("Species-specific Response to Environmental Variables") 

#Export Figure as JPEG

jpeg("~/Desktop/species_specific_response_exp10092022.jpg",width = 9500,height=7500,units="px",res=700,bg="white", pointsize = 8)
species_specific_response_plot
dev.off()

#******************************************************************
#Figure 13 (Data + Code) -----------------------------------------
#******************************************************************

#Load Data(a & b):: Size F1 Scores vs Surface Carbonate concentration and Size F2 Scores vs 2nd Order Polynomial Fit with SST 

All_core_data <- read.csv("All_core_site_data.csv", header = TRUE, sep = ',')

F1size_vs_surf_carb <- ggplot(All_core_site_data, aes(x=surf_carb, y=F1size)) + 
  geom_point(aes(colour = Region, shape = Region, size = 10)) +
  stat_smooth(method = "rlm", formula = y~x,col = "#000000",se = FALSE, size = 0.7, fullrange = T) +
  scale_shape_manual(values = c(15,16,17,18)) + 
  scale_color_manual(values = c("#b067a3", "#9c954d", "#bc7d39", "#697ed5"), 
                     guide = guide_legend(override.aes = list(size = 9))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  guides (size = "none") +
  theme(axis.text=element_text(size = 22, colour = "black"), 
        axis.title=element_text(size=24, colour = "black", face = "bold")) +
  theme(legend.text = element_text(size = 24), legend.title = element_text(size = 24)) +
  annotate("text", x = 255, y = 1.3, size = 6, colour ="black", 
           label = "italic(R)^{2}==0.41*','~italic(p)==1.18e-07", parse = TRUE) +
  labs(x = expression(Carbonate~conc.[surface] ~ (µm/kg)), y = expression("F1 Axis Scores"))


#(b) Size F2 Scores vs 2nd Order Polynomial Fit with SST

F2size_vs_SST <- ggplot(All_core_site_data, aes(x=sst, y=F2size)) + 
  geom_point(aes(colour = Region, shape = Region, size = 10)) + 
  stat_smooth(method = "lm", formula = y~poly(x,2),col = "#000000",se = FALSE, size = 0.7, fullrange = T) +
  scale_shape_manual(values = c(15,16,17,18)) + 
  scale_color_manual(values = c("#b067a3", "#9c954d", "#bc7d39", "#697ed5"),
                     guide = guide_legend(override.aes = list(size = 9))) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size = 22, colour = "black"), 
        axis.title=element_text(size=24, colour = "black", face = "bold")) +
  theme(legend.text = element_text(size = 22), legend.title = element_text(size = 24)) + 
  guides(size = "none") +
  annotate("text", x = 23, y = 1.6, size = 6, colour = "black", 
           label = "italic(R)^{2}==0.46*','~italic(p)==1.248e-08", parse = TRUE) +
labs(x = expression("SST ("*degree*"C)"), y = expression("F2 Axis Scores")) 

#Export Figures figures as JPG

jpeg("~/Desktop/Community Size Response06092022.jpg",width=15000,height=5000,units="px",res=600,bg="white", pointsize = 8)
ggarrange(ggarrange(F1size_vs_surf_carb,F2size_vs_SST, ncol = 2))
dev.off() 


png("~/Desktop/Community Size Response22082022_poster.png",width=59,height=36.26,units="cm",res=600,bg="white", pointsize = 8)
ggarrange(ggarrange(F1size_vs_surf_carb,F2size_vs_SST, ncol = 2))
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

jpeg("~/Desktop/Rillo Analyzed Plot08092022.jpg",width=6000,height=5000,units="px",res=500,bg="white", pointsize = 8)
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



