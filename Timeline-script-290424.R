install.packages("vistime")
install.packages("pals")
### Load packages
library("vistime")
library("tidyverse")
library("RColorBrewer")
library("scales")
library("cowplot")
library("pals")

# Define number of colours needed
cols_n2 <- as.numeric(n_distinct(ward_moves_Zag_280424$Ward))
# Check we have enough colours
#ifelse(cols_n2>12, 
       #"More than 12 wards - alphabet!",
       #"12 wards or fewer - using set1 colours")
# Select the colours from Set3 palette in RColorBrewer
#cols_to_use2 <- brewer.pal(n = cols_n2, "set1")

#custom_palette <- rainbow(13)

#cols_to_use2 <- setNames(custom_palette, n=cols_n2)

#wards <- c("Cardiology ICU", "Cardiology ward", "COVID19 ICU", "COVID19 ward", 
           #"Internal Medicine ICU", "Neurology ICU", "Neurology ward",
           #"Pulmonary ICU", "Surgical ICU", "Neurosurgical ICU", "Thoracic ICU",
           #"Pulmonary ward","Emergency")
ward_color <- c("red4","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33",
                "#A65628","#F781BF","#999999", "blue",
                "steelblue4", "lightblue4", "khaki4")
#cols_to_use2 <- setNames(ward_color, wards)
#ward_moves_Zag_041223_2
# Create mapping of colours to wards
col_ward_mapping2 <- data.frame(Ward=unique(c(as.character(ward_moves_Zag_280424$Ward))), color=ward_color)
# merge in the mapping to the df
ward_moves_Zag_280424_3 <- merge(ward_moves_Zag_280424,
                          col_ward_mapping2,
                          by="Ward",
                          all.x=T,all.y=T) %>%
  select(Patient, swab_date, Ward, start, end, color) %>%
  arrange(swab_date, Patient, start)
ward_moves_Zag_280424_3

## Extract swab dates
swab_dates_2_2 <- ward_moves_Zag_280424_3 %>%
  select(Patient, swab_date) %>%
  distinct(Patient, .keep_all=TRUE) %>%
  arrange(swab_date)


### Plotting
# Produce the basic plot
plot_timeline_zagreb_290424 <- gg_vistime(data = ward_moves_Zag_280424_3,
                        col.group = "Patient", # Each row will be a patient
                        col.event = "Ward", # Rows will be coloured by the ward
                        show_labels = FALSE, # Remove labels indicating the ward
                        linewidth = 4,
                        title = "Spatial and temporal relationship of the patients")

# Tweak the plot
plot_timeline_zagreb_290424 <- plot_timeline_zagreb_290424 + theme_light()+
  ggplot2::theme(
    plot.title = element_text(size=25),
    axis.text.x = element_text(size = 20, color = "black", angle = 0, vjust = 1, hjust = 1.5),
    axis.text.y = element_text(size = 15, color = "black")) +
  scale_x_datetime(breaks = breaks_width("3 month"), labels = date_format("%b-%Y"))
plot_timeline_zagreb_290424

# Adding date of positive swab
plot_timeline_zagreb_290424 <- plot_timeline_zagreb_290424 +
  annotate("point", x = as.POSIXct(swab_dates_2[1,2]), y = 91, size = 2, colour = "black") +
  annotate("point", x = as.POSIXct(swab_dates_2[2,2]), y =89, size = 2, colour = "black") +
  annotate("point", x = as.POSIXct(swab_dates_2[3,2]), y = 87, size = 2, colour = "black") +
  annotate("point", x = as.POSIXct(swab_dates_2[4,2]), y = 85, size = 2, colour = "black") +
  annotate("point", x = as.POSIXct(swab_dates_2[5,2]), y = 83, size = 2, colour = "black") +
  annotate("point", x = as.POSIXct(swab_dates_2[6,2]), y = 81, size = 2, colour = "black") +
  annotate("point", x = as.POSIXct(swab_dates_2[7,2]), y =79, size = 2, colour = "black") +
  annotate("point", x = as.POSIXct(swab_dates_2[8,2]), y = 77, size = 2, colour = "black") +
  annotate("point", x = as.POSIXct(swab_dates_2[9,2]), y = 75, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[10,2]), y = 73, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[11,2]), y = 71, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[12,2]), y = 69, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[13,2]), y = 67, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[14,2]), y = 65, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[15,2]), y = 63, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[16,2]), y = 61, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[17,2]), y = 59, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[18,2]), y = 57, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[19,2]), y = 55, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[20,2]), y = 53, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[21,2]), y = 51, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[22,2]), y = 49, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[23,2]), y = 47, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[24,2]), y = 45, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[25,2]), y = 43, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[26,2]), y = 41, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[27,2]), y = 39, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[28,2]), y = 37, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[29,2]), y = 35, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[30,2]), y = 33, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[31,2]), y = 31, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[32,2]), y = 29, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[33,2]), y = 27, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[34,2]), y = 25, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[35,2]), y = 23, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[36,2]), y = 21, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[37,2]), y = 19, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[38,2]), y = 17, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[39,2]), y = 15, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[40,2]), y = 13, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[41,2]), y = 11, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[42,2]), y = 9, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[43,2]), y = 7, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[44,2]), y = 5, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[45,2]), y = 3, size = 2, colour = "black")+
  annotate("point", x = as.POSIXct(swab_dates_2[46,2]), y = 1, size = 2, colour = "black")
plot_timeline_zagreb_290424 <- plot_timeline_zagreb_290424 + theme(title = element_text(face = "bold", size = 30),
                               axis.text.x = element_text(face = "bold", 
                                                          size = 16, vjust = 1,
                                                          hjust = 1),
                               axis.text.y = element_text(face = "bold", 
                                                          size = 14))

plot_timeline_zagreb_290424 <- plot_timeline_zagreb_290424 + theme(axis.text = element_text(family = "Helvetica"),
                  axis.text.y = element_text(family = "Helvetica"),
                  axis.title = element_text(family = "Helvetica"))
### Create a legend
data_legend <- ward_moves_Zag_280424_3 %>%
  distinct(Ward, .keep_all=T) %>%
  arrange(Ward)
data_legend$start <- as.Date("2020-01-01")
data_legend$end <- as.Date("2020-01-02")
data_legend$Patient <- "Key"
data_legend
plot_legend_timeline_zagreb_290424 <- gg_vistime(data = data_legend,
                          col.group = "Patient",
                          col.event = "Ward", 
                          show_labels = TRUE,
                          linewidth = 10,
                          title = NULL)
                
plot_legend_timeline_zagreb_290424

# Tweak the legend plot
plot_legend_timeline_zagreb_290424 <- plot_legend_timeline_zagreb_290424 + theme_void() +
  ggplot2::theme(
    plot.title = element_blank(),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank())
plot_legend_timeline_zagreb_290424


### Combine the main plot and legend into a single figure
Timeline_Zagreb <- plot_grid(plot_timeline_zagreb_290424, plot_legend_timeline_zagreb_290424,
                             rel_widths = c(1, 0.13), nrow = 1, ncol = 2)
Timelineplot_Zagreb_290424 <- plot_timeline_zagreb_290424

ggsave(path = "/Users/Ifeoluwa1/Documents/zagreb/", filename ="Timelineplot_Zagreb_290424.svg",
       plot = Timelineplot_Zagreb_290424, device = "svg", dpi = 300, units = c("in"), width = 15, height = 10)
ggsave(path = "/Users/Ifeoluwa1/Documents/zagreb/", filename ="plot_legend_timeline_zagreb_290424.svg",
       plot = plot_legend_timeline_zagreb_290424, device = "svg", dpi = 300, units = c("in"), width = 15, height = 10)
