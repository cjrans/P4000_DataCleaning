

# Libraries ---------------------------------------------------------------


library(tidyverse,lib.loc = "C:/Users/cjr8bf/Documents/R/R-4.0.5/library")
library(cowplot)
# library(ggtext)


# File locations ----------------------------------------------------------

# workingdirectory <- "~/1. Projects/P4000 Data Cleaning/P4000_DataCleaning"
# output_location <- "~/1. Projects/P4000 Data Cleaning/P4000_DataCleaning/processed_data"
workingdirectory <-"C:\\Users\\cjr8bf\\OneDrive - University of Missouri\\P4000 pre-processing\\North\\Palmer 9 Nov 2021 North"
output_location <- "C:\\Users\\cjr8bf\\OneDrive - University of Missouri\\P4000 pre-processing\\P4000 processed\\Palmer North"
setwd(workingdirectory)


temp = list.files(pattern="loc")
#read in file


field_spectra_file <- read_csv(temp)


#renaming and selecting pertentant columns
#transposing the data into a longer format (each row has a unique wavelen)

spectra2 <- field_spectra_file %>% 
  dplyr::select(loc=Loc, scan=Probe, depth=Depth, EC=EC_SH, Force, "343.23":ncol(field_spectra_file)) %>% 
  pivot_longer(cols = 6:389,
               names_to ="wavelen",
               values_to = "OD") %>% 
  relocate(wavelen, loc, scan, depth, OD, EC, Force) %>% 
  mutate(wavelen =as.numeric(wavelen))


#output as BIGout.csv; 
#column names: [1] "wavelen", [2] "loc" [3] "scan", [4] depth, [5] "OD" = optical measurment, [6] "EC", [7] Force

write.csv(spectra2, "BIGout.csv", row.names = FALSE)



# Adjusting Depth ---------------------------------------------------------

spectra_depth_adj <- spectra2 %>% 
  mutate(depthINV = depth *-1,
         depthFRC = depthINV, 
         depthEC = depthINV + 0.02,
         depthOPT = depthINV + 0.09)


OPT_data <- spectra_depth_adj %>% 
  filter(depthOPT <= 0) 


locations <- unique(OPT_data$loc)

for(i in locations) {
  print(i)
  #LocECFRC
  locECFRC <- spectra_depth_adj %>% 
    filter(loc == i)
  
  write.csv(locECFRC, file = paste(output_location,"/locECFRC",i,".csv", sep=""), row.names=FALSE)

  #optical data
  locOPT <- OPT_data %>% 
    filter(loc ==i)
  
  write.csv(locOPT, file = paste(output_location,"/LOC",i,".csv", sep=""), row.names = FALSE)
  

  P1 <- locOPT %>%
    # filter(scan==1) %>%
    ggplot(aes(x=wavelen , y=depthOPT, color=OD)) +
    ggtitle(label = paste(i)) +
    geom_point() +
    binned_scale(aesthetics = "color",
                 scale_name = "stepsn",
                 palette = function(x) c("#000066", "#0040FF", "#00AAFF", "#00FFEB", "#00FF7F","#FFFF00","#FF6633","#FF0000"),
                 breaks = c(0.648, 0.70580, 0.7575569, 0.8130966, 0.9337078, 1.072371,1.28377, 2),
                 limits = c(-1, 2),
                 # show.limits = TRUE,
                 # guide = "colorsteps"
                 
    ) +
    theme_classic() +
    theme(legend.position = "none")

  #individaul scans; OD ~ wavelen, colored by depth
  Wavscan1 <- locOPT %>%
    filter(scan==1) %>%
    ggplot(aes(x=wavelen, y = OD, color=depthOPT)) +
    geom_point()




  #EC
  P2 <- locECFRC %>%
    mutate(scan = as.factor(scan)) %>%
    ggplot(aes(x=EC , y=depthEC, color=scan, fill=scan)) +
    geom_point(size=2, alpha=0.5) +
    theme_classic() +
    theme(axis.text.x = element_text(size=10),
          legend.position = "none")
    # theme(axis.text.x = element_markdown(size=5))


  #force
  P3 <- locECFRC %>%
    mutate(scan = as.factor(scan)) %>%
    ggplot(aes(x=Force , y=depthEC, color=scan, fill=scan)) +
    geom_point(size=2, alpha=0.5) +
    theme_classic() +
    theme(axis.text.x = element_text(size=10, angle =-90, hjust=0),
          legend.position = c(0.1,0.2))
  

  plot_grid(P1,P2,P3, nrow=1, ncol =3, rel_widths = c(0.5,0.25,0.25))
  
    
}

rgb(255,0,0,maxColorValue = 255)

#develop a function to save in order to nest over everything, 
#within the function graph

#Figure of depth ~ wavelen, colored by value



#figure of depth ~ force, colored by scan



spectra2 %>% filter(loc=="S7", scan==2, depth <0.20 & depth>0.09) %>% ggplot(aes(x=wavelen, y =OD, color=depth)) + geom_point() +ggtitle("North Fall  2021 (9 to 20 cm)") +
  ylim(-0.5, 1.75)


a <- read_csv("W:\\Sudduth Lab\\Soil Sensing Projects\\2020-21 Stamps\\Svedin-Ryan Britt SH 3\\processed_data\\BIGout.csv")


a %>% filter(loc=="P-30", scan==1,depth <0.20 & depth>0.09) %>% ggplot(aes(x=wavelen, y =OD, color=depth)) + geom_point() +ggtitle("Pasture Spring  2021 (9 to 20 cm)") +
  ylim(-0.5, 1.75)
