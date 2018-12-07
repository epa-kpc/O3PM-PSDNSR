This directory contains R scripts to 1) estimate daily trade ratios and 2) make Q/A plots. 

This directory also contains the netCDF/IOAPI file that contains the grid cells considered part of the illustrative "area" for the example demonstration presented in the appendix B of the O3 IPT TGD. 

# domain plot v1.R

Makes a plot of the model domain and shows which cells are part of the "area". The netCDF/IOAPI file of the model domain and cells included in the hypothetical region were generated using the Spatial Allocator tool (https://www.cmascenter.org/sa-tools/). Many tools could be used to develop this type of file and alternatively the ratio calculator R script could be modified to read a completely different type of file format to provide this information. 

# ratio calculator v1.R

Makes a table of daily impacts and the ratio. 

# spatial plots abs impact for one source v1.R

Daily Q/A plots of absolute impacts

# spatial plots ratios windowed v3.R

Daily Q/A plots for ratio of impacts
