These utilities are intended to assist with post-processing photochemical model output files to quantify source impacts.

# uam2ncf

Program that converts CAMx output to CMAQ format for subsequent post-processing. This program requires IOAPI and netCDF4 libraries to be installed. This program was developed and is also distributed by Ramboll-Environ at www.camx.com

# hr2day

Program converts hourly data to more aggregated forms including MDA8 (8-hr daily maximum O3) and daily average PM2.5.

# combine

Program combines data aggregated using hr2day into a single file for further processing.

# iomath

Program estimates annual (or episode) average and maximum values for further processing.

# peakdist

Program extracts annual (or episode) average and maximum values near a source and outputs a text file for further processing.

# merpcalc

Program estimates MERPs using 'peakdist' output.

# ncf2ssia

Program generates ascii text file format for input to the Windows based SSIA tool for data browsing and plotting. 


