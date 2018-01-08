# combine

Extracts raw CMAQ format hourly O3 into a single file for further processing.

# hr2day

Calculates MDA8 (daily maximum 8-hr O3) using the hourly output generated from the 'combine' step.

# iomath

Calculates episode average and maximum MDA8 values using output generated from the 'hr2day' step.

# ncf2ssia

Generates an ascii text file for input to the Windows based SSIA program for data browsing and plotting for quality assurance purposes.

