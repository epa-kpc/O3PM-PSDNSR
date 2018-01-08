These scripts combine bulk and tagged contributions into monthly files that contian hourly data using the 'uam2ncf' and 'combine' programs. The 'uam2ncf' program converts CAMx output to the format used by CMAQ for further post-processing.

run.12US2.O3N.hyposources.job
        This script runs the uam2ncf program and also combine program.
        Inputs are raw CAMx output files - daily regular model output and source apportionment output files.
        Ancillary input file is a species definition file: species.camx.keyO3Nspecs.master
        Outputs are monthly files with hourly ozone including bulk model and tagged sources

run.12US2.PM.hyposources.job
        This script runs the uam2ncf program and also combine program.
        Inputs are raw CAMx output files - daily regular model output and source apportionment output files.
        Ancillary input file is a species definition file: species.camx.keyPMspecs.master
        Outputs are monthly files with hourly PM including bulk model and tagged sources

The 'uam2ncf' program is distributed by Ramboll at:
www.camx.com

The 'combine' program is actively maintained and available here:
https://github.com/USEPA/CMAQ/tree/5.2/POST


