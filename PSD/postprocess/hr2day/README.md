These scripts estimate MDA8 for O3 and daily average for PM2.5 using the 'hr2day' program.

/hr2day/run.O3N.job
        This script runs the hr2day program
        Inputs are monthly files with hourly ozone (bulk and tagged sources)
        Ancillary input file is a text file with time zone data; this does not need to be modified.
        Outputs are monthly files with MDA8 ozone for the bulk model and tagged sources

/hr2day/run.PM.job
        This script runs the hr2day program
        Inputs are monthly files with hourly PM (bulk and tagged sources)
        Ancillary input file is a text file with time zone data; this does not need to be modified.
        Outputs are monthly files with daily averaged PM for the bulk model and tagged sources

The 'hr2day' program is actively maintained and available here:
https://github.com/USEPA/CMAQ/tree/5.2/POST

