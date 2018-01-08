These scripts combine aggregated data into a single file (combine)

run.12US2.O3N.hyposources.job
        This script runs the combine program.
        Inputs are monthly files with MDA8 ozone for the bulk model and tagged sources
        Ancillary input is the species definition file: species.keyspecs.O3N.master
        Output is a single file with the entire year (or time period) of MDA8 ozone values for bulk model and tagged sources

run.12US2.PM.hyposources.job
        This script runs the combine program.
        Inputs are monthly files with daily average PM for the bulk model and tagged sources
        Ancillary input is the species definition file: species.keyspecs.PM.master
        Output is a single file with the entire year (or time period) of daily average PM values for bulk model and tagged sources

The 'combine' program is actively maintained and available here:
https://github.com/USEPA/CMAQ/tree/5.2/POST

