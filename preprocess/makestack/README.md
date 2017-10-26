#########
Single Source Emissions Processor
Documentation

Last updated 26 July 2017
J. Beidler <beidler.james@epa.gov>
#########


Purpose:
--------------
The single source emissions processor scripts are used to generate CMAQ model ready inline
emissions and stack groups files for a single point source. Scripts are included to generate
CMAQ model ready emissions for 36 sources emitting NOX, CO and SO2 (task 1) and for 36 sources
emitting VOC (task 2). The scripts have configuration options for setting stack parameters, 
emissions rates and locations for each source.


Requirements:
--------------
- Unix system with csh 
- IOAPI 3.1 or later and netCDF 4.1.2 or later
- A FORTRAN compiler if building create_pt_emission from source 


Files:
--------------
The following files are contained in this package.

create_pt_emission.x         --  Binary executable for generating CMAQ model ready IOAPI files 
README                       --  This file. 
stacklocations_12US2.csv     --  Locations information for each stack by label number and lat/lon
run.makeinln.co_so2_nox.csh  --  csh script for generating NOX, CO and SO2 emissions (task 1)
run.makeinln.voc.csh         --  csh script for generating VOC emissions (task 2)
ancillary/griddesc_lambertonly_01mar2017_v75.txt  --  model grid description file 
ancillary/stack_emissions_template.txt  --  template control file; definitions of speciation factors
src/create_pt_emission.F     -- FORTRAN source of binary executable 


Configuration:
--------------
The provided csh scripts are configured to generate CMAQ model ready inline
emissions and stack groups files.
Each user configurable emissions rate and stack parameter variable is set at the top of the script.
 - WORKPATH: Run path where scripts are located and where output files are written
 - GRID: Output IOAPI compatible grid (default 12US2)
 - pollutants: list of pollutants to include in processing; only NOX, SO2, CO and VOC are supported
 - mass: mass based emissions rate in tons per year (both tasks use 250, 500 and 1000) 
 - label: label for output files to differentiate single source runs
 - HEIGHT: stack height of the source in meters
 - DIAM: stack diameter of the source in meters
 - VELOC: stack velocity of the source in meters per second
 - TEMP: stack temperature in Kelvin
 - stacklocs: path to stack locations information file (format: stack_number,latitude,longitude)
 - start_stack: first stack location number to process with defined stack parameters and emissions
 - end_stack: last stack location number to process with defined stack parameters and emissions


Running:
--------------
To generate single source stack emissions for CO, SO2 and NOX the run.makeinln.co_so2_nox.csh 
should be configured and executed for each modeled emissions rate: 250 tons/yr, 500 tons/yr 
and 1000 tons/yr. As packaged no other configuration is necessary to generate task 1 single
source emissions.
To generate single source stack emissions for VOC the run.makeinln.voc.csh should be
configured and executed for each modeled emissions rate: 250 tons/yr, 500 tons/yr 1000 tons/yr. 
As packaged no other configuration is necessary to generate task 2 single source emissions.

Each script can be executed by making them executable and your system (chmod +x <scriptname>) and
running with ./<scriptname>

The scripts will automatically apply the pollutant emissions rate to the speciation factors in the
stack_emissions_template.txt file to create flat hourly emissions rates in native units.


Output:
--------------
Logs detailing the emissions rate, pollutants, stack parameters and locations of each run are
automatically generated in the WORKPATH directory. The log prefix is qafile and is followed by
the emissions rate and label of the run.

The CMAQ model ready inline emissions files and the stack groups files are output to the netcdf
subdirectory. The inline and stack groups file have a prefix of ptsr and stack respectively. The
remainder of the file names are formatted with output grid, label, emissions rate and location
number.   
For example, the inline and stack groups file for location 27 using task 1 stack parameters and
a pollutant emissions rate of 500 tons per year will produce the files: 
ptsr.12US2.2011_nox_so2_co_500tpy_location27.ncf and stack.12US2.2011_nox_so2_co_500tpy_location27.ncf


QA:
--------------
Basic quality assurance of the CMAQ ready output files can be performed using the ncdump netCDF
application. 
Each inline ptsr file should contain one source and 25 hours of emissions. The emissions rates in
the inline files can be manually verified using the input emissions rate and the speciation 
factors in the stack_emissions_template.txt file.
Each stack groups file can be verified by comparing the stack parameters and location to the input
parameters and the input location of the respective stack number.




