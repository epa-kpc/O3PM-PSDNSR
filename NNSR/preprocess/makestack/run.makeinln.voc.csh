#!/bin/csh -f
# Makeinln script for generating inline and stack groups files for single point source emissions
# Input: file containing stack locations, stack parameters, pollutant mass values
# Output: CMAQ ready inline single stack point source emissions


### User defined settings ###
#----------------------#
  ## Set the workpath where this script is run
  set WORKPATH = $cwd

  ## Define the IOAPI output grid
  setenv GRID 12US2

  ## Pollutants to process
  set pollutants = (VOC)

  ## Pollutant mass (tons/yr)
  set mass = 1000 

  ## Output label
  set label = '2011_voc'

  ### Source stack parameters
    ## Set the stack height of the source (m)
    setenv HEIGHT 3.048
    ## Set the stack diameter of the source (m)
    setenv DIAM   1.524
    ## Set the stack velocity of the source (m/s)
    setenv VELOC 294.322
    ## Set the stack temperature (K)
    setenv TEMP 311

  ### Stack source point locations
    ## Path to file containing lat and lon for each stack
    ##  Format of file must follow <stack number integer>,<latitude>,<longitude> 
    set stacklocs = $WORKPATH/stacklocations_12US2.csv
    ## Set the first stack number to generate in the loop
    set start_stack = 1 
    ## Set the last stack number to generate in the loop
    set end_stack = 36

#----------------------#
### End user defined settings ###




set OUTPATH = $WORKPATH/netcdf
if (! -d $OUTPATH) then
    mkdir -p $OUTPATH
endif

setenv QAFILE $cwd/qafile_${mass}_${label}.txt
rm -rf $QAFILE

echo "$mass TPY   $HEIGHT $DIAM $VELOC $TEMP" > $QAFILE
echo $pollutants >> $QAFILE

# Define the input emissions template and the temporary emissions matrix
#   This is a provided ancillary file that controls how variables are set in
#   the netCDF files. Pollutants may be added and species rate factors
#   can be adjusted as required.
set emissions_template = $WORKPATH/ancillary/stack_emissions_template.txt

set stack = $start_stack
while ($stack <= $end_stack)

    setenv EMISINPUT $WORKPATH/stack_emissions_$label.txt
    if (-e $EMISINPUT) then
        rm -f $EMISINPUT
    endif
    head -13 $emissions_template > $EMISINPUT

    # convert tons/year to tons/hr
    set emis_value = `awk -v mass=$mass 'BEGIN {print mass/8760}'`

    # Set the number of output species based on the pollutants
    set species_count = 0
    foreach poll ($pollutants)
        @ species_count += `grep -c "Q${poll}Q" $emissions_template`
        grep "Q${poll}Q" -B1 -A2 $emissions_template >> $EMISINPUT 
        perl -pi -e "s/Q${poll}Q/$emis_value/g;" $EMISINPUT
    end

    set stackline = `grep "^${stack}," ${stacklocs}`

    setenv scenario ${label}_${mass}tpy_location${stack}
    setenv LAT `echo $stackline | cut -d',' -f2`
    setenv LONG `echo $stackline | cut -d',' -f3`

    echo  "  loc $stack : $LAT $LONG" >> $QAFILE

    perl -pi -e "s/QGRIDQ/$GRID/g;" $EMISINPUT
    perl -pi -e "s/QLATQ/$LAT/g;" $EMISINPUT
    perl -pi -e "s/QLONGQ/$LONG/g;" $EMISINPUT
    perl -pi -e "s/QSPECQ/$species_count/g;" $EMISINPUT
    perl -pi -e "s/QHEIGHTQ/$HEIGHT/g;" $EMISINPUT
    perl -pi -e "s/QDIAMQ/$DIAM/g;" $EMISINPUT
    perl -pi -e "s/QTEMPQ/$TEMP/g;" $EMISINPUT
    perl -pi -e "s/QVELQ/$VELOC/g;" $EMISINPUT

    ### Step 1: Build new source emissions input file for CMAQ
    setenv PROMPTFLAG       Y
    setenv IOAPI_ISPH       6370000
    # IOAPI grid description file. Provided version contains most EPA grids.
    setenv GRIDDESC         $WORKPATH/ancillary/griddesc_lambertonly_01mar2017_v75.txt
    setenv INLINE           $OUTPATH/ptsr.$GRID.$scenario.ncf
    setenv STACK_GROUP      $OUTPATH/stack.$GRID.$scenario.ncf

    rm -f $INLINE
    rm -f $STACK_GROUP

$WORKPATH/create_pt_emission.x  << EOF 
CMAQ
LATLONG

N



EOF

    rm -f $EMISINPUT

    @ stack++

end #loop over stacks

exit()

