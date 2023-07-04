# Autocorrelated Kernel Density Estimate (aKDE)

MoveApps

Github repository: https://github.com/ctmm-initiative/moveapps_ctmm_akde/

## Description
Calculate an autocorrelated kernel density estimate from a fitted continuous time movement model (ctmm). 

## Documentation
This app is part of a `ctmm`-workflow. The requires a fitted ctmm Model.

### Input data
The app requires a *ctmm model with data* as input. 

### Output data

*Example:* MoveStack in Movebank format

### Artefacts
The app create the following artefacts: 

`homerange.gpkg`: A geopackage with the calculated home ranges.

`akde_summary.txt`: Summary of the calculated home ranges.




### Settings

`Isopleth level`: the isopleth level for which the home range is calculated and shown on the map. 

`Store settings`: click to store the current settings of the app for future workflow runs. 

### Most common errors

### Null or error handling
