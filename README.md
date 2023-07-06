# Autocorrelated Kernel Density Estimate (aKDE)

MoveApps

Github repository: https://github.com/ctmm-initiative/moveapps_ctmm_akde/

## Description
Calculate an autocorrelated kernel density estimate of your tracked animals' home-ranges from a fitted continuous time movement model (ctmm). 

## Documentation
This app is part of a `ctmm`-workflow. It requires that a fitted ctmm model is calculated using the `Fit a Continuous-Time Movement Model (ctmm)` App before running it.

Autocorrelacted kerned density estmates (aKDEs) are estimates of resident animals' range distribution that take into account the autocorrelation of telemetry/tracking data via autocorrelated kernel density estimation. Please see the [AKDE vignette](https://cran.r-project.org/web/packages/ctmm/vignettes/akde.html) or the [publication about the ctmm pacakge and aKDEs](https://doi.org/10.1111/2041-210X.12559) for further details.

### Input data
The app requires a *ctmm model with data* as input. 

### Output data
ctmm UD with Data and Model including the akde UD for all tracks

### Artefacts

`homerange.gpkg`: A geopackage with the calculated akde home ranges.

`akde_summary.txt`: Summary of the calculated home ranges. (PLEASE DEFINE THE DIFFERENT COLUMNS IN THE TABLE - e.g. home range size in km^2...)


### Settings

`Isopleth level`: the isopleth level for which the home range is calculated and shown on the map. (PLEASE DESCRIBE WHAT ISOPLETH IS)

`Store settings`: click to store the current settings of the app for future workflow runs. 

### Most common errors
Please make an issue here if you repeatedly encounter a specific error.

### Null or error handling


