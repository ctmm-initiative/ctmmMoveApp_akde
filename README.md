# Autocorrelated Kernel Density Estimate (aKDE)

MoveApps

Github repository: **todo**

## Description
Calculate an autocorrelated kernel density estimate from a fitted continuous time movement model (ctmm). 

## Documentation
This app is part of a `ctmm`-workflow. The requires a fitted ctmm Model.

### Input data
The app requires a *ctmm model with data* as input. 

### Output data
*Indicate which type of output data the App produces to be passed on to subsequent apps. Currently only R objects of class `MoveStack` can be used. This will be extend in the future. In case the App does not pass on any data (e.g. a shiny visualization app), it can be also indicated here that no output is produced to be used in subsequent apps.*

*Example:* MoveStack in Movebank format

### Artefacts
The app create the following artefacts: 

- `app-output.rds`: 
- `homerange.gpkg`: A geopackage with the calculated home ranges.

*Example:* `rest_overview.csv`: csv-file with Table of all rest site properties

### Settings
*Since our switch to use shiny bookmarks for storing seleted settings made in the UI, it is not possible any more to have settings in MoveApps to set before running the App and opening the UI. Instead a `store settings` button is included in the UI.

`Store settings`: click to store the current settings of the app for future workflow runs

### Most common errors
The app currently does not check which movement model was selected. If the animal does not exhibit home-ranging behaviour, then calculating an aKDE does not make sense and the app may run for a very long time wihtout meaningful results. 

### Null or error handling
*Please indicate for each setting as well as the input data which behaviour the App is supposed to show in case of errors or NULL values/input. Please also add notes of possible errors that can happen if UI settings/parameters are improperly set and any other important information that you find the user should be aware of.*

*Example:* **Setting `input$radius`:** If no radius AND no duration are given, the input data set is returned with a warning. If no radius is given (NULL), but a duration is defined then a default radius of 1000m = 1km is set. 
