<h2 id="About">
Flood_forecasting - Visualization tool for flood forecasters at NVE
</h2>
A Shiny app to present flood forecasting results at NVE. If you find a bug or think of an interesting new feature, please file a new [issue](https://github.com/fbaffie/Flood_forecasting/issues). Please refer to the guidelines for filing issues at the end of this readme.

----
Table of contents

-   [Aim](#Aim)
-   [Structure](#Structure)
-   [Models](Models)
     - [HBV with uncertainty model (HBV_UM)](#HBV_UM)
     - [HBV with +/- 50% precipitation (HBV_P)](#HBV_P)
     - [DDD](#DDD)
     - [ODM](#ODM)
     - [Data Driven Model (DDM)](#DDM)
-   [Other datasets](#datasets)
     - [Return levels](#rls)
     - [Probability of flooding](#prob)
     - [Station metadata](#metadata)
-   [Running the app](#run)
     - [From GitHub](#github)
     - [Locally](#local)
-    [Filing issues](#issues)

<h2 id="Aim">
Aim
</h2>
Provide the flood forecasters with an easy to use and customizable graphical user interface (GUI) for flood forecasting and model evaluation.

<h2 id="Structure">
Structure
</h2>
The GUI is based on R Shiny, with the following tabs:

1. Tab 1: Station selection (returns a list of one or more stations) based on 4 methods (many inputs, a few methods, one output list(stations)):
 * Station identification number or name
 * Logic on catchment properties and model performance: coordinates, area, elevation, previous model performance...
 * Map selection: click or draw polygon
 * Table selection: navigate on a big DT table (with similar variables as in the logic method) and click on the stations you want

This station selection would update every other tab that uses groups of stations but the other tabs would also have a simple selection tool (i.e. regine + main numbers)

2. Tab 2: FORECASTER'S DASHBOARD - plots and performance of model runs (-7 to +9 days). Only for a single station?
 * Selection of a single station based on region_number, main_number, clicking on map
 * Map markers could represent a selectable variable (temperature / precip...)
 * Plot (plotly, highchart or dygraph to have interactive plot features such as adding/removing lines) of model runs
     - The plot should include flood levels: mean flood and 5 year return level. The plots could be ticked out by default and then plotted when the forecast is within X% of those levels.
     - The plot should also include Temperature and precip. If it becomes messy with y scales, the mouse tip indicator with plotly is enough.
     - Would be good to highlight the current day somehow.
 * Summary of model performance. There is the tricky question of how long of a time-interval to use. All perf indicators should use the same interval. 1 year is probably a good choice
     - NSE
     - KGE
     - Taylor diagram
     - A hit/miss/false alarm/correct reject table could be interesting. But, if the timelapse is 1 year, the return level threshold should be discussed, 1 year return period being the maximum we could use.

3. Tab 3: MODELLER'S DASHBOARD - Analysis of model performance / properties during the calibration period.
 * time series graphs, available time period, zoom functionality, tick on/off which models:
 * discharge: observations and all models
 * temperature and precipitation
 * states/water balance: snow, sub-surface water, evaporation
 * Running bias: accumulated difference observed – modelled discharge
 * model performance
     - NSE
     - KGE
     - hit/miss/false alarm/correct reject table (And / Or success index) on calibration runs. This is not a forecast quality measure but is still useful for assessing the model performance
     - Taylor diagram

4. Tab 4: Regional analysis of model runs
  * Ouputs one plot per station of the model runs
  * Should this include a map with polygon selection or should the group of stations only originate from tab 1?

Historical hindsight: pull out past interesting events for a specific station or group of stations

Not all the files have data for the same number of stations (120-145). Check how the station numbers are organized.
Those files don't include catchment properties, so we will have to agree on the data files for those properties.
The observed flow and precip/temperature are included in all of those files.

To be continued...

*[Back to top](#About)*

<h2 id="Models">
Models
</h2>

Results from the daily model runs:

<h4 id="HBV_UM">
HBV with uncertainty model (HBV_UM)
</h4>

HBV with uncertainty model: /hdata/drift/flom/usikkerhet_grd/utskrift/vfpost_usikkerhet.txt 
 * Pure model run (not processed)
 * Updated: automated update using observed streamflow
 * Output of the uncertainty model: median result based on a distribution of meteo input data
 * A few quantiles (take 5%, 95%), also based on the distribution of meteo data
Ask Gusong if questions

<h4 id="HBV_P">
HBV with +/- 50% precipitation (HBV_P)
</h4>

HBV with + 50% precipitation: /hdata/drift/flom/usikkerhet_grd/ut_test/vfpost_usikkerhet.txt
 * Pure model run (not processed): this is in the dataset but it is the same as in 1., so shouldn't be plotted!
 * Updated: automated update using observed streamflow (different method)
 * Results from a run with precip + 50%
Ask Gusong if questions

<h4 id="DDD">
DDD
</h4>

DDD model 24 hours: /hdata/drift/flom/DDD24h2015R/24hres.txt

<h4 id="ODM">
ODM
</h4>

ODM model 24 hours: /hdata/drift/flood/H-VEPS02/simu_hbv_elev_24h/<catchment>/InputTimeSeries.txt
 * Pure model
 * Updated model
Ask Bård if questions 

<h4 id="DDM">
DDM
</h4>

Data driven model...


Potential issue with data:
The starting date of those files is fixed but sometimes changed!
So the length of data is variable (kin of random).
So, if we want to implement a feature that pulls the x interesting events in the last year, that cuold be a problem

This is not a problem for implement the DATE -7 / +9 days model plots.

*[Back to top](#About)*

<h2 id="datasets">
Dataset
</h2>

<h4 id="rls">
Return levels
</h4>
Flomtabell.rap

<h4 id="prob">
Probability of flooding
</h4>
take max(next 3days of probability "number of occurence in ensemble" for mean flood) as variable. risiko.rap there: Z:\flom\usikkerhet_grd\utskrift

<h4 id="metadata">
Station metadata
</h4>
A large variety of metadata for all stations come directly from the [`NVEDATA`](https://github.com/NVE/NVEDATA) package.

<h2 id="run">
Running the app
</h2>

<h4 id="github">
Directly from GitHub
</h4>

Not working at the moment
Load the shiny package and run the app:
```library(shiny)
runGitHub("Flood_forecasting", "fbaffie")```

If you want to update the data used by the app, first press the "Stop" button on RStudio to stop the app (Top right of the console).
Then run again:
```library(shiny)
runGitHub("Flood_forecasting", "fbaffie")```

<h4 id="local">
Locally
</h4>


*[Back to top](#About)*

<h4 id="issues">
Filing issues
</h4>

Please try to follow those guidelines for filing issues:

One issue for one purpose. Don't add more than one bug, feature request, documentation request, question, etc.. on to the same issue.

- If you've found a bug, thanks for reporting!
- If you've a request of some kind, e.g., feature request or documentation request, it'd be much appreciated if you could add **[Request]** at the beginning of the title. This helps us to prioritise easily without having to go through the entire issue.
- If you need support, e.g., installation issues or upgrade issues, please add **[Support]** at the beginning of the title. This helps us to easily identify the most common support issues, and provide solutions in a separate page.
- If you have a general question, add **[Question]** at the beginning of the title.
- If you've an issue that doesn't fall into any of these categories, then it'd be helpful if you could add **[Misc]** to your title.
