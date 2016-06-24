# Flood_forecasting
A Shiny app to present flood forecasting results at NVE.

## Aim
Provide the flood forecasters with an easy to use and customizable graphical user interface (GUI) for flood forecasting and model evaluation.

## Structure
The GUI will be based on R Shiny, with the following tabs:

1. Tab 1: Station selection (returns a list of one or more stations) based on 4 methods:
  * Station identification number or name
  * Logic on catchment properties: coordinates, area, elevation...
  * Map selection: click or draw polygon
  * Table selection: navigate on a big DT table and click on the stations you want

2. Tab 2: plots and performance of model runs (-7 to +9 days). Only for a single station?
  * Selection of a single station based on region_number, main_number, clicking on map
  * Map markers could represent a selectable variable (temperature / precip...)
  * Plot (plotly, highchart or dygraph) of model runs
  * Summary table of model performance

3. Tab 3: Regional analysis of model runs
  * Ouputs one plot per station of the model runs
  * Should this include a map with polygon selection or should the group of stations only originate from tab 1?

To be continued...

## Dataset

Not all the files have data for the same number of stations (120-145). Check how the station numbers are organized.
Those files don't include catchment properties, so we will have to agree on the data files for those properties.
The observed flow is included in all of those files.

Results from the daily model runs:
1. HBV with uncertainty model: /hdata/drift/flom/usikkerhet_grd/utskrift/vfpost_usikkerhet.txt 
 * Pure model run (not processed)
 * Updated: automated update using observed streamflow
 * Output of the uncertainty model: median result based on a distribution of meteo input data
 * A few quantiles (take 5%, 95%), also based on the distribution of meteo data
Ask Gusong if questions

2. HBV with +- 50% precipitation: /hdata/drift/flom/usikkerhet_grd/ut_test/vfpost_usikkerhet.txt
 * Pure model run (not processed): this is in the dataset but it is the same as in 1., so shouldn't be plotted!
 * Updated: automated update using observed streamflow (different method)
 * Results from a run with precip + 50%
Ask Gusong if questions

3. DDD model 24 hours: /hdata/drift/flom/DDD24h2015R/24hres.txt
 * Only pure model.
Ask Selalem if questions

4. ODM model 24 hours: /hdata/drift/flood/H-VEPS02/simu_hbv_elev_24h/<catchment>/InputTimeSeries.txt
 * Pure model
 * Updated model
Ask Bård if questions 



Potential issue with data:
The starting date of those files is fixed but sometimes changed!
So the length of data is variable (kin of random).
So, if we want to implement a feature that pulls the x interesting events in the last year, that cuold be a problem

This is not a problem for implement the DATE -7 / +9 days model plots.



