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

