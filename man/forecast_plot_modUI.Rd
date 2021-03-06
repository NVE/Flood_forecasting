% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_modules.R
\name{forecast_plot_modUI}
\alias{forecast_plot_modUI}
\title{Shiny UI module for the multimodel and multistation plot. Used in UI.R and in "mapModule_polygonFeatureUI"}
\usage{
forecast_plot_modUI(id)
}
\arguments{
\item{id}{}
}
\description{
Shiny UI module for the multimodel and multistation plot. Used in UI.R and in "mapModule_polygonFeatureUI"
}
\examples{
In UI.R
tabPanel("Velg stasjoner med nedtrekksmeny", 
mapModuleUI("multistation_map", multiple = TRUE),
multimod_forecast_selection_modUI("multistation_plot"), forecast_plot_modUI("multistation_plot"))
}
