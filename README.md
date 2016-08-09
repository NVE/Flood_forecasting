# Flood_forecasting
A Shiny app to present flood forecasting results at NVE.
If you find a bug or think of an interesting new feature, please file a new [issue](https://github.com/fbaffie/Flood_forecasting/issues). Please refer to the guidelines for filing issues at the end of this readme.

## Aim
Provide the flood forecasters with an easy to use and customizable graphical user interface (GUI) for flood forecasting and model evaluation.

## Running the app


This branch is intended to be run as a demonstration branch. It is not yet available on shinyapps.io
you can also run it locally as follows:


#### Directly from Github

```R
# If you don't have Shiny installed start with:
install.packages('shiny')

# Then load the package and run the app:
library(shiny)


runGitHub("Flood_forecasting", "fbaffie", ref = "demo")

```

#### Locally

To have a copy on your computer, first clone the git repository and then use `runApp()`:

```R
# First clone the repository with git. If you have cloned it into
# ~/Flood_forecasting, first go to that directory, then use runApp().
setwd("~/Flood_forecasting")
runApp()
```

## Filing issues

Please try to follow those guidelines for filing issues:

One issue for one purpose. Don't add more than one bug, feature request, documentation request, question, etc.. on to the same issue.

- If you've found a bug, thanks for reporting!
- If you've a request of some kind, e.g., feature request or documentation request, it'd be much appreciated if you could add **[Request]** at the beginning of the title. This helps us to prioritise easily without having to go through the entire issue.
- If you need support, e.g., installation issues or upgrade issues, please add **[Support]** at the beginning of the title. This helps us to easily identify the most common support issues, and provide solutions in a separate page.
- If you have a general question, add **[Question]** at the beginning of the title.
- If you've an issue that doesn't fall into any of these categories, then it'd be helpful if you could add **[Misc]** to your title.
