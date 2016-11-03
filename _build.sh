#!/bin/sh

Rscript -e rmarkdown::render("report/process.Rmd")
Rscript -e rmarkdown::render("report/app.Rmd")
Rscript -e rmarkdown::render("report/HBV.Rmd")
Rscript -e rmarkdown::render("report/DDD.Rmd")
Rscript -e rmarkdown::render("report/ODM.Rmd")
Rscript -e rmarkdown::render("report/DDM.Rmd")
