#!/bin/sh
pwd
cd report
pwd
Rscript -e "rmarkdown::render("process.Rmd")"
Rscript -e "rmarkdown::render("app.Rmd")"
Rscript -e "rmarkdown::render("HBV.Rmd")"
Rscript -e "rmarkdown::render("DDD.Rmd")"
Rscript -e "rmarkdown::render("ODM.Rmd")"
Rscript -e "rmarkdown::render("DDM.Rmd")"

#Rscript -e "source("render_all_rmd.R")"

#Rscript -e "rmarkdown::render("report/process.Rmd")"
#Rscript -e "rmarkdown::render("report/app.Rmd")"
#Rscript -e "rmarkdown::render("report/HBV.Rmd")"
#Rscript -e "rmarkdown::render("report/DDD.Rmd")"
#Rscript -e "rmarkdown::render("report/ODM.Rmd")"
#Rscript -e "rmarkdown::render("report/DDM.Rmd")"
