@echo off
cd C:\Users\flbk\Mine dokumenter\Github\Flood_forecasting


REM "C:\Users\flbk\Documents\R\R-3.3.0\bin\i386\R.exe" CMD BATCH C:\Users\flbk\Documents\GitHub\Flood_forecasting\update_app.R

git checkout demo
git rm --cached *.RData 
timeout 5

git add --force HBV_2014.RData
timeout 5

git status
timeout 5

git add -A
timeout 5

git status
timeout 5