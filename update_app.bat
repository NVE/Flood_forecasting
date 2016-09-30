@echo off
git checkout demo
REM git add --force HBV_2014.RData
git rm --cached *.RData 
timeout 5
REM "C:\Users\flbk\Documents\R\R-3.3.0\bin\i386\R.exe" CMD BATCH C:\Users\flbk\Documents\GitHub\Flood_forecasting\update_app.R
cd C:\Users\flbk\Mine dokumenter\Github\Flood_forecasting




git add -A
timeout 5
git status
