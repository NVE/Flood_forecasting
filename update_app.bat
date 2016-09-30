@echo off

cd C:\Users\flbk\Mine dokumenter\Github\Flood_forecasting
git checkout demo

"C:\Users\flbk\Documents\R\R-3.3.0\bin\i386\R.exe" CMD BATCH C:\Users\flbk\Documents\GitHub\Flood_forecasting\update_app.R
timeout 180

REM git rm --cached *.RData 
REM timeout 2

REM git add --force HBV_2014.RData
REM git add --force HBV_2016.RData
REM git add --force HBV_past_year.RData
REM git add --force flomtabell.RData
REM git add --force DDD.RData
REM timeout 2

git status
timeout 2

git add -A
git status
timeout 2

git commit -m "updating data files"
timeout 2

git push origin
timeout 2

git revert HEAD
timeout 2

git status
timeout 2
