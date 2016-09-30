@echo off
"C:\Users\flbk\Documents\R\R-3.3.0\bin\i386\R.exe" CMD BATCH C:\Users\flbk\Documents\GitHub\Flood_forecasting\update_app.R
timeout 100

cd C:\Users\flbk\Mine dokumenter\Github\Flood_forecasting
git checkout demo
git rm --cached *.RData 
timeout 2

git add --force HBV_2014.RData
git add --force HBV_2016.RData
git add --force HBV_past_year.RData
git add --force flomtabell.RData
git add --force DDD.RData
timeout 2

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
