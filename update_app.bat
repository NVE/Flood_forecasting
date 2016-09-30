@echo off
cd C:\Users\flbk\Mine dokumenter\Github\Flood_forecasting
git checkout demo
git rm --cached *.RData 
"C:\Users\flbk\Documents\R\R-3.3.0\bin\i386\R.exe" CMD BATCH C:\Users\flbk\Documents\GitHub\Flood_forecasting\update_app.R
