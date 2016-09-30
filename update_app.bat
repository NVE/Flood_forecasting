@echo off
cd C:\Users\flbk\Mine dokumenter\Github\Flood_forecasting
git checkout demo
"C:\Program Files\R\R-3.3.1\bin\x64\R.exe" CMD BATCH D:\Github\Flood_forecasting\update_app.R
git commit -am "updating data files"
git push origin