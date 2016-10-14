@echo off
cd C:\Users\flbk\Mine dokumenter\Github\Flood_forecasting
git checkout demo
git add -f *.Rdata
git add -A
timeout 5
git status

git commit -m "updating data files"
git push origin
git rm --cached *.RData 
