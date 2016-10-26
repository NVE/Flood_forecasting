#!/bin/bash
cd /srv/shiny-server/Flomvarsling
git checkout demo
git add -f *.RData
git commit -m "updating data"
git push origin demo
git checkout master
