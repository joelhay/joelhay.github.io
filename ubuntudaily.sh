#!/bin/bash
cd
cd joelhay.github.io
NOW=$( date '+%F_%H:%M:%S' )
Rscript daily.R
Rscript -e "require ('rmarkdown'); render('daily.Rmd')"
mv daily.html index.html
git add index.html
git commit -a -m $NOW
git push

