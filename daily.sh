#!/bin/bash
Rscript daily.R
Rscript -e "require ('rmarkdown'); render('daily.Rmd')"

