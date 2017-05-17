#!/bin/sh

# bookdown::render_book("index.html", output_format = "bookdown::pdf_book")
# system("./_build.sh")

Rscript -e "bookdown::render_book('index.Rmd')"
rm -rf docs/downloads
cp -rf downloads docs
