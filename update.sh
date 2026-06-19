#!/bin/bash
set -e

Rscript auto_update.R

[ -f Rplot.pdf ] && rm Rplot.pdf

git add -A
git commit -m "Daily Update $(date +%Y-%m-%d)"
git push
