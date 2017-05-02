#!/bin/bash

# remove existing pdf
rm ../../antarcticR.pdf

# update all packages
Rscript updateCustomPackages.R
cd ../..
R CMD Rd2pdf antarcticR
cp antarcticR.pdf ./antarcticR

cd ./antarcticR
echo "Git..."
git add *
git 
git commit
git push origin master
