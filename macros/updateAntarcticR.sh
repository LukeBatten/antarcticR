#!/bin/bash

# update all packages
Rscript updateCustomPackages.R
R CMD Rd2pdf antarcticR
cp ../../antarcticR.pdf 

git add ../*
git 
git commit
git push origin master
