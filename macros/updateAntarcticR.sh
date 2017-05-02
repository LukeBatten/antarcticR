#!/bin/bash

# update all packages
rm ../../antarcticR.pdf
Rscript updateCustomPackages.R
cp ../../antarcticR.pdf ..

git add ../*
git 
git commit
git push origin master
