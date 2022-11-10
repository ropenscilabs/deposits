#!/usr/bin/bash

# Script to filter repo of all commit histories made by constantly over-writing
# httptest2 results. First list all testthat sub-directories traced by git, as
# returned from the '--analyze' function of 'git-filter-repo'
git-filter-repo --analyze
cp .git/filter-repo/analysis/directories-all-sizes.txt .
DIRS=($(cat directories-all-sizes.txt | \
    awk -F"tests/testthat/" '{ print $2 }' | \
    awk '!/[/]|^$/ { print $1 }' | \
    sort))
rm directories-all-sizes.txt

# Then destructively remove all commits from all listed sub-directories:
for d in "${DIRS[@]}"; do
    echo "$d";
    git-filter-repo --path tests/testthat/"$d" --invert-paths
done
