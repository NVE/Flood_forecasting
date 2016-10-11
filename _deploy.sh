#!/bin/sh

set -e

[ -z "${GITHUB_PAT}" ] && exit 0
[ "${TRAVIS_BRANCH}" != "master" ] && exit 0

# configure your name and email if you have not done so
git config --global user.email "fbaffie@gmail.com"
git config --global user.name "Florian Kobierska Baffie"

# clone the repository to the book-output directory
git clone -b gh-pages https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git book-output
cd book-output
cp -r ../_book/* ./
git add --all *
git commit -m "Update the flood forecasting report"
git push origin gh-pages
