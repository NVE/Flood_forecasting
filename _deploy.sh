#!/bin/sh

set -e

[ -z "${GITHUB_PAT}" ] && exit 0
[ "${TRAVIS_BRANCH}" != "master" ] && exit 0

# configure your name and email if you have not done so
git config --global user.email "fbaffie@gmail.com"
git config --global user.name "fbaffie"

# clone the repository to the book-output directory
git clone -b gh-pages https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git travis_output
cd travis_output
cp -r ../_gh-pages/* ./
git add --all *
git commit -m "Travis update of gh-pages"
git push origin gh-pages
