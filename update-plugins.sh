#!/bin/sh

WORKING_DIR=$(pwd)

cd pathogen && git pull origin master

cd $WORKING_DIR

cd vim/bundle && for d in $(ls); do cd $d; git pull origin master; cd ..; done

cd $WORKING_DIR

git add pathogen vim/bundle
git commit -am "Plugins updated"

echo "Vim plugins updated. Make it sure that you push the changes to the main repository"
