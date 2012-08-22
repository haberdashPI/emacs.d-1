#!/bin/sh

git submodule foreach 'git pull origin master'
git commit -am "Update plugins"

echo "Vim plugins updated. Make it sure that you push the changes to the main repository"
