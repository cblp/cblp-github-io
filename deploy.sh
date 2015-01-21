#!/bin/bash
set -eu
set -o pipefail

set -x

site='cabal run'

test -d .git  # we are in git repo root
test "$(cat .git/HEAD)" == "ref: refs/heads/source"  # we are at master
# TODO: check git status?

github="$(git config remote.origin.url)"
head="$(git rev-parse HEAD)"

$site clean
git clone . _site                           \
    --config user.email=hakyll@deploy.sh    \
    --no-checkout
$site build
( cd _site
    git add .
    # at source
    git commit --quiet --reuse-message=$head
    git push --force $github source:master
    rm -rf .git
)
git fetch
