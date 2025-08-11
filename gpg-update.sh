#!/usr/bin/env bash

set -Eeuo pipefail


ID=33C3A599DB85EA9B8BAA1866B202264020068BFB
gpg --armor --export "$ID" > chreekat_gpg_key.txt
gpg --send-keys "$ID"
gpg --keyserver keyserver.ubuntu.com --send-keys "$ID"
keybase gpg update

git status chreekat_gpg_key.txt

echo
echo "Update GitHub and GitLab, too!"
