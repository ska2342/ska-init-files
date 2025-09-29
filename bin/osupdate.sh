#!/bin/bash
# Author: ska
#
# Description:
# Get rid of the holy triangle

sudo apt update
apt list --upgradable

echo
echo "ENTER to proceed"
echo
read

sudo apt upgrade

