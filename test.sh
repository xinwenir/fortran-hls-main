#!/bin/bash

if [ $# -gt 0 ]
then
    echo "$0 is the not the first time running: $1"
    if [ $1 = "rb" ]
    then 
        echo "yeah!!!"
    fi
else 
    echo "$0 is the first time running!"
fi