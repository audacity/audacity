#!/usr/bin/env bash

# A special wrapper for hdiutil, that retries hdiutil call with
# the progressive timeout. 
# This seems to be the common workaround for the CPack problem

counter=0
max_retries=10

hdiutil $@

while [ $? -ne 0 ]; do
    ((counter++))

    if [[ $counter -eq $max_retries ]]; then
        exit 1
    fi

    sleep $counter
    hdiutil $@
done
