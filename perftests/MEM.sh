#!/bin/sh

while NEW=$(grep -i vmpeak /proc/$1/status 2>/dev/null); do PREV="$NEW"; sleep 1; done; echo $PREV
