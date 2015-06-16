#!/bin/bash
file=$1
pid=$2
rm -f $file
while true; do
  ps h -p $pid -o rss >> $file
done
