#!/bin/sh
dir=DIRECTORY
ecl=$dir/ecl
if [ ! -x $ecl ] ; then
  echo Cannot find $ecl
  exit 1
fi
exec $ecl $@
