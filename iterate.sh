#!/bin/sh

PATH=$(cd $(dirname $0) && pwd)

for filepath in ${PATH}/data/*.txt; do
   fname_ext="${filepath##*/}"
   fname="${fname_ext%.*}"

   ./conversion $filepath > "${PATH}/gmls/${fname}.gml"
done
