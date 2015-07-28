#!/bin/sh

for (( i = 0; i < ${SCRIPT_INPUT_FILE_COUNT}; i++ ))
do
   in="$(eval echo \${SCRIPT_INPUT_FILE_${i}})"
   out="$(eval echo \${SCRIPT_OUTPUT_FILE_${i}})"
   cp -pPR "${in}" "${out}"
done

exit 0
