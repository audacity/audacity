#!/bin/sh

update_paths()
{
   base=$(basename "${1}")
   cp -p "${1}" "${LIBPATH}"

   for lib in $(otool -L "${1}" | awk '/libwx/{print $1}')
   do
      install_name_tool -change "${lib}" @loader_path/../Frameworks/$(basename "${lib}") "${LIBPATH}/${base}"
   done
}

EXEPATH="${BUILT_PRODUCTS_DIR}/${EXECUTABLE_PATH}"
LIBPATH="${BUILT_PRODUCTS_DIR}/${FRAMEWORKS_FOLDER_PATH}"

mkdir -p "${LIBPATH}"

for lib in $(otool -L "${EXEPATH}" | awk '/libwx/{print $1}')
do
   install_name_tool -change "${lib}" @executable_path/../Frameworks/$(basename "${lib}") "${EXEPATH}"
   update_paths "${lib}"
done

exit 0
