#!/bin/sh
set -x

resolve()
{
set -x
   dir="${1%/*}"
   base="${1##*/}"
   while [ -n "${base}" ]
   do
      lib="${dir}/${base}"
      base=$(readlink "${dir}/${base}")
   done
   echo $lib
}

update_paths()
{
set -x
   path=$(resolve "${1}")
   base="${path##*/}"
   cp -p "${path}" "${LIBPATH}"

   for lib in $(otool -L "${path}" | awk '/libwx/{print $1}')
   do
      path=$(resolve "${lib}")
      install_name_tool -change "${lib}" "@loader_path/../Frameworks/${path##*/}" "${LIBPATH}/${base}"
   done
}

EXEPATH="${BUILT_PRODUCTS_DIR}/${EXECUTABLE_PATH}"
LIBPATH="${BUILT_PRODUCTS_DIR}/${FRAMEWORKS_FOLDER_PATH}"

mkdir -p "${LIBPATH}"

for lib in $(otool -L "${EXEPATH}" | awk '/libwx/{print $1}')
do
   path=$(resolve "${lib}")
   install_name_tool -change "${lib}" "@executable_path/../Frameworks/${path##*/}" "${EXEPATH}"
   update_paths "${path}"
done

exit 0
