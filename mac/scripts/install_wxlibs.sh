#!/bin/sh

resolve()
{
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
   path=$(resolve "${1}")
   base="${path##*/}"
   cp -p "${path}" "${LIBPATH}"
   seen="${seen}:${path}"

   echo "Updating library = '$path'"

   for lib in $(otool -L "${path}" | awk '/libwx.*dylib /{print $1}')
   do
      path=$(resolve "${lib}")
      install_name_tool -change "${lib}" "@loader_path/../Frameworks/${path##*/}" "${LIBPATH}/${base}"

      if [[ ! ${seen} =~ .*:${path}.* ]]
      then
         update_paths "${path}"
      fi
   done
}

BUILT_PRODUCTS_DIR=/tmp/Audacity.dst/Audacity/Audacity.app/Contents
EXECUTABLE_PATH=MacOS/audacity
FRAMEWORKS_FOLDER_PATH=Frameworks
EXEPATH="${BUILT_PRODUCTS_DIR}/${EXECUTABLE_PATH}"
LIBPATH="${BUILT_PRODUCTS_DIR}/${FRAMEWORKS_FOLDER_PATH}"

mkdir -p "${LIBPATH}"

seen=""
for lib in $(otool -L "${EXEPATH}" | awk '/libwx/{print $1}')
do
   path=$(resolve "${lib}")
   install_name_tool -change "${lib}" "@executable_path/../Frameworks/${path##*/}" "${EXEPATH}"
   update_paths "${path}"
done

exit 0
