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
   local indent="${1}"
   local path=$(resolve "${2}")
   local base="${path##*/}"

   if [ -e "${LIBPATH}/${base}" ]
   then
      return
   fi
   
   printf "%${indent}.${indent}cCopying '${path}' into bundle\n" " "
   cp -p "${path}" "${LIBPATH}"

   for lib in $(otool -L "${path}" | awk '/libwx.*dylib /{print $1}')
   do
      path=$(resolve "${lib}")

      printf "%${indent}.${indent}cChanging '${lib}' to '@loader_path/../Frameworks/${path##*/}'\n" " "
      install_name_tool -change "${lib}" "@loader_path/../Frameworks/${path##*/}" "${LIBPATH}/${base}"

      update_paths $((indent + 2)) "${path}"
   done
}

# For testing
# BUILT_PRODUCTS_DIR=/tmp/Audacity.app/Contents
# EXECUTABLE_PATH=MacOS/audacity
# FRAMEWORKS_FOLDER_PATH=Frameworks

EXEPATH="${TARGET_BUILD_DIR}/${EXECUTABLE_PATH}"
LIBPATH="${TARGET_BUILD_DIR}/${FRAMEWORKS_FOLDER_PATH}"

mkdir -p "${LIBPATH}"

echo "Updating Audacity executable"

for lib in $(otool -L "${EXEPATH}" | awk '/libwx.*dylib /{print $1}')
do
   path=$(resolve "${lib}")

   printf "Changing '${lib}' to '@executable_path/../Frameworks/${path##*/}'\n"
   install_name_tool -change "${lib}" "@executable_path/../Frameworks/${path##*/}" "${EXEPATH}"

   update_paths 2 "${path}"
done

exit 0
