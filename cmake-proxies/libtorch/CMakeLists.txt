add_library( ${TARGET} INTERFACE )
add_library( ${symbol} ALIAS ${TARGET} )

# Pull in standard variables
def_vars()

# download a file
function(download url filename)
   # only download if file doesn't exist
   if(NOT EXISTS ${filename})
      message("downloading ${url}...")
      file(DOWNLOAD ${url} ${filename}
         SHOW_PROGRESS
         TIMEOUT 720  # seconds
      )
   endif()
endfunction(download)

# TODO: need to raise error if not building for 64 bit

if(CMAKE_SYSTEM_NAME MATCHES "Darwin")
   set (LIBTORCH_URL "https://download.pytorch.org/libtorch/cpu/libtorch-macos-1.8.1.zip")
elseif(CMAKE_SYSTEM_NAME MATCHES "Windows")
   set (LIBTORCH_URL "https://download.pytorch.org/libtorch/cpu/libtorch-win-shared-with-deps-1.8.1%2Bcpu.zip")
elseif(CMAKE_SYSTEM_NAME MATCHES "Linux")
   set (LIBTORCH_URL "https://download.pytorch.org/libtorch/cpu/libtorch-cxx11-abi-shared-with-deps-1.8.1%2Bcpu.zip")
endif()

set (LIBTORCH_ZIP_PATH "${topdir}/cmake-proxies/libtorch/libtorch.zip")
set (LIBTORCH_DIR "${topdir}/cmake-proxies/libtorch/")

# download libtorch
download("${LIBTORCH_URL}" "${LIBTORCH_ZIP_PATH}")

# extract libtorch
file(ARCHIVE_EXTRACT 
   INPUT "${LIBTORCH_ZIP_PATH}"
   DESTINATION "${LIBTORCH_DIR}"
)

# define our libtorch dir
set(LIBTORCH_DIR "${LIBTORCH_DIR}/libtorch")

# make sure we can find torch
set(Torch_DIR "${LIBTORCH_DIR}/share/cmake/Torch/")
list(APPEND CMAKE_PREFIX_PATH "${LIBTORCH_DIR}")

# find
find_package(Torch REQUIRED)

message("${TORCH_LIBRARIES}")
message("${TORCH_INCLUDE_DIRS}")
message("${TARGET}")

set_target_properties(torch PROPERTIES IMPORTED_GLOBAL On)
set_target_properties(torch_cpu PROPERTIES IMPORTED_GLOBAL On)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${TORCH_CXX_FLAGS}")
target_include_directories(${TARGET} INTERFACE "${TORCH_INCLUDE_DIRS}")
target_link_libraries( ${TARGET} INTERFACE "${TORCH_LIBRARIES}")