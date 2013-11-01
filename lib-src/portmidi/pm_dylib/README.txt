pm_dylib

The purpose of this directory is to provide a separate CMakeLists.txt 
file for building a dynamic link library version of portmidi. This
version (in Windows) is linked using the Multithreaded C Runtime DLL
whereas the static library version in ../pm_common uses the (static)
Multithreaded C Runtime. There's no good reason not to build both
versions of portmidi in ../pm_common, but (the current) Cmake
has the restriction that you must either share compiler flags across
configurations (debug and release) or across targets (static and 
dynamic). Here, we need individual settings for all combinations.
