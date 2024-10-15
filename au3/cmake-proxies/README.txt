CMake requires that its CMakeList.txt files follow the directory structure
of the projects.  However, for Audacity this would cause collision with
existing CMakeList.txt files that e.g. expat and libsoxr already provide.

Our solution is to have this proxy directory for lib-src to hold our version of
the CMakeList.txt files.  We did try bundling several libraries under one
CMakeList.txt without using subdirectories.  However, we were then fighting 
CMake too much - and did not have a clean separation of information between sub 
projects.
