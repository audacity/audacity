# Building Audacity

## NOTE: These instructions are work-in-progress and will be finalized after restructuring. 

Audacity is a 

## Requirements

* CMake
* A CMake generator (eg Ninja)
* A C++ compiler (MSVC on Windows)
* Qt 6.2.4, with modules:
  * Qt Network Auth, 
  * Qt 5 Compatibil

Maybe more. Qt Creator tends to be useful. 

To compile, target the CMakeLists.txt in au4/, NOT the one in /. 