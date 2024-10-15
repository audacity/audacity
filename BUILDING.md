# Building Audacity

## NOTE: These instructions are work-in-progress and will be finalized after restructuring. 

## Requirements

* CMake
* A CMake generator (eg Ninja)
* A C++ compiler (MSVC on Windows)
* Qt 6.2.4, with modules:
  * Qt Network Auth, 
  * Qt 5 Compatibil

Maybe more. Qt Creator tends to be useful. 

Also, large parts of Audacity 4 are based on MuseScore Studio. So many parts from https://github.com/musescore/MuseScore/wiki/Set-up-developer-environment may apply here too. 