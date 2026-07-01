/**********************************************************************

Audacity: A Digital Audio Editor

  CVSD.cpp

https://www.bluetooth.com/wp-content/uploads/Files/Specification/HTML/Core-62/out/en/br-edr-controller/baseband-specification.html#UUID-00be0b31-be51-dfbe-ac3b-d46c5138704b

**********************************************************************/
// Todo: Check out why linking fails
// The error message
// ld: warning: building for macOS-11.0, but linking with dylib '/opt/homebrew/opt/qtbase/lib/QtCore.framework/Versions/A/QtCore' which was built for newer version 14.0
// Undefined symbols for architecture arm64:
//   "CVSDEncode(char const*, unsigned int)", referenced from:
//       ExportCVSDProcessor::Process(ExportProcessorDelegate&) in libau3wrap.a[16](RegisterExportPlugins.cpp.o)
// ld: symbol(s) not found for architecture arm64
// clang++: error: linker command failed with exit code 1 (use -v to see invocation)

#include "ModuleConstants.h"

DEFINE_MODULE_ENTRIES
