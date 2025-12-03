/**********************************************************************

 Audacity: A Digital Audio Editor

 AllThemeResources.cpp

 Paul Licameli split from Theme.cpp

 **********************************************************************/

// This tiny file exists so that compilation dependency on AllThemeResources.h
// corresponds with the real linkage dependencies:  that is, generate
// the definitions corresponding to extern declarations of the header here
// only

// This declares the variables such as
// int BmpRecordButton = -1;
#define THEME_DECLARATIONS
#include "AllThemeResources.h"
