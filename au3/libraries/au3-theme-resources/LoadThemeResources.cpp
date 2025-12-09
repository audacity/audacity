/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadThemeResources.cpp

  Paul Licameli

**********************************************************************/

#include "LoadThemeResources.h"

void ThemeResources::Load()
{
    // Nothing!
    // This function merely needs to be called somewhere from the application to
    // guarantee that loading of the dynamic library is not skipped by the linker.
    // The real work is then done at static initialization time in other files
    // before this function is reached.
}
