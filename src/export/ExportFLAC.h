/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportFLAC.h

  Frederik M.J.V

**********************************************************************/

#ifndef __AUDACITY_EXPORTFLAC__
#define __AUDACITY_EXPORTFLAC__

#include "../MemoryX.h"
class ExportPlugin;

/* The only part of this class which is publically accessible is the
 * factory method New_ExportFLAC() which creates a NEW ExportFLAC object and
 * returns a pointer to it. The rest of the class declaration is in ExportFLAC.cpp
 */
movable_ptr<ExportPlugin> New_ExportFLAC();

#endif

