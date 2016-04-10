/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportPCM.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORTPCM__
#define __AUDACITY_EXPORTPCM__

#include "../MemoryX.h"
class ExportPlugin;

/** The only part of this class which is publically accessible is the
 * factory method New_ExportPCM() which creates a NEW ExportPCM object and
 * returns a pointer to it. The rest of the class declaration is in ExportPCM.cpp
 */
movable_ptr<ExportPlugin> New_ExportPCM();

#endif

