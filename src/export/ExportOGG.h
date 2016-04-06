/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportOGG.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_EXPORTOGG__
#define __AUDACITY_EXPORTOGG__

#include "../MemoryX.h"
class ExportPlugin;

/** The only part of this class which is publically accessible is the
 * factory method New_ExportOGG() which creates a NEW ExportOGG object and
 * returns a pointer to it. The rest of the class declaration is in ExportOGG.cpp
 */
movable_ptr<ExportPlugin> New_ExportOGG();

#endif

