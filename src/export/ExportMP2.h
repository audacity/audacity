/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMP2.h

  Dominic Mazzoni
  Markus Meyer

**********************************************************************/

#ifndef __AUDACITY_EXPORTMP2__
#define __AUDACITY_EXPORTMP2__

#include "../MemoryX.h"
class ExportPlugin;

/** The only part of this class which is publically accessible is the
 * factory method New_ExportMP2() which creates a NEW ExportMP2 object and
 * returns a pointer to it. The rest of the class declaration is in ExportMP2.cpp
 */
movable_ptr<ExportPlugin> New_ExportMP2();

#endif

