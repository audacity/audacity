/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportCL.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_EXPORTCL__
#define __AUDACITY_EXPORTCL__

#include "../MemoryX.h"
// forward declaration of the ExportPlugin class from Export.h
class ExportPlugin;

/** The only part of this class which is publically accessible is the
 * factory method New_ExportCL() which creates a NEW ExportCL object and
 * returns a pointer to it. The rest of the class declaration is in ExportCL.cpp
 */
movable_ptr<ExportPlugin> New_ExportCL();

#endif
