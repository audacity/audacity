/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportFLAC.h

  Frederik M.J.V

**********************************************************************/

#ifndef __AUDACITY_EXPORTFLAC__
#define __AUDACITY_EXPORTFLAC__

class ExportPlugin;

/* The only part of this class which is publically accessible is the
 * factory method New_ExportFLAC() which creates a new ExportFLAC object and
 * returns a pointer to it. The rest of the class declaration is in ExportFLAC.cpp
 */
ExportPlugin *New_ExportFLAC();

#endif

