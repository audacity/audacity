/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportPCM.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORTPCM__
#define __AUDACITY_EXPORTPCM__

class ExportPlugin;

/** The only part of this class which is publically accessible is the
 * factory method New_ExportPCM() which creates a new ExportPCM object and
 * returns a pointer to it. The rest of the class declaration is in ExportPCM.cpp
 */
ExportPlugin *New_ExportPCM();

#endif

