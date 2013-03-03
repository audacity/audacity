/**********************************************************************

Audacity: A Digital Audio Editor

ExportFFmpeg.h

LRN

**********************************************************************/

#ifndef __AUDACITY_EXPORTFFMPEG__
#define __AUDACITY_EXPORTFFMPEG__

class ExportPlugin;

/** The only part of this class which is publically accessible is the
 * factory method New_ExportFFmpeg() which creates a new ExportFFmpeg object and
 * returns a pointer to it. The rest of the class declaration is in ExportFFmpeg.cpp
 */
ExportPlugin *New_ExportFFmpeg();

#endif
