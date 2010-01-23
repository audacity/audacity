/**********************************************************************

Audacity: A Digital Audio Editor

ImportFFmpeg.h

LRN

**********************************************************************/

#ifndef __AUDACITY_IMPORT_FFMPEG__
#define __AUDACITY_IMPORT_FFMPEG__

#include "ImportPlugin.h"

class ImportPluginList;
class UnusableImportPluginList;

void GetFFmpegImportPlugin(ImportPluginList *importPluginList,
                           UnusableImportPluginList *unusableImportPluginList);

#endif
