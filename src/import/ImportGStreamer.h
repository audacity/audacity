/**********************************************************************

Audacity: A Digital Audio Editor

ImportGStreamer.h

LRN

**********************************************************************/

#ifndef __AUDACITY_IMPORT_GSTREAMER__
#define __AUDACITY_IMPORT_GSTREAMER__

#include "ImportPlugin.h"

class ImportPluginList;
class UnusableImportPluginList;

void GetGStreamerImportPlugin(ImportPluginList *importPluginList,
                           UnusableImportPluginList *unusableImportPluginList);

#endif
