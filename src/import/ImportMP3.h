/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportMP3.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_IMPORT_MP3__
#define __AUDACITY_IMPORT_MP3__

class ImportPluginList;
class UnusableImportPluginList;

void GetMP3ImportPlugin(ImportPluginList *importPluginList,
                        UnusableImportPluginList *unusableImportPluginList);

#endif
