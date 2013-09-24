/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportPCM.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_IMPORT_PCM__
#define __AUDACITY_IMPORT_PCM__

class ImportPluginList;
class UnusableImportPluginList;

void GetPCMImportPlugin(ImportPluginList *importPluginList,
                        UnusableImportPluginList *unusableImportPluginList);


#endif
