/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportFLAC.h

  Sami Liedes

**********************************************************************/

#ifndef __AUDACITY_IMPORT_FLAC__
#define __AUDACITY_IMPORT_FLAC__

class ImportPluginList;
class UnusableImportPluginList;

void GetFLACImportPlugin(ImportPluginList *importPluginList,
                         UnusableImportPluginList *unusableImportPluginList);


#endif
