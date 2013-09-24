/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportQT.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_IMPORT_QT__
#define __AUDACITY_IMPORT_QT__

class ImportPluginList;
class UnusableImportPluginList;

void GetQTImportPlugin(ImportPluginList *importPluginList,
                       UnusableImportPluginList *unusableImportPluginList);

#endif
