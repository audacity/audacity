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

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: db6c7791-1340-46db-86cb-b1b4ef5977e0

