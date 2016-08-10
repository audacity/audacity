//
//  ImportForwards.h
//  Audacity
//
//  Created by Paul Licameli on 8/10/16.
//
//

#ifndef __AUDACITY_IMPORT_FORWARDS__
#define __AUDACITY_IMPORT_FORWARDS__

#include <wx/list.h>
#include "../MemoryX.h"

class ImportPlugin;
class UnusableImportPlugin;

WX_DECLARE_LIST(ImportPlugin, ImportPluginList);
WX_DECLARE_LIST(UnusableImportPlugin, UnusableImportPluginList);

#endif
