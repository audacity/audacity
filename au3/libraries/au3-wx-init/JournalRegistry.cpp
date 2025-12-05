/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file JournalRegistry.cpp

  Paul Licameli

**********************************************************************/

#include "JournalRegistry.h"

#include <wx/log.h>
#include <wx/string.h>

namespace Journal {
namespace {
bool sError = false;

static Dictionary& sDictionary()
{
    static Dictionary theDictionary;
    return theDictionary;
}
}

bool GetError()
{
    return sError;
}

void SetError()
{
    sError = true;
}

RegisteredCommand::RegisteredCommand(
    const wxString& name, Dispatcher dispatcher)
{
    if (!sDictionary().insert({ name, dispatcher }).second) {
        wxLogDebug(wxString::Format(
                       wxT("Duplicated registration of Journal command name %s"),
                       name
                       ));
        // Cause failure of startup of journalling and graceful exit
        SetError();
    }
}

const Dictionary& GetDictionary()
{
    return sDictionary();
}

static std::vector<Initializer>& sInitializers()
{
    static std::vector<Initializer> sTheFunctions;
    return sTheFunctions;
}

RegisteredInitializer::RegisteredInitializer(Initializer initializer)
{
    sInitializers().push_back(move(initializer));
}

const Initializers& GetInitializers()
{
    return sInitializers();
}
}
