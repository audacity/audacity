/**********************************************************************

  Audacity: A Digital Audio Editor

  ApplicationPrefs.h

  Anton Gerasimov

**********************************************************************/

#ifndef __AUDACITY_APPLICATION_PREFS__
#define __AUDACITY_APPLICATION_PREFS__

#include <wx/defs.h>

#include "PrefsPanel.h"
#include "Prefs.h"

class ShuttleGui;

class ApplicationPrefs final : public PrefsPanel
{
public:
    ApplicationPrefs(wxWindow* parent, wxWindowID winid);
    ~ApplicationPrefs();
    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;

    bool Commit() override;
    ManualPageID HelpPageName() override;

private:
    void Populate();
    void PopulateOrExchange(ShuttleGui& S) override;
};

#endif
