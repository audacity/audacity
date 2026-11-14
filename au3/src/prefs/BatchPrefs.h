/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchPrefs.h

  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_BATCH_PREFS__
#define __AUDACITY_BATCH_PREFS__

#include <wx/defs.h>

#include "PrefsPanel.h"

class ShuttleGui;

#define BATCH_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol { XO("Batch") }

class BatchPrefs final : public PrefsPanel
{
public:
    BatchPrefs(wxWindow* parent, wxWindowID winid);
    ~BatchPrefs();
    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID HelpPageName() override;

    bool Commit() override;
    void PopulateOrExchange(ShuttleGui& S) override;

private:
    void Populate();

    DECLARE_EVENT_TABLE()
};

#endif
