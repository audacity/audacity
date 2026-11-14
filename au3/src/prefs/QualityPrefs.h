/**********************************************************************

  Audacity: A Digital Audio Editor

  QualityPrefs.h

  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_QUALITY_PREFS__
#define __AUDACITY_QUALITY_PREFS__

#include <vector>
#include <wx/defs.h>

#include "PrefsPanel.h"

class ShuttleGui;
enum class sampleFormat : unsigned;
enum DitherType : unsigned;

#define QUALITY_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol { XO("Quality") }

class AUDACITY_DLL_API QualityPrefs final : public PrefsPanel
{
public:
    QualityPrefs(wxWindow* parent, wxWindowID winid);
    virtual ~QualityPrefs();
    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;

    bool Commit() override;
    ManualPageID HelpPageName() override;
    void PopulateOrExchange(ShuttleGui& S) override;

private:
    void Populate();

    DECLARE_EVENT_TABLE()
};

#endif
