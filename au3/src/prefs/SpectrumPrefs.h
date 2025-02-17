/**********************************************************************

  Audacity: A Digital Audio Editor

  SpectrumPrefs.h

  Dominic Mazzoni
  James Crook

**********************************************************************/
/*
  Salvo Ventura
  November 2006

  Added selection box for windowType

  All params are saved in config file.
*/

#ifndef __AUDACITY_SPECTRUM_PREFS__
#define __AUDACITY_SPECTRUM_PREFS__

#include <vector>
#include <wx/defs.h>

#include "PrefsPanel.h"
#include "SpectrogramSettings.h"

class wxChoice;
class wxCheckBox;
class wxTextCtrl;
struct FFTParam;
class ShuttleGui;
class SpectrogramSettings;
class WaveChannel;
struct WaveChannelSubViewPlacement;

#define SPECTRUM_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol { XO("Spectrum") }

class SpectrumPrefs final : public PrefsPanel
{
public:
    SpectrumPrefs(wxWindow* parent, wxWindowID winid, AudacityProject* pProject, WaveChannel* wc);
    virtual ~SpectrumPrefs();
    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;

    void Preview() override;
    bool Commit() override;
    void PopulateOrExchange(ShuttleGui& S) override;
    void Rollback();
    bool ShowsPreviewButton() override;
    bool Validate() override;
    ManualPageID HelpPageName() override;

private:
    void Populate(size_t windowSize);
    void PopulatePaddingChoices(size_t windowSize);

    void OnControl(wxCommandEvent& event);
    void OnWindowSize(wxCommandEvent& event);
    void OnDefaults(wxCommandEvent&);
    void OnAlgorithm(wxCommandEvent&);
    DECLARE_EVENT_TABLE()

    void EnableDisableSTFTOnlyControls();

    AudacityProject* mProject{};

    WaveChannel* const mWc;
    bool mDefaulted, mOrigDefaulted;

    wxTextCtrl* mMinFreq;
    wxTextCtrl* mMaxFreq;
    wxTextCtrl* mGain;
    wxTextCtrl* mRange;
    wxTextCtrl* mFrequencyGain;

    /*
     Zero-padding factor for spectrograms can smooth the display of
     spectrograms by interpolating in frequency domain. */
    int mZeroPaddingChoice;
    wxChoice* mZeroPaddingChoiceCtrl;
    TranslatableStrings mZeroPaddingChoices;

    TranslatableStrings mTypeChoices;

    wxChoice* mAlgorithmChoice;

    wxCheckBox* mDefaultsCheckbox;

    SpectrogramSettings mTempSettings, mOrigSettings;

    std::vector<WaveChannelSubViewPlacement> mOrigPlacements;
    float mOrigMin, mOrigMax;

    bool mPopulating;
    bool mCommitted{};
};

/// A PrefsPanel::Factory that creates one SpectrumPrefs panel.
/// This factory can be parametrized by a single channel, to change settings
/// non-globally
extern PrefsPanel::Factory SpectrumPrefsFactory(WaveChannel* wc = 0);
#endif
