/**********************************************************************

  Audacity: A Digital Audio Editor

  LadspaEditor.cpp

  Dominic Mazzoni

  Paul Licameli split from LadspaEffect.cpp

*//*******************************************************************/
#include "LadspaEditor.h"

#include <float.h>
#include <wx/checkbox.h>
#include <wx/dcclient.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/scrolwin.h>
#include "ShuttleGui.h"
#include "../../widgets/NumericTextCtrl.h"
#include "../../widgets/valnum.h"

#if wxUSE_ACCESSIBILITY
#include "WindowAccessible.h"
#endif

// ============================================================================
// Tolerance to be used when comparing control values.
constexpr float ControlValueTolerance = 1.0e-5f;
// ============================================================================

enum
{
    ID_Duration = 20000,
    ID_Toggles = 21000,
    ID_Sliders = 22000,
    ID_Texts = 23000,
};

///////////////////////////////////////////////////////////////////////////////
//
// LadspaEffectMeter
//
///////////////////////////////////////////////////////////////////////////////

class LadspaEffectMeter final : public wxWindow
{
public:
    LadspaEffectMeter(wxWindow* parent, const float& val, float min, float max);

    void Disconnect()
    {
        // Stop using mVal, it might be dangling now
        mConnected = false;
    }

    virtual ~LadspaEffectMeter();

private:
    void OnErase(wxEraseEvent& evt);
    void OnPaint(wxPaintEvent& evt);
    void OnIdle(wxIdleEvent& evt);
    void OnSize(wxSizeEvent& evt);

private:
    bool mConnected{ true };
    const float& mVal;
    float mMin;
    float mMax;
    float mLastValue;

    DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(LadspaEffectMeter, wxWindow)
EVT_IDLE(LadspaEffectMeter::OnIdle)
EVT_ERASE_BACKGROUND(LadspaEffectMeter::OnErase)
EVT_PAINT(LadspaEffectMeter::OnPaint)
EVT_SIZE(LadspaEffectMeter::OnSize)
END_EVENT_TABLE()

LadspaEffectMeter::LadspaEffectMeter(wxWindow* parent, const float& val, float min, float max)
    :  wxWindow{parent, wxID_ANY, wxDefaultPosition, wxDefaultSize,
                wxSIMPLE_BORDER},
    mVal(val)
{
    mMin = min;
    mMax = max;
    mLastValue = -mVal;
    SetBackgroundColour(*wxWHITE);
    SetMinSize({ 20, 20 });
}

LadspaEffectMeter::~LadspaEffectMeter()
{
}

void LadspaEffectMeter::OnIdle(wxIdleEvent& evt)
{
    evt.Skip();
    if (!mConnected) {
        return;
    }
    if (mLastValue != mVal) {
        Refresh(false);
    }
}

void LadspaEffectMeter::OnErase(wxEraseEvent& WXUNUSED(evt))
{
    // Just ignore it to prevent flashing
}

void LadspaEffectMeter::OnPaint(wxPaintEvent& WXUNUSED(evt))
{
    if (!mConnected) {
        return;
    }

    wxPaintDC dc(this);

    // Cache some metrics
    wxRect r = GetClientRect();
    wxCoord x = r.GetLeft();
    wxCoord y = r.GetTop();
    wxCoord w = r.GetWidth();
    wxCoord h = r.GetHeight();

    // These use unscaled value, min, and max
    float val = mVal;
    if (val > mMax) {
        val = mMax;
    }
    if (val < mMin) {
        val = mMin;
    }
    val -= mMin;

    // Setup for erasing the background
    dc.SetPen(*wxTRANSPARENT_PEN);
    dc.SetBrush(wxColour(100, 100, 220));
    dc.Clear();
    dc.DrawRectangle(x, y, (w * (val / fabs(mMax - mMin))), h);

    mLastValue = mVal;
}

void LadspaEffectMeter::OnSize(wxSizeEvent& WXUNUSED(evt))
{
    Refresh(false);
}

bool LadspaEditor::UpdateUI()
{
    RefreshControls();
    return true;
}

void LadspaEditor::PopulateUI(ShuttleGui& S)
{
    auto& controls = mSettings.controls;
    auto parent = S.GetParent();

    mParent = parent;

    const auto& data = *mInstance.mData;
    mToggles.reinit(data.PortCount);
    mSliders.reinit(data.PortCount);
    mFields.reinit(data.PortCount, true);
    mLabels.reinit(data.PortCount);
    mMeters.resize(data.PortCount);

    wxASSERT(mParent); // To justify safenew
    wxScrolledWindow* const w = safenew wxScrolledWindow(mParent,
                                                         wxID_ANY,
                                                         wxDefaultPosition,
                                                         wxDefaultSize,
                                                         wxVSCROLL | wxTAB_TRAVERSAL);

    {
        auto mainSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
        w->SetScrollRate(0, 20);

        // This fools NVDA into not saying "Panel" when the dialog gets focus
        w->SetName(wxT("\a"));
        w->SetLabel(wxT("\a"));

        mainSizer->Add(w, 1, wxEXPAND);
        mParent->SetSizer(mainSizer.release());
    }

    wxSizer* marginSizer;
    {
        auto uMarginSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
        marginSizer = uMarginSizer.get();

        // Make user-adjustible input controls
        if (mNumInputControls) {
            auto paramSizer = std::make_unique<wxStaticBoxSizer>(wxVERTICAL, w, _("Effect Settings"));

            auto gridSizer = std::make_unique<wxFlexGridSizer>(5, 0, 0);
            gridSizer->AddGrowableCol(3);

            wxControl* item;

            // Add the duration control for generators
            if (mType == EffectTypeGenerate) {
                item = safenew wxStaticText(w, 0, _("Duration:"));
                gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);
                auto& extra = mAccess.Get().extra;
                mDuration = safenew
                                NumericTextCtrl(FormatterContext::SampleRateContext(mSampleRate),
                                                w, ID_Duration,
                                                NumericConverterType_TIME(),
                                                extra.GetDurationFormat(),
                                                extra.GetDuration(),
                                                NumericTextCtrl::Options{}
                                                .AutoPos(true));
                mDuration->SetName(XO("Duration"));
                gridSizer->Add(mDuration, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
                gridSizer->Add(1, 1, 0);
                gridSizer->Add(1, 1, 0);
                gridSizer->Add(1, 1, 0);
            }

            for (unsigned long p = 0; p < data.PortCount; ++p) {
                LADSPA_PortDescriptor d = data.PortDescriptors[p];
                if (LADSPA_IS_PORT_AUDIO(d) || LADSPA_IS_PORT_OUTPUT(d)) {
                    continue;
                }

                wxString labelText = LAT1CTOWX(data.PortNames[p]);
                item = safenew wxStaticText(w, 0, wxString::Format(_("%s:"), labelText));
                gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

                wxString fieldText;
                LADSPA_PortRangeHint hint = data.PortRangeHints[p];

                if (LADSPA_IS_HINT_TOGGLED(hint.HintDescriptor)) {
                    mToggles[p] = safenew wxCheckBox(w, ID_Toggles + p, wxT(""));
                    mToggles[p]->SetName(labelText);
                    mToggles[p]->SetValue(controls[p] > 0);
                    BindTo(*mToggles[p],
                           wxEVT_COMMAND_CHECKBOX_CLICKED, &LadspaEditor::OnCheckBox);
                    gridSizer->Add(mToggles[p], 0, wxALL, 5);

                    gridSizer->Add(1, 1, 0);
                    gridSizer->Add(1, 1, 0);
                    gridSizer->Add(1, 1, 0);
                    continue;
                }

                wxString bound;
                float lower = -FLT_MAX;
                float upper = FLT_MAX;
                bool haslo = false;
                bool hashi = false;
                bool forceint = false;

                if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor)) {
                    lower = hint.LowerBound;
                    haslo = true;
                }

                if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor)) {
                    upper = hint.UpperBound;
                    hashi = true;
                }

                if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor)) {
                    lower *= mSampleRate;
                    upper *= mSampleRate;
                    forceint = true;
                }

                // Limit to the UI precision
                lower = ceilf(lower * 1000000.0) / 1000000.0;
                upper = floorf(upper * 1000000.0) / 1000000.0;
                controls[p] = roundf(controls[p] * 1000000.0) / 1000000.0;

                if (haslo && controls[p] < lower) {
                    controls[p] = lower;
                }

                if (hashi && controls[p] > upper) {
                    controls[p] = upper;
                }

                // Don't specify a value at creation time.  This prevents unwanted events
                // being sent to the OnTextCtrl() handler before the associated slider
                // has been created.
                mFields[p] = safenew wxTextCtrl(w, ID_Texts + p);
                mFields[p]->SetName(labelText);
                BindTo(*mFields[p],
                       wxEVT_COMMAND_TEXT_UPDATED, &LadspaEditor::OnTextCtrl);
                gridSizer->Add(mFields[p], 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);

                wxString str;
                if (haslo) {
                    if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor) || forceint) {
                        str.Printf(wxT("%d"), (int)(lower + 0.5));
                    } else {
                        str = Internat::ToDisplayString(lower);
                    }
                    item = safenew wxStaticText(w, 0, str);
                    gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);
                } else {
                    gridSizer->Add(1, 1, 0);
                }

                mSliders[p] = safenew wxSliderWrapper(w, ID_Sliders + p,
                                                      0, 0, 1000,
                                                      wxDefaultPosition,
                                                      wxSize(200, -1));
#if wxUSE_ACCESSIBILITY
                // so that name can be set on a standard control
                mSliders[p]->SetAccessible(safenew WindowAccessible(mSliders[p]));
#endif
                mSliders[p]->SetName(labelText);
                BindTo(*mSliders[p],
                       wxEVT_COMMAND_SLIDER_UPDATED, &LadspaEditor::OnSlider);
                gridSizer->Add(mSliders[p], 0, wxALIGN_CENTER_VERTICAL | wxEXPAND | wxALL, 5);

                if (hashi) {
                    if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor) || forceint) {
                        str.Printf(wxT("%d"), (int)(upper + 0.5));
                    } else {
                        str = Internat::ToDisplayString(upper);
                    }
                    item = safenew wxStaticText(w, 0, str);
                    gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 5);
                } else {
                    gridSizer->Add(1, 1, 0);
                }

                if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor) || forceint) {
                    fieldText.Printf(wxT("%d"), (int)(controls[p] + 0.5));

                    IntegerValidator<float> vld(&controls[p]);
                    vld.SetRange(haslo ? lower : INT_MIN,
                                 hashi ? upper : INT_MAX);
                    mFields[p]->SetValidator(vld);
                } else {
                    fieldText = Internat::ToDisplayString(controls[p]);

                    // > 12 decimal places can cause rounding errors in display.
                    FloatingPointValidator<float> vld(6, &controls[p]);
                    vld.SetRange(lower, upper);

                    // Set number of decimal places
                    if (upper - lower < 10.0) {
                        vld.SetStyle(NumValidatorStyle::THREE_TRAILING_ZEROES);
                    } else if (upper - lower < 100.0) {
                        vld.SetStyle(NumValidatorStyle::TWO_TRAILING_ZEROES);
                    } else {
                        vld.SetStyle(NumValidatorStyle::ONE_TRAILING_ZERO);
                    }

                    mFields[p]->SetValidator(vld);
                }

                // Set the textctrl value.  This will trigger an event so OnTextCtrl()
                // can update the slider.
                mFields[p]->SetValue(fieldText);
            }

            paramSizer->Add(gridSizer.release(), 0, wxEXPAND | wxALL, 5);
            marginSizer->Add(paramSizer.release(), 0, wxEXPAND | wxALL, 5);
        }

        // Make output meters
        if (mNumOutputControls > 0) {
            auto paramSizer = std::make_unique<wxStaticBoxSizer>(wxVERTICAL, w, _("Effect Output"));

            auto gridSizer = std::make_unique<wxFlexGridSizer>(2, 0, 0);
            gridSizer->AddGrowableCol(1);

            wxControl* item;

            for (unsigned long p = 0; p < data.PortCount; ++p) {
                LADSPA_PortDescriptor d = data.PortDescriptors[p];
                if (LADSPA_IS_PORT_AUDIO(d) || LADSPA_IS_PORT_INPUT(d)) {
                    continue;
                }

                wxString labelText = LAT1CTOWX(data.PortNames[p]);
                item = safenew wxStaticText(
                    w, 0, wxString::Format(_("%s:"), labelText));
                gridSizer->Add(
                    item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

                //LADSPA_PortRangeHint hint = data.PortRangeHints[p];

                wxString bound;
                float lower = 0.0;
                float upper = 1.0;

                // Limit to the UI precision
                lower = ceilf(lower * 1000000.0) / 1000000.0;
                upper = floorf(upper * 1000000.0) / 1000000.0;
                controls[p] = lower;

                // Capture const reference to output control value for later
                // display update
                static float sink;
                auto pOutput = mpOutputs ? &mpOutputs->controls[p] : &sink;
                mMeters[p] = safenew LadspaEffectMeter(
                    w, *pOutput, lower, upper);
                mMeters[p]->SetLabel(labelText); // for screen readers
                gridSizer->Add(mMeters[p], 1, wxEXPAND | wxALIGN_CENTER_VERTICAL | wxALL, 5);
            }

            paramSizer->Add(gridSizer.release(), 0, wxEXPAND | wxALL, 5);
            marginSizer->Add(paramSizer.release(), 0, wxEXPAND | wxALL, 5);
        }

        w->SetSizer(uMarginSizer.release());
    }

    w->Layout();

    // Try to give the window a sensible default/minimum size
    wxSize sz1 = marginSizer->GetMinSize();
    wxSize sz2 = mParent->GetMinSize();
    w->SetMinSize({ std::min(sz1.x, sz2.x), std::min(sz1.y, sz2.y) });

    // And let the parent reduce to the NEW minimum if possible
    mParent->SetMinSize({ -1, -1 });
}

LadspaEditor::LadspaEditor(const EffectUIServices& effect,
                           const LadspaInstance& instance,
                           unsigned numInputControls, unsigned numOutputControls,
                           EffectSettingsAccess& access, double sampleRate, EffectType type,
                           const LadspaEffectOutputs* pOutputs)
    : EffectEditor{effect, access}
    , mInstance{instance}
    , mNumInputControls{numInputControls}
    , mNumOutputControls{numOutputControls}
    , mSampleRate{sampleRate}
    , mType{type}
    // Copy settings
    , mSettings{GetSettings(access.Get())}
    , mpOutputs{pOutputs}
{}

bool LadspaEditor::ValidateUI()
{
    mAccess.ModifySettings([this](EffectSettings& settings){
        if (mType == EffectTypeGenerate) {
            settings.extra.SetDuration(mDuration->GetValue());
        }
        GetSettings(settings) = mSettings;
        return nullptr;
    });
    return true;
}

void LadspaEditor::Disconnect()
{
    for (auto& meter : mMeters) {
        if (meter) {
            meter->Disconnect();
            meter = nullptr;
        }
    }
}

void LadspaEditor::OnCheckBox(wxCommandEvent& evt)
{
    int p = evt.GetId() - ID_Toggles;
    // 0.5 is a half of the interval
    UpdateControl(p, mToggles[p]->GetValue(), 0.5f);
    ValidateUI();
}

void LadspaEditor::OnSlider(wxCommandEvent& evt)
{
    int p = evt.GetId() - ID_Sliders;

    float val;
    float lower = float(0.0);
    float upper = float(10.0);
    float range;
    bool forceint = false;

    LADSPA_PortRangeHint hint = mInstance.mData->PortRangeHints[p];
    if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor)) {
        lower = hint.LowerBound;
    }
    if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor)) {
        upper = hint.UpperBound;
    }
    if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor)) {
        lower *= mSampleRate;
        upper *= mSampleRate;
        forceint = true;
    }

    range = upper - lower;
    val = (mSliders[p]->GetValue() / 1000.0) * range + lower;
    wxString str;
    if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor) || forceint) {
        str.Printf(wxT("%d"), (int)(val + 0.5));
    } else {
        str = Internat::ToDisplayString(val);
    }

    mFields[p]->SetValue(str);

    UpdateControl(p, val, ControlValueTolerance);
    ValidateUI();
}

void LadspaEditor::OnTextCtrl(wxCommandEvent& evt)
{
    int p = evt.GetId() - ID_Texts;

    float val;
    float lower = float(0.0);
    float upper = float(10.0);
    float range;

    val = Internat::CompatibleToDouble(mFields[p]->GetValue());

    LADSPA_PortRangeHint hint = mInstance.mData->PortRangeHints[p];
    if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor)) {
        lower = hint.LowerBound;
    }
    if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor)) {
        upper = hint.UpperBound;
    }
    if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor)) {
        lower *= mSampleRate;
        upper *= mSampleRate;
    }
    range = upper - lower;
    if (val < lower) {
        val = lower;
    }
    if (val > upper) {
        val = upper;
    }

    mSliders[p]->SetValue((int)(((val - lower) / range) * 1000.0 + 0.5));

    UpdateControl(p, val, ControlValueTolerance);
    ValidateUI();
}

void LadspaEditor::RefreshControls()
{
    if (!mParent) {
        return;
    }

    // Copy from the dialog
    UpdateControls(GetSettings(mAccess.Get()));

    auto& controls = mSettings.controls;

    const auto& data = *mInstance.mData;
    for (unsigned long p = 0; p < data.PortCount; ++p) {
        LADSPA_PortDescriptor d = data.PortDescriptors[p];
        if (!(LADSPA_IS_PORT_CONTROL(d))) {
            continue;
        }

        wxString fieldText;
        LADSPA_PortRangeHint hint = data.PortRangeHints[p];

        bool forceint = false;
        if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor)) {
            forceint = true;
        }

        if (LADSPA_IS_PORT_OUTPUT(d)) {
            continue;
        }

        if (LADSPA_IS_HINT_TOGGLED(hint.HintDescriptor)) {
            mToggles[p]->SetValue(controls[p] > 0);
            continue;
        }

        if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor) || forceint) {
            fieldText.Printf(wxT("%d"), (int)(controls[p] + 0.5));
        } else {
            fieldText = Internat::ToDisplayString(controls[p]);
        }

        // Set the textctrl value.  This will trigger an event so OnTextCtrl()
        // can update the slider.
        mFields[p]->SetValue(fieldText);
    }
}

void LadspaEditor::UpdateControl(int index, float value, float epsilon)
{
    auto& controls = mSettings.controls;

    assert(index < static_cast<int>(controls.size()));

    if (std::abs(controls[index] - value) < epsilon) {
        return;
    }

    controls[index] = value;
    Publish({ size_t(index), value });
}

void LadspaEditor::UpdateControls(const LadspaEffectSettings& src)
{
    const auto& data = *mInstance.mData;

    for (size_t portIndex = 0, portsCount = src.controls.size();
         portIndex < portsCount;
         ++portIndex) {
        LADSPA_PortDescriptor d = data.PortDescriptors[portIndex];

        if (!(LADSPA_IS_PORT_CONTROL(d)) || (LADSPA_IS_PORT_OUTPUT(d))) {
            continue;
        }

        LADSPA_PortRangeHint hint = mInstance.mData->PortRangeHints[portIndex];

        const bool isIntValue = (LADSPA_IS_HINT_TOGGLED(hint.HintDescriptor))
                                || (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor))
                                || (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor));

        UpdateControl(
            portIndex, src.controls[portIndex],
            isIntValue ? 0.5f : ControlValueTolerance);
    }
}
