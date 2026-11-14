/**********************************************************************

  Audacity: A Digital Audio Editor

  LegacyCompressor.cpp

  Dominic Mazzoni
  Martyn Shaw
  Steve Jolly

*******************************************************************//**

\class CompressorPanel
\brief Panel used within the EffectLegacyCompressor for EffectLegacyCompressor.

*//*******************************************************************/
#include "LegacyCompressor.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

#include <wx/brush.h>
#include <wx/checkbox.h>
#include <wx/dcclient.h>
#include <wx/slider.h>
#include <wx/stattext.h>

#include "AColor.h"
#include "ShuttleGui.h"
#include "Theme.h"
#include "float_cast.h"
#include "../widgets/LinearUpdater.h"
#include "../widgets/Ruler.h"
#include "../widgets/LinearDBFormat.h"
#include "../widgets/LinearUpdater.h"

#include "AllThemeResources.h"

enum
{
    ID_Threshold = 10000,
    ID_NoiseFloor,
    ID_Ratio,
    ID_Attack,
    ID_Decay
};

namespace {
BuiltinEffectsModule::Registration<EffectLegacyCompressor> reg;
}

BEGIN_EVENT_TABLE(EffectLegacyCompressor, wxEvtHandler)
EVT_SLIDER(wxID_ANY, EffectLegacyCompressor::OnSlider)
END_EVENT_TABLE()

// Effect Implementation

namespace {
TranslatableString ThresholdFormat(int value)
/* i18n-hint: usually leave this as is as dB doesn't get translated*/
{ return XO("%3d dB").Format(value); }

TranslatableString AttackTimeFormat(double value)
{ return XO("%.2f secs").Format(value); }

TranslatableString DecayTimeFormat(double value)
{ return XO("%.1f secs").Format(value); }

TranslatableString RatioTextFormat(int sliderValue, double value)
{
    auto format = (sliderValue % 10 == 0)
                  /* i18n-hint: Unless your language has a different convention for ratios,
                   * like 8:1, leave as is.*/
                  ? XO("%.0f:1")
                  /* i18n-hint: Unless your language has a different convention for ratios,
                   * like 8:1, leave as is.*/
                  : XO("%.1f:1");
    return format.Format(value);
}

TranslatableString RatioLabelFormat(int sliderValue, double value)
{
    auto format = (sliderValue % 10 == 0)
                  ? XO("Ratio %.0f to 1")
                  : XO("Ratio %.1f to 1");
    return format.Format(value);
}
}

std::unique_ptr<EffectEditor> EffectLegacyCompressor::PopulateOrExchange(
    ShuttleGui& S, EffectInstance&, EffectSettingsAccess&, const EffectOutputs*)
{
    mUIParent = S.GetParent();
    S.SetBorder(5);

    S.StartHorizontalLay(wxEXPAND, true);
    {
        S.SetBorder(10);
        mPanel = safenew EffectLegacyCompressorPanel(
            S.GetParent(), wxID_ANY, mThresholdDB, mNoiseFloorDB, mRatio);
        S.Prop(true)
        .Position(wxEXPAND | wxALL)
        .MinSize({ 400, 200 })
        .AddWindow(mPanel);
        S.SetBorder(5);
    }
    S.EndHorizontalLay();

    S.StartStatic({});
    {
        S.StartMultiColumn(3, wxEXPAND);
        {
            S.SetStretchyCol(1);
            mThresholdLabel = S.AddVariableText(XO("&Threshold:"), true,
                                                wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
            mThresholdSlider = S.Id(ID_Threshold)
                               .Name(XO("Threshold"))
                               .Style(wxSL_HORIZONTAL)
                               .AddSlider({},
                                          Threshold.def * Threshold.scale,
                                          Threshold.max * Threshold.scale,
                                          Threshold.min * Threshold.scale);
            mThresholdText = S.AddVariableText(ThresholdFormat(999), true,
                                               wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

            mNoiseFloorLabel = S.AddVariableText(XO("&Noise Floor:"), true,
                                                 wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
            mNoiseFloorSlider = S.Id(ID_NoiseFloor)
                                .Name(XO("Noise Floor"))
                                .Style(wxSL_HORIZONTAL)
                                .AddSlider({},
                                           NoiseFloor.def * NoiseFloor.scale,
                                           NoiseFloor.max * NoiseFloor.scale,
                                           NoiseFloor.min * NoiseFloor.scale);
            mNoiseFloorText = S.AddVariableText(ThresholdFormat(999),
                                                true, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

            mRatioLabel = S.AddVariableText(XO("&Ratio:"), true,
                                            wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
            mRatioSlider = S.Id(ID_Ratio)
                           .Name(XO("Ratio"))
                           .Style(wxSL_HORIZONTAL)
                           .AddSlider({},
                                      Ratio.def * Ratio.scale,
                                      Ratio.max * Ratio.scale,
                                      Ratio.min * Ratio.scale);
            mRatioSlider->SetPageSize(5);
            mRatioText = S.AddVariableText(RatioTextFormat(1, 99.9), true,
                                           wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

            /* i18n-hint: Particularly in percussion, sounds can be regarded as having
             * an 'attack' phase where the sound builds up and a 'decay' where the
             * sound dies away.  So this means 'onset duration'.  */
            mAttackLabel = S.AddVariableText(XO("&Attack Time:"), true,
                                             wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
            mAttackSlider = S.Id(ID_Attack)
                            /* i18n-hint: Particularly in percussion, sounds can be regarded as having
                             * an 'attack' phase where the sound builds up and a 'decay' where the
                             * sound dies away.  So this means 'onset duration'.  */
                            .Name(XO("Attack Time"))
                            .Style(wxSL_HORIZONTAL)
                            .AddSlider({},
                                       AttackTime.def * AttackTime.scale,
                                       AttackTime.max * AttackTime.scale,
                                       AttackTime.min * AttackTime.scale);
            mAttackText = S.AddVariableText(
                AttackTimeFormat(9.99),
                true, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

            /* i18n-hint: Particularly in percussion, sounds can be regarded as having
             * an 'attack' phase where the sound builds up and a 'decay' or 'release' where the
             * sound dies away.  */
            mDecayLabel = S.AddVariableText(XO("R&elease Time:"), true,
                                            wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
            mDecaySlider = S.Id(ID_Decay)
                           /* i18n-hint: Particularly in percussion, sounds can be regarded as having
                            * an 'attack' phase where the sound builds up and a 'decay' or 'release' where the
                            * sound dies away.  */
                           .Name(XO("Release Time"))
                           .Style(wxSL_HORIZONTAL)
                           .AddSlider({},
                                      ReleaseTime.def * ReleaseTime.scale,
                                      ReleaseTime.max * ReleaseTime.scale,
                                      ReleaseTime.min * ReleaseTime.scale);

            mDecayText = S.AddVariableText(
                DecayTimeFormat(99.9),
                true, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);
        }
        S.EndMultiColumn();
    }
    S.EndStatic();

    S.StartHorizontalLay(wxCENTER, false);
    {
        /* i18n-hint: Make-up, i.e. correct for any reduction, rather than fabricate it.*/
        mGainCheckBox = S.AddCheckBox(XXO("Ma&ke-up gain for 0 dB after compressing"),
                                      Normalize.def);
        /* i18n-hint: "Compress" here means reduce variations of sound volume,
         NOT related to file-size compression; Peaks means extremes in volume */
        mPeakCheckBox = S.AddCheckBox(XXO("C&ompress based on Peaks"),
                                      UsePeak.def);
    }
    S.EndHorizontalLay();
    return nullptr;
}

bool EffectLegacyCompressor::TransferDataToWindow(const EffectSettings&)
{
    mThresholdSlider->SetValue(lrint(mThresholdDB));
    mNoiseFloorSlider->SetValue(lrint(mNoiseFloorDB * NoiseFloor.scale));
    mRatioSlider->SetValue(lrint(mRatio * Ratio.scale));
    mAttackSlider->SetValue(lrint(mAttackTime * AttackTime.scale));
    mDecaySlider->SetValue(lrint(mDecayTime * ReleaseTime.scale));
    mGainCheckBox->SetValue(mNormalize);
    mPeakCheckBox->SetValue(mUsePeak);

    UpdateUI();

    return true;
}

bool EffectLegacyCompressor::TransferDataFromWindow(EffectSettings&)
{
    if (!mUIParent->Validate()) {
        return false;
    }
    return DoTransferDataFromWindow();
}

bool EffectLegacyCompressor::DoTransferDataFromWindow()
{
    // To do:  eliminate this by using control validators instead
    mThresholdDB = (double)mThresholdSlider->GetValue();
    mNoiseFloorDB = (double)mNoiseFloorSlider->GetValue() / NoiseFloor.scale;
    mRatio = (double)mRatioSlider->GetValue() / Ratio.scale;
    mAttackTime = (double)mAttackSlider->GetValue() / 100.0; //AttackTime.scale;
    mDecayTime = (double)mDecaySlider->GetValue() / ReleaseTime.scale;
    mNormalize = mGainCheckBox->GetValue();
    mUsePeak = mPeakCheckBox->GetValue();

    return true;
}

void EffectLegacyCompressor::OnSlider(wxCommandEvent& WXUNUSED(evt))
{
    DoTransferDataFromWindow();
    UpdateUI();
}

void EffectLegacyCompressor::UpdateUI()
{
    mThresholdLabel->SetName(wxString::Format(_("Threshold %d dB"), (int)mThresholdDB));
    mThresholdText->SetLabel(ThresholdFormat((int)mThresholdDB).Translation());
    mThresholdText->SetName(mThresholdText->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)

    mNoiseFloorLabel->SetName(wxString::Format(_("Noise Floor %d dB"), (int)mNoiseFloorDB));
    mNoiseFloorText->SetLabel(ThresholdFormat((int)mNoiseFloorDB).Translation());
    mNoiseFloorText->SetName(mNoiseFloorText->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)

    mRatioLabel->SetName(
        RatioLabelFormat(mRatioSlider->GetValue(), mRatio).Translation());
    mRatioText->SetLabel(
        RatioTextFormat(mRatioSlider->GetValue(), mRatio).Translation());
    mRatioText->SetName(mRatioText->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)

    mAttackLabel->SetName(wxString::Format(_("Attack Time %.2f secs"), mAttackTime));
    mAttackText->SetLabel(AttackTimeFormat(mAttackTime).Translation());
    mAttackText->SetName(mAttackText->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)

    mDecayLabel->SetName(wxString::Format(_("Release Time %.1f secs"), mDecayTime));
    mDecayText->SetLabel(DecayTimeFormat(mDecayTime).Translation());
    mDecayText->SetName(mDecayText->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)

    mPanel->Refresh(false);

    return;
}

//----------------------------------------------------------------------------
// EffectLegacyCompressorPanel
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(EffectLegacyCompressorPanel, wxPanelWrapper)
EVT_PAINT(EffectLegacyCompressorPanel::OnPaint)
EVT_SIZE(EffectLegacyCompressorPanel::OnSize)
END_EVENT_TABLE()

EffectLegacyCompressorPanel::EffectLegacyCompressorPanel(
    wxWindow* parent, wxWindowID winid, double& threshold, double& noiseFloor,
    double& ratio)
    : wxPanelWrapper(parent, winid)
    , threshold(threshold)
    , noiseFloor(noiseFloor)
    , ratio(ratio)
{
}

void EffectLegacyCompressorPanel::OnPaint(wxPaintEvent& WXUNUSED(evt))
{
    wxPaintDC dc(this);

    int width, height;
    GetSize(&width, &height);

    double rangeDB = 60;

    // Ruler
    int w = 0;
    int h = 0;

    Ruler vRuler{ LinearUpdater::Instance(), LinearDBFormat::Instance() };
    vRuler.SetBounds(0, 0, width, height);
    vRuler.SetOrientation(wxVERTICAL);
    vRuler.SetRange(0, -rangeDB);
    vRuler.SetUnits(XO("dB"));
    vRuler.GetMaxSize(&w, NULL);

    Ruler hRuler{ LinearUpdater::Instance(), LinearDBFormat::Instance() };
    hRuler.SetBounds(0, 0, width, height);
    hRuler.SetOrientation(wxHORIZONTAL);
    hRuler.SetRange(-rangeDB, 0);
    hRuler.SetUnits(XO("dB"));
    hRuler.SetFlip(true);
    hRuler.GetMaxSize(NULL, &h);

    vRuler.SetBounds(0, 0, w, height - h);
    hRuler.SetBounds(w, height - h, width, height);

    vRuler.SetTickColour(theTheme.Colour(clrGraphLabels));
    hRuler.SetTickColour(theTheme.Colour(clrGraphLabels));

#if defined(__WXMSW__)
    dc.Clear();
#endif

    wxRect border;
    border.x = w;
    border.y = 0;
    border.width = width - w;
    border.height = height - h + 1;

    dc.SetBrush(*wxWHITE_BRUSH);
    dc.SetPen(*wxBLACK_PEN);
    dc.DrawRectangle(border);

    wxRect envRect = border;
    envRect.Deflate(2, 2);

    int kneeX = lrint((rangeDB + threshold) * envRect.width / rangeDB);
    int kneeY = lrint((rangeDB + threshold / ratio) * envRect.height / rangeDB);

    int finalY = envRect.height;
    int startY = lrint((threshold * (1.0 / ratio - 1.0)) * envRect.height / rangeDB);

    // Yellow line for threshold
/*   dc.SetPen(wxPen(wxColour(220, 220, 0), 1, wxSOLID));
   AColor::Line(dc,
                envRect.x,
                envRect.y + envRect.height - kneeY,
                envRect.x + envRect.width - 1,
                envRect.y + envRect.height - kneeY);*/

    // Was: Nice dark red line for the compression diagram
//   dc.SetPen(wxPen(wxColour(180, 40, 40), 3, wxSOLID));

    // Nice blue line for compressor, same color as used in the waveform envelope.
    dc.SetPen(AColor::WideEnvelopePen);

    AColor::Line(dc,
                 envRect.x,
                 envRect.y + envRect.height - startY,
                 envRect.x + kneeX - 1,
                 envRect.y + envRect.height - kneeY);

    AColor::Line(dc,
                 envRect.x + kneeX,
                 envRect.y + envRect.height - kneeY,
                 envRect.x + envRect.width - 1,
                 envRect.y + envRect.height - finalY);

    // Paint border again
    dc.SetBrush(*wxTRANSPARENT_BRUSH);
    dc.SetPen(*wxBLACK_PEN);
    dc.DrawRectangle(border);

    vRuler.Draw(dc);
    hRuler.Draw(dc);
}

void EffectLegacyCompressorPanel::OnSize(wxSizeEvent& WXUNUSED(evt))
{
    Refresh(false);
}
