#include "CompressorEditor.h"
#include "ShuttleGui.h"

void CompressorEditor::PopulateOrExchange(ShuttleGui& S)
{
   mUIParent = S.GetParent();
   auto& ms = mSettings;

   S.SetBorder(5);
   S.AddSpace(0, 5);

   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol(2);

      AddTextboxAndSlider(
         S, mThresholdDbCtrl, XXO("&Threshold (Db):"), XXO("Threshold in dB"));

      AddTextboxAndSlider(
         S, mKneeDbCtrl, XXO("&Knee (dB):"), XXO("Knee width in dB"));

      AddTextboxAndSlider(
         S, mLookaheadMsCtrl, XXO("&Lookahead (ms):"),
         XXO("Lookahead time in ms"),
         [&](wxCommandEvent& evt) { OnLookaheadMsSlider(evt); });

      AddTextboxAndSlider(
         S, mAttackMsCtrl, XXO("&Attack (ms):"), XXO("Attack time in ms"));

      AddTextboxAndSlider(
         S, mReleaseMsCtrl, XXO("&Release (ms):"), XXO("Release time in ms"));

      AddTextboxAndSlider(
         S, mRatioCtrl, XXO("&Ratio:"), XXO("Compression ratio"),
         [&](wxCommandEvent& evt) { OnRatioSlider(evt); },
         CompressorSettings::infRatio);

      AddTextboxAndSlider(
         S, mMakeUpCtrl, XXO("&Make-up gain (dB):"), XXO("Make-up gain in dB"));
   }
   S.EndMultiColumn();
}

namespace
{
// Assumes x to be in [0, 1]
auto MapExponentially(double x)
{
   constexpr auto C = 5.;
   return (std::exp(C * x) - 1) / (std::exp(C) - 1.);
}

auto MapExponentially(double min, double max, double x)
{
   // Map min and max to 0 and 1, and x accordingly, before applying
   // exponential, or we may run into NaNs.
   const auto a = 1.0 / (max - min);
   const auto b = -min / (max - min);
   const auto result = MapExponentially(a * x + b);
   return (result - b) / a;
}
} // namespace

void CompressorEditor::OnLookaheadMsSlider(wxCommandEvent& evt)
{
   auto& ms = mSettings;
   ms.lookaheadMs = MapExponentially(
      lookaheadMs.min, lookaheadMs.max, evt.GetInt() / lookaheadMs.scale);
}

void CompressorEditor::OnRatioSlider(wxCommandEvent& evt)
{
   auto& ms = mSettings;
   ms.ratio = evt.GetInt() / ratio.scale;
   if (ms.ratio == ratio.max)
      ms.ratio = CompressorSettings::infRatio;
}

bool CompressorEditor::UpdateUI()
{
   auto& ms = mSettings;
   mAttackMsCtrl.slider->SetValue((int)(ms.attackMs * attackMs.scale));
   mRatioCtrl.slider->SetValue((int)(ms.ratio * ratio.scale));
   return DynamicRangeProcessorEditor<
      CompressorEditor, CompressorSettings>::UpdateUI();
}
