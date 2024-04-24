#include "LimiterEditor.h"
#include "ShuttleGui.h"

void LimiterEditor::PopulateOrExchange(ShuttleGui& S)
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
         XXO("Lookahead time in ms"));

      AddTextboxAndSlider(
         S, mReleaseMsCtrl, XXO("&Release (ms):"), XXO("Release time in ms"));

      AddTextboxAndSlider(
         S, mMakeUpCtrl, XXO("&Make-up gain (dB):"), XXO("Make-up gain in dB"));
   }
   S.EndMultiColumn();
}
