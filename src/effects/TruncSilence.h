/**********************************************************************

  Audacity: A Digital Audio Editor

  TruncSilence.h

  Lynn Allan (from DM's Normalize)
  //ToDo ... put BlendFrames in Effects, Project, or other class
  //ToDo ... Use ZeroCrossing logic to improve blend
  //ToDo ... BlendFrames on "fade-out"
  //ToDo ... BlendFrameCount is a user-selectable parameter
  //ToDo ... Detect transient signals that are too short to interrupt the TruncatableSilence
  Philip Van Baren (more options and boundary fixes)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_TRUNC_SILENCE__
#define __AUDACITY_EFFECT_TRUNC_SILENCE__

#include <wx/arrstr.h>
#include <wx/event.h>
#include <wx/list.h>
#include <wx/string.h>

#include "Effect.h"

class ShuttleGui;
class wxChoice;
class wxTextCtrl;
class wxCheckBox;

#define TRUNCATESILENCE_PLUGIN_SYMBOL XO("Truncate Silence")

class RegionList;

class EffectTruncSilence final : public Effect
{
public:
   EffectTruncSilence();
   virtual ~EffectTruncSilence();

   // IdentInterface implementation

   wxString GetSymbol() override;
   wxString GetDescription() override;

   // EffectIdentInterface implementation

   EffectType GetType() override;

   // EffectClientInterface implementation

   bool GetAutomationParameters(EffectAutomationParameters & parms) override;
   bool SetAutomationParameters(EffectAutomationParameters & parms) override;

   // Effect implementation

   double CalcPreviewInputLength(double previewLength) override;
   bool Startup() override;

   // Analyze a single track to find silences
   // If inputLength is not NULL we are calculating the minimum
   // amount of input for previewing.
   bool Analyze(RegionList &silenceList,
                        RegionList &trackSilences,
                        WaveTrack* wt,
                        sampleCount* silentFrame,
                        sampleCount* index,
                        int whichTrack,
                        double* inputLength = NULL,
                        double* minInputLength = NULL);

   bool Process() override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

private:
   // EffectTruncSilence implementation

   //ToDo ... put BlendFrames in Effects, Project, or other class
   // void BlendFrames(float* buffer, int leftIndex, int rightIndex, int blendFrameCount);
   void Intersect(RegionList &dest, const RegionList & src);

   void OnControlChange(wxCommandEvent & evt);
   void UpdateUI();

   bool ProcessIndependently();
   bool ProcessAll();
   bool FindSilences
      (RegionList &silences, TrackList *list, Track *firstTrack, Track *lastTrack);
   bool DoRemoval
      (const RegionList &silences, unsigned iGroup, unsigned nGroups, Track *firstTrack, Track *lastTrack,
       double &totalCutLen);

private:

   int mTruncDbChoiceIndex;
   int mActionIndex;
   double mInitialAllowedSilence;
   double mTruncLongestAllowedSilence;
   double mSilenceCompressPercent;
   bool mbIndependent;

   wxArrayString mDbChoices;

   size_t mBlendFrameCount;

   wxChoice *mTruncDbChoice;
   wxChoice *mActionChoice;
   wxTextCtrl *mInitialAllowedSilenceT;
   wxTextCtrl *mTruncLongestAllowedSilenceT;
   wxTextCtrl *mSilenceCompressPercentT;
   wxCheckBox *mIndependent;

   DECLARE_EVENT_TABLE()
};

#endif
