/**********************************************************************

  Audacity: A Digital Audio Editor

  ScoreAlignDialog.h

**********************************************************************/

#ifndef __AUDACITY_SCORE_ALIGN_DIALOG__
#define __AUDACITY_SCORE_ALIGN_DIALOG__

#ifdef EXPERIMENTAL_SCOREALIGN

#include "wxPanelWrapper.h"

#if 1

#include "ScoreAlignParams.h"

#else

// Stub definitions
struct ScoreAlignParams
{
   int mStatus;
   double mMidiStart, mMidiEnd;
   double mAudioStart, mAudioEnd;
   float mFramePeriod;
   float mWindowSize;
   float mSilenceThreshold;
   float mForceFinalAlignment;
   float mIgnoreSilence;
   float mPresmoothTime;
   float mLineTime;
   float mSmoothTime;
};
class SAProgress;
class Alg_seq;

extern int scorealign(
   void *data,
   long (*process)(void *data, float **buffer, long n),
   unsigned channels,
   double rate,
   double endTime,
   Alg_seq *seq,
   SAProgress *progress,
   ScoreAlignParams params
);

#endif

class wxButton;
class wxCheckBox;
class wxSlider;
class wxStaticText;

void CloseScoreAlignDialog();

//----------------------------------------------------------------------------
// ScoreAlignDialog
//----------------------------------------------------------------------------

// Declare window functions

class ScoreAlignDialog final : public wxDialogWrapper
{
public:
   ScoreAlignParams p;

   wxStaticText *mFramePeriodLabel;
   wxSlider *mFramePeriodSlider;
   wxStaticText *mFramePeriodText;

   wxStaticText *mWindowSizeLabel;
   wxSlider *mWindowSizeSlider;
   wxStaticText *mWindowSizeText;

   wxStaticText *mSilenceThresholdLabel;
   wxSlider *mSilenceThresholdSlider;
   wxStaticText *mSilenceThresholdText;

   wxCheckBox *mForceFinalAlignmentCheckBox;
   wxCheckBox *mIgnoreSilenceCheckBox;

   wxStaticText *mPresmoothLabel;
   wxSlider *mPresmoothSlider;
   wxStaticText *mPresmoothText;

   wxStaticText *mLineTimeLabel;
   wxSlider *mLineTimeSlider;
   wxStaticText *mLineTimeText;

   wxStaticText *mSmoothTimeLabel;
   wxSlider *mSmoothTimeSlider;
   wxStaticText *mSmoothTimeText;

   wxButton *mDefaultButton;

   // constructors and destructors
   ScoreAlignDialog(ScoreAlignParams &params);
   ~ScoreAlignDialog();

   bool TransferDataFromWindow() override;

private:
   enum {
     ID_BASE = 10000,
     ID_PRESMOOTH,
     ID_WINDOWSIZE,
     ID_FRAMEPERIOD,
     ID_LINETIME,
     ID_SMOOTHTIME,
     ID_SILENCETHRESHOLD,
     ID_FORCEFINALALIGNMENT,
     ID_IGNORESILENCE,
     ID_DEFAULT
   };

   // handlers
   void OnOK(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);
   void OnSlider(wxCommandEvent & event);
   void OnDefault(wxCommandEvent & event);

   DECLARE_EVENT_TABLE()

};

#endif

#endif
