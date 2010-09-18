/**********************************************************************

  Audacity: A Digital Audio Editor

  ScoreAlignDialog.h

**********************************************************************/

#ifndef __AUDACITY_SCORE_ALIGN_DIALOG__
#define __AUDACITY_SCORE_ALIGN_DIALOG__

#include <wx/dialog.h>
#include <wx/slider.h>
#include "ScoreAlignParams.h"

class wxButton;
class wxSizer;
class wxString;

void CloseScoreAlignDialog();

//----------------------------------------------------------------------------
// ScoreAlignDialog
//----------------------------------------------------------------------------

// Declare window functions

class ScoreAlignDialog : public wxDialog
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
   ScoreAlignDialog(wxWindow * parent, ScoreAlignParams &params);
   ~ScoreAlignDialog();

   bool TransferDataFromWindow();

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

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3


