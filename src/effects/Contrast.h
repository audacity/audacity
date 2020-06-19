/**********************************************************************

  Audacity: A Digital Audio Editor

  Contrast.h

**********************************************************************/

#ifndef __AUDACITY_CONTRAST_DIALOG__
#define __AUDACITY_CONTRAST_DIALOG__

#include "wxPanelWrapper.h" // to inherit

class wxButton;
class wxTextCtrl;

class Envelope;
class NumericTextCtrl;
class WaveTrack;

//----------------------------------------------------------------------------
// ContrastDialog
//----------------------------------------------------------------------------

// Declare window functions

class ContrastDialog final : public wxDialogWrapper
{
public:
   // constructors and destructors
   ContrastDialog(wxWindow * parent, wxWindowID id,
              const TranslatableString & title, const wxPoint & pos);

   wxButton * m_pButton_UseCurrentF;
   wxButton * m_pButton_UseCurrentB;
   wxButton * m_pButton_GetURL;
   wxButton * m_pButton_Export;
   wxButton * m_pButton_Reset;
   wxButton * m_pButton_Close;

   NumericTextCtrl *mForegroundStartT;
   NumericTextCtrl *mForegroundEndT;
   NumericTextCtrl *mBackgroundStartT;
   NumericTextCtrl *mBackgroundEndT;

   double mT0;
   double mT1;
   double mProjectRate;
   double mStartTimeF;
   double mEndTimeF;
   double mStartTimeB;
   double mEndTimeB;

private:
   // handlers
   void OnChar(wxKeyEvent &event);
   void OnGetURL(wxCommandEvent &event);
   void OnExport(wxCommandEvent &event);
   void OnGetForeground(wxCommandEvent & event);
   void OnGetBackground(wxCommandEvent & event);
   void results();
   void OnReset(wxCommandEvent & event);
   void OnClose(wxCommandEvent & event);

   wxTextCtrl *mForegroundRMSText;
   wxTextCtrl *mBackgroundRMSText;
   wxTextCtrl *mPassFailText;
   wxTextCtrl *mDiffText;

   float foregrounddB;
   float backgrounddB;
   bool  mForegroundIsDefined;
   bool  mBackgroundIsDefined;
   double mT0orig;
   double mT1orig;

   bool mDoBackground;
   bool GetDB(float & dB);
   void SetStartAndEndTime();

   double length;

   DECLARE_EVENT_TABLE()

};

#endif
