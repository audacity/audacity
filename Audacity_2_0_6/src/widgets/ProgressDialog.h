/**********************************************************************

  Audacity: A Digital Audio Editor

  ProgressDialog.h

  Copyright
     Leland Lucius
     Vaughan Johnson

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

*************************************************************************/

#ifndef __AUDACITY_WIDGETS_PROGRESSDIALOG__
#define __AUDACITY_WIDGETS_PROGRESSDIALOG__

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/dialog.h>
#include <wx/gauge.h>
#include <wx/stattext.h>
#include <wx/utils.h>

enum
{
   eProgressCancelled = 0, //<! User says that whatever is happening is undesirable and shouldn't have happened at all
   eProgressSuccess,       //<! User says nothing, everything works fine, continue doing whatever we're doing
   eProgressFailed,        //<! Something has gone wrong, we should stop and cancel everything we did
   eProgressStopped        //<! Nothing is wrong, but user says we should stop now and leave things as they are now
};

enum ProgressDialogFlags
{
   pdlgEmptyFlags = 0x00000000,
   pdlgHideStopButton = 0x00000001,
   pdlgHideCancelButton = 0x00000002
} ;

////////////////////////////////////////////////////////////
/// ProgressDialog Class
////////////////////////////////////////////////////////////

class AUDACITY_DLL_API ProgressDialog:public wxDialog
{

 public:

   ProgressDialog(const wxString & title, const wxString & message = wxEmptyString, ProgressDialogFlags flags = pdlgEmptyFlags);
   virtual ~ProgressDialog();

   bool Show(bool show = true);

   int Update(int value, const wxString & message = wxEmptyString);
   int Update(double current, const wxString & message = wxEmptyString);
   int Update(double current, double total, const wxString & message = wxEmptyString);
   int Update(wxULongLong_t current, wxULongLong_t total, const wxString & message = wxEmptyString);
   int Update(wxLongLong current, wxLongLong total, const wxString & message = wxEmptyString);
   int Update(wxLongLong_t current, wxLongLong_t total, const wxString & message = wxEmptyString);
   int Update(int current, int total, const wxString & message = wxEmptyString);
   void SetMessage(const wxString & message);

 private:
   void OnCancel(wxCommandEvent & e);
   void OnStop(wxCommandEvent & e);
   void OnCloseWindow(wxCloseEvent & e);
   void Beep();

 protected:
   wxStaticText *mElapsed;
   wxStaticText *mRemaining;
   wxGauge *mGauge;

   wxLongLong_t mStartTime;
   wxLongLong_t mLastUpdate;
   int mLastValue; // gauge value, range = [0,1000]

   bool mCancel;
   bool mStop;

 private:
   wxWindow *mHadFocus;
   wxStaticText *mMessage;
   wxWindowDisabler *mDisable;

   DECLARE_EVENT_TABLE();
};

class AUDACITY_DLL_API TimerProgressDialog : public ProgressDialog
{
 public:
   TimerProgressDialog(const wxLongLong_t duration,
                        const wxString & title,
                        const wxString & message = wxEmptyString,
                        ProgressDialogFlags flags = pdlgEmptyFlags);
   int Update(const wxString & message = wxEmptyString);

 protected:
   wxLongLong_t mDuration;
};

#endif
