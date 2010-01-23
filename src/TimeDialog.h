/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeDialog.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TimeDialog__
#define __AUDACITY_TimeDialog__

#include <wx/defs.h>
#include <wx/dialog.h>
#include <wx/event.h>
#include <wx/string.h>

#include "widgets/TimeTextCtrl.h"

class ShuttleGui;

class TimeDialog:public wxDialog
{
 public:

   TimeDialog(wxWindow *parent,
              const wxString &title,
              const wxString &prompt = _("Duration"));

   void SetFormatString(wxString formatString);
   void SetSampleRate(double sampleRate);
   void SetTimeValue(double newTime);
   const double GetTimeValue();

 private:

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

   void OnUpdate(wxCommandEvent &event);

 private:

   wxString mPrompt;
   wxString mFormat;
   double mRate;
   double mTime;

   TimeTextCtrl *mTimeCtrl;

   DECLARE_EVENT_TABLE();
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
// arch-tag: 94f72c32-970b-4f4e-bbf3-3880fce7b965
