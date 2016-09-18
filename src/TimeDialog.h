/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeDialog.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TimeDialog__
#define __AUDACITY_TimeDialog__

#include <wx/defs.h>
#include <wx/event.h>
#include <wx/string.h>

#include "widgets/wxPanelWrapper.h"

class NumericTextCtrl;
class ShuttleGui;

class TimeDialog final : public wxDialogWrapper
{
 public:

   TimeDialog(wxWindow *parent,
              const wxString &title,
              const wxString &format,
              double rate,
              double time,
              const wxString &prompt = _("Duration"));

   void SetFormatString(const wxString &formatString);
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

   NumericTextCtrl *mTimeCtrl;

   DECLARE_EVENT_TABLE()
};

#endif
