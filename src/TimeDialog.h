/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeDialog.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TimeDialog__
#define __AUDACITY_TimeDialog__

#include <wx/defs.h>

#include "ComponentInterface.h" // member variable

#include "wxPanelWrapper.h" // to inherit

class NumericTextCtrl;
class ShuttleGui;

class AUDACITY_DLL_API TimeDialog final : public wxDialogWrapper
{
 public:

   TimeDialog(wxWindow *parent,
              const TranslatableString &title,
              const NumericFormatSymbol &format,
              double rate,
              double time,
              const TranslatableString &prompt = XO("Duration"));

   void SetFormatString(const NumericFormatSymbol &formatString);
   void SetSampleRate(double sampleRate);
   void SetTimeValue(double newTime);
   const double GetTimeValue();

 private:

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

   void OnUpdate(wxCommandEvent &event);

 private:

   TranslatableString mPrompt;
   NumericFormatSymbol mFormat;
   double mRate;
   double mTime;

   NumericTextCtrl *mTimeCtrl;

   DECLARE_EVENT_TABLE()
};

#endif
