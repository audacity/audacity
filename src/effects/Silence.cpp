/**********************************************************************

  Audacity: A Digital Audio Editor

  Silence.cpp

  Dominic Mazzoni

*******************************************************************//**

\class EffectSilence
\brief An Effect for the "Generator" menu to add silence.

*//*******************************************************************/


#include "../Audacity.h"

#include <wx/defs.h> 

#include <wx/button.h> 
#include <wx/sizer.h> 
#include <wx/stattext.h> 
#include <wx/textctrl.h> 

#include "Silence.h"
#include "../WaveTrack.h"
#include "../TimeDialog.h"

bool EffectSilence::PromptUser()
{
   TimeDialog dlog(mParent, _("Silence Generator"));

   dlog.SetSampleRate(mProjectRate);

   if (mT1 > mT0) {
      // there is a selection: let's fit in there...
      mDuration = mT1 - mT0;
      dlog.SetFormatString(_("hh:mm:ss + samples"));

   } else {
      // retrieve last used values
      dlog.SetFormatString(_("seconds"));
   }
   dlog.SetTimeValue(mDuration);

   if (dlog.ShowModal() == wxID_CANCEL)
      return false;

   mDuration = dlog.GetTimeValue();

   return true;
}

bool EffectSilence::GenerateTrack(WaveTrack *tmp,
                                  const WaveTrack &track,
                                  int ntrack)
{
   const bool bResult = tmp->InsertSilence(0.0, mDuration);
   wxASSERT(bResult);
   return bResult;
}
