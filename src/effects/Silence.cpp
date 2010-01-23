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
      dlog.SetFormatString(wxT("hh:mm:ss + samples"));

   } else {
      // retrieve last used values
      dlog.SetFormatString(wxT("seconds"));
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
   tmp->InsertSilence(0.0, mDuration);
   return true;
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 78c8d521-815a-4fdb-830a-f9655cd4f529
