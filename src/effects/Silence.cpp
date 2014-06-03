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
#include "../Prefs.h"

bool EffectSilence::PromptUser()
{
   wxString fmt;

   if (mT1 > mT0) {
      // there is a selection: let's fit in there...
      mDuration = mT1 - mT0;
      fmt = _("hh:mm:ss + samples");
   } else {
      // Retrieve last used values
      gPrefs->Read(wxT("/Effects/SilenceGen/Duration"), &mDuration, 30L);
      fmt = _("hh:mm:ss + milliseconds");
   }

   TimeDialog dlog(mParent, _("Silence Generator"), fmt, mProjectRate,
      mDuration );

   if (dlog.ShowModal() == wxID_CANCEL)
      return false;

   mDuration = dlog.GetTimeValue();
   /* Save last used values.
      Save duration unless value was got from selection, so we save only
      when user explicitly set up a value */
   if (mT1 == mT0)
   {
      gPrefs->Write(wxT("/Effects/SilenceGen/Duration"), mDuration);
      gPrefs->Flush();
   }

   return true;
}

bool EffectSilence::GenerateTrack(WaveTrack *tmp,
                                  const WaveTrack & WXUNUSED(track),
                                  int WXUNUSED(ntrack))
{
   const bool bResult = tmp->InsertSilence(0.0, mDuration);
   wxASSERT(bResult);
   return bResult;
}
