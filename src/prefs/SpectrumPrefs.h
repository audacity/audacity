/**********************************************************************

  Audacity: A Digital Audio Editor

  SpectrumPrefs.h

  Dominic Mazzoni
  James Crook

**********************************************************************/
/*
  Salvo Ventura
  November 2006

  Added selection box for windowType

  All params are saved in config file.
*/


#ifndef __AUDACITY_SPECTRUM_PREFS__
#define __AUDACITY_SPECTRUM_PREFS__

#include <wx/defs.h>
#include <wx/string.h>
#include <wx/window.h>

#include "../Experimental.h"
#include "../ShuttleGui.h"

#include "PrefsPanel.h"

class SpectrumPrefs:public PrefsPanel
{
 public:
   SpectrumPrefs(wxWindow * parent);
   virtual ~SpectrumPrefs();
   virtual bool Apply();
   virtual bool Validate();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);

   wxTextCtrl *mMinFreq;
   wxTextCtrl *mMaxFreq;
   wxTextCtrl *mGain;
   wxTextCtrl *mRange;
   wxTextCtrl *mFrequencyGain;

   wxArrayString mSizeChoices;
   wxArrayInt mSizeCodes;

   wxArrayString mTypeChoices;
   wxArrayInt mTypeCodes;


#ifdef EXPERIMENTAL_FIND_NOTES
   wxTextCtrl *mFindNotesMinA;
   wxTextCtrl *mFindNotesN;
#endif
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
// arch-tag: d68b1a74-12e3-49a5-a50b-3ba6fe65b40b

