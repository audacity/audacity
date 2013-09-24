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
