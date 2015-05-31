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
#include <wx/statbmp.h>
#include <wx/dcbuffer.h>
#include <wx/colordlg.h>

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

	wxArrayString mColorScaleChoices;
	wxArrayInt mColorScaleCodes;

	wxTextCtrl *R1, *R2, *R3, *R4, *R5, *G1, *G2, *G3, *G4, *G5, *B1, *B2, *B3, *B4, *B5;
	int cScale;
	float *red, *blue, *green;
	wxColour C1, C2, C3, C4, C5;

	void PaintColorScale();
	void OnRefresh(wxCommandEvent & e);
	void OnChangeColor1(wxCommandEvent& e);
	void OnChangeColor2(wxCommandEvent& e);
	void OnChangeColor3(wxCommandEvent& e);
	void OnChangeColor4(wxCommandEvent& e);
	void OnChangeColor5(wxCommandEvent& e);

	DECLARE_EVENT_TABLE();

#ifdef EXPERIMENTAL_FIND_NOTES
   wxTextCtrl *mFindNotesMinA;
   wxTextCtrl *mFindNotesN;
#endif
};

#endif
