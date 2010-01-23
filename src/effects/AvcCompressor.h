/**********************************************************************

  Audacity: A Digital Audio Editor

  AvcCompressor.h

  Vincent A. Busam

**********************************************************************/

#ifndef __AUDACITY_EFFECT_AVCCOMPRESSOR__
#define __AUDACITY_EFFECT_AVCCOMPRESSOR__

#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/button.h>
#include <wx/statbox.h>
#include <wx/valtext.h>
#include <wx/checkbox.h>
#include <wx/intl.h>

class iAVCBufferList
{
public:
	// The following 4 values describe the buffers that still need output values inserted
	iAVCBufferList *	mpNext;
	void *				mpLeftBuffer;
	void *				mpRightBuffer;
	sampleCount			mnLen;				// number of entries in buffers
	sampleCount			mnNext;			// next output position in buffers
};

class wxString;

class iAVC;

//!!!!!!!!!!!!!!!!!!!!!!!!!  I M P O R T A N T  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// IMPORTANT:  This define determines type of sample: float or short int
#define  IAVC_FLOAT			// use floating point in iAVC

#include "SimplePairedTwoTrack.h"

#include "../../lib-src/iAVC/iAVC.h"	// for MULTIPLY_PCT_ARRAY_SIZE and IAVCSAMPLETYPE

class WaveTrack;
class AvcCompressorDialog;				// defined later in this file

#ifdef IAVC_FLOAT
	#define AVCCOMPSAMPLETYPE  floatSample
#else
	#define AVCCOMPSAMPLETYPE  int16Sample
#endif

//typedef for IAVCSAMPLETYPE is in iAVC.h

class EffectAvcCompressor: public EffectSimplePairedTwoTrack<IAVCSAMPLETYPE,AVCCOMPSAMPLETYPE> {
   
public:
   
   EffectAvcCompressor();

   virtual ~EffectAvcCompressor();
   
   virtual wxString GetEffectName() {
      return wxString(wxT("Automatic Volume Control..."));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(wxT("Changing volume"));
   }
   
   // Useful only after PromptUser values have been set. 
   virtual wxString GetEffectDescription(); 

   virtual void End();
   
protected:

   virtual bool PromptUser();		// invoked by Effect
   virtual bool TransferParameters( Shuttle & shuttle );
   virtual bool Init();				// invoked by Effect

   // invoked by SimplePairedTwoTrack
   bool ProcessSimplePairedTwoTrack ( /*IAVCSAMPLETYPE*/ void *bufferLeft, 
									  /*IAVCSAMPLETYPE*/ void *bufferRight, // may be 0
									  sampleCount len);
	AutoVolCtrl  mAutoVolCtrl;	// iAVC class (LGPL license)
	long  mnChangeWindow;

	iAVCBufferList * mpBufferList;
	iAVCBufferList * mpBufferPrevious;

	long			 mnDelay;		// delay between when sample set and when it got.

	AvcCompressorDialog*	mpDialog;

private:
   void OutputSample ( IAVCSAMPLETYPE left, IAVCSAMPLETYPE right );

};

// WDR: class declarations

//----------------------------------------------------------------------------
// NoiseRemovalDialog
//----------------------------------------------------------------------------

// Declare window functions

// If you change the following number you may need to change the EVENT_TABLE
#define NUM_CURVE_POINTS    7    // includes (0,0), (32768,32768) points

#define ID_TEXT 10000
#define ID_ADJWINTEXT 10001
#define ID_DELAYTEXT 10002
#define ID_CHANGEWINTEXT 10003
#define ID_MINPCTTEXT 10004

#define ID_RESTORE_DEFAULTS 10005

//following IDs need to be spaced out at least as much as NUM_CURVE_POINTS
#define ID_FIRST_CURVE_CHECK  10100
#define ID_FIRST_CURVE_X      10200
#define ID_FIRST_CURVE_Y      10300

class AvcCompressorDialog: public wxDialog
{
public:
   // constructors and destructors
   AvcCompressorDialog( wxWindow *parent, wxWindowID id, const wxString &title,
                       const wxPoint& pos = wxDefaultPosition,
                       const wxSize& size = wxDefaultSize,
                       long style = wxDEFAULT_DIALOG_STYLE );
   ~AvcCompressorDialog();

	long GetAdjusterWindow() { return mnAdjWin; };
	long GetDelay()          { return mnDelay; };
	long GetChangeWindow()   { return mnChangeWin; };
	long GetMinimumPercent() { return mnMinPct; };
	void GetTransformArray( unsigned short int nTransform[MULTIPLY_PCT_ARRAY_SIZE] );
   
   //wxButton *mRemoveNoiseButton;
   //wxSlider *mSlider;
      
private:
   DECLARE_EVENT_TABLE()

protected:
	wxSizer *MakeAvcCompressorDialog( wxWindow *parent, bool call_fit = TRUE,
									bool set_sizer = TRUE );
    void OnCancel( wxCommandEvent &event );
    void OnOK(wxCommandEvent &event);
	void OnRestoreDefaults(wxCommandEvent &event);
	void OnCheckBox(wxCommandEvent & event);
	void ReadPrefs();
	void WritePrefs();

	bool LongRangeCheck (  wxWindow *window,
						   const long nValue,
						   const long nMin,
						   const long nMax );

	// Values for Adjustment Settings
	wxTextCtrl *mctlAdjWin;
	wxTextCtrl *mctlDelay;
	wxTextCtrl *mctlChangeWin;
	wxTextCtrl *mctlMinPct;

	wxString mstrAdjWin;
	wxString mstrDelay;
	wxString mstrChangeWin;
	wxString mstrMinPct;

	long mnAdjWin;
	long mnDelay;
	long mnChangeWin;
	long mnMinPct;

	// Values for Amplification Settings
	wxCheckBox *mctlCheckBoxes[NUM_CURVE_POINTS];
	wxTextCtrl *mctlXAxis[NUM_CURVE_POINTS];
	wxTextCtrl *mctlYAxis[NUM_CURVE_POINTS];

	wxString mstrXAxis[NUM_CURVE_POINTS];
	wxString mstrYAxis[NUM_CURVE_POINTS];

	long mnXAxis[NUM_CURVE_POINTS];	
	long mnYAxis[NUM_CURVE_POINTS];
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
// arch-tag: 59116dc2-05df-4997-9be0-eca86dcf5ce3

