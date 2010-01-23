/**********************************************************************

  Audacity: A Digital Audio Editor

  AvcCompressor.cpp

  Vincent A. Busam

*******************************************************************//**

\class EffectAvcCompressor
\brief An unused Effect (we use EffectCompressor instead) derived 
from EffectSimplePairedTwoTrackBase

*//****************************************************************//**

\class iAVCBufferList
\brief An unused class used by EffectAvcCompressor.

*//****************************************************************//**

\class AvcCompressorDialog
\brief An unused class used by unused effect, EffectAvcCompressor.

*//*******************************************************************/
/*  TODO List:

  1.  Add graph shows curve specified by grid, keep it up to date,
        allow setting of grid points by moving points in grid.
  2.  Better help
  3.  Radio button selection so Adjustment Settings can be times instead
		of samples.
  4.  Radio button selection so Amplification Settings can be db instead
        of raw values.
  5.  Save settings by name
  6.  Remove "help" text in window when Audacity help available.

*/

#include <math.h>

#include <wx/defs.h>
#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/dcmemory.h>

#include "../WaveTrack.h"
#include "../Envelope.h"
#include "../widgets/Ruler.h"
#include "../Prefs.h"

// Including the following cpp file is quite unorthodox, but it gives the opportunity to
//		use iAVC's capability of generating inline code for the important methods.  We
//		can't trust compilers to generated inline code, even when the inline keyword is
//		used.
#ifdef _WINDOWS				// kludge for Audacity since we don't really have MS Windows
	#define max(a,b)  ( (a<b)?b:a )
#endif

//!!!!!!!!!!!!!!!!!!!!!!!!!  I M P O R T A N T  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// IMPORTANT:  This define determines if iAVC generates in line code.
//#define  IAVC_INLINE		// use inline code for get and put of samples

#include "AvcCompressor.h"

#include "../../lib-src/iAVC/iAVCsamples.h"
#include "../../lib-src/iAVC/iAVC.cpp"

#define ADJWIN_DEFAULT  DEFAULT_ADJUSTER_WINDOW_SIZE
#define ADJWIN_MIN		1000
#define ADJWIN_MAX		10000

#define DELAY_DEFAULT	DEFAULT_MINIMUM_SAMPLES_BEFORE_SWITCH
#define DELAY_MIN		0
#define DELAY_MAX		5000

#define CHANGE_DEFAULT  DEFAULT_MINIMUM_SAMPLES_BEFORE_SWITCH
#define CHANGE_MIN		1000
#define CHANGE_MAX		5000

#define MINPCT_DEFAULT	DEFAULT_MAX_PCT_CHANGE_AT_ONCE
#define MINPCT_MIN		5
#define MINPCT_MAX		50

EffectAvcCompressor::EffectAvcCompressor():
			mpBufferList ( NULL ),
			mpBufferPrevious ( NULL ),
			mnDelay ( 0 ),
			mpDialog ( NULL )
{
}

EffectAvcCompressor::~EffectAvcCompressor()
{
	if ( mpDialog != NULL )
		delete mpDialog;
}

wxString EffectAvcCompressor::GetEffectDescription() { 
   return "Applied effect: AVC"; // XXX: temporary

   // Note: This is useful only after values have been set. 

   // FIX-ME: Compile error (cannot pass wxString to Format).
   //return wxString::Format(_("Applied effect: %s"),
   //                        this->GetEffectDescription());

                           //old                           
   //return wxString::Format("Applied effect: %s change window = %d samples", 
   //(const char *)(this->GetEffectName()), mnChangeWindow); 
} 

inline
void EffectAvcCompressor::OutputSample ( IAVCSAMPLETYPE left, IAVCSAMPLETYPE right )
{
	if ( mpBufferList->mnNext >= mpBufferList->mnLen )
	{	// have filled up this buffer, move to next
		iAVCBufferList * pOld = mpBufferList;
		mpBufferList = mpBufferList->mpNext;
		delete pOld;
	}
	if ( mpBufferList == NULL )
		return;

	// set the output sample values
	((IAVCSAMPLETYPE*)mpBufferList->mpLeftBuffer)[mpBufferList->mnNext] = left;
	if ( mpBufferList->mpRightBuffer )
		((IAVCSAMPLETYPE*)mpBufferList->mpRightBuffer)[mpBufferList->mnNext] = right;

	++mpBufferList->mnNext;
}


bool EffectAvcCompressor::PromptUser()
{
   if ( mpDialog == NULL )	// reuse dialog so we keep user changes to values
	   mpDialog = new AvcCompressorDialog(mParent, -1, wxT("Automatic Volume Control"));

   mpDialog->CentreOnParent();
   mpDialog->ShowModal();

   if (!mpDialog->GetReturnCode())
      return false;

   mAutoVolCtrl.Reset();	// reset control before setting values

   // Set parameters for iAVC class
   unsigned short int *nTransform = new unsigned short int [ MULTIPLY_PCT_ARRAY_SIZE ];
   mnChangeWindow=mpDialog->GetChangeWindow();
   mpDialog->GetTransformArray(nTransform);

   if ( mAutoVolCtrl.SetSampleWindowSize(mpDialog->GetAdjusterWindow()+mnChangeWindow,
											mpDialog->GetAdjusterWindow(),
											0) == false ||
											mAutoVolCtrl.SetMinSamplesBeforeSwitch(mnChangeWindow) == false ) {
            wxMessageBox("Error setting parameters for automatic volume control.");
            return false;
   }
   mAutoVolCtrl.SetMaxPctChangeAtOnce(mpDialog->GetMinimumPercent());
   mAutoVolCtrl.SetMultipliers(nTransform);
   mAutoVolCtrl.SetNumberTracks(mnTracks);
   mnDelay = mpDialog->GetDelay();

   delete [] nTransform;

   return true;
}

bool EffectAvcCompressor::TransferParameters( Shuttle & shuttle )
{ 
   wxASSERT( false );// Not yet implemented.
//   shuttle.TransferInt("",,0);
   return true;
}

bool EffectAvcCompressor::Init()	// invoked before PromptUser
{
	if ( EffectSimplePairedTwoTrack<IAVCSAMPLETYPE,AVCCOMPSAMPLETYPE>::Init() == false )
		return false;

	return true;
}

bool EffectAvcCompressor::ProcessSimplePairedTwoTrack(/*IAVCSAMPLETYPE*/ void *bufferLeft,
													  /*IAVCSAMPLETYPE*/ void *bufferRight, // may be 0
													  sampleCount len)
{
	// build new iAVCBufferList node
	iAVCBufferList *  pBufferNode = new iAVCBufferList;
	if ( mpBufferPrevious != NULL )
		mpBufferPrevious->mpNext = pBufferNode;	// link to end of list
	else
		mpBufferList = pBufferNode;				// have first node in list
	mpBufferPrevious = pBufferNode;				// this node now the last added to list
	pBufferNode->mpNext = NULL;
	pBufferNode->mpLeftBuffer = bufferLeft;
	pBufferNode->mpRightBuffer = bufferRight;
	pBufferNode->mnLen = len;
	pBufferNode->mnNext = 0;

	// process samples in these buffer(s)
	IAVCSAMPLETYPE* typedBufferLeft  = (IAVCSAMPLETYPE*)bufferLeft;
	IAVCSAMPLETYPE* typedBufferRight = (IAVCSAMPLETYPE*)bufferRight;
	sampleCount i;
	IAVCSAMPLETYPE left;
	IAVCSAMPLETYPE right = 0;

	for ( i = 0 ; i < len ; ++i ) {
		left = typedBufferLeft[i];
		if ( typedBufferRight )
			right = typedBufferRight[i];
		#ifdef IAVC_INLINE
			// use inline SetNextSample()
			#define	IAVC_SETNEXTSAMPLE
			#include "../../lib-src/iAVC/iAVC.cpp"
			#undef  IAVC_SETNEXTSAMPLE
			// use inline GetNextSample()
			if ( mnDelay <= 0 )
			{	// get a value only if past desired delay
				#define IAVC_GETNEXTSAMPLE
				#include "../../lib-src/iAVC/iAVC.cpp"
				#undef  IAVC_SETNEXTSAMPLE
			}
		#else
			// call SetNextSample() and GetNextSample()
			mAutoVolCtrl.SetNextSample(left, right);
			if ( mnDelay <= 0 )
			{	// get a value only if past desired delay
				mAutoVolCtrl.GetNextSample(left, right);
			}
		#endif
			if ( mnDelay <= 0 )
			{	// get a value only if past desired delay
				OutputSample ( left, right );
				typedBufferLeft[i] = left;
				if ( typedBufferRight )
					typedBufferRight[i] = right;
			}
			else
			{	// count down the delay amount
				--mnDelay;
			}
	}
	return true;
}

void EffectAvcCompressor::End()
{
	IAVCSAMPLETYPE left;
	IAVCSAMPLETYPE right = 0;

	// We now need to output any samples still waiting because
	while ( mpBufferList != NULL )
	{
		#ifdef IAVC_INLINE
			// use inline GetNextSample()
			#define IAVC_GETNEXTSAMPLE
			#include "../../lib-src/iAVC/iAVC.cpp"
			#undef  IAVC_SETNEXTSAMPLE
		#else
			// call GetNextSample()
			mAutoVolCtrl.GetNextSample(left, right);
		#endif

		OutputSample ( left, right );
	}
	mpBufferPrevious = NULL;

	EffectSimplePairedTwoTrack<IAVCSAMPLETYPE,AVCCOMPSAMPLETYPE>::End();
}

// WDR: class implementations

//----------------------------------------------------------------------------
// AvcCompressorDialog
//----------------------------------------------------------------------------

#define PREF_ADJWIN "/Effect/AVC/user1/adjwin"
#define PREF_DELAY  "/Effect/AVC/user1/delay"
#define PREF_CHANGE "/Effect/AVC/user1/change"
#define PREF_MINPCT "/Effect/AVC/user1/minpct"
#define PREF_ENABLE "/Effect/AVC/user1/%d/enable"
#define PREF_HORIZ  "/Effect/AVC/user1/%d/horiz"
#define PREV_VERT   "/Effect/AVC/user1/%d/vert"


// WDR: event table for AvcCompressorDialog

BEGIN_EVENT_TABLE(AvcCompressorDialog,wxDialog)
   EVT_BUTTON( wxID_OK, AvcCompressorDialog::OnOK )
   EVT_BUTTON( wxID_CANCEL, AvcCompressorDialog::OnCancel )
   EVT_BUTTON( ID_RESTORE_DEFAULTS, AvcCompressorDialog::OnRestoreDefaults )
   EVT_CHECKBOX(ID_FIRST_CURVE_CHECK+1, AvcCompressorDialog::OnCheckBox)
   EVT_CHECKBOX(ID_FIRST_CURVE_CHECK+2, AvcCompressorDialog::OnCheckBox)
   EVT_CHECKBOX(ID_FIRST_CURVE_CHECK+3, AvcCompressorDialog::OnCheckBox)
   EVT_CHECKBOX(ID_FIRST_CURVE_CHECK+4, AvcCompressorDialog::OnCheckBox)
   EVT_CHECKBOX(ID_FIRST_CURVE_CHECK+5, AvcCompressorDialog::OnCheckBox)
END_EVENT_TABLE()

AvcCompressorDialog::AvcCompressorDialog(wxWindow *parent, wxWindowID id,
                           const wxString &title,
                           const wxPoint &position, const wxSize& size,
                           long style ) :
		wxDialog( parent, id, title, position, size, style ),
		mctlAdjWin ( 0 ),
		mctlDelay ( 0 ),
		mctlChangeWin ( 0 ),
		mctlMinPct ( 0 )
{
	for ( int i = 0 ; i < NUM_CURVE_POINTS ; ++i ) {
		mctlCheckBoxes[i] = 0;
		mctlXAxis[i] = 0;
		mctlYAxis[i] = 0;
	}

	MakeAvcCompressorDialog( this, TRUE );

	// First make sure all value initialized, especially horiz and vert first & last values
	wxCommandEvent event;
	OnRestoreDefaults(event);
	// Now read in from registry
	ReadPrefs();
}

AvcCompressorDialog::~AvcCompressorDialog()
{	// zero out pointers in case reference counts being used
	mctlAdjWin = 0;
	mctlDelay = 0;
	mctlChangeWin = 0;
	mctlMinPct = 0;

	for ( int i = 0 ; i < NUM_CURVE_POINTS ; ++i ) {
		mctlCheckBoxes[i] = 0;
		mctlXAxis[i] = 0;
		mctlYAxis[i] = 0;
	}
}

// figure out Y value for each possible X value
void AvcCompressorDialog::GetTransformArray( unsigned short int nTransform[MULTIPLY_PCT_ARRAY_SIZE] )
{
	long iCurPoint = 0;		// index to mnXAxis and mnYAxis
	long iPrevPoint= 0;		// index to mnXAxis and mnYAxis
	long iMultiply = 1;		// iMultiply and iDivide used to calculate fractional slopes
	long iDivide   = 1;
	long iBias     = 0;	    // keeps values from decreasing

	nTransform [ 0 ] = 0;

	for ( long i = 0 ; i < MULTIPLY_PCT_ARRAY_SIZE - 1 ; ++i ) {
		if ( i == mnXAxis [ iCurPoint ] && iCurPoint < NUM_CURVE_POINTS - 1 ) {
			// time to move to next point
			iPrevPoint = iCurPoint;
			// find next checked point
			while ( mctlCheckBoxes[++iCurPoint]->GetValue() == false)
				;	// last box guaranteed to be checked
			// Recalculate bias based on what would be calculated with old values and
			//		what would be calculated with new values.
			long iOld = i * iMultiply / iDivide + iBias;

			iMultiply = mnYAxis [ iCurPoint ] - mnYAxis [ iPrevPoint ];
			iDivide   = mnXAxis [ iCurPoint ] - mnXAxis [ iPrevPoint ];

			iBias = iOld - ( i * iMultiply / iDivide );
		}
		nTransform [ i ] = (unsigned short int)
         ( i * iMultiply / iDivide + iBias );
	}
	// set boundary case for loudest sound
	nTransform [ MULTIPLY_PCT_ARRAY_SIZE - 1 ] = MULTIPLY_PCT_ARRAY_SIZE - 1;
}

bool AvcCompressorDialog::LongRangeCheck ( wxWindow *window,
										   const long nValue,
										   const long nMin,
										   const long nMax )
{
	if ( nValue < nMin || nValue > nMax ) {
		// value out of range
        if ( !wxValidator::IsSilent() )
            wxBell();
		wxString strTemp;
		strTemp.Printf ( wxT("Value must be from %d to %d."), nMin, nMax );
		wxMessageBox(strTemp, wxT("Validation error"),
					 wxOK | wxICON_EXCLAMATION, GetParent() );
 		if ( window )
			window->SetFocus();
		return false;
	}
	return true;
}

// WDR: handler implementations for AvcCompressorDialog

void AvcCompressorDialog::OnOK(wxCommandEvent &event)
{
	if ( Validate() && TransferDataFromWindow() ) {
		// do our dialog specific validation

		// Check Adjustment Settings
        mstrAdjWin.ToLong(&mnAdjWin);
		if ( LongRangeCheck( mctlAdjWin, mnAdjWin, ADJWIN_MIN, ADJWIN_MAX ) == false ) {
			// value out of range
			return;
		}

        mstrDelay.ToLong(&mnDelay);
		if ( LongRangeCheck( mctlDelay, mnDelay, DELAY_MIN, DELAY_MAX ) == false ) {
			// value out of range
			return;
		}

        mstrChangeWin.ToLong(&mnChangeWin);
		if ( LongRangeCheck( mctlChangeWin, mnChangeWin, CHANGE_MIN, CHANGE_MAX ) == false ) {
			// value out of range
			return;
		}

		if ( mnChangeWin > mnAdjWin ) {
			wxMessageBox(wxT("Change window size must be less than or equal to Adjustment window size."),
					 wxT("Validation error"), wxOK | wxICON_EXCLAMATION, GetParent() );
 			if ( mctlChangeWin )
				mctlChangeWin->SetFocus();
			return;
		}

        mstrMinPct.ToLong(&mnMinPct);
		if ( LongRangeCheck( mctlMinPct, mnMinPct, MINPCT_MIN, MINPCT_MAX ) == false ) {
			// value out of range
			return;
		}

		// Check Amplification Settings
		long iPrevPoint= 0;		// index to mnXAxis and mnYAxis
		for ( int i = 0 ; i < NUM_CURVE_POINTS ; ++i ) {
			mstrXAxis[i].ToLong(&mnXAxis[i]);
			mstrYAxis[i].ToLong(&mnYAxis[i]);

			// see if this is a checked point
			if ( mctlCheckBoxes[i]->GetValue() == false)
				continue;	// last box guaranteed to be checked

			if ( i > 0 ) {
				if ( mnXAxis[i] <= mnXAxis[iPrevPoint] ) {
					wxMessageBox(wxT("Values in columns must be in ascending order."),
								 wxT("Validation error"),
								 wxOK | wxICON_EXCLAMATION, GetParent() );
					mctlXAxis[(i==NUM_CURVE_POINTS-1) ? iPrevPoint : i]->SetFocus();
					return;
				}
				if ( mnYAxis[i] <= mnYAxis[iPrevPoint] ) {
					wxMessageBox(wxT("Values in columns must be in ascending order."),
								 wxT("Validation error"),
								 wxOK | wxICON_EXCLAMATION, GetParent() );
					mctlYAxis[(i==NUM_CURVE_POINTS-1) ? iPrevPoint : i]->SetFocus();
					return;
				}
			}
			iPrevPoint = i;
		}

		// AOK, time to return
		WritePrefs();			// save values user entered for next execution

        if ( IsModal() )
            EndModal(wxID_OK);
        else {
		    SetReturnCode(wxID_OK);
		    this->Show(FALSE);
        }
	}
}

void AvcCompressorDialog::OnCancel(wxCommandEvent &event)
{
   EndModal(false);
}

	static int* naSampleChoicesHoriz[5] = {iHoriz_1K_1K,iHoriz_1K_3HK,iHoriz_E75_5K,iHoriz_75_3500,iHoriz_AE75_3HK};
	static int* naSampleChoicesVert[5]  = {iVert_1K_1K, iVert_1K_3HK, iVert_E75_5K, iVert_75_3500, iVert_AE75_3HK};

void AvcCompressorDialog::OnRestoreDefaults(wxCommandEvent &event)
{
	static int* naSampleChoicesHoriz[5] = {iHoriz_1K_1K,iHoriz_1K_3HK,iHoriz_E75_5K,iHoriz_75_3500,iHoriz_AE75_3HK};
	static int* naSampleChoicesVert[5]  = {iVert_1K_1K, iVert_1K_3HK, iVert_E75_5K, iVert_75_3500, iVert_AE75_3HK};
	mstrAdjWin.Printf("%d", ADJWIN_DEFAULT);
	mstrDelay.Printf("%d", DELAY_DEFAULT);
	mstrChangeWin.Printf("%d", CHANGE_DEFAULT);
	mstrMinPct.Printf("%d", MINPCT_DEFAULT);

	for ( int i = 0 ; i < NUM_CURVE_POINTS ; ++ i ) {
		mctlCheckBoxes[i]->SetValue(true);
		mctlXAxis[i]->Show ( true );
		mctlYAxis[i]->Show ( true );
		mstrXAxis[i].Printf( "%d", naSampleChoicesHoriz[4][i] );
		mstrYAxis[i].Printf( "%d", naSampleChoicesVert[4][i] );
	}
	TransferDataToWindow();
}

void AvcCompressorDialog::ReadPrefs()
{
	int			nTemp;
	bool		bTemp;
	wxString	strTemp;

	nTemp = gPrefs->Read ( PREF_ADJWIN, ADJWIN_DEFAULT );
	mstrAdjWin.Printf("%d", nTemp);
	nTemp = gPrefs->Read ( PREF_DELAY, DELAY_DEFAULT );
	mstrDelay.Printf("%d", nTemp);
	nTemp = gPrefs->Read ( PREF_CHANGE, CHANGE_DEFAULT );
	mstrChangeWin.Printf("%d", nTemp);
	nTemp = gPrefs->Read ( PREF_MINPCT, MINPCT_DEFAULT );
	mstrMinPct.Printf("%d", nTemp);

	for ( int i = 1 ; i < NUM_CURVE_POINTS - 1 ; ++ i ) {
		strTemp.Printf(PREF_ENABLE,i);
		bTemp = ( gPrefs->Read ( strTemp, true ) == 0 ) ? false : true;
		mctlCheckBoxes[i]->SetValue(bTemp);
		mctlXAxis[i]->Show ( bTemp );
		mctlYAxis[i]->Show ( bTemp );

		strTemp.Printf(PREF_HORIZ,i);
		nTemp = gPrefs->Read ( strTemp, naSampleChoicesHoriz[4][i] );
		mstrXAxis[i].Printf( "%d", nTemp );
		strTemp.Printf(PREV_VERT,i);
		nTemp = gPrefs->Read ( strTemp, naSampleChoicesVert[4][i] );
		mstrYAxis[i].Printf( "%d", nTemp );
	}
	TransferDataToWindow();
}

void AvcCompressorDialog::WritePrefs()
{
	wxString	strTemp;

	gPrefs->Write ( PREF_ADJWIN, mnAdjWin );
	gPrefs->Write ( PREF_DELAY, mnDelay );
	gPrefs->Write ( PREF_CHANGE, mnChangeWin );
	gPrefs->Write ( PREF_MINPCT, mnMinPct );

	for ( int i = 1 ; i < NUM_CURVE_POINTS - 1 ; ++ i ) {
		strTemp.Printf(PREF_ENABLE,i);
		gPrefs->Write ( strTemp, mctlCheckBoxes[i]->GetValue() );

		strTemp.Printf(PREF_HORIZ,i);
		gPrefs->Write ( strTemp, mnXAxis[i] );
		strTemp.Printf(PREV_VERT,i);
		gPrefs->Write ( strTemp, mnYAxis[i] );
	}
}

void AvcCompressorDialog::OnCheckBox(wxCommandEvent & event)
{
	bool bCheck = mctlCheckBoxes[event.m_id-ID_FIRST_CURVE_CHECK]->GetValue();
    mctlXAxis[event.m_id-ID_FIRST_CURVE_CHECK]->Show ( bCheck );
    mctlYAxis[event.m_id-ID_FIRST_CURVE_CHECK]->Show ( bCheck );
}

wxSizer *AvcCompressorDialog::MakeAvcCompressorDialog(wxWindow * parent, bool call_fit,
														bool set_sizer)
{
	wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
	wxStaticBoxSizer *group;
	wxBoxSizer *boxSizer;
	wxStaticText *staticText;
	//wxTextCtrl *textCtrl;
	wxButton *button;
	wxFlexGridSizer *flexGridSizer;

	staticText =
       new wxStaticText(parent, ID_TEXT,
                        wxT("Automatic Volume Control by Vincent A. Busam"),
                        wxDefaultPosition, wxDefaultSize, 0);
	mainSizer->Add(staticText, 0, wxALIGN_CENTRE | wxALL, 5);

	// 0.  Box Sizer for horizontal components
	wxBoxSizer *horizontalSizer = new wxBoxSizer(wxHORIZONTAL);

	// 1.  Box Sizer for leftmost group of controls
	wxBoxSizer *leftSizer = new wxBoxSizer(wxVERTICAL);

	// 1.1  Group Box for adjustment window settings

	group = new wxStaticBoxSizer(new wxStaticBox(parent, -1,
                                                wxT("Adjustment Settings")), wxVERTICAL);
	flexGridSizer = new wxFlexGridSizer(2, 0, 0);

	// 1.1.1  Adjustment Window
	staticText =
       new wxStaticText(parent, ID_TEXT, wxT("Adjustment Window:"),
                        wxDefaultPosition, wxDefaultSize, 0 );
	flexGridSizer->Add(staticText, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

	mctlAdjWin =
       new wxTextCtrl(parent, ID_ADJWINTEXT, "", wxDefaultPosition,
                      wxSize(40, -1), 0,
					  wxTextValidator(wxFILTER_NUMERIC, &mstrAdjWin ) );
	flexGridSizer->Add(mctlAdjWin, 0, wxALIGN_CENTRE | wxALL, 5);

	// 1.1.2  Adjustment Delay
	staticText =
       new wxStaticText(parent, ID_TEXT, wxT("Adjustment Delay:"),
                        wxDefaultPosition, wxDefaultSize, 0);
	flexGridSizer->Add(staticText, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

	mctlDelay =
       new wxTextCtrl(parent, ID_DELAYTEXT, "", wxDefaultPosition,
                      wxSize(40, -1), 0,
					  wxTextValidator(wxFILTER_NUMERIC, &mstrDelay ));
	flexGridSizer->Add(mctlDelay, 0, wxALIGN_CENTRE | wxALL, 5);

	// 1.1.3  Min Change Window
   /* i18n-hint: the minimum size of the window that is changed */
	staticText =
       new wxStaticText(parent, ID_TEXT, wxT("Minimum Change Window:"),
                        wxDefaultPosition, wxDefaultSize, 0);
	flexGridSizer->Add(staticText, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

	mctlChangeWin =
       new wxTextCtrl(parent, ID_CHANGEWINTEXT, "", wxDefaultPosition,
                      wxSize(40, -1), 0,
					  wxTextValidator(wxFILTER_NUMERIC, &mstrChangeWin ));
	flexGridSizer->Add(mctlChangeWin, 0, wxALIGN_CENTRE | wxALL, 5);

	// 1.1.4  Min Change %
	staticText =
       new wxStaticText(parent, ID_TEXT, wxT("Minimum Change %:"),
                        wxDefaultPosition, wxDefaultSize, 0);
	flexGridSizer->Add(staticText, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

	mctlMinPct =
       new wxTextCtrl(parent, ID_MINPCTTEXT, "", wxDefaultPosition,
                      wxSize(40, -1), 0,
					  wxTextValidator(wxFILTER_NUMERIC, &mstrMinPct ));
	 flexGridSizer->Add(mctlMinPct, 0, wxALIGN_CENTRE | wxALL, 5);

	// 1.1.end  Add group
	group->Add( flexGridSizer, 0,  wxALIGN_CENTRE|wxALL, 5 );

	leftSizer->Add( group, 0, wxALIGN_TOP |wxALL, 5 );

   // DMM: the following text box is too difficult to translate as-is.  Taken out of
   // i18n for now.

	// 1.2 area under group box
	staticText =
       new wxStaticText(parent, ID_TEXT,
                        ("Above values are in samples.\n"
						  "Adjustment Window defines number of \nsamples in moving average.\n"
						  "Change window defines minimum time \nbetween volume changes.\n"
						  "Minimum % change of volume adjustment \nbefore making a volume change.\n"
						  "Grid at right determines how much to amplify \neach volume level.\n"
						  "For more information see: \n"
						  "http://www.busam.com/skyland/iavc\n"
						  "7/21/02: WAV and MP3 files both work."
						 ),
                        wxDefaultPosition, wxDefaultSize, 0);
	leftSizer->Add(staticText, 0, wxALIGN_CENTRE | wxALL, 5);

	// 1.end
	horizontalSizer->Add( leftSizer, 0, wxALIGN_TOP |wxALL, 5 );

	// 2.  Group Box for volume settings

	group = new wxStaticBoxSizer(new wxStaticBox(parent, -1,
                                                wxT("Amplification Settings")), wxVERTICAL);

	// 2.1  Add one row each time through loop

	flexGridSizer = new wxFlexGridSizer(3, 0, 0);

	staticText =
       new wxStaticText(parent, ID_TEXT, _("Enabled"),
                        wxDefaultPosition, wxDefaultSize, 0);
	flexGridSizer->Add(staticText, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

	staticText =
       new wxStaticText(parent, ID_TEXT, wxT("Original Value"),
                        wxDefaultPosition, wxDefaultSize, 0);
	flexGridSizer->Add(staticText, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

	staticText =
       new wxStaticText(parent, ID_TEXT, wxT("New Value"),
                        wxDefaultPosition, wxDefaultSize, 0);
	flexGridSizer->Add(staticText, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

	for ( int i = 0 ; i < NUM_CURVE_POINTS ; ++i )
	{
		mctlCheckBoxes[i] =
		   new wxCheckBox(parent, ID_FIRST_CURVE_CHECK + i, "", wxDefaultPosition,
						  wxSize(40, -1), 0);
		flexGridSizer->Add(mctlCheckBoxes[i], 0, wxALIGN_CENTRE | wxALL, 5);
		mctlCheckBoxes[i]->SetValue(true);

		mctlXAxis[i] =
		   new wxTextCtrl(parent, ID_FIRST_CURVE_X + i, "", wxDefaultPosition,
						  wxSize(40, -1), 0,
						  wxTextValidator(wxFILTER_NUMERIC, &mstrXAxis[i] ));
		flexGridSizer->Add(mctlXAxis[i], 0, wxALIGN_CENTRE | wxALL, 5);

		mctlYAxis[i] =
		   new wxTextCtrl(parent, ID_FIRST_CURVE_Y + i, "", wxDefaultPosition,
						  wxSize(40, -1), 0,
						  wxTextValidator(wxFILTER_NUMERIC, &mstrYAxis[i] ));
		flexGridSizer->Add(mctlYAxis[i], 0, wxALIGN_CENTRE | wxALL, 5);
	}
	mctlCheckBoxes[0]->Enable(false);
	mctlXAxis[0]->Enable(false);
	mctlYAxis[0]->Enable(false);
	mctlCheckBoxes[NUM_CURVE_POINTS-1]->Enable(false);
	mctlXAxis[NUM_CURVE_POINTS-1]->Enable(false);
	mctlYAxis[NUM_CURVE_POINTS-1]->Enable(false);

	// 2.end  Add group
	group->Add( flexGridSizer, 0,  wxALIGN_CENTRE|wxALL, 5 );
	horizontalSizer->Add( group, 0, wxALIGN_CENTRE|wxALL, 5 );

	mainSizer->Add( horizontalSizer, 0,  wxALIGN_CENTRE|wxALL, 5 );

	// Last:  Add buttons
	boxSizer = new wxBoxSizer(wxHORIZONTAL);

	// Add restore defaults button
	button =
       new wxButton(parent, ID_RESTORE_DEFAULTS, wxT("Restore Defaults"), wxDefaultPosition,
                    wxDefaultSize, 0);
	boxSizer->Add(button, 0, wxALIGN_CENTRE | wxALL, 5);

	// Add Cancel button
	button =
       new wxButton(parent, wxID_CANCEL, _("&Cancel"), wxDefaultPosition,
                    wxDefaultSize, 0);
	boxSizer->Add(button, 0, wxALIGN_CENTRE | wxALL, 5);

	// Add OK button
	button =
       new wxButton(parent, wxID_OK, _("OK"), wxDefaultPosition,
                    wxDefaultSize, 0);
	button->SetDefault();
	boxSizer->Add(button, 0, wxALIGN_CENTRE | wxALL, 5);

	mainSizer->Add(boxSizer, 0, wxALIGN_CENTRE | wxALL, 5);

	if (set_sizer) {
		parent->SetAutoLayout(TRUE);
		parent->SetSizer(mainSizer);
		if (call_fit) {
			mainSizer->Fit(parent);
			mainSizer->SetSizeHints(parent);
		}
	}

	return mainSizer;
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
// arch-tag: 129a8b19-9efd-4b47-95cb-c5534840a0f0

