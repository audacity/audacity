/**********************************************************************

  Audacity: A Digital Audio Editor

  Effect/ScienFilter.cpp

  Norm C
  Mitch Golden
  Vaughan Johnson (Preview)

*******************************************************************//**

\file ScienFilter.cpp
\brief Implements EffectScienFilter, ScienFilterDialog,
ScienFilterPanel.

*//****************************************************************//**

\class EffectScienFilter
\brief An Effect.

  Performs IIR filtering that emulates analog filters, specifically
  Butterworth, Chebyshev Type I and Type II. Highpass and lowpass filters
  are supported, as are filter orders from 1 to 10.

  The filter is applied using biquads

*//****************************************************************//**

\class ScienFilterDialog
\brief Dialog used with EffectScienFilter

*//****************************************************************//**

\class ScienFilterPanel
\brief ScienFilterPanel is used with ScienFilterDialog and controls
a graph for EffectScienFilter.

*//*******************************************************************/

#include "../Audacity.h"
#include "ScienFilter.h"
#include "Equalization.h" // For SliderAx
#include "../AColor.h"
#include "../ShuttleGui.h"
#include "../PlatformCompatibility.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../WaveTrack.h"
#include "../widgets/Ruler.h"
#include "../Theme.h"
#include "../AllThemeResources.h"
#include "../WaveTrack.h"
#include "float_cast.h"

#include <wx/bitmap.h>
#include <wx/msgdlg.h>
#include <wx/brush.h>
#include <wx/dcmemory.h>
#include <wx/event.h>
#include <wx/image.h>
#include <wx/intl.h>
#include <wx/stattext.h>
#include <wx/string.h>
#include <wx/textdlg.h>
#include <wx/stdpaths.h>
#include <wx/settings.h>

#if wxUSE_TOOLTIPS
#include <wx/tooltip.h>
#endif
#include <wx/utils.h>

#include <math.h>

#include <wx/arrimpl.cpp>

#define PI 3.1415926535
#define square(a) ((a)*(a))

#ifndef __min
  #define __min(a,b) ((a) < (b) ? (a) : (b))
#endif
#ifndef __max
  #define __max(a,b) ((a) > (b) ? (a) : (b))
#endif

// Local functions

void EffectScienFilter::ReadPrefs()
{
	double dTemp;
   gPrefs->Read(wxT("/SciFilter/Order"), &mOrder, 1);
   mOrder = __max (1, mOrder);
   mOrder = __min (MAX_FILTER_ORDER, mOrder);
   gPrefs->Read(wxT("/SciFilter/FilterType"), &mFilterType, 0);
   mFilterType = __max (0, mFilterType);
   mFilterType = __min (2, mFilterType);
   gPrefs->Read(wxT("/SciFilter/FilterSubtype"), &mFilterSubtype, 0);
   mFilterSubtype = __max (0, mFilterSubtype);
   mFilterSubtype = __min (1, mFilterSubtype);
   gPrefs->Read(wxT("/SciFilter/Cutoff"), &dTemp, 1000.0);
   mCutoff = (float)dTemp;
   mCutoff = __max (1, mCutoff);
   mCutoff = __min (100000, mCutoff);
   gPrefs->Read(wxT("/SciFilter/Ripple"), &dTemp, 1.0);
   mRipple = dTemp;
   mRipple = __max (0, mRipple);
   mRipple = __min (100, mRipple);
   gPrefs->Read(wxT("/SciFilter/StopbandRipple"), &dTemp, 30.0);
   mStopbandRipple = dTemp;
   mStopbandRipple = __max (0, mStopbandRipple);
   mStopbandRipple = __min (100, mStopbandRipple);
}

EffectScienFilter::EffectScienFilter()
{
   ReadPrefs();
   mPrompting = false;
}


EffectScienFilter::~EffectScienFilter()
{
}

bool EffectScienFilter::Init()
{
   int selcount = 0;
   double rate = 0.0;
   TrackListIterator iter(GetActiveProject()->GetTracks());
   Track *t = iter.First();
   while (t) {
      if (t->GetSelected() && t->GetKind() == Track::Wave) {
         WaveTrack *track = (WaveTrack *)t;
         if (selcount==0) {
            rate = track->GetRate();
         }
         else {
            if (track->GetRate() != rate) {
               wxMessageBox(_("To apply a filter, all selected tracks must have the same sample rate."));
               return(false);
            }
         }
         selcount++;
      }
      t = iter.Next();
   }
   return(true);}

bool EffectScienFilter::PromptUser()
{
   // Detect whether we are editing a batch chain by checking the parent window
   mEditingBatchParams = (mParent != GetActiveProject());
   if (!mEditingBatchParams)
   {
      ReadPrefs();
   }

   TrackListOfKindIterator iter(Track::Wave, mTracks);
   WaveTrack *t = (WaveTrack *) iter.First();
   float hiFreq;
   if (t)
      hiFreq = ((float)(t->GetRate())/2.);
   else
      hiFreq = ((float)(GetActiveProject()->GetRate())/2.);

   ScienFilterDialog dlog(this, ((double)loFreqI), hiFreq, mParent, -1, _("Classic Filter"));

   dlog.dBMin = mdBMin;
   dlog.dBMax = mdBMax;
   dlog.Order = mOrder;
   dlog.Cutoff = mCutoff;
   dlog.FilterType = mFilterType;
   dlog.FilterSubtype = mFilterSubtype;
   dlog.Ripple = mRipple;
   dlog.StopbandRipple = mStopbandRipple;

   dlog.CentreOnParent();

   mPrompting = true;   // true when previewing, false in batch
   dlog.ShowModal();
   mPrompting = false;

   if (!dlog.GetReturnCode())
      return false;

   mdBMin = dlog.dBMin;
   mdBMax = dlog.dBMax;
   mOrder = dlog.Order;
   mCutoff = dlog.Cutoff;
   mFilterType = dlog.FilterType;
   mFilterSubtype = dlog.FilterSubtype;
   mRipple = dlog.Ripple;
   mStopbandRipple = dlog.StopbandRipple;

   if (!mEditingBatchParams)
   {
      // Save preferences
      gPrefs->Write(wxT("/SciFilter/Order"), mOrder);
      gPrefs->Write(wxT("/SciFilter/FilterType"), mFilterType);
      gPrefs->Write(wxT("/SciFilter/FilterSubtype"), mFilterSubtype);
      gPrefs->Write(wxT("/SciFilter/Cutoff"), mCutoff);
      gPrefs->Write(wxT("/SciFilter/Ripple"), mRipple);
      gPrefs->Write(wxT("/SciFilter/StopbandRipple"), mStopbandRipple);
      gPrefs->Flush();
   }

   return true;
}

bool EffectScienFilter::DontPromptUser()
{
   TrackListOfKindIterator iter(Track::Wave, mTracks);
   WaveTrack *t = (WaveTrack *) iter.First();
   float hiFreq;
   if (t)
      hiFreq = ((float)(t->GetRate())/2.);
   else
      hiFreq = ((float)(GetActiveProject()->GetRate())/2.);
   /*i18n-hint: The 'Classic  Filter' is an audio effect.  It's a low-pass or high-pass 
   filter with specfic characteristics. */
   ScienFilterDialog dlog(this, ((double)loFreqI), hiFreq, NULL, -1, _("Classic Filter"));
   dlog.dBMin = mdBMin;
   dlog.dBMax = mdBMax;
   dlog.Order = mOrder;
   dlog.Cutoff = mCutoff;
   dlog.FilterType = mFilterType;
   dlog.FilterSubtype = mFilterSubtype;
   dlog.Ripple = mRipple;

   dlog.CalcFilter(this);

   return true;
}

bool EffectScienFilter::TransferParameters( Shuttle & shuttle )
{
   // if shuttle.mbStoreInClient is true, read prefs ScienFilter/FilterType (etc.) string and put into mFilterType (etc.)
   // else put mFilterType (etc.) into string form and write prefs
   shuttle.TransferInt(wxT("FilterType"),mFilterType,0);    
   shuttle.TransferInt(wxT("FilterSubtype"),mFilterSubtype,0); // etc.
   shuttle.TransferInt(wxT("Order"),mOrder,2);
   shuttle.TransferFloat(wxT("Cutoff"),mCutoff,1000);
   shuttle.TransferFloat(wxT("PassbandRipple"),mRipple,1);
   shuttle.TransferFloat(wxT("StopbandRipple"),mStopbandRipple,30);

   if(!mPrompting)
      DontPromptUser();   // not previewing, ie batch mode or initial setup
   return true;
}

bool EffectScienFilter::Process()
{
   this->CopyInputTracks(); // Set up mOutputTracks.
   bool bGoodResult = true;

   SelectedTrackListOfKindIterator iter(Track::Wave, mOutputTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   int count = 0;
   while (track)
   {
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double t0 = mT0 < trackStart? trackStart: mT0;
      double t1 = mT1 > trackEnd? trackEnd: mT1;

      if (t1 > t0) {
         sampleCount start = track->TimeToLongSamples(t0);
         sampleCount end = track->TimeToLongSamples(t1);
         sampleCount len = (sampleCount)(end - start);

         if (!ProcessOne(count, track, start, len))
         {
            bGoodResult = false;
            break;
         }
      }

      track = (WaveTrack *) iter.Next();
      count++;
   }

   this->ReplaceProcessedTracks(bGoodResult); 
   return bGoodResult;
}


bool EffectScienFilter::ProcessOne(int count, WaveTrack * t,
                                   sampleCount start, sampleCount len)
{
   // Create a new WaveTrack to hold all of the output
   AudacityProject *p = GetActiveProject();
   WaveTrack *output = p->GetTrackFactory()->NewWaveTrack(floatSample, t->GetRate());

   sampleCount s = start;
   sampleCount idealBlockLen = t->GetMaxBlockSize();
   float *buffer = new float[idealBlockLen];
   sampleCount originalLen = len;

   TrackProgress(count, 0.0);
   bool bLoopSuccess = true;

   for (int iPair = 0; iPair < (mOrder+1)/2; iPair++)
      mpBiquad [iPair]->fPrevIn = mpBiquad [iPair]->fPrevPrevIn = mpBiquad [iPair]->fPrevOut = mpBiquad [iPair]->fPrevPrevOut = 0;

   while(len)
   {
      sampleCount block = idealBlockLen;
      if (block > len)
         block = len;

      t->Get((samplePtr)buffer, floatSample, s, block);

      for (int iPair = 0; iPair < (mOrder+1)/2; iPair++)
      {
         mpBiquad[iPair]->pfIn = buffer;
         mpBiquad[iPair]->pfOut = buffer;
         Biquad_Process (mpBiquad[iPair], block);
      }
      output->Append ((samplePtr)buffer, floatSample, block);
      len -= block;
      s += block;

      if (TrackProgress (count, (s-start)/(double)originalLen))
      {
         bLoopSuccess = false;
         break;
      }
   }
   if (bLoopSuccess)
   {
      output->Flush();
      // Now move the appropriate bit of the output back to the track
      float *bigBuffer = new float[originalLen];
      output->Get((samplePtr)bigBuffer, floatSample, 0, originalLen);
      t->Set((samplePtr)bigBuffer, floatSample, start, originalLen);
      delete[] bigBuffer;
   }

   delete[] buffer;
   delete output;

   return bLoopSuccess;
}

void EffectScienFilter::Filter(sampleCount WXUNUSED(len),
         float *WXUNUSED(buffer))
{
}


//----------------------------------------------------------------------------
// ScienFilterPanel
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(ScienFilterPanel, wxPanel)
    EVT_PAINT(ScienFilterPanel::OnPaint)
    EVT_SIZE(ScienFilterPanel::OnSize)
END_EVENT_TABLE()

ScienFilterPanel::ScienFilterPanel( double loFreq, double hiFreq,
                     ScienFilterDialog *parent,
                     wxWindowID id, const wxPoint& pos, const wxSize& size):
   wxPanel(parent, id, pos, size)
{
   mBitmap = NULL;
   mWidth = 0;
   mHeight = 0;
   mLoFreq = loFreq;
   mHiFreq = hiFreq;
   mParent = parent;
}

ScienFilterPanel::~ScienFilterPanel()
{
   if (mBitmap)
      delete mBitmap;
}

void ScienFilterPanel::OnSize(wxSizeEvent & WXUNUSED(evt))
{
   Refresh( false );
}

void ScienFilterPanel::OnPaint(wxPaintEvent & WXUNUSED(evt))
{
   wxPaintDC dc(this);
   int width, height;
   GetSize(&width, &height);

   if (!mBitmap || mWidth!=width || mHeight!=height)
   {
      if (mBitmap)
         delete mBitmap;

      mWidth = width;
      mHeight = height;
      mBitmap = new wxBitmap(mWidth, mHeight);
   }

   wxBrush bkgndBrush(wxSystemSettings::GetColour(wxSYS_COLOUR_3DFACE));

   wxMemoryDC memDC;
   memDC.SelectObject(*mBitmap);

   wxRect bkgndRect;
   bkgndRect.x = 0;
   bkgndRect.y = 0;
   bkgndRect.width = mWidth;
   bkgndRect.height = mHeight;
   memDC.SetBrush(bkgndBrush);
   memDC.SetPen(*wxTRANSPARENT_PEN);
   memDC.DrawRectangle(bkgndRect);

   bkgndRect.y = mHeight;
   memDC.DrawRectangle(bkgndRect);

   wxRect border;
   border.x = 0;
   border.y = 0;
   border.width = mWidth;
   border.height = mHeight;

   memDC.SetBrush(*wxWHITE_BRUSH);
   memDC.SetPen(*wxBLACK_PEN);
   memDC.DrawRectangle(border);

   mEnvRect = border;
   mEnvRect.Deflate(2, 2);

   // Pure blue x-axis line
   memDC.SetPen(wxPen(theTheme.Colour( clrGraphLines ), 1, wxSOLID));
   int center = (int) (mEnvRect.height * dBMax/(dBMax-dBMin) + .5);
   AColor::Line(memDC,
                mEnvRect.GetLeft(), mEnvRect.y + center,
                mEnvRect.GetRight(), mEnvRect.y + center);

   //Now draw the actual response that you will get.
   //mFilterFunc has a linear scale, window has a log one so we have to fiddle about
   memDC.SetPen(wxPen(theTheme.Colour( clrResponseLines ), 3, wxSOLID));
   double scale = (double)mEnvRect.height/(dBMax-dBMin);    // pixels per dB
   double yF;                                               // gain at this freq

   double loLog = log10(mLoFreq);
   double step = log10(mHiFreq) - loLog;
   step /= ((double)mEnvRect.width-1.);
   double freq;                                    // actual freq corresponding to x position
   int x, y, xlast = 0, ylast = 0;
   for(int i=0; i<mEnvRect.width; i++)
   {
      x = mEnvRect.x + i;
      freq = pow(10., loLog + i*step);             //Hz
      yF = mParent->FilterMagnAtFreq (freq);
      yF = 20*log10(yF);

      if(yF < dBMin)
         yF = dBMin;
      yF = center-scale*yF;
      if(yF>mEnvRect.height)
         yF = mEnvRect.height - 1;
      if(yF<0.)
         yF=0.;
      y = (int)(yF+.5);

      if (i != 0 && (y < mEnvRect.height-1 || ylast < mEnvRect.y + mEnvRect.height-1))
      {
         AColor::Line (memDC, xlast, ylast, x, mEnvRect.y + y);
      }
      xlast = x;
      ylast = mEnvRect.y + y;
   }

   memDC.SetPen(*wxBLACK_PEN);
   mParent->freqRuler->ruler.DrawGrid(memDC, mEnvRect.height+2, true, true, 0, 1);
   mParent->dBRuler->ruler.DrawGrid(memDC, mEnvRect.width+2, true, true, 1, 2);

   dc.Blit(0, 0, mWidth, mHeight,
      &memDC, 0, 0, wxCOPY, FALSE);
}


// WDR: class implementations

//----------------------------------------------------------------------------
// ScienFilterDialog
//----------------------------------------------------------------------------

// WDR: event table for ScienFilterDialog

BEGIN_EVENT_TABLE(ScienFilterDialog,wxDialog)
   EVT_SIZE( ScienFilterDialog::OnSize )
   EVT_PAINT( ScienFilterDialog::OnPaint )
   EVT_ERASE_BACKGROUND( ScienFilterDialog::OnErase )

   EVT_SLIDER( ID_DBMAX, ScienFilterDialog::OnSliderDBMAX )
   EVT_SLIDER( ID_DBMIN, ScienFilterDialog::OnSliderDBMIN )
   EVT_CHOICE( ID_FILTER_ORDER, ScienFilterDialog::OnOrder)
   EVT_CHOICE( ID_FILTER_TYPE, ScienFilterDialog::OnFilterType)
   EVT_CHOICE( ID_FILTER_SUBTYPE, ScienFilterDialog::OnFilterSubtype)
   EVT_TEXT( ID_CUTOFF, ScienFilterDialog::OnCutoff)
   EVT_TEXT( ID_RIPPLE, ScienFilterDialog::OnRipple)
   EVT_TEXT( ID_STOPBAND_RIPPLE, ScienFilterDialog::OnStopbandRipple)

   EVT_BUTTON( ID_EFFECT_PREVIEW, ScienFilterDialog::OnPreview )
   EVT_BUTTON( wxID_OK, ScienFilterDialog::OnOk )
   EVT_BUTTON( wxID_CANCEL, ScienFilterDialog::OnCancel )
END_EVENT_TABLE()

ScienFilterDialog::ScienFilterDialog(EffectScienFilter * effect,
                     double loFreq, double hiFreq,
                     wxWindow *parent, wxWindowID id,
                     const wxString &title,
                     const wxPoint &position,
                     const wxSize& size,
                     long style):
   wxDialog( parent, id, title, position, size, style | wxRESIZE_BORDER | wxMAXIMIZE_BOX )
{
   m_pEffect = effect;

   dBMin = -30.;
   dBMax = 30;

#if wxUSE_TOOLTIPS
   wxToolTip::Enable(true);
#endif

   mLoFreq = loFreq;
   mNyquist = hiFreq;

   memset (effect->mpBiquad, 0, sizeof(effect->mpBiquad));
   for (int i = 0; i < MAX_FILTER_ORDER/2; i++)
   {
      effect->mpBiquad[i] = (BiquadStruct*)calloc (sizeof (BiquadStruct), 1);
      effect->mpBiquad[i]->fNumerCoeffs [0] = 1.0;	// straight-through
   }

   // Create the dialog
   MakeScienFilterDialog();
}

ScienFilterDialog::~ScienFilterDialog()
{
}

//
// Create the ScienFilter dialog
//
void ScienFilterDialog::MakeScienFilterDialog()
{
   wxStaticText *st;
   wxSizerFlags flagslabel;
   wxSizerFlags flagsunits;

   mCutoffCtl = NULL;
   mRippleCtl = NULL;
   mStopbandRippleCtl = NULL;

   // TODO: This code would be more readable if using ShuttleGUI.
   // Some updates to ShuttleGui would help this.

   // Create the base sizer
   szrV = new wxBoxSizer( wxVERTICAL );

   // -------------------------------------------------------------------
   // ROW 1: Freq response panel and sliders for vertical scale
   // -------------------------------------------------------------------
   szr1 = new wxFlexGridSizer( 3, 0, 0 );
   szr1->AddGrowableCol( 2, 1 );
   szr1->AddGrowableRow( 0, 1 );
   szr1->SetFlexibleDirection( wxBOTH );

   szr2 = new wxBoxSizer( wxVERTICAL );
   dBMaxSlider = new wxSlider(this, ID_DBMAX, 10, 0, 20,
                           wxDefaultPosition, wxDefaultSize, wxSL_VERTICAL|wxSL_INVERSE);
   szr2->Add( dBMaxSlider, 1, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, 4 );
   dBMinSlider = new wxSlider(this, ID_DBMIN, -10, -120, -10,
                           wxDefaultPosition, wxDefaultSize, wxSL_VERTICAL|wxSL_INVERSE);
   szr2->Add( dBMinSlider, 1, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, 4 );
   szr1->Add( szr2, 0, wxEXPAND|wxALIGN_CENTRE|wxALL, 4 );

#if wxUSE_ACCESSIBILITY
   dBMaxSlider->SetName(_("Max dB"));
   dBMaxSlider->SetAccessible(new SliderAx(dBMaxSlider, wxString(wxT("%d ")) + _("dB")));
   dBMinSlider->SetName(_("Min dB"));
   dBMinSlider->SetAccessible(new SliderAx(dBMinSlider, wxString(wxT("%d ")) + _("dB")));
#endif

   dBRuler = new RulerPanel(this, wxID_ANY);
   dBRuler->ruler.SetBounds(0, 0, 100, 100); // Ruler can't handle small sizes
   dBRuler->ruler.SetOrientation(wxVERTICAL);
   dBRuler->ruler.SetRange(30.0, -120.0);
   dBRuler->ruler.SetFormat(Ruler::LinearDBFormat);
   dBRuler->ruler.SetUnits(_("dB"));
   dBRuler->ruler.SetLabelEdges(true);
   int w, h;
   dBRuler->ruler.GetMaxSize(&w, NULL);
   dBRuler->SetSize(wxSize(w, 150));  // height needed for wxGTK

   szr4 = new wxBoxSizer( wxVERTICAL );
   szr4->AddSpacer(2); // vertical space for panel border and thickness of line
   szr4->Add( dBRuler, 1, wxEXPAND|wxALIGN_LEFT|wxALL );
   szr4->AddSpacer(1); // vertical space for thickness of line
   szr1->Add( szr4, 0, wxEXPAND|wxALIGN_LEFT|wxALL );

   wxSize size;
   size.Set (400, 200);
   mPanel = new ScienFilterPanel( mLoFreq, mNyquist,
                                   this,
                                   ID_FILTERPANEL, wxDefaultPosition, size);
   szr1->Add( mPanel, 1, wxEXPAND|wxALIGN_CENTRE|wxRIGHT, 4);

   /// Next row of wxFlexGridSizer
   szr1->Add(1, 1); // horizontal spacer
   szr1->Add(1, 1); // horizontal spacer

   freqRuler  = new RulerPanel(this, wxID_ANY);
   freqRuler->ruler.SetBounds(0, 0, 100, 100); // Ruler can't handle small sizes
   freqRuler->ruler.SetOrientation(wxHORIZONTAL);
   freqRuler->ruler.SetLog(true);
   freqRuler->ruler.SetRange(mLoFreq, mNyquist);
   freqRuler->ruler.SetFormat(Ruler::IntFormat);
   freqRuler->ruler.SetUnits(wxT(""));
   freqRuler->ruler.SetFlip(true);
   freqRuler->ruler.SetLabelEdges(true);
   freqRuler->ruler.GetMaxSize(NULL, &h);
   freqRuler->SetMinSize(wxSize(-1, h));
   szr1->Add( freqRuler, 0, wxEXPAND|wxALIGN_LEFT|wxRIGHT, 4 );

   szrV->Add( szr1, 1, wxEXPAND|wxALIGN_CENTER|wxALL, 0 );

   // -------------------------------------------------------------------
   // ROW 2 and 3: Type, Order, Ripple, Subtype, Cutoff
   // -------------------------------------------------------------------
   szr3 = new wxFlexGridSizer (6, 5, 2);  // 6 columns, 5px Vertical gap, 2px Horizontal gap
   flagslabel.Border(wxLEFT, 12).Align(wxALIGN_RIGHT | wxALIGN_CENTRE_VERTICAL );
   flagsunits.Align( wxALIGN_LEFT | wxALIGN_CENTRE_VERTICAL );

   st = new wxStaticText(this, wxID_ANY, _("&Filter Type:"));
   st->SetName(wxStripMenuCodes(st->GetLabel()));  // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
   szr3->Add(st, flagslabel );
   mFilterTypeCtl = new wxChoice (this, ID_FILTER_TYPE);
   mFilterTypeCtl->SetName(wxStripMenuCodes(st->GetLabel()));
   /*i18n-hint: Butterworth is the name of the person after whom the filter type is named.*/
   mFilterTypeCtl->Append (_("Butterworth"));
   /*i18n-hint: Chebyshev is the name of the person after whom the filter type is named.*/
   mFilterTypeCtl->Append (_("Chebyshev Type I"));
   /*i18n-hint: Chebyshev is the name of the person after whom the filter type is named.*/
   mFilterTypeCtl->Append (_("Chebyshev Type II"));
   szr3->Add(mFilterTypeCtl);

   /*i18n-hint: 'Order' means the complexity of the filter, and is a number between 1 and 10.*/
   st = new wxStaticText(this, wxID_ANY, _("O&rder:"));
   st->SetName(wxStripMenuCodes(st->GetLabel()));  // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
   szr3->Add(st, flagslabel );
   mFilterOrderCtl = new wxChoice (this, ID_FILTER_ORDER);
   mFilterOrderCtl->SetName(wxStripMenuCodes(st->GetLabel()));
   mFilterOrderCtl->Append (wxT("1"));
   mFilterOrderCtl->Append (wxT("2"));
   mFilterOrderCtl->Append (wxT("3"));
   mFilterOrderCtl->Append (wxT("4"));
   mFilterOrderCtl->Append (wxT("5"));
   mFilterOrderCtl->Append (wxT("6"));
   mFilterOrderCtl->Append (wxT("7"));
   mFilterOrderCtl->Append (wxT("8"));
   mFilterOrderCtl->Append (wxT("9"));
   mFilterOrderCtl->Append (wxT("10"));
   szr3->Add(mFilterOrderCtl);

   st = new wxStaticText(this, wxID_ANY, wxT(""));
   st->SetName(wxT(""));   // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
   szr3->Add(st);    // empty field in grid to balance Hz in next row

   szrPass = new wxBoxSizer( wxHORIZONTAL );
   st = new wxStaticText(this, wxID_ANY, _("Maximum &Passband Attenuation:"));
   st->SetName(wxStripMenuCodes(st->GetLabel()));  // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
   szrPass->Add(st, flagslabel);
   wxSize Size(wxDefaultSize);
   Size.SetWidth (40);
   mRippleCtl = new wxTextCtrl (this, ID_RIPPLE, wxT("0.0"), wxDefaultPosition, Size);
   mRippleCtl->SetName( _("Maximum passband attenuation (dB):"));
   szrPass->Add(mRippleCtl, 0 );
   st = new wxStaticText(this, wxID_ANY, _("dB"));
   st->SetName(st->GetLabel());  // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
   szrPass->Add(st, flagsunits);
   szr3->Add(szrPass);

   st = new wxStaticText(this, wxID_ANY, _("&Subtype:")); 
   szr3->Add(st, flagslabel);
   st->SetName(wxStripMenuCodes(st->GetLabel()));  // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
   mFilterSubTypeCtl = new wxChoice (this, ID_FILTER_SUBTYPE);
   mFilterSubTypeCtl->SetName(wxStripMenuCodes(st->GetLabel()));
   mFilterSubTypeCtl->Append (_("Lowpass"));
   mFilterSubTypeCtl->Append (_("Highpass"));
   szr3->Add(mFilterSubTypeCtl);

   st = new wxStaticText(this, wxID_ANY, _("C&utoff:"));
   st->SetName(wxStripMenuCodes(st->GetLabel()));  // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
   szr3->Add(st, flagslabel);
   Size.SetWidth (50);
   mCutoffCtl = new wxTextCtrl (this, ID_CUTOFF, wxT("0.0"), wxDefaultPosition, Size);
   mCutoffCtl->SetName(_("Cutoff(Hz):"));
   szr3->Add(mCutoffCtl, 0);
   st = new wxStaticText(this, wxID_ANY, _("Hz"));
   st->SetName(st->GetLabel());  // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
   szr3->Add(st, flagsunits);

   szrStop = new wxBoxSizer( wxHORIZONTAL );
   st = new wxStaticText(this, wxID_ANY, _("Minimum S&topband Attenuation:") );
   st->SetName(wxStripMenuCodes(st->GetLabel()));  // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
   szrStop->Add( st, flagslabel );
   Size.SetWidth (40);
   mStopbandRippleCtl = new wxTextCtrl (this, ID_STOPBAND_RIPPLE, wxT("0.0"), wxDefaultPosition, Size);
   mStopbandRippleCtl->SetName(_("Minimum stopband attenuation (dB):"));
   szrStop->Add(mStopbandRippleCtl, 0 );
   st = new wxStaticText(this, wxID_ANY, _("dB"));
   st->SetName(st->GetLabel());  // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
   szrStop->Add(st, flagsunits);
   szr3->Add(szrStop);

   // Calculate the min size with both pass and stop-band attenuations showing, to stop them jumping around
   szrPass->Show(true);
   szrStop->Show(true);
   szr3->SetMinSize(szr3->CalcMin());

   // -------------------------------------------------------------------
   // ROW 4: Subtype, Cutoff
   // -------------------------------------------------------------------

   szrV->Add( szr3, 0, wxALIGN_CENTER | wxALL, 4 );

   // -------------------------------------------------------------------
   // ROW 5: Preview, OK, & Cancel buttons
   // -------------------------------------------------------------------
   szrV->Add(CreateStdButtonSizer(this, ePreviewButton|eCancelButton|eOkButton), 0, wxEXPAND);

   // -------------------------------------------------------------------
   // Display now
   // -------------------------------------------------------------------
   SetAutoLayout(false);

   SetSizerAndFit( szrV );
   SetSizeHints(GetSize());

   return;
}


//
// Validate data
//
bool ScienFilterDialog::Validate()
{
   // In this case I don't think there's anything the user could have screwed up
   return true;
}

//
// Populate the window with relevant variables
//
bool ScienFilterDialog::TransferDataToWindow()
{
   dBMinSlider->SetValue((int)dBMin);
   dBMin = 0;                     // force refresh in TransferGraphLimitsFromWindow()

   dBMaxSlider->SetValue((int)dBMax);
   dBMax = 0;                    // force refresh in TransferGraphLimitsFromWindow()

   mFilterTypeCtl->SetSelection (FilterType);
   mFilterOrderCtl->SetSelection (Order - 1);
   mFilterSubTypeCtl->SetSelection (FilterSubtype);
   mCutoffCtl->SetValue (Internat::ToDisplayString(Cutoff));
   mRippleCtl->SetValue (Internat::ToDisplayString(Ripple));
   mStopbandRippleCtl->SetValue (Internat::ToDisplayString(StopbandRipple));
   EnableDisableRippleCtl (FilterType);

   return TransferGraphLimitsFromWindow();
}

//
// Retrieve data from the window
//
bool ScienFilterDialog::TransferGraphLimitsFromWindow()
{
   // Read the sliders and send to the panel
   wxString tip;

   bool rr = false;
   int dB = dBMinSlider->GetValue();
   if (dB != dBMin) {
      rr = true;
      dBMin = dB;
      mPanel->dBMin = dBMin;
#if wxUSE_TOOLTIPS
      tip.Printf(wxString(wxT("%d ")) + _("dB"),(int)dBMin);
      dBMinSlider->SetToolTip(tip);
#endif
   }

   dB = dBMaxSlider->GetValue();
   if (dB != dBMax) {
      rr = true;
      dBMax = dB;
      mPanel->dBMax = dBMax;
#if wxUSE_TOOLTIPS
      tip.Printf(wxString(wxT("%d ")) + _("dB"),(int)dBMax);
      dBMaxSlider->SetToolTip(tip);
#endif
   }

   // Refresh ruler if values have changed
   if (rr) {
      int w1, w2, h;
      dBRuler->ruler.GetMaxSize(&w1, &h);
      dBRuler->ruler.SetRange(dBMax, dBMin);
      dBRuler->ruler.GetMaxSize(&w2, &h);
      if( w1 != w2 )   // Reduces flicker
      {
         dBRuler->SetSize(wxSize(w2,h));
         szr1->Layout();
         freqRuler->Refresh(false);
      }
      dBRuler->Refresh(false);
   }

   mPanel->Refresh(false);

   return true;
}

bool ScienFilterDialog::CalcFilter (EffectScienFilter* effect)
{
   TrackListOfKindIterator iter(Track::Wave, effect->mTracks);
   WaveTrack *t = (WaveTrack *) iter.First();
   float hiFreq;
   if (t)
      hiFreq = ((float)(t->GetRate())/2.);
   else
      hiFreq = ((float)(GetActiveProject()->GetRate())/2.);

   // Set up the coefficients in all the biquads
   float fNorm = Cutoff / hiFreq;
   if (fNorm >= 0.9999)
      fNorm = 0.9999F;
   float fC = tan (PI * fNorm / 2);
   float fDCPoleDistSqr = 1.0F;
   float fZPoleX, fZPoleY;
   float fZZeroX, fZZeroY;
   float beta = cos (fNorm*PI);
   switch (FilterType)
   {
   case 0:     // Butterworth
      if ((Order & 1) == 0)
      {
         // Even order
         for (int iPair = 0; iPair < Order/2; iPair++)
         {
            float fSPoleX = fC * cos (PI - (iPair + 0.5) * PI / Order);
            float fSPoleY = fC * sin (PI - (iPair + 0.5) * PI / Order);
            BilinTransform (fSPoleX, fSPoleY, &fZPoleX, &fZPoleY);
            effect->mpBiquad[iPair]->fNumerCoeffs [0] = 1;
            if (FilterSubtype == 0)		// LOWPASS
               effect->mpBiquad[iPair]->fNumerCoeffs [1] = 2;
            else
               effect->mpBiquad[iPair]->fNumerCoeffs [1] = -2;
            effect->mpBiquad[iPair]->fNumerCoeffs [2] = 1;
            effect->mpBiquad[iPair]->fDenomCoeffs [0] = -2 * fZPoleX;
            effect->mpBiquad[iPair]->fDenomCoeffs [1] = square(fZPoleX) + square(fZPoleY);
            if (FilterSubtype == 0)		// LOWPASS
               fDCPoleDistSqr *= Calc2D_DistSqr (1, 0, fZPoleX, fZPoleY);
            else
               fDCPoleDistSqr *= Calc2D_DistSqr (-1, 0, fZPoleX, fZPoleY);		// distance from Nyquist
         }
      }
      else
      {
         // Odd order - first do the 1st-order section
         float fSPoleX = -fC;
         float fSPoleY = 0;
         BilinTransform (fSPoleX, fSPoleY, &fZPoleX, &fZPoleY);
         effect->mpBiquad[0]->fNumerCoeffs [0] = 1;
         if (FilterSubtype == 0)		// LOWPASS
            effect->mpBiquad[0]->fNumerCoeffs [1] = 1;
         else
            effect->mpBiquad[0]->fNumerCoeffs [1] = -1;
         effect->mpBiquad[0]->fNumerCoeffs [2] = 0;
         effect->mpBiquad[0]->fDenomCoeffs [0] = -fZPoleX;
         effect->mpBiquad[0]->fDenomCoeffs [1] = 0;
         if (FilterSubtype == 0)		// LOWPASS
            fDCPoleDistSqr = 1 - fZPoleX;
         else
            fDCPoleDistSqr = fZPoleX + 1;    // dist from Nyquist
         for (int iPair = 1; iPair <= Order/2; iPair++)
         {
            float fSPoleX = fC * cos (PI - iPair * PI / Order);
            float fSPoleY = fC * sin (PI - iPair * PI / Order);
            BilinTransform (fSPoleX, fSPoleY, &fZPoleX, &fZPoleY);
            effect->mpBiquad[iPair]->fNumerCoeffs [0] = 1;
            if (FilterSubtype == 0)		// LOWPASS
               effect->mpBiquad[iPair]->fNumerCoeffs [1] = 2;
            else
               effect->mpBiquad[iPair]->fNumerCoeffs [1] = -2;
            effect->mpBiquad[iPair]->fNumerCoeffs [2] = 1;
            effect->mpBiquad[iPair]->fDenomCoeffs [0] = -2 * fZPoleX;
            effect->mpBiquad[iPair]->fDenomCoeffs [1] = square(fZPoleX) + square(fZPoleY);
            if (FilterSubtype == 0)		// LOWPASS
               fDCPoleDistSqr *= Calc2D_DistSqr (1, 0, fZPoleX, fZPoleY);
            else
               fDCPoleDistSqr *= Calc2D_DistSqr (-1, 0, fZPoleX, fZPoleY);		// distance from Nyquist
         }
      }
      effect->mpBiquad[0]->fNumerCoeffs [0] *= fDCPoleDistSqr / (1 << Order);	// mult by DC dist from poles, divide by dist from zeroes
      effect->mpBiquad[0]->fNumerCoeffs [1] *= fDCPoleDistSqr / (1 << Order);
      effect->mpBiquad[0]->fNumerCoeffs [2] *= fDCPoleDistSqr / (1 << Order);
      break;

   case 1:     // Chebyshev Type 1
      double eps; eps = sqrt (pow (10.0, __max(0.001, Ripple) / 10.0) - 1);
      double a; a = log (1 / eps + sqrt(1 / square(eps) + 1)) / Order;
      // Assume even order to start
      for (int iPair = 0; iPair < Order/2; iPair++)
      {
         float fSPoleX = -fC * sinh (a) * sin ((2*iPair + 1) * PI / (2 * Order));
         float fSPoleY = fC * cosh (a) * cos ((2*iPair + 1) * PI / (2 * Order));
         BilinTransform (fSPoleX, fSPoleY, &fZPoleX, &fZPoleY);
         if (FilterSubtype == 0)		// LOWPASS
         {
            fZZeroX = -1;
            fDCPoleDistSqr = Calc2D_DistSqr (1, 0, fZPoleX, fZPoleY);
            fDCPoleDistSqr /= 2*2;  // dist from zero at Nyquist
         }
         else
         {
            // Highpass - do the digital LP->HP transform on the poles and zeroes
            ComplexDiv (beta - fZPoleX, -fZPoleY, 1 - beta * fZPoleX, -beta * fZPoleY, &fZPoleX, &fZPoleY);
            fZZeroX = 1;
            fDCPoleDistSqr = Calc2D_DistSqr (-1, 0, fZPoleX, fZPoleY);		// distance from Nyquist
            fDCPoleDistSqr /= 2*2;  // dist from zero at Nyquist
         }
         effect->mpBiquad[iPair]->fNumerCoeffs [0] = fDCPoleDistSqr;
         effect->mpBiquad[iPair]->fNumerCoeffs [1] = -2 * fZZeroX * fDCPoleDistSqr;
         effect->mpBiquad[iPair]->fNumerCoeffs [2] = fDCPoleDistSqr;
         effect->mpBiquad[iPair]->fDenomCoeffs [0] = -2 * fZPoleX;
         effect->mpBiquad[iPair]->fDenomCoeffs [1] = square(fZPoleX) + square(fZPoleY);
      }
      if ((Order & 1) == 0)
      {
         float fTemp = pow (10.0, -__max(0.001, Ripple) / 20.0);      // at DC the response is down R dB (for even-order)
         effect->mpBiquad[0]->fNumerCoeffs [0] *= fTemp;
         effect->mpBiquad[0]->fNumerCoeffs [1] *= fTemp;
         effect->mpBiquad[0]->fNumerCoeffs [2] *= fTemp;
      }
      else
      {
         // Odd order - now do the 1st-order section
         float fSPoleX = -fC * sinh (a);
         float fSPoleY = 0;
         BilinTransform (fSPoleX, fSPoleY, &fZPoleX, &fZPoleY);
         if (FilterSubtype == 0)		// LOWPASS
         {
            fZZeroX = -1;
            fDCPoleDistSqr = sqrt(Calc2D_DistSqr (1, 0, fZPoleX, fZPoleY));
            fDCPoleDistSqr /= 2;  // dist from zero at Nyquist
         }
         else
         {
            // Highpass - do the digital LP->HP transform on the poles and zeroes
            ComplexDiv (beta - fZPoleX, -fZPoleY, 1 - beta * fZPoleX, -beta * fZPoleY, &fZPoleX, &fZPoleY);
            fZZeroX = 1;
            fDCPoleDistSqr = sqrt(Calc2D_DistSqr (-1, 0, fZPoleX, fZPoleY));		// distance from Nyquist
            fDCPoleDistSqr /= 2;  // dist from zero at Nyquist
         }
         effect->mpBiquad[(Order-1)/2]->fNumerCoeffs [0] = fDCPoleDistSqr;
         effect->mpBiquad[(Order-1)/2]->fNumerCoeffs [1] = -fZZeroX * fDCPoleDistSqr;
         effect->mpBiquad[(Order-1)/2]->fNumerCoeffs [2] = 0;
         effect->mpBiquad[(Order-1)/2]->fDenomCoeffs [0] = -fZPoleX;
         effect->mpBiquad[(Order-1)/2]->fDenomCoeffs [1] = 0;
      }
      break;

   case 2:     // Chebyshev Type 2
      float fSZeroX, fSZeroY;
      float fSPoleX, fSPoleY;
      eps = pow (10.0, -__max(0.001, StopbandRipple) / 20.0);
      a = log (1 / eps + sqrt(1 / square(eps) + 1)) / Order;

      // Assume even order
      for (int iPair = 0; iPair < Order/2; iPair++)
      {
         ComplexDiv (fC, 0, -sinh (a) * sin ((2*iPair + 1) * PI / (2 * Order)),
            cosh (a) * cos ((2*iPair + 1) * PI / (2 * Order)),
            &fSPoleX, &fSPoleY);
         BilinTransform (fSPoleX, fSPoleY, &fZPoleX, &fZPoleY);
         fSZeroX = 0;
         fSZeroY = fC / cos (((2 * iPair) + 1) * PI / (2 * Order));
         BilinTransform (fSZeroX, fSZeroY, &fZZeroX, &fZZeroY);

         if (FilterSubtype == 0)		// LOWPASS
         {
            fDCPoleDistSqr = Calc2D_DistSqr (1, 0, fZPoleX, fZPoleY);
            fDCPoleDistSqr /= Calc2D_DistSqr (1, 0, fZZeroX, fZZeroY);
         }
         else
         {
            // Highpass - do the digital LP->HP transform on the poles and zeroes
            ComplexDiv (beta - fZPoleX, -fZPoleY, 1 - beta * fZPoleX, -beta * fZPoleY, &fZPoleX, &fZPoleY);
            ComplexDiv (beta - fZZeroX, -fZZeroY, 1 - beta * fZZeroX, -beta * fZZeroY, &fZZeroX, &fZZeroY);
            fDCPoleDistSqr = Calc2D_DistSqr (-1, 0, fZPoleX, fZPoleY);		// distance from Nyquist
            fDCPoleDistSqr /= Calc2D_DistSqr (-1, 0, fZZeroX, fZZeroY);
         }
         effect->mpBiquad[iPair]->fNumerCoeffs [0] = fDCPoleDistSqr;
         effect->mpBiquad[iPair]->fNumerCoeffs [1] = -2 * fZZeroX * fDCPoleDistSqr;
         effect->mpBiquad[iPair]->fNumerCoeffs [2] = (square(fZZeroX) + square(fZZeroY)) * fDCPoleDistSqr;
         effect->mpBiquad[iPair]->fDenomCoeffs [0] = -2 * fZPoleX;
         effect->mpBiquad[iPair]->fDenomCoeffs [1] = square(fZPoleX) + square(fZPoleY);
      }
      // Now, if it's odd order, we have one more to do
      if (Order & 1)
      {
         int iPair = (Order-1)/2; // we'll do it as a biquad, but it's just first-order
         ComplexDiv (fC, 0, -sinh (a) * sin ((2*iPair + 1) * PI / (2 * Order)),
            cosh (a) * cos ((2*iPair + 1) * PI / (2 * Order)),
            &fSPoleX, &fSPoleY);
         BilinTransform (fSPoleX, fSPoleY, &fZPoleX, &fZPoleY);
         fZZeroX = -1;     // in the s-plane, the zero is at infinity
         fZZeroY = 0;
         if (FilterSubtype == 0)		// LOWPASS
         {
            fDCPoleDistSqr = sqrt(Calc2D_DistSqr (1, 0, fZPoleX, fZPoleY));
            fDCPoleDistSqr /= 2;
         }
         else
         {
            // Highpass - do the digital LP->HP transform on the poles and zeroes
            ComplexDiv (beta - fZPoleX, -fZPoleY, 1 - beta * fZPoleX, -fZPoleY, &fZPoleX, &fZPoleY);
            fZZeroX = 1;
            fDCPoleDistSqr = sqrt(Calc2D_DistSqr (-1, 0, fZPoleX, fZPoleY));		// distance from Nyquist
            fDCPoleDistSqr /= 2;
         }
         effect->mpBiquad[iPair]->fNumerCoeffs [0] = fDCPoleDistSqr;
         effect->mpBiquad[iPair]->fNumerCoeffs [1] = -fZZeroX * fDCPoleDistSqr;
         effect->mpBiquad[iPair]->fNumerCoeffs [2] = 0;
         effect->mpBiquad[iPair]->fDenomCoeffs [0] = -fZPoleX;
         effect->mpBiquad[iPair]->fDenomCoeffs [1] = 0;
      }
      break;
   }
   effect->mOrder = Order;    // ?? needed for ProcessOne to work in Preview. This probably should be done a different way, but how?
   return true;
}

static double s_fChebyCoeffs [MAX_FILTER_ORDER][MAX_FILTER_ORDER+1] = {
   // For Chebyshev polynomials of the first kind (see http://en.wikipedia.org/wiki/Chebyshev_polynomial)
   // Coeffs are in the order 0, 1, 2...9
   {0, 1},        // order 1
   {-1, 0,  2},   // order 2 etc.
   {0, -3,  0,  4},
   {1,  0, -8,  0,    8},
   {0,  5,  0, -20, 0,   16},
   {-1, 0,  18, 0,  -48,    0,   32},
   {0, -7,  0, 56,    0, -112,    0,   64},
   {1,  0, -32, 0,  160,    0, -256,    0, 128},
   {0,  9,  0, -120,  0,  432,    0, -576,    0, 256},
   {-1, 0,  50, 0, -400,    0, 1120,    0, -1280,    0, 512}
};

static double ChebyPoly (int Order, double NormFreq)   // NormFreq = 1 at the f0 point (where response is R dB down)
{
   // Calc cosh (Order * acosh (NormFreq));
   double x = 1;
   double fSum = 0;
   wxASSERT (Order > 0 && Order <= MAX_FILTER_ORDER);
   for (int i = 0; i <= Order; i++)
   {
      fSum += s_fChebyCoeffs [Order-1][i] * x;
      x *= NormFreq;
   }
   return fSum;
}

float ScienFilterDialog::FilterMagnAtFreq (float Freq)
{
   float Magn;
   if (Freq >= mNyquist)
      Freq = mNyquist - 1;	// prevent tan(PI/2)
   float FreqWarped = tan (PI * Freq/(2*mNyquist));
   if (Cutoff >= mNyquist)
      Cutoff = mNyquist - 1;
   float CutoffWarped = tan (PI * Cutoff/(2*mNyquist));
   float fOverflowThresh = pow (10.0, 12.0 / (2*Order));    // once we exceed 10^12 there's not much to be gained and overflow could happen

   switch (FilterType)
   {
   case 0:		// Butterworth
   default:
      switch (FilterSubtype)
      {
      case 0:	// lowpass
      default:
         if (FreqWarped/CutoffWarped > fOverflowThresh)	// prevent pow() overflow
            Magn = 0;
         else
            Magn = sqrt (1 / (1 + pow (FreqWarped/CutoffWarped, 2*Order)));
         break;
      case 1:	// highpass
         if (FreqWarped/CutoffWarped > fOverflowThresh)
            Magn = 1;
         else
            Magn = sqrt (pow (FreqWarped/CutoffWarped, 2*Order) / (1 + pow (FreqWarped/CutoffWarped, 2*Order)));
         break;
      }
      break;

   case 1:     // Chebyshev Type 1
      double eps; eps = sqrt(pow (10.0, __max(0.001, Ripple)/10.0) - 1);
      switch (FilterSubtype)
      {
      case 0:	// lowpass
      default:
         Magn = sqrt (1 / (1 + square(eps) * square(ChebyPoly(Order, FreqWarped/CutoffWarped))));
         break;
      case 1:
         Magn = sqrt (1 / (1 + square(eps) * square(ChebyPoly(Order, CutoffWarped/FreqWarped))));
         break;
      }
      break;

   case 2:     // Chebyshev Type 2
      eps = 1 / sqrt(pow (10.0, __max(0.001, StopbandRipple)/10.0) - 1);
      switch (FilterSubtype)
      {
      case 0:	// lowpass
      default:
         Magn = sqrt (1 / (1 + 1 / (square(eps) * square(ChebyPoly(Order, CutoffWarped/FreqWarped)))));
         break;
      case 1:
         Magn = sqrt (1 / (1 + 1 / (square(eps) * square(ChebyPoly(Order, FreqWarped/CutoffWarped)))));
         break;
      }
      break;
   }

   return Magn;
}



// WDR: handler implementations for ScienFilterDialog

void ScienFilterDialog::OnOrder(wxCommandEvent &WXUNUSED(event))
{
   Order = mFilterOrderCtl->GetSelection() + 1;	// 0..n-1 -> 1..n
   mPanel->Refresh (false);
}

void ScienFilterDialog::OnFilterType (wxCommandEvent &WXUNUSED(event))
{
   FilterType = mFilterTypeCtl->GetSelection();
   EnableDisableRippleCtl (FilterType);
   mPanel->Refresh (false);
}

void ScienFilterDialog::OnFilterSubtype (wxCommandEvent &WXUNUSED(event))
{
   FilterSubtype = mFilterSubTypeCtl->GetSelection();
   mPanel->Refresh (false);
}

void ScienFilterDialog::OnCutoff (wxCommandEvent &WXUNUSED(event))
{
   double CutoffTemp;
   if (mCutoffCtl)
   {
      if (mCutoffCtl->GetValue().ToDouble(&CutoffTemp))
      {
         Cutoff = CutoffTemp;
         if (Cutoff >= mNyquist)
         {
            Cutoff = mNyquist - 1;  // could handle Nyquist as a special case? eg. straight through if LPF
            mCutoffCtl->SetValue(Internat::ToDisplayString(Cutoff));
         }
         wxButton *ok = (wxButton *) FindWindow(wxID_OK);
         if (Cutoff < 0.1)   // 0.1 Hz min
         {
            // Disable OK button
            ok->Enable(0);
         }
         else
            ok->Enable(1);
      }
      mPanel->Refresh (false);
   }
}

void ScienFilterDialog::OnRipple (wxCommandEvent &WXUNUSED(event))
{
   double RippleTemp;
   if (mRippleCtl)
   {
      if (mRippleCtl->GetValue().ToDouble(&RippleTemp))
         Ripple = RippleTemp;
      mPanel->Refresh (false);
   }
}

void ScienFilterDialog::OnStopbandRipple (wxCommandEvent &WXUNUSED(event))
{
   double RippleTemp;
   if (mStopbandRippleCtl)
   {
      if (mStopbandRippleCtl->GetValue().ToDouble(&RippleTemp))
         StopbandRipple = RippleTemp;
      mPanel->Refresh (false);
   }
}

void ScienFilterDialog::OnSliderDBMIN(wxCommandEvent &WXUNUSED(event))
{
   TransferGraphLimitsFromWindow();
}

void ScienFilterDialog::OnSliderDBMAX(wxCommandEvent &WXUNUSED(event))
{
   TransferGraphLimitsFromWindow();
}


void ScienFilterDialog::OnErase(wxEraseEvent &WXUNUSED(event))
{
   // Ignore it
}

void ScienFilterDialog::OnPaint(wxPaintEvent &WXUNUSED(event))
{
   wxPaintDC dc(this);

#if defined(__WXGTK__)
   dc.SetBackground(wxBrush(wxSystemSettings::GetColour(wxSYS_COLOUR_3DFACE)));
#endif

   dc.Clear();
}

void ScienFilterDialog::OnSize(wxSizeEvent &event)
{
   Layout();

   event.Skip();
}

void ScienFilterDialog::OnPreview(wxCommandEvent &WXUNUSED(event))
{
   CalcFilter (m_pEffect);
   m_pEffect->Preview();
}


void ScienFilterDialog::Finish(bool ok)
{
   mPanel = NULL;
   EndModal(ok);
}

void ScienFilterDialog::OnCancel(wxCommandEvent &WXUNUSED(event))
{
   Finish(false);
}

void ScienFilterDialog::OnOk(wxCommandEvent &event)
{
   CalcFilter (m_pEffect);

   if( Validate() )
   {
      Finish(true);
   }
   else
   {
      event.Skip(false);
   }
}

void ScienFilterDialog::EnableDisableRippleCtl (int FilterType)
{
   if (FilterType == 0)    // Butterworth
   {
      szrPass->Show(false);
      szrStop->Show(false);
/*      mRippleCtl->SetEditable (false);
      mStopbandRippleCtl->SetEditable (false);
      mRippleCtl->SetBackgroundColour (*wxLIGHT_GREY);
      mStopbandRippleCtl->SetBackgroundColour (*wxLIGHT_GREY);*/
   }
   else if (FilterType == 1)    // Chebyshev Type1
   {
      szrPass->Show(true);
      szrStop->Show(false);
/*      mRippleCtl->SetEditable (true);
      mStopbandRippleCtl->SetEditable (false);
      mRippleCtl->SetBackgroundColour (*wxWHITE);
      mStopbandRippleCtl->SetBackgroundColour (*wxLIGHT_GREY);*/
   }
   else                        // Chebyshev Type2
   {
      szrPass->Show(false);
      szrStop->Show(true);
/*      mRippleCtl->SetEditable (false);
      mStopbandRippleCtl->SetEditable (true);
      mRippleCtl->SetBackgroundColour (*wxLIGHT_GREY);
      mStopbandRippleCtl->SetBackgroundColour (*wxWHITE);*/
   }
}

