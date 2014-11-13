/**********************************************************************

  Audacity: A Digital Audio Editor

  MeterToolBar.cpp

  Dominic Mazzoni
  Leland Lucius

  See MeterToolBar.h for details

*******************************************************************//*!

\class MeterToolBar
\brief A ToolBar that holds the VU Meter

*//*******************************************************************/


#include "../Audacity.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/event.h>
#include <wx/intl.h>
#include <wx/tooltip.h>
#endif

#include <wx/gbsizer.h>

#include "MeterToolBar.h"

#include "../AudioIO.h"
#include "../widgets/Meter.h"

IMPLEMENT_CLASS(MeterToolBar, ToolBar);

////////////////////////////////////////////////////////////
/// Methods for MeterToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE( MeterToolBar, ToolBar )
   EVT_SIZE( MeterToolBar::OnSize )
   EVT_COMMAND(wxID_ANY, EVT_METER_PREFERENCES_CHANGED, MeterToolBar::OnMeterPrefsUpdated)
END_EVENT_TABLE()

//Standard contructor
MeterToolBar::MeterToolBar(int WhichMeters)
: ToolBar(MeterBarID, _("Combined Meter"), wxT("CombinedMeter"), true)
{
   mWhichMeters = WhichMeters;
   if( mWhichMeters == kWithRecordMeter ){
      mType = RecordMeterBarID;
      mLabel = _("Recording Meter");
      mSection = wxT("RecordMeter");
   }
   if( mWhichMeters == kWithPlayMeter ){
      mType = PlayMeterBarID;
      mLabel = _("Playback Meter");
      mSection = wxT("PlayMeter");
   }
   mSizer = NULL;
   mPlayMeter = NULL;
   mRecordMeter = NULL;
}

MeterToolBar::~MeterToolBar()
{
}


void MeterToolBar::Create(wxWindow * parent)
{
   ToolBar::Create(parent);

   // Simulate a size event to set initial meter placement/size
   wxSizeEvent dummy;
   OnSize(dummy);
}

void MeterToolBar::Populate()
{
   mSizer = new wxGridBagSizer();
   Add( mSizer, 1, wxEXPAND );

   if( mWhichMeters & kWithRecordMeter ){
      //JKC: Record on left, playback on right.  Left to right flow
      //(maybe we should do it differently for Arabic language :-)  )
      mRecordMeter = new Meter( this,
                                wxID_ANY,
                                true,
                                wxDefaultPosition,
                                wxSize( 260, 28 ) );
      /* i18n-hint: (noun) The meter that shows the loudness of the audio being recorded.*/
      mRecordMeter->SetName( _("Record Meter"));
      /* i18n-hint: (noun) The meter that shows the loudness of the audio being recorded.
       This is the name used in screen reader software, where having 'Meter' first
       apparently is helpful to partially sighted people.  */
      mRecordMeter->SetLabel( _("Meter-Record") );
      mSizer->Add( mRecordMeter, wxGBPosition( 0, 0 ), wxDefaultSpan, wxEXPAND );
   }

   if( mWhichMeters & kWithPlayMeter ){
      mPlayMeter = new Meter( this,
                              wxID_ANY,
                              false,
                              wxDefaultPosition,
                              wxSize( 260, 28 ) );
      /* i18n-hint: (noun) The meter that shows the loudness of the audio playing.*/
      mPlayMeter->SetName( _("Play Meter"));
      /* i18n-hint: (noun) The meter that shows the loudness of the audio playing.
       This is the name used in screen reader software, where having 'Meter' first
       apparently is helpful to partially sighted people.  */
      mPlayMeter->SetLabel( _("Meter-Play"));
      mSizer->Add( mPlayMeter, wxGBPosition( (mWhichMeters & kWithRecordMeter)?1:0, 0 ), wxDefaultSpan, wxEXPAND );
   }
   if( IsVisible() )
      MeterToolBars::AddMeters( mPlayMeter, mRecordMeter );
   RegenerateTooltips();
}

void MeterToolBar::UpdatePrefs()
{
   if( mPlayMeter )
      mPlayMeter->UpdatePrefs();
   if( mRecordMeter )
      mRecordMeter->UpdatePrefs();

   RegenerateTooltips();

   // Set label to pull in language change
   SetLabel(_("Meter"));

   // Give base class a chance
   ToolBar::UpdatePrefs();
}

void MeterToolBar::RegenerateTooltips()
{
#if wxUSE_TOOLTIPS
   if( mPlayMeter )
      mPlayMeter->SetToolTip( _("Playback Level.") );
   if( mRecordMeter )
      mRecordMeter->SetToolTip( 
         !gAudioIO->IsMonitoring() ? 
         _("Recording Level. (Click to monitor).") :
         _("Recording Level. (Click to turn monitoring off).") 
         );
#endif
}

bool MeterToolBar::DestroyChildren()
{
   MeterToolBars::RemoveMeters( mPlayMeter, mRecordMeter );
   mPlayMeter = NULL;
   mRecordMeter = NULL;

   return ToolBar::DestroyChildren();
}

void MeterToolBar::OnMeterPrefsUpdated(wxCommandEvent & WXUNUSED(evt))
{
   UpdatePrefs();   
}

void MeterToolBar::OnSize( wxSizeEvent & WXUNUSED(event) )
{
   int width, height;

   // We can be resized before populating...protect against it
   if( !mSizer ) {
      return;
   }

   // Update the layout
   Layout();

   // Get the usable area
   wxSize sz = GetSizer()->GetSize();
   width = sz.x; height = sz.y;

   int nMeters = 
      ((mRecordMeter ==NULL) ? 0:1) +
      ((mPlayMeter ==NULL) ? 0:1);

   bool bHorizontal = ( width > height );
   bool bEndToEnd   = ( nMeters > 1 ) && wxMin( width, height ) < (60 * nMeters);

   // Default location for second meter
   wxGBPosition pos( 0, 0 );
   // If 2 meters, share the height or width.
   if( nMeters > 1 ){
      if( bHorizontal ^ bEndToEnd ){
         height /= nMeters;
         pos = wxGBPosition( 1, 0 );
      } else {
         width /= nMeters;
         pos = wxGBPosition( 0, 1 );
      }
   }

   if( mRecordMeter ) {
      mRecordMeter->SetStyle(bHorizontal ? Meter::HorizontalStereo : Meter::VerticalStereo);
      mRecordMeter->SetMinSize( wxSize( width, height ));
   }
   if( mPlayMeter ) {
      mPlayMeter->SetStyle(bHorizontal ? Meter::HorizontalStereo : Meter::VerticalStereo);
      mPlayMeter->SetMinSize( wxSize( width, height ));
      mSizer->SetItemPosition( mPlayMeter, pos );
   }

   // And make it happen
   Layout();
}

bool MeterToolBar::Expose( bool show )
{
   if( show )
      MeterToolBars::AddMeters( mPlayMeter, mRecordMeter );
   else
      MeterToolBars::RemoveMeters( mPlayMeter, mRecordMeter );
   return ToolBar::Expose( show );
}

wxSize MeterToolBar::GetDockedSize()
{
   const int tbs = toolbarSingle + toolbarGap;
   wxSize sz = GetSize();
   wxSize sz2 = GetMinSize();
   sz.x = wxMax( sz.x, sz2.x );
   sz.y = wxMax( sz.y, sz2.y );
   // 50 is the size where we switch from expanded to compact.
   if( sz.y < 50 )
      sz.y = tbs-1;
   else 
      sz.y = 2 * tbs -1;
   return sz;
}

// Locally defined - we can change implementation easily later.
namespace MeterToolBars {
   Meter * mPlayMeter=NULL;
   Meter * mRecordMeter=NULL;
}

void MeterToolBars::AddMeters(Meter *playMeter, Meter *recordMeter)
{
   if( playMeter != NULL )      mPlayMeter = playMeter;
   if( recordMeter != NULL )    mRecordMeter = recordMeter;
}

void MeterToolBars::RemoveMeters(Meter *playMeter, Meter *recordMeter)
{
   if( mPlayMeter == playMeter )      mPlayMeter = NULL;
   if( mRecordMeter == recordMeter )  mRecordMeter = NULL;
}

void MeterToolBars::GetMeters(Meter **playMeter, Meter **recordMeter)
{
   *playMeter = mPlayMeter;
   *recordMeter = mRecordMeter;
}

void MeterToolBars::StartMonitoring()
{
   if( mRecordMeter == NULL )
      return;
   wxASSERT( mRecordMeter );
   mRecordMeter->StartMonitoring();
   //wxASSERT( mPlayMeter );
   //mPlayMeter->StartMonitoring();
}

void MeterToolBars::Clear()
{
   if (mPlayMeter)   mPlayMeter->Clear();
   if (mRecordMeter) mRecordMeter->Clear();
}

