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



#include "MeterToolBar.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include <wx/setup.h> // for wxUSE_* macros

#ifndef WX_PRECOMP
#include <wx/event.h>
#include <wx/intl.h>
#include <wx/tooltip.h>
#endif

#include <wx/gbsizer.h>

#include "../AllThemeResources.h"
#include "../ProjectAudioIO.h"
#include "../widgets/MeterPanel.h"

IMPLEMENT_CLASS(MeterToolBar, ToolBar);

////////////////////////////////////////////////////////////
/// Methods for MeterToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE( MeterToolBar, ToolBar )
   EVT_SIZE( MeterToolBar::OnSize )
END_EVENT_TABLE()

//Standard constructor
MeterToolBar::MeterToolBar(AudacityProject &project, int type)
: ToolBar(project, type, XO("Combined Meter"), wxT("CombinedMeter"), true)
{
   if( mType == RecordMeterBarID ){
      mWhichMeters = kWithRecordMeter;
      mLabel = XO("Recording Meter");
      mSection = wxT("RecordMeter");
   } else if( mType == PlayMeterBarID ){
      mWhichMeters = kWithPlayMeter;
      mLabel = XO("Playback Meter");
      mSection = wxT("PlayMeter");
   } else {
      mWhichMeters = kWithPlayMeter | kWithRecordMeter;
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

   UpdatePrefs();

   // Simulate a size event to set initial meter placement/size
   wxSizeEvent dummy;
   OnSize(dummy);
}

void MeterToolBar::ReCreateButtons()
{
   MeterPanel::State playState{ false }, recordState{ false };

   auto &projectAudioIO = ProjectAudioIO::Get( mProject );
   if (mPlayMeter &&
      projectAudioIO.GetPlaybackMeter() == mPlayMeter->GetMeter())
   {
      playState = mPlayMeter->SaveState();
      projectAudioIO.SetPlaybackMeter( nullptr );
   }

   if (mRecordMeter &&
      projectAudioIO.GetCaptureMeter() == mRecordMeter->GetMeter())
   {
      recordState = mRecordMeter->SaveState();
      projectAudioIO.SetCaptureMeter( nullptr );
   }

   ToolBar::ReCreateButtons();

   mPlayMeter->RestoreState(playState);
   if( playState.mSaved  ){
      projectAudioIO.SetPlaybackMeter( mPlayMeter->GetMeter() );
   }
   mRecordMeter->RestoreState(recordState);
   if( recordState.mSaved ){
      projectAudioIO.SetCaptureMeter( mRecordMeter->GetMeter() );
   }
}

void MeterToolBar::Populate()
{
   SetBackgroundColour( theTheme.Colour( clrMedium  ) );
   Add((mSizer = safenew wxGridBagSizer()), 1, wxEXPAND);

   if( mWhichMeters & kWithRecordMeter ){
      //JKC: Record on left, playback on right.  Left to right flow
      //(maybe we should do it differently for Arabic language :-)  )
      mRecordMeter = safenew MeterPanel( &mProject,
                                this,
                                wxID_ANY,
                                true,
                                wxDefaultPosition,
                                wxSize( 260, 28 ) );
      /* i18n-hint: (noun) The meter that shows the loudness of the audio being recorded.*/
      mRecordMeter->SetName( XO("Record Meter"));
      /* i18n-hint: (noun) The meter that shows the loudness of the audio being recorded.
       This is the name used in screen reader software, where having 'Meter' first
       apparently is helpful to partially sighted people.  */
      mRecordMeter->SetLabel( XO("Meter-Record") );
      mSizer->Add( mRecordMeter, wxGBPosition( 0, 0 ), wxDefaultSpan, wxEXPAND );
   }

   if( mWhichMeters & kWithPlayMeter ){
      mPlayMeter = safenew MeterPanel( &mProject,
                              this,
                              wxID_ANY,
                              false,
                              wxDefaultPosition,
                              wxSize( 260, 28 ) );
      /* i18n-hint: (noun) The meter that shows the loudness of the audio playing.*/
      mPlayMeter->SetName( XO("Play Meter"));
      /* i18n-hint: (noun) The meter that shows the loudness of the audio playing.
       This is the name used in screen reader software, where having 'Meter' first
       apparently is helpful to partially sighted people.  */
      mPlayMeter->SetLabel( XO("Meter-Play"));
      mSizer->Add( mPlayMeter, wxGBPosition( (mWhichMeters & kWithRecordMeter)?1:0, 0 ), wxDefaultSpan, wxEXPAND );
   }

   RegenerateTooltips();
}

void MeterToolBar::UpdatePrefs()
{
   RegenerateTooltips();

   // Set label to pull in language change
   SetLabel(XO("Meter"));

   // Give base class a chance
   ToolBar::UpdatePrefs();
}

void MeterToolBar::RegenerateTooltips()
{
#if wxUSE_TOOLTIPS
   if( mPlayMeter )
      mPlayMeter->SetToolTip( XO("Playback Level") );
   if( mRecordMeter )
      mRecordMeter->SetToolTip( XO("Recording Level") );
#endif
}

void MeterToolBar::OnSize( wxSizeEvent & event) //WXUNUSED(event) )
{
   event.Skip();
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
      mRecordMeter->SetMinSize( wxSize( width, height ));
   }
   if( mPlayMeter ) {
      mPlayMeter->SetMinSize( wxSize( width, height));
      mSizer->SetItemPosition( mPlayMeter, pos );
   }

   // And make it happen
   Layout();
   Fit();
}

bool MeterToolBar::Expose( bool show )
{
   auto &projectAudioIO = ProjectAudioIO::Get( mProject );
   if( show ) {
      if( mPlayMeter ) {
         projectAudioIO.SetPlaybackMeter( mPlayMeter->GetMeter() );
      }

      if( mRecordMeter ) {
         projectAudioIO.SetCaptureMeter( mRecordMeter->GetMeter() );
      }
   } else {
      if( mPlayMeter &&
         projectAudioIO.GetPlaybackMeter() == mPlayMeter->GetMeter() ) {
         projectAudioIO.SetPlaybackMeter( nullptr );
      }

      if( mRecordMeter &&
         projectAudioIO.GetCaptureMeter() == mRecordMeter->GetMeter() ) {
         projectAudioIO.SetCaptureMeter( nullptr );
      }
   }

   return ToolBar::Expose( show );
}

// The meter's sizing code does not take account of the resizer
// Hence after docking we need to enlarge the bar (using fit)
// so that the resizer can be reached.
void MeterToolBar::SetDocked(ToolDock *dock, bool pushed) {
   ToolBar::SetDocked(dock, pushed);
   Fit();
}

static RegisteredToolbarFactory factory1{ RecordMeterBarID,
   []( AudacityProject &project ){
      return ToolBar::Holder{
         safenew MeterToolBar{ project, RecordMeterBarID } }; }
};
static RegisteredToolbarFactory factory2{ PlayMeterBarID,
   []( AudacityProject &project ){
      return ToolBar::Holder{
         safenew MeterToolBar{ project, PlayMeterBarID } }; }
};
static RegisteredToolbarFactory factory3{ MeterBarID,
   []( AudacityProject &project ){
      return ToolBar::Holder{
         safenew MeterToolBar{ project, MeterBarID } }; }
};

#include "ToolManager.h"

namespace {
AttachedToolBarMenuItem sAttachment1{
   /* i18n-hint: Clicking this menu item shows the toolbar
      with the recording level meters */
   RecordMeterBarID, wxT("ShowRecordMeterTB"), XXO("&Recording Meter Toolbar"),
   {}, { MeterBarID }
};
AttachedToolBarMenuItem sAttachment2{
   /* i18n-hint: Clicking this menu item shows the toolbar
      with the playback level meter */
   PlayMeterBarID, wxT("ShowPlayMeterTB"), XXO("&Playback Meter Toolbar"),
   {}, { MeterBarID }
};
//AttachedToolBarMenuItem sAttachment3{
//   /* --i18nhint: Clicking this menu item shows the toolbar
//      which has sound level meters */
//   MeterBarID, wxT("ShowMeterTB"), XXO("Co&mbined Meter Toolbar"),
//   { Registry::OrderingHint::After, "ShowPlayMeterTB" },
//   { PlayMeterBarID, RecordMeterBarID }
//};
}
