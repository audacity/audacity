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
END_EVENT_TABLE()

//Standard contructor
MeterToolBar::MeterToolBar()
: ToolBar(MeterBarID, _("Meter"), wxT("Meter"), true)
{
   mSizer = NULL;
   mPlayMeter = NULL;
   mRecordMeter = NULL;
}

MeterToolBar::~MeterToolBar()
{
}

void MeterToolBar::Clear()
{
   if (mPlayMeter)   mPlayMeter->Clear();
   if (mRecordMeter) mRecordMeter->Clear();
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

   mPlayMeter = new Meter( this,
                           wxID_ANY,
                           false,
                           wxDefaultPosition,
                           wxSize( 130, 55 ) );
   /* i18n-hint: (noun) The meter that shows the loudness of the audio playing.*/
   mPlayMeter->SetName( _("Play Meter"));
   /* i18n-hint: (noun) The meter that shows the loudness of the audio playing.
    This is the name used in screen reader software, where having 'Meter' first
    apparently is helpful to partially sighted people.  */
   mPlayMeter->SetLabel( _("Meter-Play"));
   mSizer->Add( mPlayMeter, wxGBPosition( 0, 0 ), wxDefaultSpan, wxEXPAND );

   mRecordMeter = new Meter( this,
                             wxID_ANY,
                             true,
                             wxDefaultPosition,
                             wxSize( 130, 55 ) );
   /* i18n-hint: (noun) The meter that shows the loudness of the audio being recorded.*/
   mRecordMeter->SetName( _("Record Meter"));
   /* i18n-hint: (noun) The meter that shows the loudness of the audio being recorded.
    This is the name used in screen reader software, where having 'Meter' first
    apparently is helpful to partially sighted people.  */
   mRecordMeter->SetLabel( _("Meter-Record") );
   mSizer->Add( mRecordMeter, wxGBPosition( 0, 1 ), wxDefaultSpan, wxEXPAND );

   RegenerateTooltips();
}

void MeterToolBar::UpdatePrefs()
{
   mPlayMeter->UpdatePrefs();
   mPlayMeter->HandleLayout();
   mRecordMeter->UpdatePrefs();
   mRecordMeter->HandleLayout();

   RegenerateTooltips();

   // Set label to pull in language change
   SetLabel(_("Meter"));

   // Give base class a chance
   ToolBar::UpdatePrefs();
}

void MeterToolBar::RegenerateTooltips()
{
#if wxUSE_TOOLTIPS
   mPlayMeter->SetToolTip( _("Playback Level") );
   mRecordMeter->SetToolTip( _("Recording Level (Click to monitor.)") );
#endif
}

bool MeterToolBar::DestroyChildren()
{
   mPlayMeter = NULL;
   mRecordMeter = NULL;

   return ToolBar::DestroyChildren();
}

void MeterToolBar::OnSize( wxSizeEvent & WXUNUSED(event) )
{
   int width, height;

   // We can be resized before populating...protect against it
   if( !mSizer )
   {
      return;
   }

   // Update the layout
   Layout();

   // Get the usable area
//   GetClientSize( &width, &height );
//   width -= mSizer->GetPosition().x;
   wxSize sz = GetSizer()->GetSize();
   width = sz.x; height = sz.y;

   // Default location for record meter
   wxGBPosition pos( 0, 1 );

   // Two horizontal
   if( width > height )
   {
      if( height > 120 )
      {
         // Stacked
         mPlayMeter->SetMinSize( wxSize( width, ( height / 2 ) ) );
         mRecordMeter->SetMinSize( wxSize( width, ( height / 2 ) ) );
         pos.SetCol( 0 );
         pos.SetRow( 1 );
      }
      else
      {
         // Side-by-side
         mPlayMeter->SetMinSize( wxSize( ( width / 2 ), height ) );
         mRecordMeter->SetMinSize( wxSize( ( width / 2 ), height ) );
      }

      mPlayMeter->SetStyle(Meter::HorizontalStereo);
      mRecordMeter->SetStyle(Meter::HorizontalStereo);
   }
   else
   {
      // Two vertical, side-by-side
      mPlayMeter->SetMinSize( wxSize( ( width / 2 ), height ) );
      mRecordMeter->SetMinSize( wxSize( ( width / 2 ), height ) );
      mPlayMeter->SetStyle(Meter::VerticalStereo);
      mRecordMeter->SetStyle(Meter::VerticalStereo);
   }

   // Position the record meter
   mSizer->SetItemPosition( mRecordMeter, pos );

   // And make it happen
   Layout();
}

void MeterToolBar::GetMeters(Meter **playMeter, Meter **recordMeter)
{
   *playMeter = mPlayMeter;
   *recordMeter = mRecordMeter;
}

void MeterToolBar::StartMonitoring()
{
   wxASSERT( mRecordMeter );
   mRecordMeter->StartMonitoring();
   //wxASSERT( mPlayMeter );
   //mPlayMeter->StartMonitoring();

}
