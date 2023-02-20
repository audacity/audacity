/**********************************************************************

  Audacity: A Digital Audio Editor

  SelectionBar.cpp

  Copyright 2005 Dominic Mazzoni
            2023 Dmitry Vedenko

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

*******************************************************************//**

\class SelectionBar
\brief (not quite a Toolbar) at foot of screen for setting and viewing the
selection range.

*//****************************************************************//**

\class SelectionBarListener
\brief A parent class of SelectionBar, used to forward events to do
with changes in the SelectionBar.

*//*******************************************************************/



#include "SelectionBar.h"

#include "SelectionBarListener.h"
#include "ToolManager.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include <wx/setup.h> // for wxUSE_* macros

#ifndef WX_PRECOMP
#include <wx/checkbox.h>
#include <wx/combobox.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/valtext.h>
#include <wx/stattext.h>
#endif
#include <wx/statline.h>
#include <wx/menu.h>


#include "AudioIO.h"
#include "AColor.h"
#include "../KeyboardCapture.h"
#include "Prefs.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectRate.h"
#include "../ProjectSettings.h"
#include "ViewInfo.h"
#include "AllThemeResources.h"
#include "../widgets/AButton.h"
#include "../widgets/auStaticText.h"
#include "../widgets/BasicMenu.h"
#include "../widgets/NumericTextCtrl.h"
#include "wxWidgetsWindowPlacement.h"

#if wxUSE_ACCESSIBILITY
#include "WindowAccessible.h"
#endif

IMPLEMENT_CLASS(SelectionBar, ToolBar);

const static wxChar *numbers[] =
{
   wxT("0"), wxT("1"), wxT("2"), wxT("3"), wxT("4"),
   wxT("5"), wxT("6"), wxT("7"), wxT("8"), wxT("9")
};

enum {
   SelectionBarFirstID = 2700,
   
   StartTimeID,
   LengthTimeID,
   CenterTimeID,
   EndTimeID,
};

BEGIN_EVENT_TABLE(SelectionBar, ToolBar)
   EVT_SIZE(SelectionBar::OnSize)
   EVT_TEXT(StartTimeID, SelectionBar::OnChangedTime)
   EVT_TEXT(LengthTimeID, SelectionBar::OnChangedTime)
   EVT_TEXT(CenterTimeID, SelectionBar::OnChangedTime)
   EVT_TEXT(EndTimeID, SelectionBar::OnChangedTime)
   
   EVT_IDLE( SelectionBar::OnIdle )

   EVT_COMMAND(wxID_ANY, EVT_TIMETEXTCTRL_UPDATED, SelectionBar::OnUpdate)
   EVT_COMMAND(wxID_ANY, EVT_CAPTURE_KEY, SelectionBar::OnCaptureKey)
END_EVENT_TABLE()

Identifier SelectionBar::ID()
{
   return wxT("Selection");
}

SelectionBar::SelectionBar( AudacityProject &project )
: ToolBar(project, XO("Selection"), ID()),
  mListener(NULL), mRate(0.0),
  mStart(0.0), mEnd(0.0), mLength(0.0), mCenter(0.0), mAudio(0.0),
  mDrive1( StartTimeID), mDrive2( EndTimeID ),
  mSelectionMode(0),
  mStartTime(NULL), mCenterTime(NULL), mLengthTime(NULL), mEndTime(NULL),
  mAudioTime(NULL),
  mRateChangedSubscription(
     ProjectRate::Get(project).Subscribe(
         [this](double rate) { UpdateRate(rate); }))
{
   // Make sure we have a valid rate as the NumericTextCtrl()s
   // created in Populate()
   // depend on it.  Otherwise, division-by-zero floating point exceptions
   // will occur.
   // Refer to bug #462 for a scenario where the division-by-zero causes
   // Audacity to fail.
   // We expect mRate to be set from the project later.
   mRate = ProjectRate::Get(project).GetRate();

   // Selection mode of 0 means showing 'start' and 'end' only.
   mSelectionMode = gPrefs->ReadLong(wxT("/SelectionToolbarMode"),  0);
}

SelectionBar::~SelectionBar()
{
}

bool SelectionBar::ShownByDefault() const
{
   return
#ifdef EXPERIMENTAL_DA
      false
#else
      true
#endif
   ;
}

ToolBar::DockID SelectionBar::DefaultDockID() const
{
   return BotDockID;
}

SelectionBar &SelectionBar::Get( AudacityProject &project )
{
   auto &toolManager = ToolManager::Get( project );
   return *static_cast<SelectionBar*>(toolManager.GetToolBar(ID()));
}

const SelectionBar &SelectionBar::Get( const AudacityProject &project )
{
   return Get( const_cast<AudacityProject&>( project )) ;
}

void SelectionBar::Create(wxWindow * parent)
{
   ToolBar::Create(parent);
   UpdatePrefs();
}


auStaticText * SelectionBar::AddTitle(
   const TranslatableString & Title, wxSizer * pSizer ){
   const auto translated = Title.Translation();
   auStaticText * pTitle = safenew auStaticText(this, translated );
   pTitle->SetBackgroundColour( theTheme.Colour( clrMedium ));
   pTitle->SetForegroundColour( theTheme.Colour( clrTrackPanelText ) );
   pSizer->Add( pTitle, 0, wxEXPAND | wxRIGHT, 5 );

   return pTitle;
}


NumericTextCtrl * SelectionBar::AddTime(
   const TranslatableString &Name, int id, wxSizer * pSizer ){
   auto formatName = mListener ? mListener->AS_GetSelectionFormat()
      : NumericFormatSymbol{};
   auto pCtrl = safenew NumericTextCtrl(
      this, id, NumericConverter::TIME, formatName, 0.0, mRate);
   pCtrl->SetName( Name );
   pSizer->Add(pCtrl, 0, wxALIGN_TOP | wxRIGHT, 5);
   return pCtrl;
}

void SelectionBar::AddVLine(  wxSizer * pSizer ){
   pSizer->Add(safenew wxStaticLine(this, -1, wxDefaultPosition,
                                   wxSize(1, toolbarSingle-10),
                                   wxLI_VERTICAL),
                  0,  wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);
}

void SelectionBar::Populate()
{
   SetBackgroundColour( theTheme.Colour( clrMedium  ) );

   mStartTime = mEndTime = mLengthTime = mCenterTime = mAudioTime = nullptr;

   // Outer sizer has space top and left.
   // Inner sizers have space on right only.
   // This choice makes for a nice border and internal spacing and places clear responsibility
   // on each sizer as to what spacings it creates.
   wxFlexGridSizer *mainSizer = safenew wxFlexGridSizer(2, 1, 1);
   Add(mainSizer, 0, wxALIGN_CENTER_VERTICAL | wxLEFT, 5);

   // Top row (mostly labels)
   
   {
      auto vSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
      auto selectionText = AddTitle(XO("Selection"), vSizer.get());

      auto setupBtn =
         safenew AButton(this, wxID_ANY, wxDefaultPosition, { 20, 20 });

      auto showMenu = [this, setupBtn]()
      {
         const wxString choices[4] = {
            _("Start and End of Selection"),
            _("Start and Length of Selection"),
            _("Length and End of Selection"),
            _("Length and Center of Selection"),
         };
         //mSelectionMode
         wxMenu menu;
         int id = 0;
         for (auto& choice : choices)
         {
            auto subMenu = menu.AppendRadioItem(wxID_ANY, choice);

            if (id == mSelectionMode)
               subMenu->Check();

            menu.Bind(
               wxEVT_MENU,
               [this, id](auto& evt)
               {
                  SetSelectionMode(id);
                  SelectionModeUpdated();
               },
               subMenu->GetId());

            ++id;
         }

         menu.Bind(wxEVT_MENU_CLOSE, [setupBtn](auto&) { setupBtn->PopUp(); });
         
         
         BasicMenu::Handle { &menu }.Popup(
            wxWidgetsWindowPlacement { setupBtn });
      };

      setupBtn->Bind(wxEVT_BUTTON, [this, showMenu](auto&) { showMenu(); });

      vSizer->AddStretchSpacer();
      vSizer->Add(setupBtn, 0, wxALIGN_RIGHT | wxBOTTOM | wxRIGHT, 5);
      
      mainSizer->Add(vSizer.release(), 0, wxALIGN_TOP | wxRIGHT | wxEXPAND, 0);    
   }

   {
      auto vSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);

      mStartTime  = AddTime( XO("Start"), StartTimeID, vSizer.get() );
      mLengthTime = AddTime( XO("Length"), LengthTimeID, vSizer.get() );
      mCenterTime = AddTime( XO("Center"), CenterTimeID, vSizer.get() );
      mEndTime    = AddTime( XO("End"), EndTimeID, vSizer.get() );
      mainSizer->Add(vSizer.release(), 0, wxALIGN_TOP | wxRIGHT, 0);
   }

   // This shows/hides controls.
   // Do this before layout so that we are sized right.
   SetSelectionMode(mSelectionMode);
   mainSizer->Layout();
   RegenerateTooltips();
   Layout();
}

void SelectionBar::UpdatePrefs()
{
   // The project rate is no longer driven from here.
   // When preferences change, the Project learns about it too.
   // If necessary we can drive the SelectionBar mRate via the Project
   // calling our SetRate().
   // As of 13-Sep-2018, changes to the sample rate pref will only affect 
   // creation of new projects, not the sample rate in existing ones.

   // This will only change the selection mode during a "Reset Configuration"
   // action since the read value will be the same during a normal preferences
   // update.
   mSelectionMode = gPrefs->ReadLong(wxT("/SelectionToolbarMode"),  0);

   // This will only change the time format during a "Reset Configuration"
   // action since the read value will be the same during a normal preferences
   // update.
   wxCommandEvent e;
   e.SetString(NumericTextCtrl::LookupFormat(
               NumericConverter::TIME,
               gPrefs->Read(wxT("/SelectionFormat"), wxT(""))).Internal());
   OnUpdate(e);

   // Set label to pull in language change
   SetLabel(XO("Selection"));

   RegenerateTooltips();
   // Give base class a chance
   ToolBar::UpdatePrefs();
}

void SelectionBar::SetListener(SelectionBarListener *l)
{
   mListener = l;
   SetSelectionFormat(mListener->AS_GetSelectionFormat());
};

void SelectionBar::RegenerateTooltips()
{
#if wxUSE_TOOLTIPS
   auto formatName =
      mListener
         ? mListener->AS_GetSelectionFormat()
         : NumericFormatSymbol{};
#endif
}

void SelectionBar::OnSize(wxSizeEvent &evt)
{
   Refresh( true );

   evt.Skip();
}

// When a control value is changed, this function is called.
// It determines the values for the other controls.
void SelectionBar::ModifySelection(int newDriver, bool done)
{
   // If the user moved to a different control, then update which
   // two controls drive the others.
   if( newDriver != mDrive2 )
      SetDrivers( mDrive2, newDriver);

   // Only update a value if user typed something in.
   // The reason is the controls may be less accurate than 
   // the values.
   if( newDriver == StartTimeID )
      mStart = mStartTime->GetValue();
   if( newDriver == EndTimeID )
      mEnd = mEndTime->GetValue();
   if( newDriver == LengthTimeID )
      mLength = mLengthTime->GetValue();
   if( newDriver == CenterTimeID )
      mCenter = mCenterTime->GetValue();

   // There are four controls, and two constraints, which are:
   //    mid = (start+end)/2
   //    length = (end-start)
   // Therefore we can select any two controls as 'drivers' of
   // the other two.
   // Here we compute 'i' which combines the identity of the two 
   // driving controls, to use it as an index.
   // The order of the two drivers generally does not matter much,
   // except that we want:
   //    start < end
   // and preserve that by adjusting the least dominant driving
   // control.
   int i = mDrive1 + 4 * mDrive2;
   switch(i) {
   case StartTimeID + 4 * EndTimeID:
      if( mEnd < mStart )
         mStart = mEnd;
   case StartTimeID * 4 + EndTimeID:
      if( mStart > mEnd )
         mEnd = mStart;
      mLength = mEnd - mStart;
      mCenter = (mStart+mEnd)/2.0;
      break;

   case StartTimeID + 4 * LengthTimeID:
   case StartTimeID * 4 + LengthTimeID:
      mEnd = mStart+mLength;
      mCenter = (mStart+mEnd)/2.0;
      break;

   case EndTimeID + 4 * LengthTimeID:
      if( mEnd - mLength < 0 )
         mEnd += (mLength - mEnd);
   case EndTimeID * 4 + LengthTimeID:
      if( mEnd - mLength < 0)
         mLength -= (mLength - mEnd);
      mStart = mEnd - mLength;
      mCenter = (mStart+mEnd)/2.0;
      break;

   case LengthTimeID + 4 * CenterTimeID:
      if( mCenter - (mLength / 2) < 0 )
         mLength = (mCenter * 2);
   case LengthTimeID * 4 + CenterTimeID:
      if( mCenter - (mLength / 2) < 0 )
         mCenter = (mLength / 2);
      mStart = mCenter - mLength/2.0;
      mEnd = mCenter + mLength/2.0;
      break;

   default:
      // The above should cover all legal combinations of two distinct controls.
      wxFAIL_MSG( "Illegal sequence of selection changes");
   }

   // Refresh the controls now
   ValuesToControls();

   // Places the start-end markers on the track panel.
   mListener->AS_ModifySelection(mStart, mEnd, done);
}

void SelectionBar::OnChangedTime(wxCommandEvent & event)
{
   ModifySelection(event.GetId(), event.GetInt() != 0);
}

// Called when one of the format drop downs is changed.
void SelectionBar::OnUpdate(wxCommandEvent &evt)
{
   wxWindow *w = FindFocus();
   NumericTextCtrl ** Ctrls[5] = { &mStartTime, &mEndTime, &mLengthTime, &mCenterTime, &mAudioTime };
   int i;
   int iFocus = -1;
   for(i=0;i<5;i++)
      if( w == *Ctrls[i] )
         iFocus = i;

   evt.Skip(false);

   auto format = NumericTextCtrl::LookupFormat(NumericConverter::TIME, evt.GetString());

   // Save format name before recreating the controls so they resize properly
   if (mStartTime)
   {
      if (mListener)
         mListener->AS_SetSelectionFormat(format);
   }

   // ReCreateButtons() will get rid of our sizers and controls
   // so reset pointers first.
   for( i=0;i<5;i++)
      *Ctrls[i]=NULL;

   ToolBar::ReCreateButtons();

   ValuesToControls();

   for( i=0;i<5;i++)
      if( *Ctrls[i] )
         (*Ctrls[i])->SetFormatName( format );

   if( iFocus >=0 )
      if( *Ctrls[iFocus] )
         (*Ctrls[iFocus])->SetFocus();

   RegenerateTooltips();

   Updated();
}

// The two drivers are the numbers of the two controls which drive the other ones.
// The user gets to see which controls are drivers and which driven.
void SelectionBar::SetDrivers( int driver1, int driver2 )
{
   mDrive1 = driver1;
   mDrive2 = driver2;

   NumericTextCtrl ** Ctrls[4] = { &mStartTime, &mCenterTime, &mLengthTime, &mEndTime};
   static TranslatableString Text[4] = {
      /* i18n-hint noun */
      XO("Start"),
      XO("Center"),
      XO("Length"),
      /* i18n-hint noun */
      XO("End")
   };

   for(int i=0;i<4;i++){
      int id = i + StartTimeID;
      int fixed = (( id == mDrive2 )?mDrive1:mDrive2)-StartTimeID;

      const auto &Temp = Text[i];
      auto Title = ( (id!=mDrive1) && (id!=mDrive2 ) )
         /* i18n-hint: %s is replaced e.g by one of 'Length', 'Center',
            'Start', or 'End' (translated), to indicate that it will be
            calculated from other parameters. */
         ? XO("%s - driven").Format( Temp )
         : Temp ;
      auto VoiceOverText =
         /* i18n-hint: each string is replaced by one of 'Length', 'Center',
            'Start', or 'End' (translated) */
         XO("Selection %s. %s won't change.").Format( Temp, Text[fixed] );
      if( *Ctrls[i] ){
         (*Ctrls[i])->SetName( Temp );
      }
   }
}

void SelectionBar::OnIdle( wxIdleEvent &evt )
{
   evt.Skip();
   auto &project = mProject;
   const auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;

   double audioTime;

   auto &projectAudioIO = ProjectAudioIO::Get( project );
   if ( projectAudioIO.IsAudioActive() ){
      auto gAudioIO = AudioIO::Get();
      audioTime = gAudioIO->GetStreamTime();
   }
   else {
      const auto &playRegion = ViewInfo::Get( project ).playRegion;
      audioTime = playRegion.GetStart();
   }

   SetTimes(selectedRegion.t0(), selectedRegion.t1(), audioTime);
}

void SelectionBar::SelectionModeUpdated()
{
   // We just changed the mode.  Remember it.
   gPrefs->Write(wxT("/SelectionToolbarMode"), mSelectionMode);
   gPrefs->Flush();

   wxSize sz = GetMinSize();
   sz.SetWidth( 10 );
   SetMinSize( sz );
   Fit();
   Layout();
   Updated();
}

// We used to have 8 modes which showed different combinations of the
// length, start, end, center controls.
// Mode 7 for example showed all four at the same time.
void SelectionBar::SetSelectionMode(int mode)
{
   // Only modes 0 to 3 are now supported,
   // so fix up a mode that could have come from the config.
   const int maxMode = 3;

   if( mode > maxMode )
      mode = 0;
   if( mode < 0 )
      mode = 0;
   mSelectionMode = mode;

   // First decide which two controls drive the others...
   // For example the last option is with all controls shown, and in that mode we 
   // initially have start and end driving.
   int Drive2[] = { StartTimeID, StartTimeID,  LengthTimeID, LengthTimeID, 
                    StartTimeID, StartTimeID,  StartTimeID,  StartTimeID};
   int Drive1[] = { EndTimeID,   LengthTimeID, EndTimeID,    CenterTimeID, 
                    EndTimeID,   LengthTimeID, EndTimeID,    EndTimeID};

   SetDrivers( Drive1[mode], Drive2[mode] );
   // Then show/hide the relevant controls.
   ShowHideControls( mode );
}

// Our mode determines which controls are visible.
void SelectionBar::ShowHideControls(int mode)
{
   // The bits in these say which controls are visible.
   int masks[8]= { 
      9, 5, 12, 6, // 2 items shown
      13, 7, 11,// 3 items shown
      15};
   int mask = masks[mode];

   NumericTextCtrl ** Ctrls[4]  = { &mStartTime,  &mCenterTime,  &mLengthTime,  &mEndTime};
   for(int i=0;i<4;i++){
      if( *Ctrls[i]){ 
         (*Ctrls[i])->Show( (mask & (1<<i))!=0 );
         (*Ctrls[i])->Refresh();
      }
   }
}

void SelectionBar::ValuesToControls()
{
   NumericTextCtrl ** Ctrls[5] = { &mStartTime, &mEndTime, &mLengthTime, &mCenterTime, &mAudioTime };
   double Values[5] = {mStart, mEnd, mLength, mCenter, mAudio };
   int i;
   for(i=0;i<5;i++)
      if( *Ctrls[i] )
         (*Ctrls[i])->SetValue( Values[i] );
}

// A time has been set.  Update the control values.
void SelectionBar::SetTimes(double start, double end, double audio)
{
   if ( start != mStart || end != mEnd || audio != mAudio
      || mLastSelectionMode != mSelectionMode
   ) {
      mStart = start;
      mEnd = end;
      mLength = end-start;
      mCenter = (end+start)/2.0;
      mAudio = audio;
      mLastSelectionMode = mSelectionMode;

      ValuesToControls();
   }
}

void SelectionBar::SetSelectionFormat(const NumericFormatSymbol & format)
{
   bool changed =
      mStartTime->SetFormatString(mStartTime->GetBuiltinFormat(format));

   // Test first whether changed, to avoid infinite recursion from OnUpdate
   if ( changed ) {
      wxCommandEvent e;
      e.SetString(format.Internal());
      OnUpdate(e);
   }
}

void SelectionBar::UpdateRate(double rate)
{
   if (rate != mRate)
   {
      // if the rate is actually being changed
      mRate = rate; // update the stored rate

      // update the TimeTextCtrls if they exist
      NumericTextCtrl** Ctrls[5] = { &mStartTime, &mEndTime, &mLengthTime,
                                     &mCenterTime, &mAudioTime };
      int i;
      for (i = 0; i < 5; i++)
         if (*Ctrls[i])
            (*Ctrls[i])->SetSampleRate(rate);
   }
}

void SelectionBar::OnFocus(wxFocusEvent &event)
{
   KeyboardCapture::OnFocus( *this, event );
}

void SelectionBar::OnCaptureKey(wxCommandEvent &event)
{
   wxKeyEvent *kevent = (wxKeyEvent *)event.GetEventObject();
   wxWindow *w = FindFocus();
   int keyCode = kevent->GetKeyCode();

   // Convert numeric keypad entries.
   if ((keyCode >= WXK_NUMPAD0) && (keyCode <= WXK_NUMPAD9)) {
      keyCode -= WXK_NUMPAD0 - '0';
   }

   if (keyCode >= '0' && keyCode <= '9') {
      return;
   }

   event.Skip();
}

static RegisteredToolbarFactory factory{
   []( AudacityProject &project ){
      return ToolBar::Holder{ safenew SelectionBar{ project } }; }
};

namespace {
AttachedToolBarMenuItem sAttachment{
   /* i18n-hint: Clicking this menu item shows the toolbar
      for selecting a time range of audio */
   SelectionBar::ID(), wxT("ShowSelectionTB"), XXO("&Selection Toolbar")
};
}

