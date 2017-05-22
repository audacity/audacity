/**********************************************************************

  Audacity: A Digital Audio Editor

  SelectionBar.cpp

  Copyright 2005 Dominic Mazzoni

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


#include "../Audacity.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/combobox.h>
#include <wx/intl.h>
#include <wx/radiobut.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/valtext.h>
#include <wx/stattext.h>
#endif
#include <wx/statline.h>


#include "SelectionBarListener.h"
#include "SelectionBar.h"

#include "../widgets/AButton.h"
#include "../AudioIO.h"
#include "../AColor.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Snap.h"
#include "../widgets/NumericTextCtrl.h"
#include "../AllThemeResources.h"

IMPLEMENT_CLASS(SelectionBar, ToolBar);

const static wxChar *numbers[] =
{
   wxT("0"), wxT("1"), wxT("2"), wxT("3"), wxT("4"),
   wxT("5"), wxT("6"), wxT("7"), wxT("8"), wxT("9")
};

enum {
   SelectionBarFirstID = 2700,
   OnRateID,
   OnSnapToID,
   //OnMenuId,
   OnStartTimeID,
   OnCenterTimeID,
   OnLengthTimeID,
   OnEndTimeID,
   OnAudioTimeID,
   SelTBFirstButton,
   SelTBMenuID = SelTBFirstButton,
};

BEGIN_EVENT_TABLE(SelectionBar, ToolBar)
   EVT_SIZE(SelectionBar::OnSize)
   EVT_TEXT(OnStartTimeID, SelectionBar::OnChangedTime)
   EVT_TEXT(OnEndTimeID, SelectionBar::OnChangedTime)
   EVT_TEXT(OnLengthTimeID, SelectionBar::OnChangedTime)
   EVT_TEXT(OnCenterTimeID, SelectionBar::OnChangedTime)
   EVT_CHOICE(OnSnapToID, SelectionBar::OnSnapTo)
   EVT_COMBOBOX(OnRateID, SelectionBar::OnRate)
   EVT_TEXT(OnRateID, SelectionBar::OnRate)

   EVT_COMMAND_RANGE( SelTBMenuID,
                      SelTBMenuID,
                      wxEVT_COMMAND_BUTTON_CLICKED,
                      SelectionBar::OnButton )
// EVT_COMMAND( OnMenuId, wxEVT_COMMAND_BUTTON_CLICKED, SelectionBar::OnButton )
   EVT_COMMAND(wxID_ANY, EVT_TIMETEXTCTRL_UPDATED, SelectionBar::OnUpdate)
   EVT_COMMAND(wxID_ANY, EVT_CAPTURE_KEY, SelectionBar::OnCaptureKey)
END_EVENT_TABLE()

SelectionBar::SelectionBar()
: ToolBar(SelectionBarID, _("Selection"), wxT("Selection")),
  mListener(NULL), mRate(0.0), 
  mStart(0.0), mEnd(0.0), mLength(0.0), mCenter(0.0), mAudio(0.0),
  mStartTime(NULL), mEndTime(NULL), mLengthTime(NULL), mCenterTime(NULL), 
  mAudioTime(NULL),
  mStartTitle(NULL), mCenterTitle(NULL), mLengthTitle(NULL), mEndTitle(NULL),
  mDrive1( OnStartTimeID), mDrive2( OnEndTimeID ),
  mSelectionMode(0)
{
   // Make sure we have a valid rate as the NumericTextCtrl()s
   // created in Populate()
   // depend on it.  Otherwise, division-by-zero floating point exceptions
   // will occur.
   // Refer to bug #462 for a scenario where the division-by-zero causes
   // Audacity to fail.
   mRate = (double) gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleRate"),
      AudioIO::GetOptimalSupportedSampleRate());

   // Selection mode of 0 means showing 'start' and 'end' only.
   mSelectionMode = gPrefs->ReadLong(wxT("/SelectionToolbarMode"),  0);
}

SelectionBar::~SelectionBar()
{
}

void SelectionBar::Create(wxWindow * parent)
{
   ToolBar::Create(parent);
}

// There currently is only one clickable AButton
// so we just do what it needs.
void SelectionBar::OnButton(wxCommandEvent & event)
{
   AudacityProject *p = GetActiveProject();
   if (!p) return;

   AButton * pBtn = mButtons[ SelTBMenuID-SelTBFirstButton];// use event.GetId();

   auto cleanup = finally( [&] { pBtn->InteractionOver();}
   );
   wxLogDebug( "Clicked");
   SetFocus();

   wxMenu Menu;
   Menu.AppendRadioItem( 0, _("Start - End") );
   Menu.AppendRadioItem( 1, _("Start - Length") );
   Menu.AppendRadioItem( 2, _("Length - End") );
   Menu.AppendRadioItem( 3, _("Center - Length") );
   Menu.AppendRadioItem( 4, _("Start - Length - End") );
   Menu.AppendRadioItem( 5, _("Start - Center - Length") );
   Menu.AppendRadioItem( 6, _("Start - Center - End") );
   Menu.AppendRadioItem( 7, _("Start - Center - Length - End") );
   Menu.Check( mSelectionMode, true );
   // Pop it up where the mouse is.
   pBtn->PopupMenu(&Menu);//, wxPoint(0, 0));

   // only one radio button should be checked.
   for( int i=0;i<8;i++)
      if( Menu.IsChecked(i))
         SetSelectionMode( i );

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


// Add Radio Button function is not used anymore.
// But maybe we will need it again in the future.
//
// Can't set textcolour of radio buttons.
// so instead if we want to them, we make the text empty and add in a wxStaticText
// and we can set the colour of that.
// Slight regression relative ot Audacity, in that this text is not 
// clickable/active.  You have to click on the actual button.
// And you can't tab between and hear the labels with voice over.
// So VI users should use blend themes (which is the default).
// Should not be a hardship for them, as themes make little difference 
// for them, except Hi-Contrast, which should be used with blend thems
// and a windows theme that is close enough to actually blend.

wxRadioButton * SelectionBar::AddRadioButton( const wxString & Name, 
   int id, std::unique_ptr<wxBoxSizer>& pSizer, long style )
{
   bool bUseNativeRadioButton = theTheme.IsUsingSyestemTextColour();
   wxRadioButton * pBtn;
   // Safenew because the button is being create dinto this window.
   pBtn = safenew wxRadioButton(this, id,bUseNativeRadioButton ? Name : wxT(""),
      wxDefaultPosition, wxDefaultSize, style);
   pBtn->SetName(Name);
   pBtn->SetForegroundColour( theTheme.Colour( clrTrackPanelText ));

   pSizer->Add(pBtn, 0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);
   if( !bUseNativeRadioButton )
   {
      wxStaticText * pText = safenew wxStaticText(this, -1, Name);
      pText->SetForegroundColour( theTheme.Colour( clrTrackPanelText ) );
      pSizer->Add(pText, 0, wxRIGHT | wxALIGN_CENTER_VERTICAL, 5);
   }
   return pBtn;
}

wxStaticText * SelectionBar::AddTitle( const wxString & Title, wxSizer * pSizer ){
   wxStaticText * pTitle = safenew wxStaticText(this, -1,Title );
   pTitle->SetForegroundColour( theTheme.Colour( clrTrackPanelText ) );
   pSizer->Add( pTitle,0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);
   return pTitle;
}


NumericTextCtrl * SelectionBar::AddTime( const wxString Name, int id, wxSizer * pSizer ){
   wxString formatName = mListener ? mListener->AS_GetSelectionFormat() 
      : wxString(wxEmptyString);
   NumericTextCtrl * pCtrl = safenew NumericTextCtrl(
      NumericConverter::TIME, this, id, formatName, 0.0, mRate);
   pCtrl->SetName(Name);
   pCtrl->SetForegroundColour( theTheme.Colour( clrTrackPanelText ) );
   pCtrl->EnableMenu();
   pSizer->Add(pCtrl, 0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);
   return pCtrl;
}

void SelectionBar::Populate()
{
   SetBackgroundColour( theTheme.Colour( clrMedium  ) );
   mStartTime = mEndTime = mLengthTime = mCenterTime = mAudioTime = nullptr;
   // This will be inherited by all children:
   SetFont(wxFont(
#ifdef __WXMAC__
                  12
#else
                  9
#endif
                  ,
                  wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL));

   wxFlexGridSizer *mainSizer;

   /* we don't actually need a control yet, but we want to use its methods
    * to do some look-ups, so we'll have to create one. We can't make the
    * look-ups static because they depend on translations which are done at
    * runtime */

   Add((mainSizer = safenew wxFlexGridSizer(10, 1, 1)), 0, wxALIGN_CENTER_VERTICAL);

   //
   // Top row (mostly labels)
   //

   wxColour clrText =  theTheme.Colour( clrTrackPanelText );
   wxColour clrText2 = *wxBLUE;
   wxStaticText * pProjRate = safenew wxStaticText(this, -1, _("Project Rate (Hz):"),
   // LLL:  On my Ubuntu 7.04 install, the label wraps to two lines
   //       and I could not figure out why.  Thus...hackage.
#if defined(__WXGTK__)
                  wxDefaultPosition, wxSize(110, -1));
#else
                  wxDefaultPosition, wxDefaultSize);
#endif
   pProjRate->SetForegroundColour( clrText );
   mainSizer->Add(pProjRate,0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);
   mainSizer->Add(5, 1);

   AddTitle( _("Snap-To"), mainSizer );
   // Not enough room to say 'Selection Options".  There is a tooltip instead.
   AddTitle( wxT(""), mainSizer );
   mStartTitle = AddTitle( _("Start"), mainSizer );
   mCenterTitle = AddTitle( _("Center"), mainSizer );
   mLengthTitle = AddTitle( _("Length"), mainSizer );
   mEndTitle = AddTitle( _("End"), mainSizer );
   mainSizer->Add(5, 1);
   AddTitle( _("Audio Position"), mainSizer );

   //
   // Middle row (mostly time controls)
   //

   mRateBox = safenew wxComboBox(this, OnRateID,
                             wxT(""),
                             wxDefaultPosition, wxSize(80, -1));
   mRateBox->SetName(_("Project Rate (Hz):"));
   //mRateBox->SetForegroundColour( clrText2 );
   wxTextValidator vld(wxFILTER_INCLUDE_CHAR_LIST);
   vld.SetIncludes(wxArrayString(10, numbers));
   mRateBox->SetValidator(vld);
   mRateBox->SetValue(wxString::Format(wxT("%d"), (int)mRate));
   UpdateRates(); // Must be done _after_ setting value on mRateBox!

   // We need to capture the SetFocus and KillFocus events to set up
   // for keyboard capture.  On Windows and GTK it's easy since the
   // combobox is presented as one control to hook into.
   mRateText = mRateBox;

#if defined(__WXMAC__)
   // The Mac uses a standard wxTextCtrl for the edit portion and that's
   // the control that gets the focus events.  So we have to find the
   // textctrl.
   wxWindowList kids = mRateBox->GetChildren();
   for (unsigned int i = 0; i < kids.GetCount(); i++) {
      wxClassInfo *ci = kids[i]->GetClassInfo();
      if (ci->IsKindOf(CLASSINFO(wxTextCtrl))) {
         mRateText = kids[i];
         break;
      }
   }
#endif

   mRateText->Connect(wxEVT_SET_FOCUS,
                      wxFocusEventHandler(SelectionBar::OnFocus),
                      NULL,
                      this);
   mRateText->Connect(wxEVT_KILL_FOCUS,
                      wxFocusEventHandler(SelectionBar::OnFocus),
                      NULL,
                      this);

   mainSizer->Add(mRateBox, 0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);

   mainSizer->Add(safenew wxStaticLine(this, -1, wxDefaultPosition,
                                   wxSize(1, toolbarSingle),
                                   wxLI_VERTICAL),
                  0,  wxRIGHT, 5);

   mSnapTo = safenew wxChoice(this, OnSnapToID,
                          wxDefaultPosition, wxDefaultSize,
                          SnapManager::GetSnapLabels());
   mainSizer->Add(mSnapTo,
                  0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);
   mSnapTo->SetName(_("Snap To"));
   //mSnapTo->SetForegroundColour( clrText2 );
   mSnapTo->SetSelection(mListener ? mListener->AS_GetSnapTo() : SNAP_OFF);

   mSnapTo->Connect(wxEVT_SET_FOCUS,
                    wxFocusEventHandler(SelectionBar::OnFocus),
                    NULL,
                    this);
   mSnapTo->Connect(wxEVT_KILL_FOCUS,
                    wxFocusEventHandler(SelectionBar::OnFocus),
                    NULL,
                    this);

   AButton *& pBtn = mButtons[ SelTBMenuID - SelTBFirstButton];
   pBtn = ToolBar::MakeButton(this,
      bmpRecoloredUpSmall, bmpRecoloredDownSmall, bmpRecoloredHiliteSmall,
      bmpOptions, bmpOptions, bmpOptionsDisabled, 
      SelTBMenuID,
      wxDefaultPosition,
      false,
      theTheme.ImageSize( bmpRecoloredUpSmall ));

   pBtn->SetLabel("Selection options");
   pBtn->SetToolTip("Selection options");
   mainSizer->Add( pBtn, 0,  wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);

   mStartTime  = AddTime(_("Start"), OnStartTimeID, mainSizer );
   mCenterTime = AddTime(_("Center"), OnCenterTimeID, mainSizer );
   mLengthTime = AddTime(_("Length"), OnLengthTimeID, mainSizer );
   mEndTime    = AddTime(_("End"), OnEndTimeID, mainSizer );

   mainSizer->Add(safenew wxStaticLine(this, -1, wxDefaultPosition,
                                   wxSize(1, toolbarSingle),
                                   wxLI_VERTICAL),
                  0, wxRIGHT, 5);

   mAudioTime = AddTime(_("Audio Position"), OnAudioTimeID, mainSizer );

   // This shows/hides controls.
   // Do this before layout so that we are sized right.
   SetSelectionMode(mSelectionMode);

   mainSizer->Layout();

   RegenerateTooltips();

   Layout();

   SetMinSize( GetSizer()->GetMinSize() );
}

void SelectionBar::UpdatePrefs()
{
   mRate = (double) gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleRate"), AudioIO::GetOptimalSupportedSampleRate());

   wxCommandEvent e;
   e.SetInt(mStartTime->GetFormatIndex());
   OnUpdate(e);

   // Set label to pull in language change
   SetLabel(_("Selection"));

   RegenerateTooltips();

   // Give base class a chance
   ToolBar::UpdatePrefs();
}

void SelectionBar::SetListener(SelectionBarListener *l)
{
   mListener = l;
   SetRate(mListener->AS_GetRate());
   SetSnapTo(mListener->AS_GetSnapTo());
   SetSelectionFormat(mListener->AS_GetSelectionFormat());
};

void SelectionBar::RegenerateTooltips()
{
#if wxUSE_TOOLTIPS
   wxString formatName = mListener ? mListener->AS_GetSelectionFormat() : wxString(wxEmptyString);
   mSnapTo->SetToolTip(wxString::Format(_("Snap Clicks/Selections to %s"), formatName.c_str()));
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
   if( newDriver == OnStartTimeID )
      mStart = mStartTime->GetValue();
   if( newDriver == OnEndTimeID )
      mEnd = mEndTime->GetValue();
   if( newDriver == OnLengthTimeID )
      mLength = mLengthTime->GetValue();
   if( newDriver == OnCenterTimeID )
      mCenter = mCenterTime->GetValue();

   int i = mDrive1 + 4 * mDrive2;
   switch(i){
   case OnStartTimeID + 4 * OnEndTimeID:
      if( mEnd < mStart )
         mEnd = mStart;
   case OnStartTimeID * 4 + OnEndTimeID:
      if( mStart > mEnd )
         mStart = mEnd;
      mLength = mEnd - mStart;
      mCenter = (mStart+mEnd)/2.0;
      break;
   case OnStartTimeID + 4 * OnLengthTimeID:
   case OnStartTimeID * 4 + OnLengthTimeID:
      if( mLength < 0 )
         mLength = 0;
      mEnd = mStart+mLength;
      mCenter = (mStart+mEnd)/2.0;
      break;
   case OnStartTimeID + 4 * OnCenterTimeID:
      if( mCenter < mStart )
         mCenter = mStart;
   case OnStartTimeID * 4 + OnCenterTimeID:
      if( mStart > mCenter )
         mStart = mCenter;
      mEnd = mCenter * 2 - mStart;
      mLength = mStart - mEnd;
      break;
   case OnEndTimeID + 4 * OnLengthTimeID:
   case OnEndTimeID * 4 + OnLengthTimeID:
      if( mLength < 0 )
         mLength = 0;
      mStart = mEnd - mLength;
      mCenter = (mStart+mEnd)/2.0;
      break;
   case OnEndTimeID + 4 * OnCenterTimeID:
      if( mCenter > mEnd )
         mCenter = mEnd;
   case OnEndTimeID * 4 + OnCenterTimeID:
      if( mEnd < mCenter )
         mEnd = mCenter;
      mStart = mCenter * 2.0 - mEnd;
      mLength = mEnd - mStart;
      break;
   case OnLengthTimeID + 4 * OnCenterTimeID:
   case OnLengthTimeID * 4 + OnCenterTimeID:
      if( mLength < 0 )
         mLength = 0;
      mStart = mCenter - mLength/2.0;
      mEnd = mCenter + mLength/2.0;
      break;
   default:
      // The above should cover all legal combinations of two distinct controls.
      wxFAIL_MSG( "Illegal sequence of selection changes");
   }

   // Places the start-end mrkers on the track panel.
   mListener->AS_ModifySelection(mStart, mEnd, done);
}

void SelectionBar::OnChangedTime(wxCommandEvent & event)
{
   ModifySelection(event.GetId(), event.GetInt() != 0);
}

// Called when one of the format drop downs is changed.
void SelectionBar::OnUpdate(wxCommandEvent &evt)
{
   int index = evt.GetInt();
   wxWindow *w = FindFocus();
   NumericTextCtrl ** Ctrls[5] = { &mStartTime, &mEndTime, &mLengthTime, &mCenterTime, &mAudioTime };
   int i;
   int iFocus = -1;
   for(i=0;i<5;i++)
      if( w == *Ctrls[i] )
         iFocus = i;

   evt.Skip(false);

   wxString format;

   // Save format name before recreating the controls so they resize properly
   format = mStartTime->GetBuiltinName(index);
   mListener->AS_SetSelectionFormat(format);

   RegenerateTooltips();

   // ToolBar::ReCreateButtons() will get rid of our sizers and controls
   // so reset pointers first.
   for( i=0;i<5;i++)
      *Ctrls[i]=NULL;

   mRateBox = NULL;
   mRateText = NULL;

   ToolBar::ReCreateButtons();

   ValuesToControls();

   format = mStartTime->GetBuiltinFormat(index);
   for( i=0;i<5;i++)
      (*Ctrls[i])->SetFormatString( format );

   if( iFocus >=0 )
      (*Ctrls[iFocus])->SetFocus();
   Updated();
}

// The two drivers are the numbers of the two controls which drive the other ones.
// The user gets to see which controls are drivers and which driven.
void SelectionBar::SetDrivers( int driver1, int driver2 )
{
   mDrive1 = driver1;
   mDrive2 = driver2;

   wxStaticText ** Titles[4] = { &mStartTitle, &mCenterTitle, &mLengthTitle, &mEndTitle};
   NumericTextCtrl ** Ctrls[4] = { &mStartTime, &mCenterTime, &mLengthTime, &mEndTime};
   wxString Text[4] = { _("Start"), _("Center"), _("Length"),  _("End")  };

   for(int i=0;i<4;i++){
      int id = i + OnStartTimeID;
      if( (id!=mDrive1) && (id!=mDrive2 ) )
         // i18n-hint: This is an indicator that a parameter is set (driven) from other parameters.
         Text[i] += _(" - driven");
      if( *Titles[i] ){
         (*Titles[i])->SetLabelText( Text[i] );
      }
      if( *Ctrls[i] ){
         (*Ctrls[i])->SetName( Text[i] );
      }
   }
}


void SelectionBar::SetSelectionMode(int mode)
{
   mSelectionMode = mode;

   // First decide which two controls drive the others...
   // For example the last option is with all controls shown, and in that mode we 
   // initially have start and end driving.
   int Drive2[] = { OnStartTimeID, OnStartTimeID, OnLengthTimeID, OnCenterTimeID, 
                     OnStartTimeID, OnStartTimeID, OnStartTimeID, OnStartTimeID};
   int Drive1[] = { OnEndTimeID,   OnLengthTimeID, OnEndTimeID, OnLengthTimeID, 
                     OnEndTimeID, OnLengthTimeID, OnEndTimeID, OnEndTimeID};

   SetDrivers( Drive1[mode], Drive2[mode] );

   // Then show/hide the relevant controls.
   ShowHideControls( mode );
}

// Our mode determines which controls are visible.
void SelectionBar::ShowHideControls(int mode)
{
   // These 
   int masks[8]= { 
      9, 5, 12, 6, // 2 items
      13, 7, 11,// 3 items
      15};
   int mask = masks[mode];

   NumericTextCtrl ** Ctrls[4] = { &mStartTime, &mCenterTime, &mLengthTime, &mEndTime};
   wxStaticText ** Titles[4] = { &mStartTitle, &mCenterTitle, &mLengthTitle, &mEndTitle};
   for(int i=0;i<4;i++){
      if( *Ctrls[i]) 
         (*Ctrls[i])->Show( (mask & (1<<i))!=0 );
      if( *Titles[i]) 
         (*Titles[i])->Show( (mask & (1<<i))!=0 );
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

void SelectionBar::SetTimes(double start, double end, double audio)
{
   mStart = start;
   mEnd = end;
   mLength = end-start;
   mCenter = (end+start)/2.0;
   mAudio = audio;

   ValuesToControls();
}

void SelectionBar::SetField(const wxChar *msg, int fieldNum)
{
   if (fieldNum < 0 || fieldNum >= 10)
      return;

   if (mField[fieldNum] != msg) {
      mField[fieldNum] = msg;
      Refresh(false);
   }
}

void SelectionBar::SetSnapTo(int snap)
{
   mSnapTo->SetSelection(snap);
}

void SelectionBar::SetSelectionFormat(const wxString & format)
{
   mStartTime->SetFormatString(mStartTime->GetBuiltinFormat(format));

   wxCommandEvent e;
   e.SetInt(mStartTime->GetFormatIndex());
   OnUpdate(e);
}

void SelectionBar::SetRate(double rate)
{
   if (rate != mRate) {
      // if the rate is actually being changed
      mRate = rate;   // update the stored rate
      mRateBox->SetValue(wxString::Format(wxT("%d"), (int)rate));
      // update the TimeTextCtrls if they exist
      NumericTextCtrl ** Ctrls[5] = { &mStartTime, &mEndTime, &mLengthTime, &mCenterTime, &mAudioTime };
      int i;
      for(i=0;i<5;i++)
         if( *Ctrls[i] )
            (*Ctrls[i])->SetSampleRate( rate );
   }
}

void SelectionBar::OnRate(wxCommandEvent & WXUNUSED(event))
{
   if (mRateBox->GetValue().ToDouble(&mRate) && // is a numeric value
         (mRate != 0.0))
   {
      NumericTextCtrl ** Ctrls[5] = { &mStartTime, &mEndTime, &mLengthTime, &mCenterTime, &mAudioTime };
      int i;
      for(i=0;i<5;i++)
         if( *Ctrls[i] )
            (*Ctrls[i])->SetSampleRate( mRate );
      if (mListener) mListener->AS_SetRate(mRate);
   }
}

void SelectionBar::UpdateRates()
{
   wxString oldValue = mRateBox->GetValue();
   mRateBox->Clear();
   for (int i = 0; i < AudioIO::NumStandardRates; i++) {
      mRateBox->Append(wxString::Format(wxT("%d"), AudioIO::StandardRates[i]));
   }
   mRateBox->SetValue(oldValue);
}

void SelectionBar::OnFocus(wxFocusEvent &event)
{
   if (event.GetEventType() == wxEVT_KILL_FOCUS) {
      AudacityProject::ReleaseKeyboard(this);
   }
   else {
      AudacityProject::CaptureKeyboard(this);
   }

   Refresh(false);
   event.Skip();
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

   // UP/DOWN/LEFT/RIGHT for mRateText
   if (w == mRateText) {
      switch (keyCode)
      {
         case WXK_LEFT:
         case WXK_RIGHT:
         case WXK_UP:
         case WXK_DOWN:
         case WXK_DELETE:
         case WXK_BACK:
            return;
      }
   }

   event.Skip();
}

void SelectionBar::OnSnapTo(wxCommandEvent & WXUNUSED(event))
{
   mListener->AS_SetSnapTo(mSnapTo->GetSelection());
}
