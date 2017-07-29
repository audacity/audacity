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
   RateID,
   SnapToID,
   OnMenuID,

   ChoiceID,

   StartTitleID,
   LengthTitleID,
   CenterTitleID,
   EndTitleID,
   AudioTitleID,

   LeftID,
   CentralNameID,
   RightID,

   StartEndRadioID,
   StartLengthRadioID,
   LengthEndRadioID,
   LengthCenterRadioID,

   SelTBFirstButton,
   SelTBMenuID = SelTBFirstButton,

   id2,
   StartTimeID,
   LengthTimeID,
   CenterTimeID,
   EndTimeID,
   AudioTimeID,
};

BEGIN_EVENT_TABLE(SelectionBar, ToolBar)
   EVT_SIZE(SelectionBar::OnSize)
   EVT_TEXT(StartTimeID, SelectionBar::OnChangedTime)
   EVT_TEXT(LengthTimeID, SelectionBar::OnChangedTime)
   EVT_TEXT(CenterTimeID, SelectionBar::OnChangedTime)
   EVT_TEXT(EndTimeID, SelectionBar::OnChangedTime)
   EVT_CHOICE(SnapToID, SelectionBar::OnSnapTo)
   EVT_CHOICE(ChoiceID, SelectionBar::OnChoice )
   EVT_COMBOBOX(RateID, SelectionBar::OnRate)
   EVT_TEXT(RateID, SelectionBar::OnRate)
   EVT_RADIOBUTTON(StartEndRadioID, SelectionBar::OnFieldChoice )
   EVT_RADIOBUTTON(StartLengthRadioID, SelectionBar::OnFieldChoice )
   EVT_RADIOBUTTON(LengthEndRadioID, SelectionBar::OnFieldChoice )
   EVT_RADIOBUTTON(LengthCenterRadioID, SelectionBar::OnFieldChoice )

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
#ifdef SEL_RADIO_TITLES
  mStartTitle(NULL), mCenterTitle(NULL), mLengthTitle(NULL), mEndTitle(NULL),
  mStartEndProxy(NULL), mStartLengthProxy(NULL), mLengthEndProxy(NULL), mLengthCenterProxy(NULL),
#endif
#ifdef SEL_CHOICE
  mChoice(NULL),
#endif
  mDrive1( StartTimeID), mDrive2( EndTimeID ),
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

#ifdef SEL_BUTTON_TITLES
   mButtonTitles[0]=NULL;
   mButtonTitles[1]=NULL;
   mButtonTitles[2]=NULL; 
   mHyphen[0]=NULL;
   mHyphen[1]=NULL;
   mHyphen[2]=NULL;
#endif

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
   int id, wxSizer *pSizer, long style )
{
   bool bUseNativeRadioButton = theTheme.IsUsingSyestemTextColour();
   wxRadioButton * pBtn;
   // Safenew because the button is being create dinto this window.
   pBtn = safenew wxRadioButton(this, id,bUseNativeRadioButton ? Name : wxT(""),
      wxDefaultPosition, wxDefaultSize, style);
   pBtn->SetName(Name);
   pBtn->SetForegroundColour( theTheme.Colour( clrTrackPanelText ));

   pSizer->Add(pBtn, 0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);
   // Hacky code to return a second optional value via the variable mProxy.
   // If not NULL, we made a NEW proxy label
   mProxy = NULL;
   if( !bUseNativeRadioButton )
   {
      mProxy = safenew wxStaticText(this, -1, Name);
      mProxy->SetForegroundColour( theTheme.Colour( clrTrackPanelText ) );
      pSizer->Add(mProxy, 0, wxRIGHT | wxALIGN_CENTER_VERTICAL, 5);
   }
   return pBtn;
}

wxStaticText * SelectionBar::AddTitle( const wxString & Title, int id, wxSizer * pSizer ){
   wxStaticText * pTitle = safenew wxStaticText(this, id,Title );
   pTitle->SetForegroundColour( theTheme.Colour( clrTrackPanelText ) );
   pSizer->Add( pTitle,0, wxALIGN_CENTER_VERTICAL | wxRIGHT,  (Title.Length() == 1 ) ? 0:5);
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
#ifdef SEL_RADIO_TITLE
   mStartEndProxy = mStartLengthProxy = mLengthEndProxy = mLengthCenterProxy = nullptr;
#endif


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

   // Outer sizer has space top and left.
   // Inner sizers have space on right only.
   // This choice makes for a nice border and internal spacing and places clear responsibility
   // on each sizer as to what spacings it creates.
   Add((mainSizer = safenew wxFlexGridSizer(SIZER_COLS, 1, 1)), 0, wxALIGN_TOP | wxLEFT | wxTOP, 5);

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
   AddVLine( mainSizer );

   AddTitle( _("Snap-To"), -1, mainSizer );
#ifdef OPTIONS_BUTTON
   // Not enough room to say 'Selection Options".  There is a tooltip instead.
   AddTitle( wxT(""), -1, mainSizer );
#endif
   AddVLine( mainSizer );

   AddTitle( _("Audio Position"), -1, mainSizer );
   AddVLine( mainSizer );

   {
#ifdef SEL_RADIO_TITLE
      auto hSizer = std::make_unique<wxBoxSizer>(wxHORIZONTAL);
      (mStartEndRadBtn = AddRadioButton(  _("Start-End"), StartEndRadioID, hSizer.get(), wxRB_GROUP))
         ->SetValue( mSelectionMode == 0 );
      mStartEndProxy = mProxy;
      (mStartLengthRadBtn  = AddRadioButton( _("Start-Length"), StartLengthRadioID, hSizer.get(), 0))
         ->SetValue( mSelectionMode == 1 );
      mStartLengthProxy = mProxy;
      (mLengthEndRadBtn    = AddRadioButton( _("Length-End"), LengthEndRadioID, hSizer.get(), 0))
         ->SetValue( mSelectionMode == 2 );
      mLengthEndProxy = mProxy;
      (mLengthCenterRadBtn = AddRadioButton( _("Length-Center"), LengthCenterRadioID, hSizer.get(), 0))
         ->SetValue( mSelectionMode == 3 );
      mLengthCenterProxy = mProxy;
      mainSizer->Add(hSizer.release(), 0, wxALIGN_TOP| wxRIGHT, 0);
#endif

#ifdef SEL_BUTTON_TITLES
      auto vSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
      auto hSizer = std::make_unique<wxBoxSizer>(wxHORIZONTAL);
      mButtonTitles[0] = AddTitle( "< ", LeftID, hSizer.get() );
      mButtonTitles[1] = AddTitle( "Start - End ", CentralNameID, hSizer.get() );
      mButtonTitles[2] = AddTitle( " >", RightID, hSizer.get() );
      mButtonTitles[0]->Bind( wxEVT_LEFT_DOWN,&SelectionBar::OnModeDecClicked,this );
      mButtonTitles[1]->Bind( wxEVT_LEFT_DOWN,&SelectionBar::OnChooserTitleClicked, this );
      mButtonTitles[2]->Bind( wxEVT_LEFT_DOWN,&SelectionBar::OnModeIncClicked,this );
      vSizer->Add( hSizer.release(), 0, wxALIGN_CENTER, 0);
      mainSizer->Add(vSizer.release(), 0, wxALIGN_CENTER, 0 );
#endif

#ifdef SEL_CHOICE
   const wxString choices[4] = {
      _("Start and End of Selection"),
      _("Start and Length of Selection"),
      _("Length and End of Selection"),
      _("Length and Center of Selection"),
   };
   mChoice = safenew wxChoice
      (this, ChoiceID, wxDefaultPosition, wxDefaultSize, 4, choices,
       0, wxDefaultValidator, "");
   mChoice->SetName(wxT("\a"));     // stop Jaws screen reader using nearby text for name when name is empty
   mChoice->SetSelection(0);
#ifdef __WXGTK__
   // Combo boxes are taller on Linux, and if we don't do the following, the selection toolbar will
   // be three units high.
   wxSize sz = mChoice->GetBestSize();
   sz.SetHeight( sz.y-4);
   mChoice->SetMinSize( sz );
#endif
   mainSizer->Add(mChoice, 0, wxALIGN_TOP | wxEXPAND | wxRIGHT, 6);
#endif

   }

#ifdef SEL_PLAIN_TITLES
   mStartTitle = AddTitle( _("Start"), mainSizer );
   mLengthTitle = AddTitle( _("Length"), mainSizer );
   mCenterTitle = AddTitle( _("Center"), mainSizer );
   mEndTitle = AddTitle( _("End"), mainSizer );

   mStartTitle->Bind( wxEVT_LEFT_DOWN,&SelectionBar::OnStartTitleClicked,this );
   mLengthTitle->Bind( wxEVT_LEFT_DOWN,&SelectionBar::OnLengthTitleClicked,this );
   mCenterTitle->Bind( wxEVT_LEFT_DOWN,&SelectionBar::OnCenterTitleClicked,this );
   mEndTitle->Bind( wxEVT_LEFT_DOWN,&SelectionBar::OnEndTitleClicked,this );
#endif

   //
   // Botton row, (mostly time controls)
   //

   mRateBox = safenew wxComboBox(this, RateID,
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

#ifdef __WXGTK__
   // Combo boxes are taller on Linux, and if we don't do the following, the selection toolbar will
   // be three units high.
   wxSize sz = mRateBox->GetBestSize();
   sz.SetHeight( sz.y-4);
   mRateBox->SetMinSize( sz );
#endif

   mainSizer->Add(mRateBox, 0, wxALIGN_TOP | wxRIGHT, 5);
   AddVLine( mainSizer );

   mSnapTo = safenew wxChoice(this, SnapToID,
                          wxDefaultPosition, wxDefaultSize,
                          SnapManager::GetSnapLabels());

#ifdef __WXGTK__
   // Combo boxes are taller on Linux, and if we don't do the following, the selection toolbar will
   // be three units high.
   sz = mSnapTo->GetBestSize();
   sz.SetHeight( sz.y-4);
   mSnapTo->SetMinSize( sz );
#endif

   mainSizer->Add(mSnapTo,
                  0, wxALIGN_TOP | wxRIGHT, 5);
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

#ifdef OPTION_BUTTON
   // Old code which placed a button from which to select options.
   // Retained in case we want a button for selection-toolbar options at a future date.
   AButton *& pBtn = mButtons[ SelTBMenuID - SelTBFirstButton];
   pBtn = ToolBar::MakeButton(this,
      bmpRecoloredUpSmall, bmpRecoloredDownSmall, bmpRecoloredHiliteSmall,
      bmpOptions, bmpOptions, bmpOptionsDisabled, 
      SelTBMenuID,
      wxDefaultPosition,
      false,
      theTheme.ImageSize( bmpRecoloredUpSmall ));

   pBtn->SetLabel(_("Selection options"));
   pBtn->SetToolTip(_("Selection options"));
   //pBtn->Disable();
   mainSizer->Add( pBtn, 0,  wxALIGN_TOP | wxRIGHT, 5);
#endif 
   AddVLine( mainSizer );

   mAudioTime = AddTime(_("Audio Position"), AudioTimeID, mainSizer );
   // This vertical line is NOT just for decoration!
   // It works around a wxWidgets-on-Windows RadioButton bug, where tabbing
   // into the radiobutton group jumps to selecting the first item in the 
   // group even if some other item had been selected.
   // It is an important bug to work around for sceen reader users, who use TAB 
   // a lot in navigation.
   // More about the bug here:
   // https://forums.wxwidgets.org/viewtopic.php?t=41120
   AddVLine( mainSizer );

   {
      auto hSizer = std::make_unique<wxBoxSizer>(wxHORIZONTAL);

      mStartTime  = AddTime(_("Start"), StartTimeID, hSizer.get() );
#ifdef SEL_BUTTON_TITLES
      mHyphen[0] = AddTitle( "-", -1, hSizer.get() );
#endif
      mLengthTime = AddTime(_("Length"), LengthTimeID, hSizer.get() );
#ifdef SEL_BUTTON_TITLES
      mHyphen[1] = AddTitle( "-", -1, hSizer.get() );
#endif
      mCenterTime = AddTime(_("Center"), CenterTimeID, hSizer.get() );
#ifdef SEL_BUTTON_TITLES
      mHyphen[2] = AddTitle( "-", -1, hSizer.get() );
#endif
      mEndTime    = AddTime(_("End"), EndTimeID, hSizer.get() );
      mainSizer->Add(hSizer.release(), 0, wxALIGN_TOP | wxRIGHT, 0);

#ifdef SEL_RADIO_TITLES
      // Put choice of what fields to show immediately before the fields.
      mStartEndRadBtn->MoveBeforeInTabOrder( mStartTime );
      mStartLengthRadBtn->MoveAfterInTabOrder( mStartEndRadBtn );
      mLengthEndRadBtn->MoveAfterInTabOrder( mStartLengthRadBtn );
      mLengthCenterRadBtn->MoveAfterInTabOrder( mLengthEndRadBtn );
#endif
   }

#ifdef SEL_CHOICE
   mChoice->MoveBeforeInTabOrder( mStartTime );
#endif
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
   if( newDriver == StartTimeID )
      mStart = mStartTime->GetValue();
   if( newDriver == EndTimeID )
      mEnd = mEndTime->GetValue();
   if( newDriver == LengthTimeID )
      mLength = mLengthTime->GetValue();
   if( newDriver == CenterTimeID )
      mCenter = mCenterTime->GetValue();

   int i = mDrive1 + 4 * mDrive2;
   switch(i){
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
      if( mLength < 0 )
         mLength = 0;
      mEnd = mStart+mLength;
      mCenter = (mStart+mEnd)/2.0;
      break;
   case StartTimeID + 4 * CenterTimeID:
      if( mCenter < mStart )
         mCenter = mStart;
   case StartTimeID * 4 + CenterTimeID:
      if( mStart > mCenter )
         mStart = mCenter;
      mEnd = mCenter * 2 - mStart;
      mLength = mStart - mEnd;
      break;
   case EndTimeID + 4 * LengthTimeID:
   case EndTimeID * 4 + LengthTimeID:
      if( mLength < 0 )
         mLength = 0;
      mStart = mEnd - mLength;
      mCenter = (mStart+mEnd)/2.0;
      break;
   case EndTimeID + 4 * CenterTimeID:
      if( mCenter > mEnd )
         mCenter = mEnd;
   case EndTimeID * 4 + CenterTimeID:
      if( mEnd < mCenter )
         mEnd = mCenter;
      mStart = mCenter * 2.0 - mEnd;
      mLength = mEnd - mStart;
      break;
   case LengthTimeID + 4 * CenterTimeID:
   case LengthTimeID * 4 + CenterTimeID:
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


void SelectionBar::OnTitleClicked(int newDriver)
{
   // Ensure newDriver is the most recent driver.
   if( newDriver != mDrive2 )
      SetDrivers( mDrive2, newDriver);
}

// These functions give the IDs of the associated control, NOT the ID of the title.
void SelectionBar::OnStartTitleClicked(wxMouseEvent & event){ OnTitleClicked( StartTimeID );};
void SelectionBar::OnLengthTitleClicked(wxMouseEvent & event){ OnTitleClicked( LengthTimeID );};
void SelectionBar::OnCenterTitleClicked(wxMouseEvent & event){ OnTitleClicked( CenterTimeID );};
void SelectionBar::OnEndTitleClicked(wxMouseEvent & event){ OnTitleClicked( EndTimeID );};

void SelectionBar::OnModeDecClicked(wxMouseEvent & event){
   SetSelectionMode( (mSelectionMode +3)%4 );
   SelectionModeUpdated();
}

void SelectionBar::OnModeIncClicked(wxMouseEvent & event){
   SetSelectionMode( (mSelectionMode +1)%4 );
   SelectionModeUpdated();
}

void SelectionBar::OnChooserTitleClicked(wxMouseEvent & event){
   wxCommandEvent evt;
   OnButton( evt );
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

#ifdef SEL_PLAIN_TITLES
   wxStaticText ** Titles[4] = { &mStartTitle, &mCenterTitle, &mLengthTitle, &mEndTitle};
#endif
   NumericTextCtrl ** Ctrls[4] = { &mStartTime, &mCenterTime, &mLengthTime, &mEndTime};
   wxString Text[4] = { _("Start"), _("Center"), _("Length"),  _("End")  };

   for(int i=0;i<4;i++){
      int id = i + StartTimeID;
      int fixed = (( id == mDrive2 )?mDrive1:mDrive2)-StartTimeID;

      wxString Temp = Text[i];
      // i18n-hint: %s is replaced e.g by 'Length', to indicate that it will be calculated from other parameters.
      wxString Format = ( (id!=mDrive1) && (id!=mDrive2 ) ) ? _("%s - driven") : "%s";
      wxString Title= wxString::Format( Format, Temp );
      // i18n-hint: %s1 is replaced e.g by 'Length', %s2 e.g by 'Center'.
      wxString VoiceOverText = wxString::Format(_("Selection %s.  %s won't change."), Temp, Text[fixed]);
      // i18n-hint: %s is replaced e.g by 'Length'.  This is a tooltip on a numerical control.
      //wxString Tooltip = wxString::Format( _(" With %s fixed.  (Use context menu to change format.) "),  Text[fixed] );
#ifdef SEL_PLAIN_TITLES
      if( *Titles[i] ){
         (*Titles[i])->SetLabelText( Title );
      }
#endif
      if( *Ctrls[i] ){
         (*Ctrls[i])->SetName( Temp );
         //(*Ctrls[i])->SetToolTip( Tooltip );
      }
   }
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
   SetFocus();

   wxMenu Menu;
   Menu.AppendRadioItem( 0, _("Start - End") );
   Menu.AppendRadioItem( 1, _("Start - Length") );
   Menu.AppendRadioItem( 2, _("Length - End") );
   Menu.AppendRadioItem( 3, _("Length - Center") );
#if 0
   // These four options were found to be too confusing.
   Menu.AppendRadioItem( 4, _("Start - Length - End") );
   Menu.AppendRadioItem( 5, _("Start - Center - Length") );
   Menu.AppendRadioItem( 6, _("Start - Center - End") );
   Menu.AppendRadioItem( 7, _("Start - Length - Center - End") );
#endif
   Menu.Check( mSelectionMode, true );
   // Pop it up where the mouse is.
   pBtn->PopupMenu(&Menu);//, wxPoint(0, 0));

   // only one radio button should be checked.
   for( int i=0;i<4;i++)
      if( Menu.IsChecked(i))
         SetSelectionMode( i );

   SelectionModeUpdated();
}

void SelectionBar::OnFieldChoice(wxCommandEvent &event)
{
   int id = event.GetId();
   SetSelectionMode( id - StartEndRadioID );
   SelectionModeUpdated();
}

void SelectionBar::OnChoice(wxCommandEvent & WXUNUSED(event))
{
   int mode = mChoice->GetSelection();
   SetSelectionMode( mode );
   SelectionModeUpdated();
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


void SelectionBar::SetSelectionMode(int mode)
{
#if defined(SEL_BUTTON_TITLES) || defined( SEL_CHOICE)
   // With SEL_BUTTON_TITLES only modes 0 to 3 are supported,
   // so fix up a mode that could have come from the config.
   const int maxMode = 3;
#else 
   const int maxMode = 7;
#endif

   if( mode > maxMode )
      mode = 0;
   if( mode < 0 )
      mode = 0;
   mSelectionMode = mode;

   int id = mode + StartEndRadioID;

#ifdef SEL_RADIO_TITLES
   if( mStartEndProxy == NULL ){
      // The line breaks are a little funny in order that the i18n hints occur i the right place in 
      // the .pot file
      mStartEndRadBtn->SetLabelText(     (id == StartEndRadioID) ?      _("Start - End") : 
      // i18n-hint: S-E is an abbreviation of Start-End
         _("S-E") );
      mStartLengthRadBtn->SetLabelText(  (id == StartLengthRadioID) ?   _("Start - Length") : 
      // i18n-hint: S-L is an abbreviation of Start-Length
         _("S-L") );
      mLengthEndRadBtn->SetLabelText(    (id == LengthEndRadioID) ?     _("Length - End") : 
      // i18n-hint: L-E is an abbreviation of Length-End
         _("L-E") );
      mLengthCenterRadBtn->SetLabelText( (id == LengthCenterRadioID) ?  _("Length - Center") : 
      // i18n-hint: L-C is an abbreviation of Length-Center
         _("L-C") );
   }
   else
   {
      mStartEndProxy->SetLabelText(     (id == StartEndRadioID) ?      _("Start - End") : _("S-E") );
      mStartLengthProxy->SetLabelText(  (id == StartLengthRadioID) ?   _("Start - Length") : _("S-L") );
      mLengthEndProxy->SetLabelText(    (id == LengthEndRadioID) ?     _("Length - End") : _("L-E") );
      mLengthCenterProxy->SetLabelText( (id == LengthCenterRadioID) ?  _("Length - Center") : _("L-C") );
   }

   mStartEndRadBtn->SetToolTip(     (id != StartEndRadioID) ?      _("Show start time and end time") : "" );
   mStartLengthRadBtn->SetToolTip(  (id != StartLengthRadioID) ?   _("Show start time and length") : "" );
   mLengthEndRadBtn->SetToolTip(    (id != LengthEndRadioID) ?     _("Show length and end time") : "" );
   mLengthCenterRadBtn->SetToolTip( (id != LengthCenterRadioID) ?  _("Show length and center") : "" );

   mStartEndRadBtn->SetValue(     id == StartEndRadioID     );
   mStartLengthRadBtn->SetValue(  id == StartLengthRadioID  );
   mLengthEndRadBtn->SetValue(    id == LengthEndRadioID    );
   mLengthCenterRadBtn->SetValue( id == LengthCenterRadioID );
#endif

#ifdef SEL_BUTTON_TITLES
   // Not translated.  This is old experiemental code that is probably on the way out.
   wxString CenterNames[] = { "       Start  -  End        ", "       Start  -  Length   ", "   Length  -  End        ", "   Length  -  Center   " };
   mButtonTitles[1]->SetLabel( CenterNames[mode] );
#endif
#ifdef SEL_CHOICE
   mChoice->SetSelection( mode ); 
#endif

   // First decide which two controls drive the others...
   // For example the last option is with all controls shown, and in that mode we 
   // initially have start and end driving.
   int Drive2[] = { StartTimeID, StartTimeID,  LengthTimeID, LengthTimeID, 
                    StartTimeID, StartTimeID,  StartTimeID,  StartTimeID};
   int Drive1[] = { EndTimeID,   LengthTimeID, EndTimeID,    CenterTimeID, 
                    EndTimeID,   LengthTimeID, EndTimeID,    EndTimeID};

   SetDrivers( Drive1[mode], Drive2[mode] );
#ifdef SEL_BUTTON_TITLES
   // Show just the hyphen after the first of the items.
   for(int i=0;i<3;i++){
      mHyphen[i]->SetLabel( ((i+StartTimeID) == Drive2[mode]) ? "-  " : "" );
   }
#endif
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

#ifdef SEL_PLAIN_TITLES
   wxStaticText    ** Titles[4] = { &mStartTitle, &mCenterTitle, &mLengthTitle, &mEndTitle};
#endif
   NumericTextCtrl ** Ctrls[4]  = { &mStartTime,  &mCenterTime,  &mLengthTime,  &mEndTime};
   for(int i=0;i<4;i++){
      if( *Ctrls[i]) 
         (*Ctrls[i])->Show( (mask & (1<<i))!=0 );
#ifdef SEL_PLAIN_TITLES
      if( *Titles[i]) 
         (*Titles[i])->Show( (mask & (1<<i))!=0 );
#endif
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
