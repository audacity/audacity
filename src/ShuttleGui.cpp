/**********************************************************************

  Audacity: A Digital Audio Editor

  ShuttleGui.cpp

  James Crook

  Audacity is free software.
  This file is licensed under the wxWidgets license, see License.txt

**********************************************************************//**

\file ShuttleGui.cpp
\brief Implements ShuttleGui, ShuttleGuiBase and InvisiblePanel.

*//***************************************************************//**

\class ShuttleGui
\brief
  Derived from ShuttleGuiBase, an Audacity specific class for shuttling
  data to and from GUI.

  ShuttleGui extends the idea of the data Shuttle class to include creation
  of dialog controls.  As part of this it provides an interface to sizers
  that leads to shorter more readable code.

  It also allows the code that is used to create dialogs to be reused
  to shuttle information in and out.

  Most of the ShuttleGui functions are actually defined in
  ShuttleGuiBase.
     - wxWidgets widgets are dealt with by ShuttleGuiBase.
     - Audacity specific widgets are dealt with by ShuttleGui

  There is documentation on how to use this class in \ref ShuttleSystem

*//***************************************************************//**

\class ShuttleGuiBase
\brief Base class for shuttling data to and from a GUI.

see also ShuttleGui

Use the:
  - \p Start / \p End methods for containers, like two-column-layout.
  - \p Add methods if you are only interested in creating the controls.
  - \p Tie methods if you also want to exchange data using ShuttleGui.

The code in this file is fairly repetitive.  We are dealing with
  - Many different types of Widget.
  - Creation / Reading / Writing / Exporting / Importing
  - int, float, string variants (for example of TextCtrl contents).

A technique used to reduce the size of the \p Tie functions is to
have one generic \p Tie function that uses WrappedType for its
data type.  Type specific \p Tie functions themselves call the generic
variant.

A second technique used to reduce the size of \p Tie functions
only comes into play for two-step \p Tie functions.  (A two step
\p Tie function is one that transfers data between the registry
and the GUI via an intermediate temporary variable). In the two
step style, a function ShuttleGuiBase::DoStep() determines which
transfers in the function are to be done, reducing repetitive
if-then-else's.

Although unusual, these two techniques make the code easier to
add to and much easier to check for correctness.  The alternative
'more obvious' code that just repeats code as needed is
considerably longer.

You would rarely use ShuttleGuiBase directly, instead you'd use
ShuttleGui.

There is DOxygen documentation on how to use the ShuttleGui
class in \ref ShuttleSystem .

*//***************************************************************//**

\class InvisiblePanel
\brief An InvisiblePanel is a panel which does not repaint its
own background.

It is used (a) To group together widgets which need to be refreshed
together.  A single refresh of the panel causes all the subwindows to
refresh.  (b) as a base class for some flicker-free classes for which
the backgorund is never repainted.

JKC: InvisiblePanel will probably be replaced in time by a mechanism
for registering for changes.

*//******************************************************************/


#include "Audacity.h"
#include "ShuttleGui.h"

#include "MemoryX.h"
#include <wx/wx.h>
#include <wx/wxprec.h>
#include <wx/listctrl.h>
#include <wx/notebook.h>
#include <wx/treectrl.h>
#include <wx/spinctrl.h>
#include "Internat.h"
#include "Experimental.h"
#include "Shuttle.h"
#include "WrappedType.h"
#include "widgets/wxPanelWrapper.h"

ShuttleGuiBase::ShuttleGuiBase(wxWindow * pParent, teShuttleMode ShuttleMode )
{
   wxASSERT( (pParent != NULL ) || ( ShuttleMode != eIsCreating));

   mpParent = pParent;
   mShuttleMode = ShuttleMode;
   mpDlg = pParent;
   Init();
}

ShuttleGuiBase::~ShuttleGuiBase()
{
}

void ShuttleGuiBase::Init()
{
   mpShuttle = NULL;
   mpSizer = NULL;
   mpWind = NULL;
   mpSubSizer = NULL;

   mSettingName = wxT("");
   mRadioCount = -1;

   miBorder = 5;
   miStyle = 0;
   miProp=0;
   miPropSetByUser=-1;
   miSizerProp=0;
   mSizerDepth=-1;

   miIdSetByUser = -1;
   miId = -1;
   miIdNext = 3000;

   miNoMatchSelector = 0;

   if( mShuttleMode != eIsCreating )
      return;

   mpSizer = mpParent->GetSizer();

#if 0
   if( mpSizer == NULL )
   {
      wxWindow * pGrandParent = mpParent->GetParent();
      if( pGrandParent )
      {
         mpSizer = pGrandParent->GetSizer();
      }
   }
#endif

   if( !mpSizer )
   {
      mpParent->SetSizer(mpSizer = safenew wxBoxSizer(wxVERTICAL));
   }
   PushSizer();
   mpSizer->SetMinSize(250,100);
}

void ShuttleGuiBase::EnableCtrl( bool bEnable )
{
   if( mShuttleMode != eIsCreating )
      return;
   mpLastWind->Enable( bEnable );
}

/// Used to modify an already placed Window.
void ShuttleGuiBase::SetSizeHints( int minX, int minY )
{
   if( mShuttleMode != eIsCreating )
      return;
   mpLastWind->SetSizeHints( minX, minY );
}


/// Used to modify an already placed FlexGridSizer to make a column stretchy.
void ShuttleGuiBase::SetStretchyCol( int i )
{
   if( mShuttleMode != eIsCreating )
      return;
   wxFlexGridSizer *pSizer = wxDynamicCast(mpSizer, wxFlexGridSizer);
   wxASSERT( pSizer );
   pSizer->AddGrowableCol( i, 1 );
}

/// Used to modify an already placed FlexGridSizer to make a row stretchy.
void ShuttleGuiBase::SetStretchyRow( int i )
{
   if( mShuttleMode != eIsCreating )
      return;
   wxFlexGridSizer *pSizer = wxDynamicCast(mpSizer, wxFlexGridSizer);
   wxASSERT( pSizer );
   pSizer->AddGrowableRow( i, 1 );
}


//---- Add Functions.

/// Right aligned text string.
void ShuttleGuiBase::AddPrompt(const wxString &Prompt)
{
   if( Prompt.IsEmpty() )
      return;
   if( mShuttleMode != eIsCreating )
      return;
   miProp=1;
   mpWind = safenew wxStaticText(GetParent(), -1, Prompt, wxDefaultPosition, wxDefaultSize,
      Style( wxALIGN_RIGHT ));
   mpWind->SetName(wxStripMenuCodes(Prompt)); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
   UpdateSizersCore( false, wxALL | wxALIGN_CENTRE_VERTICAL );
}

/// Left aligned text string.
void ShuttleGuiBase::AddUnits(const wxString &Prompt)
{
   if( Prompt.IsEmpty() )
      return;
   if( mShuttleMode != eIsCreating )
      return;
   miProp=1;
   mpWind = safenew wxStaticText(GetParent(), -1, Prompt, wxDefaultPosition, wxDefaultSize,
      Style( wxALIGN_LEFT ));
   mpWind->SetName(Prompt); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
   UpdateSizersCore( false, wxALL | wxALIGN_CENTRE_VERTICAL );
}

/// Centred text string.
void ShuttleGuiBase::AddTitle(const wxString &Prompt)
{
   if( Prompt.IsEmpty() )
      return;
   if( mShuttleMode != eIsCreating )
      return;
   mpWind = safenew wxStaticText(GetParent(), -1, Prompt, wxDefaultPosition, wxDefaultSize,
      Style( wxALIGN_CENTRE ));
   mpWind->SetName(Prompt); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
   UpdateSizers();
}

/// Very generic 'Add' function.  We can add anything we like.
/// Useful for unique controls
wxWindow * ShuttleGuiBase::AddWindow(wxWindow * pWindow, int Flags )
{
   if( mShuttleMode != eIsCreating )
      return pWindow;
   mpWind = pWindow;
   SetProportions( 0 );
   UpdateSizersCore(false, Flags);
   return pWindow;
}

wxCheckBox * ShuttleGuiBase::AddCheckBox( const wxString &Prompt, const wxString &Selected)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxCheckBox);
   wxCheckBox * pCheckBox;
   miProp=0;
   mpWind = pCheckBox = safenew wxCheckBox(GetParent(), miId, Prompt, wxDefaultPosition, wxDefaultSize,
      Style( 0 ));
   pCheckBox->SetValue(Selected == wxT("true"));
   pCheckBox->SetName(wxStripMenuCodes(Prompt));
   UpdateSizers();
   return pCheckBox;
}

/// For a consistant two-column layout we want labels on the left and
/// controls on the right.  CheckBoxes break that rule, so we fake it by
/// placing a static text label and then a tick box with an empty label.
wxCheckBox * ShuttleGuiBase::AddCheckBoxOnRight( const wxString &Prompt, const wxString &Selected)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxCheckBox);
   wxCheckBox * pCheckBox;
   miProp=0;
   AddPrompt( Prompt );
   mpWind = pCheckBox = safenew wxCheckBox(GetParent(), miId, wxT(""), wxDefaultPosition, wxDefaultSize,
      Style( 0 ));
   pCheckBox->SetValue(Selected==wxT("true"));
   pCheckBox->SetName(wxStripMenuCodes(Prompt));
   UpdateSizers();
   return pCheckBox;
}

wxButton * ShuttleGuiBase::AddButton(const wxString &Text, int PositionFlags)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxButton);
   wxButton * pBtn;
   mpWind = pBtn = safenew wxButton(GetParent(), miId, Text, wxDefaultPosition, wxDefaultSize,
      Style( 0 ) );
   mpWind->SetName(wxStripMenuCodes(Text));
   miProp=0;
   UpdateSizersCore(false, PositionFlags | wxALL);
   return pBtn;
}

wxBitmapButton * ShuttleGuiBase::AddBitmapButton(const wxBitmap &Bitmap, int PositionFlags)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxBitmapButton);
   wxBitmapButton * pBtn;
   mpWind = pBtn = safenew wxBitmapButton(GetParent(), miId, Bitmap,
      wxDefaultPosition, wxDefaultSize, Style( wxNO_BORDER ) );
   pBtn->SetBackgroundColour(
      wxColour( 246,246,243));
//      wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE));
   miProp=0;
   UpdateSizersCore(false, PositionFlags | wxALL);
   return pBtn;
}

wxChoice * ShuttleGuiBase::AddChoice( const wxString &Prompt, const wxString &Selected, const wxArrayString * pChoices )
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxChoice);
   wxChoice * pChoice;
   miProp=0;

   AddPrompt( Prompt );
   mpWind = pChoice = safenew wxChoice(
      GetParent(),
      miId,
      wxDefaultPosition,
      wxDefaultSize,
      *pChoices,
      Style( 0 ) );

   pChoice->SetSizeHints( 180,-1);// Use -1 for 'default size' - Platform specific.
   pChoice->SetName(wxStripMenuCodes(Prompt));
   pChoice->SetStringSelection( Selected );

   UpdateSizers();
   return pChoice;
}

void ShuttleGuiBase::AddFixedText(const wxString &Str, bool bCenter)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return;
   mpWind = safenew wxStaticText(GetParent(), miId, Str, wxDefaultPosition, wxDefaultSize,
      Style( wxALIGN_LEFT ));
   mpWind->SetName(wxStripMenuCodes(Str)); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
   if( bCenter )
   {
      miProp=1;
      UpdateSizersC();
   }
   else
      UpdateSizers();
}

wxStaticText * ShuttleGuiBase::AddVariableText(const wxString &Str, bool bCenter, int PositionFlags)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxStaticText);

   wxStaticText *pStatic;
   mpWind = pStatic = safenew wxStaticText(GetParent(), miId, Str, wxDefaultPosition, wxDefaultSize,
      Style( wxALIGN_LEFT ));
   mpWind->SetName(wxStripMenuCodes(Str)); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
   if( bCenter )
   {
      miProp=1;
      if( PositionFlags )
         UpdateSizersCore( false, PositionFlags );
      else
         UpdateSizersC();
   }
   else
      if( PositionFlags )
         UpdateSizersCore( false, PositionFlags );
      else
         UpdateSizers();
   return pStatic;
}

wxComboBox * ShuttleGuiBase::AddCombo( const wxString &Prompt, const wxString &Selected,const wxArrayString * pChoices, long style )
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxComboBox);
   wxComboBox * pCombo;
   miProp=0;

   int n = pChoices->GetCount();
   if( n>50 ) n=50;
   int i;
   wxString Choices[50];
   for(i=0;i<n;i++)
   {
      Choices[i] = (*pChoices)[i];
   }

   AddPrompt( Prompt );

   mpWind = pCombo = safenew wxComboBox(GetParent(), miId, Selected, wxDefaultPosition, wxDefaultSize,
      n, Choices, Style( style ));
   mpWind->SetName(wxStripMenuCodes(Prompt));

   UpdateSizers();
   return pCombo;
}


wxRadioButton * ShuttleGuiBase::AddRadioButton(const wxString &Prompt)
{
   /// \todo This function and the next one, suitably adapted, could be
   /// used by TieRadioButton.
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxRadioButton);
   wxRadioButton * pRad;
   mpWind = pRad = safenew wxRadioButton(GetParent(), miId, Prompt,
      wxDefaultPosition, wxDefaultSize, Style( wxRB_GROUP ) );
   mpWind->SetName(wxStripMenuCodes(Prompt));
   pRad->SetValue(true );
   UpdateSizers();
   return pRad;
}

wxRadioButton * ShuttleGuiBase::AddRadioButtonToGroup(const wxString &Prompt)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxRadioButton);
   wxRadioButton * pRad;
   mpWind = pRad = safenew wxRadioButton(GetParent(), miId, Prompt,
      wxDefaultPosition, wxDefaultSize, Style( 0 ) );
   mpWind->SetName(wxStripMenuCodes(Prompt));
   UpdateSizers();
   return pRad;
}

wxSlider * ShuttleGuiBase::AddSlider(const wxString &Prompt, int pos, int Max, int Min)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxSlider);
   AddPrompt( Prompt );
   wxSlider * pSlider;
   mpWind = pSlider = safenew wxSlider(GetParent(), miId,
      pos, Min, Max,
      wxDefaultPosition, wxDefaultSize,
      Style( wxSL_HORIZONTAL | wxSL_LABELS | wxSL_AUTOTICKS )
      );
   mpWind->SetName(wxStripMenuCodes(Prompt));
   miProp=1;
   UpdateSizers();
   return pSlider;
}

wxSpinCtrl * ShuttleGuiBase::AddSpinCtrl(const wxString &Prompt, int Value, int Max, int Min)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxSpinCtrl);
   AddPrompt( Prompt );
   wxSpinCtrl * pSpinCtrl;
   mpWind = pSpinCtrl = safenew wxSpinCtrl(GetParent(), miId,
      wxEmptyString,
      wxDefaultPosition, wxDefaultSize,
      Style( wxSP_VERTICAL | wxSP_ARROW_KEYS ),
      Min, Max, Value
      );
   mpWind->SetName(wxStripMenuCodes(Prompt));
   miProp=1;
   UpdateSizers();
   return pSpinCtrl;
}

wxTextCtrl * ShuttleGuiBase::AddTextBox(const wxString &Caption, const wxString &Value, const int nChars)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxTextCtrl);
   wxTextCtrl * pTextCtrl;
   wxSize Size(wxDefaultSize);
   if( nChars > 0 )
   {
      Size.SetWidth( nChars *5 );
   }
   AddPrompt( Caption );
   miProp=0;

#ifdef EXPERIMENTAL_RIGHT_ALIGNED_TEXTBOXES
   long flags = wxTE_RIGHT;
#else
   long flags = wxTE_LEFT;
#endif

   mpWind = pTextCtrl = safenew wxTextCtrl(GetParent(), miId, Value,
      wxDefaultPosition, Size, Style( flags ));
   mpWind->SetName(wxStripMenuCodes(Caption));
   UpdateSizers();
   return pTextCtrl;
}

wxTextCtrl * ShuttleGuiBase::AddNumericTextBox(const wxString &Caption, const wxString &Value, const int nChars)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxTextCtrl);
   wxTextCtrl * pTextCtrl;
   wxSize Size(wxDefaultSize);
   if( nChars > 0 )
   {
      Size.SetWidth( nChars *5 );
   }
   AddPrompt( Caption );
   miProp=0;

#ifdef EXPERIMENTAL_RIGHT_ALIGNED_TEXTBOXES
   long flags = wxTE_RIGHT;
#else
   long flags = wxTE_LEFT;
#endif

   wxTextValidator Validator(wxFILTER_NUMERIC);
   mpWind = pTextCtrl = safenew wxTextCtrl(GetParent(), miId, Value,
      wxDefaultPosition, Size, Style( flags ),
      Validator // It's OK to pass this.  It will be cloned.
      );
   mpWind->SetName(wxStripMenuCodes(Caption));
   UpdateSizers();
   return pTextCtrl;
}

/// Multiline text box that grows.
wxTextCtrl * ShuttleGuiBase::AddTextWindow(const wxString &Value)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxTextCtrl);
   wxTextCtrl * pTextCtrl;
   SetProportions( 1 );
   mpWind = pTextCtrl = safenew wxTextCtrl(GetParent(), miId, Value,
      wxDefaultPosition, wxDefaultSize, Style( wxTE_MULTILINE ));
   UpdateSizers();
   // Start off at start of window...
   pTextCtrl->SetInsertionPoint( 0 );
   pTextCtrl->ShowPosition( 0 );
   return pTextCtrl;
}

/// Single line text box of fixed size.
void ShuttleGuiBase::AddConstTextBox(const wxString &Prompt, const wxString &Value)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return;
//      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wx);
   miProp=0;
   AddPrompt( Prompt );
   UpdateSizers();
   miProp=0;
   mpWind = safenew wxStaticText(GetParent(), miId, Value, wxDefaultPosition, wxDefaultSize,
      Style( 0 ));
   mpWind->SetName(Value); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
   UpdateSizers();
}

wxListBox * ShuttleGuiBase::AddListBox(const wxArrayString * pChoices, long style)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxListBox);
   wxListBox * pListBox;
   SetProportions( 1 );
   mpWind = pListBox = safenew wxListBox(GetParent(), miId,
      wxDefaultPosition, wxDefaultSize,*pChoices, style);
   pListBox->SetMinSize( wxSize( 120,150 ));
   UpdateSizers();
   return pListBox;
}


wxListCtrl * ShuttleGuiBase::AddListControl()
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxListCtrl);
   wxListCtrl * pListCtrl;
   SetProportions( 1 );
   mpWind = pListCtrl = safenew wxListCtrl(GetParent(), miId,
      wxDefaultPosition, wxDefaultSize, Style( wxLC_ICON ));
   pListCtrl->SetMinSize( wxSize( 120,150 ));
   UpdateSizers();
   return pListCtrl;
}

wxGrid * ShuttleGuiBase::AddGrid()
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxGrid);
   wxGrid * pGrid;
   SetProportions( 1 );
   mpWind = pGrid = safenew wxGrid(GetParent(), miId, wxDefaultPosition,
      wxDefaultSize, Style( wxWANTS_CHARS ));
   pGrid->SetMinSize( wxSize( 120, 150 ));
   UpdateSizers();
   return pGrid;
}

wxListCtrl * ShuttleGuiBase::AddListControlReportMode()
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxListCtrl);
   wxListCtrl * pListCtrl;
   SetProportions( 1 );
   mpWind = pListCtrl = safenew wxListCtrl(GetParent(), miId,
      wxDefaultPosition, wxSize(230,120),//wxDefaultSize,
      Style( wxLC_REPORT | wxLC_HRULES | wxLC_VRULES | wxSUNKEN_BORDER ));
//   pListCtrl->SetMinSize( wxSize( 120,150 ));
   UpdateSizers();
   return pListCtrl;
}

wxTreeCtrl * ShuttleGuiBase::AddTree()
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxTreeCtrl);
   wxTreeCtrl * pTreeCtrl;
   SetProportions( 1 );
   mpWind = pTreeCtrl = safenew wxTreeCtrl(GetParent(), miId, wxDefaultPosition, wxDefaultSize,
      Style( wxTR_HAS_BUTTONS ));
   pTreeCtrl->SetMinSize( wxSize( 120,650 ));
   UpdateSizers();
   return pTreeCtrl;
}

void ShuttleGuiBase::AddIcon(wxBitmap *pBmp)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
//      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wx);
      return;
   wxBitmapButton * pBtn;
   mpWind = pBtn = safenew wxBitmapButton(GetParent(), miId, *pBmp,
      wxDefaultPosition, wxDefaultSize, Style( wxBU_AUTODRAW ) );
   pBtn->SetWindowStyle( 0 );
   UpdateSizersC();
}

ShuttleGuiBase & ShuttleGuiBase::Prop( int iProp )
{
   miPropSetByUser = iProp;
   return *this;
}

wxMenuBar * ShuttleGuiBase::AddMenuBar( )
{
   auto menuBar = std::make_unique<wxMenuBar>();
   mpMenuBar = menuBar.get();

   wxFrame * pFrame = (wxFrame*)mpParent;
   pFrame->SetThemeEnabled( true );
   mpMenuBar->SetThemeEnabled( true );
   pFrame->SetMenuBar(menuBar.release());

   return mpMenuBar;
}

wxMenu * ShuttleGuiBase::AddMenu( const wxString & Title )
{
   mpMenuBar->Append( (mpMenu = safenew wxMenu), Title );
   return mpMenu;
}



/// Starts a static box around a number of controls.
///  @param Str   The text of the title for the box.
///  @param iProp The resizing proportion value.
/// Use iProp == 0 for a minimum sized static box.
/// Use iProp == 1 for a box that grows if there is space to spare.
wxStaticBox * ShuttleGuiBase::StartStatic(const wxString &Str, int iProp)
{
   UseUpId();
   mBoxName = Str;
   if( mShuttleMode != eIsCreating )
      return NULL;
   wxStaticBox * pBox = safenew wxStaticBox(GetParent(), miId,
      Str );
   pBox->SetLabel( Str );
   pBox->SetName(wxStripMenuCodes(Str));
   mpSubSizer = std::make_unique<wxStaticBoxSizer>(
      pBox,
      wxVERTICAL );
   miSizerProp = iProp;
   UpdateSizers();
   return pBox;
}

void ShuttleGuiBase::EndStatic()
{
   if( mShuttleMode != eIsCreating )
      return;
   PopSizer();
}

/// This allows subsequent controls and static boxes to be in
/// a scrolled panel.  Very handy if you are running out of space
/// on a dialog.
///
/// The iStyle parameter is used in some very hacky code that
/// dynamically repopulates a dialog.  It also controls the
/// background colour.  Look at the code for details.
///  @param istyle deprecated parameter, but has been used for hacking.
wxScrolledWindow * ShuttleGuiBase::StartScroller(int iStyle)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxScrolledWindow);

   wxScrolledWindow * pScroller;
   mpWind = pScroller = safenew wxScrolledWindow(GetParent(), miId, wxDefaultPosition, wxDefaultSize,
      Style( wxSUNKEN_BORDER ) );
   pScroller->SetScrollRate( 20,20 );

   // This fools NVDA into not saying "Panel" when the dialog gets focus
   pScroller->SetName(wxT("\a"));
   pScroller->SetLabel(wxT("\a"));

   SetProportions( 1 );
   if( iStyle==2 )
   {
      UpdateSizersAtStart();
   }
   else
   {
      mpWind->SetBackgroundColour(
         iStyle==0
         ? wxColour( 245,244,240) :
         wxSystemSettings::GetColour(wxSYS_COLOUR_3DFACE)
         );
      UpdateSizers();  // adds window in to current sizer.
   }

   // create a sizer within the window...
   mpParent = pScroller;
   pScroller->SetSizer(mpSizer = safenew wxBoxSizer(wxVERTICAL));
   PushSizer();
   return pScroller;
}

void ShuttleGuiBase::EndScroller()
{
   if( mShuttleMode != eIsCreating )
      return;
   wxSize ScrollSize = mpSizer->GetMinSize();
   int yMin = ScrollSize.y+4;
   int xMin = ScrollSize.x+4;
   if( yMin > 400)
   {
      yMin = 400;
      xMin+=50;// extra space for vertical scrollbar.
   }

   mpParent->SetMinSize( wxSize(xMin, yMin) );

   PopSizer();
   mpParent = mpParent->GetParent();
}

wxPanel * ShuttleGuiBase::StartPanel(int iStyle)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxPanel);
   wxPanel * pPanel;
   mpWind = pPanel = safenew wxPanelWrapper( GetParent(), miId, wxDefaultPosition, wxDefaultSize,
      Style( wxNO_BORDER ));

   if( iStyle != 0 )
   {
      mpWind->SetBackgroundColour(
         iStyle==1
         ? wxColour( 190,200,230) :
         wxSystemSettings::GetColour(wxSYS_COLOUR_WINDOW)
         );
   }
   SetProportions(0);
   miBorder=2;
   UpdateSizers();  // adds window in to current sizer.

   // create a sizer within the window...
   mpParent = pPanel;
   pPanel->SetSizer(mpSizer = safenew wxBoxSizer(wxVERTICAL));
   PushSizer();
   return pPanel;
}

void ShuttleGuiBase::EndPanel()
{
   if( mShuttleMode != eIsCreating )
      return;
   PopSizer();
   mpParent = mpParent->GetParent();
}

wxNotebook * ShuttleGuiBase::StartNotebook()
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxNotebook);
   wxNotebook * pNotebook;
   mpWind = pNotebook = safenew wxNotebook(GetParent(),
      miId, wxDefaultPosition, wxDefaultSize, Style( 0 ));
   SetProportions( 1 );
   UpdateSizers();
   mpParent = pNotebook;
   return pNotebook;
}

void ShuttleGuiBase::EndNotebook()
{
   //PopSizer();
   mpParent = mpParent->GetParent();
}


wxNotebookPage * ShuttleGuiBase::StartNotebookPage( const wxString & Name )
{
   if( mShuttleMode != eIsCreating )
      return NULL;
//      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wx);
   wxNotebook * pNotebook = (wxNotebook*)mpParent;
   wxNotebookPage * pPage = safenew wxPanelWrapper(GetParent());
   pPage->SetName(Name);

   pNotebook->AddPage(
      pPage,
      Name);
   PushSizer();

   SetProportions( 1 );
   mpParent = pPage;
   pPage->SetSizer(mpSizer = safenew wxBoxSizer(wxVERTICAL));
   mpSizer->SetMinSize(250, 500);
   //   UpdateSizers();
   return pPage;
}

void ShuttleGuiBase::StartNotebookPage( const wxString & Name, wxNotebookPage * pPage )
{
   if( mShuttleMode != eIsCreating )
      return;
//      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wx);
   wxNotebook * pNotebook = (wxNotebook*)mpParent;
//   wxNotebookPage * pPage = safenew wxPanelWrapper(GetParent());
   pPage->Create( mpParent, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL, wxT("panel"));
   pPage->SetName(Name);

   pNotebook->AddPage(
      pPage,
      Name);
   PushSizer();

   SetProportions( 1 );
   mpParent = pPage;
   pPage->SetSizer(mpSizer = safenew wxBoxSizer(wxVERTICAL));
   mpSizer->SetMinSize(250, 500);
   //   UpdateSizers();
}

void ShuttleGuiBase::EndNotebookPage()
{
   if( mShuttleMode != eIsCreating )
      return;
   PopSizer();
   mpParent = mpParent->GetParent();
}

// Doxygen description is at the start of the file
// this is a wxPanel with erase background disabled.
class InvisiblePanel final : public wxPanelWrapper
{
public:
   InvisiblePanel(
      wxWindow* parent,
      wxWindowID id = -1,
      const wxPoint& pos = wxDefaultPosition,
      const wxSize& size = wxDefaultSize,
      long style = wxTAB_TRAVERSAL ) :
      wxPanelWrapper( parent, id, pos, size, style )
   {
   };
   ~InvisiblePanel(){;};
   void OnPaint( wxPaintEvent &event );
   void OnErase(wxEraseEvent &/*evt*/){;};
   DECLARE_EVENT_TABLE()
};


BEGIN_EVENT_TABLE(InvisiblePanel, wxPanelWrapper)
//   EVT_PAINT(InvisiblePanel::OnPaint)
     EVT_ERASE_BACKGROUND( InvisiblePanel::OnErase)
END_EVENT_TABLE()

void InvisiblePanel::OnPaint( wxPaintEvent & WXUNUSED(event))
{
   // Don't repaint my background.
   wxPaintDC dc(this);
   // event.Skip(); // swallow the paint event.
}

wxPanel * ShuttleGuiBase::StartInvisiblePanel()
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxPanel);
   wxPanel * pPanel;
   mpWind = pPanel = safenew wxPanelWrapper(GetParent(), miId, wxDefaultPosition, wxDefaultSize,
      wxNO_BORDER);

   mpWind->SetBackgroundColour(
      wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE)
      );
   SetProportions( 1 );
   miBorder=0;
   UpdateSizers();  // adds window in to current sizer.

   // create a sizer within the window...
   mpParent = pPanel;
   pPanel->SetSizer(mpSizer = safenew wxBoxSizer(wxVERTICAL));
   PushSizer();
   return pPanel;
}

void ShuttleGuiBase::EndInvisiblePanel()
{
   EndPanel();
}


/// Starts a Horizontal Layout.
///  - Use wxEXPAND and 0 to expand horizontally but not vertically.
///  - Use wxEXPAND and 1 to expand horizontally and vertically.
///  - Use wxCENTRE and 1 for no expansion.
/// @param PositionFlag  Typically wxEXPAND or wxALIGN_CENTER.
/// @param iProp         Proportionality for resizing.
void ShuttleGuiBase::StartHorizontalLay( int PositionFlags, int iProp)
{
   if( mShuttleMode != eIsCreating )
      return;
   miSizerProp=iProp;
   mpSubSizer = std::make_unique<wxBoxSizer>( wxHORIZONTAL );
   UpdateSizersCore( false, PositionFlags | wxALL );
}

void ShuttleGuiBase::EndHorizontalLay()
{
   if( mShuttleMode != eIsCreating )
      return;
   PopSizer();
}

void ShuttleGuiBase::StartVerticalLay(int iProp)
{
   if( mShuttleMode != eIsCreating )
      return;
   miSizerProp=iProp;
   mpSubSizer = std::make_unique<wxBoxSizer>( wxVERTICAL );
   UpdateSizers();
}

void ShuttleGuiBase::EndVerticalLay()
{
   if( mShuttleMode != eIsCreating )
      return;
   PopSizer();
}

void ShuttleGuiBase::StartMultiColumn(int nCols, int PositionFlags)
{
   if( mShuttleMode != eIsCreating )
      return;
   mpSubSizer = std::make_unique<wxFlexGridSizer>( nCols );
   UpdateSizersCore( false, PositionFlags | wxALL );
}

void ShuttleGuiBase::EndMultiColumn()
{
   if( mShuttleMode != eIsCreating )
      return;
   PopSizer();
}

/// When we're exchanging with the configured shuttle rather than with the GUI
/// We use this function.
void ShuttleGuiBase::DoDataShuttle( const wxString &Name, WrappedType & WrappedRef )
{
    wxASSERT( mpShuttle );
    mpShuttle->TransferWrappedType( Name, WrappedRef );
}

//-----------------------------------------------------------------------//

// We now have a group of tie functions which are generic in the type
// they bind to (i.e. WrappedType).
// The type specific versions are much shorter and are later
// in this file.
wxCheckBox * ShuttleGuiBase::TieCheckBox(const wxString &Prompt, WrappedType & WrappedRef)
{
   // The Add function does a UseUpId(), so don't do it here in that case.
   if( mShuttleMode == eIsCreating )
      return AddCheckBox( Prompt, WrappedRef.ReadAsString());

   UseUpId();

   wxWindow * pWnd      = wxWindow::FindWindowById( miId, mpDlg);
   wxCheckBox * pCheckBox = wxDynamicCast(pWnd, wxCheckBox);

   switch( mShuttleMode )
   {
   // IF setting internal storage from the controls.
   case eIsGettingFromDialog:
      {
         wxASSERT( pCheckBox );
         WrappedRef.WriteToAsBool( pCheckBox->GetValue() );
      }
      break;
   case eIsSettingToDialog:
      {
         wxASSERT( pCheckBox );
         pCheckBox->SetValue( WrappedRef.ReadAsBool() );
      }
      break;
   // IF Saving settings to external storage...
   // or IF Getting settings from external storage.
   case eIsSavingViaShuttle:
   case eIsGettingViaShuttle:
      DoDataShuttle( Prompt, WrappedRef );
      break;
   default:
      wxASSERT( false );
      break;
   }
   return pCheckBox;
}

wxCheckBox * ShuttleGuiBase::TieCheckBoxOnRight(const wxString &Prompt, WrappedType & WrappedRef)
{
   // The Add function does a UseUpId(), so don't do it here in that case.
   if( mShuttleMode == eIsCreating )
      return AddCheckBoxOnRight( Prompt, WrappedRef.ReadAsString());

   UseUpId();

   wxWindow * pWnd      = wxWindow::FindWindowById( miId, mpDlg);
   wxCheckBox * pCheckBox = wxDynamicCast(pWnd, wxCheckBox);

   switch( mShuttleMode )
   {
   // IF setting internal storage from the controls.
   case eIsGettingFromDialog:
      {
         wxASSERT( pCheckBox );
         WrappedRef.WriteToAsBool( pCheckBox->GetValue() );
      }
      break;
   case eIsSettingToDialog:
      {
         wxASSERT( pCheckBox );
         pCheckBox->SetValue( WrappedRef.ReadAsBool() );
      }
      break;
   // IF Saving settings to external storage...
   // or IF Getting settings from external storage.
   case eIsSavingViaShuttle:
   case eIsGettingViaShuttle:
      DoDataShuttle( Prompt, WrappedRef );
      break;
   default:
      wxASSERT( false );
      break;
   }
   return pCheckBox;
}

wxSpinCtrl * ShuttleGuiBase::TieSpinCtrl( const wxString &Prompt, WrappedType & WrappedRef, const int max, const int min )
{
   // The Add function does a UseUpId(), so don't do it here in that case.
   if( mShuttleMode == eIsCreating )
      return AddSpinCtrl( Prompt, WrappedRef.ReadAsInt(), max, min );

   UseUpId();
   wxSpinCtrl * pSpinCtrl=NULL;

   wxWindow * pWnd  = wxWindow::FindWindowById( miId, mpDlg);
   pSpinCtrl = wxDynamicCast(pWnd, wxSpinCtrl);

   switch( mShuttleMode )
   {
      // IF setting internal storage from the controls.
   case eIsGettingFromDialog:
      {
         wxASSERT( pSpinCtrl );
         WrappedRef.WriteToAsInt( pSpinCtrl->GetValue() );
      }
      break;
   case eIsSettingToDialog:
      {
         wxASSERT( pSpinCtrl );
         pSpinCtrl->SetValue( WrappedRef.ReadAsInt() );
      }
      break;
      // IF Saving settings to external storage...
      // or IF Getting settings from external storage.
   case eIsGettingViaShuttle:
   case eIsSavingViaShuttle:
      DoDataShuttle( Prompt, WrappedRef );
      break;
   default:
      wxASSERT( false );
      break;
   }
   return pSpinCtrl;
}

wxTextCtrl * ShuttleGuiBase::TieTextBox( const wxString &Prompt, WrappedType & WrappedRef, const int nChars)
{
   // The Add function does a UseUpId(), so don't do it here in that case.
   if( mShuttleMode == eIsCreating )
      return AddTextBox( Prompt, WrappedRef.ReadAsString(), nChars );

   UseUpId();
   wxTextCtrl * pTextBox=NULL;

   wxWindow * pWnd  = wxWindow::FindWindowById( miId, mpDlg);
   pTextBox = wxDynamicCast(pWnd, wxTextCtrl);

   switch( mShuttleMode )
   {
   // IF setting internal storage from the controls.
   case eIsGettingFromDialog:
      {
         wxASSERT( pTextBox );
         WrappedRef.WriteToAsString( pTextBox->GetValue() );
      }
      break;
   case eIsSettingToDialog:
      {
         wxASSERT( pTextBox );
         pTextBox->SetValue( WrappedRef.ReadAsString() );
      }
      break;
   // IF Saving settings to external storage...
   // or IF Getting settings from external storage.
   case eIsGettingViaShuttle:
   case eIsSavingViaShuttle:
      DoDataShuttle( Prompt, WrappedRef );
      break;
   default:
      wxASSERT( false );
      break;
   }
   return pTextBox;
}

wxTextCtrl * ShuttleGuiBase::TieNumericTextBox( const wxString &Prompt, WrappedType & WrappedRef, const int nChars)
{
   // The Add function does a UseUpId(), so don't do it here in that case.
   if( mShuttleMode == eIsCreating )
      return AddNumericTextBox( Prompt, WrappedRef.ReadAsString(), nChars );

   UseUpId();
   wxTextCtrl * pTextBox=NULL;

   wxWindow * pWnd  = wxWindow::FindWindowById( miId, mpDlg);
   pTextBox = wxDynamicCast(pWnd, wxTextCtrl);

   switch( mShuttleMode )
   {
   // IF setting internal storage from the controls.
   case eIsGettingFromDialog:
      {
         wxASSERT( pTextBox );
         WrappedRef.WriteToAsString( pTextBox->GetValue() );
      }
      break;
   case eIsSettingToDialog:
      {
         wxASSERT( pTextBox );
         pTextBox->SetValue( WrappedRef.ReadAsString() );
      }
      break;
   // IF Saving settings to external storage...
   // or IF Getting settings from external storage.
   case eIsGettingViaShuttle:
   case eIsSavingViaShuttle:
      DoDataShuttle( Prompt, WrappedRef );
      break;
   default:
      wxASSERT( false );
      break;
   }
   return pTextBox;
}

wxSlider * ShuttleGuiBase::TieSlider( const wxString &Prompt, WrappedType & WrappedRef, const int max, int min )
{
   // The Add function does a UseUpId(), so don't do it here in that case.
   if( mShuttleMode != eIsCreating )
      UseUpId();
   wxSlider * pSlider=NULL;
   switch( mShuttleMode )
   {
      case eIsCreating:
         {
            pSlider = AddSlider( Prompt, WrappedRef.ReadAsInt(), max, min );
         }
         break;
      // IF setting internal storage from the controls.
      case eIsGettingFromDialog:
         {
            wxWindow * pWnd  = wxWindow::FindWindowById( miId, mpDlg);
            pSlider = wxDynamicCast(pWnd, wxSlider);
            wxASSERT( pSlider );
            WrappedRef.WriteToAsInt( pSlider->GetValue() );
         }
         break;
      case eIsSettingToDialog:
         {
            wxWindow * pWnd  = wxWindow::FindWindowById( miId, mpDlg);
            pSlider = wxDynamicCast(pWnd, wxSlider);
            wxASSERT( pSlider );
            pSlider->SetValue( WrappedRef.ReadAsInt() );
         }
         break;
      // IF Saving settings to external storage...
      // or IF Getting settings from external storage.
      case eIsSavingViaShuttle:
      case eIsGettingViaShuttle:
         DoDataShuttle( Prompt, WrappedRef );
         break;
      default:
         wxASSERT( false );
         break;
   }
   return pSlider;
}


wxChoice * ShuttleGuiBase::TieChoice(
   const wxString &Prompt,
   WrappedType &WrappedRef,
   const wxArrayString * pChoices )
{
   // The Add function does a UseUpId(), so don't do it here in that case.
   if( mShuttleMode != eIsCreating )
      UseUpId();
   wxChoice * pChoice=NULL;
   switch( mShuttleMode )
   {
   case eIsCreating:
      {
         if( WrappedRef.IsString() )
            pChoice = AddChoice( Prompt, WrappedRef.ReadAsString(), pChoices );
         else
         {
            wxString Temp;
            if( pChoices && ( WrappedRef.ReadAsInt() < (int)pChoices->GetCount() ) )
            {
               Temp = (*pChoices)[WrappedRef.ReadAsInt()];
            }
            pChoice = AddChoice( Prompt, Temp, pChoices );
         }
      }
      break;
   // IF setting internal storage from the controls.
   case eIsGettingFromDialog:
      {
         wxWindow * pWnd  = wxWindow::FindWindowById( miId, mpDlg);
         pChoice = wxDynamicCast(pWnd, wxChoice);
         wxASSERT( pChoice );
         if( WrappedRef.IsString())
            WrappedRef.WriteToAsString( pChoice->GetStringSelection());
         else
            WrappedRef.WriteToAsInt( pChoice->GetSelection() );
      }
      break;
   case eIsSettingToDialog:
      {
         wxWindow * pWnd  = wxWindow::FindWindowById( miId, mpDlg);
         pChoice = wxDynamicCast(pWnd, wxChoice);
         wxASSERT( pChoice );
         if( WrappedRef.IsString() )
            pChoice->SetStringSelection( WrappedRef.ReadAsString() );
         else
            pChoice->SetSelection( WrappedRef.ReadAsInt() );
      }
      break;
   // IF Saving settings to external storage...
   // or IF Getting settings from external storage.
   case eIsSavingViaShuttle:
   case eIsGettingViaShuttle:
      DoDataShuttle( Prompt, WrappedRef );
      break;
   default:
      wxASSERT( false );
      break;
   }
   return pChoice;
}

wxRadioButton * ShuttleGuiBase::TieRadioButton(const wxString &Prompt, WrappedType & WrappedRef)
{
   wxASSERT( mRadioCount >= 0); // Did you remember to use StartRadioButtonGroup() ?
   mRadioCount++;
   UseUpId();
   wxRadioButton * pRadioButton = NULL;

   switch( mShuttleMode )
   {
   case eIsCreating:
      {
         mpWind = pRadioButton = safenew wxRadioButton(GetParent(), miId, Prompt,
            wxDefaultPosition, wxDefaultSize,
            (mRadioCount==1)?wxRB_GROUP:0);
         pRadioButton->SetValue(WrappedRef.ValuesMatch( mRadioValue ));
         pRadioButton->SetName(wxStripMenuCodes(Prompt));
         UpdateSizers();
      }
      break;
   case eIsGettingFromDialog:
      {
         wxWindow * pWnd  = wxWindow::FindWindowById( miId, mpDlg);
         pRadioButton = wxDynamicCast(pWnd, wxRadioButton);
         wxASSERT( pRadioButton );
         if( pRadioButton->GetValue() )
         {
            mRadioValue.WriteToAsWrappedType( WrappedRef );
         }
      }
      break;
   default:
      wxASSERT( false );
      break;
   }
   return pRadioButton;
}

/// Call this before any TieRadioButton calls.
/// This is the generic version and requires mRadioValue already initialised.
/// Versions for specific types must do that initialisation.
void ShuttleGuiBase::StartRadioButtonGroup( const wxString & SettingName )
{
   wxASSERT( mRadioValue.eWrappedType != eWrappedNotSet );
   mSettingName = SettingName;
   mRadioCount = 0;
   if( mShuttleMode == eIsCreating )
      DoDataShuttle( SettingName, mRadioValue );
}

/// Call this after any TieRadioButton calls.
/// It's generic too.  We don't need type-specific ones.
void ShuttleGuiBase::EndRadioButtonGroup()
{
   if( mShuttleMode == eIsGettingFromDialog )
      DoDataShuttle( mSettingName, mRadioValue );
   mRadioValue.Init();// Clear it out...
   mSettingName = wxT("");
   mRadioCount = -1; // So we detect a problem.
}

//-----------------------------------------------------------------------//
//-- Now we are into type specific Tie() functions.
//-- These are all 'one-step' tie functions.

wxCheckBox * ShuttleGuiBase::TieCheckBox(const wxString &Prompt, bool &Var)
{
   WrappedType WrappedRef( Var );
   return TieCheckBox( Prompt, WrappedRef );
}

// See comment in AddCheckBoxOnRight() for why we have this variant.
wxCheckBox * ShuttleGuiBase::TieCheckBoxOnRight(const wxString &Prompt, bool &Var)
{
   // Only odes anything different if it's creating.
   WrappedType WrappedRef( Var );
   if( mShuttleMode == eIsCreating )
      return AddCheckBoxOnRight( Prompt, WrappedRef.ReadAsString() );
   return TieCheckBox( Prompt, WrappedRef );
}

wxSpinCtrl * ShuttleGuiBase::TieSpinCtrl( const wxString &Prompt, int &Value, const int max, const int min )
{
   WrappedType WrappedRef(Value);
   return TieSpinCtrl( Prompt, WrappedRef, max, min );
}

wxTextCtrl * ShuttleGuiBase::TieTextBox( const wxString &Prompt, wxString &Selected, const int nChars)
{
   WrappedType WrappedRef(Selected);
   return TieTextBox( Prompt, WrappedRef, nChars );
}

wxTextCtrl * ShuttleGuiBase::TieTextBox( const wxString &Prompt, int &Selected, const int nChars)
{
   WrappedType WrappedRef( Selected );
   return TieTextBox( Prompt, WrappedRef, nChars );
}

wxTextCtrl * ShuttleGuiBase::TieTextBox( const wxString &Prompt, double &Value, const int nChars)
{
   WrappedType WrappedRef( Value );
   return TieTextBox( Prompt, WrappedRef, nChars );
}

wxTextCtrl * ShuttleGuiBase::TieNumericTextBox( const wxString &Prompt, wxString &Selected, const int nChars)
{
   WrappedType WrappedRef(Selected);
   return TieNumericTextBox( Prompt, WrappedRef, nChars );
}

wxTextCtrl * ShuttleGuiBase::TieNumericTextBox( const wxString &Prompt, int &Selected, const int nChars)
{
   WrappedType WrappedRef( Selected );
   return TieNumericTextBox( Prompt, WrappedRef, nChars );
}

wxTextCtrl * ShuttleGuiBase::TieNumericTextBox( const wxString &Prompt, double &Value, const int nChars)
{
   WrappedType WrappedRef( Value );
   return TieNumericTextBox( Prompt, WrappedRef, nChars );
}

wxSlider * ShuttleGuiBase::TieSlider( const wxString &Prompt, int &pos, const int max, const int min )
{
   WrappedType WrappedRef( pos );
   return TieSlider( Prompt, WrappedRef, max, min );
}

wxSlider * ShuttleGuiBase::TieSlider( const wxString &Prompt, double &pos, const double max, const double min )
{
   WrappedType WrappedRef( pos );
   return TieSlider( Prompt, WrappedRef, max, min );
}

wxSlider * ShuttleGuiBase::TieSlider( const wxString &Prompt, float &pos, const float fMin, const float fMax)
{
   const float RoundFix=0.0000001f;
   int iVal=(pos-fMin+RoundFix)*100.0/(fMax-fMin);
   wxSlider * pWnd = TieSlider( Prompt, iVal, 100 );
   pos = iVal*(fMax-fMin)*0.01+fMin;
   return pWnd;
}

wxSlider * ShuttleGuiBase::TieVSlider( const wxString &Prompt, float &pos, const float fMin, const float fMax)
{
   int iVal=(pos-fMin)*100.0/(fMax-fMin);
//   if( mShuttleMode == eIsCreating )
//   {
//      return AddVSlider( Prompt, iVal, 100 );
//   }
   wxSlider * pWnd = TieSlider( Prompt, iVal, 100 );
   pos = iVal*(fMax-fMin)*0.01+fMin;
   return pWnd;
}

wxChoice * ShuttleGuiBase::TieChoice(
   const wxString &Prompt,
   wxString &Selected,
   const wxArrayString * pChoices )
{
   WrappedType WrappedRef( Selected );
   return TieChoice( Prompt, WrappedRef, pChoices );
}

wxChoice * ShuttleGuiBase::TieChoice(
   const wxString &Prompt,
   int &Selected,
   const wxArrayString * pChoices )
{
   WrappedType WrappedRef( Selected );
   return TieChoice( Prompt, WrappedRef, pChoices );
}

//-----------------------------------------------------------------------//

// ShuttleGui utility functions to look things up in a list.
// If not present, we use the configured default index value.

//-----------------------------------------------------------------------//

/// String-to-Index
int ShuttleGuiBase::TranslateToIndex( const wxString &Value, const wxArrayString &Choices )
{
   int n = Choices.Index( Value );
   if( n== wxNOT_FOUND )
      n=miNoMatchSelector;
   miNoMatchSelector = 0;
   return n;
}

/// Index-to-String
wxString ShuttleGuiBase::TranslateFromIndex( const int nIn, const wxArrayString &Choices )
{
   int n = nIn;
   if( n== wxNOT_FOUND )
      n=miNoMatchSelector;
   miNoMatchSelector = 0;
   if( n < (int)Choices.GetCount() )
   {
      return Choices[n];
   }
   return wxT("");
}

/// Int-to-Index (choices can be items like e.g 0x400120 )
int ShuttleGuiBase::TranslateToIndex( const int Value, const wxArrayInt &Choices )
{
   int n = Choices.Index( Value );
   if( n== wxNOT_FOUND )
      n=miNoMatchSelector;
   miNoMatchSelector = 0;
   return n;
}

/// Index-to-int (choices can be items like e.g 0x400120 )
int ShuttleGuiBase::TranslateFromIndex( const int nIn, const wxArrayInt &Choices )
{
   int n = nIn;
   if( n== wxNOT_FOUND )
      n=miNoMatchSelector;
   miNoMatchSelector = 0;
   if( n < (int)Choices.GetCount() )
   {
      return Choices[n];
   }
   return 0;
}

//-----------------------------------------------------------------------//


// ShuttleGui code uses the model that you read into program variables
// and write out from program variables.

// In programs like Audacity which don't use internal program variables
// you have to do both steps in one go, using variants of the standard
// 'Tie' functions which call the underlying Tie functions twice.

//----------------------------------------------------------------------//


/**
 Code-Condenser function.

We have functions which need to do:

\code
  // Either: Values are coming in:
  DoDataShuttle( SettingName, WrappedRef );
  TieMyControl( Prompt, WrappedRef );

  // Or: Values are going out:
  TieMyControl( Prompt, WrappedRef );
  DoDataShuttle( SettingName, WrappedRef );
\endcode

So we make a list of all the possible steps,
and have DoStep choose which ones are actually done,
like this:

\code
  if( DoStep(1) ) DoFirstThing();
  if( DoStep(2) ) DoSecondThing();
  if( DoStep(3) ) DoThirdThing();
\endcode

The repeated choice logic can then be taken out of those
functions.

JKC: This paves the way for doing data validation too,
though when we add that we wil need to renumber the
steps.
*/
bool ShuttleGuiBase::DoStep( int iStep )
{
   // Get value and create
   if( mShuttleMode == eIsCreating )
   {
      return (iStep==1) || (iStep==2);
   }
   // Like creating, get the value and set.
   if( mShuttleMode == eIsSettingToDialog )
   {
      return (iStep==1) || (iStep==2);
   }
   if( mShuttleMode == eIsGettingFromDialog )
   {
      return (iStep==2) || (iStep==3);
   }
   wxASSERT( false );
   return false;
}


/// Variant of the standard TieCheckBox which does the two step exchange
/// between gui and stack variable and stack variable and shuttle.
wxCheckBox * ShuttleGuiBase::TieCheckBox(
   const wxString &Prompt,
   const wxString &SettingName,
   const bool bDefault)
{
   wxCheckBox * pCheck=NULL;

   bool bValue=bDefault;
   WrappedType WrappedRef( bValue );
   if( DoStep(1) ) DoDataShuttle( SettingName, WrappedRef );
   if( DoStep(2) ) pCheck = TieCheckBox( Prompt, WrappedRef );
   if( DoStep(3) ) DoDataShuttle( SettingName, WrappedRef );

   return pCheck;
}

/// Variant of the standard TieCheckBox which does the two step exchange
/// between gui and stack variable and stack variable and shuttle.
wxCheckBox * ShuttleGuiBase::TieCheckBoxOnRight(
   const wxString &Prompt,
   const wxString &SettingName,
   const bool bDefault)
{
   wxCheckBox * pCheck=NULL;

   bool bValue=bDefault;
   WrappedType WrappedRef( bValue );
   if( DoStep(1) ) DoDataShuttle( SettingName, WrappedRef );
   if( DoStep(2) ) pCheck = TieCheckBoxOnRight( Prompt, WrappedRef );
   if( DoStep(3) ) DoDataShuttle( SettingName, WrappedRef );

   return pCheck;
}

/// Variant of the standard TieSlider which does the two step exchange
/// between gui and stack variable and stack variable and shuttle.
wxSlider * ShuttleGuiBase::TieSlider(
   const wxString &Prompt,
   const wxString &SettingName,
   const int iDefault,
   const int max,
   const int min)
{
   wxSlider * pSlider=NULL;

   int iValue=iDefault;
   WrappedType WrappedRef( iValue );
   if( DoStep(1) ) DoDataShuttle( SettingName, WrappedRef );
   if( DoStep(2) ) pSlider = TieSlider( Prompt, WrappedRef, max, min );
   if( DoStep(3) ) DoDataShuttle( SettingName, WrappedRef );

   return pSlider;
}

/// Variant of the standard TieSpinCtrl which does the two step exchange
/// between gui and stack variable and stack variable and shuttle.
wxSpinCtrl * ShuttleGuiBase::TieSpinCtrl(
                                     const wxString &Prompt,
                                     const wxString &SettingName,
                                     const int Value,
                                     const int max,
                                     const int min)
{
   wxSpinCtrl * pSpinCtrl=NULL;

   int iValue = Value;
   WrappedType WrappedRef( iValue );
   if( DoStep(1) ) DoDataShuttle( SettingName, WrappedRef );
   if( DoStep(2) ) pSpinCtrl = TieSpinCtrl( Prompt, WrappedRef, max, min );
   if( DoStep(3) ) DoDataShuttle( SettingName, WrappedRef );

   return pSpinCtrl;
}

/// Variant of the standard TieTextBox which does the two step exchange
/// between gui and stack variable and stack variable and shuttle.
wxTextCtrl * ShuttleGuiBase::TieTextBox(
   const wxString & Prompt,
   const wxString & SettingName,
   const wxString & Default,
   const int nChars)
{
   wxTextCtrl * pText=(wxTextCtrl*)NULL;

   wxString Temp = Default;
   WrappedType WrappedRef( Temp );
   if( DoStep(1) ) DoDataShuttle( SettingName, WrappedRef );
   if( DoStep(2) ) pText = TieTextBox( Prompt, WrappedRef, nChars );
   if( DoStep(3) ) DoDataShuttle( SettingName, WrappedRef );
   return pText;
}

/// Variant of the standard TieTextBox which does the two step exchange
/// between gui and stack variable and stack variable and shuttle.
wxTextCtrl * ShuttleGuiBase::TieNumericTextBox(
   const wxString & Prompt,
   const wxString & SettingName,
   const wxString & Default,
   const int nChars)
{
   wxTextCtrl * pText=(wxTextCtrl*)NULL;

   wxString Temp = Default;
   WrappedType WrappedRef( Temp );
   if( DoStep(1) ) DoDataShuttle( SettingName, WrappedRef );
   if( DoStep(2) ) pText = TieNumericTextBox( Prompt, WrappedRef, nChars );
   if( DoStep(3) ) DoDataShuttle( SettingName, WrappedRef );
   return pText;
}
/// Variant of the standard TieTextBox which does the two step exchange
/// between gui and stack variable and stack variable and shuttle.
/// This one does it for double values...
wxTextCtrl * ShuttleGuiBase::TieTextBox(
   const wxString & Prompt,
   const wxString & SettingName,
   const double & Default,
   const int nChars)
{
   wxTextCtrl * pText=(wxTextCtrl*)NULL;

   double Temp = Default;
   WrappedType WrappedRef( Temp );
   if( DoStep(1) ) DoDataShuttle( SettingName, WrappedRef );
   if( DoStep(2) ) pText = TieTextBox( Prompt, WrappedRef, nChars );
   if( DoStep(3) ) DoDataShuttle( SettingName, WrappedRef );
   return pText;
}

/// Variant of the standard TieTextBox which does the two step exchange
/// between gui and stack variable and stack variable and shuttle.
/// This one does it for double values...
wxTextCtrl * ShuttleGuiBase::TieNumericTextBox(
   const wxString & Prompt,
   const wxString & SettingName,
   const double & Default,
   const int nChars)
{
   wxTextCtrl * pText=(wxTextCtrl*)NULL;

   double Temp = Default;
   WrappedType WrappedRef( Temp );
   if( DoStep(1) ) DoDataShuttle( SettingName, WrappedRef );
   if( DoStep(2) ) pText = TieNumericTextBox( Prompt, WrappedRef, nChars );
   if( DoStep(3) ) DoDataShuttle( SettingName, WrappedRef );
   return pText;
}

/// Variant of the standard TieChoice which does the two step exchange
/// between gui and stack variable and stack variable and shuttle.
///   @param Prompt             The prompt shown beside the control.
///   @param SettingName        The setting name as stored in gPrefs
///   @param Default            The default value for this control (translated)
///   @param Choices            An array of choices that appear on screen.
///   @param TranslatedChoices  The corresponding values (as a string array)
wxChoice * ShuttleGuiBase::TieChoice(
   const wxString &Prompt,
   const wxString &SettingName,
   const wxString &Default,
   const wxArrayString & Choices,
   const wxArrayString & TranslatedChoices)
{
   wxChoice * pChoice=(wxChoice*)NULL;

   int TempIndex=0;
//   int TempIndex = TranslateToIndex( Default, TranslatedChoices );
   wxString TempStr = Default;
   WrappedType WrappedRef( TempStr );
   // Get from prefs does 1 and 2.
   // Put to prefs does 2 and 3.
   if( DoStep(1) ) DoDataShuttle( SettingName, WrappedRef ); // Get Index from Prefs.
   if( DoStep(1) ) TempIndex = TranslateToIndex( TempStr, TranslatedChoices ); // To an index
   if( DoStep(2) ) pChoice = TieChoice( Prompt, TempIndex, &Choices ); // Get/Put index from GUI.
   if( DoStep(3) ) TempStr = TranslateFromIndex( TempIndex, TranslatedChoices ); // To a string
   if( DoStep(3) ) DoDataShuttle( SettingName, WrappedRef ); // Put into Prefs.
   return pChoice;
}

/// Variant of the standard TieChoice which does the two step exchange
/// between gui and stack variable and stack variable and shuttle.
/// Difference to previous one is that the Translated choices and default
/// are integers, not Strings.
///   @param Prompt             The prompt shown beside the control.
///   @param SettingName        The setting name as stored in gPrefs
///   @param Default            The default value for this control (translated)
///   @param Choices            An array of choices that appear on screen.
///   @param TranslatedChoices  The correcponding values (as an integer array)
wxChoice * ShuttleGuiBase::TieChoice(
   const wxString &Prompt,
   const wxString &SettingName,
   const int Default,
   const wxArrayString & Choices,
   const wxArrayInt & TranslatedChoices)
{
   wxChoice * pChoice=(wxChoice*)NULL;

   int TempIndex=0;
   int TranslatedInt = Default;
   WrappedType WrappedRef( TranslatedInt );
   // Get from prefs does 1 and 2.
   // Put to prefs does 2 and 3.
   if( DoStep(1) ) DoDataShuttle( SettingName, WrappedRef ); // Get Int from Prefs.
   if( DoStep(1) ) TempIndex = TranslateToIndex( TranslatedInt, TranslatedChoices ); // Int to an index.
   if( DoStep(2) ) pChoice = TieChoice( Prompt, TempIndex, &Choices ); // Get/Put index from GUI.
   if( DoStep(3) ) TranslatedInt = TranslateFromIndex( TempIndex, TranslatedChoices ); // Index to int
   if( DoStep(3) ) DoDataShuttle( SettingName, WrappedRef ); // Put into Prefs.
   return pChoice;
}

/// Integer specific version of StartRadioButtonGroup.
/// All 'TieRadioButton()' enclosed must be ints.
void ShuttleGuiBase::StartRadioButtonGroup( const wxString & SettingName, const int iDefaultValue )
{
   // Configure the generic type mechanism to use OUR integer.
   mRadioValueInt = iDefaultValue;
   mRadioValue.SetTo( mRadioValueInt );
   // Now actually start the radio button group.
   StartRadioButtonGroup( SettingName );
}

/// String specific version of StartRadioButtonGroup.
/// All 'TieRadioButton()' enclosed must be strings.
void ShuttleGuiBase::StartRadioButtonGroup( const wxString & SettingName, const wxString & DefaultValue )
{
   // Configure the generic type mechanism to use OUR string.
   mRadioValueString = DefaultValue;
   mRadioValue.SetTo( mRadioValueString );
   // Now actually start the radio button group.
   StartRadioButtonGroup( SettingName );
}


/// This function must be within a StartRadioButtonGroup - EndRadioButtonGroup pair.
wxRadioButton * ShuttleGuiBase::TieRadioButton(
   const wxString &Prompt,
   const int iValue)
{
   int iTemp = iValue;
   WrappedType WrappedRef( iTemp );
   return TieRadioButton( Prompt, WrappedRef );
}

/// This function must be within a StartRadioButtonGroup - EndRadioButtonGroup pair.
wxRadioButton * ShuttleGuiBase::TieRadioButton(
   const wxString &Prompt,
   const wxString &Value)
{
   // In what follows, WrappedRef is used in read only mode, but we
   // don't have a 'read-only' version, so we copy to deal with the constness.
   wxString Temp = Value;
   WrappedType WrappedRef( Temp );
   return TieRadioButton( Prompt, WrappedRef );
}

//------------------------------------------------------------------//

// We're now into ShuttleGuiBase sizer and misc functions.

/// The Ids increment as we add NEW controls.
/// However, the user can force the id manually, for example
/// if they need a specific Id for a button, and then let it
/// resume normal numbering later.
/// UseUpId() sets miId to the next Id, either using the
/// user specicfied one, or resuming the sequence.
void ShuttleGuiBase::UseUpId()
{
   if( miIdSetByUser > 0)
   {
      miId = miIdSetByUser;
      miIdSetByUser = -1;
      return;
   }
   miId = miIdNext++;
}

void ShuttleGuiBase::SetProportions( int Default )
{
   if( miPropSetByUser >=0 )
   {
      miProp = miPropSetByUser;
      miPropSetByUser =-1;
      return;
   }
   miProp = Default;
}


void ShuttleGuiBase::UpdateSizersCore(bool bPrepend, int Flags)
{
   if( mpWind && mpParent )
   {
      if( mpSizer){
         if( bPrepend )
         {
            mpSizer->Prepend(mpWind, miProp, Flags,miBorder);
         }
         else
         {
            mpSizer->Add(mpWind, miProp, Flags,miBorder);
         }
      }
   }

   if( mpSubSizer && mpSizer )
   {
      // When adding sizers into sizers, don't add a border.
      // unless it's a static box sizer.
      wxSizer *const pSubSizer = mpSubSizer.get();
      if (wxDynamicCast(pSubSizer, wxStaticBoxSizer))
      {
         mpSizer->Add( mpSubSizer.release(), miSizerProp, Flags , miBorder);
      }
      else
      {
         mpSizer->Add( mpSubSizer.release(), miSizerProp, Flags ,0);//miBorder);
      }
      mpSizer = pSubSizer;
      PushSizer();
   }
   mpLastWind = mpWind;
   mpWind = NULL;
   miProp = 0;
   miSizerProp =0;
}

// Sizer is added into parent sizer, and will expand/shrink.
void ShuttleGuiBase::UpdateSizers()
{  UpdateSizersCore( false, wxEXPAND | wxALL );}

// Sizer is added into parent sizer, centred
void ShuttleGuiBase::UpdateSizersC()
{  UpdateSizersCore( false, wxALIGN_CENTRE | wxALL );}

// Sizer is added into parent sizer, and will expand/shrink.
// added to start of sizer list.
void ShuttleGuiBase::UpdateSizersAtStart()
{  UpdateSizersCore( true, wxEXPAND | wxALL );}

void ShuttleGuiBase::PopSizer()
{
   mSizerDepth--;
   wxASSERT( mSizerDepth >=0 );
   mpSizer = pSizerStack[ mSizerDepth ];
}

void ShuttleGuiBase::PushSizer()
{
   mSizerDepth++;
   wxASSERT( mSizerDepth < nMaxNestedSizers );
   pSizerStack[ mSizerDepth ] = mpSizer;
}

long ShuttleGuiBase::Style( long style )
{
   if( miStyle )
      style = miStyle;
   miStyle = 0;
   return style;
}

// A rarely used helper function that sets a pointer
// ONLY if the value it is to be set to is non NULL.
void SetIfCreated( wxChoice * &Var, wxChoice * Val )
{
   if( Val != NULL )
      Var = Val;
};
void SetIfCreated( wxTextCtrl * &Var, wxTextCtrl * Val )
{
   if( Val != NULL )
      Var = Val;
};
void SetIfCreated( wxStaticText *&Var, wxStaticText * Val )
{
   if( Val != NULL )
      Var = Val;
};

#ifdef EXPERIMENTAL_TRACK_PANEL
// Additional includes down here, to make it easier to split this into
// two files at some later date.
#include "../extnpanel-src/GuiWaveTrack.h"
#endif
#include "./widgets/Ruler.h"
#include "./widgets/AttachableScrollBar.h"
#include "ShuttlePrefs.h"

ShuttleGui::ShuttleGui(wxWindow * pParent, teShuttleMode ShuttleMode) :
   ShuttleGuiBase( pParent, ShuttleMode )
{
   if( ShuttleMode == eIsCreatingFromPrefs )
   {
      mShuttleMode = eIsCreating;
      Init(); // Wasn't fully done in base constructor because it is only done when eIsCreating is set.
   }
   else if( ShuttleMode == eIsSavingToPrefs )
   {
      mShuttleMode = eIsGettingFromDialog;
   }
   else
   {
      return;
   }

   mpShuttle = std::make_unique<ShuttlePrefs>();
   // In this case the client is the GUI, so if creating we do want to
   // store in the client.
   mpShuttle->mbStoreInClient = (mShuttleMode == eIsCreating );
};

ShuttleGui::~ShuttleGui()
{
}

// Now we have Audacity specific shuttle functions.
ShuttleGui & ShuttleGui::Id(int id )
{
   miIdSetByUser = id;
   return *this;
}

GuiWaveTrack * ShuttleGui::AddGuiWaveTrack( const wxString & WXUNUSED(Name))
{
#ifdef EXPERIMENTAL_TRACK_PANEL
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return (GuiWaveTrack*)NULL;
//      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), GuiWaveTrack);
   GuiWaveTrack * pGuiWaveTrack;
   miProp=1;
   mpWind = pGuiWaveTrack = safenew GuiWaveTrack(mpParent, miId, Name);
   mpWind->SetMinSize(wxSize(100,50));
   UpdateSizers();
   return pGuiWaveTrack;
#else
   return NULL;
#endif
}

RulerPanel * ShuttleGui::AddRulerVertical(float low, float hi, const wxString & Units )
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return (RulerPanel*)NULL;
//    return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), RulerPanel);
   RulerPanel * pRulerPanel;
   miProp=0;
   mpWind = pRulerPanel = safenew RulerPanel(
      GetParent(),
      miId,
      wxDefaultPosition,
      wxDefaultSize
      );
   Ruler & Ruler = pRulerPanel->ruler;
   Ruler.SetOrientation(wxVERTICAL);
   Ruler.SetBounds(0, 0, 100, 100); // Ruler can't handle small sizes
   Ruler.SetRange(low, hi);
   Ruler.SetFormat(Ruler::RealFormat);
   Ruler.SetUnits(Units);
   Ruler.SetLabelEdges(true);

   mpWind->SetMinSize(wxSize(38,50));
   UpdateSizers();
   return pRulerPanel;
}

AttachableScrollBar * ShuttleGui::AddAttachableScrollBar( long style )
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return (AttachableScrollBar*)NULL;
//      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), AttachableScrollBar);
   AttachableScrollBar * pAttachableScrollBar;
   miProp=0;
   mpWind = pAttachableScrollBar = safenew AttachableScrollBar(
      mpParent,
      miId,
      wxDefaultPosition,
      wxDefaultSize,
      style
      );
   mpWind->SetMinSize(wxSize(10,20));
   UpdateSizers();
   return pAttachableScrollBar;
}

std::unique_ptr<wxSizer> CreateStdButtonSizer(wxWindow *parent, long buttons, wxWindow *extra)
{
   wxASSERT(parent != NULL); // To justify safenew

   int margin;
   {
#if defined(__WXMAC__)
      margin = 12;
#elif defined(__WXGTK20__)
      margin = 12;
#elif defined(__WXMSW__)
      wxButton b(parent, 0, wxEmptyString);
      margin = b.ConvertDialogToPixels(wxSize(2, 0)).x;
#else
      wxButton b(parent, 0, wxEmptyString);
      margin = b->ConvertDialogToPixels(wxSize(4, 0)).x;
#endif
   }

   wxButton *b = NULL;
   auto bs = std::make_unique<wxStdDialogButtonSizer>();

   if( buttons & eOkButton )
   {
      b = safenew wxButton(parent, wxID_OK);
      b->SetDefault();
      bs->AddButton( b );
   }

   if( buttons & eCancelButton )
   {
      bs->AddButton(safenew wxButton(parent, wxID_CANCEL));
   }

   if( buttons & eYesButton )
   {
      b = safenew wxButton(parent, wxID_YES);
      b->SetDefault();
      bs->AddButton( b );
   }

   if( buttons & eNoButton )
   {
      bs->AddButton(safenew wxButton(parent, wxID_NO));
   }

   if( buttons & eApplyButton )
   {
      b = safenew wxButton(parent, wxID_APPLY);
      b->SetDefault();
      bs->AddButton( b );
   }

   if( buttons & eCloseButton )
   {
      bs->AddButton(safenew wxButton(parent, wxID_CANCEL, _("&Close")));
   }

   if( buttons & eHelpButton )
   {
      bs->AddButton(safenew wxButton(parent, wxID_HELP));
   }

   if (buttons & ePreviewButton)
   {
      bs->Add(safenew wxButton(parent, ePreviewID, _("&Preview")), 0, wxALIGN_CENTER | wxLEFT | wxRIGHT, margin);
   }
   if (buttons & ePreviewDryButton)
   {
      bs->Add(safenew wxButton(parent, ePreviewDryID, _("Dry Previe&w")), 0, wxALIGN_CENTER | wxLEFT | wxRIGHT, margin);
      bs->Add( 20, 0 );
   }

   if( buttons & eSettingsButton )
   {
      bs->Add(safenew wxButton(parent, eSettingsID, _("&Settings")), 0, wxALIGN_CENTER | wxLEFT | wxRIGHT, margin);
      bs->Add( 20, 0 );
   }

   if( extra )
   {
      bs->Add( extra, 0, wxALIGN_CENTER | wxLEFT | wxRIGHT, margin );
      bs->Add( 40, 0 );
   }

   bs->AddStretchSpacer();
   bs->Realize();

   // Add any buttons that need to cuddle up to the right hand cluster
   if( buttons & eDebugButton )
   {
      size_t lastLastSpacer = 0;
      size_t lastSpacer = 0;
      wxSizerItemList & list = bs->GetChildren();
      for( size_t i = 0, cnt = list.GetCount(); i < cnt; i++ )
      {
         if( list[i]->IsSpacer() )
         {
            lastSpacer = i;
         }
         else  
         {
            lastLastSpacer = lastSpacer;
         }
      }

      b = safenew wxButton(parent, eDebugID, _("Debu&g"));
      bs->Insert( lastLastSpacer + 1, b, 0, wxALIGN_CENTER | wxLEFT | wxRIGHT, margin );
   }

   auto s = std::make_unique<wxBoxSizer>( wxVERTICAL );
   s->Add( bs.release(), 1, wxEXPAND | wxALL, 7 );
   s->Add( 0, 3 );   // a little extra space

   return std::unique_ptr<wxSizer>{ s.release() };
}

void ShuttleGui::AddStandardButtons(long buttons, wxButton *extra)
{
   if( mShuttleMode != eIsCreating )
      return;

   StartVerticalLay( false );

   miSizerProp = false;
   mpSubSizer = CreateStdButtonSizer( mpParent, buttons, extra );
   UpdateSizers();
   PopSizer();

   EndVerticalLay();
}

wxSizerItem * ShuttleGui::AddSpace( int width, int height )
{
   if( mShuttleMode != eIsCreating )
      return NULL;

   return mpSizer->Add( width, height, 0);
}

void ShuttleGui::SetSizeHints( wxWindow *window, const wxArrayString & items )
{
   int maxw = 0;

   for( size_t i = 0; i < items.GetCount(); i++ )
   {
      int x;
      int y;

      window->GetTextExtent(items[i], &x, &y );
      if( x > maxw )
      {
         maxw = x;
      }
   }

   // Would be nice to know the sizes of the button and borders, but this is
   // the best we can do for now.
#if defined(__WXMAC__)
   maxw += 50;
#elif defined(__WXMSW__)
   maxw += 50;
#elif defined(__WXGTK__)
   maxw += 50;
#else
   maxw += 50;
#endif

   window->SetSizeHints( maxw, -1 );
}

void ShuttleGui::SetSizeHints( wxWindow *window, const wxArrayInt & items )
{
   wxArrayString strs;

   for( size_t i = 0; i < items.GetCount(); i++ )
   {
      strs.Add( wxString::Format( wxT("%d"), items[i] ) );
   }

   SetSizeHints( window, strs );
}

void ShuttleGui::SetSizeHints( const wxArrayString & items )
{
   if( mShuttleMode != eIsCreating )
      return;

   SetSizeHints( mpLastWind, items );
}

void ShuttleGui::SetSizeHints( const wxArrayInt & items )
{
   if( mShuttleMode != eIsCreating )
      return;

   SetSizeHints( mpLastWind, items );
}

void ShuttleGui::SetSizeHints( int minX, int minY )
{
   ShuttleGuiBase::SetSizeHints( minX, minY );
}
