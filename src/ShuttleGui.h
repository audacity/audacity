/**********************************************************************

  Audacity: A Digital Audio Editor

  ShuttleGui.h

  James Crook

  Audacity is free software.
  This file is licensed under the wxWidgets license, see License.txt

**********************************************************************/

#ifndef SHUTTLE_GUI
#define SHUTTLE_GUI

#include "Audacity.h"
#include "audacity/Types.h"

#include <vector>
#include <wx/slider.h> // to inherit
#include "MemoryX.h"
#include <wx/listbase.h> // for wxLIST_FORMAT_LEFT

#include "WrappedType.h"

class ChoiceSetting;

class wxArrayStringEx;


const int nMaxNestedSizers = 20;

enum teShuttleMode
{
   eIsCreating,
   eIsGettingFromDialog,
   eIsSettingToDialog,
   eIsSavingViaShuttle,
   eIsGettingViaShuttle,
   eIsGettingMetadata,

   // Next two are only ever seen in constructor.
   // After that they revert to one of the modes above.
   // They are used to achieve 'two step' operation,
   // where we transfer between two shuttles in one go.
   eIsCreatingFromPrefs,
   eIsSavingToPrefs
};

class wxListCtrl;
class wxCheckBox;
class wxChoice;
class wxComboBox;
class wxScrolledWindow;
class wxStaticText;
class wxTreeCtrl;
class wxTextCtrl;
class wxSlider;
class wxNotebook;
typedef wxWindow wxNotebookPage;  // so far, any window can be a page
class wxButton;
class wxBitmapButton;
class wxRadioButton;
class wxBitmap;
class wxPanel;
class wxSizer;
class wxSizerItem;
class wxStaticBox;
class wxMenuBar;
class wxMenu;
class wxSpinCtrl;
class wxListBox;
class wxGrid;
class Shuttle;

class WrappedType;

#ifdef __WXMAC__

#include <wx/statbox.h> // to inherit

class wxStaticBoxWrapper
   : public wxStaticBox // inherit to get access to m_container
{
public:
   template< typename... Args >
   wxStaticBoxWrapper( Args &&...args )
      : wxStaticBox( std::forward<Args>(args)... )
   {
      m_container.EnableSelfFocus();
   }
};

/// Fix a defect in TAB key navigation to sliders, known to happen in wxWidgets
/// 3.1.1 and maybe in earlier versions
class wxSliderWrapper : public wxSlider
{
public:
   using wxSlider::wxSlider;
   void SetFocus() override;
};
#else
using wxStaticBoxWrapper = wxStaticBox;
using wxSliderWrapper = wxSlider;
#endif

template< typename T > class SettingSpec {
public:
   SettingSpec( const RegistryPath &path, const T &defaultValue = {} )
      : mPath{ path }, mDefaultValue{ defaultValue }
   {}

   const RegistryPath &GetPath() const { return mPath; }
   const T &GetDefault() const { return mDefaultValue; }

private:
   RegistryPath mPath;
   T mDefaultValue;
};

class AUDACITY_DLL_API ShuttleGuiBase /* not final */
{
public:
   ShuttleGuiBase(wxWindow * pParent,teShuttleMode ShuttleMode);
   virtual ~ShuttleGuiBase();
   void Init();
   void ResetId();

//-- Add functions.  These only add a widget or 2.
   void HandleOptionality(const wxString &Prompt);
   void AddPrompt(const wxString &Prompt);
   void AddUnits(const wxString &Prompt);
   void AddTitle(const wxString &Prompt);
   // Applies wxALL (which affects borders) only when in Flags:
   wxWindow * AddWindow(wxWindow * pWindow, int Flags = wxALIGN_CENTRE | wxALL );
   wxSlider * AddSlider(const wxString &Prompt, int pos, int Max, int Min = 0);
   wxSlider * AddVSlider(const wxString &Prompt, int pos, int Max);
   wxSpinCtrl * AddSpinCtrl(const wxString &Prompt, int Value, int Max, int Min);
   wxTreeCtrl * AddTree();

   // Pass the same initValue to the sequence of calls to AddRadioButton and
   // AddRadioButtonToGroup.
   // The radio button is filled if selector == initValue
   wxRadioButton * AddRadioButton(
      const wxString & Prompt, int selector = 0, int initValue = 0 );
   wxRadioButton * AddRadioButtonToGroup(
      const wxString & Prompt, int selector = 1, int initValue = 0 );

   // Only the last button specified as default (if more than one) will be
   // Always ORs the flags with wxALL (which affects borders):
   wxButton * AddButton(
      const wxString & Text, int PositionFlags = wxALIGN_CENTRE,
      bool setDefault = false );
   // Only the last button specified as default (if more than one) will be
   // Always ORs the flags with wxALL (which affects borders):
   wxBitmapButton * AddBitmapButton(
      const wxBitmap &Bitmap, int PositionFlags = wxALIGN_CENTRE,
      bool setDefault = false );
   // When PositionFlags is 0, applies wxALL (which affects borders),
   // and either wxALIGN_CENTER (if bCenter) or else wxEXPAND
   wxStaticText * AddVariableText(const wxString &Str, bool bCenter = false, int PositionFlags = 0);
   wxTextCtrl * AddTextBox(const wxString &Caption, const wxString &Value, const int nChars);
   wxTextCtrl * AddNumericTextBox(const wxString &Caption, const wxString &Value, const int nChars);
   wxTextCtrl * AddTextWindow(const wxString &Value);
   wxListBox * AddListBox(const wxArrayStringEx &choices, long style = 0);

   struct ListControlColumn{
      ListControlColumn(
         wxString h, int f = wxLIST_FORMAT_LEFT, int w = wxLIST_AUTOSIZE)
         : heading(h), format(f), width(w)
      {}

      wxString heading;
      int format;
      int width;
   };
   wxListCtrl * AddListControl(
      std::initializer_list<const ListControlColumn> columns = {},
      long listControlStyles = 0
   );
   wxListCtrl * AddListControlReportMode(
      std::initializer_list<const ListControlColumn> columns = {},
      long listControlStyles = 0
   );

   wxGrid * AddGrid();
   wxCheckBox * AddCheckBox( const wxString &Prompt, bool Selected);
   wxCheckBox * AddCheckBoxOnRight( const wxString &Prompt, bool Selected);
   wxComboBox * AddCombo( const wxString &Prompt, const wxString &Selected,const wxArrayStringEx & choices, long style = 0 );
   wxChoice   * AddChoice( const wxString &Prompt,
      const wxArrayStringEx &choices, int Selected = -1 );
   wxChoice   * AddChoice( const wxString &Prompt,
      const wxArrayStringEx &choices, const wxString &selected );
   wxMenuBar  * AddMenuBar( );
   wxMenu     * AddMenu( const wxString & Title );
   void AddIcon( wxBitmap * pBmp);
   void AddIconButton( const wxString & Command, const wxString & Params,wxBitmap * pBmp );
   void AddFixedText( const wxString & Str, bool bCenter = false );
   void AddConstTextBox( const wxString &Caption, const wxString & Value );

//-- Start and end functions.  These are used for sizer, or other window containers
//   and create the appropriate widget.
   void StartHorizontalLay(int PositionFlags=wxALIGN_CENTRE, int iProp=1);
   void EndHorizontalLay();
   void StartVerticalLay(int iProp=1);
   void StartVerticalLay(int PositionFlags, int iProp);

   void EndVerticalLay();
   wxScrolledWindow * StartScroller(int iStyle=0);
   void EndScroller();
   wxPanel * StartPanel(int iStyle=0);
   void EndPanel();
   void StartMultiColumn(int nCols, int PositionFlags=wxALIGN_LEFT);
   void EndMultiColumn();

   void StartTwoColumn() {StartMultiColumn(2);};
   void EndTwoColumn() {EndMultiColumn();};
   void StartThreeColumn(){StartMultiColumn(3);};
   void EndThreeColumn(){EndMultiColumn();};

   wxStaticBox * StartStatic( const wxString & Str, int iProp=0 );
   void EndStatic();

   wxNotebook * StartNotebook();
   void EndNotebook();

   // IDs of notebook pages cannot be chosen by the caller
   wxNotebookPage * StartNotebookPage( const wxString & Name );
   void StartNotebookPage( const wxString & Name, wxNotebookPage * pPage );

   void EndNotebookPage();
   wxPanel * StartInvisiblePanel();
   void EndInvisiblePanel();

   // SettingName is a key in Preferences.
   void StartRadioButtonGroup( const ChoiceSetting &Setting );
   void EndRadioButtonGroup();

   bool DoStep( int iStep );
   int TranslateToIndex( const wxString &Value, const wxArrayStringEx &Choices );
   wxString TranslateFromIndex( const int nIn, const wxArrayStringEx &Choices );

//-- Tie functions both add controls and also read/write to them.

   wxTextCtrl * TieTextBox( const wxString &Caption, wxString & Value, const int nChars=0);
   wxTextCtrl * TieTextBox( const wxString &Prompt, int &Selected, const int nChars=0);
   wxTextCtrl * TieTextBox( const wxString &Prompt, double &Value, const int nChars=0);

   wxTextCtrl * TieNumericTextBox( const wxString &Prompt, int &Value, const int nChars=0);
   wxTextCtrl * TieNumericTextBox( const wxString &Prompt, double &Value, const int nChars=0);

   wxCheckBox * TieCheckBox( const wxString &Prompt, bool & Var );
   wxCheckBox * TieCheckBoxOnRight( const wxString & Prompt, bool & Var );

   wxChoice * TieChoice( const wxString &Prompt, wxString &Selected, const wxArrayStringEx &choices );
   wxChoice * TieChoice( const wxString &Prompt, int &Selected, const wxArrayStringEx &choices );

   wxSlider * TieSlider( const wxString &Prompt, int &pos, const int max, const int min = 0);
   wxSlider * TieSlider( const wxString &Prompt, double &pos, const double max, const double min = 0.0);
   wxSlider * TieSlider( const wxString &Prompt, float &pos, const float fMin, const float fMax);
   wxSlider * TieVSlider( const wxString &Prompt, float &pos, const float fMin, const float fMax);

   // Must be called between a StartRadioButtonGroup / EndRadioButtonGroup pair,
   // and as many times as there are values in the enumeration.
   wxRadioButton * TieRadioButton();

   wxSpinCtrl * TieSpinCtrl( const wxString &Prompt, int &Value, const int max, const int min = 0 );


//-- Variants of the standard Tie functions which do two step exchange in one go
// Note that unlike the other Tie functions, ALL the arguments are const.
// That's because the data is being exchanged between the dialog and mpShuttle
// so it doesn't need an argument that is writeable.
   virtual wxCheckBox * TieCheckBox(
      const wxString &Prompt,
      const SettingSpec< bool > &Setting);
   virtual wxCheckBox * TieCheckBoxOnRight(
      const wxString &Prompt,
      const SettingSpec< bool > &Setting);

   virtual wxChoice *TieChoice(
      const wxString &Prompt,
      const ChoiceSetting &choiceSetting );

   // This overload presents what is really a numerical setting as a choice among
   // commonly used values, but the choice is not necessarily exhaustive.
   // This behaves just like the previous for building dialogs, but the
   // behavior is different when the call is intercepted for purposes of
   // emitting scripting information about Preferences.
   virtual wxChoice * TieNumberAsChoice(
      const wxString &Prompt,
      const SettingSpec< int > &Setting,
      const TranslatableStrings & Choices,
      const std::vector<int> * pInternalChoices = nullptr,
      int iNoMatchSelector = 0 );

   virtual wxTextCtrl * TieTextBox(
      const wxString &Prompt,
      const SettingSpec< wxString > &Setting,
      const int nChars);
   virtual wxTextCtrl * TieIntegerTextBox(
      const wxString & Prompt,
      const SettingSpec< int > &Setting,
      const int nChars);
   virtual wxTextCtrl * TieNumericTextBox(
      const wxString & Prompt,
      const SettingSpec< double > &Setting,
      const int nChars);
   virtual wxSlider * TieSlider(
      const wxString & Prompt,
      const SettingSpec< int > &Setting,
      const int max,
      const int min = 0);
   virtual wxSpinCtrl * TieSpinCtrl(
      const wxString &Prompt,
      const SettingSpec< int > &Setting,
      const int max,
      const int min);
//-- End of variants.
   void EnableCtrl( bool bEnable );
   void SetSizeHints( int minX, int minY );
   void SetBorder( int Border ) {miBorder = Border;};
   void SetStyle( int Style ) {miStyle = Style;};
   void SetSizerProportion( int iProp ) {miSizerProp = iProp;};
   void SetStretchyCol( int i );
   void SetStretchyRow( int i );

//--Some Additions since June 2007 that don't fit in elsewhere...
   wxWindow * GetParent()
   {
      // This assertion justifies the use of safenew in many places where GetParent()
      // is used to construct a window
      wxASSERT(mpParent != NULL);
      return mpParent;
   }
   ShuttleGuiBase & Prop( int iProp );
   void UseUpId();

   wxSizer * GetSizer() {return mpSizer;}

protected:
   void SetProportions( int Default );
   void PushSizer();
   void PopSizer();

   void UpdateSizersCore( bool bPrepend, int Flags );
   void UpdateSizers();
   void UpdateSizersC();
   void UpdateSizersAtStart();

   long Style( long Style );

private:
   void SetSizeHints( const wxArrayStringEx & items );

   void DoInsertListColumns(
      wxListCtrl *pListCtrl,
      long listControlStyles,
      std::initializer_list<const ListControlColumn> columns );

public:
   static void SetSizeHints( wxWindow *window, const wxArrayStringEx & items );

protected:
   wxWindow * mpLastWind;
   wxWindow *const mpDlg;
   wxSizer * pSizerStack[ nMaxNestedSizers ];

   std::unique_ptr<Shuttle> mpShuttle; /*! Controls source/destination of shuttled data.  You can
   leave this NULL if you are shuttling to variables */
   int miNoMatchSelector; //! Used in choices to determine which item to use on no match.

   teShuttleMode mShuttleMode;

   int miSizerProp;
   int mSizerDepth;
   int miBorder;
   long miStyle;
   int miProp;

   // See UseUpId() for explanation of these three.
   int miId;
   int miIdNext;
   int miIdSetByUser;
   // Proportion set by user rather than default.
   int miPropSetByUser;

   bool * mpbOptionalFlag;

   std::unique_ptr<wxSizer> mpSubSizer;
   wxSizer * mpSizer;
   wxWindow * mpParent;
   wxWindow * mpWind;
   wxMenuBar * mpMenuBar;
   wxMenu * mpMenu;

private:
   void DoDataShuttle( const wxString &Name, WrappedType & WrappedRef );
   wxCheckBox * DoTieCheckBoxOnRight( const wxString & Prompt, WrappedType & WrappedRef );
   wxTextCtrl * DoTieTextBox( const wxString &Prompt, WrappedType &  WrappedRef, const int nChars);
   wxTextCtrl * DoTieNumericTextBox( const wxString &Prompt, WrappedType &  WrappedRef, const int nChars);
   wxCheckBox * DoTieCheckBox( const wxString &Prompt, WrappedType & WrappedRef );
   wxChoice * DoTieChoice( const wxString &Prompt, WrappedType & WrappedRef, const wxArrayStringEx & choices );
   wxSlider * DoTieSlider( const wxString &Prompt, WrappedType & WrappedRef, const int max, const int min = 0 );
   wxSpinCtrl * DoTieSpinCtrl( const wxString &Prompt, WrappedType & WrappedRef, const int max, const int min = 0 );

   std::vector<EnumValueSymbol> mRadioSymbols;
   wxString mRadioSettingName; /// The setting controlled by a group.
   Maybe<WrappedType> mRadioValue;  /// The wrapped value associated with the active radio button.
   int mRadioCount;       /// The index of this radio item.  -1 for none.
   wxString mRadioValueString; /// Unwrapped string value.
   wxRadioButton * DoAddRadioButton(
      const wxString &Prompt, int style, int selector, int initValue);
};

// A rarely used helper function that sets a pointer
// ONLY if the value it is to be set to is non NULL.
extern void SetIfCreated( wxChoice *&Var, wxChoice * Val );
extern void SetIfCreated( wxTextCtrl *&Var, wxTextCtrl * Val );
extern void SetIfCreated( wxStaticText *&Var, wxStaticText * Val );

class GuiWaveTrack;
class AttachableScrollBar;
class ViewInfo;

#include <wx/defs.h>  // to get wxSB_HORIZONTAL

// CreateStdButtonSizer defs...should probably move to widgets subdir
enum
{
   eOkButton      = 0x0001,
   eCancelButton  = 0x0002,
   eYesButton     = 0x0004,
   eNoButton      = 0x0008,
   eHelpButton    = 0x0010,
   ePreviewButton = 0x0020,
   eDebugButton   = 0x0040,
   eSettingsButton= 0x0080,
   ePreviewDryButton  = 0x0100,
   eApplyButton   = 0x0200,
   eCloseButton   = 0x0400,
};

enum
{
   // ePreviewID     = wxID_LOWEST - 1,
   // But there is a wxID_PREVIEW
   ePreviewID     = wxID_PREVIEW,

   eDebugID       = wxID_LOWEST - 2,
   eSettingsID    = wxID_LOWEST - 3,
   ePreviewDryID  = wxID_LOWEST - 4,
   eCloseID       = wxID_CANCEL
};

AUDACITY_DLL_API std::unique_ptr<wxSizer> CreateStdButtonSizer( wxWindow *parent,
                               long buttons = eOkButton | eCancelButton,
                               wxWindow *extra = NULL );

// ShuttleGui extends ShuttleGuiBase with Audacity specific extensions.
class AUDACITY_DLL_API ShuttleGui /* not final */ : public ShuttleGuiBase
{
public:
   ShuttleGui(wxWindow * pParent,teShuttleMode ShuttleMode);
   ~ShuttleGui(void);
public:
   ShuttleGui & Optional( bool & bVar );
   ShuttleGui & Id(int id );
   // Prop() sets the proportion value, defined as in wxSizer::Add().
   ShuttleGui & Prop( int iProp ){ ShuttleGuiBase::Prop(iProp); return *this;}; // Has to be here too, to return a ShuttleGui and not a ShuttleGuiBase.
   GuiWaveTrack * AddGuiWaveTrack( const wxString & Name);
   AttachableScrollBar * AddAttachableScrollBar( long style = wxSB_HORIZONTAL );

   // The first of these buttons, if any, that is included will be default:
   // Apply, Yes, OK
   void AddStandardButtons(
      long buttons = eOkButton | eCancelButton, wxButton *extra = NULL );

   wxSizerItem * AddSpace( int width, int height );
   wxSizerItem * AddSpace( int size ) { return AddSpace( size, size ); };

   teShuttleMode GetMode() { return  mShuttleMode; };
};

#endif
