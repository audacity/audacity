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

#include "MemoryX.h"
#include <wx/grid.h>
#include <wx/sizer.h>
#include <wx/string.h>

#include "WrappedType.h"

const int nMaxNestedSizers = 20;

enum teShuttleMode
{
   eIsCreating,
   eIsGettingFromDialog,
   eIsSettingToDialog,
   eIsSavingViaShuttle,
   eIsGettingViaShuttle,

   // Next two are only ever seen in constructor.
   // After that they revert to one of the modes above.
   // They are used to achieve 'two step' operation,
   // where we transfer between two shuttles in one go.
   eIsCreatingFromPrefs,
   eIsSavingToPrefs
};

class wxArrayInt;
class wxListCtrl;
class wxCheckBox;
class wxChoice;
class wxComboBox;
class wxScrolledWindow;
class wxStaticText;
class wxTreeCtrl;
class wxTextCtrl;
class wxSlider;
class wxTreeListCtrl;
class wxNotebook;
typedef wxWindow wxNotebookPage;  // so far, any window can be a page
class wxButton;
class wxBitmapButton;
class wxRadioButton;
class wxBitmap;
class wxPanel;
class wxSizer;
class wxStaticBox;
class wxMenuBar;
class wxMenu;
class wxSpinCtrl;
class wxListBox;
class wxGrid;
class Shuttle;

class WrappedType;

class AUDACITY_DLL_API ShuttleGuiBase /* not final */
{
public:
   ShuttleGuiBase(wxWindow * pParent,teShuttleMode ShuttleMode);
   ~ShuttleGuiBase(void);
   void Init();

//-- Add functions.  These only add a widget or 2.
   void AddPrompt(const wxString &Prompt);
   void AddUnits(const wxString &Prompt);
   void AddTitle(const wxString &Prompt);
   wxWindow * AddWindow(wxWindow * pWindow, int Flags = wxALIGN_CENTRE | wxALL );
   wxSlider * AddSlider(const wxString &Prompt, int pos, int Max, int Min = 0);
   wxSlider * AddVSlider(const wxString &Prompt, int pos, int Max);
   wxSpinCtrl * AddSpinCtrl(const wxString &Prompt, int Value, int Max, int Min);
   wxTreeCtrl * AddTree();
   wxRadioButton * AddRadioButton( const wxString & Prompt );
   wxRadioButton * AddRadioButtonToGroup( const wxString & Prompt);
   wxButton * AddButton( const wxString & Text, int PositionFlags = wxALIGN_CENTRE );
   wxBitmapButton * AddBitmapButton(const wxBitmap &Bitmap, int PositionFlags = wxALIGN_CENTRE);
   wxStaticText * AddVariableText(const wxString &Str, bool bCenter = false, int PositionFlags = 0);
   wxTextCtrl * AddTextBox(const wxString &Caption, const wxString &Value, const int nChars);
   wxTextCtrl * AddNumericTextBox(const wxString &Caption, const wxString &Value, const int nChars);
   wxTextCtrl * AddTextWindow(const wxString &Value);
   wxListBox * AddListBox(const wxArrayString * pChoices, long style = 0);
   wxListCtrl * AddListControl();
   wxListCtrl * AddListControlReportMode();
   wxGrid * AddGrid();
   wxCheckBox * AddCheckBox( const wxString &Prompt, const wxString &Selected);
   wxCheckBox * AddCheckBoxOnRight( const wxString &Prompt, const wxString &Selected);
   wxComboBox * AddCombo( const wxString &Prompt, const wxString &Selected,const wxArrayString * pChoices, long style = 0 );
   wxChoice   * AddChoice( const wxString &Prompt, const wxString &Selected, const wxArrayString * pChoices );
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
   wxNotebookPage * StartNotebookPage( const wxString & Name );
   void StartNotebookPage( const wxString & Name, wxNotebookPage * pPage );
   void EndNotebookPage();
   wxPanel * StartInvisiblePanel();
   void EndInvisiblePanel();

   void StartRadioButtonGroup( const wxString & SettingName );
   void EndRadioButtonGroup();

   void StartRadioButtonGroup( const wxString & SettingName, const int iDefaultValue );
   void StartRadioButtonGroup( const wxString & SettingName, const wxString &DefaultValue );

   void DoDataShuttle( const wxString &Name, WrappedType & WrappedRef );

   bool DoStep( int iStep );
   int TranslateToIndex( const wxString &Value, const wxArrayString &Choices );
   wxString TranslateFromIndex( const int nIn, const wxArrayString &Choices );
   int TranslateToIndex( const int Value, const wxArrayInt &Choices );
   int TranslateFromIndex( const int nIn, const wxArrayInt &Choices );

//-- Tie functions both add controls and also read/write to them.
// The ones taking a 'WrappedType' are type-generic and are used by the type specific ones.

   wxTextCtrl * TieTextBox( const wxString &Prompt, WrappedType &  WrappedRef, const int nChars);
   wxTextCtrl * TieTextBox( const wxString &Caption, wxString & Value, const int nChars=0);
   wxTextCtrl * TieTextBox( const wxString &Prompt, int &Selected, const int nChars=0);
   wxTextCtrl * TieTextBox( const wxString &Prompt, double &Value, const int nChars=0);

   wxTextCtrl * TieNumericTextBox( const wxString &Prompt, WrappedType &  WrappedRef, const int nChars);
   wxTextCtrl * TieNumericTextBox( const wxString &Caption, wxString & Value, const int nChars=0);
   wxTextCtrl * TieNumericTextBox( const wxString &Prompt, int &Selected, const int nChars=0);
   wxTextCtrl * TieNumericTextBox( const wxString &Prompt, double &Value, const int nChars=0);

   wxCheckBox * TieCheckBox( const wxString &Prompt, WrappedType & WrappedRef );
   wxCheckBox * TieCheckBox( const wxString &Prompt, const wxString &Selected );
   wxCheckBox * TieCheckBox( const wxString &Prompt, bool & Var );
   wxCheckBox * TieCheckBoxOnRight( const wxString & Prompt, WrappedType & WrappedRef );
   wxCheckBox * TieCheckBoxOnRight( const wxString & Prompt, bool & Var );

   wxChoice * TieChoice( const wxString &Prompt, WrappedType & WrappedRef, const wxArrayString * pChoices );
   wxChoice * TieChoice( const wxString &Prompt, wxString &Selected, const wxArrayString * pChoices );
   wxChoice * TieChoice( const wxString &Prompt, int &Selected, const wxArrayString * pChoices );

   wxSlider * TieSlider( const wxString &Prompt, WrappedType & WrappedRef, const int max, const int min = 0 );
   wxSlider * TieSlider( const wxString &Prompt, int &pos, const int max, const int min = 0);
   wxSlider * TieSlider( const wxString &Prompt, double &pos, const double max, const double min = 0.0);
   wxSlider * TieSlider( const wxString &Prompt, float &pos, const float fMin, const float fMax);
   wxSlider * TieVSlider( const wxString &Prompt, float &pos, const float fMin, const float fMax);

   wxRadioButton * TieRadioButton( const wxString & Prompt, WrappedType &WrappedRef);
   wxRadioButton * TieRadioButton( const wxString &Prompt, const int iValue);
   wxRadioButton * TieRadioButton( const wxString &Prompt, const wxString &Value);

   wxSpinCtrl * TieSpinCtrl( const wxString &Prompt, WrappedType & WrappedRef, const int max, const int min = 0 );
   wxSpinCtrl * TieSpinCtrl( const wxString &Prompt, int &Value, const int max, const int min = 0 );


//-- Variants of the standard Tie functions which do two step exchange in one go
// Note that unlike the other Tie functions, ALL the arguments are const.
// That's because the data is being exchanged between the dialog and mpShuttle
// so it doesn't need an argument that is writeable.
   wxCheckBox * TieCheckBox(
      const wxString &Prompt,
      const wxString &SettingName,
      const bool bDefault);
   wxCheckBox * TieCheckBoxOnRight(
      const wxString &Prompt,
      const wxString &SettingName,
      const bool bDefault);
   wxChoice * TieChoice(
      const wxString &Prompt,
      const wxString &SettingName,
      const wxString &Default,
      const wxArrayString &Choices,
      const wxArrayString & TranslatedChoices );
   wxChoice * TieChoice(
      const wxString &Prompt,
      const wxString &SettingName,
      const int Default,
      const wxArrayString & Choices,
      const wxArrayInt & TranslatedChoices);
   wxTextCtrl * TieTextBox(
      const wxString &Prompt,
      const wxString &SettingName,
      const wxString &Default,
      const int nChars);
   wxTextCtrl * TieTextBox(
      const wxString & Prompt,
      const wxString & SettingName,
      const double & Default,
      const int nChars);
   wxTextCtrl * TieNumericTextBox(
      const wxString &Prompt,
      const wxString &SettingName,
      const wxString &Default,
      const int nChars);
   wxTextCtrl * TieNumericTextBox(
      const wxString & Prompt,
      const wxString & SettingName,
      const double & Default,
      const int nChars);
   wxSlider * TieSlider(
      const wxString & Prompt,
      const wxString & SettingName,
      const int iDefault,
      const int max,
      const int min = 0);
   wxSpinCtrl * TieSpinCtrl(
      const wxString &Prompt,
      const wxString &SettingName,
      const int Value,
      const int max,
      const int min);
//-- End of variants.
   void EnableCtrl( bool bEnable );
   void SetSizeHints( int minX, int minY );
   void SetBorder( int Border ) {miBorder = Border;};
   void SetStyle( int Style ) {miStyle = Style;};
   void SetNoMatchSelector( int iSelector ) {miNoMatchSelector = iSelector;};
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
   int GetId() {return miIdNext;};
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

   wxWindow * mpLastWind;
   wxWindow * mpDlg;
   wxSizer * pSizerStack[ nMaxNestedSizers ];
   wxString mBoxName;

   std::unique_ptr<Shuttle> mpShuttle; /*! Controls source/destination of shuttled data.  You can
   leave this NULL if you are shuttling to variables */
   int miNoMatchSelector; //! Used in choices to determine which item to use on no match.

   teShuttleMode mShuttleMode;

   // These five are needed to handle radio button groups.
   wxString mSettingName; /// The setting controlled by a group.
   int mRadioCount;       /// The index of this radio item.  -1 for none.

   WrappedType mRadioValue;  /// The wrapped value associated with the active radio button.
   wxString mRadioValueString; /// Unwrapped string value.
   int mRadioValueInt;         /// Unwrapped integer value.

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

   std::unique_ptr<wxSizer> mpSubSizer;
   wxSizer * mpSizer;
   wxWindow * mpParent;
   wxWindow * mpWind;
   wxMenuBar * mpMenuBar;
   wxMenu * mpMenu;
};

// A rarely used helper function that sets a pointer
// ONLY if the value it is to be set to is non NULL.
extern void SetIfCreated( wxChoice *&Var, wxChoice * Val );
extern void SetIfCreated( wxTextCtrl *&Var, wxTextCtrl * Val );
extern void SetIfCreated( wxStaticText *&Var, wxStaticText * Val );

class GuiWaveTrack;
class RulerPanel;
class AttachableScrollBar;
class ViewInfo;
#include <wx/scrolbar.h>  // to get wxSB_HORIZONTAL

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
   ePreviewID     = wxID_LOWEST - 1,
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
   ShuttleGui & Id(int id );
   // Prop() sets the proportion value, defined as in wxSizer::Add().
   ShuttleGui & Prop( int iProp ){ ShuttleGuiBase::Prop(iProp); return *this;}; // Has to be here too, to return a ShuttleGui and not a ShuttleGuiBase.
   GuiWaveTrack * AddGuiWaveTrack( const wxString & Name);
   RulerPanel * AddRulerVertical( float low, float hi, const wxString & Units );
   AttachableScrollBar * AddAttachableScrollBar( long style = wxSB_HORIZONTAL );
   void AddStandardButtons( long buttons = eOkButton | eCancelButton, wxButton *extra = NULL );
   wxSizerItem * AddSpace( int width, int height );
   wxSizerItem * AddSpace( int size ) { return AddSpace( size, size ); };
   int GetBorder() { return miBorder; };

   void SetSizeHints( int minX = -1, int minY = -1 );
   void SetSizeHints( const wxArrayString & items );
   void SetSizeHints( const wxArrayInt & items );
   static void SetSizeHints( wxWindow *window, const wxArrayString & items );
   static void SetSizeHints( wxWindow *window, const wxArrayInt & items );

   teShuttleMode GetMode() { return  mShuttleMode; };
};
#endif
