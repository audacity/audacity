/**********************************************************************

  Audacity: A Digital Audio Editor

  ToolBar.h

  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_TOOLBAR__
#define __AUDACITY_TOOLBAR__

#include <functional>
#include <vector>
#include <wx/defs.h>

#include "Prefs.h"
#include "Theme.h"
#include "wxPanelWrapper.h" // to inherit
#include <wx/windowptr.h>

class wxBoxSizer;
class wxDC;
class wxEraseEvent;
class wxMouseEvent;
class wxObject;
class wxPaintEvent;
class wxPoint;
class wxSize;
class wxSizeEvent;
class wxString;
class wxWindow;

class AButton;
class Grabber;
class ToolDock;

class ToolBarResizer;

#ifdef __WXMAC__
#define USE_AQUA_THEME 1
#endif

////////////////////////////////////////////////////////////
/// class ToolBar
////////////////////////////////////////////////////////////

//
// Custom event
//
DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_TOOLBAR_UPDATED, -1);

//
// Height of a single line toolbar
//
#define toolbarSingle 27

//
// Size of border around toolbars
//
#define toolbarGap 1

// How may pixels padding each side of a floating toolbar
enum { ToolBarFloatMargin = 1 };

class AudacityProject;

class AUDACITY_DLL_API ToolBar /* not final */
: public wxPanelWrapper
, protected PrefsListener
{

 public:

   using Holder = wxWindowPtr<ToolBar>;

   ToolBar( AudacityProject &project,
      const TranslatableString & label, const Identifier &section,
      bool resizable = false);
   virtual ~ToolBar();

   //! Whether the toolbar should be shown by default.  Default implementation returns true
   virtual bool ShownByDefault() const;

   //! Default implementation returns false
   virtual bool HideAfterReset() const;

   //! Identifies one of the docking areas for toolbars
   enum DockID {
      TopDockID = 1,
      BotDockID = 2
   };

   //! Which dock the toolbar defaults into.  Default implementation chooses the top dock
   virtual DockID DefaultDockID() const;

   bool AcceptsFocus() const override { return false; };
   bool AcceptsFocusFromKeyboard() const override;

   virtual void SetToDefaultSize();
   //NEW virtuals:
   virtual void Create(wxWindow *parent);
   virtual void EnableDisableButtons() = 0;
   virtual void ReCreateButtons();
   void UpdatePrefs() override;
   virtual void RegenerateTooltips() = 0;

   //! Get a value used for computing cascading positions of undocked bars
   int GetIndex() const { return mIndex; }
   //! Set a value used for computing cascading positions of undocked bars
   void SetIndex(int index) { mIndex = index; }

   TranslatableString GetTitle();
   TranslatableString GetLabel();
   Identifier GetSection();
   ToolDock *GetDock();

   void SetPreferredNeighbors(Identifier left, Identifier top = {});

private:
   void SetLabel(const wxString & label) override;
public:
   void SetLabel(const TranslatableString & label);
   virtual void SetDocked(ToolDock *dock, bool pushed);

   //! Defaults to (NoBarID, NoBarId)
   std::pair<Identifier, Identifier> PreferredNeighbors() const noexcept;

   // NEW virtual:
   virtual bool Expose(bool show = true);

   bool IsResizable() const;
   bool IsVisible() const;
   bool IsDocked() const;
   bool IsPositioned(){ return mPositioned; };
   void SetVisible( bool bVisible );
   void SetPositioned(){ mPositioned = true;};

   /// Resizable toolbars should implement these.
   // NEW virtuals:
   virtual int GetInitialWidth() { return -1; }
   virtual int GetMinToolbarWidth() { return GetInitialWidth(); }
   virtual wxSize GetDockedSize() { return GetMinSize(); }
   
   // Utility function for certain resizable toolbars.
   // Allows them to dock at normal or double size.
   wxSize GetSmartDockedSize();

 public:
   static
   AButton *MakeButton(wxWindow *parent,
                       teBmps eUp,
                       teBmps eDown,
                       teBmps eHilite,
                       teBmps eDownHi,
                       teBmps eStandardUp,
                       teBmps eStandardDown,
                       teBmps eDisabled,
                       wxWindowID id,
                       wxPoint placement,
                       bool processdownevents,
                       wxSize size);


   static
   AButton *MakeButton(ToolBar *parent,
                       teBmps eEnabledUp,
                       teBmps eEnabledDown,
                       teBmps eDisabled,
                       int id,
                       bool processdownevents,
                       const TranslatableString &label);

   static
   void MakeAlternateImages(AButton &button, int idx,
                            teBmps eUp,
                            teBmps eDown,
                            teBmps eHilite,
                            teBmps eDownHi,
                            teBmps eStandardUp,
                            teBmps eStandardDown,
                            teBmps eDisabled,
                            wxSize size);

   static
   void SetButtonToolTip
      (AudacityProject &project, AButton &button,
       // If a shortcut key is defined for the command, then it is appended,
       // parenthesized, after the translated name.
       const ComponentInterfaceSymbol commands[], size_t nCommands);

   static void MakeButtonBackgroundsSmall();
   static void MakeButtonBackgroundsLarge();
   virtual void ResizingDone() {};

 protected:
   void SetButton(bool down, AButton *button);

   static void MakeMacRecoloredImage(teBmps eBmpOut, teBmps eBmpIn);
   static void MakeMacRecoloredImageSize(teBmps eBmpOut, teBmps eBmpIn, const wxSize& size);
   static void MakeRecoloredImage(teBmps eBmpOut, teBmps eBmpIn);
   static void MakeRecoloredImageSize(teBmps eBmpOut, teBmps eBmpIn, const wxSize& size);

   wxBoxSizer *GetSizer();

   void Add(wxWindow *window,
            int proportion = 0,
            int flag = wxALIGN_TOP,
            int border = 0,
            wxObject *userData = NULL);

   // Takes ownership of sizer
   void Add(wxSizer *sizer,
             int proportion = 0,
             int flag = 0,
             int border = 0,
             wxObject *userData = NULL);

   void Add(int width,
            int height,
            int proportion = 0,
            int flag = 0,
            int border = 0,
            wxObject *userData = NULL);

   void AddSpacer(int size = 14);
   void AddStretchSpacer(int prop = 1);

   void Detach(wxWindow *window);
   void Detach(wxSizer *sizer);

   void Updated();

   /// Returns the width in pixels of the resizer element
   int GetResizeGrabberWidth();

   virtual void Populate() = 0;
   virtual void Repaint(wxDC *dc) = 0;

   void OnErase(wxEraseEvent & event);
   void OnPaint(wxPaintEvent & event);
   void OnMouseEvents(wxMouseEvent &event);

 protected:
   AudacityProject &mProject;
   TranslatableString mLabel;
   Identifier mSection;
   int mIndex{0};
 private:
   void Init(wxWindow *parent, int type, const wxString & title, const wxString & label);

   wxWindow *mParent;

   Grabber *mGrabber;
   ToolBarResizer *mResizer;

   wxBoxSizer *mHSizer;

   Identifier mPreferredLeftNeighbor;
   Identifier mPreferredTopNeighbor;

   bool mVisible;
   bool mResizable;
   bool mPositioned; // true if position floating determined.

 public:

   DECLARE_CLASS(ToolBar)
   DECLARE_EVENT_TABLE()

   friend class ToolBarResizer;
};

struct AUDACITY_DLL_API RegisteredToolbarFactory {
   using Function = std::function< ToolBar::Holder( AudacityProject & ) >;
   using Functions = std::vector< Function >;

   RegisteredToolbarFactory( const Function &function );

   static const Functions &GetFactories();
};

#endif
