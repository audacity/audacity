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
#include "../Theme.h"
#include "../widgets/wxPanelWrapper.h" // to inherit
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

//
// ToolBar IDs
//
enum ToolBarID
{
   NoBarID = -1,
   TransportBarID,
   ToolsBarID,
   MeterBarID,
   RecordMeterBarID,
   PlayMeterBarID,
   MixerBarID,
   EditBarID,
   TranscriptionBarID,
   ScrubbingBarID,
   DeviceBarID,
   SelectionBarID,
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   SpectralSelectionBarID,
#endif
   TimeBarID,
   ToolBarCount
};

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
      int type, const TranslatableString & label, const wxString & section,
      bool resizable = false);
   virtual ~ToolBar();

   bool AcceptsFocus() const override { return false; };

   virtual void SetToDefaultSize();
   //NEW virtuals:
   virtual void Create(wxWindow *parent);
   virtual void EnableDisableButtons() = 0;
   virtual void ReCreateButtons();
   void UpdatePrefs() override;
   virtual void RegenerateTooltips() = 0;

   int GetType();
   TranslatableString GetTitle();
   TranslatableString GetLabel();
   wxString GetSection();
   ToolDock *GetDock();

private:
   void SetLabel(const wxString & label) override;
public:
   void SetLabel(const TranslatableString & label);
   virtual void SetDocked(ToolDock *dock, bool pushed);

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
   static void MakeRecoloredImage(teBmps eBmpOut, teBmps eBmpIn);

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
   wxString mSection;
   int mType;
 private:
   void Init(wxWindow *parent, int type, const wxString & title, const wxString & label);

   wxWindow *mParent;

   Grabber *mGrabber;
   ToolBarResizer *mResizer;

   wxBoxSizer *mHSizer;

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

   RegisteredToolbarFactory( int id, const Function &function );

   static const Functions &GetFactories();
};

#endif
