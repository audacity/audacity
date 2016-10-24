/**********************************************************************

  Audacity: A Digital Audio Editor

  ToolBar.h

  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_TOOLBAR__
#define __AUDACITY_TOOLBAR__

#include "../Experimental.h"

#include <vector>
#include <wx/defs.h>
#include <wx/sizer.h>

#include "../Theme.h"
#include "../widgets/wxPanelWrapper.h"

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
enum
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
   ToolBarCount
};

// How may pixels padding each side of a floating toolbar
enum { ToolBarFloatMargin = 1 };

class ToolBar /* not final */ : public wxPanelWrapper
{

 public:

   using Holder = Destroy_ptr<ToolBar>;

   ToolBar(int type, const wxString & label, const wxString & section, bool resizable = false);
   virtual ~ToolBar();

   bool AcceptsFocus() const override { return false; };

   //NEW virtuals:
   virtual void Create(wxWindow *parent);
   virtual void EnableDisableButtons() = 0;
   virtual void ReCreateButtons();
   virtual void UpdatePrefs();
   virtual void RegenerateTooltips() = 0;

   int GetType();
   wxString GetTitle();
   wxString GetLabel();
   wxString GetSection();
   ToolDock *GetDock();

   void SetLabel(const wxString & label);
   void SetDock( ToolDock *dock);

   void SetDocked(ToolDock *dock, bool pushed);

   // NEW virtual:
   virtual bool Expose(bool show = true);

   bool IsResizable() const;
   bool IsVisible() const;
   bool IsDocked() const;
   bool IsPositioned(){ return mPositioned; };
   void SetVisible( bool bVisible );
   void SetPositioned(){ mPositioned = true;};

   /// Resizable toolbars should implement this.
   // NEW virtuals:
   virtual int GetInitialWidth() { return -1; }
   virtual int GetMinToolbarWidth() { return GetInitialWidth(); }
   virtual wxSize GetDockedSize() { return GetMinSize(); }

 public:
   static
   AButton *MakeButton(wxWindow *parent,
                       teBmps eUp,
                       teBmps eDown,
                       teBmps eHilite,
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
                            teBmps eStandardUp,
                            teBmps eStandardDown,
                            teBmps eDisabled,
                            wxSize size);

   static
   void SetButtonToolTip
      (AButton &button,
       // An array, alternating user-visible strings, and
       // non-user-visible command names.  If a shortcut key is defined
       // for the command, then it is appended, parenthesized, after the
       // user-visible string.
       const std::vector<wxString> &commands,
       // If more than one pair of strings is given, then use this separator.
       const wxString &separator = wxT(" / "));

 protected:
   void SetButton(bool down, AButton *button);

   void MakeMacRecoloredImage(teBmps eBmpOut, teBmps eBmpIn);
   void MakeRecoloredImage(teBmps eBmpOut, teBmps eBmpIn);
   void MakeButtonBackgroundsLarge();
   void MakeButtonBackgroundsSmall();

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
   wxString mLabel;
   wxString mSection;
   int mType;
 private:
   void Init(wxWindow *parent, int type, const wxString & title, const wxString & label);

   wxWindow *mParent;

   Grabber *mGrabber;
   ToolBarResizer *mResizer;

   wxBoxSizer *mHSizer;
   wxSizerItem *mSpacer;

   bool mVisible;
   bool mResizable;
   bool mPositioned; // true if position floating determined.

 public:

   DECLARE_CLASS(ToolBar)
   DECLARE_EVENT_TABLE()

   friend class ToolBarResizer;
};

#endif
