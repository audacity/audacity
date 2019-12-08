/**********************************************************************

  Audacity: A Digital Audio Editor

  ExpandingToolBar.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPANDING_TOOL_BAR__
#define __AUDACITY_EXPANDING_TOOL_BAR__

#include <vector>
#include <wx/defs.h>
#include <wx/dragimag.h> // use macros and typedefs in this header
#include <wx/timer.h> // member variable
#include <wx/minifram.h> // to inherit

#include "wxPanelWrapper.h" // to inherit

#include <unordered_map>

class AButton;

class ExpandingToolBar;
class ImageRollPanel;
class ToolBarFrame;
class ToolBarDialog;
class ToolBarArea;
class ToolBarGrabber;

class ToolBarArrangement;

using WindowHash = std::unordered_map<void*, int>;

class ExpandingToolBarEvtHandler;

//
// A smart ToolBar class that has a "MainPanel" which is always
// displayed, and an "ExtraPanel" that can be hidden to save space.
// Can be docked into a ToolBarArea or floated in an ToolBarFrame;
//

class ExpandingToolBar final : public wxPanelWrapper
{
 public:
   DECLARE_DYNAMIC_CLASS(ExpandingToolBar)

   ExpandingToolBar(wxWindow* parent, wxWindowID id,
                    const wxPoint& pos = wxDefaultPosition,
                    const wxSize& size = wxDefaultSize);

   virtual ~ExpandingToolBar();

   wxPanel *GetMainPanel() { return mMainPanel; }
   wxPanel *GetExtraPanel() { return mExtraPanel; }

   void SetAutoExpand(bool enabled) { mAutoExpand = enabled; }
   bool GetAutoExpand() { return mAutoExpand; }

   void Expand();
   void Collapse(bool now = false);

   bool IsExpanded() { return mIsExpanded; }

   void OnSize(wxSizeEvent &evt);
   void OnToggle(wxCommandEvent &evt);
   void OnTimer(wxTimerEvent &evt);

   void StartMoving();
   void UpdateMoving();
   void FinishMoving();

   bool Layout() override;
   void Fit() override;

 protected:
   void RecursivelyPushEventHandlers(wxWindow *win);
   bool IsCursorInWindow();
   void ReparentExtraPanel();
   void MoveDrawer(wxSize prevSize);
   wxBitmap GetToolbarBitmap();

   void TryAutoExpand();
   void TryAutoCollapse();

   wxPanel *mMainPanel;
   wxPanel *mExtraPanel;
   ToolBarGrabber *mGrabber;
   AButton *mToggleButton;
   bool mIsAutoExpanded;
   bool mIsManualExpanded;
   bool mIsExpanded;
   bool mAutoExpand;
   bool mFirstTime;
   wxSize mMainSize;
   wxSize mExtraSize;
   wxSize mButtonSize;
   wxSize mGrabberSize;
   wxSize mCurrentDrawerSize;
   wxSize mTargetDrawerSize;
   wxSize mCurrentTotalSize;
   WindowHash mWindowHash;
   wxTimer mTimer;
   ToolBarFrame *mFrameParent;
   ToolBarDialog *mDialogParent;
   ToolBarArea *mAreaParent;
   std::unique_ptr<ToolBarArrangement> mSavedArrangement;
   ImageRollPanel *mTargetPanel;
   std::unique_ptr<wxDragImage> mDragImage;
   wxWindow *mTopLevelParent;
   std::vector<wxRect> mDropTargets;
   wxRect mDropTarget;

   static int msNoAutoExpandStack;

   DECLARE_EVENT_TABLE()

   friend class ExpandingToolBarEvtHandler;
   std::vector< std::unique_ptr< ExpandingToolBarEvtHandler > > mHandlers;
};

class ToolBarGrabber final : public wxPanelWrapper
{
 public:
   DECLARE_DYNAMIC_CLASS(ToolBarGrabber)

   ToolBarGrabber(wxWindow *parent,
                  wxWindowID id,
                  ExpandingToolBar *ownerToolbar,
                  const wxPoint& pos = wxDefaultPosition,
                  const wxSize& size = wxDefaultSize);

   void OnPaint(wxPaintEvent &evt);
   void OnSize(wxSizeEvent &evt);
   void OnMouse(wxMouseEvent &evt);

 protected:
   int               mState;
   //ImageRoll         mImageRoll[2];
   ExpandingToolBar *mOwnerToolBar;

   DECLARE_EVENT_TABLE()
};

class ToolBarDialog final : public wxDialogWrapper
{
 public:
   DECLARE_DYNAMIC_CLASS(ToolBarDialog)

   ToolBarDialog(wxWindow* parent,
                 wxWindowID id,
                 const TranslatableString& name = {},
                 const wxPoint& pos = wxDefaultPosition);

   ~ToolBarDialog();

   void Fit() override;

   void SetChild(ExpandingToolBar *child);

 protected:
   ExpandingToolBar *mChild;

   DECLARE_EVENT_TABLE()
};

class ToolBarFrame final : public wxMiniFrame
{
 public:
   DECLARE_DYNAMIC_CLASS(ToolBarFrame)

   ToolBarFrame(wxWindow* parent,
                wxWindowID id,
                const wxString& name = {},
                const wxPoint& pos = wxDefaultPosition);

   ~ToolBarFrame();

   void Fit() override;

   void SetChild(ExpandingToolBar *child);

 protected:
   ExpandingToolBar *mChild;

   DECLARE_EVENT_TABLE()
};

//
// Note: with a ToolBarArea, the parent sets the width, but the
// ToolBarArea sets the height dynamically based on the number of
// toolbars it contains.
//
class ToolBarArea final : public wxPanelWrapper
{
 public:
   DECLARE_DYNAMIC_CLASS(ToolBarArea)

   ToolBarArea(wxWindow* parent,
               wxWindowID id,
               const wxPoint& pos = wxDefaultPosition,
               const wxSize& size = wxDefaultSize);
   ~ToolBarArea();

   bool Layout() override;
   void Fit() override;

   void OnSize(wxSizeEvent &evt);
   void OnMouse(wxMouseEvent &evt);

   void CollapseAll(bool now = false);

   // Does not add or DELETE the window, just relates to layout...
   void AddChild(ExpandingToolBar *child);
   void RemoveChild(ExpandingToolBar *child);

   std::unique_ptr<ToolBarArrangement> SaveArrangement();
   void RestoreArrangement(std::unique_ptr<ToolBarArrangement>&& arrangement);

   std::vector<wxRect> GetDropTargets();
   void MoveChild(ExpandingToolBar *child, wxRect dropTarget);

   void SetCapturedChild(ExpandingToolBar *child);

 protected:
   void ContractRow(int rowIndex);
   bool ExpandRow(int rowIndex);
   void LayoutOne(int childIndex);
   void AdjustLayout();
   void Fit(bool horizontal, bool vertical);

   std::vector<ExpandingToolBar*> mChildArray;
   std::vector<int>         mRowArray;
   wxSize                   mLastLayoutSize;
   bool                     mInOnSize;

   ExpandingToolBar        *mCapturedChild;

   wxSize                   mMinSize;
   wxSize                   mMaxSize;
   wxSize                   mActualSize;

   std::vector<wxRect>      mDropTargets;
   std::vector<int>         mDropTargetIndices;
   std::vector<int>         mDropTargetRows;

   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_EXPANDING_TOOL_BAR__
