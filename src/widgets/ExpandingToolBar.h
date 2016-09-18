/**********************************************************************

  Audacity: A Digital Audio Editor

  ExpandingToolBar.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPANDING_TOOL_BAR__
#define __AUDACITY_EXPANDING_TOOL_BAR__

#include "../MemoryX.h"
#include <vector>
#include <wx/defs.h>
#include <wx/dialog.h>
#include <wx/dynarray.h>
#include <wx/panel.h>
#include <wx/hashmap.h>
#include <wx/timer.h>
#include <wx/minifram.h>

#include "ImageRoll.h"
#include "wxPanelWrapper.h"

class wxDragImage;

class AButton;

class ExpandingToolBar;
class ToolBarFrame;
class ToolBarDialog;
class ToolBarArea;
class ToolBarGrabber;

class ToolBarArrangement;

WX_DECLARE_VOIDPTR_HASH_MAP(int, WindowHash);
WX_DEFINE_ARRAY(ExpandingToolBar *, ExpandingToolBarArray);
WX_DECLARE_OBJARRAY(wxRect, wxArrayRect);

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
   wxArrayRect mDropTargets;
   wxRect mDropTarget;

   static int msNoAutoExpandStack;

   DECLARE_EVENT_TABLE()

   friend class ExpandingToolBarEvtHandler;
   std::vector< movable_ptr< ExpandingToolBarEvtHandler > > mHandlers;
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
   ImageRoll         mImageRoll[2];
   ExpandingToolBar *mOwnerToolBar;

   DECLARE_EVENT_TABLE()
};

class ToolBarDialog final : public wxDialogWrapper
{
 public:
   DECLARE_DYNAMIC_CLASS(ToolBarDialog)

   ToolBarDialog(wxWindow* parent,
                 wxWindowID id,
                 const wxString& name = wxEmptyString,
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
                const wxString& name = wxEmptyString,
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

   wxArrayRect GetDropTargets();
   void MoveChild(ExpandingToolBar *child, wxRect dropTarget);

   void SetCapturedChild(ExpandingToolBar *child);

 protected:
   void ContractRow(int rowIndex);
   bool ExpandRow(int rowIndex);
   void LayoutOne(int childIndex);
   void AdjustLayout();
   void Fit(bool horizontal, bool vertical);

   ExpandingToolBarArray    mChildArray;
   wxArrayInt               mRowArray;
   wxSize                   mLastLayoutSize;
   bool                     mInOnSize;

   ExpandingToolBar        *mCapturedChild;

   wxSize                   mMinSize;
   wxSize                   mMaxSize;
   wxSize                   mActualSize;

   wxArrayRect              mDropTargets;
   wxArrayInt               mDropTargetIndices;
   wxArrayInt               mDropTargetRows;

   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_EXPANDING_TOOL_BAR__
