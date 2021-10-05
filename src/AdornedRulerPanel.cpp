/**********************************************************************

  Audacity: A Digital Audio Editor

  AdornedRulerPanel.cpp

  Dominic Mazzoni

*******************************************************************//**

\class AdornedRulerPanel
\brief This is an Audacity Specific ruler panel which additionally
  has border, selection markers, play marker.
  
  Once TrackPanel uses wxSizers, we will derive it from some
  wxWindow and the GetSize and SetSize functions
  will then be wxWidgets functions instead.

*//******************************************************************/


#include "AdornedRulerPanel.h"

#include <wx/app.h>
#include <wx/setup.h> // for wxUSE_* macros
#include <wx/tooltip.h>

#include "AColor.h"
#include "AllThemeResources.h"
#include "AudioIO.h"
#include "widgets/BasicMenu.h"
#include "CellularPanel.h"
#include "HitTestResult.h"
#include "Menus.h"
#include "Prefs.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectAudioManager.h"
#include "ProjectWindows.h"
#include "ProjectStatus.h"
#include "ProjectWindow.h"
#include "RefreshCode.h"
#include "SelectUtilities.h"
#include "Snap.h"
#include "Track.h"
#include "TrackPanelMouseEvent.h"
#include "UIHandle.h"
#include "ViewInfo.h"
#include "prefs/TracksPrefs.h"
#include "prefs/ThemePrefs.h"
#include "toolbars/ToolBar.h"
#include "tracks/ui/Scrubbing.h"
#include "tracks/ui/TrackView.h"
#include "widgets/AButton.h"
#include "widgets/AudacityMessageBox.h"
#include "widgets/Grabber.h"
#include "widgets/wxWidgetsWindowPlacement.h"

#include <wx/dcclient.h>
#include <wx/menu.h>

using std::min;
using std::max;

//#define SCRUB_ABOVE

#define SELECT_TOLERANCE_PIXEL 4

#define PLAY_REGION_TRIANGLE_SIZE 6
#define PLAY_REGION_RECT_WIDTH 1
#define PLAY_REGION_RECT_HEIGHT 3
#define PLAY_REGION_GLOBAL_OFFSET_Y 7

enum : int {
   IndicatorSmallWidth = 9,
   IndicatorMediumWidth = 13,
   IndicatorOffset = 1,

   TopMargin = 1,
   BottomMargin = 2, // for bottom bevel and bottom line
   LeftMargin = 1, 

   RightMargin = 1,
};

enum {
   ScrubHeight = 14,
   ProperRulerHeight = 29
};

inline int IndicatorHeightForWidth(int width)
{
   return ((width / 2) * 3) / 2;
}

inline int IndicatorWidthForHeight(int height)
{
   // Not an exact inverse of the above, with rounding, but good enough
   return std::max(static_cast<int>(IndicatorSmallWidth),
                   (((height) * 2) / 3) * 2
                   );
}

inline int IndicatorBigHeight()
{
   return std::max((int)(ScrubHeight - TopMargin),
                   (int)(IndicatorMediumWidth));
}

inline int IndicatorBigWidth()
{
   return IndicatorWidthForHeight(IndicatorBigHeight());
}

/**********************************************************************

QuickPlayRulerOverlay.
Graphical helper for AdornedRulerPanel.

**********************************************************************/

class QuickPlayIndicatorOverlay;

// This is an overlay drawn on the ruler.  It draws the little triangle or
// the double-headed arrow.
class AdornedRulerPanel::QuickPlayRulerOverlay final : public Overlay
{
public:
   QuickPlayRulerOverlay(QuickPlayIndicatorOverlay &partner);

   // Available to this and to partner

   int mNewQPIndicatorPos { -1 };
   bool mNewQPIndicatorSnapped {};
   bool mNewPreviewingScrub {};

   bool mNewScrub {};
   bool mNewSeek {};

   void Update();

private:
   AdornedRulerPanel *GetRuler() const;

   unsigned SequenceNumber() const override;

   std::pair<wxRect, bool> DoGetRectangle(wxSize size) override;
   void Draw(OverlayPanel &panel, wxDC &dc) override;

   QuickPlayIndicatorOverlay &mPartner;

   // Used by this only
   int mOldQPIndicatorPos { -1 };
   bool mOldScrub {};
   bool mOldSeek {};
};

/**********************************************************************

 QuickPlayIndicatorOverlay.
 Graphical helper for AdornedRulerPanel.

 **********************************************************************/

// This is an overlay drawn on a different window, the track panel.
// It draws the pale guide line that follows mouse movement.
class AdornedRulerPanel::QuickPlayIndicatorOverlay final : public Overlay
{
   friend QuickPlayRulerOverlay;
   friend AdornedRulerPanel;

public:
   QuickPlayIndicatorOverlay(AudacityProject *project);

private:
   unsigned SequenceNumber() const override;
   std::pair<wxRect, bool> DoGetRectangle(wxSize size) override;
   void Draw(OverlayPanel &panel, wxDC &dc) override;

   AudacityProject *mProject;

   std::shared_ptr<QuickPlayRulerOverlay> mPartner
      { std::make_shared<QuickPlayRulerOverlay>(*this) };

   int mOldQPIndicatorPos { -1 };
   bool mOldQPIndicatorSnapped {};
   bool mOldPreviewingScrub {};
};

/**********************************************************************

 Implementation of QuickPlayRulerOverlay.

 **********************************************************************/

AdornedRulerPanel::QuickPlayRulerOverlay::QuickPlayRulerOverlay(
   QuickPlayIndicatorOverlay &partner)
: mPartner(partner)
{
}

AdornedRulerPanel *AdornedRulerPanel::QuickPlayRulerOverlay::GetRuler() const
{
   return &Get( *mPartner.mProject );
}

void AdornedRulerPanel::QuickPlayRulerOverlay::Update()
{
   const auto project = mPartner.mProject;
   auto &scrubber = Scrubber::Get( *project );
   auto ruler = GetRuler();

   // Hide during transport, or if mouse is not in the ruler, unless scrubbing
   if ((!ruler->LastCell() || ProjectAudioIO::Get( *project ).IsAudioActive())
       && (!scrubber.IsScrubbing() || scrubber.IsSpeedPlaying()
          || scrubber.IsKeyboardScrubbing()))
      mNewQPIndicatorPos = -1;
   else {
      const auto &selectedRegion = ViewInfo::Get( *project ).selectedRegion;
      double latestEnd =
         std::max(ruler->mTracks->GetEndTime(), selectedRegion.t1());
      if (ruler->mQuickPlayPos >= latestEnd)
         mNewQPIndicatorPos = -1;
      else {
         // This will determine the x coordinate of the line and of the
         // ruler indicator
         mNewQPIndicatorPos = ruler->Time2Pos(ruler->mQuickPlayPos);

         // These determine which shape is drawn on the ruler, and whether
         // in the scrub or the qp zone
         mNewScrub =
            ruler->mMouseEventState == AdornedRulerPanel::mesNone &&
            (ruler->LastCell() == ruler->mScrubbingCell ||
             (scrubber.HasMark()));
         mNewSeek = mNewScrub &&
            (scrubber.Seeks() || scrubber.TemporarilySeeks());

         // These two will determine the color of the line stroked over
         // the track panel, green for scrub or yellow for snapped or white
         mNewPreviewingScrub =
            ruler->LastCell() == ruler->mScrubbingCell &&
            !scrubber.IsScrubbing();
         mNewQPIndicatorSnapped = ruler->mIsSnapped;
      }
   }
}

unsigned
AdornedRulerPanel::QuickPlayRulerOverlay::SequenceNumber() const
{
   return 30;
}

std::pair<wxRect, bool>
AdornedRulerPanel::QuickPlayRulerOverlay::DoGetRectangle(wxSize /*size*/)
{
   Update();

   const auto x = mOldQPIndicatorPos;
   if (x >= 0) {
      // These dimensions are always sufficient, even if a little
      // excessive for the small triangle:
      const int width = IndicatorBigWidth() * 3 / 2;
      //const auto height = IndicatorHeightForWidth(width);

      const int indsize = width / 2;

      auto xx = x - indsize;
      auto yy = 0;
      return {
         { xx, yy,
            indsize * 2 + 1,
            GetRuler()->GetSize().GetHeight() },
         (x != mNewQPIndicatorPos
          || (mOldScrub != mNewScrub)
          || (mOldSeek != mNewSeek) )
      };
   }
   else
      return { {}, mNewQPIndicatorPos >= 0 };
}

void AdornedRulerPanel::QuickPlayRulerOverlay::Draw(
   OverlayPanel & /*panel*/, wxDC &dc)
{
   mOldQPIndicatorPos = mNewQPIndicatorPos;
   mOldScrub = mNewScrub;
   mOldSeek = mNewSeek;
   if (mOldQPIndicatorPos >= 0) {
      auto ruler = GetRuler();
      auto width = mOldScrub ? IndicatorBigWidth() : IndicatorSmallWidth;
      ruler->DoDrawIndicator(
         &dc, mOldQPIndicatorPos, true, width, mOldScrub, mOldSeek);
   }
}

/**********************************************************************

 Implementation of QuickPlayIndicatorOverlay.

 **********************************************************************/

AdornedRulerPanel::QuickPlayIndicatorOverlay::QuickPlayIndicatorOverlay(
   AudacityProject *project)
   : mProject(project)
{
}

unsigned
AdornedRulerPanel::QuickPlayIndicatorOverlay::SequenceNumber() const
{
   return 30;
}

std::pair<wxRect, bool>
AdornedRulerPanel::QuickPlayIndicatorOverlay::DoGetRectangle(wxSize size)
{
   mPartner->Update();

   wxRect rect(mOldQPIndicatorPos, 0, 1, size.GetHeight());
   return std::make_pair(
      rect,
      (mOldQPIndicatorPos != mPartner->mNewQPIndicatorPos ||
       mOldQPIndicatorSnapped != mPartner->mNewQPIndicatorSnapped ||
       mOldPreviewingScrub != mPartner->mNewPreviewingScrub)
   );
}

void AdornedRulerPanel::QuickPlayIndicatorOverlay::Draw(
   OverlayPanel &panel, wxDC &dc)
{
   mOldQPIndicatorPos = mPartner->mNewQPIndicatorPos;
   mOldQPIndicatorSnapped = mPartner->mNewQPIndicatorSnapped;
   mOldPreviewingScrub = mPartner->mNewPreviewingScrub;

   if (mOldQPIndicatorPos >= 0) {
      mOldPreviewingScrub
      ? AColor::IndicatorColor(&dc, true) // Draw green line for preview.
      : mOldQPIndicatorSnapped
        ? AColor::SnapGuidePen(&dc)
        : AColor::Light(&dc, false)
      ;

      // Draw indicator in all visible tracks
      auto pCellularPanel = dynamic_cast<CellularPanel*>( &panel );
      if ( !pCellularPanel ) {
         wxASSERT( false );
         return;
      }
      pCellularPanel
         ->VisitCells( [&]( const wxRect &rect, TrackPanelCell &cell ) {
            const auto pTrackView = dynamic_cast<TrackView*>(&cell);
            if (!pTrackView)
               return;

            // Draw the NEW indicator in its NEW location
            AColor::Line(dc,
               mOldQPIndicatorPos,
               rect.GetTop(),
               mOldQPIndicatorPos,
               rect.GetBottom());
      } );
   }
}

/**********************************************************************

  Implementation of AdornedRulerPanel.
  Either we find a way to make this more generic, Or it will move
  out of the widgets subdirectory into its own source file.

**********************************************************************/

enum {
   OnToggleQuickPlayID = 7000,
   OnSyncQuickPlaySelID,
   OnAutoScrollID,
   OnLockPlayRegionID,
   OnTogglePinnedStateID,
};

BEGIN_EVENT_TABLE(AdornedRulerPanel, CellularPanel)
   EVT_IDLE( AdornedRulerPanel::OnIdle )
   EVT_PAINT(AdornedRulerPanel::OnPaint)
   EVT_SIZE(AdornedRulerPanel::OnSize)

   // Context menu commands
   EVT_MENU(OnToggleQuickPlayID, AdornedRulerPanel::OnToggleQuickPlay)
   EVT_MENU(OnSyncQuickPlaySelID, AdornedRulerPanel::OnSyncSelToQuickPlay)
   EVT_MENU(OnAutoScrollID, AdornedRulerPanel::OnAutoScroll)
   EVT_MENU(OnLockPlayRegionID, AdornedRulerPanel::OnLockPlayRegion)
   EVT_MENU( OnTogglePinnedStateID, AdornedRulerPanel::OnTogglePinnedState )

   EVT_COMMAND( OnTogglePinnedStateID,
               wxEVT_COMMAND_BUTTON_CLICKED,
               AdornedRulerPanel::OnPinnedButton )

END_EVENT_TABLE()

class AdornedRulerPanel::CommonCell : public TrackPanelCell
{
public:
   explicit
   CommonCell( AdornedRulerPanel *parent, MenuChoice menuChoice )
   : mParent{ parent }
   , mMenuChoice{ menuChoice }
   {}
   
   HitTestPreview DefaultPreview
      (const TrackPanelMouseState &state, const AudacityProject *pProject)
      override
   {
      (void)pProject;// Compiler food
      (void)state;// Compiler food
      // May come here when recording is in progress, so hit tests are turned
      // off.
      TranslatableString tooltip;
      if (mParent->mTimelineToolTip)
         tooltip = XO("Timeline actions disabled during recording");

      static wxCursor cursor{ wxCURSOR_DEFAULT };
      return {
         {},
         &cursor,
         tooltip,
      };
   }

   unsigned DoContextMenu
      (const wxRect &rect,
       wxWindow *pParent, const wxPoint *pPosition, AudacityProject*) final
   {
      (void)pParent;// Compiler food
      (void)rect;// Compiler food
      mParent->ShowContextMenu(mMenuChoice, pPosition);
      return 0;
   }

protected:
   AdornedRulerPanel *mParent;
   const MenuChoice mMenuChoice;
};

class AdornedRulerPanel::CommonRulerHandle : public UIHandle
{
public:
   explicit
   CommonRulerHandle(
      AdornedRulerPanel *pParent, wxCoord xx, MenuChoice menuChoice )
      : mParent(pParent)
      , mX( xx )
      , mChoice( menuChoice )
   {}

   bool Clicked() const { return mClicked != Button::None; }

   static UIHandle::Result NeedChangeHighlight
   (const CommonRulerHandle &oldState, const CommonRulerHandle &newState)
   {
      if (oldState.mX != newState.mX)
         return RefreshCode::DrawOverlays;
      return 0;
   }

protected:
   bool HandlesRightClick() override { return true; }

   Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *) override
   {
      mClicked = event.event.LeftIsDown() ? Button::Left : Button::Right;
      return RefreshCode::DrawOverlays;
   }

   Result Drag
      (const TrackPanelMouseEvent &, AudacityProject *) override
   {
      return RefreshCode::DrawOverlays;
   }

   Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *,
       wxWindow *) override
   {
      if ( mParent && mClicked == Button::Right ) {
         const auto pos = event.event.GetPosition();
         mParent->ShowContextMenu( mChoice, &pos );
      }
      return RefreshCode::DrawOverlays;
   }

   Result Cancel(AudacityProject *pProject) override
   {
      (void)pProject;// Compiler food
      return RefreshCode::DrawOverlays;
   }
   
   void Enter(bool, AudacityProject *) override
   {
      mChangeHighlight = RefreshCode::DrawOverlays;
   }

   wxWeakRef<AdornedRulerPanel> mParent;

   wxCoord mX;
   
   MenuChoice mChoice;

   enum class Button { None, Left, Right };
   Button mClicked{ Button::None };
};

class AdornedRulerPanel::QPHandle final : public CommonRulerHandle
{
public:
   explicit
   QPHandle( AdornedRulerPanel *pParent, wxCoord xx )
   : CommonRulerHandle( pParent, xx, MenuChoice::QuickPlay )
   {
   }
   
   std::unique_ptr<SnapManager> mSnapManager;

private:
   Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   HitTestPreview Preview
      (const TrackPanelMouseState &state, AudacityProject *pProject)
   override;

   Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
       wxWindow *pParent) override;

   Result Cancel(AudacityProject *pProject) override;

   SelectedRegion mOldSelection;
};

namespace
{

wxCoord GetPlayHeadX( const AudacityProject *pProject )
{
   const auto &viewInfo = ViewInfo::Get( *pProject );
   auto width = viewInfo.GetTracksUsableWidth();
   return viewInfo.GetLeftOffset()
      + width * TracksPrefs::GetPinnedHeadPositionPreference();
}

double GetPlayHeadFraction( const AudacityProject *pProject, wxCoord xx )
{
   const auto &viewInfo = ViewInfo::Get( *pProject );
   auto width = viewInfo.GetTracksUsableWidth();
   auto fraction = (xx - viewInfo.GetLeftOffset()) / double(width);
   return std::max(0.0, std::min(1.0, fraction));
}

// Handle for dragging the pinned play head, which so far does not need
// to be a friend of the AdornedRulerPanel class, so we don't make it nested.
class PlayheadHandle : public UIHandle
{
public:
   explicit
   PlayheadHandle( wxCoord xx )
      : mX( xx )
   {}

   static UIHandle::Result NeedChangeHighlight
   (const PlayheadHandle &oldState, const PlayheadHandle &newState)
   {
      if (oldState.mX != newState.mX)
         return RefreshCode::DrawOverlays;
      return 0;
   }
   
   static std::shared_ptr<PlayheadHandle>
   HitTest( const AudacityProject *pProject, wxCoord xx )
   {
      if( Scrubber::Get( *pProject )
         .IsTransportingPinned() &&
          ProjectAudioIO::Get( *pProject ).IsAudioActive() )
      {
         const auto targetX = GetPlayHeadX( pProject );
         if ( abs( xx - targetX ) <= SELECT_TOLERANCE_PIXEL )
            return std::make_shared<PlayheadHandle>( xx );
      }
      return {};
   }
   
protected:
   Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override
   {
      (void)pProject;// Compiler food
      if (event.event.LeftDClick()) {
         // Restore default position on double click
         TracksPrefs::SetPinnedHeadPositionPreference( 0.5, true );
      
         return RefreshCode::DrawOverlays |
            // Do not start a drag
            RefreshCode::Cancelled;
      }
      // Fix for Bug 2357
      if (!event.event.LeftIsDown())
         return RefreshCode::Cancelled;

      mOrigPreference = TracksPrefs::GetPinnedHeadPositionPreference();
      return 0;
   }

   Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override
   {

      auto value = GetPlayHeadFraction(pProject, event.event.m_x );
      TracksPrefs::SetPinnedHeadPositionPreference( value );
      return RefreshCode::DrawOverlays;
   }

   HitTestPreview Preview
      (const TrackPanelMouseState &state, AudacityProject *pProject)
      override
   {
      (void)pProject;// Compiler food
      (void)state;// Compiler food

      static wxCursor cursor{ wxCURSOR_SIZEWE };
      return {
         XO( "Click and drag to adjust, double-click to reset" ),
         &cursor,
         /* i18n-hint: This text is a tooltip on the icon (of a pin) representing 
         the temporal position in the audio.  */
         XO( "Record/Play head" )
      };
   }

   Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
       wxWindow *) override
   {
      auto value = GetPlayHeadFraction(pProject, event.event.m_x );
      TracksPrefs::SetPinnedHeadPositionPreference( value, true );
      return RefreshCode::DrawOverlays;
   }

   Result Cancel(AudacityProject *pProject) override
   {
      (void)pProject;// Compiler food
      TracksPrefs::SetPinnedHeadPositionPreference( mOrigPreference );
      return RefreshCode::DrawOverlays;
   }
   
   void Enter(bool, AudacityProject *) override
   {
      mChangeHighlight = RefreshCode::DrawOverlays;
   }

   wxCoord mX;
   double mOrigPreference {};
};

}

class AdornedRulerPanel::QPCell final : public CommonCell
{
public:
   explicit
   QPCell( AdornedRulerPanel *parent )
   : AdornedRulerPanel::CommonCell{ parent, MenuChoice::QuickPlay }
   {}
   
   std::vector<UIHandlePtr> HitTest
      (const TrackPanelMouseState &state,
       const AudacityProject *pProject) override;
   
   // Return shared_ptr to self, stored in parent
   std::shared_ptr<TrackPanelCell> ContextMenuDelegate() override
      { return mParent->mQPCell; }

   bool Hit() const { return !mHolder.expired(); }
   bool Clicked() const {
      if (auto ptr = mHolder.lock())
         return ptr->Clicked();
      return false;
   }
   
   std::weak_ptr<QPHandle> mHolder;
   std::weak_ptr<PlayheadHandle> mPlayheadHolder;
};

std::vector<UIHandlePtr> AdornedRulerPanel::QPCell::HitTest
(const TrackPanelMouseState &state,
 const AudacityProject *pProject)
{
   // Creation of overlays on demand here -- constructor of AdornedRulerPanel
   // is too early to do it
   mParent->CreateOverlays();
   
   std::vector<UIHandlePtr> results;
   auto xx = state.state.m_x;

#ifdef EXPERIMENTAL_DRAGGABLE_PLAY_HEAD
   {
      // Allow click and drag on the play head even while recording
      // Make this handle more prominent then the quick play handle
      auto result = PlayheadHandle::HitTest( pProject, xx );
      if (result) {
         result = AssignUIHandlePtr( mPlayheadHolder, result );
         results.push_back( result );
      }
   }
#endif
   
   // Disable mouse actions on Timeline while recording.
   if (!mParent->mIsRecording) {

   mParent->UpdateQuickPlayPos( xx, state.state.ShiftDown() );

   auto result = std::make_shared<QPHandle>( mParent, xx );
   result = AssignUIHandlePtr( mHolder, result );
   results.push_back( result );

   }

   return results;
}

class AdornedRulerPanel::ScrubbingHandle final : public CommonRulerHandle
{
public:
   explicit
   ScrubbingHandle( AdornedRulerPanel *pParent, wxCoord xx )
   : CommonRulerHandle( pParent, xx, MenuChoice::Scrub )
   {
   }

private:
   Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override
   {
      auto result = CommonRulerHandle::Click(event, pProject);
      if (!( result & RefreshCode::Cancelled )) {
         if (mClicked == Button::Left) {
            auto &scrubber = Scrubber::Get( *pProject );
            // only if scrubbing is allowed now
            bool canScrub =
               scrubber.CanScrub() &&
               mParent &&
               mParent->ShowingScrubRuler();

            if (!canScrub)
               return RefreshCode::Cancelled;
            if (!scrubber.HasMark()) {
               // Asynchronous scrub poller gets activated here
               scrubber.MarkScrubStart(
                  event.event.m_x, Scrubber::ShouldScrubPinned(), false);
            }
         }
      }
      return result;
   }

   Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override
   {
      auto result = CommonRulerHandle::Drag(event, pProject);
      if (!( result & RefreshCode::Cancelled )) {
         // Nothing needed here.  The scrubber works by polling mouse state
         // after the start has been marked.
      }
      return result;
   }

   HitTestPreview Preview
      (const TrackPanelMouseState &state, AudacityProject *pProject)
   override;

   Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
       wxWindow *pParent) override {
      auto result = CommonRulerHandle::Release(event, pProject, pParent);
      if (!( result & RefreshCode::Cancelled )) {
         // Nothing needed here either.  The scrub poller may have decided to
         // seek because a drag happened before button up, or it may decide
         // to start a scrub, as it watches mouse movement after the button up.
      }
      return result;
   }

   Result Cancel(AudacityProject *pProject) override
   {
      auto result = CommonRulerHandle::Cancel(pProject);

      if (mClicked == Button::Left) {
         auto &scrubber = Scrubber::Get( *pProject );
         scrubber.Cancel();
         
         ProjectAudioManager::Get( *pProject ).Stop();
      }

      return result;
   }
};

class AdornedRulerPanel::ScrubbingCell final : public CommonCell
{
public:
   explicit
   ScrubbingCell( AdornedRulerPanel *parent )
   : AdornedRulerPanel::CommonCell{ parent, MenuChoice::Scrub }
   {}
   
   std::vector<UIHandlePtr> HitTest
      (const TrackPanelMouseState &state,
       const AudacityProject *pProject) override;
   
   // Return shared_ptr to self, stored in parent
   std::shared_ptr<TrackPanelCell> ContextMenuDelegate() override
      { return mParent->mScrubbingCell; }
   
   bool Hit() const { return !mHolder.expired(); }
   bool Clicked() const {
      if (auto ptr = mHolder.lock())
         return ptr->Clicked();
      return false;
   }
   
private:
   std::weak_ptr<ScrubbingHandle> mHolder;
};

std::vector<UIHandlePtr> AdornedRulerPanel::ScrubbingCell::HitTest
(const TrackPanelMouseState &state,
 const AudacityProject *pProject)
{
   (void)pProject;// Compiler food
   // Creation of overlays on demand here -- constructor of AdornedRulerPanel
   // is too early to do it
   mParent->CreateOverlays();
   
   std::vector<UIHandlePtr> results;
   
   // Disable mouse actions on Timeline while recording.
   if (!mParent->mIsRecording) {

   auto xx = state.state.m_x;
   mParent->UpdateQuickPlayPos( xx, state.state.ShiftDown() );
   auto result = std::make_shared<ScrubbingHandle>( mParent, xx );
   result = AssignUIHandlePtr( mHolder, result );
   results.push_back( result );

   }
   
   return results;
}

namespace{
AttachedWindows::RegisteredFactory sKey{
[]( AudacityProject &project ) -> wxWeakRef< wxWindow > {
   auto &viewInfo = ViewInfo::Get( project );
   auto &window = ProjectWindow::Get( project );

   return safenew AdornedRulerPanel( &project, window.GetTopPanel(),
      wxID_ANY,
      wxDefaultPosition,
      wxSize( -1, AdornedRulerPanel::GetRulerHeight(false) ),
      &viewInfo );
}
};
}

AdornedRulerPanel &AdornedRulerPanel::Get( AudacityProject &project )
{
   return GetAttachedWindows(project).Get< AdornedRulerPanel >( sKey );
}

const AdornedRulerPanel &AdornedRulerPanel::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

void AdornedRulerPanel::Destroy( AudacityProject &project )
{
   auto *pPanel = GetAttachedWindows(project).Find( sKey );
   if (pPanel) {
      pPanel->wxWindow::Destroy();
      GetAttachedWindows(project).Assign( sKey, nullptr );
   }
}

AdornedRulerPanel::AdornedRulerPanel(AudacityProject* project,
                                     wxWindow *parent,
                                     wxWindowID id,
                                     const wxPoint& pos,
                                     const wxSize& size,
                                     ViewInfo *viewinfo)
:  CellularPanel(parent, id, pos, size, viewinfo)
, mProject(project)
{
   SetLayoutDirection(wxLayout_LeftToRight);

   mQPCell = std::make_shared<QPCell>( this );
   mScrubbingCell = std::make_shared<ScrubbingCell>( this );
   
   for (auto &button : mButtons)
      button = nullptr;

   SetLabel( XO("Timeline") );
   SetName();
   SetBackgroundStyle(wxBG_STYLE_PAINT);

   mLeftOffset = 0;
   mIndTime = -1;

   mLeftDownClick = -1;
   mMouseEventState = mesNone;
   mIsDragging = false;

   mOuter = GetClientRect();

   mRuler.SetUseZoomInfo(mLeftOffset, mViewInfo);
   mRuler.SetLabelEdges( false );
   mRuler.SetFormat( Ruler::TimeFormat );

   mTracks = &TrackList::Get( *project );

   mIsSnapped = false;

   mIsRecording = false;

   mTimelineToolTip = !!gPrefs->Read(wxT("/QuickPlay/ToolTips"), 1L);
   mPlayRegionDragsSelection = (gPrefs->Read(wxT("/QuickPlay/DragSelection"), 0L) == 1)? true : false; 
   mQuickPlayEnabled = !!gPrefs->Read(wxT("/QuickPlay/QuickPlayEnabled"), 1L);

#if wxUSE_TOOLTIPS
   wxToolTip::Enable(true);
#endif

   wxTheApp->Bind(EVT_AUDIOIO_CAPTURE,
                     &AdornedRulerPanel::OnAudioStartStop,
                     this);
   wxTheApp->Bind(EVT_AUDIOIO_PLAYBACK,
                     &AdornedRulerPanel::OnAudioStartStop,
                     this);

   // Delay until after CommandManager has been populated:
   this->CallAfter( &AdornedRulerPanel::UpdatePrefs );

   wxTheApp->Bind(EVT_THEME_CHANGE, &AdornedRulerPanel::OnThemeChange, this);

   // Bind event that updates the play region
   mViewInfo->selectedRegion.Bind(EVT_SELECTED_REGION_CHANGE,
      &AdornedRulerPanel::OnSelectionChange, this);

   // And call it once to initialize it
   DoSelectionChange( mViewInfo->selectedRegion );
}

AdornedRulerPanel::~AdornedRulerPanel()
{
}

void AdornedRulerPanel::Refresh( bool eraseBackground, const wxRect *rect )
{
   CellularPanel::Refresh( eraseBackground, rect );
   CallAfter([this]{ CellularPanel::HandleCursorForPresentMouseState(); } );
}

void AdornedRulerPanel::UpdatePrefs()
{
   if (mNeedButtonUpdate) {
      // Visit this block once only in the lifetime of this panel
      mNeedButtonUpdate = false;
      // Do this first time setting of button status texts
      // when we are sure the CommandManager is initialized.
      ReCreateButtons();
   }

   // Update button texts for language change
   UpdateButtonStates();

   mTimelineToolTip = !!gPrefs->Read(wxT("/QuickPlay/ToolTips"), 1L);

#ifdef EXPERIMENTAL_SCROLLING_LIMITS
#ifdef EXPERIMENTAL_TWO_TONE_TIME_RULER
   {
      auto scrollBeyondZero = ScrollingPreference.Read();
      mRuler.SetTwoTone(scrollBeyondZero);
   }
#endif
#endif
}

void AdornedRulerPanel::ReCreateButtons()
{
   // TODO: Should we do this to destroy the grabber??
   // Get rid of any children we may have
   // DestroyChildren();

   ToolBar::MakeButtonBackgroundsSmall();
   SetBackgroundColour(theTheme.Colour( clrMedium ));

   for (auto & button : mButtons) {
      if (button)
         button->Destroy();
      button = nullptr;
   }

   size_t iButton = 0;
   // Make the short row of time ruler pushbottons.
   // Don't bother with sizers.  Their sizes and positions are fixed.
   // Add a grabber converted to a spacer.
   // This makes it visually clearer that the button is a button.

   wxPoint position( 1, 0 );

   Grabber * pGrabber = safenew Grabber(this, this->GetId());
   pGrabber->SetAsSpacer( true );
   //pGrabber->SetSize( 10, 27 ); // default is 10,27
   pGrabber->SetPosition( position );

   position.x = 12;

   auto size = theTheme.ImageSize( bmpRecoloredUpSmall );
   size.y = std::min(size.y, GetRulerHeight(false));

   auto buttonMaker = [&]
   (wxWindowID id, teBmps bitmap, bool toggle)
   {
      const auto button =
      ToolBar::MakeButton(
         this,
         bmpRecoloredUpSmall, bmpRecoloredDownSmall, 
         bmpRecoloredUpHiliteSmall, bmpRecoloredHiliteSmall, 
         bitmap, bitmap, bitmap,
         id, position, toggle, size
      );

      position.x += size.GetWidth();
      mButtons[iButton++] = button;
      return button;
   };
   auto button = buttonMaker(OnTogglePinnedStateID, bmpPlayPointerPinned, true);
   ToolBar::MakeAlternateImages(
	   *button, 3,
	   bmpRecoloredUpSmall, bmpRecoloredDownSmall,
	   bmpRecoloredUpHiliteSmall, bmpRecoloredHiliteSmall,
	   //bmpUnpinnedPlayHead, bmpUnpinnedPlayHead, bmpUnpinnedPlayHead,
	   bmpRecordPointer, bmpRecordPointer, bmpRecordPointer,
	   size);
   ToolBar::MakeAlternateImages(
	   *button, 2,
	   bmpRecoloredUpSmall, bmpRecoloredDownSmall,
	   bmpRecoloredUpHiliteSmall, bmpRecoloredHiliteSmall,
	   //bmpUnpinnedPlayHead, bmpUnpinnedPlayHead, bmpUnpinnedPlayHead,
	   bmpRecordPointerPinned, bmpRecordPointerPinned, bmpRecordPointerPinned,
	   size);
   ToolBar::MakeAlternateImages(
      *button, 1,
      bmpRecoloredUpSmall, bmpRecoloredDownSmall, 
      bmpRecoloredUpHiliteSmall, bmpRecoloredHiliteSmall, 
      //bmpUnpinnedPlayHead, bmpUnpinnedPlayHead, bmpUnpinnedPlayHead,
      bmpPlayPointer, bmpPlayPointer, bmpPlayPointer,
      size);

   UpdateButtonStates();
}

void AdornedRulerPanel::InvalidateRuler()
{
   mRuler.Invalidate();
}

namespace {
   const TranslatableString StartScrubbingMessage(const Scrubber &/*scrubber*/)
   {
#if 0
      if(scrubber.Seeks())
      /* i18n-hint: These commands assist the user in finding a sound by ear. ...
       "Scrubbing" is variable-speed playback, ...
       "Seeking" is normal speed playback but with skips
       */
         return XO("Click or drag to begin Seek");
      else
      /* i18n-hint: These commands assist the user in finding a sound by ear. ...
       "Scrubbing" is variable-speed playback, ...
       "Seeking" is normal speed playback but with skips
       */
         return XO("Click or drag to begin Scrub");
#else
      /* i18n-hint: These commands assist the user in finding a sound by ear. ...
       "Scrubbing" is variable-speed playback, ...
       "Seeking" is normal speed playback but with skips
       */
      return XO("Click & move to Scrub. Click & drag to Seek.");
#endif
   }

   const TranslatableString ContinueScrubbingMessage(
      const Scrubber &scrubber, bool clicked)
   {
#if 0
      if(scrubber.Seeks())
      /* i18n-hint: These commands assist the user in finding a sound by ear. ...
       "Scrubbing" is variable-speed playback, ...
       "Seeking" is normal speed playback but with skips
       */
         return XO("Move to Seek");
      else
      /* i18n-hint: These commands assist the user in finding a sound by ear. ...
       "Scrubbing" is variable-speed playback, ...
       "Seeking" is normal speed playback but with skips
       */
         return XO("Move to Scrub");
#else
      if( clicked ) {
         // Since mouse is down, mention dragging first.
         // IsScrubbing is true if Scrubbing OR seeking.
         if( scrubber.IsScrubbing() )
            // User is dragging already, explain.
            return XO("Drag to Seek. Release to stop seeking.");
         else
            // User has clicked but not yet moved or released.
            return XO("Drag to Seek. Release and move to Scrub.");
      }
      // Since mouse is up, mention moving first.
      return XO("Move to Scrub. Drag to Seek.");
#endif
   }

   const TranslatableString ScrubbingMessage(const Scrubber &scrubber, bool clicked)
   {
      if (scrubber.HasMark())
         return ContinueScrubbingMessage(scrubber, clicked);
      else
         return StartScrubbingMessage(scrubber);
   }
}

void AdornedRulerPanel::OnIdle( wxIdleEvent &evt )
{
   evt.Skip();
   DoIdle();
}

void AdornedRulerPanel::DoIdle()
{
   bool changed = UpdateRects();
   changed = SetPanelSize() || changed;

   auto &project = *mProject;
   auto &viewInfo = ViewInfo::Get( project );
   const auto &selectedRegion = viewInfo.selectedRegion;

   bool dirtySelectedRegion = mDirtySelectedRegion
      || ( mLastDrawnSelectedRegion != selectedRegion );

   changed = changed
     || dirtySelectedRegion
     || mLastDrawnH != viewInfo.h
     || mLastDrawnZoom != viewInfo.GetZoom()
     || mLastPlayRegionLocked != viewInfo.playRegion.Locked()
   ;
   if (changed)
      // Cause ruler redraw anyway, because we may be zooming or scrolling,
      // showing or hiding the scrub bar, etc.
      Refresh();

   mDirtySelectedRegion = false;
}

void AdornedRulerPanel::OnAudioStartStop(wxCommandEvent & evt)
{
   evt.Skip();

   if ( evt.GetEventType() == EVT_AUDIOIO_CAPTURE ) {
      if (evt.GetInt() != 0)
      {
         mIsRecording = true;
         this->CellularPanel::CancelDragging( false );
         this->CellularPanel::ClearTargets();

         UpdateButtonStates();
      }
      else {
         mIsRecording = false;
         UpdateButtonStates();
      }
   }

   if ( evt.GetInt() == 0 )
      // So that the play region is updated
      DoSelectionChange( mViewInfo->selectedRegion );
}

void AdornedRulerPanel::OnPaint(wxPaintEvent & WXUNUSED(evt))
{
   auto &viewInfo = ViewInfo::Get( *GetProject() );
   mLastDrawnH = viewInfo.h;
   mLastDrawnZoom = viewInfo.GetZoom();
   mDirtySelectedRegion = (mLastDrawnSelectedRegion != viewInfo.selectedRegion);
   mLastDrawnSelectedRegion = viewInfo.selectedRegion;
   // To do, note other fisheye state when we have that

   wxPaintDC dc(this);

   auto &backDC = GetBackingDCForRepaint();

   DoDrawBackground(&backDC);

   if (!mViewInfo->selectedRegion.isPoint())
   {
      DoDrawSelection(&backDC);
   }

   DoDrawMarks(&backDC, true);

   DoDrawPlayRegion(&backDC);

   DoDrawEdge(&backDC);

   DisplayBitmap(dc);

   // Stroke extras direct to the client area,
   // maybe outside of the damaged area
   // As with TrackPanel, do not make a NEW wxClientDC or else Mac flashes badly!
   dc.DestroyClippingRegion();
   DrawOverlays(true, &dc);
}

void AdornedRulerPanel::OnSize(wxSizeEvent &evt)
{
   mOuter = GetClientRect();
   if (mOuter.GetWidth() == 0 || mOuter.GetHeight() == 0)
   {
      return;
   }

   UpdateRects();

   OverlayPanel::OnSize(evt);
}

void AdornedRulerPanel::OnThemeChange(wxCommandEvent& evt)
{
   evt.Skip();
   ReCreateButtons();
}

void AdornedRulerPanel::OnSelectionChange(SelectedRegionEvent& evt)
{
   evt.Skip();
   if (!evt.pRegion)
      return;
   auto &selectedRegion = *evt.pRegion;
   DoSelectionChange( selectedRegion );
}

void AdornedRulerPanel::DoSelectionChange( const SelectedRegion &selectedRegion )
{

   auto gAudioIO = AudioIOBase::Get();
   if ( !ViewInfo::Get( *mProject ).playRegion.Locked() ) {
      SetPlayRegion( selectedRegion.t0(), selectedRegion.t1() );
   }
}

bool AdornedRulerPanel::UpdateRects()
{
   auto inner = mOuter;
   wxRect scrubZone;
   inner.x += LeftMargin;
   inner.width -= (LeftMargin + RightMargin);

   auto top = &inner;
   auto bottom = &inner;

   if (ShowingScrubRuler()) {
      scrubZone = inner;
      auto scrubHeight = std::min(scrubZone.height, (int)(ScrubHeight));

      int topHeight;
#ifdef SCRUB_ABOVE
      top = &scrubZone, topHeight = scrubHeight;
#else
      auto qpHeight = scrubZone.height - scrubHeight;
      bottom = &scrubZone, topHeight = qpHeight;
      // Increase scrub zone height so that hit testing finds it and
      // not QP region, when on bottom 'edge'.
      scrubZone.height+=BottomMargin;
#endif

      top->height = topHeight;
      bottom->height -= topHeight;
      bottom->y += topHeight;
   }

   top->y += TopMargin;
   top->height -= TopMargin;

   bottom->height -= BottomMargin;

   if (!ShowingScrubRuler())
      scrubZone = inner;

   if ( inner == mInner && scrubZone == mScrubZone )
      // no changes
      return false;

   mInner = inner;
   mScrubZone = scrubZone;

   mRuler.SetBounds(mInner.GetLeft(),
                    mInner.GetTop(),
                    mInner.GetRight(),
                    mInner.GetBottom());

   return true;
}

double AdornedRulerPanel::Pos2Time(int p, bool ignoreFisheye)
{
   return mViewInfo->PositionToTime(p, mLeftOffset
      , ignoreFisheye
   );
}

int AdornedRulerPanel::Time2Pos(double t, bool ignoreFisheye)
{
   return mViewInfo->TimeToPosition(t, mLeftOffset
      , ignoreFisheye
   );
}


bool AdornedRulerPanel::IsWithinMarker(int mousePosX, double markerTime)
{
   if (markerTime < 0)
      return false;

   int pixelPos = Time2Pos(markerTime);
   int boundLeft = pixelPos - SELECT_TOLERANCE_PIXEL;
   int boundRight = pixelPos + SELECT_TOLERANCE_PIXEL;

   return mousePosX >= boundLeft && mousePosX < boundRight;
}

auto AdornedRulerPanel::QPHandle::Click
(const TrackPanelMouseEvent &event, AudacityProject *pProject) -> Result
{
   auto result = CommonRulerHandle::Click(event, pProject);
   if (!( result & RefreshCode::Cancelled )) {
      if (mClicked == Button::Left) {
         if (!(mParent && mParent->mQuickPlayEnabled))
            return RefreshCode::Cancelled;

         auto &scrubber = Scrubber::Get( *pProject );
         if(scrubber.HasMark()) {
            // We can't stop scrubbing yet (see comments in Bug 1391),
            // but we can pause it.
            ProjectAudioManager::Get( *pProject ).OnPause();
         }

         // Store the initial play region state
         const auto &viewInfo = ViewInfo::Get( *pProject );
         const auto &playRegion = viewInfo.playRegion;
         mParent->mOldPlayRegion = playRegion;

         // Save old selection, in case drag of selection is cancelled
         mOldSelection = ViewInfo::Get( *pProject ).selectedRegion;

         mParent->HandleQPClick( event.event, mX );
         mParent->HandleQPDrag( event.event, mX );
      }
   }
   
   return result;
}

void AdornedRulerPanel::HandleQPClick(wxMouseEvent &evt, wxCoord mousePosX)
{
   // Temporarily unlock locked play region
   if (mOldPlayRegion.Locked() && evt.LeftDown()) {
      //mPlayRegionLock = true;
      SelectUtilities::UnlockPlayRegion(*mProject);
   }

   mLeftDownClickUnsnapped = mQuickPlayPosUnsnapped;
   mLeftDownClick = mQuickPlayPos;
   bool isWithinStart = IsWithinMarker(mousePosX, mOldPlayRegion.GetStart());
   bool isWithinEnd = IsWithinMarker(mousePosX, mOldPlayRegion.GetEnd());

   if (isWithinStart || isWithinEnd) {
      // If Quick-Play is playing from a point, we need to treat it as a click
      // not as dragging.
      if (mOldPlayRegion.Empty())
         mMouseEventState = mesSelectingPlayRegionClick;
      // otherwise check which marker is nearer
      else {
         // Don't compare times, compare positions.
         //if (fabs(mQuickPlayPos - mPlayRegionStart) < fabs(mQuickPlayPos - mPlayRegionEnd))
         auto start = mOldPlayRegion.GetStart();
         auto end = mOldPlayRegion.GetEnd();
         if (abs(Time2Pos(mQuickPlayPos) - Time2Pos(start)) <
             abs(Time2Pos(mQuickPlayPos) - Time2Pos(end)))
            mMouseEventState = mesDraggingPlayRegionStart;
         else
            mMouseEventState = mesDraggingPlayRegionEnd;
      }
   }
   else {
      // Clicked but not yet dragging
      mMouseEventState = mesSelectingPlayRegionClick;
   }
}

auto AdornedRulerPanel::QPHandle::Drag
(const TrackPanelMouseEvent &event, AudacityProject *pProject) -> Result
{
   auto result = CommonRulerHandle::Drag(event, pProject);
   if (!( result & RefreshCode::Cancelled )) {
      if (mClicked == Button::Left) {
         if ( mParent ) {
            mX = event.event.m_x;
            mParent->UpdateQuickPlayPos( mX, event.event.ShiftDown() );
            mParent->HandleQPDrag( event.event, mX );
         }
      }
   }
   return result;
}

void AdornedRulerPanel::HandleQPDrag(wxMouseEvent &/*event*/, wxCoord mousePosX)
{
   bool isWithinClick =
      (mLeftDownClickUnsnapped >= 0) &&
      IsWithinMarker(mousePosX, mLeftDownClickUnsnapped);
   bool isWithinStart = IsWithinMarker(mousePosX, mOldPlayRegion.GetStart());
   bool isWithinEnd = IsWithinMarker(mousePosX, mOldPlayRegion.GetEnd());
   bool canDragSel = !mOldPlayRegion.Locked() && mPlayRegionDragsSelection;
   auto &viewInfo = ViewInfo::Get( *GetProject() );
   auto &playRegion = viewInfo.playRegion;

   switch (mMouseEventState)
   {
      case mesNone:
         // If close to either end of play region, snap to closest
         if (isWithinStart || isWithinEnd) {
            if (fabs(mQuickPlayPos - mOldPlayRegion.GetStart()) < fabs(mQuickPlayPos - mOldPlayRegion.GetEnd()))
               mQuickPlayPos = mOldPlayRegion.GetStart();
            else
               mQuickPlayPos = mOldPlayRegion.GetEnd();
         }
         break;
      case mesDraggingPlayRegionStart:
         // Don't start dragging until beyond tolerance initial playback start
         if (!mIsDragging && isWithinStart)
            mQuickPlayPos = mOldPlayRegion.GetStart();
         else
            mIsDragging = true;
         // avoid accidental tiny selection
         if (isWithinEnd)
            mQuickPlayPos = mOldPlayRegion.GetEnd();
         playRegion.SetStart( mQuickPlayPos );
         if (canDragSel) {
            DragSelection();
         }
         break;
      case mesDraggingPlayRegionEnd:
         if (!mIsDragging && isWithinEnd) {
            mQuickPlayPos = mOldPlayRegion.GetEnd();
         }
         else
            mIsDragging = true;
         if (isWithinStart) {
            mQuickPlayPos = mOldPlayRegion.GetStart();
         }
         playRegion.SetEnd( mQuickPlayPos );
         if (canDragSel) {
            DragSelection();
         }
         break;
      case mesSelectingPlayRegionClick:

         // Don't start dragging until mouse is beyond tolerance of initial click.
         if (isWithinClick || mLeftDownClick == -1) {
            mQuickPlayPos = mLeftDownClick;
            playRegion.SetTimes(mLeftDownClick, mLeftDownClick);
         }
         else {
            mMouseEventState = mesSelectingPlayRegionRange;
         }
         break;
      case mesSelectingPlayRegionRange:
         if (isWithinClick) {
            mQuickPlayPos = mLeftDownClick;
         }

         if (mQuickPlayPos < mLeftDownClick)
            playRegion.SetTimes( mQuickPlayPos, mLeftDownClick );
         else
            playRegion.SetTimes( mLeftDownClick, mQuickPlayPos );
         if (canDragSel) {
            DragSelection();
         }
         break;
   }
   Refresh();
   Update();
}

auto AdornedRulerPanel::ScrubbingHandle::Preview
(const TrackPanelMouseState &state, AudacityProject *pProject)
-> HitTestPreview
{
   (void)state;// Compiler food
   auto &scrubber = Scrubber::Get( *pProject );
   auto message = ScrubbingMessage(scrubber, mClicked == Button::Left);

   return {
      message,
      {},
      // Tooltip is same as status message, or blank
      ((mParent && mParent->mTimelineToolTip) ? message : TranslatableString{}),
   };
}

auto AdornedRulerPanel::QPHandle::Preview
(const TrackPanelMouseState &state, AudacityProject *pProject)
-> HitTestPreview
{
   TranslatableString tooltip;
   if (mParent && mParent->mTimelineToolTip) {
      if (!mParent->mQuickPlayEnabled)
         tooltip = XO("Quick-Play disabled");
      else
         tooltip = XO("Quick-Play enabled");
   }

   TranslatableString message;
   auto &scrubber = Scrubber::Get( *pProject );
   const bool scrubbing = scrubber.HasMark();
   if (scrubbing)
      // Don't distinguish zones
      message = ScrubbingMessage(scrubber, false);
   else
      // message = Insert timeline status bar message here
      ;

   static wxCursor cursorHand{ wxCURSOR_HAND };
   static wxCursor cursorSizeWE{ wxCURSOR_SIZEWE };
   
   bool showArrows = false;
   if (mParent && mParent->mQuickPlayEnabled)
      showArrows =
         (mClicked == Button::Left)
         || mParent->IsWithinMarker(
               state.state.m_x, mParent->mOldPlayRegion.GetStart())
         || mParent->IsWithinMarker(
               state.state.m_x, mParent->mOldPlayRegion.GetEnd());
   
   return {
      message,
      showArrows ? &cursorSizeWE : &cursorHand,
      tooltip,
   };
}

auto AdornedRulerPanel::QPHandle::Release
(const TrackPanelMouseEvent &event, AudacityProject *pProject,
 wxWindow *pParent) -> Result
{
   // Keep a shared pointer to self.  Otherwise *this might get deleted
   // in HandleQPRelease on Windows!  Because there is an event-loop yield
   // stopping playback, which caused OnCaptureLost to be called, which caused
   // clearing of CellularPanel targets!
   auto saveMe = mParent->mQPCell->mHolder.lock();

   auto result = CommonRulerHandle::Release(event, pProject, pParent);
   if (!( result & RefreshCode::Cancelled )) {
      if (mClicked == Button::Left) {
         if ( mParent ) {
            mParent->HandleQPRelease( event.event );
            // Update the hot zones for cursor changes
            const auto &viewInfo = ViewInfo::Get( *pProject );
            const auto &playRegion = viewInfo.playRegion;
            mParent->mOldPlayRegion = playRegion;
         }
      }
   }
   return result;
}

void AdornedRulerPanel::HandleQPRelease(wxMouseEvent &evt)
{
   auto &viewInfo = ViewInfo::Get( *GetProject() );
   auto &playRegion = viewInfo.playRegion;
   playRegion.Order();

   const double t0 = mTracks->GetStartTime();
   const double t1 = mTracks->GetEndTime();
   const auto &selectedRegion = viewInfo.selectedRegion;
   const double sel0 = selectedRegion.t0();
   const double sel1 = selectedRegion.t1();

   // We want some audio in the selection, but we allow a dragged
   // region to include selected white-space and space before audio start.
   if (evt.ShiftDown() && playRegion.Empty()) {
      // Looping the selection or project.
      // Disable if track selection is in white-space beyond end of tracks and
      // play position is outside of track contents.
      if (((sel1 < t0) || (sel0 > t1)) &&
          ((playRegion.GetStart() < t0) || (playRegion.GetStart() > t1))) {
         ClearPlayRegion();
      }
   }
   // Disable if beyond end.
   else if (playRegion.GetStart() >= t1) {
      ClearPlayRegion();
   }
   // Disable if empty selection before start.
   // (allow Quick-Play region to include 'pre-roll' white space)
   else if (
      playRegion.GetEnd() - playRegion.GetStart() > 0.0 &&
      playRegion.GetEnd() < t0
   ) {
      ClearPlayRegion();
   }

   mMouseEventState = mesNone;
   mIsDragging = false;
   mLeftDownClick = -1;

   auto cleanup = finally( [&] {
      if (mOldPlayRegion.Locked()) {
         // Restore Locked Play region
         SetPlayRegion(mOldPlayRegion.GetStart(), mOldPlayRegion.GetEnd());
         SelectUtilities::LockPlayRegion(*mProject);
         // and release local lock
         mOldPlayRegion.SetLocked( false );
      }
   } );

   StartQPPlay(evt.ShiftDown(), evt.ControlDown());
}

auto AdornedRulerPanel::QPHandle::Cancel
(AudacityProject *pProject) -> Result
{
   auto result = CommonRulerHandle::Cancel(pProject);

   if (mClicked == Button::Left) {
      if( mParent ) {
         ViewInfo::Get( *pProject ).selectedRegion = mOldSelection;
         mParent->mMouseEventState = mesNone;
         mParent->SetPlayRegion(
            mParent->mOldPlayRegion.GetStart(), mParent->mOldPlayRegion.GetEnd());
         if (mParent->mOldPlayRegion.Locked()) {
            // Restore Locked Play region
            SelectUtilities::LockPlayRegion(*pProject);
            // and release local lock
            mParent->mOldPlayRegion.SetLocked( false );
         }
      }
   }

   return result;
}

void AdornedRulerPanel::StartQPPlay(bool looped, bool cutPreview)
{
   const double t0 = mTracks->GetStartTime();
   const double t1 = mTracks->GetEndTime();
   auto &viewInfo = ViewInfo::Get( *mProject );
   auto &playRegion = viewInfo.playRegion;
   const auto &selectedRegion = viewInfo.selectedRegion;
   const double sel0 = selectedRegion.t0();
   const double sel1 = selectedRegion.t1();

   // Start / Restart playback on left click.
   bool startPlaying = (playRegion.GetStart() >= 0);

   if (startPlaying) {
      bool loopEnabled = true;
      double start, end;

      if (playRegion.Empty() && looped) {
         // Loop play a point will loop either a selection or the project.
         if ((playRegion.GetStart() > sel0) && (playRegion.GetStart() < sel1)) {
            // we are in a selection, so use the selection
            start = sel0;
            end = sel1;
         } // not in a selection, so use the project
         else {
            start = t0;
            end = t1;
         }
      }
      else {
         start = playRegion.GetStart();
         end = playRegion.GetEnd();
      }
      // Looping a tiny selection may freeze, so just play it once.
      loopEnabled = ((end - start) > 0.001)? true : false;

      bool looped = (loopEnabled && looped);
      if (looped)
         cutPreview = false;
      auto options = DefaultPlayOptions( *mProject, looped );

      auto oldStart = playRegion.GetStart();
      if (!cutPreview)
         options.pStartTime = &oldStart;
      else
         options.envelope = nullptr;

      auto mode =
         cutPreview ? PlayMode::cutPreviewPlay
         : looped ? PlayMode::loopedPlay
         : PlayMode::normalPlay;

      // Stop only after deciding where to start again, because an event
      // callback may change the play region back to the selection
      auto &projectAudioManager = ProjectAudioManager::Get( *mProject );
      projectAudioManager.Stop();

      // Change play region display while playing
      playRegion.SetTimes( start, end );
      Refresh();

      projectAudioManager.PlayPlayRegion((SelectedRegion(start, end)),
                          options, mode,
                          false,
                          true);

   }
}

#if 0
// This version toggles ruler state indirectly via the scrubber
// to ensure that all the places where the state is shown update.
// For example buttons and menus must update.
void AdornedRulerPanel::OnToggleScrubRulerFromMenu(wxCommandEvent&)
{
   auto &scrubber = Scrubber::Get( *mProject );
   scrubber.OnToggleScrubRuler(*mProject);
}
#endif


bool AdornedRulerPanel::SetPanelSize()
{
   const auto oldSize = GetSize();
   wxSize size { oldSize.GetWidth(), GetRulerHeight(ShowingScrubRuler()) };
   if ( size != oldSize ) {
      SetSize(size);
      SetMinSize(size);
      GetParent()->PostSizeEventToParent();
      return true;
   }
   else
      return false;
}

void AdornedRulerPanel::DrawBothOverlays()
{
   auto pCellularPanel =
      dynamic_cast<CellularPanel*>( &GetProjectPanel( *GetProject() ) );
   if ( !pCellularPanel ) {
      wxASSERT( false );
   }
   else
      pCellularPanel->DrawOverlays( false );
   DrawOverlays( false );
}

void AdornedRulerPanel::UpdateButtonStates()
{
   auto common = [this](
      AButton &button, const CommandID &commandName, const TranslatableString &label) {
      ComponentInterfaceSymbol command{ commandName, label };
      ToolBar::SetButtonToolTip( *mProject, button, &command, 1u );
      button.SetLabel( Verbatim( button.GetToolTipText() ) );

      button.UpdateStatus();
   };

   {
      // The button always reflects the pinned head preference, even though
      // there is also a Playback preference that may overrule it for scrubbing
      bool state = TracksPrefs::GetPinnedHeadPreference();
      auto pinButton = static_cast<AButton*>(FindWindow(OnTogglePinnedStateID));
      if( !state )
         pinButton->PopUp();
      else
         pinButton->PushDown();
      auto gAudioIO = AudioIO::Get();
      pinButton->SetAlternateIdx(
         (gAudioIO->IsCapturing() ? 2 : 0) + (state ? 0 : 1));
      // Bug 1584: Tooltip now shows what clicking will do.
      // Bug 2357: Action of button (and hence tooltip wording) updated.
      const auto label = XO("Timeline Options");
      common(*pinButton, wxT("PinnedHead"), label);
   }
}

void AdornedRulerPanel::OnPinnedButton(wxCommandEvent & /*event*/)
{
   ShowContextMenu(MenuChoice::QuickPlay, NULL);
}

void AdornedRulerPanel::OnTogglePinnedState(wxCommandEvent & /*event*/)
{
   TogglePinnedHead();
   UpdateButtonStates();
}

void AdornedRulerPanel::UpdateQuickPlayPos(wxCoord &mousePosX, bool shiftDown)
{
   // Keep Quick-Play within usable track area.
   const auto &viewInfo = ViewInfo::Get( *mProject );
   auto width = viewInfo.GetTracksUsableWidth();
   mousePosX = std::max(mousePosX, viewInfo.GetLeftOffset());
   mousePosX = std::min(mousePosX, viewInfo.GetLeftOffset() + width - 1);

   mQuickPlayPosUnsnapped = mQuickPlayPos = Pos2Time(mousePosX);

   HandleSnapping();

   // If not looping, restrict selection to end of project
   if ((LastCell() == mQPCell || mQPCell->Clicked()) && !shiftDown) {
      const double t1 = mTracks->GetEndTime();
      mQuickPlayPos = std::min(t1, mQuickPlayPos);
   }
}

// Pop-up menus

void AdornedRulerPanel::ShowMenu(const wxPoint & pos)
{
   const auto &viewInfo = ViewInfo::Get( *GetProject() );
   const auto &playRegion = viewInfo.playRegion;
   wxMenu rulerMenu;

   rulerMenu.AppendCheckItem(OnToggleQuickPlayID, _("Enable Quick-Play"))->
      Check(mQuickPlayEnabled);

   auto pDrag = rulerMenu.AppendCheckItem(OnSyncQuickPlaySelID, _("Enable dragging selection"));
   pDrag->Check(mPlayRegionDragsSelection && !playRegion.Locked());
   pDrag->Enable(mQuickPlayEnabled && !playRegion.Locked());

   rulerMenu.AppendCheckItem(OnAutoScrollID, _("Update display while playing"))->
      Check(mViewInfo->bUpdateTrackIndicator);

   auto pLock = rulerMenu.AppendCheckItem(OnLockPlayRegionID, _("Lock Play Region"));
   pLock->Check(playRegion.Locked());
   pLock->Enable( playRegion.Locked() || !playRegion.Empty() );


   rulerMenu.AppendSeparator();
   rulerMenu.AppendCheckItem(OnTogglePinnedStateID, _("Pinned Play Head"))->
      Check(TracksPrefs::GetPinnedHeadPreference());

   BasicMenu::Handle{ &rulerMenu }.Popup(
      wxWidgetsWindowPlacement{ this },
      { pos.x, pos.y }
   );
}

void AdornedRulerPanel::ShowScrubMenu(const wxPoint & pos)
{
   auto &scrubber = Scrubber::Get( *mProject );
   PushEventHandler(&scrubber);
   auto cleanup = finally([this]{ PopEventHandler(); });

   wxMenu rulerMenu;
   scrubber.PopulatePopupMenu(rulerMenu);
   BasicMenu::Handle{ &rulerMenu }.Popup(
      wxWidgetsWindowPlacement{ this },
      { pos.x, pos.y }
   );
}

void AdornedRulerPanel::OnToggleQuickPlay(wxCommandEvent&)
{
   mQuickPlayEnabled = (mQuickPlayEnabled)? false : true;
   gPrefs->Write(wxT("/QuickPlay/QuickPlayEnabled"), mQuickPlayEnabled);
   gPrefs->Flush();
}

void AdornedRulerPanel::OnSyncSelToQuickPlay(wxCommandEvent&)
{
   mPlayRegionDragsSelection = (mPlayRegionDragsSelection)? false : true;
   gPrefs->Write(wxT("/QuickPlay/DragSelection"), mPlayRegionDragsSelection);
   gPrefs->Flush();
}

void AdornedRulerPanel::DragSelection()
{
   auto &viewInfo = ViewInfo::Get( *GetProject() );
   const auto &playRegion = viewInfo.playRegion;
   auto &selectedRegion = viewInfo.selectedRegion;
   selectedRegion.setT0(playRegion.GetStart(), false);
   selectedRegion.setT1(playRegion.GetEnd(), true);
}

void AdornedRulerPanel::HandleSnapping()
{
   auto handle = mQPCell->mHolder.lock();
   if (handle) {
      auto &pSnapManager = handle->mSnapManager;
      if (! pSnapManager)
         pSnapManager =
            std::make_unique<SnapManager>(*mProject, *mTracks, *mViewInfo);
      
      auto results = pSnapManager->Snap(NULL, mQuickPlayPos, false);
      mQuickPlayPos = results.outTime;
      mIsSnapped = results.Snapped();
   }
}

#if 0
void AdornedRulerPanel::OnTimelineToolTips(wxCommandEvent&)
{
   mTimelineToolTip = (mTimelineToolTip)? false : true;
   gPrefs->Write(wxT("/QuickPlay/ToolTips"), mTimelineToolTip);
   gPrefs->Flush();
}
#endif

void AdornedRulerPanel::OnAutoScroll(wxCommandEvent&)
{
   if (mViewInfo->bUpdateTrackIndicator)
      gPrefs->Write(wxT("/GUI/AutoScroll"), false);
   else
      gPrefs->Write(wxT("/GUI/AutoScroll"), true);

   gPrefs->Flush();

   PrefsListener::Broadcast(ViewInfo::UpdateScrollPrefsID());
}


void AdornedRulerPanel::OnLockPlayRegion(wxCommandEvent&)
{
   const auto &viewInfo = ViewInfo::Get( *GetProject() );
   const auto &playRegion = viewInfo.playRegion;
   if (playRegion.Locked())
      SelectUtilities::UnlockPlayRegion(*mProject);
   else
      SelectUtilities::LockPlayRegion(*mProject);
}


// Draws the horizontal <===>
void AdornedRulerPanel::DoDrawPlayRegion(wxDC * dc)
{
   const auto &viewInfo = ViewInfo::Get( *GetProject() );
   const auto &playRegion = viewInfo.playRegion;
   auto start = playRegion.GetStart();
   auto end = playRegion.GetEnd();

   if (start >= 0)
   {
      const int x1 = Time2Pos(start);
      const int x2 = Time2Pos(end)-2;
      int y = mInner.y - TopMargin + mInner.height/2;

      bool isLocked = mLastPlayRegionLocked = playRegion.Locked();
      AColor::PlayRegionColor(dc, isLocked);

      wxPoint tri[3];
      wxRect r;

      tri[0].x = x1;
      tri[0].y = y + PLAY_REGION_GLOBAL_OFFSET_Y;
      tri[1].x = x1 + PLAY_REGION_TRIANGLE_SIZE;
      tri[1].y = y - PLAY_REGION_TRIANGLE_SIZE + PLAY_REGION_GLOBAL_OFFSET_Y;
      tri[2].x = x1 + PLAY_REGION_TRIANGLE_SIZE;
      tri[2].y = y + PLAY_REGION_TRIANGLE_SIZE + PLAY_REGION_GLOBAL_OFFSET_Y;
      dc->DrawPolygon(3, tri);

      r.x = x1;
      r.y = y - PLAY_REGION_TRIANGLE_SIZE + PLAY_REGION_GLOBAL_OFFSET_Y;
      r.width = PLAY_REGION_RECT_WIDTH;
      r.height = PLAY_REGION_TRIANGLE_SIZE*2 + 1;
      dc->DrawRectangle(r);

      if (end != start)
      {
         tri[0].x = x2;
         tri[0].y = y + PLAY_REGION_GLOBAL_OFFSET_Y;
         tri[1].x = x2 - PLAY_REGION_TRIANGLE_SIZE;
         tri[1].y = y - PLAY_REGION_TRIANGLE_SIZE + PLAY_REGION_GLOBAL_OFFSET_Y;
         tri[2].x = x2 - PLAY_REGION_TRIANGLE_SIZE;
         tri[2].y = y + PLAY_REGION_TRIANGLE_SIZE + PLAY_REGION_GLOBAL_OFFSET_Y;
         dc->DrawPolygon(3, tri);

         r.x = x2 - PLAY_REGION_RECT_WIDTH + 1;
         r.y = y - PLAY_REGION_TRIANGLE_SIZE + PLAY_REGION_GLOBAL_OFFSET_Y;
         r.width = PLAY_REGION_RECT_WIDTH;
         r.height = PLAY_REGION_TRIANGLE_SIZE*2 + 1;
         dc->DrawRectangle(r);

         r.x = x1 + PLAY_REGION_TRIANGLE_SIZE;
         r.y = y - PLAY_REGION_RECT_HEIGHT/2 + PLAY_REGION_GLOBAL_OFFSET_Y;
         r.width = std::max(0, x2-x1 - PLAY_REGION_TRIANGLE_SIZE*2);
         r.height = PLAY_REGION_RECT_HEIGHT;
         dc->DrawRectangle(r);
      }
   }
}

void AdornedRulerPanel::ShowContextMenu( MenuChoice choice, const wxPoint *pPosition)
{
   wxPoint position;
   if(pPosition)
      position = *pPosition;
   else
   {
      auto rect = GetRect();
      //Old code put menu too low down.  y position applied twice.
      //position = { rect.GetLeft() + 1, rect.GetBottom() + 1 };

      // The cell does not pass in the mouse or button position.
      // We happen to know this is the pin/unpin button
      // so these magic values 'fix a bug' - but really the cell should
      // pass more information to work with in.
      position = { rect.GetLeft() + 38, rect.GetHeight()/2 + 1 };
   }

   switch (choice) {
      case MenuChoice::QuickPlay:
         ShowMenu(position); 
         UpdateButtonStates();
         break;
      case MenuChoice::Scrub:
         ShowScrubMenu(position); break;
      default:
         return;
   }
}

void AdornedRulerPanel::DoDrawBackground(wxDC * dc)
{
   // Draw AdornedRulerPanel border
   AColor::UseThemeColour( dc, clrTrackInfo );
   dc->DrawRectangle( mInner );

   if (ShowingScrubRuler()) {
      // Let's distinguish the scrubbing area by using a themable
      // colour and a line to set it off.  
      AColor::UseThemeColour(dc, clrScrubRuler, clrTrackPanelText );
      wxRect ScrubRect = mScrubZone;
      ScrubRect.Inflate( 1,0 );
      dc->DrawRectangle(ScrubRect);
   }
}

void AdornedRulerPanel::DoDrawEdge(wxDC *dc)
{
   wxRect r = mOuter;
   r.width -= RightMargin;
   r.height -= BottomMargin;
   AColor::BevelTrackInfo( *dc, true, r );

   // Black stroke at bottom
   dc->SetPen( *wxBLACK_PEN );
   AColor::Line( *dc, mOuter.x,
                mOuter.y + mOuter.height - 1,
                mOuter.x + mOuter.width - 1	,
                mOuter.y + mOuter.height - 1 );
}

void AdornedRulerPanel::DoDrawMarks(wxDC * dc, bool /*text */ )
{
   const double min = Pos2Time(0);
   const double hiddenMin = Pos2Time(0, true);
   const double max = Pos2Time(mInner.width);
   const double hiddenMax = Pos2Time(mInner.width, true);

   mRuler.SetTickColour( theTheme.Colour( clrTrackPanelText ) );
   mRuler.SetRange( min, max, hiddenMin, hiddenMax );
   mRuler.Draw( *dc );
}

void AdornedRulerPanel::DrawSelection()
{
   Refresh();
}

void AdornedRulerPanel::DoDrawSelection(wxDC * dc)
{
   // Draw selection
   const int p0 = max(1, Time2Pos(mViewInfo->selectedRegion.t0()));
   const int p1 = min(mInner.width, Time2Pos(mViewInfo->selectedRegion.t1()));

   dc->SetBrush( wxBrush( theTheme.Colour( clrRulerBackground )) );
   dc->SetPen(   wxPen(   theTheme.Colour( clrRulerBackground )) );

   wxRect r;
   r.x = p0;
   r.y = mInner.y;
   r.width = p1 - p0 - 1;
   r.height = mInner.height;
   dc->DrawRectangle( r );
}

int AdornedRulerPanel::GetRulerHeight(bool showScrubBar)
{
   return ProperRulerHeight + (showScrubBar ? ScrubHeight : 0);
}

void AdornedRulerPanel::SetLeftOffset(int offset)
{
   mLeftOffset = offset;
   mRuler.SetUseZoomInfo(offset, mViewInfo);
}

// Draws the play/recording position indicator.
void AdornedRulerPanel::DoDrawIndicator
   (wxDC * dc, wxCoord xx, bool playing, int width, bool scrub, bool seek)
{
   ADCChanger changer(dc); // Undo pen and brush changes at function exit

   AColor::IndicatorColor( dc, playing );

   wxPoint tri[ 3 ];
   if (seek) {
      auto height = IndicatorHeightForWidth(width);
      // Make four triangles
      const int TriangleWidth = width * 3 / 8;

      // Double-double headed, left-right
      auto yy = ShowingScrubRuler()
      ? mScrubZone.y
      : (mInner.GetBottom() + 1) - 1 /* bevel */ - height;
      tri[ 0 ].x = xx - IndicatorOffset;
      tri[ 0 ].y = yy;
      tri[ 1 ].x = xx - IndicatorOffset;
      tri[ 1 ].y = yy + height;
      tri[ 2 ].x = xx - TriangleWidth;
      tri[ 2 ].y = yy + height / 2;
      dc->DrawPolygon( 3, tri );

      tri[ 0 ].x -= TriangleWidth;
      tri[ 1 ].x -= TriangleWidth;
      tri[ 2 ].x -= TriangleWidth;
      dc->DrawPolygon( 3, tri );

      tri[ 0 ].x = tri[ 1 ].x = xx + IndicatorOffset;
      tri[ 2 ].x = xx + TriangleWidth;
      dc->DrawPolygon( 3, tri );


      tri[ 0 ].x += TriangleWidth;
      tri[ 1 ].x += TriangleWidth;
      tri[ 2 ].x += TriangleWidth;
      dc->DrawPolygon( 3, tri );
   }
   else if (scrub) {
      auto height = IndicatorHeightForWidth(width);
      const int IndicatorHalfWidth = width / 2;

      // Double headed, left-right
      auto yy = ShowingScrubRuler()
         ? mScrubZone.y
         : (mInner.GetBottom() + 1) - 1 /* bevel */ - height;
      tri[ 0 ].x = xx - IndicatorOffset;
      tri[ 0 ].y = yy;
      tri[ 1 ].x = xx - IndicatorOffset;
      tri[ 1 ].y = yy + height;
      tri[ 2 ].x = xx - IndicatorHalfWidth;
      tri[ 2 ].y = yy + height / 2;
      dc->DrawPolygon( 3, tri );
      tri[ 0 ].x = tri[ 1 ].x = xx + IndicatorOffset;
      tri[ 2 ].x = xx + IndicatorHalfWidth;
      dc->DrawPolygon( 3, tri );
   }
   else {
      auto pair = GetIndicatorBitmap( xx, playing );
      dc->DrawBitmap( pair.second, pair.first.x, pair.first.y );
#if 0

      // Down pointing triangle
      auto height = IndicatorHeightForWidth(width);
      const int IndicatorHalfWidth = width / 2;
      tri[ 0 ].x = xx - IndicatorHalfWidth;
      tri[ 0 ].y = mInner.y;
      tri[ 1 ].x = xx + IndicatorHalfWidth;
      tri[ 1 ].y = mInner.y;
      tri[ 2 ].x = xx;
      tri[ 2 ].y = mInner.y + height;
      dc->DrawPolygon( 3, tri );
#endif
   }
}

// Returns the appropriate bitmap, and panel-relative coordinates for its
// upper left corner.
std::pair< wxPoint, wxBitmap >
AdornedRulerPanel::GetIndicatorBitmap(wxCoord xx, bool playing) const
{
   bool pinned = Scrubber::Get( *mProject ).IsTransportingPinned();
   wxBitmap & bmp = theTheme.Bitmap( pinned ?
      (playing ? bmpPlayPointerPinned : bmpRecordPointerPinned) :
      (playing ? bmpPlayPointer : bmpRecordPointer)
   );
   const int IndicatorHalfWidth = bmp.GetWidth() / 2;
   return {
      { xx - IndicatorHalfWidth - 1, mInner.y },
      bmp
   };
}

void AdornedRulerPanel::SetPlayRegion(double playRegionStart,
                                      double playRegionEnd)
{
   // This is called by AudacityProject to make the play region follow
   // the current selection. But while the user is selecting a play region
   // with the mouse directly in the ruler, changes from outside are blocked.
   if (mMouseEventState != mesNone)
      return;

   auto &viewInfo = ViewInfo::Get( *GetProject() );
   auto &playRegion = viewInfo.playRegion;
   playRegion.SetTimes( playRegionStart, playRegionEnd );

   Refresh();
}

void AdornedRulerPanel::ClearPlayRegion()
{
   ProjectAudioManager::Get( *mProject ).Stop();

   auto &viewInfo = ViewInfo::Get( *GetProject() );
   auto &playRegion = viewInfo.playRegion;
   playRegion.SetTimes( -1, -1 );

   Refresh();
}

void AdornedRulerPanel::GetMaxSize(wxCoord *width, wxCoord *height)
{
   mRuler.GetMaxSize(width, height);
}

bool AdornedRulerPanel::s_AcceptsFocus{ false };

auto AdornedRulerPanel::TemporarilyAllowFocus() -> TempAllowFocus {
   s_AcceptsFocus = true;
   return TempAllowFocus{ &s_AcceptsFocus };
}

void AdornedRulerPanel::SetFocusFromKbd()
{
   auto temp = TemporarilyAllowFocus();
   SetFocus();
}

// Second-level subdivision includes quick-play region and maybe the scrub bar
// and also shaves little margins above and below
struct AdornedRulerPanel::Subgroup final : TrackPanelGroup {
   explicit Subgroup( const AdornedRulerPanel &ruler ) : mRuler{ ruler } {}
   Subdivision Children( const wxRect & ) override
   {
      return { Axis::Y, ( mRuler.ShowingScrubRuler() )
         ? Refinement{
            { mRuler.mInner.GetTop(), mRuler.mQPCell },
            { mRuler.mScrubZone.GetTop(), mRuler.mScrubbingCell },
            { mRuler.mScrubZone.GetBottom() + 1, nullptr }
         }
         : Refinement{
            { mRuler.mInner.GetTop(), mRuler.mQPCell },
            { mRuler.mInner.GetBottom() + 1, nullptr }
         }
      };
   }
   const AdornedRulerPanel &mRuler;
};

// Top-level subdivision shaves little margins off left and right
struct AdornedRulerPanel::MainGroup final : TrackPanelGroup {
   explicit MainGroup( const AdornedRulerPanel &ruler ) : mRuler{ ruler } {}
   Subdivision Children( const wxRect & ) override
   { return { Axis::X, Refinement{
      // Subgroup is a throwaway object
      { mRuler.mInner.GetLeft(), std::make_shared< Subgroup >( mRuler ) },
      { mRuler.mInner.GetRight() + 1, nullptr }
   } }; }
   const AdornedRulerPanel &mRuler;
};

bool AdornedRulerPanel::ShowingScrubRuler() const
{
   auto &scrubber = Scrubber::Get( *GetProject() );
   return scrubber.ShowsBar();
}

// CellularPanel implementation
std::shared_ptr<TrackPanelNode> AdornedRulerPanel::Root()
{
   // Root is a throwaway object
   return std::make_shared< MainGroup >( *this );
}

AudacityProject * AdornedRulerPanel::GetProject() const
{
   return mProject;
}


TrackPanelCell *AdornedRulerPanel::GetFocusedCell()
{
   // No switching of focus yet to the other, scrub zone
   return mQPCell.get();
}


void AdornedRulerPanel::SetFocusedCell()
{
}


void AdornedRulerPanel::ProcessUIHandleResult
(TrackPanelCell *pClickedTrack, TrackPanelCell *pLatestCell,
 unsigned refreshResult)
{
   (void)pLatestCell;// Compiler food
   (void)pClickedTrack;// Compiler food
   if (refreshResult & RefreshCode::DrawOverlays)
      DrawBothOverlays();
}

void AdornedRulerPanel::UpdateStatusMessage( const TranslatableString &message )
{
   ProjectStatus::Get( *GetProject() ).Set(message);
}

void AdornedRulerPanel::CreateOverlays()
{
   if (!mOverlay) {
      mOverlay =
         std::make_shared<QuickPlayIndicatorOverlay>( mProject );
      auto pCellularPanel =
         dynamic_cast<CellularPanel*>( &GetProjectPanel( *GetProject() ) );
      if ( !pCellularPanel ) {
         wxASSERT( false );
      }
      else
         pCellularPanel->AddOverlay( mOverlay );
      this->AddOverlay( mOverlay->mPartner );
   }
}

void AdornedRulerPanel::TogglePinnedHead()
{
   bool value = !TracksPrefs::GetPinnedHeadPreference();
   TracksPrefs::SetPinnedHeadPreference(value, true);
   MenuManager::ModifyAllProjectToolbarMenus();

   auto &project = *mProject;
   // Update button image
   UpdateButtonStates();

   auto &scrubber = Scrubber::Get( project );
   if (scrubber.HasMark())
      scrubber.SetScrollScrubbing(value);
}
