/**********************************************************************

Audacity: A Digital Audio Editor

ProjectWindow.h

Paul Licameli split from AudacityProject.h

**********************************************************************/

#ifndef __AUDACITY_PROJECT_WINDOW__
#define __AUDACITY_PROJECT_WINDOW__

#include <memory>
#include "ClientData.h"
#include "ProjectWindowBase.h" // to inherit
#include "Prefs.h"
#include "Observer.h"

class CommandContext;
class Track;
class TrackList;

class wxScrollBar;
class wxPanel;
class wxSplitterWindow;
class RealtimeEffectPanel;
enum class ProjectFileIOMessage : int;

class ProjectWindow;
void InitProjectWindow( ProjectWindow &window );

//! Message sent when the project window is closed.
struct ProjectWindowDestroyedMessage final : Observer::Message {};

//! A callback facade hiding GUI toolkit details
class AUDACITY_DLL_API ViewportCallbacks {
public:
   virtual ~ViewportCallbacks();

   //! Width and height in pixels of proper viewport area (excluding scrollbars)
   virtual std::pair<int, int> ViewportSize() const = 0;

   virtual bool MayScrollBeyondZero() const = 0;

   virtual unsigned MinimumTrackHeight() = 0;
   virtual bool IsTrackMinimized(const Track &track) = 0;
   virtual void SetMinimized(Track &track, bool minimized) = 0;
   virtual int GetTrackHeight(const Track &track) = 0;
   virtual void SetChannelHeights(Track &track, unsigned height) = 0;
   virtual int GetTotalHeight(const TrackList &trackList) = 0;

   virtual int GetHorizontalThumbPosition() const = 0;
   virtual int GetHorizontalThumbSize() const = 0;
   virtual int GetHorizontalRange() const = 0;
   virtual void SetHorizontalThumbPosition(int viewStart) = 0;
   virtual void SetHorizontalScrollbar(int position, int thumbSize,
      int range, int pageSize, bool refresh) = 0;
   virtual void ShowHorizontalScrollbar(bool shown) = 0;

   virtual int GetVerticalThumbPosition() const = 0;
   virtual int GetVerticalThumbSize() const = 0;
   virtual int GetVerticalRange() const = 0;
   virtual void SetVerticalThumbPosition(int viewStart) = 0;
   virtual void SetVerticalScrollbar(int position, int thumbSize,
      int range, int pageSize, bool refresh) = 0;
   virtual void ShowVerticalScrollbar(bool shown) = 0;
};

struct ViewportMessage {
   const bool rescroll;
   const bool scrollbarVisibilityChanged;
   const bool resize;
};

class AUDACITY_DLL_API Viewport final
   : public Observer::Publisher<ViewportMessage>
   , public ClientData::Base
{
public:
   static Viewport &Get(AudacityProject &project);
   static const Viewport &Get(const AudacityProject &project);

   explicit Viewport(AudacityProject &project);
   void SetCallbacks(std::unique_ptr<ViewportCallbacks> pCallbacks);

   double ScrollingLowerBoundTime() const;

   //! Cause refresh of viewport contents after setting scrolling or zooming
   void DoScroll();

   /*!
    This method 'rewinds' the track, by setting the cursor to 0 and
    scrolling the window to fit 0 on the left side of it
    (maintaining current zoom).
    If extend is true, it will extend the left edge of the
    selection to 0 (holding right edge constant), otherwise it will
    move both left and right edge of selection to 0
    */
   void ScrollToStart(bool extend);

   /*!
    This method 'fast-forwards' the track, by setting the cursor to
    the end of the samples on the selected track and scrolling the
    window to fit the end on its right side (maintaining current zoom).
    If extend is true, it will extend the right edge of the
    selection to the end (holding left edge constant), otherwise it will
    move both left and right edge of selection to the end
    */
   void ScrollToEnd(bool extend);

   void ScrollToTop();
   void ScrollToBottom();

   //! Center view horizontally at the given time, if it was not in bounds
   void ScrollIntoView(double pos);

   //! Center the view horizontally at the given pixel position relative to
   //! the left edge, if it was not in bounds
   void ScrollIntoView(int x);

   /// This method handles general left-scrolling, either for drag-scrolling
   /// or when the scrollbar is clicked to the left of the thumb
   void OnScrollLeft();

   /// This method handles general right-scrolling, either for drag-scrolling
   /// or when the scrollbar is clicked to the right of the thumb
   void OnScrollRight();

   ///  This handles the event when the left direction button on the scrollbar
   ///  is depressed
   void OnScrollLeftButton();

   ///  This handles  the event when the right direction button on the scrollbar
   ///  is depressed
   void OnScrollRightButton();

   void OnScroll();

   // Scroll vertically. A positive argument makes the window
   // scroll down, while a negative argument scrolls up.
   bool ScrollUpDown(int delta);

   void SetHorizontalThumb(double scrollto, bool doScroll = true);

   //! Set timeline magnification; unchanged left edge time
   void Zoom(double pixelsPerSecond);

   //! Multiply the magnification; unchanged left edge time
   void ZoomBy(double multiplier);

   //! Multiply timeline magnification, conserving a selected portion that
   //! incompletely fills the width, if possible; else like ZoomAboutCenter
   void ZoomAboutSelection(double multiplier);

   //! Multiply timeline magnification, conserving the midpoint time if possible
   void ZoomAboutCenter(double multiplier);

   //! Fit horizontally; scroll vertically so that the given track (or if that's
   //! null, the first selected track, or if none such, the first track) is
   //! visible
   void ZoomFitHorizontallyAndShowTrack(Track *pTrack);

   //! Find pixels-per-second that would fit all tracks on the timeline
   double GetZoomOfToFit() const;

   /// Set horizontal zoom according to the extents of the tracks, and scroll
   /// to the start
   void ZoomFitHorizontally();

   /// Give uncollapsed audio tracks equal height, fitting into the view if
   /// possible, and scroll to the top
   void ZoomFitVertically();

   void ExpandAllTracks();
   void CollapseAllTracks();

   //! Change scrollbar bounds in response to changes in the TrackList,
   //! and sometimes rescroll to the top or left and repaint the whole view
   void UpdateScrollbarsForTracks();

   void HandleResize();

   void ReinitScrollbars() { mbInitializingScrollbar = true; }

private:
   // How many pixels are covered by the period from lowermost scrollable time, to the given time:
   // PRL: Bug1197: we seem to need to compute all in double, to avoid differing results on Mac
   double PixelWidthBeforeTime(double scrollto) const;

   void FinishAutoScroll();

   AudacityProject &mProject;
   std::unique_ptr<ViewportCallbacks> mpCallbacks{};
   bool mAutoScrolling{ false };
   bool mbInitializingScrollbar{ false };
};

///\brief A top-level window associated with a project, and handling scrollbars
/// and zooming
class AUDACITY_DLL_API ProjectWindow final : public ProjectWindowBase
, public PrefsListener
   , public Observer::Publisher<ProjectWindowDestroyedMessage>
{
public:
   using Observer::Publisher<ProjectWindowDestroyedMessage>::Publish;
   static ProjectWindow &Get( AudacityProject &project );
   static const ProjectWindow &Get( const AudacityProject &project );
   static ProjectWindow *Find( AudacityProject *pProject );
   static const ProjectWindow *Find( const AudacityProject *pProject );

   static void OnResetWindow(const CommandContext& context);

   explicit ProjectWindow(
      wxWindow * parent, wxWindowID id,
      const wxPoint & pos, const wxSize &size,
      AudacityProject &project );
   ~ProjectWindow() override;

   // Next available ID for sub-windows
   int NextWindowID();

   bool IsActive() override;
   bool IsIconized() const override;

   bool IsBeingDeleted() const { return mIsDeleting; }
   void SetIsBeingDeleted() { mIsDeleting = true; }

   void Reset();

   /**
    * \brief Track list window is the parent container for TrackPanel
    * \return Pointer to a track list window (not null)
    */
   wxWindow* GetTrackListWindow() noexcept;
   /**
    * \brief Container is a parent window for both effects panel and
    * track list windows
    * \return Pointer to a container window (not null)
    */
   wxSplitterWindow* GetContainerWindow() noexcept;
   /**
    * \brief Top panel contains project-related controls and tools.
    * \return Pointer to a top panel window (not null)
    */
   wxPanel *GetTopPanel() noexcept;

   void UpdateStatusWidths();

   struct PlaybackScrollerMessage : Observer::Message {};

   class PlaybackScroller final
      : public Observer::Publisher<PlaybackScrollerMessage>
   {
   public:
      explicit PlaybackScroller(AudacityProject *project);

      enum class Mode {
         Off,
         Refresh,
         Pinned,
         Right,
      };

      Mode GetMode() const { return mMode; }
      void Activate(Mode mode)
      {
         mMode = mode;
      }

      double GetRecentStreamTime() const { return mRecentStreamTime; }

      void OnTimer();

   private:
      AudacityProject *mProject;
      Mode mMode { Mode::Off };

      // During timer update, grab the volatile stream time just once, so that
      // various other drawing code can use the exact same value.
      double mRecentStreamTime{ -1.0 };
   };
   PlaybackScroller &GetPlaybackScroller() { return *mPlaybackScroller; }

   void SetNormalizedWindowState(wxRect pSizeAndLocation) {  mNormalizedWindowState = pSizeAndLocation;   }
   wxRect GetNormalizedWindowState() const { return mNormalizedWindowState;   }

   void RedrawProject();
   
   void ApplyUpdatedTheme();

   // Scrollbars

   wxScrollBar &GetVerticalScrollBar() { return *mVsbar; }
   wxScrollBar &GetHorizontalScrollBar() { return *mHsbar; }

   void OnScrollLeftButton(wxScrollEvent & event);
   void OnScrollRightButton(wxScrollEvent & event);

   std::pair<int, int> ViewportSize() const;
   bool MayScrollBeyondZero() const;
   unsigned MinimumTrackHeight() ;
   bool IsTrackMinimized(const Track &track) ;
   void SetMinimized(Track &track, bool minimized) ;
   int GetTrackHeight(const Track &track) ;
   void SetChannelHeights(Track &track, unsigned height) ;
   int GetTotalHeight(const TrackList &trackList) ;
   int GetHorizontalThumbPosition() const ;
   int GetHorizontalThumbSize() const ;
   int GetHorizontalRange() const ;
   void SetHorizontalThumbPosition(int viewStart) ;
   void SetHorizontalScrollbar(int position, int thumbSize,
      int range, int pageSize, bool refresh) ;
   void ShowHorizontalScrollbar(bool shown) ;

   int GetVerticalThumbPosition() const ;
   int GetVerticalThumbSize() const ;
   int GetVerticalRange() const ;
   void SetVerticalThumbPosition(int viewStart) ;
   void SetVerticalScrollbar(int position, int thumbSize,
      int range, int pageSize, bool refresh) ;
   void ShowVerticalScrollbar(bool shown) ;

   // PRL:  old and incorrect comment below, these functions are used elsewhere than TrackPanel
   // TrackPanel access
   wxSize GetTPTracksUsableArea() /* not override */;
   void RefreshTPTrack(Track* pTrk, bool refreshbacking = true) /* not override */;

 private:

   void OnThemeChange(struct ThemeChangeMessage);

   // PrefsListener implementation
   void UpdatePrefs() override;

 public:
   // Message Handlers

   void OnMenu(wxCommandEvent & event);
   void OnUpdateUI(wxUpdateUIEvent & event);

   void MacShowUndockedToolbars(bool show);
   void OnActivate(wxActivateEvent & event);

   void OnMouseEvent(wxMouseEvent & event);
   void OnIconize(wxIconizeEvent &event);
   void OnSize(wxSizeEvent & event);
   void UpdateLayout();
   void OnShow(wxShowEvent & event);
   void OnMove(wxMoveEvent & event);
   void OnScroll(wxScrollEvent & event);
   void OnToolBarUpdate(wxCommandEvent & event);
   void OnUndoPushedModified();
   void OnUndoRedo();
   void OnUndoReset();
   void OnProjectTitleChange(ProjectFileIOMessage);

private:
   wxRect mNormalizedWindowState;

   wxPanel *mTopPanel{};
   wxSplitterWindow* mContainerWindow;
   wxWindow* mTrackListWindow{};
   
   wxScrollBar *mHsbar{};
   wxScrollBar *mVsbar{};

   int mNextWindowID{};

   bool mActive{ true };
   bool mIconized{ false };
   bool mShownOnce{ false };



   bool mIsDeleting{ false };

private:
   void OnViewportMessage(const ViewportMessage &message);

   Observer::Subscription mUndoSubscription
      , mThemeChangeSubscription
      , mTitleChangeSubscription
      , mSnappingChangedSubscription
   ;
   std::unique_ptr<PlaybackScroller> mPlaybackScroller;
   const Observer::Subscription mViewportSubscription;

   DECLARE_EVENT_TABLE()
};

void GetDefaultWindowRect(wxRect *defRect);
void GetNextWindowPlacement(wxRect *nextRect, bool *pMaximized, bool *pIconized);

extern AUDACITY_DLL_API BoolSetting ProjectWindowMaximized;
extern AUDACITY_DLL_API BoolSetting ProjectWindowIconized;
extern AUDACITY_DLL_API IntSetting ProjectWindowX;
extern AUDACITY_DLL_API IntSetting ProjectWindowY;
extern AUDACITY_DLL_API IntSetting ProjectWindowWidth;
extern AUDACITY_DLL_API IntSetting ProjectWindowHeight;
extern AUDACITY_DLL_API IntSetting ProjectWindowNormalX;
extern AUDACITY_DLL_API IntSetting ProjectWindowNormalY;
extern AUDACITY_DLL_API IntSetting ProjectWindowNormalWidth;
extern AUDACITY_DLL_API IntSetting ProjectWindowNormalHeight;

#endif
