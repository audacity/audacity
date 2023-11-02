/**********************************************************************

Audacity: A Digital Audio Editor

ProjectWindow.h

Paul Licameli split from AudacityProject.h

**********************************************************************/

#ifndef __AUDACITY_PROJECT_WINDOW__
#define __AUDACITY_PROJECT_WINDOW__

#include <memory>
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

};

class AUDACITY_DLL_API Viewport
   : public Observer::Publisher<ViewportMessage>
{
public:
   explicit Viewport(AudacityProject &project);
   void SetCallbacks(ViewportCallbacks *pCallbacks);

   double ScrollingLowerBoundTime() const;

   //! Cause refresh of viewport contents after setting scrolling or zooming
   void DoScroll();

   /*!
    This method 'rewinds' the track, by setting the cursor to 0 and
    scrolling the window to fit 0 on the left side of it
    (maintaining  current zoom).
    If extend is true, it will extend the left edge of the
    selection to 0 (holding right edge constant), otherwise it will
    move both left and right edge of selection to 0
    */
   void ScrollToStart(bool extend);

   void ScrollToTop();

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

   void SetHorizontalThumb(double scrollto, bool doScroll = true);

   /// Give uncollapsed audio tracks equal height, fitting into the view if
   /// possible, and scroll to the top
   void ZoomFitVertically();

   void ExpandAllTracks();
   void CollapseAllTracks();

protected:
   // How many pixels are covered by the period from lowermost scrollable time, to the given time:
   // PRL: Bug1197: we seem to need to compute all in double, to avoid differing results on Mac
   double PixelWidthBeforeTime(double scrollto) const;

private:
   void FinishAutoScroll();

   std::shared_ptr<AudacityProject> LockProject()
      { return mwProject.lock(); }
   std::shared_ptr<const AudacityProject> LockProject() const
      { return mwProject.lock(); }

   std::weak_ptr<AudacityProject> mwProject;
   ViewportCallbacks *mpCallbacks{};
   bool mAutoScrolling{ false };
};

///\brief A top-level window associated with a project, and handling scrollbars
/// and zooming
class AUDACITY_DLL_API ProjectWindow final : public ProjectWindowBase
   , public ViewportCallbacks
   , public Viewport // this inheritance will be removed
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

   void Zoom(double level);
   void ZoomInByFactor( double ZoomFactor );
   void ZoomOutByFactor( double ZoomFactor );
   void ZoomBy(double multiplier);
   void ZoomAfterImport(Track *pTrack);
   double GetZoomOfToFit() const;

   /// Set horizontal zoom according to the extents of the tracks, and scroll
   /// to the start
   void ZoomFitHorizontally();
   
   void ApplyUpdatedTheme();

   // Scrollbars

   wxScrollBar &GetVerticalScrollBar() { return *mVsbar; }
   wxScrollBar &GetHorizontalScrollBar() { return *mHsbar; }

   void ScrollIntoView(double pos);
   void ScrollIntoView(int x);

   void SkipEnd(bool shift);

   void ScrollToBottom();

   void OnScrollLeftButton(wxScrollEvent & event);
   void OnScrollRightButton(wxScrollEvent & event);

   //! Change scrollbar bounds in response to changes in the TrackList,
   //! and sometimes rescroll to the top or left and repaint the whole view
   void UpdateScrollbarsForTracks();

   std::pair<int, int> ViewportSize() const override;
   bool MayScrollBeyondZero() const override;
   unsigned MinimumTrackHeight() override;
   bool IsTrackMinimized(const Track &track) override;
   void SetMinimized(Track &track, bool minimized) override;
   int GetTrackHeight(const Track &track) override;
   void SetChannelHeights(Track &track, unsigned height) override;
   int GetTotalHeight(const TrackList &trackList) override;
   int GetHorizontalThumbPosition() const override;
   int GetHorizontalThumbSize() const override;
   int GetHorizontalRange() const override;
   void SetHorizontalThumbPosition(int viewStart) override;
   void SetHorizontalScrollbar(int position, int thumbSize,
      int range, int pageSize, bool refresh) override;
   void ShowHorizontalScrollbar(bool shown) override;

   int GetVerticalThumbPosition() const override;
   int GetVerticalThumbSize() const override;
   int GetVerticalRange() const override;
   void SetVerticalThumbPosition(int viewStart) override;
   void SetVerticalScrollbar(int position, int thumbSize,
      int range, int pageSize, bool refresh) override;
   void ShowVerticalScrollbar(bool shown) override;

   // PRL:  old and incorrect comment below, these functions are used elsewhere than TrackPanel
   // TrackPanel access
   wxSize GetTPTracksUsableArea() /* not override */;
   void RefreshTPTrack(Track* pTrk, bool refreshbacking = true) /* not override */;

   bool ScrollUpDown(int delta);
   void HandleResize();

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

   bool mbInitializingScrollbar{ false };

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
