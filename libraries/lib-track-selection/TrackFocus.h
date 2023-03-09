/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanelAx.h

  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL_ACCESSIBILITY__
#define __AUDACITY_TRACK_PANEL_ACCESSIBILITY__



#include <functional>
#include <memory>

#include <wx/setup.h> // for wxUSE_* macros
#include <wx/weakref.h>

#if wxUSE_ACCESSIBILITY
#include "WindowAccessible.h" // to inherit
#endif

#include "ClientData.h" // to inherit
#include "Observer.h"

class wxRect;
class wxWindow;

class AudacityProject;
class Track;
class TrackList;

struct TrackFocusChangeMessage {};

//! A façade hiding platform-specific accessibility API
struct TrackFocusCallbacks {
   virtual ~TrackFocusCallbacks();

   virtual void MessageForScreenReader(const TranslatableString& message) = 0;
   virtual void BeginChangeFocus() = 0;
   virtual void EndChangeFocus(const std::shared_ptr<Track> &track) = 0;
   virtual void UpdateAccessibility() = 0;
};

class AUDACITY_DLL_API TrackFocus final
   : public ClientData::Base
   , public Observer::Publisher<TrackFocusChangeMessage>
   , public std::enable_shared_from_this<TrackFocus>
{
public:
   static TrackFocus &Get(AudacityProject &project);
   static const TrackFocus &Get(const AudacityProject &project);

   explicit TrackFocus(AudacityProject &project);
   ~TrackFocus() final;

   TrackFocus(const TrackFocus &) = delete;
   TrackFocus& operator=(const TrackFocus &) = delete;
   
   void SetCallbacks(std::unique_ptr<TrackFocusCallbacks> pCallbacks);

   /*!
    @return the currently focused track, which may be null, otherwise is
    a leader track
   
    This function is not const, because it may have a side effect of setting
    a focus if none was already set
    */

   Track *Get();

   // Set the track focus to a given track or to null
   void Set(Track *pTrack);

   void MessageForScreenReader(const TranslatableString& message);

   //! Called to signal changes to a track
   void UpdateAccessibility();

   // Returns currently focused track, never changing it
   std::shared_ptr<Track> PeekFocus() const;

   int TrackNum(const std::shared_ptr<Track> &track) const;

   int NumFocusedTrack() const { return mNumFocusedTrack; }

   // Returns currently focused track or first one if none focused
   std::shared_ptr<Track> GetFocus();

   const TrackList &GetTracks() const;

   std::shared_ptr<Track> FindTrack(int num) const;

   // Changes focus to a specified track
   // Return is the actual focused track, which may be different from
   // the argument when that is null
   std::shared_ptr<Track> SetFocus(std::shared_ptr<Track> track = {});

private:
   TrackList &GetTracks();

   AudacityProject &mProject;
   std::unique_ptr<TrackFocusCallbacks> mpCallbacks;
   std::weak_ptr<Track> mFocusedTrack;
   int mNumFocusedTrack{ 0 };
};

class TrackPanelAx final
   : public TrackFocusCallbacks
#if wxUSE_ACCESSIBILITY
   , public WindowAccessible
#endif
   , public wxTrackable
{
public:
   // Adapts the mismatch of wxWeakPtr and std::weak_ptr
   struct Adapter : TrackFocusCallbacks {
      Adapter(TrackPanelAx *pAx) : mwAx{ pAx } {}

      ~Adapter() override;
      void MessageForScreenReader(const TranslatableString& message) override;
      void BeginChangeFocus() override;
      void EndChangeFocus(const std::shared_ptr<Track> &track) override;
      void UpdateAccessibility() override;

   private:
      const wxWeakRef<TrackPanelAx> mwAx;
   };

   using RectangleFinder = std::function< wxRect(Track&) >;

   explicit TrackPanelAx(
      std::weak_ptr<TrackFocus> wFocus, RectangleFinder finder);
   ~TrackPanelAx() override;


   // Called to signal changes to a track
   void Updated();

   void MessageForScreenReader(const TranslatableString& message) override;

#if wxUSE_ACCESSIBILITY
   // Retrieves the address of an IDispatch interface for the specified child.
   // All objects must support this property.
   wxAccStatus GetChild(int childId, wxAccessible** child) override;

   // Gets the number of children.
   wxAccStatus GetChildCount(int* childCount) override;

   // Gets the default action for this object (0) or > 0 (the action for a child).
   // Return wxACC_OK even if there is no action. actionName is the action, or the empty
   // string if there is no action.
   // The retrieved string describes the action that is performed on an object,
   // not what the object does as a result. For example, a toolbar button that prints
   // a document has a default action of "Press" rather than "Prints the current document."
   wxAccStatus GetDefaultAction(int childId, wxString *actionName) override;

   // Returns the description for this object or a child.
   wxAccStatus GetDescription(int childId, wxString *description) override;

   // Gets the window with the keyboard focus.
   // If childId is 0 and child is NULL, no object in
   // this subhierarchy has the focus.
   // If this object has the focus, child should be 'this'.
   wxAccStatus GetFocus(int *childId, wxAccessible **child) override;

   // Returns help text for this object or a child, similar to tooltip text.
   wxAccStatus GetHelpText(int childId, wxString *helpText) override;

   // Returns the keyboard shortcut for this object or child.
   // Return e.g. ALT+K
   wxAccStatus GetKeyboardShortcut(int childId, wxString *shortcut) override;

   // Returns the rectangle for this object (id = 0) or a child element (id > 0).
   // rect is in screen coordinates.
   wxAccStatus GetLocation(wxRect& rect, int elementId) override;

   // Gets the name of the specified object.
   wxAccStatus GetName(int childId, wxString *name) override;

   // Returns a role constant.
   wxAccStatus GetRole(int childId, wxAccRole *role) override;

   // Gets a variant representing the selected children
   // of this object.
   // Acceptable values:
   // - a null variant (IsNull() returns TRUE)
   // - a list variant (GetType() == wxT("list"))
   // - an integer representing the selected child element,
   //   or 0 if this object is selected (GetType() == wxT("long"))
   // - a "void*" pointer to a wxAccessible child object
   wxAccStatus GetSelections(wxVariant *selections) override;

   // Returns a state constant.
   wxAccStatus GetState(int childId, long* state) override;

   // Returns a localized string representing the value for the object
   // or child.
   wxAccStatus GetValue(int childId, wxString* strValue) override;

   // Navigates from fromId to toId/toObject
   wxAccStatus Navigate(wxNavDir navDir, int fromId, int* toId, wxAccessible** toObject) override;

   // Modify focus or selection
   wxAccStatus Select(int childId, wxAccSelectionFlags selectFlags) override;
#else
   wxWindow *GetWindow() const { return mWindow; }
   void SetWindow( wxWindow *window ) { mWindow = window; }
#endif

private:
   void BeginChangeFocus() override;
   void EndChangeFocus(const std::shared_ptr<Track> &track) override;
   void UpdateAccessibility() override;

   std::weak_ptr<TrackFocus> mwFocus;

#if !wxUSE_ACCESSIBILITY
   wxWindow *mWindow{};
#endif

   RectangleFinder mFinder;

   wxString mMessage;
   int mMessageCount{ 0 };
   bool mTrackName{ true };
};

#endif // __AUDACITY_TRACK_PANEL_ACCESSIBILITY__
