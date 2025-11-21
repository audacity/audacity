/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackFocus.h

  Leland Lucius

  Paul Licameli split from TrackPanelAx.h

**********************************************************************/
#ifndef __AUDACITY_TRACK_FOCUS__
#define __AUDACITY_TRACK_FOCUS__

#include <memory>

#include "ClientData.h" // to inherit
#include "Observer.h"

class AudacityProject;
class Track;
class TrackList;

struct TrackFocusChangeMessage {
    bool focusPanel = false; //!< whether to focus the window that shows tracks
};

//! A façade hiding platform-specific accessibility API
struct TRACK_SELECTION_API TrackFocusCallbacks {
    virtual ~TrackFocusCallbacks();

    virtual void MessageForScreenReader(const TranslatableString& message) = 0;
    virtual void BeginChangeFocus() = 0;
    virtual void EndChangeFocus(const std::shared_ptr<Track>& track) = 0;
    virtual void UpdateAccessibility() = 0;
};

class TRACK_SELECTION_API TrackFocus final : public ClientData::Base, public Observer::Publisher<TrackFocusChangeMessage>,
    public std::enable_shared_from_this<TrackFocus>
{
public:
    static TrackFocus& Get(AudacityProject& project);
    static const TrackFocus& Get(const AudacityProject& project);

    explicit TrackFocus(AudacityProject& project);
    ~TrackFocus() final;

    TrackFocus(const TrackFocus&) = delete;
    TrackFocus& operator=(const TrackFocus&) = delete;

    void SetCallbacks(std::unique_ptr<TrackFocusCallbacks> pCallbacks);

    /*!
     @return the currently focused track, which may be null

     This function is not const, because it may have a side effect of setting
     a focus if none was already set
     */

    Track* Get();

    //! Set the track focus to a given track or to null
    /*!
     @param focusPanel whether also to focus the window that shows tracks
     */
    void Set(Track* pTrack, bool focusPanel = false);

    void MessageForScreenReader(const TranslatableString& message);

    //! Called to signal changes to a track
    void UpdateAccessibility();

    // Returns currently focused track, never changing it
    std::shared_ptr<Track> PeekFocus() const;

    int TrackNum(const std::shared_ptr<Track>& track) const;

    int NumFocusedTrack() const { return mNumFocusedTrack; }

    // Returns currently focused track or first one if none focused
    std::shared_ptr<Track> GetFocus();

    const TrackList& GetTracks() const;

    std::shared_ptr<Track> FindTrack(int num) const;

    // Changes focus to a specified track
    // Return is the actual focused track, which may be different from
    // the argument when that is null
    std::shared_ptr<Track> SetFocus(std::shared_ptr<Track> track = {}, bool focusPanel = false);

private:
    TrackList& GetTracks();

    AudacityProject& mProject;
    std::unique_ptr<TrackFocusCallbacks> mpCallbacks;
    std::weak_ptr<Track> mFocusedTrack;
    int mNumFocusedTrack{ 0 };
};

#endif // __AUDACITY_TRACK_FOCUS__
