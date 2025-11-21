/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackFocus.cpp

  Leland Lucius
  and lots of other contributors

*//*******************************************************************/
#include "TrackFocus.h"

#include "BasicUI.h"
#include "Project.h"
#include "Track.h"

TrackFocusCallbacks::~TrackFocusCallbacks() = default;

TrackList& TrackFocus::GetTracks()
{
    return TrackList::Get(mProject);
}

const TrackList& TrackFocus::GetTracks() const
{
    return TrackList::Get(mProject);
}

// Returns currently focused track
// if that track no longer exists, if there is a track at
// the same position, use that, else if there is a first
// track, use that.
std::shared_ptr<Track> TrackFocus::GetFocus()
{
    auto focusedTrack = mFocusedTrack.lock();
    if (!focusedTrack) {
        if (mNumFocusedTrack >= 1) {
            // This prevents the focus from being unnecessarily set to track 1
            // when effects are applied. (Applying an effect can change
            // the pointers of the selected tracks.)
            focusedTrack = FindTrack(mNumFocusedTrack);
        }
        if (!focusedTrack) {
            focusedTrack
                =Track::SharedPointer(*GetTracks().Any().first);
            // only call SetFocus if the focus has changed to avoid
            // unnecessary focus events
            if (focusedTrack) {
                focusedTrack = SetFocus();
            }
        }
    }

    if (!TrackNum(focusedTrack)) {
        mFocusedTrack.reset();
        return {};
    }

    return focusedTrack;
}

std::shared_ptr<Track> TrackFocus::PeekFocus() const
{
    return mFocusedTrack.lock();
}

// Changes focus to a specified track
std::shared_ptr<Track> TrackFocus::SetFocus(
    std::shared_ptr<Track> track, bool focusPanel)
{
    if (mpCallbacks) {
        mpCallbacks->BeginChangeFocus();
    }

    if (!track) {
        track = Track::SharedPointer(*GetTracks().begin());
    }

    const bool focusChanged = (PeekFocus() != track);
    if (focusChanged) {
        mFocusedTrack = track;
    }
    if (focusChanged || focusPanel) {
        BasicUI::CallAfter([
                               wFocus = weak_from_this(), focusPanel
                           ]{
            if (auto pFocus = wFocus.lock()) {
                pFocus->Publish({ focusPanel });
            }
        });
    }
    mNumFocusedTrack = TrackNum(track);

    if (mpCallbacks) {
        mpCallbacks->EndChangeFocus(track);
    }

    return track;
}

int TrackFocus::TrackNum(const std::shared_ptr<Track>& target) const
{
    // Find 1-based position of the target in the visible tracks, or 0 if not
    // found
    int ndx = 0;

    for (auto t : GetTracks()) {
        ndx++;
        if (t == target.get()) {
            return ndx;
        }
    }

    return 0;
}

std::shared_ptr<Track> TrackFocus::FindTrack(int num) const
{
    int ndx = 0;

    for (auto t : TrackList::Get(mProject)) {
        ndx++;
        if (ndx == num) {
            return t->SharedPointer();
        }
    }

    return {};
}

static const AudacityProject::AttachedObjects::RegisteredFactory key{
    [](AudacityProject& parent){
        return std::make_shared<TrackFocus>(parent);
    }
};

TrackFocus& TrackFocus::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get< TrackFocus >(key);
}

const TrackFocus& TrackFocus::Get(const AudacityProject& project)
{
    return Get(const_cast< AudacityProject& >(project));
}

TrackFocus::TrackFocus(AudacityProject& project)
    : mProject{project}
{
}

TrackFocus::~TrackFocus()
{
}

void TrackFocus::SetCallbacks(std::unique_ptr<TrackFocusCallbacks> pCallbacks)
{
    mpCallbacks = move(pCallbacks);
}

Track* TrackFocus::Get()
{
    return GetFocus().get();
}

void TrackFocus::Set(Track* pTrack, bool focusPanel)
{
    SetFocus(Track::SharedPointer(pTrack), focusPanel);
}

void TrackFocus::MessageForScreenReader(const TranslatableString& message)
{
    if (mpCallbacks) {
        mpCallbacks->MessageForScreenReader(message);
    }
}

void TrackFocus::UpdateAccessibility()
{
    if (mpCallbacks) {
        mpCallbacks->UpdateAccessibility();
    }
}
