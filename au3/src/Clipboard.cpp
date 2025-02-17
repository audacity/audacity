/**********************************************************************

  Audacity: A Digital Audio Editor

  Clipboard.cpp

*//*******************************************************************/

#include "Clipboard.h"
#include "BasicUI.h"
#include "Track.h"

Clipboard::Clipboard()
    : mTracks{TrackList::Create(nullptr)}
{
}

Clipboard::~Clipboard() = default;

void Clipboard::Swap(Clipboard& other)
{
    std::swap(mTracks, other.mTracks);
    std::swap(mProject, other.mProject);
    std::swap(mT0, other.mT0);
    std::swap(mT1, other.mT1);
}

Clipboard& Clipboard::Get()
{
    static Clipboard instance;
    return instance;
}

//static
const TrackList& Clipboard::GetTracks() const
{
    return *mTracks;
}

void Clipboard::Clear()
{
    mT0 = 0.0;
    mT1 = 0.0;
    mProject.reset();
    mTracks->Clear();

    if (this == &Get()) {
        // Delayed message at idle time
        // Don't need to capture a weak pointer to the global object
        BasicUI::CallAfter([this]{ Publish({}); });
    }
}

void Clipboard::Assign(TrackList&& newContents,
                       double t0, double t1, const std::weak_ptr<AudacityProject>& pProject)
{
    assert(!newContents.GetOwner());
    newContents.Swap(*mTracks);
    newContents.Clear();

    mT0 = t0;
    mT1 = t1;
    mProject = pProject;

    if (this == &Get()) {
        // Delayed message at idle time
        // Don't need to capture a weak pointer to the global object
        BasicUI::CallAfter([this]{ Publish({}); });
    }
}

Clipboard::Scope::Scope()
{
    Get().Swap(mTempClipboard);
}

Clipboard::Scope::~Scope()
{
    Get().Swap(mTempClipboard);
}
