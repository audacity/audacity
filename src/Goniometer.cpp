#include "Goniometer.h"

#include "AColor.h"
#include "Track.h"
#include "TrackPanelDrawingContext.h"

#include <wx/dc.h>

namespace{
AttachedTrackObjects::RegisteredFactory sKey{
   []( Track &track ) {
      return std::make_shared<Goniometer>( track );
   }
};
}

Goniometer &Goniometer::Get( Track &track )
{
   return track.AttachedObjects::Get< Goniometer >( sKey );
}

Goniometer::Goniometer( Track &track )
   : mpTrack{ track.shared_from_this() }
{
}

Goniometer::~Goniometer() = default;

void Goniometer::Clear()
{
}

void Goniometer::Reset(double sampleRate, bool resetClipping)
{
}

void Goniometer::Update(unsigned numChannels,
   unsigned long numFrames, const float *sampleData, bool interleaved)
{
}

bool Goniometer::IsDisabled() const
{
   return false;
}

std::shared_ptr<Track> Goniometer::DoFindTrack()
{
   return mpTrack.lock();
}

std::vector<UIHandlePtr> Goniometer::HitTest
   (const TrackPanelMouseState &,
    const AudacityProject *)
{
   return {};
}

void Goniometer::Reparent( const std::shared_ptr<Track> &parent )
{
   mpTrack = parent;
}
