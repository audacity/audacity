#include "WaveClipList.h"

#include "WaveClip.h"

void WaveClipList::PushBack(WaveClipHolder clip)
{
   if (mProjectTempo.has_value())
   {
      clip->OnProjectTempoChange(*mProjectTempo, *mProjectTempo);
   }
   mClips.push_back(std::move(clip));
}

void WaveClipList::Erase(const WaveClipHolders::const_iterator& it)
{
   mClips.erase(it);
}

bool WaveClipList::Empty() const
{
   return mClips.empty();
}

WaveClipHolder WaveClipList::Back() const
{
   return mClips.back();
}

WaveClipHolders::const_iterator WaveClipList::Begin() const
{
   return mClips.begin();
}

WaveClipHolders::const_iterator WaveClipList::End() const
{
   return mClips.end();
}

size_t WaveClipList::Size() const
{
   return mClips.size();
}

const WaveClipHolders& WaveClipList::Get() const
{
   return mClips;
}

WaveClipHolder WaveClipList::operator[](size_t i) const
{
   return mClips[i];
}

void WaveClipList::OnProjectTempoChange(double oldTempo, double newTempo)
{
   for (const auto& clip : mClips)
   {
      clip->OnProjectTempoChange(oldTempo, newTempo);
   }
   mProjectTempo = newTempo;
}
