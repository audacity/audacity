#include "WaveClipList.h"

#include "WaveClip.h"

void WaveClipList::PushBack(WaveClipHolder clip)
{
   if (mProjectTempo.has_value())
      clip->SetProjectTempo(*mProjectTempo);
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

WaveClipHolders& WaveClipList::Get()
{
   return mClips;
}

const WaveClipHolders& WaveClipList::Get() const
{
   return mClips;
}

WaveClipHolder WaveClipList::operator[](size_t i) const
{
   return mClips[i];
}

void WaveClipList::SetProjectTempo(double newTempo)
{
   for (const auto& clip : mClips)
      clip->SetProjectTempo(newTempo);
   mProjectTempo = newTempo;
}
