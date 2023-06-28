#pragma once

#include <memory>
#include <optional>
#include <vector>

class WaveClip;

using WaveClipHolder = std::shared_ptr<WaveClip>;
using WaveClipHolders = std::vector<WaveClipHolder>;

//! Secures control of operations on wave clips as they become part of a track,
//! e.g. project tempo synchronization.
class WaveClipList
{
public:
   void SetProjectTempo(double newTempo);
   bool Empty() const;
   size_t Size() const;

   // Accessors
   WaveClipHolders& Get();
   const WaveClipHolders& Get() const;
   WaveClipHolder Back() const;
   WaveClipHolder operator[](size_t i) const;
   WaveClipHolders::const_iterator Begin() const;
   WaveClipHolders::const_iterator End() const;

   // Modifiers
   void PushBack(WaveClipHolder clip);
   void Erase(const WaveClipHolders::const_iterator&);

private:
   std::optional<double> mProjectTempo;
   WaveClipHolders mClips;
};
