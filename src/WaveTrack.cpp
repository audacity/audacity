/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrack.cpp

  Dominic Mazzoni

*******************************************************************//**

\class WaveTrack
\brief A Track that contains audio waveform data.

*//****************************************************************//**

\class WaveTrack::Location
\brief Used only by WaveTrack, a special way to hold location that
can accommodate merged regions.

*//****************************************************************//**

\class TrackFactory
\brief Used to create a WaveTrack, or a LabelTrack..  Implementation 
of the functions of this class are dispersed through the different 
Track classes.

*//*******************************************************************/


#include <wx/defs.h>
#include <wx/intl.h>
#include <wx/debug.h>

#include <float.h>
#include <math.h>
#include <algorithm>

#include "float_cast.h"

#include "WaveTrack.h"
#include "LabelTrack.h"

#include "Envelope.h"
#include "Sequence.h"
#include "Spectrum.h"

#include "Project.h"
#include "Internat.h"

#include "AudioIO.h"
#include "Prefs.h"

#include "ondemand/ODManager.h"

#include "effects/TimeWarper.h"

using std::max;

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
bool WaveTrack::mMonoAsVirtualStereo;
#endif

WaveTrack* TrackFactory::DuplicateWaveTrack(WaveTrack &orig)
{
   return (WaveTrack*)(orig.Duplicate());
}


WaveTrack *TrackFactory::NewWaveTrack(sampleFormat format, double rate)
{
   return new WaveTrack(mDirManager, format, rate);
}

WaveTrack::WaveTrack(DirManager *projDirManager, sampleFormat format, double rate):
   Track(projDirManager)
{
   if (format == (sampleFormat)0) 
   {
      format = GetActiveProject()->GetDefaultFormat();
   }
   if (rate == 0) 
   {
      rate = GetActiveProject()->GetRate();
   }

   gPrefs->Read(wxT("/GUI/DefaultViewMode"), &mDisplay, 0);

   mLegacyProjectFileOffset = 0;

   mFormat = format;
   mRate = (int) rate;
   mGain = 1.0;
   mPan = 0.0;
   SetDefaultName(_("Audio Track"));
   SetName(GetDefaultName());
   mDisplayMin = -1.0;
   mDisplayMax = 1.0;
   mDisplayNumLocations = 0;
   mDisplayLocations = NULL;
   mDisplayNumLocationsAllocated = 0;
}

WaveTrack::WaveTrack(WaveTrack &orig):
   Track(orig)
{
   gPrefs->Read(wxT("/GUI/DefaultViewMode"), &mDisplay, 0);
   mLastDisplay=-1;

   mLegacyProjectFileOffset = 0;

   Init(orig);

   for (WaveClipList::compatibility_iterator node = orig.mClips.GetFirst(); node; node = node->GetNext())
      mClips.Append(new WaveClip(*node->GetData(), mDirManager));
}

// Copy the track metadata but not the contents.
void WaveTrack::Init(const WaveTrack &orig)
{
   Track::Init(orig);
   mFormat = orig.mFormat;
   mRate = orig.mRate;
   mGain = orig.mGain;
   mPan = orig.mPan;
   SetDefaultName(orig.GetDefaultName());
   SetName(orig.GetName());
   mDisplay = orig.mDisplay;
   mDisplayMin = orig.mDisplayMin;
   mDisplayMax = orig.mDisplayMax;
   mDisplayNumLocations = 0;
   mDisplayLocations = NULL;
   mDisplayNumLocationsAllocated = 0;
}

void WaveTrack::Merge(const Track &orig)
{
   if (orig.GetKind() == Wave)
   {
      mDisplay = ((WaveTrack &)orig).mDisplay;
      mGain    = ((WaveTrack &)orig).mGain;
      mPan     = ((WaveTrack &)orig).mPan;
   }
   Track::Merge(orig);
}

WaveTrack::~WaveTrack()
{   
   //Let the ODManager know this WaveTrack is disappearing.  
   //Deschedules tasks associated with this track.  
   if(ODManager::IsInstanceCreated())
      ODManager::Instance()->RemoveWaveTrack(this);
   
   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
      delete it->GetData();
   mClips.Clear();
   if (mDisplayLocations)
      delete [] mDisplayLocations;
      
}

double WaveTrack::GetOffset()
{
   return GetStartTime();
}

void WaveTrack::SetOffset(double o)
{
   double delta = o - GetOffset();

   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip* clip = it->GetData();
      clip->SetOffset(clip->GetOffset() + delta);
   }

   mOffset = o;
}

void WaveTrack::GetDisplayBounds(float *min, float *max)
{
   *min = mDisplayMin;
   *max = mDisplayMax;
}

void WaveTrack::SetDisplayBounds(float min, float max)
{
   mDisplayMin = min;
   mDisplayMax = max;
}

Track *WaveTrack::Duplicate()
{
   return new WaveTrack(*this);
}

double WaveTrack::GetRate() const
{
   return mRate;
}

void WaveTrack::SetRate(double newRate)
{
   mRate = (int) newRate;
   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
      it->GetData()->SetRate((int) newRate);
}

float WaveTrack::GetGain() const
{
   return mGain;
}

void WaveTrack::SetGain(float newGain)
{
   mGain = newGain;
}

float WaveTrack::GetPan() const
{
   return mPan;
}

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
bool WaveTrack::SetPan(float newPan)
{
   float p=mPan;
   bool panZero=false;
   int temp;

   if (newPan > 1.0)
      mPan = 1.0;
   else if (newPan < -1.0)
      mPan = -1.0;
   else
      mPan = newPan;

   if(mDisplay == WaveTrack::WaveformDisplay && mChannel == Track::MonoChannel && (p == 0.0f && newPan != 0.0f || p != 0.0f && newPan == 0.0f) && mMonoAsVirtualStereo)
   {
      panZero=true;
      if(!mPan){
         mHeight = mHeight + mHeightv;
      }else{
         temp = mHeight;
         mHeight = temp*mPerY;
         mHeightv = temp - mHeight;      
      }
      ReorderList();
   }

   return panZero;
}

#else // EXPERIMENTAL_OUTPUT_DISPLAY
void WaveTrack::SetPan(float newPan)
{
   if (newPan > 1.0)
      mPan = 1.0;
   else if (newPan < -1.0)
      mPan = -1.0;
   else
      mPan = newPan;
}
#endif // EXPERIMENTAL_OUTPUT_DISPLAY

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
void WaveTrack::SetVirtualState(bool state, bool half)
{
   int temp;

   if(half)
      mPerY = 0.5;

   if(state){
      if(mPan){
         temp = mHeight;
         mHeight = temp*mPerY;
         mHeightv = temp - mHeight;
      }
      ReorderList();
   }else{
      if(mPan){
         mHeight = mHeight + mHeightv;
      }
   }
}

int WaveTrack::GetMinimizedHeight() const
{
   if (GetLink()) {
      return 20;
   }

   if(GetChannel() == MonoChannel && GetPan() != 0 && mMonoAsVirtualStereo &&  mDisplay == WaveformDisplay)
      return 20;
   else
      return 40;
}

void WaveTrack::VirtualStereoInit()
{
   int temp;

   if(mChannel == Track::MonoChannel && mPan != 0.0f && mMonoAsVirtualStereo){
      temp = mHeight;
      mHeight = temp*mPerY;
      mHeightv = temp - mHeight; 
      ReorderList(false);
   }
}
#endif

float WaveTrack::GetChannelGain(int channel)
{
   float left = 1.0;
   float right = 1.0;

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   if(mVirtualStereo)
      channel = 3;
#endif

   if (mPan < 0)
      right = (mPan + 1.0);
   else if (mPan > 0)
      left = 1.0 - mPan;

   if ((channel%2) == 0)
      return left*mGain;
   else
      return right*mGain;
}

bool WaveTrack::ConvertToSampleFormat(sampleFormat format)
{
   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
      it->GetData()->ConvertToSampleFormat(format);
   mFormat = format;

   return true;
}

bool WaveTrack::IsEmpty(double t0, double t1)
{
   WaveClipList::compatibility_iterator it;

   //printf("Searching for overlap in %.6f...%.6f\n", t0, t1);
   for (it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip *clip = it->GetData();

      if (!clip->BeforeClip(t1) && !clip->AfterClip(t0)) {
         //printf("Overlapping clip: %.6f...%.6f\n",
         //       clip->GetStartTime(),
         //       clip->GetEndTime());
         // We found a clip that overlaps this region
         return false;
      }
   }
   //printf("No overlap found\n");

   // Otherwise, no clips overlap this region
   return true;
}

bool WaveTrack::Cut(double t0, double t1, Track **dest)
{
   if (t1 < t0)
      return false;

   if (!Copy(t0, t1, dest))
      return false;

   return Clear(t0, t1);
}

bool WaveTrack::SplitCut(double t0, double t1, Track **dest)
{
   if (t1 < t0)
      return false;

   // SplitCut is the same as 'Copy', then 'SplitDelete'
   if (!Copy(t0, t1, dest))
      return false;
   return SplitDelete(t0, t1);
}

bool WaveTrack::CutAndAddCutLine(double t0, double t1, Track **dest)
{
   if (t1 < t0)
      return false;

   // Cut is the same as 'Copy', then 'Delete'
   if (!Copy(t0, t1, dest))
      return false;
   return ClearAndAddCutLine(t0, t1);
}



//Trim trims within a clip, rather than trimming everything.
//If a bound is outside a clip, it trims everything.
bool WaveTrack::Trim (double t0, double t1)
{
   bool inside0 = false;
   bool inside1 = false;
   //Keeps track of the offset of the first clip greater than
   // the left selection t0.
   double firstGreaterOffset = -1;

   WaveClipList::compatibility_iterator  it;
   for(it = GetClipIterator(); it; it = it->GetNext())
   {
      WaveClip * clip = it->GetData();

      //Find the first clip greater than the offset.
      //If we end up clipping the entire track, this is useful.
      if(firstGreaterOffset < 0 && 
            clip->GetStartTime() >= t0)
         firstGreaterOffset = clip->GetStartTime();

      if(t1 > clip->GetStartTime() && t1 < clip->GetEndTime())
      {
         if (!clip->Clear(t1,clip->GetEndTime()))
            return false;
         inside1 = true;
      }

      if(t0 > clip->GetStartTime() && t0 < clip->GetEndTime())
      {
         if (!clip->Clear(clip->GetStartTime(),t0))
            return false;
         clip->SetOffset(t0);
         inside0 = true;
      }
   }

   //if inside0 is false, then the left selector was between
   //clips, so delete everything to its left.
   if(false == inside1)
   {
      if (!Clear(t1,GetEndTime()))
         return false;
   }

   if(false == inside0)
   {
      if (!SplitDelete(0,t0))
         return false;
   }
   
   return true;
}




bool WaveTrack::Copy(double t0, double t1, Track **dest)
{
   *dest = NULL;

   if (t1 <= t0)
      return false;

   WaveTrack *newTrack = new WaveTrack(mDirManager);

   newTrack->Init(*this);

   WaveClipList::compatibility_iterator it;
   
   for (it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip *clip = it->GetData();

      if (t0 <= clip->GetStartTime() && t1 >= clip->GetEndTime())
      {
         // Whole clip is in copy region
         //printf("copy: clip %i is in copy region\n", (int)clip);
         
         WaveClip *newClip = new WaveClip(*clip, mDirManager);
         newClip->RemoveAllCutLines();
         newClip->Offset(-t0);
         newTrack->mClips.Append(newClip);
      } else
      if (t1 > clip->GetStartTime() && t0 < clip->GetEndTime())
      {
         // Clip is affected by command
         //printf("copy: clip %i is affected by command\n", (int)clip);
         
         WaveClip *newClip = new WaveClip(*clip, mDirManager);
         newClip->RemoveAllCutLines();
         double clip_t0 = t0;
         double clip_t1 = t1;
         if (clip_t0 < clip->GetStartTime())
            clip_t0 = clip->GetStartTime();
         if (clip_t1 > clip->GetEndTime())
            clip_t1 = clip->GetEndTime();

         //printf("copy: clip_t0=%f, clip_t1=%f\n", clip_t0, clip_t1);

         newClip->Offset(-t0);
         if (newClip->GetOffset() < 0)
            newClip->SetOffset(0);

         //printf("copy: clip offset is now %f\n", newClip->GetOffset());
            
         if (!newClip->CreateFromCopy(clip_t0, clip_t1, clip))
         {
            //printf("paste: CreateFromCopy(%f, %f, %i) returns false, quitting\n",
            //   clip_t0, clip_t1, (int)clip);
            // JKC: July 2007, previously we did 'return false' here which
            // could leave *dest undefined.
            // I think this is dealing with clips that don't have any sequence content
            // i.e. we don't copy cut lines and such - anyone like to explain more?
            delete newClip;
         }
         else
         {
            newTrack->mClips.Append(newClip);
         }
      }
   }
   
   // AWD, Oct 2009: If the selection ends in whitespace, create a placeholder
   // clip representing that whitespace
   if (newTrack->GetEndTime() + 1.0 / newTrack->GetRate() < t1 - t0)
   {
      WaveClip *placeholder = new WaveClip(mDirManager,
            newTrack->GetSampleFormat(), newTrack->GetRate());
      placeholder->SetIsPlaceholder(true);
      if ( ! placeholder->InsertSilence(
               0, (t1 - t0) - newTrack->GetEndTime()) )
      {
         delete placeholder;
      }
      else
      {
         placeholder->Offset(newTrack->GetEndTime());
         newTrack->mClips.Append(placeholder);
      }
   }

   *dest = newTrack;

   return true;
}

bool WaveTrack::Clear(double t0, double t1)
{
   return HandleClear(t0, t1, false, false);
}

bool WaveTrack::ClearAndAddCutLine(double t0, double t1)
{
   return HandleClear(t0, t1, true, false);
}

//
// ClearAndPaste() is a specialized version of HandleClear()
// followed by Paste() and is used mostly by effects that
// can't replace track data directly using Get()/Set().
//
// HandleClear() removes any cut/split lines lines with the
// cleared range, but, in most cases, effects want to preserve
// the existing cut/split lines, so they are saved before the
// HandleClear()/Paste() and restored after.
//
// If the pasted track overlaps two or more clips, then it will
// be pasted with visible split lines.  Normally, effects do not
// want these extra lines, so they may be merged out.
//
bool WaveTrack::ClearAndPaste(double t0, // Start of time to clear
                              double t1, // End of time to clear
                              Track *src, // What to paste
                              bool preserve, // Whether to reinsert splits/cuts
                              bool merge, // Whether to remove 'extra' splits
                              TimeWarper *effectWarper // How does time change
                              )
{
   WaveClipList::compatibility_iterator ic;
   WaveClipList::compatibility_iterator it;
   double dur = wxMin(t1 - t0, src->GetEndTime());
   wxArrayDouble splits;
   WaveClipArray cuts;
   WaveClip *clip;

   // If duration is 0, then it's just a plain paste
   if (dur == 0.0) {
      return Paste(t0, src);
   }

   // If provided time warper was NULL, use a default one that does nothing
   TimeWarper *warper = NULL;
   if (effectWarper != NULL) {
      warper = effectWarper;
   } else {
      warper = new IdentityTimeWarper();
   }

   // Align to a sample
   t0 = LongSamplesToTime(TimeToLongSamples(t0));
   t1 = LongSamplesToTime(TimeToLongSamples(t1));

   // Save the cut/split lines whether preserving or not since merging
   // needs to know if a clip boundary is being crossed since Paste()
   // will add split lines around the pasted clip if so.
   for (ic = GetClipIterator(); ic; ic = ic->GetNext()) {
      double st;

      clip = ic->GetData();

      // Remember clip boundaries as locations to split
      st = LongSamplesToTime(TimeToLongSamples(clip->GetStartTime()));
      if (st >= t0 && st <= t1 && splits.Index(st) == wxNOT_FOUND) {
         splits.Add(st);
      }

      st = LongSamplesToTime(TimeToLongSamples(clip->GetEndTime()));
      if (st >= t0 && st <= t1 && splits.Index(st) == wxNOT_FOUND) {
         splits.Add(st);
      }

      // Search for cut lines
      WaveClipList* cutlines = clip->GetCutLines();
      it = cutlines->GetFirst();
      while (it) {
         WaveClipList::compatibility_iterator in = it->GetNext();
         WaveClip *cut = it->GetData();
         double cs = LongSamplesToTime(TimeToLongSamples(clip->GetOffset() +
                                                         cut->GetOffset()));

         // Remember cut point
         if (cs >= t0 && cs <= t1) {
            // Remove cut point from this clips cutlines array, otherwise
            // it will not be deleted when HandleClear() is called.
            cutlines->DeleteNode(it);

            // Remember the absolute offset and add to our cuts array.
            cut->SetOffset(cs);
            cuts.Add(cut);
         }

         it = in;
      }
   }

   // Now, clear the selection
   if (HandleClear(t0, t1, false, false)) {

      // And paste in the new data
      if (Paste(t0, src)) {
         unsigned int i;

         // First, merge the new clip(s) in with the existing clips
         if (merge && splits.GetCount() > 0)
         {
            WaveClipArray clips;

            // Now t1 represents the absolute end of the pasted data.
            t1 = t0 + src->GetEndTime();

            // Get a sorted array of the clips
            FillSortedClipArray(clips);

            // Scan the sorted clips for the first clip whose start time
            // exceeds the pasted regions end time.
            for (i = 0; i < clips.GetCount(); i++) {
               clip = clips[i];
               
               // Merge this clip and the previous clip if the end time
               // falls within it and this isn't the first clip in the track.
               if (fabs(t1 - clip->GetStartTime()) < WAVETRACK_MERGE_POINT_TOLERANCE) {
                  if (i > 0) {
                     bool bResult = MergeClips(GetClipIndex(clips[i - 1]), GetClipIndex(clip));
                     wxASSERT(bResult); // TO DO: Actually handle this.
                  }
                  break;
               }
            }

            // Refill the array since clips have changed.
            FillSortedClipArray(clips);

            // Scan the sorted clips to look for the start of the pasted
            // region.
            for (i = 0; i < clips.GetCount(); i++) {
               clip = clips[i];

               // Merge this clip and the next clip if the start time
               // falls within it and this isn't the last clip in the track.
               if (fabs(t0 - clip->GetEndTime()) < WAVETRACK_MERGE_POINT_TOLERANCE) {
                  if (i < clips.GetCount() - 1) {
                     bool bResult = MergeClips(GetClipIndex(clip), GetClipIndex(clips[i + 1]));
                     wxASSERT(bResult); // TO DO: Actually handle this.
                  }
                  break;
               }
            }
         }

         // Restore cut/split lines
         if (preserve) {

            // Restore the split lines, transforming the position appropriately
            for (i = 0; i < splits.GetCount(); i++) {
               SplitAt(warper->Warp(splits[i]));
            }

            // Restore the saved cut lines, also transforming if time altered
            for (ic = GetClipIterator(); ic; ic = ic->GetNext()) {
               double st;
               double et;

               clip = ic->GetData();
               st = clip->GetStartTime();
               et = clip->GetEndTime();

               // Scan the cuts for any that live within this clip
               for (i = 0; i < cuts.GetCount(); i++) {
                  WaveClip *cut = cuts[i];
                  double cs = cut->GetOffset();

                  // Offset the cut from the start of the clip and add it to
                  // this clips cutlines.
                  if (cs >= st && cs <= et) {
                     cut->SetOffset(warper->Warp(cs) - st);
                     clip->GetCutLines()->Append(cut);
                     cuts.RemoveAt(i);
                     i--;
                  }
               }
            }
         }
      }
   }

   // If we created a default time warper, we need to delete it
   if (effectWarper == NULL)
      delete warper;

   return true;
}

bool WaveTrack::SplitDelete(double t0, double t1)
{
   bool addCutLines = false;
   bool split = true;
   return HandleClear(t0, t1, addCutLines, split);
}

WaveClip* WaveTrack::RemoveAndReturnClip(WaveClip* clip)
{
   WaveClipList::compatibility_iterator node = mClips.Find(clip);
   WaveClip* clipReturn = node->GetData();
   mClips.DeleteNode(node);
   return clipReturn;
}

void WaveTrack::AddClip(WaveClip* clip)
{
   // Uncomment the following line after we correct the problem of zero-length clips
   //if (CanInsertClip(clip))
      mClips.Append(clip);
}

bool WaveTrack::HandleClear(double t0, double t1,
                            bool addCutLines, bool split)
{
   if (t1 < t0)
      return false;

   bool editClipCanMove = true;
   gPrefs->Read(wxT("/GUI/EditClipCanMove"), &editClipCanMove);

   WaveClipList::compatibility_iterator it;
   WaveClipList clipsToDelete;
   WaveClipList clipsToAdd;
   
   // We only add cut lines when deleting in the middle of a single clip
   // The cut line code is not really prepared to handle other situations
   if (addCutLines)
   {
      for (it=GetClipIterator(); it; it=it->GetNext())
      {
         WaveClip *clip = it->GetData();
         
         if (!clip->BeforeClip(t1) && !clip->AfterClip(t0) &&
               (clip->BeforeClip(t0) || clip->AfterClip(t1)))
         {
            addCutLines = false;
            break;
         }
      }
   }   

   for (it=GetClipIterator(); it; it=it->GetNext())   // main loop through clips
   {
      WaveClip *clip = it->GetData();

      if (clip->BeforeClip(t0) && clip->AfterClip(t1))
      {
         // Whole clip must be deleted - remember this
         clipsToDelete.Append(clip);
      } else
      if (!clip->BeforeClip(t1) && !clip->AfterClip(t0))
      {
         // Clip data is affected by command
         if (addCutLines)
         {
            if (!clip->ClearAndAddCutLine(t0,t1))
               return false;
         } else
         {
            if (split) {
               // Three cases:

               if (clip->BeforeClip(t0)) {
                  // Delete from the left edge
                  clip->Clear(clip->GetStartTime(), t1);
                  clip->Offset(t1-clip->GetStartTime());
               } else
               if (clip->AfterClip(t1)) {
                  // Delete to right edge
                  clip->Clear(t0, clip->GetEndTime());
               } else
               {
                  // Delete in the middle of the clip...we actually create two
                  // new clips out of the left and right halves...

                  WaveClip *left = new WaveClip(*clip, mDirManager);
                  left->Clear(t0, clip->GetEndTime());
                  clipsToAdd.Append(left);

                  WaveClip *right = new WaveClip(*clip, mDirManager);
                  right->Clear(clip->GetStartTime(), t1);
                  right->Offset(t1-clip->GetStartTime());
                  clipsToAdd.Append(right);

                  clipsToDelete.Append(clip);
               }
            }
            else { // (We are not doing a split cut)
               /* We are going to delete part of the clip here. The clip may
                * have envelope points, and we need to ensure that the envelope
                * outside of the cleared region is not affected. This means
                * putting in "glue" points where the clip enters and leaves the
                * region being cleared. If one of the ends of the clip is inside
                * the region, then one of the glue points will be redundant. */
               // clip->Clear keeps points < t0 and >= t1 via Envelope::CollapseRegion
               if (clip->GetEnvelope()->GetNumberOfPoints() > 0) {   // don't insert env pts if none exist
                  double val;
                  if (clip->WithinClip(t0))
                     {  // start of region within clip
                     val = clip->GetEnvelope()->GetValue(t0);
                     clip->GetEnvelope()->Insert(t0 - clip->GetOffset() - 1.0/clip->GetRate(), val);
                     }
                  if (clip->WithinClip(t1))
                     {  // end of region within clip
                     val = clip->GetEnvelope()->GetValue(t1);
                     clip->GetEnvelope()->Insert(t1 - clip->GetOffset(), val);
                     }
               }
               if (!clip->Clear(t0,t1))
                  return false;
               clip->GetEnvelope()->RemoveUnneededPoints(t0);
            }
         }
      } else
      if (clip->BeforeClip(t1))
      {
         // Clip is "behind" the region -- offset it unless we're splitting
         // or we're using the "don't move other clips" mode
         if (!split && editClipCanMove)
            clip->Offset(-(t1-t0));
      }
   }

   for (it=clipsToDelete.GetFirst(); it; it=it->GetNext())
   {
      mClips.DeleteObject(it->GetData());
      delete it->GetData();
   }

   for (it=clipsToAdd.GetFirst(); it; it=it->GetNext())
   {
      mClips.Append(it->GetData());
   }

   return true;
}

bool WaveTrack::SyncLockAdjust(double oldT1, double newT1)
{
   if (newT1 > oldT1) {
      // Insert space within the track

      // JKC: This is a rare case where using >= rather than > on a float matters.
      // GetEndTime() looks through the clips and may give us EXACTLY the same
      // value as T1, when T1 was set to be at the end of one of those clips.
      if (oldT1 >= GetEndTime())
         return true;

      // If track is empty at oldT1 insert whitespace; otherwise, silence
      if (IsEmpty(oldT1, oldT1))
      {
         bool ret = false;

         // Check if clips can move
         bool clipsCanMove = true;
         gPrefs->Read(wxT("/GUI/EditClipCanMove"), &clipsCanMove);
         if (clipsCanMove) {
            Track *tmp = NULL;
            ret = Cut (oldT1, GetEndTime() + 1.0/GetRate(), &tmp);
            if (!ret) return false;

            ret = Paste(newT1, tmp);
            wxASSERT(ret);
            delete tmp;
         }

         return ret;
      }
      else {
         // AWD: Could just use InsertSilence() on its own here, but it doesn't
         // follow EditClipCanMove rules (Paste() does it right)
         AudacityProject *p = GetActiveProject();
         if (!p) return false;
         TrackFactory *f = p->GetTrackFactory();
         if (!f) return false;
         WaveTrack *tmp = f->NewWaveTrack(GetSampleFormat(), GetRate());

         bool bResult = tmp->InsertSilence(0.0, newT1 - oldT1);
         wxASSERT(bResult); // TO DO: Actually handle this.
         tmp->Flush();
         bResult = Paste(oldT1, tmp);
         wxASSERT(bResult); // TO DO: Actually handle this.
         delete tmp;
      }
   }
   else if (newT1 < oldT1) {
      return Clear(newT1, oldT1);
   }

   // fall-through: no change
   return true;
}

bool WaveTrack::Paste(double t0, Track *src)
{
   bool editClipCanMove = true;
   gPrefs->Read(wxT("/GUI/EditClipCanMove"), &editClipCanMove);

   if( src == NULL )
      return false;

   if (src->GetKind() != Track::Wave)
      return false;
      
   WaveTrack* other = (WaveTrack*)src;
   
   //
   // Pasting is a bit complicated, because with the existence of multiclip mode,
   // we must guess the behaviour the user wants.
   //
   // Currently, two modes are implemented:
   //
   // - If a single clip should be pasted, and it should be pasted inside another
   //   clip, no new clips are generated. The audio is simply inserted.
   //   This resembles the old (pre-multiclip support) behaviour. However, if
   //   the clip is pasted outside of any clip, a new clip is generated. This is
   //   the only behaviour which is different to what was done before, but it
   //   shouldn't confuse users too much.
   //
   // - If multiple clips should be pasted, or a single clip that does not fill
   // the duration of the pasted track, these are always pasted as single
   // clips, and the current clip is splitted, when necessary. This may seem
   // strange at first, but it probably is better than trying to auto-merge
   // anything. The user can still merge the clips by hand (which should be a
   // simple command reachable by a hotkey or single mouse click).
   //

   if (other->GetNumClips() == 0)
      return false;

   //printf("paste: we have at least one clip\n");
   
   bool singleClipMode = (other->GetNumClips() == 1 &&
         other->GetStartTime() == 0.0);

   double insertDuration = other->GetEndTime();
   WaveClipList::compatibility_iterator it;

   //printf("Check if we need to make room for the pasted data\n");
   
   // Make room for the pasted data
   if (editClipCanMove) {
      if (!singleClipMode) {
         // We need to insert multiple clips, so split the current clip and
         // move everything to the right, then try to paste again
         if (!IsEmpty(t0, GetEndTime())) {
            Track *tmp = NULL;
            Cut(t0, GetEndTime()+1.0/mRate, &tmp);
            bool bResult = Paste(t0 + insertDuration, tmp);
            wxASSERT(bResult); // TO DO: Actually handle this.
            delete tmp;
         }
      } else
      {
         // We only need to insert one single clip, so just move all clips
         // to the right of the paste point out of the way
         for (it=GetClipIterator(); it; it=it->GetNext())
         {
            WaveClip* clip = it->GetData();
            if (clip->GetStartTime() > t0-(1.0/mRate))
               clip->Offset(insertDuration);
         }
      }
   }

   if (singleClipMode)
   {
      // Single clip mode
      // printf("paste: checking for single clip mode!\n");
      
      WaveClip *insideClip = NULL;

      for (it=GetClipIterator(); it; it=it->GetNext())
      {
         WaveClip *clip = it->GetData();

         if (editClipCanMove)
         {
            if (clip->WithinClip(t0))
            {
               //printf("t0=%.6f: inside clip is %.6f ... %.6f\n",
               //       t0, clip->GetStartTime(), clip->GetEndTime());
               insideClip = clip;
               break;
            }
         } else
         {
            // If clips are immovable we also allow prepending to clips
            if (clip->WithinClip(t0) ||
                  TimeToLongSamples(t0) == clip->GetStartSample())
            {
               insideClip = clip;
               break;
            }
         }
      }

      if (insideClip)
      {
         // Exhibit traditional behaviour
         //printf("paste: traditional behaviour\n");
         if (!editClipCanMove)
         {
            // We did not move other clips out of the way already, so
            // check if we can paste without having to move other clips
            for (it=GetClipIterator(); it; it=it->GetNext())
            {
               WaveClip *clip = it->GetData();
               
               if (clip->GetStartTime() > insideClip->GetStartTime() &&
                   insideClip->GetEndTime() + insertDuration >
                                                      clip->GetStartTime())
               {
                  wxMessageBox(
                     _("There is not enough room available to paste the selection"),
                     _("Error"), wxICON_STOP);
                  return false;
               }
            }
         }
         
         return insideClip->Paste(t0, other->GetClipByIndex(0));
      }

      // Just fall through and exhibit new behaviour
   }

   // Insert new clips
   //printf("paste: multi clip mode!\n");

   if (!editClipCanMove && !IsEmpty(t0, t0+insertDuration-1.0/mRate))
   {
      wxMessageBox(
         _("There is not enough room available to paste the selection"),
         _("Error"), wxICON_STOP);
      return false;
   }

   for (it=other->GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip* clip = it->GetData();

      // AWD Oct. 2009: Don't actually paste in placeholder clips
      if (!clip->GetIsPlaceholder())
      {
         WaveClip* newClip = new WaveClip(*clip, mDirManager);
         newClip->Resample(mRate);
         newClip->Offset(t0);
         newClip->MarkChanged();
         mClips.Append(newClip);
      }
   }
   return true;
}

bool WaveTrack::Silence(double t0, double t1)
{
   if (t1 < t0)
      return false;

   sampleCount start = (sampleCount)floor(t0 * mRate + 0.5);
   sampleCount len = (sampleCount)floor(t1 * mRate + 0.5) - start;
   bool result = true;

   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip *clip = it->GetData();

      sampleCount clipStart = clip->GetStartSample();
      sampleCount clipEnd = clip->GetEndSample();

      if (clipEnd > start && clipStart < start+len)
      {
         // Clip sample region and Get/Put sample region overlap
         sampleCount samplesToCopy = start+len - clipStart;
         if (samplesToCopy > clip->GetNumSamples())
            samplesToCopy = clip->GetNumSamples();
         sampleCount inclipDelta = 0;
         sampleCount startDelta = clipStart - start;
         if (startDelta < 0)
         {
            inclipDelta = -startDelta; // make positive value
            samplesToCopy -= inclipDelta;
            startDelta = 0;
         }

         if (!clip->GetSequence()->SetSilence(inclipDelta, samplesToCopy))
         {
            wxASSERT(false); // should always work
            return false;
         }
         clip->MarkChanged();
      }
   }

   return result;
}

bool WaveTrack::InsertSilence(double t, double len)
{
   if (len <= 0)
      return false;

   if (mClips.IsEmpty())
   {
      // Special case if there is no clip yet
      WaveClip* clip = CreateClip();
      return clip->InsertSilence(0, len);
   }

   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip *clip = it->GetData();
      if (clip->BeforeClip(t))
         clip->Offset(len);
      else if (clip->WithinClip(t))
      {
         if (!clip->InsertSilence(t, len)) {
            return false;
         }
      }
   }

   return true;
}

//Performs the opposite of Join
//Analyses selected region for possible Joined clips and disjoins them
bool WaveTrack::Disjoin(double t0, double t1)
{
   sampleCount minSamples = TimeToLongSamples( WAVETRACK_MERGE_POINT_TOLERANCE );
   sampleCount maxAtOnce = 1048576;
   float *buffer = new float[ maxAtOnce ];
   Regions regions;
   
   wxBusyCursor busy;
   
   for( WaveClipList::compatibility_iterator it = GetClipIterator(); it; it = it->GetNext() )
   {
      WaveClip *clip = it->GetData();

      double startTime = clip->GetStartTime();
      double endTime = clip->GetEndTime();

      if( endTime < t0 || startTime > t1 )
         continue;

      if( t0 > startTime )
         startTime = t0;
      if( t1 < endTime )
         endTime = t1;
      
      //simply look for a sequence of zeroes and if the sequence
      //is greater than minimum number, split-delete the region
      
      sampleCount seqStart = -1;
      sampleCount start, end;
      clip->TimeToSamplesClip( startTime, &start );
      clip->TimeToSamplesClip( endTime, &end );
       
      sampleCount len = ( end - start );
      for( sampleCount done = 0; done < len; done += maxAtOnce )
      {
         sampleCount numSamples = maxAtOnce;
         if( done + maxAtOnce > len )
            numSamples = len - done;
           
         clip->GetSamples( ( samplePtr )buffer, floatSample, start + done, 
               numSamples );
         for( sampleCount i = 0; i < numSamples; i++ )
         {
            sampleCount curSamplePos = start + done + i;

            //start a new sequence
            if( buffer[ i ] == 0.0 && seqStart == -1 )
               seqStart = curSamplePos;
            else if( buffer[ i ] != 0.0 || curSamplePos == end - 1 )
            {
               if( seqStart != -1 )
               {
                  sampleCount seqEnd;
                  
                  //consider the end case, where selection ends in zeroes
                  if( curSamplePos == end - 1 && buffer[ i ] == 0.0 )
                     seqEnd = end;
                  else
                     seqEnd = curSamplePos;
                  if( seqEnd - seqStart + 1 > minSamples )
                  {
                     Region *region = new Region;
                     region->start = seqStart / GetRate() + clip->GetStartTime();
                     region->end = seqEnd / GetRate() + clip->GetStartTime();
                     regions.Add( region );
                  }
                  seqStart = -1;
               }
            }
         }
      }
   }

   for( unsigned int i = 0; i < regions.GetCount(); i++ )
   {
      SplitDelete( regions.Item( i )->start, regions.Item( i )->end );
      delete regions.Item( i );
   }
   
   delete[] buffer;
   return true;
}

bool WaveTrack::Join(double t0, double t1)
{
   // Merge all WaveClips overlapping selection into one

   WaveClipList::compatibility_iterator it;
   WaveClipList clipsToDelete;
   WaveClip *newClip;

   for (it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip *clip = it->GetData();   

      if (clip->GetStartTime() < t1-(1.0/mRate) &&
          clip->GetEndTime()-(1.0/mRate) > t0) {

         // Put in sorted order
         int i;
         for(i=0; i<(int)clipsToDelete.GetCount(); i++)
            if (clipsToDelete[i]->GetStartTime() > clip->GetStartTime())
               break;
         //printf("Insert clip %.6f at position %d\n", clip->GetStartTime(), i);
         clipsToDelete.Insert(i, clip);
      }
   }

   //if there are no clips to delete, nothing to do
   if( clipsToDelete.GetCount() == 0 )
      return true;
   
   newClip = CreateClip();
   double t = clipsToDelete[0]->GetOffset();
   newClip->SetOffset(t);
   for(it=clipsToDelete.GetFirst(); it; it=it->GetNext()) 
   {
      WaveClip *clip = it->GetData();

      //printf("t=%.6f adding clip (offset %.6f, %.6f ... %.6f)\n",
      //       t, clip->GetOffset(), clip->GetStartTime(), clip->GetEndTime());

      if (clip->GetOffset() - t > (1.0 / mRate)) {
         double addedSilence = (clip->GetOffset() - t);
         //printf("Adding %.6f seconds of silence\n");
         bool bResult = newClip->InsertSilence(t, addedSilence);
         wxASSERT(bResult); // TO DO: Actually handle this.
         t += addedSilence;
      }

      //printf("Pasting at %.6f\n", t);
      bool bResult = newClip->Paste(t, clip);
      wxASSERT(bResult); // TO DO: Actually handle this.
      t = newClip->GetEndTime();      

      mClips.DeleteObject(clip);
      delete clip;
   }

   return true;
}

bool WaveTrack::Append(samplePtr buffer, sampleFormat format,
                       sampleCount len, unsigned int stride /* = 1 */,
                       XMLWriter *blockFileLog /* = NULL */)
{
   return GetLastOrCreateClip()->Append(buffer, format, len, stride,
                                        blockFileLog);
}

bool WaveTrack::AppendAlias(wxString fName, sampleCount start,
                            sampleCount len, int channel,bool useOD)
{
   return GetLastOrCreateClip()->AppendAlias(fName, start, len, channel,useOD);
}


bool WaveTrack::AppendCoded(wxString fName, sampleCount start,
                            sampleCount len, int channel, int decodeType)
{
   return GetLastOrCreateClip()->AppendCoded(fName, start, len, channel, decodeType);
}

///gets an int with OD flags so that we can determine which ODTasks should be run on this track after save/open, etc.
unsigned int WaveTrack::GetODFlags()
{
   unsigned int ret = 0;
   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip* clip = it->GetData();
      ret = ret | clip->GetSequence()->GetODFlags();
   }
   return ret;
}


sampleCount WaveTrack::GetBestBlockSize(sampleCount s)
{
   sampleCount bestBlockSize = GetMaxBlockSize();

   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip* clip = it->GetData();
      sampleCount startSample = (sampleCount)floor(clip->GetStartTime()*mRate + 0.5);
      sampleCount endSample = startSample + clip->GetNumSamples();
      if (s >= startSample && s < endSample)
      {
         bestBlockSize = clip->GetSequence()->GetMaxBlockSize();
         break;
      }
   }

   return bestBlockSize;
}

sampleCount WaveTrack::GetMaxBlockSize()
{
   int maxblocksize = 0;
   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip* clip = it->GetData();
      if (clip->GetSequence()->GetMaxBlockSize() > maxblocksize)
         maxblocksize = clip->GetSequence()->GetMaxBlockSize();
   }

   if (maxblocksize == 0)
   {
      // We really need the maximum block size, so create a
      // temporary sequence to get it.
      Sequence *tempseq = new Sequence(mDirManager, mFormat);
      maxblocksize = tempseq->GetMaxBlockSize();
      delete tempseq;
   }

   wxASSERT(maxblocksize > 0);

   return maxblocksize;
}

sampleCount WaveTrack::GetIdealBlockSize()
{
   return GetLastOrCreateClip()->GetSequence()->GetIdealBlockSize();
}

bool WaveTrack::Flush()
{
   return GetLastOrCreateClip()->Flush();
}

bool WaveTrack::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   if (!wxStrcmp(tag, wxT("wavetrack"))) {
      double dblValue;
      long nValue;
      while(*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;
         
         if (!value)
            break;
         
         const wxString strValue = value;
         if (!wxStrcmp(attr, wxT("rate")))
         {
            // mRate is an int, but "rate" in the project file is a float.
            if (!XMLValueChecker::IsGoodString(strValue) || 
                  !Internat::CompatibleToDouble(strValue, &dblValue) ||
                  (dblValue < 1.0) || (dblValue > 1000000.0)) // allow a large range to be read
               return false;
            mRate = lrint(dblValue);
         }
         else if (!wxStrcmp(attr, wxT("offset")) && 
                  XMLValueChecker::IsGoodString(strValue) && 
                  Internat::CompatibleToDouble(strValue, &dblValue))
         {
            // Offset is only relevant for legacy project files. The value
            // is cached until the actual WaveClip containing the legacy
            // track is created.
            mLegacyProjectFileOffset = dblValue;
         }
         else if (!wxStrcmp(attr, wxT("mute")) && 
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            mMute = (nValue != 0);
         else if (!wxStrcmp(attr, wxT("solo")) && 
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            mSolo = (nValue != 0);
         else if (!wxStrcmp(attr, wxT("height")) && 
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            mHeight = nValue;
         else if (!wxStrcmp(attr, wxT("minimized")) && 
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            mMinimized = (nValue != 0);
         else if (!wxStrcmp(attr, wxT("isSelected")) && 
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            this->SetSelected(nValue != 0);
         else if (!wxStrcmp(attr, wxT("gain")) && 
                  XMLValueChecker::IsGoodString(strValue) && 
                  Internat::CompatibleToDouble(strValue, &dblValue))
            mGain = dblValue;
         else if (!wxStrcmp(attr, wxT("pan")) && 
                  XMLValueChecker::IsGoodString(strValue) && 
                  Internat::CompatibleToDouble(strValue, &dblValue) && 
                  (dblValue >= -1.0) && (dblValue <= 1.0))
            mPan = dblValue;
         else if (!wxStrcmp(attr, wxT("name")) && XMLValueChecker::IsGoodString(strValue))
            mName = strValue;
         else if (!wxStrcmp(attr, wxT("channel")))
         {
            if (!XMLValueChecker::IsGoodInt(strValue) || !strValue.ToLong(&nValue) || 
                  !XMLValueChecker::IsValidChannel(nValue))
               return false;
            mChannel = nValue;
         }
         else if (!wxStrcmp(attr, wxT("linked")) && 
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            SetLinked(nValue != 0);
         
      } // while
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      VirtualStereoInit();
#endif
      return true;
   }

   return false;
}

void WaveTrack::HandleXMLEndTag(const wxChar * WXUNUSED(tag))
{
   // In case we opened a pre-multiclip project, we need to
   // simulate closing the waveclip tag.
   GetLastOrCreateClip()->HandleXMLEndTag( wxT("waveclip") );
}

XMLTagHandler *WaveTrack::HandleXMLChild(const wxChar *tag)
{
   //
   // This is legacy code (1.2 and previous) and is not called for new projects!
   //
   if (!wxStrcmp(tag, wxT("sequence")) || !wxStrcmp(tag, wxT("envelope")))
   {
      // This is a legacy project, so set the cached offset
      GetLastOrCreateClip()->SetOffset(mLegacyProjectFileOffset);
 
      // Legacy project file tracks are imported as one single wave clip
      if (!wxStrcmp(tag, wxT("sequence")))
         return GetLastOrCreateClip()->GetSequence();
      else if (!wxStrcmp(tag, wxT("envelope")))
         return GetLastOrCreateClip()->GetEnvelope();
   }
   
   // JKC... for 1.1.0, one step better than what we had, but still badly broken.
   //If we see a waveblock at this level, we'd better generate a sequence.
   if( !wxStrcmp( tag, wxT("waveblock" )))
   {
      // This is a legacy project, so set the cached offset
      GetLastOrCreateClip()->SetOffset(mLegacyProjectFileOffset);
      Sequence *pSeq = GetLastOrCreateClip()->GetSequence();
      return pSeq;
   }

   //
   // This is for the new file format (post-1.2)
   //
   if (!wxStrcmp(tag, wxT("waveclip")))
      return CreateClip();
   else
      return NULL;
}

void WaveTrack::WriteXML(XMLWriter &xmlFile)
{
   xmlFile.StartTag(wxT("wavetrack"));
   xmlFile.WriteAttr(wxT("name"), mName);
   xmlFile.WriteAttr(wxT("channel"), mChannel);
   xmlFile.WriteAttr(wxT("linked"), mLinked);
   xmlFile.WriteAttr(wxT("mute"), mMute);
   xmlFile.WriteAttr(wxT("solo"), mSolo);
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   int height;
   if(MONO_PAN)
      height = mHeight + mHeightv;
   else
      height = this->GetActualHeight();
   xmlFile.WriteAttr(wxT("height"), height);
#else
   xmlFile.WriteAttr(wxT("height"), this->GetActualHeight());
#endif
   xmlFile.WriteAttr(wxT("minimized"), this->GetMinimized());
   xmlFile.WriteAttr(wxT("isSelected"), this->GetSelected());
   xmlFile.WriteAttr(wxT("rate"), mRate);
   xmlFile.WriteAttr(wxT("gain"), (double)mGain);
   xmlFile.WriteAttr(wxT("pan"), (double)mPan);
   
   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
   {
      it->GetData()->WriteXML(xmlFile);
   }

   xmlFile.EndTag(wxT("wavetrack"));
}

bool WaveTrack::GetErrorOpening()
{
   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
      if (it->GetData()->GetSequence()->GetErrorOpening())
         return true;

   return false;
}

bool WaveTrack::Lock()
{
   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
      it->GetData()->Lock();

   return true;
}

bool WaveTrack::CloseLock()
{
   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
      it->GetData()->CloseLock();

   return true;
}


bool WaveTrack::Unlock()
{
   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
      it->GetData()->Unlock();

   return true;
}

AUDACITY_DLL_API sampleCount WaveTrack::TimeToLongSamples(double t0) const
{
   return (sampleCount)floor(t0 * mRate + 0.5);
}

double WaveTrack::LongSamplesToTime(sampleCount pos)
{
   return ((double)pos) / mRate;
}

double WaveTrack::GetStartTime()
{
   bool found = false;
   double best = 0.0;

   if (mClips.IsEmpty())
      return 0;

   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
      if (!found)
      {
         found = true;
         best = it->GetData()->GetStartTime();
      } else if (it->GetData()->GetStartTime() < best)
         best = it->GetData()->GetStartTime();

   return best;
}

double WaveTrack::GetEndTime()
{
   bool found = false;
   double best = 0.0;

   if (mClips.IsEmpty())
      return 0;

   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
      if (!found)
      {
         found = true;
         best = it->GetData()->GetEndTime();
      } else if (it->GetData()->GetEndTime() > best)
         best = it->GetData()->GetEndTime();

   return best;
}

//
// Getting/setting samples.  The sample counts here are
// expressed relative to t=0.0 at the track's sample rate.
//

bool WaveTrack::GetMinMax(float *min, float *max,
                          double t0, double t1)
{
   bool clipFound = false;

   *min = FLT_MAX;   // we need these at extremes to make sure we find true min and max
   *max = -FLT_MAX;

   if (t0 > t1)
      return false;

   if (t0 == t1)
      return true;

   bool result = true;

   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip* clip = it->GetData();

      if (t1 >= clip->GetStartTime() && t0 <= clip->GetEndTime())
      {
         clipFound = true;
         float clipmin, clipmax;
         if (it->GetData()->GetMinMax(&clipmin, &clipmax, t0, t1))
         {
            if (clipmin < *min)
               *min = clipmin;
            if (clipmax > *max)
               *max = clipmax;
         } else
         {
            result = false;
         }
      }
   }

   if(!clipFound)
   {
      *min = float(0.0);   // sensible defaults if no clips found
      *max = float(0.0);
   }

   return result;
}

bool WaveTrack::GetRMS(float *rms, double t0, double t1)
{
   if (t0 > t1)
      return false;

   if (t0 == t1)
      return true;

   bool result = true;
   double sumsq = 0.0;
   sampleCount length = 0;

   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip* clip = it->GetData();

      if (t1 >= clip->GetStartTime() && t0 <= clip->GetEndTime())
      {
         float cliprms;
         sampleCount clipStart, clipEnd;

         if (it->GetData()->GetRMS(&cliprms, t0, t1))
         {
            clip->TimeToSamplesClip(wxMax(t0, clip->GetStartTime()), &clipStart);
            clip->TimeToSamplesClip(wxMin(t1, clip->GetEndTime()), &clipEnd);
            sumsq += cliprms * cliprms * (clipEnd - clipStart);
            length += (clipEnd - clipStart);
         } else
         {
            result = false;
         }
      }
   }
   *rms = sqrt(sumsq/length);

   return result;
}

bool WaveTrack::Get(samplePtr buffer, sampleFormat format,
                    sampleCount start, sampleCount len, fillFormat fill )
{
   // Simple optimization: When this buffer is completely contained within one clip,
   // don't clear anything (because we won't have to). Otherwise, just clear
   // everything to be on the safe side.
   WaveClipList::compatibility_iterator it;
   
   bool doClear = true;
   for (it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip *clip = it->GetData();
      if (start >= clip->GetStartSample() && start+len <= clip->GetEndSample())
      {
         doClear = false;
         break;
      }
   }
   if (doClear)
   {
      // Usually we fill in empty sapce with zero
      if( fill == fillZero )
         ClearSamples(buffer, format, 0, len);
      // but we don't have to.
      else if( fill==fillTwo )
      {
         wxASSERT( format==floatSample );
         float * pBuffer = (float*)buffer;
         for(int i=0;i<len;i++)
            pBuffer[i]=2.0f;
      }
      else
      {
         wxFAIL_MSG(wxT("Invalid fill format"));
      }
   }

   for (it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip *clip = it->GetData();

      sampleCount clipStart = clip->GetStartSample();
      sampleCount clipEnd = clip->GetEndSample();

      if (clipEnd > start && clipStart < start+len)
      {
         // Clip sample region and Get/Put sample region overlap
         sampleCount samplesToCopy = start+len - clipStart;
         if (samplesToCopy > clip->GetNumSamples())
            samplesToCopy = clip->GetNumSamples();
         sampleCount inclipDelta = 0;
         sampleCount startDelta = clipStart - start;
         if (startDelta < 0)
         {
            inclipDelta = -startDelta; // make positive value
            samplesToCopy -= inclipDelta;
            startDelta = 0;
         }

         if (!clip->GetSamples((samplePtr)(((char*)buffer)+startDelta*SAMPLE_SIZE(format)),
                               format, inclipDelta, samplesToCopy))
         {
            wxASSERT(false); // should always work
            return false;
         }
      }
   }

   return true;
}

bool WaveTrack::Set(samplePtr buffer, sampleFormat format,
                    sampleCount start, sampleCount len)
{
   bool result = true;

   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip *clip = it->GetData();

      sampleCount clipStart = clip->GetStartSample();
      sampleCount clipEnd = clip->GetEndSample();

      if (clipEnd > start && clipStart < start+len)
      {
         // Clip sample region and Get/Put sample region overlap
         sampleCount samplesToCopy = start+len - clipStart;
         if (samplesToCopy > clip->GetNumSamples())
            samplesToCopy = clip->GetNumSamples();
         sampleCount inclipDelta = 0;
         sampleCount startDelta = clipStart - start;
         if (startDelta < 0)
         {
            inclipDelta = -startDelta; // make positive value
            samplesToCopy -= inclipDelta;
            startDelta = 0;
         }

         if (!clip->SetSamples((samplePtr)(((char*)buffer)+startDelta*SAMPLE_SIZE(format)),
                               format, inclipDelta, samplesToCopy))
         {
            wxASSERT(false); // should always work
            return false;
         }
         clip->MarkChanged();
      }
   }

   return result;
}

void WaveTrack::GetEnvelopeValues(double *buffer, int bufferLen,
                         double t0, double tstep)
{
   // Possibly nothing to do.
   if( bufferLen <= 0 )
      return;

   // This is useful in debugging, to easily find null envelope settings, but 
   // should not be necessary in Release build. 
   // If we were going to set it to failsafe values in Release build, better to set each element to 1.0.
   #ifdef __WXDEBUG__
      memset(buffer, 0, sizeof(double)*bufferLen);
   #endif

   double startTime = t0;
   double endTime = t0+tstep*bufferLen;

   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip *clip = it->GetData();
      
      // IF clip intersects startTime..endTime THEN...
      double dClipStartTime = clip->GetStartTime();
      double dClipEndTime = clip->GetEndTime();
      if ((dClipStartTime < endTime) && (dClipEndTime > startTime))
      {
         double* rbuf = buffer;
         int rlen = bufferLen;
         double rt0 = t0;

         if (rt0 < dClipStartTime)
         {
            sampleCount nDiff = (sampleCount)floor((dClipStartTime - rt0) * mRate + 0.5);
            rbuf += nDiff;
            rlen -= nDiff;
            rt0 = dClipStartTime;
         }

         if (rt0 + rlen*tstep > dClipEndTime)
         {
            //vvvvv debugging   int nStartSample = clip->GetStartSample();
            //vvvvv debugging   int nEndSample = clip->GetEndSample();
            int nClipLen = clip->GetEndSample() - clip->GetStartSample();

            if (nClipLen <= 0) // Testing for bug 641, this problem is consistently '== 0', but doesn't hurt to check <. 
               return; 

            // This check prevents problem cited in http://bugzilla.audacityteam.org/show_bug.cgi?id=528#c11, 
            // Gale's cross_fade_out project, which was already corrupted by bug 528.
            // This conditional prevents the previous write past the buffer end, in clip->GetEnvelope() call.
            if (nClipLen < rlen) // Never increase rlen here. 
               rlen = nClipLen;
         }
         clip->GetEnvelope()->GetValues(rbuf, rlen, rt0, tstep);
      }
   }
}

WaveClip* WaveTrack::GetClipAtX(int xcoord)
{
   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
   {
      wxRect r;
      it->GetData()->GetDisplayRect(&r);
      if (xcoord >= r.x && xcoord < r.x+r.width)
         return it->GetData();
   }

   return NULL;
}

WaveClip* WaveTrack::GetClipAtSample(sampleCount sample)
{
   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip *clip;
      sampleCount start, len;

      clip  = it->GetData();
      start = clip->GetStartSample();
      len   = clip->GetNumSamples();

      if (sample >= start && sample < start + len)
         return clip;
   }

   return NULL;
}

Envelope* WaveTrack::GetEnvelopeAtX(int xcoord)
{
   WaveClip* clip = GetClipAtX(xcoord);
   if (clip)
      return clip->GetEnvelope();
   else
      return NULL;
}

// Search for any active DragPoint on the current track
Envelope* WaveTrack::GetActiveEnvelope(void)
{
   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip* clip = it->GetData();
      Envelope* env = clip->GetEnvelope() ;
      if (env->GetDragPoint() >= 0)
         return env;
   }
   return NULL;
}

Sequence* WaveTrack::GetSequenceAtX(int xcoord)
{
   WaveClip* clip = GetClipAtX(xcoord);
   if (clip)
      return clip->GetSequence();
   else
      return NULL;
}

WaveClip* WaveTrack::CreateClip()
{
   WaveClip* clip = new WaveClip(mDirManager, mFormat, mRate);
   mClips.Append(clip);
   return clip;
}

WaveClip* WaveTrack::GetLastOrCreateClip()
{
   if (mClips.IsEmpty()) {
      WaveClip *clip = CreateClip();
      clip->SetOffset(mOffset);
      return clip;
   }
   else
      return mClips.GetLast()->GetData();
}

int WaveTrack::GetClipIndex(WaveClip* clip)
{
   return mClips.IndexOf(clip);
}

WaveClip* WaveTrack::GetClipByIndex(int index)
{
   if(index < (int)mClips.GetCount())
      return mClips.Item(index)->GetData();
   else
      return NULL;
}

int WaveTrack::GetNumClips() const
{
   return mClips.GetCount();
}

// unused
//void WaveTrack::MoveClipToTrack(int clipIndex, WaveTrack* dest)
//{
//   WaveClipList::compatibility_iterator node = mClips.Item(clipIndex);
//   WaveClip* clip = node->GetData();
//   mClips.DeleteNode(node);
//   dest->mClips.Append(clip);
//}

void WaveTrack::MoveClipToTrack(WaveClip *clip, WaveTrack* dest)
{
   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext()) {
      if (it->GetData() == clip) {
         WaveClip* clip = it->GetData(); //vvv ANSWER-ME: Why declare and assign this to another variable, when we just verified the 'clip' parameter is the right value?!
         mClips.DeleteNode(it);
         dest->mClips.Append(clip);
         return; // JKC iterator is now 'defunct' so better return straight away.
      }
   }
}

bool WaveTrack::CanOffsetClip(WaveClip* clip, double amount,
                              double *allowedAmount /* = NULL */)
{
   if (allowedAmount)
      *allowedAmount = amount;

   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip* c = it->GetData();
      if (c != clip && c->GetStartTime() < clip->GetEndTime()+amount &&
                       c->GetEndTime() > clip->GetStartTime()+amount)
      {
         if (!allowedAmount)
            return false; // clips overlap

         if (amount > 0)
         {
            if (c->GetStartTime()-clip->GetEndTime() < *allowedAmount)
               *allowedAmount = c->GetStartTime()-clip->GetEndTime();
            if (*allowedAmount < 0)
               *allowedAmount = 0;
         } else
         {
            if (c->GetEndTime()-clip->GetStartTime() > *allowedAmount)
               *allowedAmount = c->GetEndTime()-clip->GetStartTime();
            if (*allowedAmount > 0)
               *allowedAmount = 0;
         }
      }
   }

   if (allowedAmount)
   {
      if (*allowedAmount == amount)
         return true;

      // Check if the new calculated amount would not violate
      // any other constraint
      if (!CanOffsetClip(clip, *allowedAmount, NULL)) {
         *allowedAmount = 0; // play safe and don't allow anything
         return false;
      }
      else
         return true;
   } else
      return true;
}

bool WaveTrack::CanInsertClip(WaveClip* clip)
{
   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip* c = it->GetData();
      if (c->GetStartTime() < clip->GetEndTime() && c->GetEndTime() > clip->GetStartTime())
         return false; // clips overlap
   }

   return true;
}

bool WaveTrack::Split( double t0, double t1 )
{
   bool ret = SplitAt( t0 );
   if( ret && t0 != t1 )
      ret = SplitAt( t1 );
   return ret;
}

bool WaveTrack::SplitAt(double t)
{
   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip* c = it->GetData();

      if (c->WithinClip(t))
      {
         double val;
         t = LongSamplesToTime(TimeToLongSamples(t)); // put t on a sample
         val = c->GetEnvelope()->GetValue(t);
         //make two envelope points to preserve the value.  
         //handle the case where we split on the 1st sample (without this we hit an assert)
         if(t - 1.0/c->GetRate() >= c->GetOffset())
            c->GetEnvelope()->Insert(t - c->GetOffset() - 1.0/c->GetRate(), val);  // frame end points
         c->GetEnvelope()->Insert(t - c->GetOffset(), val);
         WaveClip* newClip = new WaveClip(*c, mDirManager);
         if (!c->Clear(t, c->GetEndTime()))
         {
            delete newClip;
            return false;
         }
         if (!newClip->Clear(c->GetStartTime(), t))
         {
            delete newClip;
            return false;
         }
         
         //offset the new clip by the splitpoint (noting that it is already offset to c->GetStartTime())
         sampleCount here = llrint(floor(((t - c->GetStartTime()) * mRate) + 0.5));
         newClip->Offset((double)here/(double)mRate);
         mClips.Append(newClip);
         return true;
      }
   }

   return true;
}

void WaveTrack::UpdateLocationsCache()
{
   unsigned int i;
   WaveClipArray clips;
   
   FillSortedClipArray(clips);
   
   mDisplayNumLocations = 0;
   
   // Count number of display locations
   for (i = 0; i < clips.GetCount(); i++)
   {
      WaveClip* clip = clips.Item(i);
      
      mDisplayNumLocations += clip->GetCutLines()->GetCount();

      if (i > 0 && fabs(clips.Item(i - 1)->GetEndTime() -
                  clip->GetStartTime()) < WAVETRACK_MERGE_POINT_TOLERANCE)
         mDisplayNumLocations++;
   }

   if (mDisplayNumLocations == 0)
      return;

   // Alloc necessary number of display locations
   if (mDisplayNumLocations > mDisplayNumLocationsAllocated)
   {
      // Only realloc, if we need more space than before. Otherwise
      // just use block from before.
      if (mDisplayLocations)
         delete[] mDisplayLocations;
      mDisplayLocations = new Location[mDisplayNumLocations];
      mDisplayNumLocationsAllocated = mDisplayNumLocations;
   }

   // Add all display locations to cache
   int curpos = 0;
   
   for (i = 0; i < clips.GetCount(); i++)
   {
      WaveClip* clip = clips.Item(i);

      WaveClipList* cutlines = clip->GetCutLines();
      for (WaveClipList::compatibility_iterator it = cutlines->GetFirst(); it;
           it = it->GetNext())
      {
         // Add cut line expander point
         mDisplayLocations[curpos].typ = locationCutLine;
         mDisplayLocations[curpos].pos =
            clip->GetOffset() + it->GetData()->GetOffset();
         curpos++;
      }
      
      if (i > 0)
      {
         WaveClip* previousClip = clips.Item(i - 1);

         if (fabs(previousClip->GetEndTime() - clip->GetStartTime())
                                          < WAVETRACK_MERGE_POINT_TOLERANCE)
         {
            // Add merge point
            mDisplayLocations[curpos].typ = locationMergePoint;
            mDisplayLocations[curpos].pos = clips.Item(i-1)->GetEndTime();
            mDisplayLocations[curpos].clipidx1 = mClips.IndexOf(previousClip);
            mDisplayLocations[curpos].clipidx2 = mClips.IndexOf(clip);
            curpos++;
         }
      }
   }
   
   wxASSERT(curpos == mDisplayNumLocations);
}

// Expand cut line (that is, re-insert audio, then delete audio saved in cut line)
bool WaveTrack::ExpandCutLine(double cutLinePosition, double* cutlineStart,
                              double* cutlineEnd)
{
   bool editClipCanMove = true;
   gPrefs->Read(wxT("/GUI/EditClipCanMove"), &editClipCanMove);

   // Find clip which contains this cut line   
   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip* clip = it->GetData();
      double start = 0, end = 0;

      if (clip->FindCutLine(cutLinePosition, &start, &end))
      {
         WaveClipList::compatibility_iterator it2;
         
         if (!editClipCanMove)
         {
            // We are not allowed to move the other clips, so see if there
            // is enough room to expand the cut line
            for (it2=GetClipIterator(); it2; it2=it2->GetNext())
            {
               WaveClip *clip2 = it2->GetData();
               
               if (clip2->GetStartTime() > clip->GetStartTime() &&
                   clip->GetEndTime() + end - start > clip2->GetStartTime())
               {
                  wxMessageBox(
                     _("There is not enough room available to expand the cut line"),
                     _("Error"), wxICON_STOP);
                  return false;
               }
            }
         }
         
         if (!clip->ExpandCutLine(cutLinePosition))
            return false;

         if (cutlineStart)
            *cutlineStart = start;
         if (cutlineEnd)
            *cutlineEnd = end;

         // Move clips which are to the right of the cut line
         if (editClipCanMove)
         {
            for (it2=GetClipIterator(); it2;
                 it2=it2->GetNext())
            {
               WaveClip* clip2 = it2->GetData();
   
               if (clip2->GetStartTime() > clip->GetStartTime())
                  clip2->Offset(end - start);
            }
         }

         return true;
      }
   }

   return false;
}

bool WaveTrack::RemoveCutLine(double cutLinePosition)
{
   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
      if (it->GetData()->RemoveCutLine(cutLinePosition))
         return true;

   return false;
}

bool WaveTrack::MergeClips(int clipidx1, int clipidx2)
{
   WaveClip* clip1 = GetClipByIndex(clipidx1);
   WaveClip* clip2 = GetClipByIndex(clipidx2);

   if (!clip2) // Could happen if one track of a linked pair had a split and the other didn't.
      return false;
   
   // Append data from second clip to first clip
   if (!clip1->Paste(clip1->GetEndTime(), clip2))
      return false;

   // Delete second clip
   mClips.DeleteObject(clip2);
   delete clip2;
   
   return true;
}

bool WaveTrack::Resample(int rate, ProgressDialog *progress)
{
   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
      if (!it->GetData()->Resample(rate, progress))
      {
         wxLogDebug( wxT("Resampling problem!  We're partially resampled") );
         // FIXME: The track is now in an inconsistent state since some
         //        clips are resampled and some are not
         return false;
      }
      
   mRate = rate;
   
   return true;
}

static int SortClipArrayCmpFunc(WaveClip** clip1, WaveClip** clip2)
{
   if((*clip1)->GetStartTime() < (*clip2)->GetStartTime())
      return -1;
   else
      return 1;
}

void WaveTrack::FillSortedClipArray(WaveClipArray& clips)
{
   clips.Empty();
   
   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
      clips.Add(it->GetData());
   
   clips.Sort(SortClipArrayCmpFunc);
}

///Deletes all clips' wavecaches.  Careful, This may not be threadsafe.
void WaveTrack::DeleteWaveCaches()
{
   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
      it->GetData()->DeleteWaveCache();
}

///Adds an invalid region to the wavecache so it redraws that portion only.
void WaveTrack::AddInvalidRegion(sampleCount startSample, sampleCount endSample)
{
   for (WaveClipList::compatibility_iterator it=GetClipIterator(); it; it=it->GetNext())
      it->GetData()->AddInvalidRegion(startSample,endSample);
}
