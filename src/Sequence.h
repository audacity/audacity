/**********************************************************************

  Audacity: A Digital Audio Editor

  Sequence.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_SEQUENCE__
#define __AUDACITY_SEQUENCE__

#include <wx/string.h>
#include <wx/dynarray.h>

#include "SampleFormat.h"
#include "xml/XMLTagHandler.h"
#include "xml/XMLWriter.h"
#include "ondemand/ODTaskThread.h"

typedef wxLongLong_t sampleCount; /** < A native 64-bit integer type, because
                                    32-bit integers may not be enough */

class BlockFile;
class DirManager;

// This is an internal data structure!  For advanced use only.
class SeqBlock {
 public:
   BlockFile * f;
   ///the sample in the global wavetrack that this block starts at.
   sampleCount start;
};
WX_DEFINE_ARRAY(SeqBlock *, BlockArray);

class Sequence: public XMLTagHandler {
 public:

   // Temporary: only until we delete TrackArtist once and for all
   friend class TrackArtist;

   //
   // Static methods
   //

   static void SetMaxDiskBlockSize(int bytes);
   static int GetMaxDiskBlockSize();

   //
   // Constructor / Destructor / Duplicator
   //

   Sequence(DirManager * projDirManager, sampleFormat format);

   // The copy constructor and duplicate operators take a
   // DirManager as a parameter, because you might be copying
   // from one project to another...
   Sequence(const Sequence &orig, DirManager *projDirManager);
   Sequence *Duplicate(DirManager *projDirManager) const {
      return new Sequence(*this, projDirManager);
   }

   virtual ~Sequence();

   //
   // Editing
   //

   sampleCount GetNumSamples() const { return mNumSamples; }

   bool Get(samplePtr buffer, sampleFormat format,
            sampleCount start, sampleCount len) const;
   bool Set(samplePtr buffer, sampleFormat format,
            sampleCount start, sampleCount len);

   bool GetWaveDisplay(float *min, float *max, float *rms,int* bl,
                       int len, sampleCount *where,
                       double samplesPerPixel);

   bool Copy(sampleCount s0, sampleCount s1, Sequence **dest);
   bool Paste(sampleCount s0, const Sequence *src);

   sampleCount GetIdealAppendLen();
   bool Append(samplePtr buffer, sampleFormat format, sampleCount len,
               XMLWriter* blockFileLog=NULL);
   bool Delete(sampleCount start, sampleCount len);
   bool AppendAlias(wxString fullPath,
                    sampleCount start,
                    sampleCount len, int channel,bool useOD);

   bool AppendCoded(wxString fName, sampleCount start,
                            sampleCount len, int channel, int decodeType);

   ///gets an int with OD flags so that we can determine which ODTasks should be run on this track after save/open, etc.
   unsigned int GetODFlags();

   // Append a blockfile. The blockfile pointer is then "owned" by the
   // sequence. This function is used by the recording log crash recovery
   // code, but may be useful for other purposes. The blockfile must already
   // be registered within the dir manager hash. This is the case
   // when the blockfile is created using DirManager::NewSimpleBlockFile or
   // loaded from an XML file via DirManager::HandleXMLTag
   void AppendBlockFile(BlockFile* blockFile);

   bool SetSilence(sampleCount s0, sampleCount len);
   bool InsertSilence(sampleCount s0, sampleCount len);

   DirManager* GetDirManager() { return mDirManager; }

   //
   // XMLTagHandler callback methods for loading and saving
   //

   virtual bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   virtual void HandleXMLEndTag(const wxChar *tag);
   virtual XMLTagHandler *HandleXMLChild(const wxChar *tag);
   virtual void WriteXML(XMLWriter &xmlFile);

   bool GetErrorOpening() { return mErrorOpening; }

   //
   // Lock/Unlock all of this sequence's BlockFiles, keeping them
   // from being moved.  Call this if you want to copy a
   // track to a different DirManager.  See BlockFile.h
   // for details.
   //

   bool Lock();
   bool CloseLock();//similar to Lock but should be called upon project close.
   bool Unlock();

   //
   // Manipulating Sample Format
   //

   sampleFormat GetSampleFormat() const;
   bool SetSampleFormat(sampleFormat format);
   bool ConvertToSampleFormat(sampleFormat format, bool* pbChanged);

   //
   // Retrieving summary info
   //

   bool GetMinMax(sampleCount start, sampleCount len,
                  float * min, float * max) const;
   bool GetRMS(sampleCount start, sampleCount len,
                  float * outRMS) const;

   //
   // Getting block size information
   //

   sampleCount GetBestBlockSize(sampleCount start) const;
   sampleCount GetMaxBlockSize() const;
   sampleCount GetIdealBlockSize() const;

   //
   // This should only be used if you really, really know what
   // you're doing!
   //

   BlockArray *GetBlockArray() {return mBlock;}

   ///
   void LockDeleteUpdateMutex(){mDeleteUpdateMutex.Lock();}
   void UnlockDeleteUpdateMutex(){mDeleteUpdateMutex.Unlock();}

 private:

   //
   // Private static variables
   //

   static int    sMaxDiskBlockSize;

   //
   // Private variables
   //

   DirManager   *mDirManager;

   BlockArray   *mBlock;
   sampleFormat  mSampleFormat;
   sampleCount   mNumSamples;

   sampleCount   mMinSamples; // min samples per block
   sampleCount   mMaxSamples; // max samples per block

   bool          mErrorOpening;

   ///To block the Delete() method against the ODCalcSummaryTask::Update() method
   ODLock   mDeleteUpdateMutex;

   //
   // Private methods
   //

   void CalcSummaryInfo();

   int FindBlock(sampleCount pos) const;
   int FindBlock(sampleCount pos, sampleCount lo,
                 sampleCount guess, sampleCount hi) const;

   bool AppendBlock(SeqBlock *b);

   bool Read(samplePtr buffer, sampleFormat format,
             SeqBlock * b,
             sampleCount start, sampleCount len) const;

   // These are the two ways to write data to a block
   bool FirstWrite(samplePtr buffer, SeqBlock * b, sampleCount len);
   bool CopyWrite(samplePtr buffer, SeqBlock * b,
                  sampleCount start, sampleCount len);

   // Both block-writing methods and AppendAlias call this
   // method to write the summary data
   void *GetSummary(samplePtr buffer, sampleCount len,
                    float *min, float *max, float *rms);

   BlockArray *Blockify(samplePtr buffer, sampleCount len);

 public:

   //
   // Public methods intended for debugging only
   //

   // This function makes sure that the track isn't messed up
   // because of inconsistent block starts & lengths
   bool ConsistencyCheck(const wxChar *whereStr);

   // This function prints information to stdout about the blocks in the
   // tracks and indicates if there are inconsistencies.
   void DebugPrintf(wxString *dest);
};

#endif // __AUDACITY_SEQUENCE__

