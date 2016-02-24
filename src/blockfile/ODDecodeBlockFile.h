/**********************************************************************

  Audacity: A Digital Audio Editor

  ODDecodeBlockFile.h

  Created by Michael Chinen (mchinen)
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODDecodeBlockFile
\brief ODDecodeBlockFile is a special type of SimpleBlockFile that does not necessarily have summary OR audio data available
The summary and audio is eventually computed and written to a file in a background thread.

Load On-Demand implementation of the SimpleBlockFIle for audio files that need to be decoded (mp3,flac,etc..).

Also, see ODPCMAliasBlockFile for a similar file.
*//*******************************************************************/






#ifndef __AUDACITY_ODDecodeBlockFile__
#define __AUDACITY_ODDecodeBlockFile__

#include "SimpleBlockFile.h"
#include "../BlockFile.h"
#include "../ondemand/ODTaskThread.h"
#include "../DirManager.h"
#include "../ondemand/ODDecodeTask.h"
#include <wx/thread.h>

/// An AliasBlockFile that references uncompressed data in an existing file
class ODDecodeBlockFile final : public SimpleBlockFile
{
 public:

   // Constructor / Destructor

   /// Create a disk file and write summary and sample data to it
   ODDecodeBlockFile(wxFileName baseFileName,wxFileName audioFileName, sampleCount aliasStart,
                     sampleCount aliasLen, int aliasChannel, unsigned int decodeType);
   /// Create the memory structure to refer to the given block file
   ODDecodeBlockFile(wxFileName existingFile, wxFileName audioFileName, sampleCount aliasStart,
                     sampleCount aliasLen, int aliasChannel, unsigned int decodeType,
                   float min, float max, float rms, bool dataAvailable);

   virtual ~ODDecodeBlockFile();
   //checks to see if summary data has been computed and written to disk yet.  Thread safe.  Blocks if we are writing summary data.
   bool IsSummaryAvailable() override;

   /// Returns TRUE if this block's complete data is ready to be accessed by Read()
   bool IsDataAvailable() override;

   /// Returns TRUE if the summary has not yet been written, but is actively being computed and written to disk
   bool IsSummaryBeingComputed() override { return false; }

   //Calls that rely on summary files need to be overidden
   wxLongLong GetSpaceUsage() override;
   /// Gets extreme values for the specified region
   void GetMinMax(sampleCount start, sampleCount len,
                          float *outMin, float *outMax, float *outRMS) override;
   /// Gets extreme values for the entire block
   void GetMinMax(float *outMin, float *outMax, float *outRMS) override;
   /// Returns the 256 byte summary data block
   bool Read256(float *buffer, sampleCount start, sampleCount len) override;
   /// Returns the 64K summary data block
   bool Read64K(float *buffer, sampleCount start, sampleCount len) override;

   /// returns true before decoding is complete, because it is linked to the encoded file until then.
   /// returns false afterwards.



   ///Makes NEW ODPCMAliasBlockFile or PCMAliasBlockFile depending on summary availability
   BlockFile *Copy(wxFileName fileName) override;

   ///Saves as xml ODPCMAliasBlockFile or PCMAliasBlockFile depending on summary availability
   void SaveXML(XMLWriter &xmlFile) override;

   ///Reconstructs from XML a ODPCMAliasBlockFile and reschedules it for OD loading
   static BlockFile *BuildFromXML(DirManager &dm, const wxChar **attrs);

   ///Writes the summary file if summary data is available
   void Recover(void) override;

   ///A public interface to WriteSummary
   int DoWriteBlockFile(){return WriteODDecodeBlockFile();}

   int WriteODDecodeBlockFile();

   ///Sets the value that indicates where the first sample in this block corresponds to the global sequence/clip.  Only for display use.
   void SetStart(sampleCount startSample){mStart = startSample;}

   ///Gets the value that indicates where the first sample in this block corresponds to the global sequence/clip.  Only for display use.
   sampleCount GetStart(){return mStart;}

   //returns the number of samples from the beginning of the track that this blockfile starts at
   sampleCount GetGlobalStart(){return mClipOffset+mStart;}

   //returns the number of samples from the beginning of the track that this blockfile ends at
   sampleCount GetGlobalEnd(){return mClipOffset+mStart+GetLength();}

   //Below calls are overrided just so we can take wxlog calls out, which are not threadsafe.

   /// Reads the specified data from the aliased file using libsndfile
   int ReadData(samplePtr data, sampleFormat format,
                        sampleCount start, sampleCount len) override;

   /// Read the summary into a buffer
   bool ReadSummary(void *data) override;

   ///Returns the type of audiofile this blockfile is loaded from.
   unsigned int GetDecodeType() /* not override */ { return mType; }
   // void SetDecodeType(unsigned int type) /* not override */ { mType = type; }

   ///sets the amount of samples the clip associated with this blockfile is offset in the wavetrack (non effecting)
   void SetClipOffset(sampleCount numSamples){mClipOffset= numSamples;}

   ///Gets the number of samples the clip associated with this blockfile is offset by.
   sampleCount GetClipOffset(){return mClipOffset;}

   //OD TODO:set ISAlias to true while we have no data?

   ///set the decoder,
   void SetODFileDecoder(ODFileDecoder* decoder);

   wxFileName GetAudioFileName(){return mAudioFileName;}

   ///sets the file name the summary info will be saved in.  threadsafe.
   void SetFileName(wxFileName &name) override;
   wxFileName GetFileName() override;

   /// Prevents a read on other threads of the encoded audio file.
   void LockRead() override;
   /// Allows reading of encoded file on other threads.
   void UnlockRead() override;

   ///// Get the name of the file where the audio data for this block is
   /// stored.
   wxFileName GetEncodedAudioFilename()
   {
      return mAudioFileName;
   }

   /// Modify this block to point at a different file.  This is generally
   /// looked down on, but it is necessary in one case: see
   /// DirManager::EnsureSafeFilename().
   void ChangeAudioFile(wxFileName newAudioFile);

  protected:

//   void WriteSimpleBlockFile() override;
   void *CalcSummary(samplePtr buffer, sampleCount len,
                             sampleFormat format) override;
   //The on demand type.
   unsigned int mType;

   ///This lock is for the filename (string) of the blockfile that contains summary/audio data
   ///after decoding
   ODLock mFileNameMutex;

   ///The original file the audio came from.
   wxFileName mAudioFileName;

   ODLock    mDataAvailableMutex;
   bool mDataAvailable;
   bool mDataBeingComputed;

   ODFileDecoder* mDecoder;
   ODLock mDecoderMutex;

   ///For accessing the audio file that will be decoded.  Used by dir manager;
   ODLock mReadDataMutex;

   ///for reporting after task is complete.  Only for display use.
   sampleCount mStart;

   ///the ODTask needs to know where this blockfile lies in the track, so for convenience, we have this here.
   sampleCount mClipOffset;

   sampleFormat mFormat;

   sampleCount mAliasStart;//where in the encoded audio file this block corresponds to.
   int         mAliasChannel;//The channel number in the encoded file..

};

#endif

