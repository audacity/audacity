/**********************************************************************

  Audacity: A Digital Audio Editor

  ODDecodeFlacTask.h

  Created by Michael Chinen (mchinen) on 8/11/08.
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODDecodeFlacTask
\brief Decodes a flac file into a oddecodeBlockFile, but not immediately.

This is an abstract class that subclasses will have to derive the types
from.  For any type there should only be one ODDecodeTask associated with
a given track.
There could be the ODBlockFiles of several FLACs in one track (after copy and pasting),
so things aren't as simple as they seem - the implementation needs to be
robust enough to allow all the user changes such as copy/paste, delete, and so on.

*//*******************************************************************/




#ifndef __AUDACITY_ODDecodeFLACTask__
#define __AUDACITY_ODDecodeFLACTask__

#include <vector>
#include "ODDecodeTask.h"
#include "ODTaskThread.h"

#include "FLAC++/decoder.h"

class ODDecodeBlockFile;
class WaveTrack;
class ODFileDecoder;

class ODFlacDecoder;
class ODFLACFile;

/// A class representing a modular task to be used with the On-Demand structures.
class ODDecodeFlacTask:public ODDecodeTask
{
 public:

   /// Constructs an ODTask
   ODDecodeFlacTask(){}
   virtual ~ODDecodeFlacTask();


   virtual ODTask* Clone();
   ///Creates an ODFileDecoder that decodes a file of filetype the subclass handles.
   virtual ODFileDecoder* CreateFileDecoder(const wxString & fileName);

   ///Lets other classes know that this class handles flac
   ///Subclasses should override to return respective type.
   virtual unsigned int GetODType(){return eODFLAC;}
};


class ODFLACFile : public FLAC::Decoder::File
{
 public:
   ODFLACFile(ODFlacDecoder *decoder) : mDecoder(decoder)
   {
      mWasError = false;
      set_metadata_ignore_all();
      set_metadata_respond(FLAC__METADATA_TYPE_VORBIS_COMMENT);
      set_metadata_respond(FLAC__METADATA_TYPE_STREAMINFO);
   }

   bool get_was_error() const
   {
      return mWasError;
   }
 private:
   friend class ODFlacDecoder;
   ODFlacDecoder *mDecoder;
   bool                  mWasError;
   wxArrayString         mComments;

 protected:
   virtual FLAC__StreamDecoderWriteStatus write_callback(const FLAC__Frame *frame,
                                                         const FLAC__int32 * const buffer[]);
   virtual void metadata_callback(const FLAC__StreamMetadata *metadata);
   virtual void error_callback(FLAC__StreamDecoderErrorStatus status);
};


///class to decode a particular file (one per file).  Saves info such as filename and length (after the header is read.)
class ODFlacDecoder:public ODFileDecoder
{
   friend class ODFLACFile;
public:
   ///This should handle unicode converted to UTF-8 on mac/linux, but OD TODO:check on windows
   ODFlacDecoder(const wxString & fileName):ODFileDecoder(fileName),mSamplesDone(0){mFile=NULL;}
   virtual ~ODFlacDecoder();

   ///Decodes the samples for this blockfile from the real file into a float buffer.
   ///This is file specific, so subclasses must implement this only.
   ///the buffer was defined like
   ///samplePtr sampleData = NewSamples(mLen, floatSample);
   ///this->ReadData(sampleData, floatSample, 0, mLen);
   ///This class should call ReadHeader() first, so it knows the length, and can prepare
   ///the file object if it needs to.
   virtual int Decode(samplePtr & data, sampleFormat & format, sampleCount start, sampleCount len, unsigned int channel);


   ///Read header.  Subclasses must override.  Probably should save the info somewhere.
   ///Ideally called once per decoding of a file.  This complicates the task because
   virtual bool ReadHeader();

   ///FLAC specific file (inherited from FLAC::Decoder::File)
   ODFLACFile* GetFlacFile();

private:
   friend class FLACImportFileHandle;
   sampleFormat          mFormat;
   ODFLACFile           *mFile;
   ODLock         mFlacFileLock;//for mFile;
   wxFFile               mHandle;
   unsigned long         mSampleRate;
   unsigned long         mNumChannels;
   unsigned long         mBitsPerSample;
   FLAC__uint64          mNumSamples;
   FLAC__uint64          mSamplesDone;
   sampleCount          mLastDecodeStartSample;
   bool                  mStreamInfoDone;
   int                   mUpdateResult;
   WaveTrack           **mChannels;
   unsigned int          mTargetChannel;
   unsigned int         mDecodeBufferWritePosition;
   unsigned int         mDecodeBufferLen;
   samplePtr            mDecodeBuffer;
};

#endif



