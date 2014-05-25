/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2012 Audacity Team.
   License: GPL v2.  See License.txt.

   ODDecodeFFmpegTask.cpp
   Michael Chinen

******************************************************************/

#include "../Audacity.h"	// needed before FFmpeg.h

#include <wx/wxprec.h>
// For compilers that support precompilation, includes "wx/wx.h".
#ifndef WX_PRECOMP
// Include your minimal set of headers here, or wx.h
#include <wx/window.h>
#endif

#include "../Experimental.h" 

#ifdef USE_FFMPEG
#ifdef EXPERIMENTAL_OD_FFMPEG

#include "../FFmpeg.h"		// which brings in avcodec.h, avformat.h
#include "../import/ImportFFmpeg.h"


extern FFmpegLibs *FFmpegLibsInst;
#include "ODDecodeFFmpegTask.h"


#define ODFFMPEG_SEEKING_TEST_UNKNOWN 0
#define ODFFMPEG_SEEKING_TEST_FAILED -1
#define ODFFMPEG_SEEKING_TEST_SUCCESS 1

#define kMaxSamplesInCache 4410000

//struct for caching the decoded samples to be used over multiple blockfiles
typedef struct _FFmpegDecodeCache
{
   uint8_t* samplePtr;//interleaved samples
   sampleCount start;
   sampleCount len;
   int         numChannels;
   AVSampleFormat samplefmt; // input (from libav) sample format

} FFMpegDecodeCache;


//------ ODFFmpegDecoder declaration and defs - here because we strip dependencies from .h files

///class to decode a particular file (one per file).  Saves info such as filename and length (after the header is read.)
class ODFFmpegDecoder:public ODFileDecoder
{


public:
   ///This should handle unicode converted to UTF-8 on mac/linux, but OD TODO:check on windows
   ODFFmpegDecoder(const wxString & fileName, streamContext** scs, int numStreams,WaveTrack*** channels, AVFormatContext* formatContext, int streamIndex);
   virtual ~ODFFmpegDecoder();
      
   ///Decodes the samples for this blockfile from the real file into a float buffer.  
   ///This is file specific, so subclasses must implement this only.
   ///the buffer was defined like
   ///samplePtr sampleData = NewSamples(mLen, floatSample);
   ///this->ReadData(sampleData, floatSample, 0, mLen);
   ///This class should call ReadHeader() first, so it knows the length, and can prepare 
   ///the file object if it needs to. 
   virtual int Decode(samplePtr & data, sampleFormat & format, sampleCount start, sampleCount len, unsigned int channel);

   ///This is a must implement abstract virtual in the superclass.  
   ///However it doesn't do anything because ImportFFMpeg does all that for us.
   virtual bool ReadHeader() {return true;}
   
   bool SeekingAllowed() ;
   
private:
   void InsertCache(FFMpegDecodeCache* cache);

   //puts the actual audio samples into the blockfile's data array
   int FillDataFromCache(samplePtr & data, sampleFormat outFormat, sampleCount & start, sampleCount& len, unsigned int channel);
         ///REFACTORABLE CODE FROM IMPORT FFMPEG
      ///! Reads next audio frame
   ///\return pointer to the stream context structure to which the frame belongs to or NULL on error, or 1 if stream is not to be imported.
   streamContext* ReadNextFrame();
   
   ///! Decodes the frame
   ///\param sc - stream context (from ReadNextFrame)
   ///\param flushing - true if flushing (no more frames left), false otherwise
   ///\return 0 on success, -1 if it can't decode any further
   int DecodeFrame(streamContext *sc, bool flushing);
   
   int                  mNumStreams;
   streamContext       **mScs;           //!< Array of pointers to stream contexts. Length is mNumStreams.
   WaveTrack***        mChannels;
   AVFormatContext      *mFormatContext; //!< Format description, also contains metadata and some useful info
   std::vector<FFMpegDecodeCache*> mDecodeCache;
   int                  mNumSamplesInCache;
   sampleCount                  mCurrentPos;     //the index of the next sample to be decoded
   sampleCount                  mCurrentLen;     //length of the last packet decoded
   
   bool                 mSeekingAllowedStatus;
   int                  mStreamIndex;
};


//------ ODDecodeFFmpegTask definitions 
ODDecodeFFmpegTask::ODDecodeFFmpegTask(void* scs,int numStreams, WaveTrack*** channels, void* formatContext, int streamIndex)
{
   mScs=scs;
   mNumStreams=numStreams;
   mChannels=channels;
   mFormatContext = formatContext;
   mStreamIndex = streamIndex;
   //TODO we probably need to create a new WaveTrack*** pointer and copy.
   //same for streamContext, but we should also use a ref counting system - this should be added to streamContext
 //  mScs = (streamContext**)malloc(sizeof(streamContext**)*mFormatContext->nb_streams);
}
ODDecodeFFmpegTask::~ODDecodeFFmpegTask()
{
}


ODTask* ODDecodeFFmpegTask::Clone()
{
   //we need to create copies of mScs.  It would be better to use a reference counter system.

   ODDecodeFFmpegTask* clone = new ODDecodeFFmpegTask((void*)mScs,mNumStreams,mChannels,mFormatContext,mStreamIndex);
   clone->mDemandSample=GetDemandSample();

   //the decoders and blockfiles should not be copied.  They are created as the task runs.
   return clone;
}

///Creates an ODFileDecoder that decodes a file of filetype the subclass handles.
//
//compare to FLACImportPlugin::Open(wxString filename)
ODFileDecoder* ODDecodeFFmpegTask::CreateFileDecoder(const wxString & fileName)
{
   // Open the file for import
   ODFFmpegDecoder *decoder = new ODFFmpegDecoder(fileName, (streamContext**) mScs,mNumStreams,mChannels,(AVFormatContext*)mFormatContext, mStreamIndex);

   mDecoders.push_back(decoder);
   return decoder;

}

/// subclasses need to override this if they cannot always seek.
/// seeking will be enabled once this is true.
bool ODFFmpegDecoder::SeekingAllowed() 
{
   return false;
   /*
   if(ODFFMPEG_SEEKING_TEST_UNKNOWN != mSeekingAllowedStatus)
      return mSeekingAllowedStatus == ODFFMPEG_SEEKING_TEST_SUCCESS;

   //we can seek if the following checks pass:
   //-sample rate is less than the reciprocal of the time_base of the seeking stream.  
   //-a seek test has been made and dts updates as expected.
   //we want to clone this to run a seek test.
   AVFormatContext* ic = (AVFormatContext*)mFormatContext;
   bool  audioStreamExists = false;
   AVStream* st;

   //test the audio stream(s)
   for (unsigned int i = 0; i < ic->nb_streams; i++)
   {
      if (ic->streams[i]->codec->codec_type == AVMEDIA_TYPE_AUDIO)
      {  
         audioStreamExists = true;                 
         st = ic->streams[i];
         if(st->duration <= 0 || st->codec->sample_rate <= 0)
            goto test_failed;
         
         //if the time base reciprocal is less than the sample rate it means we can't accurately represent a sample with the timestamp in av.  
         float time_base_inv = ((float)st->time_base.den/st->time_base.num);     
         if(time_base_inv < st->codec->sample_rate)
            goto test_failed;
         
      }
   }
   
   if(!audioStreamExists)
      goto test_failed;
   //TODO: now try a seek and see if dts/pts (decode/presentation timestamp) is updated as we expected it to be.  
   //This should be done using a new AVFormatContext clone so that we don't ruin the file pointer if we fail.
//   url_fseek(mFormatContext->pb,0,SEEK_SET); 

   
   AVFormatContext* tempContext;
   int err;
   err = ufile_fopen_input(&tempContext, mFName);
   if (err < 0)
   {
      goto test_failed;
   }

   err = av_find_stream_info(tempContext);
   if (err < 0)
   {
      goto test_failed;
   }

   if(av_seek_frame(tempContext, st->index, 0, 0) >= 0) {
      mSeekingAllowedStatus = ODFFMPEG_SEEKING_TEST_SUCCESS;
      if (tempContext) av_close_input_file(tempContext);
      return SeekingAllowed();
   }

   if (tempContext) av_close_input_file(tempContext);

test_failed:
   mSeekingAllowedStatus = ODFFMPEG_SEEKING_TEST_FAILED;
   return SeekingAllowed();
   */
}


//------ ODDecodeFFmpegFileDecoder
ODFFmpegDecoder::ODFFmpegDecoder(const wxString & fileName, streamContext** scs,int numStreams,WaveTrack*** channels, AVFormatContext* formatContext, int streamIndex)
:ODFileDecoder(fileName),
//mSamplesDone(0),
mNumStreams(numStreams),
mScs(scs),
mFormatContext(formatContext),
mNumSamplesInCache(0),
mCurrentLen(0),
mSeekingAllowedStatus(ODFFMPEG_SEEKING_TEST_UNKNOWN),
mStreamIndex(streamIndex)
{
   PickFFmpegLibs();   
   
   //do a shallow copy of the 2d array.
   mChannels = new WaveTrack **[mNumStreams];

   for (int s = 0; s < mNumStreams; s++)
   {
      mChannels[s] = new WaveTrack *[mScs[s]->m_stream->codec->channels];
      int c;
      for (c = 0; c < mScs[s]->m_stream->codec->channels; c++)
      {
         mChannels[s][c] = channels[s][c];
      }
   }
   // get the current stream start time.
   int64_t stream_delay = 0;
   if (mScs[streamIndex]->m_stream->start_time != int64_t(AV_NOPTS_VALUE) &&
       mScs[streamIndex]->m_stream->start_time > 0) {
      stream_delay = mScs[streamIndex]->m_stream->start_time;
   }
   mCurrentPos = double(stream_delay) / AV_TIME_BASE;
   
   //TODO: add a ref counter to scs?  This will be necessary if we want to allow copy and paste of not-yet decoded 
   //ODDecodeBlockFiles that point to FFmpeg files.
}

//we have taken ownership, so delete the ffmpeg stuff allocated in ImportFFmpeg that was given to us.
ODFFmpegDecoder::~ODFFmpegDecoder()
{
   if (FFmpegLibsInst->ValidLibsLoaded())
   {
      if (mFormatContext) av_close_input_file(mFormatContext);
      av_log_set_callback(av_log_default_callback);
   }

   for (int i = 0; i < mNumStreams; i++)
   {
      if (mScs[i]->m_decodedAudioSamples != NULL)
         av_free(mScs[i]->m_decodedAudioSamples);

      delete mScs[i];
   }
   free(mScs);
   
   //delete our caches.
   while(mDecodeCache.size())
   {
      free(mDecodeCache[0]->samplePtr);
      delete mDecodeCache[0];
      mDecodeCache.erase(mDecodeCache.begin()); 
   }
   
   //free the channel pointer arrays
   for (int s = 0; s < mNumStreams; s++)
   {
      delete[] mChannels[s];
   }
   delete[] mChannels;
   DropFFmpegLibs();
}

//we read the file from left to right, so in some cases it makes more sense not to seek and just carry on the decode if the gap is small enough.
//this value controls this amount.  this should be a value that is much larger than the payload for a single packet, and around block file size around 1-10 secs.
#define kDecodeSampleAllowance 400000
//number of jump backwards seeks
#define kMaxSeekRewindAttempts 8            
int ODFFmpegDecoder::Decode(samplePtr & data, sampleFormat & format, sampleCount start, sampleCount len, unsigned int channel)
{
   format = mScs[mStreamIndex]->m_osamplefmt;

   data = NewSamples(len, format);
   samplePtr bufStart = data;
   streamContext* sc = NULL;
   
   int nChannels;
   
   // printf("start %llu len %llu\n", start, len);   
   //TODO update this to work with seek - this only works linearly now.
   if(mCurrentPos > start && mCurrentPos  <= start+len + kDecodeSampleAllowance)
   {
      //this next call takes data, start and len as reference variables and updates them to reflect the new area that is needed.
      FillDataFromCache(bufStart, format, start,len,channel);
   }
   
   bool seeking = false;
   //look at the decoding timestamp and see if the next sample that will be decoded is not the next sample we need.
   if(len && (mCurrentPos > start + len  || mCurrentPos + kDecodeSampleAllowance < start ) && SeekingAllowed()) {
      sc = mScs[mStreamIndex];
      AVStream* st = sc->m_stream;
      int stindex = -1;
      uint64_t targetts;
      
      //printf("attempting seek to %llu\n", start);   
      //we have to find the index for this stream.
      for (unsigned int i = 0; i < mFormatContext->nb_streams; i++) {
         if (mFormatContext->streams[i] == sc->m_stream )
            stindex =i;
      }
      
      if(stindex >=0) {
         int numAttempts = 0;
         //reset mCurrentPos to a bogus value
         mCurrentPos = start+len +1;
         while(numAttempts++ < kMaxSeekRewindAttempts && mCurrentPos > start) {
            //we want to move slightly before the start of the block file, but not too far ahead
            targetts = (start-kDecodeSampleAllowance*numAttempts/kMaxSeekRewindAttempts)  * ((double)st->time_base.den/(st->time_base.num * st->codec->sample_rate ));
            if(targetts<0) 
               targetts=0;

            //printf("attempting seek to %llu, attempts %d\n", targetts, numAttempts);   
            if(av_seek_frame(mFormatContext,stindex,targetts,0) >= 0){
               //find out the dts we've seekd to.  
               sampleCount actualDecodeStart = 0.5 + st->codec->sample_rate * st->cur_dts  * ((double)st->time_base.num/st->time_base.den);      //this is mostly safe because den is usually 1 or low number but check for high values.
               
               mCurrentPos = actualDecodeStart;
               seeking = true;

               //if the seek was past our desired position, rewind a bit.  
               //printf("seek ok to %llu samps, float: %f\n",actualDecodeStart,actualDecodeStartDouble);
            } else {
               printf("seek failed");
               break;
            }
         }
         if(mCurrentPos>start){
            mSeekingAllowedStatus = (bool)ODFFMPEG_SEEKING_TEST_FAILED;
            //               url_fseek(mFormatContext->pb,sc->m_pkt.pos,SEEK_SET);                              
            printf("seek fail, reverting to previous pos\n");
            return -1;
         }
      }
   }
   bool firstpass = true;
   
   //we decode up to the end of the blockfile
   while (len>0 && (mCurrentPos < start+len) && (sc = ReadNextFrame()) != NULL)
   {
      // ReadNextFrame returns 1 if stream is not to be imported
      if (sc != (streamContext*)1)
      {
         nChannels = sc->m_stream->codec->channels < sc->m_initialchannels ? sc->m_stream->codec->channels : sc->m_initialchannels;
         //find out the dts we've seekd to.  can't use the stream->cur_dts because it is faulty.  also note that until we do the first seek, pkt.dts can be false and will change for the same samples after the initial seek.
         sampleCount actualDecodeStart = mCurrentPos;

         // we need adjacent samples, so don't use dts most of the time which will leave gaps between frames
         // for some formats
         // The only other case for inserting silence is for initial offset and ImportFFmpeg.cpp does this for us
         if (seeking) {
            actualDecodeStart = 0.52 + (sc->m_stream->codec->sample_rate * sc->m_pkt.dts 
                                        * ((double)sc->m_stream->time_base.num / sc->m_stream->time_base.den));
            //this is mostly safe because den is usually 1 or low number but check for high values.

            //hack to get rounding to work to neareset frame size since dts isn't exact
            if (sc->m_stream->codec->frame_size) {
               actualDecodeStart = ((actualDecodeStart + sc->m_stream->codec->frame_size/2) / sc->m_stream->codec->frame_size) * sc->m_stream->codec->frame_size;
            }
            // reset for the next one
            seeking = false;
         }
         if(actualDecodeStart != mCurrentPos)
            printf("ts not matching - now:%llu , last:%llu, lastlen:%llu, start %llu, len %llu\n",actualDecodeStart, mCurrentPos, mCurrentLen, start, len);
            //if we've skipped over some samples, fill the gap with silence.  This could happen often in the beginning of the file.
         if(actualDecodeStart>start && firstpass) {
            // find the number of samples for the leading silence
            int amt = actualDecodeStart - start;
            FFMpegDecodeCache* cache = new FFMpegDecodeCache;

            //printf("skipping/zeroing %i samples. - now:%llu (%f), last:%llu, lastlen:%llu, start %llu, len %llu\n",amt,actualDecodeStart, actualDecodeStartdouble, mCurrentPos, mCurrentLen, start, len);
                  
            //put it in the cache so the other channels can use it.
            cache->numChannels = sc->m_stream->codec->channels;                  
            cache->len = amt;
            cache->start=start;
            // 8 bit and 16 bit audio output from ffmpeg means
            // 16 bit int out.
            // 32 bit int, float, double mean float out.
            if (format == int16Sample)
               cache->samplefmt = SAMPLE_FMT_S16;
            else
               cache->samplefmt = SAMPLE_FMT_FLT;

            cache->samplePtr = (uint8_t*) malloc(amt * cache->numChannels * SAMPLE_SIZE(format));

            memset(cache->samplePtr, 0, amt * cache->numChannels * SAMPLE_SIZE(format));
                  
            InsertCache(cache);
         }
         firstpass=false;
         mCurrentPos = actualDecodeStart;
         //decode the entire packet (unused bits get saved in cache, so as long as cache size limit is bigger than the
         //largest packet size, we're ok.
         while (sc->m_pktRemainingSiz > 0)
            //Fill the cache with decoded samples
               if (DecodeFrame(sc,false) < 0)
                  break;

         // Cleanup after frame decoding
         if (sc->m_pktValid)
         {
            av_free_packet(&sc->m_pkt);
            sc->m_pktValid = 0;
         }    
      }
   }

   // Flush the decoders if we're done.
   if((!sc || sc == (streamContext*) 1)&& len>0)
   {
      for (int i = 0; i < mNumStreams; i++)
      {
         if (DecodeFrame(mScs[i], true) == 0)
         {
            if (mScs[i]->m_pktValid)
            {
               av_free_packet(&mScs[i]->m_pkt);
               mScs[i]->m_pktValid = 0;
            }				
         }
      }
   }
      
   //this next call takes data, start and len as reference variables and updates them to reflect the new area that is needed.
   FillDataFromCache(bufStart, format, start, len, channel);
   
   // CHECK: not sure if we need this.  In any case it has to be updated for the new float case (not just int16)
   //if for some reason we couldn't get the samples, fill them with silence
   /*
   int16_t* outBuf = (int16_t*) bufStart;
   for(int i=0;i<len;i++)
      outBuf[i]=0;
   */
   return 1;
}


//puts the actual audio samples into the blockfile's data array
// the minimum amount of cache entries necessary to warrant a binary search.
#define kODFFmpegSearchThreshold 10
///returns the number of samples filled in from start.
//also updates data and len to reflect new unfilled area - start is unmodified.
int ODFFmpegDecoder::FillDataFromCache(samplePtr & data, sampleFormat outFormat, sampleCount &start, sampleCount& len, unsigned int channel)
{
   if(mDecodeCache.size() <= 0)
      return 0;
   int samplesFilled=0;
   
   //do a search for the best position to start at.  
   //Guess that the array is evenly spaced from end to end - (dictionary sort)
   //assumes the array is sorted.  
   //all we need for this to work is a location in the cache array
   //that has a start time of less than our start sample, but try to get closer with binary search
   int searchStart = 0;
   int searchEnd = mDecodeCache.size();
   int guess;
   if(searchEnd>kODFFmpegSearchThreshold)
   {
      //first just guess that the cache is contiguous and we can just use math to figure it out like a dictionary
      //by guessing where our hit will be.
      while(searchStart+1<searchEnd)
      {
         guess = (searchStart+searchEnd)/2;//find a midpoint. //searchStart+ (searchEnd-searchStart)*  ((float)start - mDecodeCache[searchStart]->start )/mDecodeCache[searchEnd]->start;
   
         //we want guess to point at the first index that hits even if there are duplicate start times (which can happen)
         if(mDecodeCache[guess]->start+mDecodeCache[guess]->len >= start)
            searchEnd = --guess;
         else
            searchStart = guess;
      }
   }
   
   //this is a sorted array
   for(int i=searchStart; i < (int)mDecodeCache.size(); i++)
   {     
      //check for a cache hit - be careful to include the first/last sample an nothing more.
      //we only accept cache hits that touch either end - no piecing out of the middle.
      //this way the amount to be decoded remains set.
      if(start < mDecodeCache[i]->start+mDecodeCache[i]->len &&
         start + len > mDecodeCache[i]->start) 
      {
         uint8_t* outBuf;
         outBuf = (uint8_t*)data;
         //reject buffers that would split us into two pieces because we don't have
         //a method of dealing with this yet, and it won't happen very often.
         if(start<mDecodeCache[i]->start && start+len  > mDecodeCache[i]->start+mDecodeCache[i]->len)
            continue;
      
         int samplesHit;
         int hitStartInCache;
         int hitStartInRequest;
         int nChannels = mDecodeCache[i]->numChannels;
         samplesHit = FFMIN(start+len,mDecodeCache[i]->start+mDecodeCache[i]->len) 
                        - FFMAX(mDecodeCache[i]->start,start);
         //find the start of the hit relative to the cache buffer start.
         hitStartInCache   = FFMAX(0,start-mDecodeCache[i]->start);
         //we also need to find out which end was hit - if it is the tail only we need to update from a later index.
         hitStartInRequest = start <mDecodeCache[i]->start?len - samplesHit: 0;
         sampleCount outIndex,inIndex;
         for(int j=0;j<samplesHit;j++)
         {
            outIndex = hitStartInRequest + j;
            inIndex = (hitStartInCache + j) * nChannels + channel;
            switch (mDecodeCache[i]->samplefmt)
            {
               case SAMPLE_FMT_U8:
                  //printf("u8 in %llu out %llu cachelen %llu outLen %llu\n", inIndex, outIndex, mDecodeCache[i]->len, len);
                  ((int16_t *)outBuf)[outIndex] = (int16_t) (((uint8_t*)mDecodeCache[i]->samplePtr)[inIndex] - 0x80) << 8;
               break;
               
               case SAMPLE_FMT_S16:
                  //printf("u16 in %llu out %llu cachelen %llu outLen %llu\n", inIndex, outIndex, mDecodeCache[i]->len, len);
                  ((int16_t *)outBuf)[outIndex] = ((int16_t*)mDecodeCache[i]->samplePtr)[inIndex];
               break;
               
               case SAMPLE_FMT_S32:
                  //printf("s32 in %llu out %llu cachelen %llu outLen %llu\n", inIndex, outIndex, mDecodeCache[i]->len, len);                  
                  ((float *)outBuf)[outIndex] = (float) ((int32_t*)mDecodeCache[i]->samplePtr)[inIndex] * (1.0 / (1 << 31));
               break;
               
               case SAMPLE_FMT_FLT:
                  //printf("f in %llu out %llu cachelen %llu outLen %llu\n", inIndex, outIndex, mDecodeCache[i]->len, len);
                  ((float *)outBuf)[outIndex] = (float) ((float*)mDecodeCache[i]->samplePtr)[inIndex];
               break;
               
               case SAMPLE_FMT_DBL:
                  //printf("dbl in %llu out %llu cachelen %llu outLen %llu\n", inIndex, outIndex, mDecodeCache[i]->len, len);            
                  ((float *)outBuf)[outIndex] = (float) ((double*)mDecodeCache[i]->samplePtr)[inIndex];
               break;

               default:
                  printf("ODDecodeFFMPEG TASK unrecognized sample format\n");
                  return 1;
               break;
            }
         }
         //update the cursor
         samplesFilled += samplesHit;
         
         //update the input start/len params - if the end was hit we can take off just len.
         //otherwise, we can assume only the front of the request buffer was hit since we don't allow it to be split.
         if(start < mDecodeCache[i]->start)
            len-=samplesHit;
         else
         {
            //we update data pointer too- but it is a typedef'd char* so be careful with the pointer math
            data+= samplesHit * (SAMPLE_SIZE(outFormat) / sizeof(*data));
            start+=samplesHit;
            len -=samplesHit;
         }
      }
      //if we've had our fill, leave.  if we've passed the point which can have hits, leave.
      if(len<=0 ||  mDecodeCache[i]->start > start+len)
         break;
   }
   return samplesFilled;
}


//these next few look highly refactorable.
//get the right stream pointer.
streamContext* ODFFmpegDecoder::ReadNextFrame()
{
   return import_ffmpeg_read_next_frame(mFormatContext, mScs, mNumStreams);
}


int ODFFmpegDecoder::DecodeFrame(streamContext *sc, bool flushing)
{
   int ret = import_ffmpeg_decode_frame(sc, flushing);

   if (ret == 0 && sc->m_frameValid) {
      //stick it in the cache.
      //TODO- consider growing/unioning a few cache buffers like WaveCache does.  
      //however we can't use wavecache as it isn't going to handle our stereo interleaved part, and isn't for samples
      //However if other ODDecode tasks need this, we should do a new class for caching.
      FFMpegDecodeCache* cache = new FFMpegDecodeCache;
      //len is number of samples per channel
      cache->numChannels = sc->m_stream->codec->channels;

      cache->len = (sc->m_decodedAudioSamplesValidSiz / sc->m_samplesize) / cache->numChannels;
      cache->start = mCurrentPos;
      cache->samplePtr = (uint8_t*) malloc(sc->m_decodedAudioSamplesValidSiz);
      cache->samplefmt = sc->m_samplefmt;
      memcpy(cache->samplePtr, sc->m_decodedAudioSamples, sc->m_decodedAudioSamplesValidSiz);
      
      InsertCache(cache);
   }
   return ret;
}

void ODFFmpegDecoder::InsertCache(FFMpegDecodeCache* cache) {
   int searchStart = 0;
   int searchEnd = mDecodeCache.size(); //size() is also a valid insert index. 
   int guess = 0;
   //first just guess that the cache is contiguous and we can just use math to figure it out like a dictionary
   //by guessing where our hit will be.
   
//   printf("inserting cache start %llu, mCurrentPos %llu\n", cache->start, mCurrentPos);
   while(searchStart<searchEnd)
   {
      guess = (searchStart+searchEnd)/2;//searchStart+ (searchEnd-searchStart)*  ((float)cache->start - mDecodeCache[searchStart]->start )/mDecodeCache[searchEnd]->start;
      //check greater than OR equals because we want to insert infront of old dupes.
      if(mDecodeCache[guess]->start>= cache->start) {
//         if(mDecodeCache[guess]->start == cache->start) {
//            printf("dupe! start cache %llu start new cache %llu, mCurrentPos %llu\n",mDecodeCache[guess]->start, cache->start, mCurrentPos);
//         }
         searchEnd = guess;
      }
      else
         searchStart = ++guess;
   }
   mCurrentLen = cache->len;
   mCurrentPos=cache->start+cache->len; 
   mDecodeCache.insert(mDecodeCache.begin()+guess, cache);
   //      mDecodeCache.push_back(cache);
   
   mNumSamplesInCache+=cache->len;
   
   //if the cache is too big, drop some.  
   while(mNumSamplesInCache>kMaxSamplesInCache)
   {
      int dropindex;
      //drop which ever index is further from our newly added one.
      dropindex = (guess > (int)mDecodeCache.size()/2) ? 0 : (mDecodeCache.size()-1);
      mNumSamplesInCache-=mDecodeCache[dropindex]->len;
      free(mDecodeCache[dropindex]->samplePtr);
      delete mDecodeCache[dropindex];
      mDecodeCache.erase(mDecodeCache.begin()+dropindex); 
   }
}

#endif	//EXPERIMENTAL_OD_FFMPEG
#endif //USE_FFMPEG
