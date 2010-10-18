/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportPCM.cpp

  Dominic Mazzoni
  Leland Lucius

*//****************************************************************//**

\class PCMImportFileHandle
\brief An ImportFileHandle for PCM data

*//****************************************************************//**

\class PCMImportPlugin
\brief An ImportPlugin for PCM data

*//*******************************************************************/

#include "../Audacity.h"
#include "../AudacityApp.h"
#include "../Internat.h"
#include "../Tags.h"
#include "ImportPCM.h"

#include <wx/wx.h>
#include <wx/string.h>
#include <wx/utils.h>
#include <wx/intl.h>
#include <wx/ffile.h>
#include <wx/sizer.h>
#include <wx/checkbox.h>
#include <wx/button.h>
#include <wx/stattext.h>

#include "sndfile.h"

#include "../ondemand/ODManager.h"
#include "../ondemand/ODComputeSummaryTask.h"

//If OD is enabled, he minimum number of samples a file has to use it.
//Otherwise, we use the older PCMAliasBlockFile method since it should be fast enough.
#define kMinimumODFileSampleSize 44100*30

#ifndef SNDFILE_1
#error Requires libsndfile 1.0 or higher
#endif

#include "../FileFormats.h"
#include "../Prefs.h"
#include "../WaveTrack.h"
#include "ImportPlugin.h"

#ifdef USE_LIBID3TAG 
   #include <id3tag.h>
   // DM: the following functions were supposed to have been
   // included in id3tag.h - should be fixed in the next release
   // of mad.
   extern "C" {
      struct id3_frame *id3_frame_new(char const *);
      id3_length_t id3_latin1_length(id3_latin1_t const *);
      void id3_latin1_decode(id3_latin1_t const *, id3_ucs4_t *);
   } 
#endif

#define DESC _("WAV, AIFF, and other uncompressed types")

class PCMImportPlugin : public ImportPlugin
{
public:
   PCMImportPlugin()
   :  ImportPlugin(wxArrayString())
   {
      mExtensions = sf_get_all_extensions();
   }

   ~PCMImportPlugin() { }

   wxString GetPluginStringID() { return wxT("libsndfile"); }
   wxString GetPluginFormatDescription();
   ImportFileHandle *Open(wxString Filename);
};


class PCMImportFileHandle : public ImportFileHandle
{
public:
   PCMImportFileHandle(wxString name, SNDFILE *file, SF_INFO info);
   ~PCMImportFileHandle();

   wxString GetFileDescription();
   int GetFileUncompressedBytes();
   int Import(TrackFactory *trackFactory, Track ***outTracks,
              int *outNumTracks, Tags *tags);

   wxInt32 GetStreamCount(){ return 1; }

   wxArrayString *GetStreamInfo(){ return NULL; }

   void SetStreamUsage(wxInt32 StreamID, bool Use){}

private:
   SNDFILE              *mFile;
   SF_INFO               mInfo;
   sampleFormat          mFormat;
};

void GetPCMImportPlugin(ImportPluginList *importPluginList,
                        UnusableImportPluginList *unusableImportPluginList)
{
   importPluginList->Append(new PCMImportPlugin);
}

wxString PCMImportPlugin::GetPluginFormatDescription()
{
    return DESC;
}

ImportFileHandle *PCMImportPlugin::Open(wxString filename)
{
   SF_INFO info;
   SNDFILE *file = NULL;

   memset(&info, 0, sizeof(info));

   wxFile f;   // will be closed when it goes out of scope

   if (f.Open(filename)) {
      // Even though there is an sf_open() that takes a filename, use the one that
      // takes a file descriptor since wxWidgets can open a file with a Unicode name and
      // libsndfile can't (under Windows).
      file = sf_open_fd(f.fd(), SFM_READ, &info, TRUE);
   }

   // The file descriptor is now owned by "file", so we must tell "f" to leave
   // it alone.  The file descriptor is closed by sf_open_fd() even if an error
   // occurs.
   f.Detach();

   if (!file) {
      // TODO: Handle error
      //char str[1000];
      //sf_error_str((SNDFILE *)NULL, str, 1000);

      return NULL;
   }

   return new PCMImportFileHandle(filename, file, info);
}

PCMImportFileHandle::PCMImportFileHandle(wxString name,
                                         SNDFILE *file, SF_INFO info)
:  ImportFileHandle(name),
   mFile(file),
   mInfo(info)
{
   //
   // Figure out the format to use.
   //
   // In general, go with the user's preferences.  However, if
   // the file is higher-quality, go with a format which preserves
   // the quality of the original file.
   //
   
   mFormat = (sampleFormat)
      gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleFormat"), floatSample);

   if (mFormat != floatSample &&
       sf_subtype_more_than_16_bits(mInfo.format))
      mFormat = floatSample;
}

wxString PCMImportFileHandle::GetFileDescription()
{
   return sf_header_name(mInfo.format);
}

int PCMImportFileHandle::GetFileUncompressedBytes()
{
   return mInfo.frames * mInfo.channels * SAMPLE_SIZE(mFormat);
}

int PCMImportFileHandle::Import(TrackFactory *trackFactory,
                                Track ***outTracks,
                                int *outNumTracks,
                                Tags *tags)
{
   wxASSERT(mFile);

   CreateProgress();

   WaveTrack **channels = new WaveTrack *[mInfo.channels];

   int c;
   for (c = 0; c < mInfo.channels; c++) {
      channels[c] = trackFactory->NewWaveTrack(mFormat, mInfo.samplerate);

      if (mInfo.channels > 1)
         switch (c) {
         case 0:
            channels[c]->SetChannel(Track::LeftChannel);
            break;
         case 1:
            channels[c]->SetChannel(Track::RightChannel);
            break;
         default:
            channels[c]->SetChannel(Track::MonoChannel);
         }
   }

   if (mInfo.channels == 2) {
      channels[0]->SetLinked(true);
   }

   sampleCount fileTotalFrames = (sampleCount)mInfo.frames;
   sampleCount maxBlockSize = channels[0]->GetMaxBlockSize();
   int updateResult = false;
   
   wxString copyEdit =
       gPrefs->Read(wxT("/FileFormats/CopyOrEditUncompressedData"), wxT("edit"));

   // Fall back to "edit" if it doesn't match anything else
   bool doEdit = true;          
   if (copyEdit.IsSameAs(wxT("copy"), false))
      doEdit = false;
      
   // If the format is not seekable, we must use 'copy' mode,
   // because 'edit' mode depends on the ability to seek to an
   // arbitrary location in the file.
   if (!mInfo.seekable)
      doEdit = false;

   if (doEdit) {
      // If this mode has been selected, we form the tracks as
      // aliases to the files we're editing, i.e. ("foo.wav", 12000-18000)
      // instead of actually making fresh copies of the samples.
      
      // lets use OD only if the file is longer than 30 seconds.  Otherwise, why wake up extra threads.
      //todo: make this a user pref.
      bool useOD =fileTotalFrames>kMinimumODFileSampleSize;
      int updateCounter = 0;

      for (sampleCount i = 0; i < fileTotalFrames; i += maxBlockSize) {
	  
         sampleCount blockLen = maxBlockSize;
         if (i + blockLen > fileTotalFrames)
            blockLen = fileTotalFrames - i;

         for (c = 0; c < mInfo.channels; c++)
            channels[c]->AppendAlias(mFilename, i, blockLen, c,useOD);

         if (++updateCounter == 50) {
            updateResult = mProgress->Update(i, fileTotalFrames);
            updateCounter = 0;
            if (updateResult != eProgressSuccess)
               break;
         }
      }
      updateResult = mProgress->Update(fileTotalFrames, fileTotalFrames);
       
      if(useOD)
      { 
         ODComputeSummaryTask* computeTask=new ODComputeSummaryTask;
         bool moreThanStereo = mInfo.channels>2;
         for (c = 0; c < mInfo.channels; c++)
         {
            computeTask->AddWaveTrack(channels[c]);
            if(moreThanStereo)
            {
               //if we have 3 more channels, they get imported on seperate tracks, so we add individual tasks for each.
               ODManager::Instance()->AddNewTask(computeTask);
               computeTask=new ODComputeSummaryTask;
            }
         }
         //if we have a linked track, we add ONE task.
         if(!moreThanStereo)
            ODManager::Instance()->AddNewTask(computeTask);
      }
   }
   else {
      // Otherwise, we're in the "copy" mode, where we read in the actual
      // samples from the file and store our own local copy of the
      // samples in the tracks.
      
      samplePtr srcbuffer = NewSamples(maxBlockSize * mInfo.channels,
                                       mFormat);
      samplePtr buffer = NewSamples(maxBlockSize, mFormat);

      unsigned long framescompleted = 0;
      
      long block;
      do {
         block = maxBlockSize;
         
         if (mFormat == int16Sample)
            block = sf_readf_short(mFile, (short *)srcbuffer, block);
         //import 24 bit int as float and have the append function convert it.  This is how PCMAliasBlockFile works too.
         else
            block = sf_readf_float(mFile, (float *)srcbuffer, block);
         
         if (block) {
            for(c=0; c<mInfo.channels; c++) {
               if (mFormat==int16Sample) {
                  for(int j=0; j<block; j++)
                     ((short *)buffer)[j] =
                        ((short *)srcbuffer)[mInfo.channels*j+c];
               }
               else {
                  for(int j=0; j<block; j++)
                     ((float *)buffer)[j] =
                        ((float *)srcbuffer)[mInfo.channels*j+c];
               }
               
               channels[c]->Append(buffer, (mFormat == int16Sample)?int16Sample:floatSample, block);
            }
            framescompleted += block;
         }

         updateResult = mProgress->Update((long long unsigned)framescompleted,
                                        (long long unsigned)fileTotalFrames);
         if (updateResult != eProgressSuccess)
            break;

      } while (block > 0);

      DeleteSamples(buffer);
      DeleteSamples(srcbuffer);
   }

   if (updateResult == eProgressFailed || updateResult == eProgressCancelled) {
      for (c = 0; c < mInfo.channels; c++)
         delete channels[c];
      delete[] channels;

      return updateResult;
   }

   *outNumTracks = mInfo.channels;
   *outTracks = new Track *[mInfo.channels];
   for(c = 0; c < mInfo.channels; c++) {
         channels[c]->Flush();
         (*outTracks)[c] = channels[c];
      }
      delete[] channels;

   const char *str;

   str = sf_get_string(mFile, SF_STR_TITLE);
   if (str) {
      tags->SetTag(TAG_TITLE, UTF8CTOWX(str));
   }

   str = sf_get_string(mFile, SF_STR_ARTIST);
   if (str) {
      tags->SetTag(TAG_ARTIST, UTF8CTOWX(str));
   }

   str = sf_get_string(mFile, SF_STR_COMMENT);
   if (str) {
      tags->SetTag(TAG_COMMENTS, UTF8CTOWX(str));
   }

   str = sf_get_string(mFile, SF_STR_DATE);
   if (str) {
      tags->SetTag(TAG_YEAR, UTF8CTOWX(str));
   }

   str = sf_get_string(mFile, SF_STR_COPYRIGHT);
   if (str) {
      tags->SetTag(wxT("Copyright"), UTF8CTOWX(str));
   }

   str = sf_get_string(mFile, SF_STR_SOFTWARE);
   if (str) {
      tags->SetTag(wxT("Software"), UTF8CTOWX(str));
   }

#if defined(USE_LIBID3TAG)
   if ((mInfo.format & SF_FORMAT_TYPEMASK) == SF_FORMAT_AIFF) {
      wxFFile f(mFilename, wxT("rb"));
      if (f.IsOpened()) {
         char id[5];
         wxUint32 len;

         id[4] = '\0';

         f.Seek(12);        // Skip filetype, length, and formtype

         while (!f.Error()) {
            f.Read(id, 4);    // Get chunk type
            if (f.Eof()) {
               break;
            }
            f.Read(&len, 4);
            len = wxUINT32_SWAP_ON_LE(len);

            if (strcmp(id, "ID3 ") != 0) {
               f.Seek(len + (len & 0x01), wxFromCurrent);
               continue;
            }

            id3_byte_t *buffer = (id3_byte_t *)malloc(len);
            if (!buffer) {
               break;
            }

            f.Read(buffer, len);
            struct id3_tag *tp = id3_tag_parse(buffer, len);
            free(buffer);

            if (!tp) {
               break;
            }

            tags->SetID3V2( tp->options & ID3_TAG_OPTION_ID3V1 ? false : true );

            // Loop through all frames
            for (int i = 0; i < (int) tp->nframes; i++) {
               struct id3_frame *frame = tp->frames[i];

               // printf("ID: %08x '%4s'\n", (int) *(int *)frame->id, frame->id);
               // printf("Desc: %s\n", frame->description);
               // printf("Num fields: %d\n", frame->nfields);

               // for (int j = 0; j < (int) frame->nfields; j++) {
               //    printf("field %d type %d\n", j, frame->fields[j].type );
               //    if (frame->fields[j].type == ID3_FIELD_TYPE_STRINGLIST) {
               //       printf("num strings %d\n", frame->fields[j].stringlist.nstrings);
               //    }
               // }

               wxString n, v;

               // Determine the tag name
               if (strcmp(frame->id, ID3_FRAME_TITLE) == 0) {
                  n = TAG_TITLE;
               }
               else if (strcmp(frame->id, ID3_FRAME_ARTIST) == 0) {
                  n = TAG_ARTIST;
               }
               else if (strcmp(frame->id, ID3_FRAME_ALBUM) == 0) {
                  n = TAG_ALBUM;
               }
               else if (strcmp(frame->id, ID3_FRAME_TRACK) == 0) {
                  n = TAG_TRACK;
               }
               else if (strcmp(frame->id, "TYER") == 0) {
                  n = TAG_YEAR;
               }
               else if (strcmp(frame->id, ID3_FRAME_YEAR) == 0) {
                  n = TAG_YEAR;
               }
               else if (strcmp(frame->id, ID3_FRAME_COMMENT) == 0) {
                  n = TAG_COMMENTS;
               }
               else if (strcmp(frame->id, ID3_FRAME_GENRE) == 0) {
                  n = TAG_GENRE;
               }
               else {
                  // Use frame description as default tag name.  The descriptions
                  // may include several "meanings" separated by "/" characters, so
                  // we just use the first meaning
                  n = UTF8CTOWX(frame->description).BeforeFirst(wxT('/'));
               }

               const id3_ucs4_t *ustr = NULL;

               if (n == TAG_COMMENTS) {
                  ustr = id3_field_getfullstring(&frame->fields[3]);
               }
               else if (frame->nfields == 3) {
                  ustr = id3_field_getstring(&frame->fields[1]);
                  if (ustr) {
                     char *str = (char *)id3_ucs4_utf8duplicate(ustr);
                     n = UTF8CTOWX(str);
                     free(str);
                  }

                  ustr = id3_field_getstring(&frame->fields[2]);
               }
               else if (frame->nfields >= 2) {
                  ustr = id3_field_getstrings(&frame->fields[1], 0);
               }

               if (ustr) {
                  char *str = (char *)id3_ucs4_utf8duplicate(ustr);
                  v = UTF8CTOWX(str);
                  free(str);
               }

               if (!n.IsEmpty() && !v.IsEmpty()) {
                  tags->SetTag(n, v);
               }
            }

            // Convert v1 genre to name
            if (tags->HasTag(TAG_GENRE)) {
               long g = -1;
               if (tags->GetTag(TAG_GENRE).ToLong(&g)) {
                  tags->SetTag(TAG_GENRE, tags->GetGenre(g));
               }
            }

            id3_tag_delete(tp);
            break;
         }

         f.Close();
      }
   }
#endif

   return updateResult;
}

PCMImportFileHandle::~PCMImportFileHandle()
{
   sf_close(mFile);
}


