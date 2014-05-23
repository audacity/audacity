/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportOGG.cpp

  Joshua Haberman
  Leland Lucius

*//****************************************************************//**

\class ImportFileHandle
\brief An ImportFileHandle for data

  The Ogg format supports multiple logical bitstreams that can be chained
  within the physical bitstream. The sampling rate and number of channels
  can vary between these logical bitstreams. For the moment, we'll ignore
  all but the first logical bitstream.

  Ogg also allows for an arbitrary number of channels. Luckily, so does
  Audacity. We'll call the first channel LeftChannel, the second
  RightChannel, and all others after it MonoChannel.

*//****************************************************************//**

\class OGGImportPlugin
\brief An ImportPlugin for OGG data

*//*******************************************************************/

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/window.h>
#endif

#include <wx/intl.h>
#include "../Audacity.h"
#include "ImportOGG.h"
#include "../Internat.h"
#include "../Tags.h"

#define DESC _("Ogg Vorbis files")

static const wxChar *exts[] =
{
   wxT("ogg")
};

#ifndef USE_LIBVORBIS
/* BPF There is no real reason to compile without LIBVORBIS, but if you do, you will needs this header */
#include "ImportPlugin.h"  

void GetOGGImportPlugin(ImportPluginList *importPluginList,
                        UnusableImportPluginList *unusableImportPluginList)
{
   UnusableImportPlugin* oggIsUnsupported =
      new UnusableImportPlugin(DESC, wxArrayString(WXSIZEOF(exts), exts));

   unusableImportPluginList->Append(oggIsUnsupported);
}

#else /* USE_LIBVORBIS */

#include <wx/log.h>
#include <wx/string.h>
#include <wx/utils.h>
#include <wx/intl.h>
/* ffile.h must be included AFTER at least one other wx header that includes
 * wx/setup.h, otherwise #ifdefs erronously collapse it to nothing. This is
 * a bug in wxWidgets (ffile.h should itself include wx/setup.h), and it
 * was a bitch to track down. */
#include <wx/ffile.h>

#include <vorbis/vorbisfile.h>

#include "../WaveTrack.h"
#include "ImportPlugin.h"

class OggImportPlugin : public ImportPlugin
{
public:
   OggImportPlugin()
   :  ImportPlugin(wxArrayString(WXSIZEOF(exts), exts))
   {
   }

   ~OggImportPlugin() { }

   wxString GetPluginStringID() { return wxT("liboggvorbis"); }
   wxString GetPluginFormatDescription();
   ImportFileHandle *Open(wxString Filename);
};


class OggImportFileHandle : public ImportFileHandle
{
public:
   OggImportFileHandle(const wxString & filename,
                       wxFFile *file,
                       OggVorbis_File *vorbisFile)
   :  ImportFileHandle(filename),
      mFile(file),
      mVorbisFile(vorbisFile)
   {
      mStreamInfo = new wxArrayString();
      mStreamUsage = new int[vorbisFile->links];
      for (int i = 0; i < vorbisFile->links; i++)
      {
         wxString strinfo;
         strinfo.Printf(wxT("Index[%02x] Version[%d], Channels[%d], Rate[%d]"),i,vorbisFile->vi[i].version,vorbisFile->vi[i].channels,vorbisFile->vi[i].rate);
         mStreamInfo->Add(strinfo);
         mStreamUsage[i] = 0;
      }
      
   }
   ~OggImportFileHandle();

   wxString GetFileDescription();
   int GetFileUncompressedBytes();
   int Import(TrackFactory *trackFactory, Track ***outTracks,
              int *outNumTracks, Tags *tags);

   wxInt32 GetStreamCount()
   {
      if (mVorbisFile)
         return mVorbisFile->links;
      else
         return 0;
   }

   wxArrayString *GetStreamInfo()
   {
      return mStreamInfo;
   }

   void SetStreamUsage(wxInt32 StreamID, bool Use)
   {
      if (mVorbisFile)
      {
         if (StreamID < mVorbisFile->links)
            mStreamUsage[StreamID] = (Use ? 1 : 0);
      }
   }

private:
   wxFFile        *mFile;
   OggVorbis_File *mVorbisFile;

   int            *mStreamUsage;
   wxArrayString  *mStreamInfo;
   WaveTrack    ***mChannels;
};

void GetOGGImportPlugin(ImportPluginList *importPluginList,
                        UnusableImportPluginList * WXUNUSED(unusableImportPluginList))
{
   importPluginList->Append(new OggImportPlugin);
}

wxString OggImportPlugin::GetPluginFormatDescription()
{
    return DESC;
}

ImportFileHandle *OggImportPlugin::Open(wxString filename)
{
   OggVorbis_File *vorbisFile = new OggVorbis_File;
   wxFFile *file = new wxFFile(filename, wxT("rb"));

   if (!file->IsOpened()) {
      // No need for a message box, it's done automatically (but how?)
      delete vorbisFile;
      delete file;
      return NULL;
   }

   int err = ov_open(file->fp(), vorbisFile, NULL, 0);

   if (err < 0) {
      wxString message;

      switch (err) {
         case OV_EREAD:
            message = _("Media read error");
            break;
         case OV_ENOTVORBIS:
            message = _("Not an Ogg Vorbis file");
            break;
         case OV_EVERSION:
            message = _("Vorbis version mismatch");
            break;
         case OV_EBADHEADER:
            message = _("Invalid Vorbis bitstream header");
            break;
         case OV_EFAULT:
            message = _("Internal logic fault");
            break;
      }

      // what to do with message?
      file->Close();
      delete vorbisFile;
      delete file;
      return NULL;
   }

   return new OggImportFileHandle(filename, file, vorbisFile);
}

wxString OggImportFileHandle::GetFileDescription()
{
   return DESC;
}

int OggImportFileHandle::GetFileUncompressedBytes()
{
   // TODO:
   return 0;
}

int OggImportFileHandle::Import(TrackFactory *trackFactory, Track ***outTracks,
                                int *outNumTracks, Tags *tags)
{
   wxASSERT(mFile->IsOpened());

   CreateProgress();

   //Number of streams used may be less than mVorbisFile->links,
   //but this way bitstream matches array index.
   mChannels = new WaveTrack **[mVorbisFile->links];

   int i,c;
   for (i = 0; i < mVorbisFile->links; i++)
   {
      //Stream is not used
      if (mStreamUsage[i] == 0)
      {
         //This is just a padding to keep bitstream number and
         //array indices matched.
         mChannels[i] = NULL;
         continue;
      }

      vorbis_info *vi = ov_info(mVorbisFile, i);

      mChannels[i] = new WaveTrack *[vi->channels];

      for (c = 0; c < vi->channels; c++) {
         mChannels[i][c] = trackFactory->NewWaveTrack(int16Sample, vi->rate);

         if (vi->channels == 2) {
            switch (c) {
         case 0:
            mChannels[i][c]->SetChannel(Track::LeftChannel);
            mChannels[i][c]->SetLinked(true);
            break;
         case 1:
            mChannels[i][c]->SetChannel(Track::RightChannel);
            break;
            }
         }
         else {
            mChannels[i][c]->SetChannel(Track::MonoChannel);
         }
      }
   }

/* The number of bytes to get from the codec in each run */
#define CODEC_TRANSFER_SIZE 4096

/* The number of samples to read between calls to the callback.
 * Balance between responsiveness of the GUI and throughput of import. */
#define SAMPLES_PER_CALLBACK 100000

   short *mainBuffer = new short[CODEC_TRANSFER_SIZE];

   /* determine endianness (clever trick courtesy of Nicholas Devillard,
    * (http://www.eso.org/~ndevilla/endian/) */
   int testvar = 1, endian;
   if(*(char *)&testvar)
      endian = 0;  // little endian
   else
      endian = 1;  // big endian

   /* number of samples currently in each channel's buffer */
   int updateResult = eProgressSuccess;
   long bytesRead = 0;
   long samplesRead = 0;
   int bitstream = 0;
   int samplesSinceLastCallback = 0;

   // You would think that the stream would already be seeked to 0, and
   // indeed it is if the file is legit.  But I had several ogg files on
   // my hard drive that have malformed headers, and this added call
   // causes them to be read correctly.  Otherwise they have lots of
   // zeros inserted at the beginning
   ov_pcm_seek(mVorbisFile, 0);
   
   do {
      /* get data from the decoder */
      bytesRead = ov_read(mVorbisFile, (char *) mainBuffer,
                          CODEC_TRANSFER_SIZE,
                          endian,
                          2,    // word length (2 for 16 bit samples)
                          1,    // signed
                          &bitstream);

      if (bytesRead == OV_HOLE) {
         wxFileName f(mFilename);
         wxLogError(wxT("Ogg Vorbis importer: file %s is malformed, ov_read() reported a hole"),
                    f.GetFullName().c_str());
         /* http://lists.xiph.org/pipermail/vorbis-dev/2001-February/003223.html
          * is the justification for doing this - best effort for malformed file,
          * hence the message.
          */
         continue;
      } else if (bytesRead < 0) {
         /* Malformed Ogg Vorbis file. */
         /* TODO: Return some sort of meaningful error. */
         wxLogError(wxT("Ogg Vorbis importer: ov_read() returned error %i"),
                    bytesRead);
         break;
      }

      samplesRead = bytesRead / mVorbisFile->vi[bitstream].channels / sizeof(short);

      /* give the data to the wavetracks */
      if (mStreamUsage[bitstream] != 0)
      {
         for (c = 0; c < mVorbisFile->vi[bitstream].channels; c++)
            mChannels[bitstream][c]->Append((char *)(mainBuffer + c),
            int16Sample,
            samplesRead,
            mVorbisFile->vi[bitstream].channels);
      }

      samplesSinceLastCallback += samplesRead;
      if (samplesSinceLastCallback > SAMPLES_PER_CALLBACK) {
          updateResult = mProgress->Update(ov_time_tell(mVorbisFile),
                                         ov_time_total(mVorbisFile, bitstream));
          samplesSinceLastCallback -= SAMPLES_PER_CALLBACK;

      }
   } while (updateResult == eProgressSuccess && bytesRead != 0);

   delete[]mainBuffer;

   int res = updateResult;
   if (bytesRead < 0)
     res = eProgressFailed;

   if (res == eProgressFailed || res == eProgressCancelled) {
      for (i = 0; i < mVorbisFile->links; i++)
      {
         if (mChannels[i])
         {
            for(c = 0; c < mVorbisFile->vi[bitstream].channels; c++) {
               if (mChannels[i][c])
                  delete mChannels[i][c];
            }
            delete[] mChannels[i];
         }
      }
      delete[] mChannels;
      return res;
   }

   *outNumTracks = 0;
   for (int s = 0; s < mVorbisFile->links; s++)
   {
      if (mStreamUsage[s] != 0)
         *outNumTracks += mVorbisFile->vi[s].channels;
   }

   *outTracks = new Track *[*outNumTracks];
   
   int trackindex = 0;
   for (i = 0; i < mVorbisFile->links; i++)
   {
      if (mChannels[i])
      {
         for (c = 0; c < mVorbisFile->vi[i].channels; c++) {
            mChannels[i][c]->Flush();
            (*outTracks)[trackindex++] = mChannels[i][c];
         }
         delete[] mChannels[i];
      }      
   }
   delete[] mChannels;

   //\todo { Extract comments from each stream? }
   if (mVorbisFile->vc[0].comments > 0) {
      tags->Clear();
      for (c = 0; c < mVorbisFile->vc[0].comments; c++) {
         wxString comment = UTF8CTOWX(mVorbisFile->vc[0].user_comments[c]);
         wxString name = comment.BeforeFirst(wxT('='));
         wxString value = comment.AfterFirst(wxT('='));
         if (name.Upper() == wxT("DATE") && !tags->HasTag(TAG_YEAR)) {
            long val;
            if (value.Length() == 4 && value.ToLong(&val)) {
               name = TAG_YEAR;
            }
         }
         tags->SetTag(name, value);
      }
   }

   return res;
}

OggImportFileHandle::~OggImportFileHandle()
{
   ov_clear(mVorbisFile);
   mFile->Detach();    // so that it doesn't try to close the file (ov_clear()
                       // did that already)
   delete mStreamInfo;
   delete[] mStreamUsage;
   delete mVorbisFile;
   delete mFile;
}

#endif                          /* USE_LIBVORBIS */
