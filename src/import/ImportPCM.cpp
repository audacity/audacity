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



#include "Import.h"
#include "Tags.h"

#include <wx/wx.h>
#include <wx/ffile.h>
#include <wx/checkbox.h>
#include <wx/stattext.h>

#include "sndfile.h"

#include "ShuttleGui.h"

#include "../widgets/ProgressDialog.h"

#ifndef SNDFILE_1
#error Requires libsndfile 1.0 or higher
#endif

#include "../FileFormats.h"
#include "Prefs.h"
#include "ShuttleGui.h"
#include "WaveTrack.h"
#include "ImportPlugin.h"

#include <algorithm>

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

#define DESC XO("WAV, AIFF, and other uncompressed types")

class PCMImportPlugin final : public ImportPlugin
{
public:
   PCMImportPlugin()
   :  ImportPlugin(sf_get_all_extensions())
   {
   }

   ~PCMImportPlugin() { }

   wxString GetPluginStringID() override { return wxT("libsndfile"); }
   TranslatableString GetPluginFormatDescription() override;
   std::unique_ptr<ImportFileHandle> Open(
      const FilePath &Filename, AudacityProject*) override;
};


class PCMImportFileHandle final : public ImportFileHandle
{
public:
   PCMImportFileHandle(const FilePath &name, SFFile &&file, SF_INFO info);
   ~PCMImportFileHandle();

   TranslatableString GetFileDescription() override;
   ByteCount GetFileUncompressedBytes() override;
   ProgressResult Import(WaveTrackFactory *trackFactory, TrackHolders &outTracks,
              Tags *tags) override;

   wxInt32 GetStreamCount() override { return 1; }

   const TranslatableStrings &GetStreamInfo() override
   {
      static TranslatableStrings empty;
      return empty;
   }

   void SetStreamUsage(wxInt32 WXUNUSED(StreamID), bool WXUNUSED(Use)) override
   {}

private:
   SFFile                mFile;
   const SF_INFO         mInfo;
   sampleFormat          mEffectiveFormat;
   sampleFormat          mFormat;
};

TranslatableString PCMImportPlugin::GetPluginFormatDescription()
{
    return DESC;
}

std::unique_ptr<ImportFileHandle> PCMImportPlugin::Open(
   const FilePath &filename, AudacityProject*)
{
   SF_INFO info;
   wxFile f;   // will be closed when it goes out of scope
   SFFile file;

   memset(&info, 0, sizeof(info));


#ifdef __WXGTK__
   if (filename.Lower().EndsWith(wxT("mp3"))) {
      // There is a bug in libsndfile where mp3s with duplicated metadata tags
      // will crash libsndfile and thus audacity.
      // We have patched the lib-src version of libsndfile, but
      // for linux the user can build against the system libsndfile which
      // still has this bug.
      // This happens in sf_open_fd, which is the very first point of
      // interaction with libsndfile, so the only workaround is to hardcode
      // ImportPCM to not handle .mp3.  Of course, this will still fail for mp3s
      // that are mislabeled with a .wav or other extension.
      // So, in the future we may want to write a simple parser to detect mp3s here.
      return NULL;
   }
#endif


   if (f.Open(filename)) {
      // Even though there is an sf_open() that takes a filename, use the one that
      // takes a file descriptor since wxWidgets can open a file with a Unicode name and
      // libsndfile can't (under Windows).
      file.reset(SFCall<SNDFILE*>(sf_open_fd, f.fd(), SFM_READ, &info, TRUE));
   }

   // The file descriptor is now owned by "file", so we must tell "f" to leave
   // it alone.  The file descriptor is closed by the destructor of file even if an error
   // occurs.
   f.Detach();

   if (!file) {
      // TODO: Handle error
      //char str[1000];
      //sf_error_str((SNDFILE *)NULL, str, 1000);

      return nullptr;
   } else if (file &&
              (info.format & SF_FORMAT_TYPEMASK) == SF_FORMAT_OGG) {
      // mchinen 15.1.2012 - disallowing libsndfile to handle
      // ogg files because seeking is broken at this date (very slow,
      // seeks from beginning of file each seek).
      // This was said by Erik (libsndfile maintainer).
      // Note that this won't apply to our local libsndfile, so only
      // linux builds that use --with-libsndfile=system are affected,
      // as our local libsndfile doesn't do OGG.
      // In particular ubuntu 10.10 and 11.04 are known to be affected
      // When the bug is fixed, we can check version to avoid only
      // the broken builds.

      return nullptr;
   }

   // Success, so now transfer the duty to close the file from "file".
   return std::make_unique<PCMImportFileHandle>(filename, std::move(file), info);
}

static Importer::RegisteredImportPlugin registered{ "PCM",
   std::make_unique< PCMImportPlugin >()
};

PCMImportFileHandle::PCMImportFileHandle(const FilePath &name,
                                         SFFile &&file, SF_INFO info)
:  ImportFileHandle(name),
   mFile(std::move(file)),
   mInfo(info)
{
   wxASSERT(info.channels >= 0);

   //
   // Figure out the format to use.
   //
   // In general, go with the user's preferences.  However, if
   // the file is higher-quality, go with a format which preserves
   // the quality of the original file.
   //

   // Effective format
   mEffectiveFormat = sf_subtype_to_effective_format(mInfo.format);
   // But maybe different storage format
   mFormat = ChooseFormat(mEffectiveFormat);
}

TranslatableString PCMImportFileHandle::GetFileDescription()
{
   // Library strings
   // See the major_formats and subtype_formats tables in command.c in
   // libsndfile for this list of possibilities

using Unevaluated = decltype(
   /* major_formats */
     XO("AIFF (Apple/SGI)")
   , XO("AU (Sun/NeXT)")
   , XO("AVR (Audio Visual Research)")
   , XO("CAF (Apple Core Audio File)")
   /* i18n-hint: "codec" is short for a "coder-decoder" algorithm */
   , XO("FLAC (FLAC Lossless Audio Codec)")
   , XO("HTK (HMM Tool Kit)")
   , XO("IFF (Amiga IFF/SVX8/SV16)")
   , XO("MAT4 (GNU Octave 2.0 / Matlab 4.2)")
   , XO("MAT5 (GNU Octave 2.1 / Matlab 5.0)")
   , XO("MPC (Akai MPC 2k)")
   , XO("OGG (OGG Container format)")
   , XO("PAF (Ensoniq PARIS)")
   , XO("PVF (Portable Voice Format)")
   , XO("RAW (header-less)")
   , XO("RF64 (RIFF 64)")
   , XO("SD2 (Sound Designer II)")
   , XO("SDS (Midi Sample Dump Standard)")
   , XO("SF (Berkeley/IRCAM/CARL)")
   , XO("VOC (Creative Labs)")
   , XO("W64 (SoundFoundry WAVE 64)")
   , XO("WAV (Microsoft)")
   , XO("WAV (NIST Sphere)")
   , XO("WAVEX (Microsoft)")
   , XO("WVE (Psion Series 3)")
   , XO("XI (FastTracker 2)")
);

using Unevaluated2 = decltype(
   /* subtype_formats */
     XO("Signed 8 bit PCM")
   , XO("Signed 16 bit PCM")
   , XO("Signed 24 bit PCM")
   , XO("Signed 32 bit PCM")
   , XO("Unsigned 8 bit PCM")
   , XO("32 bit float")
   , XO("64 bit float")
   , XO("U-Law")
   , XO("A-Law")
   , XO("IMA ADPCM")
   , XO("Microsoft ADPCM")
   , XO("GSM 6.10")
   , XO("32kbs G721 ADPCM")
   , XO("24kbs G723 ADPCM")
   , XO("12 bit DWVW")
   , XO("16 bit DWVW")
   , XO("24 bit DWVW")
   , XO("VOX ADPCM")
   , XO("16 bit DPCM")
   , XO("8 bit DPCM")
   , XO("Vorbis")
);

   auto untranslated = SFCall<wxString>(sf_header_name, mInfo.format);
   return TranslatableString{
      untranslated, {} };
}

auto PCMImportFileHandle::GetFileUncompressedBytes() -> ByteCount
{
   return mInfo.frames * mInfo.channels * SAMPLE_SIZE(mFormat);
}

#ifdef USE_LIBID3TAG
struct id3_tag_deleter {
   void operator () (id3_tag *p) const { if (p) id3_tag_delete(p); }
};
using id3_tag_holder = std::unique_ptr<id3_tag, id3_tag_deleter>;
#endif

using NewChannelGroup = std::vector< std::shared_ptr<WaveTrack> >;

ProgressResult PCMImportFileHandle::Import(WaveTrackFactory *trackFactory,
                                TrackHolders &outTracks,
                                Tags *tags)
{
   outTracks.clear();

   wxASSERT(mFile.get());

   CreateProgress();

   NewChannelGroup channels(mInfo.channels);

   {
      // iter not used outside this scope.
      auto iter = channels.begin();
      for (int c = 0; c < mInfo.channels; ++iter, ++c)
         *iter = NewWaveTrack(*trackFactory, mFormat, mInfo.samplerate);
   }

   auto fileTotalFrames =
      (sampleCount)mInfo.frames; // convert from sf_count_t
   auto maxBlockSize = channels.begin()->get()->GetMaxBlockSize();
   auto updateResult = ProgressResult::Cancelled;

   {
      // Otherwise, we're in the "copy" mode, where we read in the actual
      // samples from the file and store our own local copy of the
      // samples in the tracks.

      // PRL:  guard against excessive memory buffer allocation in case of many channels
      using type = decltype(maxBlockSize);
      if (mInfo.channels < 1)
         return ProgressResult::Failed;
      auto maxBlock = std::min(maxBlockSize,
         std::numeric_limits<type>::max() /
            (mInfo.channels * SAMPLE_SIZE(mFormat))
      );
      if (maxBlock < 1)
         return ProgressResult::Failed;

      SampleBuffer srcbuffer, buffer;
      wxASSERT(mInfo.channels >= 0);
      while (NULL == srcbuffer.Allocate(maxBlock * mInfo.channels, mFormat).ptr() ||
             NULL == buffer.Allocate(maxBlock, mFormat).ptr())
      {
         maxBlock /= 2;
         if (maxBlock < 1)
            return ProgressResult::Failed;
      }

      decltype(fileTotalFrames) framescompleted = 0;

      long block;
      do {
         block = maxBlock;

         if (mFormat == int16Sample)
            block = SFCall<sf_count_t>(sf_readf_short, mFile.get(), (short *)srcbuffer.ptr(), block);
         //import 24 bit int as float and have the append function convert it.  This is how PCMAliasBlockFile worked too.
         else
            block = SFCall<sf_count_t>(sf_readf_float, mFile.get(), (float *)srcbuffer.ptr(), block);

         if(block < 0 || block > (long)maxBlock) {
            wxASSERT(false);
            block = maxBlock;
         }

         if (block) {
            auto iter = channels.begin();
            for(int c=0; c<mInfo.channels; ++iter, ++c) {
               if (mFormat==int16Sample) {
                  for(int j=0; j<block; j++)
                     ((short *)buffer.ptr())[j] =
                        ((short *)srcbuffer.ptr())[mInfo.channels*j+c];
               }
               else {
                  for(int j=0; j<block; j++)
                     ((float *)buffer.ptr())[j] =
                        ((float *)srcbuffer.ptr())[mInfo.channels*j+c];
               }

               iter->get()->Append(
                  buffer.ptr(),
                  (mFormat == int16Sample) ? int16Sample : floatSample,
                  block, 1, mEffectiveFormat);
            }
            framescompleted += block;
         }

         updateResult = mProgress->Update(
            framescompleted.as_long_long(),
            fileTotalFrames.as_long_long()
         );
         if (updateResult != ProgressResult::Success)
            break;

      } while (block > 0);
   }

   if (updateResult == ProgressResult::Failed || updateResult == ProgressResult::Cancelled) {
      return updateResult;
   }

   for(const auto &channel : channels)
      channel->Flush();

   if (!channels.empty())
      outTracks.push_back(std::move(channels));

   const char *str;

   str = sf_get_string(mFile.get(), SF_STR_TITLE);
   if (str) {
      tags->SetTag(TAG_TITLE, UTF8CTOWX(str));
   }

   str = sf_get_string(mFile.get(), SF_STR_ALBUM);
   if (str) {
      tags->SetTag(TAG_ALBUM, UTF8CTOWX(str));
   }

   str = sf_get_string(mFile.get(), SF_STR_ARTIST);
   if (str) {
      tags->SetTag(TAG_ARTIST, UTF8CTOWX(str));
   }

   str = sf_get_string(mFile.get(), SF_STR_COMMENT);
   if (str) {
      tags->SetTag(TAG_COMMENTS, UTF8CTOWX(str));
   }

   str = sf_get_string(mFile.get(), SF_STR_DATE);
   if (str) {
      tags->SetTag(TAG_YEAR, UTF8CTOWX(str));
   }

   str = sf_get_string(mFile.get(), SF_STR_COPYRIGHT);
   if (str) {
      tags->SetTag(TAG_COPYRIGHT, UTF8CTOWX(str));
   }

   str = sf_get_string(mFile.get(), SF_STR_SOFTWARE);
   if (str) {
      tags->SetTag(TAG_SOFTWARE, UTF8CTOWX(str));
   }

   str = sf_get_string(mFile.get(), SF_STR_TRACKNUMBER);
   if (str) {
      tags->SetTag(TAG_TRACK, UTF8CTOWX(str));
   }

   str = sf_get_string(mFile.get(), SF_STR_GENRE);
   if (str) {
      tags->SetTag(TAG_GENRE, UTF8CTOWX(str));
   }

#if defined(USE_LIBID3TAG)
   if (((mInfo.format & SF_FORMAT_TYPEMASK) == SF_FORMAT_AIFF) ||
       ((mInfo.format & SF_FORMAT_TYPEMASK) == SF_FORMAT_WAV)) {
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
            if((mInfo.format & SF_FORMAT_TYPEMASK) == SF_FORMAT_AIFF)
               len = wxUINT32_SWAP_ON_LE(len);

            if (wxStricmp(id, "ID3 ") != 0) {  // must be case insensitive
               f.Seek(len + (len & 0x01), wxFromCurrent);
               continue;
            }


            id3_tag_holder tp;
            {
               ArrayOf<id3_byte_t> buffer{ len };
               if (!buffer) {
                  break;
               }

               f.Read(buffer.get(), len);
               tp.reset( id3_tag_parse(buffer.get(), len) );
            }

            if (!tp) {
               break;
            }

            // Loop through all frames
            bool have_year = false;
            for (int i = 0; i < (int) tp->nframes; i++) {
               struct id3_frame *frame = tp->frames[i];

               // wxPrintf("ID: %08x '%4s'\n", (int) *(int *)frame->id, frame->id);
               // wxPrintf("Desc: %s\n", frame->description);
               // wxPrintf("Num fields: %d\n", frame->nfields);

               // for (int j = 0; j < (int) frame->nfields; j++) {
               //    wxPrintf("field %d type %d\n", j, frame->fields[j].type );
               //    if (frame->fields[j].type == ID3_FIELD_TYPE_STRINGLIST) {
               //       wxPrintf("num strings %d\n", frame->fields[j].stringlist.nstrings);
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
               else if (strcmp(frame->id, ID3_FRAME_YEAR) == 0) {
                  // LLL:  When libid3tag encounters the "TYER" tag, it converts it to a
                  //       "ZOBS" (obsolete) tag and adds a "TDRC" tag at the end of the
                  //       list of tags using the first 4 characters of the "TYER" tag.
                  //       Since we write both the "TDRC" and "TYER" tags, the "TDRC" tag
                  //       will always be encountered first in the list.  We want use it
                  //       since the converted "TYER" tag may have been truncated.
                  if (have_year) {
                     continue;
                  }
                  n = TAG_YEAR;
                  have_year = true;
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
                     // Is this duplication really needed?
                     MallocString<> convStr{ (char *)id3_ucs4_utf8duplicate(ustr) };
                     n = UTF8CTOWX(convStr.get());
                  }

                  ustr = id3_field_getstring(&frame->fields[2]);
               }
               else if (frame->nfields >= 2) {
                  ustr = id3_field_getstrings(&frame->fields[1], 0);
               }

               if (ustr) {
                  // Is this duplication really needed?
                  MallocString<> convStr{ (char *)id3_ucs4_utf8duplicate(ustr) };
                  v = UTF8CTOWX(convStr.get());
               }

               if (!n.empty() && !v.empty()) {
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

            break;
         }
      }
   }
#endif

   return updateResult;
}

PCMImportFileHandle::~PCMImportFileHandle()
{
}
