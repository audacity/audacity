/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMP2.cpp

  Joshua Haberman
  Markus Meyer

  Copyright 2002, 2003 Joshua Haberman.
  Copyright 2006 Markus Meyer
  Some portions may be Copyright 2003 Paolo Patruno.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*******************************************************************//**

\class MP2Exporter
\brief Class used to export MP2 files

*/

#include "../Audacity.h"
#include "ExportMP2.h"

#ifdef USE_LIBTWOLAME

#include <wx/defs.h>
#include <wx/textctrl.h>
#include <wx/dynlib.h>
#include <wx/msgdlg.h>
#include <wx/utils.h>
#include <wx/timer.h>
#include <wx/window.h>
#include <wx/log.h>
#include <wx/intl.h>

#include "Export.h"
#include "../FileIO.h"
#include "../Internat.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../ShuttleGui.h"
#include "../Tags.h"
#include "../Track.h"

#define LIBTWOLAME_STATIC
#include "twolame.h"

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

//----------------------------------------------------------------------------
// ExportMP2Options
//----------------------------------------------------------------------------

static int iBitrates[] = {
   16, 24, 32, 40, 48, 56, 64,
   80, 96, 112, 128, 160,
   192, 224, 256, 320, 384
};

class ExportMP2Options final : public wxPanelWrapper
{
public:
   ExportMP2Options(wxWindow *parent, int format);
   virtual ~ExportMP2Options();

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

private:
   wxArrayString mBitRateNames;
   wxArrayInt    mBitRateLabels;
};

///
///
ExportMP2Options::ExportMP2Options(wxWindow *parent, int WXUNUSED(format))
:  wxPanelWrapper(parent, wxID_ANY)
{
   for (unsigned int i=0; i < (sizeof(iBitrates)/sizeof(int)); i++)
   {
      mBitRateNames.Add(wxString::Format(_("%i kbps"),iBitrates[i]));
      mBitRateLabels.Add(iBitrates[i]);
   }

   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);

   TransferDataToWindow();
}

///
///
ExportMP2Options::~ExportMP2Options()
{
   TransferDataFromWindow();
}

///
///
void ExportMP2Options::PopulateOrExchange(ShuttleGui & S)
{
   S.StartVerticalLay();
   {
      S.StartHorizontalLay(wxCENTER);
      {
         S.StartMultiColumn(2, wxCENTER);
         {
            S.TieChoice(_("Bit Rate:"), wxT("/FileFormats/MP2Bitrate"),
               160, mBitRateNames, mBitRateLabels);
         }
         S.EndMultiColumn();
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();
}

///
///
bool ExportMP2Options::TransferDataToWindow()
{
   return true;
}

///
///
bool ExportMP2Options::TransferDataFromWindow()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   gPrefs->Flush();

   return true;
}

//----------------------------------------------------------------------------
// ExportMP2
//----------------------------------------------------------------------------

class ExportMP2 final : public ExportPlugin
{
public:

   ExportMP2();

   // Required

   wxWindow *OptionsCreate(wxWindow *parent, int format);
   int Export(AudacityProject *project,
               unsigned channels,
               const wxString &fName,
               bool selectedOnly,
               double t0,
               double t1,
               MixerSpec *mixerSpec = NULL,
               const Tags *metadata = NULL,
               int subformat = 0) override;

private:

   int AddTags(AudacityProject *project, char **buffer, bool *endOfFile, const Tags *tags);
#ifdef USE_LIBID3TAG
   void AddFrame(struct id3_tag *tp, const wxString & n, const wxString & v, const char *name);
#endif

};

ExportMP2::ExportMP2()
:  ExportPlugin()
{
   AddFormat();
   SetFormat(wxT("MP2"),0);
   AddExtension(wxT("mp2"),0);
   SetMaxChannels(2,0);
   SetCanMetaData(true,0);
   SetDescription(_("MP2 Files"),0);
}

int ExportMP2::Export(AudacityProject *project,
   unsigned channels, const wxString &fName,
   bool selectionOnly, double t0, double t1, MixerSpec *mixerSpec, const Tags *metadata,
   int WXUNUSED(subformat))
{
   bool stereo = (channels == 2);
   long bitrate = gPrefs->Read(wxT("/FileFormats/MP2Bitrate"), 160);
   double rate = project->GetRate();
   const TrackList *tracks = project->GetTracks();

   wxLogNull logNo;             /* temporarily disable wxWidgets error messages */

   twolame_options *encodeOptions;
   encodeOptions = twolame_init();

   twolame_set_in_samplerate(encodeOptions, (int)(rate + 0.5));
   twolame_set_out_samplerate(encodeOptions, (int)(rate + 0.5));
   twolame_set_bitrate(encodeOptions, bitrate);
   twolame_set_num_channels(encodeOptions, stereo ? 2 : 1);

   if (twolame_init_params(encodeOptions) != 0)
   {
      wxMessageBox(_("Cannot export MP2 with this sample rate and bit rate"),
         _("Error"), wxICON_STOP);
      twolame_close(&encodeOptions);
      return false;
   }

   // Put ID3 tags at beginning of file
   if (metadata == NULL)
      metadata = project->GetTags();

   FileIO outFile(fName, FileIO::Output);
   if (!outFile.IsOpened()) {
      wxMessageBox(_("Unable to open target file for writing"));
      twolame_close(&encodeOptions);
      return false;
   }

   char *id3buffer = NULL;
   int id3len;
   bool endOfFile;
   id3len = AddTags(project, &id3buffer, &endOfFile, metadata);
   if (id3len && !endOfFile)
      outFile.Write(id3buffer, id3len);

   // Values taken from the twolame simple encoder sample
   const int pcmBufferSize = 9216 / 2; // number of samples
   const int mp2BufferSize = 16384; // bytes

   // We allocate a buffer which is twice as big as the
   // input buffer, which should always be enough.
   // We have to multiply by 4 because one sample is 2 bytes wide!
   unsigned char* mp2Buffer = new unsigned char[mp2BufferSize];

   const WaveTrackConstArray waveTracks =
      tracks->GetWaveTrackConstArray(selectionOnly, false);
   int updateResult = eProgressSuccess;
   {
      auto mixer = CreateMixer(waveTracks,
         tracks->GetTimeTrack(),
         t0, t1,
         stereo ? 2 : 1, pcmBufferSize, true,
         rate, int16Sample, true, mixerSpec);

      ProgressDialog progress(wxFileName(fName).GetName(),
         selectionOnly ?
         wxString::Format(_("Exporting selected audio at %ld kbps"), bitrate) :
         wxString::Format(_("Exporting entire file at %ld kbps"), bitrate));

      while (updateResult == eProgressSuccess) {
         auto pcmNumSamples = mixer->Process(pcmBufferSize);

         if (pcmNumSamples == 0)
            break;

         short *pcmBuffer = (short *)mixer->GetBuffer();

         int mp2BufferNumBytes = twolame_encode_buffer_interleaved(
            encodeOptions,
            pcmBuffer,
            pcmNumSamples,
            mp2Buffer,
            mp2BufferSize);

         outFile.Write(mp2Buffer, mp2BufferNumBytes);

         updateResult = progress.Update(mixer->MixGetCurrentTime() - t0, t1 - t0);
      }
   }

   int mp2BufferNumBytes = twolame_encode_flush(
      encodeOptions,
      mp2Buffer,
      mp2BufferSize);

   if (mp2BufferNumBytes > 0)
      outFile.Write(mp2Buffer, mp2BufferNumBytes);

   twolame_close(&encodeOptions);

   delete[] mp2Buffer;

   /* Write ID3 tag if it was supposed to be at the end of the file */

   if (id3len && endOfFile)
      outFile.Write(id3buffer, id3len);

   if (id3buffer) {
   free(id3buffer);
   }

   /* Close file */

   outFile.Close();

   return updateResult;
}

wxWindow *ExportMP2::OptionsCreate(wxWindow *parent, int format)
{
   wxASSERT(parent); // to justify safenew
   return safenew ExportMP2Options(parent, format);
}

// returns buffer len; caller frees
int ExportMP2::AddTags(AudacityProject * WXUNUSED(project), char **buffer, bool *endOfFile, const Tags *tags)
{
#ifdef USE_LIBID3TAG
   struct id3_tag *tp = id3_tag_new();

   for (const auto &pair : tags->GetRange()) {
      const auto &n = pair.first;
      const auto &v = pair.second;
      const char *name = "TXXX";

      if (n.CmpNoCase(TAG_TITLE) == 0) {
         name = ID3_FRAME_TITLE;
      }
      else if (n.CmpNoCase(TAG_ARTIST) == 0) {
         name = ID3_FRAME_ARTIST;
      }
      else if (n.CmpNoCase(TAG_ALBUM) == 0) {
         name = ID3_FRAME_ALBUM;
      }
      else if (n.CmpNoCase(TAG_YEAR) == 0) {
         // LLL:  Some apps do not like the newer frame ID (ID3_FRAME_YEAR),
         //       so we add old one as well.
         AddFrame(tp, n, v, "TYER");
         name = ID3_FRAME_YEAR;
      }
      else if (n.CmpNoCase(TAG_GENRE) == 0) {
         name = ID3_FRAME_GENRE;
      }
      else if (n.CmpNoCase(TAG_COMMENTS) == 0) {
         name = ID3_FRAME_COMMENT;
      }
      else if (n.CmpNoCase(TAG_TRACK) == 0) {
         name = ID3_FRAME_TRACK;
      }

      AddFrame(tp, n, v, name);
   }

   tp->options &= (~ID3_TAG_OPTION_COMPRESSION); // No compression

   // If this version of libid3tag supports it, use v2.3 ID3
   // tags instead of the newer, but less well supported, v2.4
   // that libid3tag uses by default.
   #ifdef ID3_TAG_HAS_TAG_OPTION_ID3V2_3
   tp->options |= ID3_TAG_OPTION_ID3V2_3;
   #endif

   *endOfFile = false;

   id3_length_t len;

   len = id3_tag_render(tp, 0);
   *buffer = (char *)malloc(len);
   len = id3_tag_render(tp, (id3_byte_t *)*buffer);

   id3_tag_delete(tp);

   return len;
#else //ifdef USE_LIBID3TAG
   return 0;
#endif
}

#ifdef USE_LIBID3TAG
void ExportMP2::AddFrame(struct id3_tag *tp, const wxString & n, const wxString & v, const char *name)
{
   struct id3_frame *frame = id3_frame_new(name);

   if (!n.IsAscii() || !v.IsAscii()) {
      id3_field_settextencoding(id3_frame_field(frame, 0), ID3_FIELD_TEXTENCODING_UTF_16);
   }
   else {
      id3_field_settextencoding(id3_frame_field(frame, 0), ID3_FIELD_TEXTENCODING_ISO_8859_1);
   }

   id3_ucs4_t *ucs4 =
      id3_utf8_ucs4duplicate((id3_utf8_t *) (const char *) v.mb_str(wxConvUTF8));

   if (strcmp(name, ID3_FRAME_COMMENT) == 0) {
      // A hack to get around iTunes not recognizing the comment.  The
      // language defaults to XXX and, since it's not a valid language,
      // iTunes just ignores the tag.  So, either set it to a valid language
      // (which one???) or just clear it.  Unfortunately, there's no supported
      // way of clearing the field, so do it directly.
      id3_field *f = id3_frame_field(frame, 1);
      memset(f->immediate.value, 0, sizeof(f->immediate.value));
      id3_field_setfullstring(id3_frame_field(frame, 3), ucs4);
   }
   else if (strcmp(name, "TXXX") == 0) {
      id3_field_setstring(id3_frame_field(frame, 2), ucs4);
      free(ucs4);

      ucs4 = id3_utf8_ucs4duplicate((id3_utf8_t *) (const char *) n.mb_str(wxConvUTF8));

      id3_field_setstring(id3_frame_field(frame, 1), ucs4);
   }
   else {
      id3_field_setstrings(id3_frame_field(frame, 1), 1, &ucs4);
   }

   free(ucs4);

   id3_tag_attachframe(tp, frame);
}
#endif

movable_ptr<ExportPlugin> New_ExportMP2()
{
   return make_movable<ExportMP2>();
}

#endif // #ifdef USE_LIBTWOLAME

