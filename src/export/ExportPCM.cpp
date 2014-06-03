/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportPCM.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/defs.h>

#include <wx/choice.h>
#include <wx/dynlib.h>
#include <wx/filename.h>
#include <wx/intl.h>
#include <wx/timer.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/string.h>
#include <wx/textctrl.h>
#include <wx/window.h>

#include "sndfile.h"

#include "../Audacity.h"
#include "../FileFormats.h"
#include "../Internat.h"
#include "../LabelTrack.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Tags.h"
#include "../Track.h"
#include "../WaveTrack.h"
#include "../ondemand/ODManager.h"

#include "Export.h"
#include "ExportPCM.h"

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
// Statics
//----------------------------------------------------------------------------

static int ReadExportFormatPref()
{
#if defined(__WXMAC__)
   return gPrefs->Read(wxT("/FileFormats/ExportFormat_SF1"),
                       (long int)(SF_FORMAT_AIFF | SF_FORMAT_PCM_16));
#else
   return gPrefs->Read(wxT("/FileFormats/ExportFormat_SF1"),
                       (long int)(SF_FORMAT_WAV | SF_FORMAT_PCM_16));
#endif
}

static void WriteExportFormatPref(int format)
{
   gPrefs->Write(wxT("/FileFormats/ExportFormat_SF1"), (long int)format);
   gPrefs->Flush();
}

//----------------------------------------------------------------------------
// ExportPCMOptions Class
//----------------------------------------------------------------------------

#define ID_HEADER_CHOICE           7102
#define ID_ENCODING_CHOICE         7103

class ExportPCMOptions : public wxDialog
{
public:

   ExportPCMOptions(wxWindow *parent, int format);
   void PopulateOrExchange(ShuttleGui & S);
   void OnHeaderChoice(wxCommandEvent & evt);
   void OnOK(wxCommandEvent& event);

private:

   bool ValidatePair(int format);
   int GetFormat();

private:

   wxArrayString mHeaderNames;
   wxArrayString mEncodingNames;
   wxChoice *mHeaderChoice;
   wxChoice *mEncodingChoice;
   wxButton *mOk;
   int mHeaderFromChoice;
   int mEncodingFromChoice;
   wxArrayInt mEncodingFormats;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(ExportPCMOptions, wxDialog)
   EVT_CHOICE(ID_HEADER_CHOICE,   ExportPCMOptions::OnHeaderChoice)
   EVT_BUTTON(wxID_OK,            ExportPCMOptions::OnOK)
END_EVENT_TABLE()

ExportPCMOptions::ExportPCMOptions(wxWindow * WXUNUSED(parent), int selformat)
:  wxDialog(NULL, wxID_ANY,
            wxString(_("Specify Uncompressed Options")))
{
   mOk = NULL;
   int format = 0;
   switch (selformat)
   {
   case 0:  // other uncompressed
      format = ReadExportFormatPref();
      break;
   case 1:  // 16-bit AIFF
      format = SF_FORMAT_AIFF | SF_FORMAT_PCM_16;
      break;
   case 2:  // 16-bit WAV
      format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;
      break;
   case 3:  // GSM WAV
      format = SF_FORMAT_WAV | SF_FORMAT_GSM610;
      break;
   default: // bug - forgot to extend this case statement
      wxASSERT(false);
      format = ReadExportFormatPref();
      break;
   }
   int i;
   int num;
   int sel;

   num = sf_num_headers();
   sel = 0;
   for (i = 0; i < num; i++) {
      mHeaderNames.Add(sf_header_index_name(i));
      if ((format & SF_FORMAT_TYPEMASK) == (int)sf_header_index_to_type(i))
         sel = i;
   }
   mHeaderFromChoice = sel;

   mEncodingFormats.Clear();
   num = sf_num_encodings();
   mEncodingFromChoice = sel = 0;
   for (i = 0; i < num; i++) {
      int enc = sf_encoding_index_to_subtype(i);
      int fmt = (format & SF_FORMAT_TYPEMASK) | enc;
      bool valid  = ValidatePair(fmt);
      if (valid)
      {

         mEncodingNames.Add(sf_encoding_index_name(i));
         mEncodingFormats.Add(enc);
         if ((format & SF_FORMAT_SUBMASK) == (int)sf_encoding_index_to_subtype(i))
            mEncodingFromChoice = sel;
         else
            sel++;
      }
   }

   ShuttleGui S(this, eIsCreatingFromPrefs);

   PopulateOrExchange(S);

   Layout();
   Fit();
   Center();
}

void ExportPCMOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxEXPAND, true);
   {
      S.StartStatic(_("Uncompressed Export Setup"), true);
      {
         S.StartMultiColumn(2, wxEXPAND);
         {
            S.SetStretchyCol(1);
            mHeaderChoice = S.Id(ID_HEADER_CHOICE)
               .AddChoice(_("Header:"),
                          mHeaderNames[mHeaderFromChoice],
                          &mHeaderNames);
            mEncodingChoice = S.Id(ID_ENCODING_CHOICE)
               .AddChoice(_("Encoding:"),
                          mEncodingNames[mEncodingFromChoice],
                          &mEncodingNames);
         }
         S.EndMultiColumn();
         S.AddFixedText(_("(Not all combinations of headers and encodings are possible.)"));
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();

   S.AddStandardButtons();
   mOk = (wxButton *)wxWindow::FindWindowById(wxID_OK, this);

   return;
}

void ExportPCMOptions::OnHeaderChoice(wxCommandEvent & WXUNUSED(evt))
{
   int format = sf_header_index_to_type(mHeaderChoice->GetSelection());
   mEncodingNames.Clear();
   mEncodingChoice->Clear();
   mEncodingFormats.Clear();
   int sel = wxNOT_FOUND;
   int i,j;

   int sfnum = sf_num_simple_formats();
   wxArrayInt sfs;

   for (i = 0; i < sfnum; i++)
   {
      SF_FORMAT_INFO *fi = sf_simple_format(i);
      sfs.Add(fi->format);
   }

   int num = sf_num_encodings();
   for (i = 0; i < num; i++)
   {
      int enc = sf_encoding_index_to_subtype(i);
      int fmt = format | enc;
      bool valid  = ValidatePair(fmt);
      if (valid)
      {
         mEncodingNames.Add(sf_encoding_index_name(i));
         mEncodingChoice->Append(sf_encoding_index_name(i));
         mEncodingFormats.Add(enc);
         for (j = 0; j < sfnum; j++)
         {
            int enc = sfs[j];
            if ((sel == wxNOT_FOUND) && (fmt == enc))
            {
               sel = mEncodingFormats.GetCount()-1;
               break;
            }
         }
      }
   }

   if (sel == wxNOT_FOUND) sel = 0;
   mEncodingFromChoice = sel;
   mEncodingChoice->SetSelection(sel);
   ValidatePair(GetFormat());
}

void ExportPCMOptions::OnOK(wxCommandEvent& WXUNUSED(event))
{
   WriteExportFormatPref(GetFormat());

   EndModal(wxID_OK);

   return;
}

int ExportPCMOptions::GetFormat()
{
   int hdr = sf_header_index_to_type(mHeaderChoice->GetSelection());
   int sel = mEncodingChoice->GetSelection();
   int enc = mEncodingFormats[sel];
   return hdr | enc;
}

/// Calls a libsndfile library function to determine whether the user's
/// choice of sample encoding (e.g. pcm 16-bit or GSM 6.10 compression)
/// is compatible with their choice of file format (e.g. WAV, AIFF)
/// and enables/disables the OK button accordingly.
bool ExportPCMOptions::ValidatePair(int format)
{
   SF_INFO info;
   memset(&info, 0, sizeof(info));
   info.frames = 0;
   info.samplerate = 44100;
   info.channels = 1;
   info.format = format;
   info.sections = 1;
   info.seekable = 0;

   int valid = sf_format_check(&info);
   if (mOk)
      mOk->Enable(valid != 0 ? true : false);
   return valid != 0 ? true : false;
}

//----------------------------------------------------------------------------
// ExportPCM Class
//----------------------------------------------------------------------------

class ExportPCM : public ExportPlugin
{
public:

   ExportPCM();
   void Destroy();

   // Required

   bool DisplayOptions(wxWindow *parent, int format = 0);
   int Export(AudacityProject *project,
               int channels,
               wxString fName,
               bool selectedOnly,
               double t0,
               double t1,
               MixerSpec *mixerSpec = NULL,
               Tags *metadata = NULL,
               int subformat = 0);
   // optional
   wxString GetExtension(int index = 0);

private:

   char *AdjustString(wxString wxStr, int sf_format);
   bool AddStrings(AudacityProject *project, SNDFILE *sf, Tags *tags, int sf_format);
   void AddID3Chunk(wxString fName, Tags *tags, int sf_format);

};

ExportPCM::ExportPCM()
:  ExportPlugin()
{

   SF_INFO si;

   si.samplerate = 0;
   si.channels = 0;

   int format; // the index of the format we are setting up at the moment

   /* add default user controlled format to list of formats this plug-in does
    * at position 0 */
   format = AddFormat() - 1;  // store the index = 1 less than the count
   SetFormat(wxT("LIBSNDFILE"),format);
   SetCanMetaData(true,format);
   SetDescription(_("Other uncompressed files"),format);
   wxArrayString allext = sf_get_all_extensions();
   wxString wavext = sf_header_extension(SF_FORMAT_WAV);   // get WAV ext.
#if defined(wxMSW)
   // On Windows make sure WAV is at the beginning of the list of all possible
   // extensions for this format
   allext.Remove(wavext);
   allext.Insert(wavext,0);
#endif
   SetExtensions(allext,format);
   SetMaxChannels(255,format);

   /* add AIFF 16-bit to list of sub-formats for this plug-in at position 1 */
   si.format = SF_FORMAT_AIFF | SF_FORMAT_PCM_16;
   for (si.channels = 1; sf_format_check(&si); si.channels++){};
   format = AddFormat() - 1;
   SetFormat(wxT("AIFF"),format);
   SetCanMetaData(true,format);
   SetDescription(_("AIFF (Apple) signed 16 bit PCM"),format);
   wxString aiffext = sf_header_extension(si.format);
   AddExtension(aiffext,format);
   SetMaxChannels(si.channels - 1,format);

   /* add WAV 16-bit at position 2 */
   si.format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;
   for (si.channels = 1; sf_format_check(&si); si.channels++){};
   format = AddFormat() - 1;
   SetFormat(wxT("WAV"),format);
   SetCanMetaData(true,format);
   SetDescription(_("WAV (Microsoft) signed 16 bit PCM"),format);
   // we sorted out wavext near the begining
   AddExtension(wavext,format);
   SetMaxChannels(si.channels - 1,format);

   /* add GSM6.10 to list of formats at position 3 */
   si.format = SF_FORMAT_WAV | SF_FORMAT_GSM610;
   for (si.channels = 1; sf_format_check(&si); si.channels++){};
   format = AddFormat() - 1;
   SetFormat(wxT("GSM610"),format);
   SetCanMetaData(true,format);
   SetDescription(_("GSM 6.10 WAV (mobile)"),format);
   AddExtension(sf_header_extension(si.format),format);
   SetMaxChannels(si.channels - 1,format);
}

void ExportPCM::Destroy()
{
   delete this;
}

/**
 *
 * @param subformat Control whether we are doing a "preset" export to a popular
 * file type, or giving the user full control over libsndfile. Set to 0
 * (default) gives full control, 1 gives 16-bit AIFF, 2 gives 16-bit WAV
 * 3 gives a GSM 6.10 WAV file */
int ExportPCM::Export(AudacityProject *project,
                       int numChannels,
                       wxString fName,
                       bool selectionOnly,
                       double t0,
                       double t1,
                       MixerSpec *mixerSpec,
                       Tags *metadata,
                       int subformat)
{
   double       rate = project->GetRate();
   TrackList   *tracks = project->GetTracks();
   int sf_format;
   switch (subformat)
   {
   case 0:  // other uncompressed
      sf_format = ReadExportFormatPref();
      break;
   case 1:  // AIFF
      sf_format = SF_FORMAT_AIFF | SF_FORMAT_PCM_16;
      break;
   case 2:  // WAV
      sf_format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;
      break;
   case 3:
      sf_format = SF_FORMAT_WAV | SF_FORMAT_GSM610;
      break;
   default: // land here if supplied a sub-format that we don't know about
      wxASSERT(false);  // raise assertion - this is a bug
      sf_format = ReadExportFormatPref(); // treat it like 0 so users get a
      // working result
   break;
   }

   wxString     formatStr;
   SF_INFO      info;
   SNDFILE     *sf = NULL;
   int          err;

   //This whole operation should not occur while a file is being loaded on OD,
   //(we are worried about reading from a file being written to,) so we block.
   //Furthermore, we need to do this because libsndfile is not threadsafe.
   ODManager::LockLibSndFileMutex();
   formatStr = sf_header_name(sf_format & SF_FORMAT_TYPEMASK);

   ODManager::UnlockLibSndFileMutex();

   // Use libsndfile to export file

   info.samplerate = (unsigned int)(rate + 0.5);
   info.frames = (unsigned int)((t1 - t0)*rate + 0.5);
   info.channels = numChannels;
   info.format = sf_format;
   info.sections = 1;
   info.seekable = 0;

   // If we can't export exactly the format they requested,
   // try the default format for that header type...
   if (!sf_format_check(&info))
      info.format = (info.format & SF_FORMAT_TYPEMASK);
   if (!sf_format_check(&info)) {
      wxMessageBox(_("Cannot export audio in this format."));
      return false;
   }

   wxFile f;   // will be closed when it goes out of scope

   if (f.Open(fName, wxFile::write)) {
      // Even though there is an sf_open() that takes a filename, use the one that
      // takes a file descriptor since wxWidgets can open a file with a Unicode name and
      // libsndfile can't (under Windows).
      ODManager::LockLibSndFileMutex();
      sf = sf_open_fd(f.fd(), SFM_WRITE, &info, FALSE);
      //add clipping for integer formats.  We allow floats to clip.
      sf_command(sf, SFC_SET_CLIPPING, NULL,sf_subtype_is_integer(sf_format)?SF_TRUE:SF_FALSE) ;
      ODManager::UnlockLibSndFileMutex();
   }

   if (!sf) {
      wxMessageBox(wxString::Format(_("Cannot export audio to %s"),
                                    fName.c_str()));
      return false;
   }
   // Retrieve tags if not given a set
   if (metadata == NULL)
      metadata = project->GetTags();

    // Install the metata at the beginning of the file (except for
    // WAV and WAVEX formats)
    if ((sf_format & SF_FORMAT_TYPEMASK) != SF_FORMAT_WAV &&
        (sf_format & SF_FORMAT_TYPEMASK) != SF_FORMAT_WAVEX) {
       if (!AddStrings(project, sf, metadata, sf_format)) {
          sf_close(sf);
          return false;
       }
   }

   sampleFormat format;
   if (sf_subtype_more_than_16_bits(info.format))
      format = floatSample;
   else
      format = int16Sample;

   int maxBlockLen = 44100 * 5;

   int updateResult = eProgressSuccess;

   int numWaveTracks;
   WaveTrack **waveTracks;
   tracks->GetWaveTracks(selectionOnly, &numWaveTracks, &waveTracks);
   Mixer *mixer = CreateMixer(numWaveTracks, waveTracks,
                            tracks->GetTimeTrack(),
                            t0, t1,
                            info.channels, maxBlockLen, true,
                            rate, format, true, mixerSpec);

   ProgressDialog *progress = new ProgressDialog(wxFileName(fName).GetName(),
      selectionOnly ?
      wxString::Format(_("Exporting the selected audio as %s"),
                       formatStr.c_str()) :
      wxString::Format(_("Exporting the entire project as %s"),
                       formatStr.c_str()));

   while(updateResult == eProgressSuccess) {
      sampleCount samplesWritten;
      sampleCount numSamples = mixer->Process(maxBlockLen);

      if (numSamples == 0)
         break;

      samplePtr mixed = mixer->GetBuffer();

      ODManager::LockLibSndFileMutex();
      if (format == int16Sample)
         samplesWritten = sf_writef_short(sf, (short *)mixed, numSamples);
      else
         samplesWritten = sf_writef_float(sf, (float *)mixed, numSamples);
      ODManager::UnlockLibSndFileMutex();

      if (samplesWritten != numSamples) {
        char buffer2[1000];
        sf_error_str(sf, buffer2, 1000);
        wxMessageBox(wxString::Format(
           /* i18n-hint: %s will be the error message from libsndfile, which
            * is usually something unhelpful (and untranslated) like "system
            * error" */
           _("Error while writing %s file (disk full?).\nLibsndfile says \"%s\""),
           formatStr.c_str(),
           wxString::FromAscii(buffer2).c_str()));
        break;
      }

      updateResult = progress->Update(mixer->MixGetCurrentTime()-t0, t1-t0);
   }

   delete progress;

   delete mixer;

   delete[] waveTracks;

   // Install the WAV metata in a "LIST" chunk at the end of the file
   if ((sf_format & SF_FORMAT_TYPEMASK) == SF_FORMAT_WAV ||
       (sf_format & SF_FORMAT_TYPEMASK) == SF_FORMAT_WAVEX) {
      if (!AddStrings(project, sf, metadata, sf_format)) {
         sf_close(sf);
         return false;
      }
   }

   ODManager::LockLibSndFileMutex();
   err = sf_close(sf);
   ODManager::UnlockLibSndFileMutex();

   if (err) {
      char buffer[1000];
      sf_error_str(sf, buffer, 1000);
      wxMessageBox(wxString::Format
            /* i18n-hint: %s will be the error message from libsndfile */
                   (_("Error (file may not have been written): %s"),
                    buffer));
   }

   if (((sf_format & SF_FORMAT_TYPEMASK) == SF_FORMAT_AIFF) ||
       ((sf_format & SF_FORMAT_TYPEMASK) == SF_FORMAT_WAV))
      AddID3Chunk(fName, metadata, sf_format);

#ifdef __WXMAC__
   wxFileName fn(fName);
   fn.MacSetTypeAndCreator(sf_header_mactype(sf_format & SF_FORMAT_TYPEMASK),
                           AUDACITY_CREATOR);
#endif

   return updateResult;
}

char *ExportPCM::AdjustString(const wxString wxStr, int sf_format)
{
   bool b_aiff = false;
   if ((sf_format & SF_FORMAT_TYPEMASK) == SF_FORMAT_AIFF)
         b_aiff = true;    // Apple AIFF file

   // We must convert the string to 7 bit ASCII
   size_t  sz = wxStr.length();
   if(sz == 0)
      return NULL;
   // Size for secure malloc in case of local wide char usage
   size_t  sr = (sz+4) * 2;

   char *pDest = (char *)malloc(sr);
   if (!pDest)
      return NULL;
   char *pSrc = (char *)malloc(sr);
   if (!pSrc)
   {
      free(pDest);
      return NULL;
   }
   memset(pDest, 0, sr);
   memset(pSrc, 0, sr);

   if(wxStr.mb_str(wxConvISO8859_1))
      strncpy(pSrc, wxStr.mb_str(wxConvISO8859_1), sz);
   else if(wxStr.mb_str())
      strncpy(pSrc, wxStr.mb_str(), sz);
   else {
      free(pDest);
      free(pSrc);
      return NULL;
   }

   char *pD = pDest;
   char *pS = pSrc;
   unsigned char c;

   // ISO Latin to 7 bit ascii conversion table (best approximation)
   static char aASCII7Table[256] = {
      0x00, 0x5f, 0x5f, 0x5f, 0x5f, 0x5f, 0x5f, 0x5f,
      0x5f, 0x09, 0x0a, 0x5f, 0x0d, 0x5f, 0x5f, 0x5f,
      0x5f, 0x5f, 0x5f, 0x5f, 0x5f, 0x5f, 0x5f, 0x5f,
      0x5f, 0x5f, 0x5f, 0x5f, 0x5f, 0x5f, 0x5f, 0x5f,
      0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
      0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f,
      0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
      0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f,
      0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
      0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f,
      0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,
      0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f,
      0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
      0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f,
      0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
      0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f,
      0x45, 0x20, 0x2c, 0x53, 0x22, 0x2e, 0x2b, 0x2b,
      0x5e, 0x25, 0x53, 0x28, 0x4f, 0x20, 0x5a, 0x20,
      0x20, 0x27, 0x27, 0x22, 0x22, 0x2e, 0x2d, 0x5f,
      0x22, 0x54, 0x73, 0x29, 0x6f, 0x20, 0x7a, 0x59,
      0x20, 0x21, 0x63, 0x4c, 0x6f, 0x59, 0x7c, 0x53,
      0x22, 0x43, 0x61, 0x22, 0x5f, 0x2d, 0x43, 0x2d,
      0x6f, 0x7e, 0x32, 0x33, 0x27, 0x75, 0x50, 0x27,
      0x2c, 0x31, 0x6f, 0x22, 0x5f, 0x5f, 0x5f, 0x3f,
      0x41, 0x41, 0x41, 0x41, 0x41, 0x41, 0x41, 0x43,
      0x45, 0x45, 0x45, 0x45, 0x49, 0x49, 0x49, 0x49,
      0x44, 0x4e, 0x4f, 0x4f, 0x4f, 0x4f, 0x4f, 0x78,
      0x4f, 0x55, 0x55, 0x55, 0x55, 0x59, 0x70, 0x53,
      0x61, 0x61, 0x61, 0x61, 0x61, 0x61, 0x61, 0x63,
      0x65, 0x65, 0x65, 0x65, 0x69, 0x69, 0x69, 0x69,
      0x64, 0x6e, 0x6f, 0x6f, 0x6f, 0x6f, 0x6f, 0x2f,
      0x6f, 0x75, 0x75, 0x75, 0x75, 0x79, 0x70, 0x79
   };

   size_t i;
   for(i = 0; i < sr; i++) {
      c = (unsigned char) *pS++;
      *pD++ = aASCII7Table[c];
      if(c == 0)
         break;
   }
   *pD = '\0';

   free(pSrc);

   if(b_aiff) {
      int len = (int)strlen(pDest);
      if((len % 2) != 0) {
         // In case of an odd length string, add a space char
         strcat(pDest, " ");
      }
   }

   return pDest;
}

bool ExportPCM::AddStrings(AudacityProject * WXUNUSED(project), SNDFILE *sf, Tags *tags, int sf_format)
{
   if (tags->HasTag(TAG_TITLE)) {
      char * ascii7Str = AdjustString(tags->GetTag(TAG_TITLE), sf_format);
      if (ascii7Str) {
         sf_set_string(sf, SF_STR_TITLE, ascii7Str);
         free(ascii7Str);
      }
   }

   if (tags->HasTag(TAG_ALBUM)) {
      char * ascii7Str = AdjustString(tags->GetTag(TAG_ALBUM), sf_format);
      if (ascii7Str) {
         sf_set_string(sf, SF_STR_ALBUM, ascii7Str);
         free(ascii7Str);
      }
   }

   if (tags->HasTag(TAG_ARTIST)) {
      char * ascii7Str = AdjustString(tags->GetTag(TAG_ARTIST), sf_format);
      if (ascii7Str) {
         sf_set_string(sf, SF_STR_ARTIST, ascii7Str);
         free(ascii7Str);
      }
   }

   if (tags->HasTag(TAG_COMMENTS)) {
      char * ascii7Str = AdjustString(tags->GetTag(TAG_COMMENTS), sf_format);
      if (ascii7Str) {
         sf_set_string(sf, SF_STR_COMMENT, ascii7Str);
         free(ascii7Str);
      }
   }

   if (tags->HasTag(TAG_YEAR)) {
      char * ascii7Str = AdjustString(tags->GetTag(TAG_YEAR), sf_format);
      if (ascii7Str) {
         sf_set_string(sf, SF_STR_DATE, ascii7Str);
         free(ascii7Str);
      }
   }

   if (tags->HasTag(TAG_GENRE)) {
      char * ascii7Str = AdjustString(tags->GetTag(TAG_GENRE), sf_format);
      if (ascii7Str) {
         sf_set_string(sf, SF_STR_GENRE, ascii7Str);
         free(ascii7Str);
      }
   }

   if (tags->HasTag(TAG_COPYRIGHT)) {
      char * ascii7Str = AdjustString(tags->GetTag(TAG_COPYRIGHT), sf_format);
      if (ascii7Str) {
         sf_set_string(sf, SF_STR_COPYRIGHT, ascii7Str);
         free(ascii7Str);
      }
   }

   if (tags->HasTag(TAG_SOFTWARE)) {
      char * ascii7Str = AdjustString(tags->GetTag(TAG_SOFTWARE), sf_format);
      if (ascii7Str) {
         sf_set_string(sf, SF_STR_SOFTWARE, ascii7Str);
         free(ascii7Str);
      }
   }

   if (tags->HasTag(TAG_TRACK)) {
      char * ascii7Str = AdjustString(tags->GetTag(TAG_TRACK), sf_format);
      if (ascii7Str) {
         sf_set_string(sf, SF_STR_TRACKNUMBER, ascii7Str);
         free(ascii7Str);
      }
   }

   return true;
}

void ExportPCM::AddID3Chunk(wxString fName, Tags *tags, int sf_format)
{
#ifdef USE_LIBID3TAG
   struct id3_tag *tp = id3_tag_new();

   wxString n, v;
   for (bool cont = tags->GetFirst(n, v); cont; cont = tags->GetNext(n, v)) {
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
      else if (n.CmpNoCase(wxT("composer")) == 0) {
         name = "TCOM";
      }

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

   tp->options &= (~ID3_TAG_OPTION_COMPRESSION); // No compression

   // If this version of libid3tag supports it, use v2.3 ID3
   // tags instead of the newer, but less well supported, v2.4
   // that libid3tag uses by default.
#ifdef ID3_TAG_HAS_TAG_OPTION_ID3V2_3
   tp->options |= ID3_TAG_OPTION_ID3V2_3;
#endif

   id3_length_t len;

   len = id3_tag_render(tp, 0);
   if (len == 0) {
      id3_tag_delete(tp);
      return;
   }
   if ((len % 2) != 0) len++;   // Length must be even.
   id3_byte_t *buffer = (id3_byte_t *)malloc(len);
   if (buffer == NULL) {
      id3_tag_delete(tp);
      return;
   }
   // Zero all locations, for ending odd UTF16 content
   // correctly, i.e., two '\0's at the end.
   memset(buffer, 0, len);

   id3_tag_render(tp, buffer);

   id3_tag_delete(tp);

   wxFFile f(fName, wxT("r+b"));
   if (f.IsOpened()) {
      wxUint32 sz;

      sz = (wxUint32) len;
      f.SeekEnd(0);
      if ((sf_format & SF_FORMAT_TYPEMASK) == SF_FORMAT_WAV)
         f.Write("id3 ", 4);	// Must be lower case for foobar2000.
      else {
         f.Write("ID3 ", 4);
         sz = wxUINT32_SWAP_ON_LE(sz);
      }
      f.Write(&sz, 4);

      f.Write(buffer, len);

      sz = (wxUint32) f.Tell() - 8;
      if ((sf_format & SF_FORMAT_TYPEMASK) == SF_FORMAT_AIFF)
         sz = wxUINT32_SWAP_ON_LE(sz);

      f.Seek(4);
      f.Write(&sz, 4);

      f.Close();
   }

   free(buffer);
#endif
   return;
}

/** @param format The same information as the subformat argument to the Export
 * method. Controls use of pre-defined export settings.*/
bool ExportPCM::DisplayOptions(wxWindow *parent, int format)
{
   wxString nopt(_("There are no options for this format.\n"));
   /* i18n-hint: This is pointing users at another possible export format in
    * the list. So you should translate the quoted string
    * 'Other uncompressed files' exactly the same as you do the same string
    * when it comes up on it's own.*/
   wxString usepcm(_("If you need more control over the export format please use the 'Other uncompressed files' format."));

   /* actual code - decide what options if any are useful to show */
   if (format == 1)
   {   // 16-bit AIFF
      wxMessageBox(nopt + _("Your file will be exported as a 16-bit AIFF (Apple/SGI) file.\n") + usepcm);
      return true;
   }
   else if (format == 2)
   {  // 16-bit WAV
      wxMessageBox(nopt + _("Your file will be exported as a 16-bit WAV (Microsoft) file.\n") + usepcm);
      return true;
   }
   else if (format == 3)
   {  // GSM WAV
      wxMessageBox(nopt + _("Your file will be exported as a GSM 6.10 WAV file.\n") + usepcm);
      return true;
   }

   // default, full user control
   ExportPCMOptions od(parent,format);
   od.ShowModal();

   return true;
}

wxString ExportPCM::GetExtension(int index)
{
   if (index == 0) {
      // get extension libsndfile thinks is correct for currently selected format
      return sf_header_extension(ReadExportFormatPref());
   }
   else {
      // return the default
      return ExportPlugin::GetExtension(index);
   }
}

ExportPlugin *New_ExportPCM()
{
   return new ExportPCM();
}
