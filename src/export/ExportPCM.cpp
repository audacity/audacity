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

#define ID_FORMAT_CHOICE           7101
#define ID_HEADER_CHOICE           7102
#define ID_ENCODING_CHOICE         7103

class ExportPCMOptions : public wxDialog
{
public:

   ExportPCMOptions(wxWindow *parent, int format);
   void PopulateOrExchange(ShuttleGui & S);
   void OnHeaderChoice(wxCommandEvent & evt);
   void OnChoice(wxCommandEvent & event);
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
   EVT_CHOICE(ID_ENCODING_CHOICE, ExportPCMOptions::OnChoice)
   EVT_BUTTON(wxID_OK,            ExportPCMOptions::OnOK)
END_EVENT_TABLE()

/// 
/// 
ExportPCMOptions::ExportPCMOptions(wxWindow *parent, int selformat)
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
      if ((format & SF_FORMAT_TYPEMASK) == sf_header_index_to_type(i)) {
         sel = i;
      }
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
         if ((format & SF_FORMAT_SUBMASK) == sf_encoding_index_to_subtype(i)) {
            mEncodingFromChoice = sel;
         }
         else sel++;
      }
   }

   ShuttleGui S(this, eIsCreatingFromPrefs);

   PopulateOrExchange(S);

   Layout();
   Fit();
   Center();
}
/// 
/// 
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

///
///
void ExportPCMOptions::OnHeaderChoice(wxCommandEvent & evt)
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

///
///
void ExportPCMOptions::OnChoice(wxCommandEvent & event)
{
}

/// 
/// 
void ExportPCMOptions::OnOK(wxCommandEvent& event)
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

   bool AddStrings(AudacityProject *project, SNDFILE *sf, Tags *tags);
   void AddID3Chunk(wxString fName, Tags *tags);

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

   if (!AddStrings(project, sf, metadata)) { // meta data presence check
      sf_close(sf);
      return false;
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

   if ((sf_format & SF_FORMAT_TYPEMASK) == SF_FORMAT_AIFF) {
      AddID3Chunk(fName, metadata);
   }

#ifdef __WXMAC__
   wxFileName fn(fName);
   fn.MacSetTypeAndCreator(sf_header_mactype(sf_format & SF_FORMAT_TYPEMASK),
                           AUDACITY_CREATOR);
#endif
   
   return updateResult;
}

bool ExportPCM::AddStrings(AudacityProject *project, SNDFILE *sf, Tags *tags)
{
   if (tags->HasTag(TAG_TITLE)) {
      sf_set_string(sf, SF_STR_TITLE, tags->GetTag(TAG_TITLE).mb_str(wxConvUTF8));
   }

   if (tags->HasTag(TAG_ARTIST)) {
      sf_set_string(sf, SF_STR_ARTIST, tags->GetTag(TAG_ARTIST).mb_str(wxConvUTF8));
   }

   if (tags->HasTag(TAG_COMMENTS)) {
      sf_set_string(sf, SF_STR_COMMENT, tags->GetTag(TAG_COMMENTS).mb_str(wxConvUTF8));
   }

   if (tags->HasTag(TAG_YEAR)) {
      sf_set_string(sf, SF_STR_DATE, tags->GetTag(TAG_YEAR).mb_str(wxConvUTF8));
   }

   if (tags->HasTag(wxT("Copyright"))) {
      sf_set_string(sf, SF_STR_COPYRIGHT, tags->GetTag(wxT("Copyright")).mb_str(wxConvUTF8));
   }

   if (tags->HasTag(wxT("Software"))) {
      sf_set_string(sf, SF_STR_SOFTWARE, tags->GetTag(wxT("Software")).mb_str(wxConvUTF8));
   }

   return true;
}

void ExportPCM::AddID3Chunk(wxString fName, Tags *tags)
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

   id3_byte_t *buffer = (id3_byte_t *)malloc(len);
   if (buffer == NULL) {
      id3_tag_delete(tp);
      return;
   }

   len = id3_tag_render(tp, buffer);

   id3_tag_delete(tp);

   wxFFile f(fName, wxT("r+b"));
   if (f.IsOpened()) {
      wxUint32 sz;
      
      sz = wxUINT32_SWAP_ON_LE((wxUint32) len);
      f.SeekEnd(0);
      f.Write("ID3 ", 4);
      f.Write(&sz, 4);

      f.Write(buffer, len + (len & 0x01));

      sz = wxUINT32_SWAP_ON_LE((wxUint32) f.Tell() - 8);
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
   if(format == 1)
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
