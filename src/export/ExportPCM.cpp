/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportPCM.cpp

  Dominic Mazzoni

**********************************************************************/



#include <wx/defs.h>

#include <wx/app.h>
#include <wx/choice.h>
#include <wx/dynlib.h>
#include <wx/filename.h>
#include <wx/intl.h>
#include <wx/timer.h>
#include <wx/string.h>
#include <wx/textctrl.h>
#include <wx/window.h>

#include "sndfile.h"

#include "Dither.h"
#include "../FileFormats.h"
#include "../Mix.h"
#include "Prefs.h"
#include "ProjectRate.h"
#include "../ShuttleGui.h"
#include "../Tags.h"
#include "../Track.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/ProgressDialog.h"
#include "../widgets/wxWidgetsWindowPlacement.h"
#include "wxFileNameWrapper.h"

#include "Export.h"

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

struct
{
   int format;
   const wxChar *name;
   const TranslatableString desc;
}
static const kFormats[] =
{
#if defined(__WXMAC__)
   {SF_FORMAT_AIFF | SF_FORMAT_PCM_16, wxT("AIFF"),   XO("AIFF (Apple/SGI)")},
#endif
   {SF_FORMAT_WAV | SF_FORMAT_PCM_16,  wxT("WAV"),    XO("WAV (Microsoft)")},
};

enum
{
#if defined(__WXMAC__)
   FMT_AIFF,
#endif
   FMT_WAV,
   FMT_OTHER
};

//----------------------------------------------------------------------------
// Statics
//----------------------------------------------------------------------------

static int LoadOtherFormat(int def = 0)
{
   return gPrefs->Read(wxT("/FileFormats/ExportFormat_SF1"),
                       kFormats[0].format & SF_FORMAT_TYPEMASK);
}

static void SaveOtherFormat(int val)
{
   gPrefs->Write(wxT("/FileFormats/ExportFormat_SF1"), val);
   gPrefs->Flush();
}

static int LoadEncoding(int type)
{
   return gPrefs->Read(wxString::Format(wxT("/FileFormats/ExportFormat_SF1_Type/%s_%x"),
                                        sf_header_shortname(type), type), (long int) 0);
}

static void SaveEncoding(int type, int val)
{
   gPrefs->Write(wxString::Format(wxT("/FileFormats/ExportFormat_SF1_Type/%s_%x"),
                                  sf_header_shortname(type), type), val);
   gPrefs->Flush();
}

//----------------------------------------------------------------------------
// ExportPCMOptions Class
//----------------------------------------------------------------------------

#define ID_HEADER_CHOICE           7102
#define ID_ENCODING_CHOICE         7103

class ExportPCMOptions final : public wxPanelWrapper
{
public:

   ExportPCMOptions(wxWindow *parent, int format);
   virtual ~ExportPCMOptions();

   void PopulateOrExchange(ShuttleGui & S);

   void OnShow(wxShowEvent & evt);
   void OnHeaderChoice(wxCommandEvent & evt);
   void OnEncodingChoice(wxCommandEvent & evt);

private:

   void GetTypes();
   void GetEncodings(int enc = 0);
   void SendSuffixEvent();

private:

   std::vector<int> mHeaderIndexes;
   TranslatableStrings mHeaderNames;
   wxChoice *mHeaderChoice;
   int mHeaderFromChoice;

   std::vector<int> mEncodingIndexes;
   TranslatableStrings mEncodingNames;
   wxChoice *mEncodingChoice;
   int mEncodingFromChoice;

   int mSelFormat;
   int mType;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(ExportPCMOptions, wxPanelWrapper)
   EVT_CHOICE(ID_HEADER_CHOICE, ExportPCMOptions::OnHeaderChoice)
   EVT_CHOICE(ID_ENCODING_CHOICE, ExportPCMOptions::OnEncodingChoice)
END_EVENT_TABLE()

ExportPCMOptions::ExportPCMOptions(wxWindow *parent, int selformat)
:  wxPanelWrapper(parent, wxID_ANY)
{
   // Remember the selection format
   mSelFormat = selformat;

   // Init choices
   mHeaderFromChoice = 0;
   mEncodingFromChoice = 0;

   if (mSelFormat < FMT_OTHER)
   {
      mType = kFormats[selformat].format & SF_FORMAT_TYPEMASK;
      GetEncodings(mType & SF_FORMAT_SUBMASK);   
   }
   else
   {
      GetTypes();
      GetEncodings();
   }

   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);

   parent->Bind(wxEVT_SHOW, &ExportPCMOptions::OnShow, this);
}

ExportPCMOptions::~ExportPCMOptions()
{
   // Save the encoding
   SaveOtherFormat(mType);
   SaveEncoding(mType, sf_encoding_index_to_subtype(mEncodingIndexes[mEncodingFromChoice]));
}

void ExportPCMOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartVerticalLay();
   {
      S.StartHorizontalLay(wxCENTER);
      {
         S.StartMultiColumn(2, wxCENTER);
         {
            S.SetStretchyCol(1);
            if (mSelFormat == FMT_OTHER)
            {
               mHeaderChoice = S.Id(ID_HEADER_CHOICE)
                  .AddChoice(XXO("Header:"),
                             mHeaderNames,
                             mHeaderFromChoice);
            }
            mEncodingChoice = S.Id(ID_ENCODING_CHOICE)
               .AddChoice(XXO("Encoding:"),
                          mEncodingNames,
                          mEncodingFromChoice);
         }
         S.EndMultiColumn();
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   return;
}

void ExportPCMOptions::OnShow(wxShowEvent & evt)
{
   evt.Skip();

   // Since the initial file name may not have the "correct" extension,
   // send off an event to have it changed.  Note that this will be
   // done every time a user changes filters in the dialog.
   if (evt.IsShown())
   {
      SendSuffixEvent();
   }
}

void ExportPCMOptions::OnHeaderChoice(wxCommandEvent & evt)
{
   evt.Skip();

   // Remember new selection
   mHeaderFromChoice = evt.GetInt();

   // Get the type for this selection
   mType = sf_header_index_to_type(mHeaderIndexes[mHeaderFromChoice]);

   // Save the newly selected type
   SaveOtherFormat(mType);

   // Reload the encodings valid for this new type
   GetEncodings();

   // Repopulate the encoding choices
   mEncodingChoice->Clear();
   for (int i = 0, num = mEncodingNames.size(); i < num; ++i)
   {
      mEncodingChoice->AppendString(mEncodingNames[i].StrippedTranslation());
   }

   // Select the desired encoding
   mEncodingChoice->SetSelection(mEncodingFromChoice);

   // Send the event indicating a file suffix change.
   SendSuffixEvent();
}

void ExportPCMOptions::OnEncodingChoice(wxCommandEvent & evt)
{
   evt.Skip();

   // Remember new selection
   mEncodingFromChoice = evt.GetInt();

   // And save it
   SaveEncoding(mType, sf_encoding_index_to_subtype(mEncodingIndexes[mEncodingFromChoice]));
}
 
void ExportPCMOptions::GetTypes()
{
   // Reset arrays
   mHeaderIndexes.clear();
   mHeaderNames.clear();

   // Get the previously saved type. Note that this is ONLY used for
   // the FMT_OTHER ("Other uncompressed files") types.
   int typ = LoadOtherFormat() & SF_FORMAT_TYPEMASK;

   // Rebuild the arrays
   mHeaderFromChoice = 0;
   for (int i = 0, num = sf_num_headers(); i < num; ++i)
   {
      int type = sf_header_index_to_type(i);

      switch (type)
      {
         // On the Mac, do not include in header list
#if defined(__WXMAC__)
         case SF_FORMAT_AIFF:
         break;
#endif

         // Do not include in header list
         case SF_FORMAT_WAV:
         break;

         default:
            // Remember the index if this is the desired type
            if (type == typ)
            {
               mHeaderFromChoice = mHeaderIndexes.size();
            }

            // Store index and name
            mHeaderIndexes.push_back(i);
            mHeaderNames.push_back(Verbatim(sf_header_index_name(i)));
         break;
      }
   }

   // Refresh the current type
   mType = sf_header_index_to_type(mHeaderIndexes[mHeaderFromChoice]);
}
 
void ExportPCMOptions::GetEncodings(int enc)
{
   // Setup for queries
   SF_INFO info = {};
   info.samplerate = 44100;
   info.channels = 1;
   info.sections = 1;

   // Reset arrays
   mEncodingIndexes.clear();
   mEncodingNames.clear();

   // If the encoding wasn't supplied, look it up
   if (!(enc & SF_FORMAT_SUBMASK))
   {
      enc = LoadEncoding(mType);
   }
   enc &= SF_FORMAT_SUBMASK;

   // Fix for Bug 1218 - AIFF with no encoding should default to 16 bit.
   if (mType == SF_FORMAT_AIFF && enc == 0)
   {
      enc = SF_FORMAT_PCM_16;
   }

   // Rebuild the arrays
   mEncodingFromChoice = 0;
   for (int i = 0, num = sf_num_encodings(); i < num; ++i)
   {
      int sub = sf_encoding_index_to_subtype(i);

      // Since we're traversing the subtypes linearly, we have to
      // make sure it can be paired with our current type.
      info.format = mType | sub;
      if (sf_format_check(&info))
      {
         // If this subtype matches our last saved encoding, remember
         // its index so we can set it in the dialog.
         if (sub == enc)
         {
            mEncodingFromChoice = mEncodingIndexes.size();
         }

         // Store index and name
         mEncodingIndexes.push_back(i);
         mEncodingNames.push_back(Verbatim(sf_encoding_index_name(i)));
      }
   }
}

void ExportPCMOptions::SendSuffixEvent()
{
   // Synchronously process a change in suffix.
   wxCommandEvent evt(AUDACITY_FILE_SUFFIX_EVENT, GetId());
   evt.SetEventObject(this);
   evt.SetString(sf_header_extension(mType));
   ProcessWindowEvent(evt);
}

//----------------------------------------------------------------------------
// ExportPCM Class
//----------------------------------------------------------------------------

class ExportPCM final : public ExportPlugin
{
public:

   ExportPCM();

   // Required

   void OptionsCreate(ShuttleGui &S, int format) override;
   ProgressResult Export(AudacityProject *project,
                         std::unique_ptr<ProgressDialog> &pDialog,
                         unsigned channels,
                         const wxFileNameWrapper &fName,
                         bool selectedOnly,
                         double t0,
                         double t1,
                         MixerSpec *mixerSpec = NULL,
                         const Tags *metadata = NULL,
                         int subformat = 0) override;
   // optional
   wxString GetFormat(int index) override;
   FileExtension GetExtension(int index) override;
   unsigned GetMaxChannels(int index) override;

private:
   void ReportTooBigError(wxWindow * pParent);
   ArrayOf<char> AdjustString(const wxString & wxStr, int sf_format);
   bool AddStrings(AudacityProject *project, SNDFILE *sf, const Tags *tags, int sf_format);
   bool AddID3Chunk(
      const wxFileNameWrapper &fName, const Tags *tags, int sf_format);

};

ExportPCM::ExportPCM()
   : ExportPlugin()
{
   int selformat; // the index of the format we are setting up at the moment

   // Add the "special" formats first
   for (size_t i = 0; i < WXSIZEOF(kFormats); ++i)
   {
      selformat = AddFormat() - 1;
      AddExtension(sf_header_extension(kFormats[i].format), selformat);
      SetFormat(kFormats[i].name, selformat);
      SetDescription(kFormats[i].desc, selformat);
      SetCanMetaData(true, selformat);
      SetMaxChannels(255, selformat);
   }

   // Then add the generic libsndfile "format"
   selformat = AddFormat() - 1;     // Matches FMT_OTHER
   SetExtensions(sf_get_all_extensions(), selformat);
   SetFormat(wxT("LIBSNDFILE"), selformat);
   SetDescription(XO("Other uncompressed files"), selformat);
   SetCanMetaData(true, selformat);
   SetMaxChannels(255, selformat);
}

void ExportPCM::ReportTooBigError(wxWindow * pParent)
{
   //Temporary translation hack, to say 'WAV or AIFF' rather than 'WAV'
   auto message =
      XO("You have attempted to Export a WAV or AIFF file which would be greater than 4GB.\n"
      "Audacity cannot do this, the Export was abandoned.");

   BasicUI::ShowErrorDialog( wxWidgetsWindowPlacement{ pParent },
      XO("Error Exporting"), message,
      wxT("Size_limits_for_WAV_and_AIFF_files"));

// This alternative error dialog was to cover the possibility we could not 
// compute the size in advance.
#if 0
   BasicUI::ShowErrorDialog( wxWidgetsWindowPlacement{ pParent },
                  XO("Error Exporting"),
                  XO("Your exported WAV file has been truncated as Audacity cannot export WAV\n"
                    "files bigger than 4GB."),
                  wxT("Size_limits_for_WAV_files"));
#endif
}

/**
 *
 * @param subformat Control whether we are doing a "preset" export to a popular
 * file type, or giving the user full control over libsndfile.
 */
ProgressResult ExportPCM::Export(AudacityProject *project,
                                 std::unique_ptr<ProgressDialog> &pDialog,
                                 unsigned numChannels,
                                 const wxFileNameWrapper &fName,
                                 bool selectionOnly,
                                 double t0,
                                 double t1,
                                 MixerSpec *mixerSpec,
                                 const Tags *metadata,
                                 int subformat)
{
   double rate = ProjectRate::Get( *project ).GetRate();
   const auto &tracks = TrackList::Get( *project );

   // Set a default in case the settings aren't found
   int sf_format;

   switch (subformat)
   {
#if defined(__WXMAC__)
      case FMT_AIFF:
         sf_format = SF_FORMAT_AIFF;
      break;
#endif

      case FMT_WAV:
         sf_format = SF_FORMAT_WAV;
      break;

      default:
         // Retrieve the current format.
         sf_format = LoadOtherFormat();
      break;
   }

   // Prior to v2.4.0, sf_format will include the subtype. If not present,
   // check for the format specific preference.
   if (!(sf_format & SF_FORMAT_SUBMASK))
   {
      sf_format |= LoadEncoding(sf_format);
   }

   // If subtype is still not specified, supply a default.
   if (!(sf_format & SF_FORMAT_SUBMASK))
   {
      sf_format |= SF_FORMAT_PCM_16;
   }

   int fileFormat = sf_format & SF_FORMAT_TYPEMASK;
   
   auto updateResult = ProgressResult::Success;
   {
      wxFile f;   // will be closed when it goes out of scope
      SFFile       sf; // wraps f

      wxString     formatStr;
      SF_INFO      info;
      //int          err;

      //This whole operation should not occur while a file is being loaded on OD,
      //(we are worried about reading from a file being written to,) so we block.
      //Furthermore, we need to do this because libsndfile is not threadsafe.
      formatStr = SFCall<wxString>(sf_header_name, fileFormat);

      // Use libsndfile to export file

      info.samplerate = (unsigned int)(rate + 0.5);
      info.frames = (unsigned int)((t1 - t0)*rate + 0.5);
      info.channels = numChannels;
      info.format = sf_format;
      info.sections = 1;
      info.seekable = 0;

      // Bug 46.  Trap here, as sndfile.c does not trap it properly.
      if( (numChannels != 1) && ((sf_format & SF_FORMAT_SUBMASK) == SF_FORMAT_GSM610) )
      {
         AudacityMessageBox( XO("GSM 6.10 requires mono") );
         return ProgressResult::Cancelled;
      }

      if (sf_format == SF_FORMAT_WAVEX + SF_FORMAT_GSM610) {
         AudacityMessageBox(
            XO("WAVEX and GSM 6.10 formats are not compatible") );
         return ProgressResult::Cancelled;
      }

      // If we can't export exactly the format they requested,
      // try the default format for that header type...
      // 
      // LLL: I don't think this is valid since libsndfile checks
      // for all allowed subtypes explicitly and doesn't provide
      // for an unspecified subtype.
      if (!sf_format_check(&info))
         info.format = (info.format & SF_FORMAT_TYPEMASK);
      if (!sf_format_check(&info)) {
         AudacityMessageBox( XO("Cannot export audio in this format.") );
         return ProgressResult::Cancelled;
      }
      const auto path = fName.GetFullPath();
      if (f.Open(path, wxFile::write)) {
         // Even though there is an sf_open() that takes a filename, use the one that
         // takes a file descriptor since wxWidgets can open a file with a Unicode name and
         // libsndfile can't (under Windows).
         sf.reset(SFCall<SNDFILE*>(sf_open_fd, f.fd(), SFM_WRITE, &info, FALSE));
         //add clipping for integer formats.  We allow floats to clip.
         sf_command(sf.get(), SFC_SET_CLIPPING, NULL, sf_subtype_is_integer(sf_format)?SF_TRUE:SF_FALSE) ;
      }

      if (!sf) {
         AudacityMessageBox( XO("Cannot export audio to %s").Format( path ) );
         return ProgressResult::Cancelled;
      }
      // Retrieve tags if not given a set
      if (metadata == NULL)
         metadata = &Tags::Get( *project );

      // Install the meta data at the beginning of the file (except for
      // WAV and WAVEX formats)
      if (fileFormat != SF_FORMAT_WAV &&
          fileFormat != SF_FORMAT_WAVEX) {
         if (!AddStrings(project, sf.get(), metadata, sf_format)) {
            return ProgressResult::Cancelled;
         }
      }

      sampleFormat format;
      if (sf_subtype_more_than_16_bits(info.format))
         format = floatSample;
      else
         format = int16Sample;

      // Bug 2200
      // Only trap size limit for file types we know have an upper size limit.
      // The error message mentions aiff and wav.
      if( (fileFormat == SF_FORMAT_WAV) ||
          (fileFormat == SF_FORMAT_WAVEX) ||
          (fileFormat == SF_FORMAT_AIFF ))
      {
         float sampleCount = (float)(t1-t0)*rate*info.channels;
         float byteCount = sampleCount * sf_subtype_bytes_per_sample( info.format);
         // Test for 4 Gibibytes, rather than 4 Gigabytes
         if( byteCount > 4.295e9)
         {
            ReportTooBigError( wxTheApp->GetTopWindow() );
            return ProgressResult::Failed;
         }
      }
      size_t maxBlockLen = 44100 * 5;

      {
         std::vector<char> dither;
         if ((info.format & SF_FORMAT_SUBMASK) == SF_FORMAT_PCM_24) {
            dither.reserve(maxBlockLen * info.channels * SAMPLE_SIZE(int24Sample));
         }

         wxASSERT(info.channels >= 0);
         auto mixer = CreateMixer(tracks, selectionOnly,
                                  t0, t1,
                                  info.channels, maxBlockLen, true,
                                  rate, format, mixerSpec);

         InitProgress( pDialog, fName,
            (selectionOnly
               ? XO("Exporting the selected audio as %s")
               : XO("Exporting the audio as %s"))
               .Format( formatStr ) );
         auto &progress = *pDialog;

         while (updateResult == ProgressResult::Success) {
            sf_count_t samplesWritten;
            size_t numSamples = mixer->Process(maxBlockLen);

            if (numSamples == 0)
               break;

            auto mixed = mixer->GetBuffer();

            // Bug 1572: Not ideal, but it does add the desired dither
            if ((info.format & SF_FORMAT_SUBMASK) == SF_FORMAT_PCM_24) {
               for (int c = 0; c < info.channels; ++c) {
                  CopySamples(
                     mixed + (c * SAMPLE_SIZE(format)), format,
                     dither.data() + (c * SAMPLE_SIZE(int24Sample)), int24Sample,
                     numSamples, gHighQualityDither, info.channels, info.channels
                  );
                  // Copy back without dither
                  CopySamples(
                     dither.data() + (c * SAMPLE_SIZE(int24Sample)), int24Sample,
                     const_cast<samplePtr>(mixed) // PRL fix this!
                        + (c * SAMPLE_SIZE(format)), format,
                     numSamples, DitherType::none, info.channels, info.channels);
               }
            }

            if (format == int16Sample)
               samplesWritten = SFCall<sf_count_t>(sf_writef_short, sf.get(), (const short *)mixed, numSamples);
            else
               samplesWritten = SFCall<sf_count_t>(sf_writef_float, sf.get(), (const float *)mixed, numSamples);

            if (static_cast<size_t>(samplesWritten) != numSamples) {
               char buffer2[1000];
               sf_error_str(sf.get(), buffer2, 1000);
               //Used to give this error message
#if 0
               AudacityMessageBox(
                  XO(
                  /* i18n-hint: %s will be the error message from libsndfile, which
                   * is usually something unhelpful (and untranslated) like "system
                   * error" */
"Error while writing %s file (disk full?).\nLibsndfile says \"%s\"")
                     .Format( formatStr, wxString::FromAscii(buffer2) ));
#else
               // But better to give the same error message as for
               // other cases of disk exhaustion.
               // The thrown exception doesn't escape but GuardedCall
               // will enqueue a message.
               GuardedCall([&fName]{
                  throw FileException{
                     FileException::Cause::Write, fName }; });
#endif
               updateResult = ProgressResult::Cancelled;
               break;
            }
            
            updateResult = progress.Update(mixer->MixGetCurrentTime() - t0, t1 - t0);
         }
      }
      
      // Install the WAV metata in a "LIST" chunk at the end of the file
      if (updateResult == ProgressResult::Success ||
          updateResult == ProgressResult::Stopped) {
         if (fileFormat == SF_FORMAT_WAV ||
             fileFormat == SF_FORMAT_WAVEX) {
            if (!AddStrings(project, sf.get(), metadata, sf_format)) {
               // TODO: more precise message
               ShowExportErrorDialog("PCM:675");
               return ProgressResult::Cancelled;
            }
         }
         if (0 != sf.close()) {
            // TODO: more precise message
            ShowExportErrorDialog("PCM:681");
            return ProgressResult::Cancelled;
         }
      }
   }

   if (updateResult == ProgressResult::Success ||
       updateResult == ProgressResult::Stopped)
      if ((fileFormat == SF_FORMAT_AIFF) ||
          (fileFormat == SF_FORMAT_WAV))
         // Note: file has closed, and gets reopened and closed again here:
         if (!AddID3Chunk(fName, metadata, sf_format) ) {
            // TODO: more precise message
            ShowExportErrorDialog("PCM:694");
            return ProgressResult::Cancelled;
         }

   return updateResult;
}

ArrayOf<char> ExportPCM::AdjustString(const wxString & wxStr, int sf_format)
{
   bool b_aiff = false;
   if ((sf_format & SF_FORMAT_TYPEMASK) == SF_FORMAT_AIFF)
         b_aiff = true;    // Apple AIFF file

   // We must convert the string to 7 bit ASCII
   size_t  sz = wxStr.length();
   if(sz == 0)
      return {};
   // Size for secure allocation in case of local wide char usage
   size_t  sr = (sz+4) * 2;

   ArrayOf<char> pDest{ sr, true };
   if (!pDest)
      return {};
   ArrayOf<char> pSrc{ sr, true };
   if (!pSrc)
      return {};

   if(wxStr.mb_str(wxConvISO8859_1))
      strncpy(pSrc.get(), wxStr.mb_str(wxConvISO8859_1), sz);
   else if(wxStr.mb_str())
      strncpy(pSrc.get(), wxStr.mb_str(), sz);
   else
      return {};

   char *pD = pDest.get();
   char *pS = pSrc.get();
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

   if(b_aiff) {
      int len = (int)strlen(pDest.get());
      if((len % 2) != 0) {
         // In case of an odd length string, add a space char
         strcat(pDest.get(), " ");
      }
   }

   return pDest;
}

bool ExportPCM::AddStrings(AudacityProject * WXUNUSED(project), SNDFILE *sf, const Tags *tags, int sf_format)
{
   if (tags->HasTag(TAG_TITLE)) {
      auto ascii7Str = AdjustString(tags->GetTag(TAG_TITLE), sf_format);
      if (ascii7Str) {
         sf_set_string(sf, SF_STR_TITLE, ascii7Str.get());
      }
   }

   if (tags->HasTag(TAG_ALBUM)) {
      auto ascii7Str = AdjustString(tags->GetTag(TAG_ALBUM), sf_format);
      if (ascii7Str) {
         sf_set_string(sf, SF_STR_ALBUM, ascii7Str.get());
      }
   }

   if (tags->HasTag(TAG_ARTIST)) {
      auto ascii7Str = AdjustString(tags->GetTag(TAG_ARTIST), sf_format);
      if (ascii7Str) {
         sf_set_string(sf, SF_STR_ARTIST, ascii7Str.get());
      }
   }

   if (tags->HasTag(TAG_COMMENTS)) {
      auto ascii7Str = AdjustString(tags->GetTag(TAG_COMMENTS), sf_format);
      if (ascii7Str) {
         sf_set_string(sf, SF_STR_COMMENT, ascii7Str.get());
      }
   }

   if (tags->HasTag(TAG_YEAR)) {
      auto ascii7Str = AdjustString(tags->GetTag(TAG_YEAR), sf_format);
      if (ascii7Str) {
         sf_set_string(sf, SF_STR_DATE, ascii7Str.get());
      }
   }

   if (tags->HasTag(TAG_GENRE)) {
      auto ascii7Str = AdjustString(tags->GetTag(TAG_GENRE), sf_format);
      if (ascii7Str) {
         sf_set_string(sf, SF_STR_GENRE, ascii7Str.get());
      }
   }

   if (tags->HasTag(TAG_COPYRIGHT)) {
      auto ascii7Str = AdjustString(tags->GetTag(TAG_COPYRIGHT), sf_format);
      if (ascii7Str) {
         sf_set_string(sf, SF_STR_COPYRIGHT, ascii7Str.get());
      }
   }

   if (tags->HasTag(TAG_SOFTWARE)) {
      auto ascii7Str = AdjustString(tags->GetTag(TAG_SOFTWARE), sf_format);
      if (ascii7Str) {
         sf_set_string(sf, SF_STR_SOFTWARE, ascii7Str.get());
      }
   }

   if (tags->HasTag(TAG_TRACK)) {
      auto ascii7Str = AdjustString(tags->GetTag(TAG_TRACK), sf_format);
      if (ascii7Str) {
         sf_set_string(sf, SF_STR_TRACKNUMBER, ascii7Str.get());
      }
   }

   return true;
}

#ifdef USE_LIBID3TAG
struct id3_tag_deleter {
   void operator () (id3_tag *p) const { if (p) id3_tag_delete(p); }
};
using id3_tag_holder = std::unique_ptr<id3_tag, id3_tag_deleter>;
#endif

bool ExportPCM::AddID3Chunk(
   const wxFileNameWrapper &fName, const Tags *tags, int sf_format)
{
#ifdef USE_LIBID3TAG
   id3_tag_holder tp { id3_tag_new() };

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

      MallocString<id3_ucs4_t> ucs4{
         id3_utf8_ucs4duplicate((id3_utf8_t *) (const char *) v.mb_str(wxConvUTF8)) };

      if (strcmp(name, ID3_FRAME_COMMENT) == 0) {
         // A hack to get around iTunes not recognizing the comment.  The
         // language defaults to XXX and, since it's not a valid language,
         // iTunes just ignores the tag.  So, either set it to a valid language
         // (which one???) or just clear it.  Unfortunately, there's no supported
         // way of clearing the field, so do it directly.
         id3_field *f = id3_frame_field(frame, 1);
         memset(f->immediate.value, 0, sizeof(f->immediate.value));
         id3_field_setfullstring(id3_frame_field(frame, 3), ucs4.get());
      }
      else if (strcmp(name, "TXXX") == 0) {
         id3_field_setstring(id3_frame_field(frame, 2), ucs4.get());

         ucs4.reset(id3_utf8_ucs4duplicate((id3_utf8_t *) (const char *) n.mb_str(wxConvUTF8)));

         id3_field_setstring(id3_frame_field(frame, 1), ucs4.get());
      }
      else {
         auto addr = ucs4.get();
         id3_field_setstrings(id3_frame_field(frame, 1), 1, &addr);
      }

      id3_tag_attachframe(tp.get(), frame);
   }

   tp->options &= (~ID3_TAG_OPTION_COMPRESSION); // No compression

   // If this version of libid3tag supports it, use v2.3 ID3
   // tags instead of the newer, but less well supported, v2.4
   // that libid3tag uses by default.
#ifdef ID3_TAG_HAS_TAG_OPTION_ID3V2_3
   tp->options |= ID3_TAG_OPTION_ID3V2_3;
#endif

   id3_length_t len;

   len = id3_tag_render(tp.get(), 0);
   if (len == 0)
      return true;

   if ((len % 2) != 0) len++;   // Length must be even.
   ArrayOf<id3_byte_t> buffer { len, true };
   if (buffer == NULL)
      return false;

   // Zero all locations, for ending odd UTF16 content
   // correctly, i.e., two '\0's at the end.

   id3_tag_render(tp.get(), buffer.get());

   wxFFile f(fName.GetFullPath(), wxT("r+b"));
   if (f.IsOpened()) {
      wxUint32 sz;

      sz = (wxUint32) len;
      if (!f.SeekEnd(0))
         return false;
      if ((sf_format & SF_FORMAT_TYPEMASK) == SF_FORMAT_WAV)
         {
            if (4 != f.Write("id3 ", 4))// Must be lower case for foobar2000.
               return false ;
         }
      else {
         if (4 != f.Write("ID3 ", 4))
            return false;
         sz = wxUINT32_SWAP_ON_LE(sz);
      }
      if (4 != f.Write(&sz, 4))
         return false;

      if (len != f.Write(buffer.get(), len))
         return false;

      sz = (wxUint32) f.Tell() - 8;
      if ((sf_format & SF_FORMAT_TYPEMASK) == SF_FORMAT_AIFF)
         sz = wxUINT32_SWAP_ON_LE(sz);

      if (!f.Seek(4))
         return false;
      if (4 != f.Write(&sz, 4))
         return false;

      if (!f.Flush())
         return false;

      if (!f.Close())
         return false;
   }
   else
      return false;
#endif
   return true;
}

void ExportPCM::OptionsCreate(ShuttleGui &S, int format)
{
   switch (format)
   {
#if defined(__WXMAC__)
      case FMT_AIFF:
#endif
      case FMT_WAV:
      case FMT_OTHER:
         S.AddWindow(safenew ExportPCMOptions{ S.GetParent(), format });
      break;

      default:
         ExportPlugin::OptionsCreate(S, format);
      break;
   }
}

wxString ExportPCM::GetFormat(int index)
{
   if (index != FMT_OTHER)
   {
      return ExportPlugin::GetFormat(index);
   }

   // Get the saved type
   int typ = LoadOtherFormat() & SF_FORMAT_TYPEMASK;

   // Return the format name for that type
   return sf_header_shortname(typ);
}

FileExtension ExportPCM::GetExtension(int index)
{
   if (index != FMT_OTHER)
   {
      return ExportPlugin::GetExtension(index);
   }

   // Get the saved type
   int typ = LoadOtherFormat() & SF_FORMAT_TYPEMASK;

   // Return the extension for that type
   return sf_header_extension(typ);
}

unsigned ExportPCM::GetMaxChannels(int index)
{
   SF_INFO si = {};

   if (index < FMT_OTHER)
   {
      si.format = kFormats[index].format;
   }
   else
   {
      // Get the saved type
      si.format = LoadOtherFormat() & SF_FORMAT_TYPEMASK;
      si.format |= LoadEncoding(si.format);
   }

   for (si.channels = 1; sf_format_check(&si); si.channels++)
   {
      // just counting
   }

   // Return the max number of channels
   return si.channels - 1;
}

static Exporter::RegisteredExportPlugin sRegisteredPlugin{ "PCM",
   []{ return std::make_unique< ExportPCM >(); }
};
