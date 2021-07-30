/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportRaw.cpp

  Dominic Mazzoni

*******************************************************************//**

\file ImportRaw.cpp
\brief Functions for guessing audio type and attempting to read from
unknown sample audio data.  Implements ImportRawDialog.

*//****************************************************************//**

\class ImportRawDialog
\brief ImportRawDialog prompts you with options such as endianness
and sample size to help you importing data of an unknown format.

*//*******************************************************************/



#include "ImportRaw.h"

#include "ImportPlugin.h"

#include "../AudioIOBase.h"
#include "../FileFormats.h"
#include "Prefs.h"
#include "../ProjectSettings.h"
#include "../SelectFile.h"
#include "../ShuttleGui.h"
#include "UserException.h"
#include "../WaveTrack.h"
#include "../widgets/ProgressDialog.h"

#include <cmath>
#include <cstdio>
#include <stdint.h>
#include <vector>

#include <wx/crt.h>
#include <wx/defs.h>
#include <wx/button.h>
#include <wx/choice.h>
#include <wx/combobox.h>
#include <wx/intl.h>
#include <wx/panel.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/timer.h>

// #include "RawAudioGuess.h"
#include "FormatClassifier.h"

#include "sndfile.h"

class ImportRawDialog final : public wxDialogWrapper {

  public:
   ImportRawDialog(wxWindow * parent,
                   int encoding, unsigned channels,
                   int offset, double rate);
   ~ImportRawDialog();

   void OnOK(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);
   void OnPlay(wxCommandEvent & event);
   void OnChoice(wxCommandEvent & event);

   // in and out
   int mEncoding;
   unsigned mChannels;
   int mOffset;
   double mRate;
   double mPercent;

 private:

   wxButton   *mOK;
   wxChoice   *mEncodingChoice;
   wxChoice   *mEndianChoice;
   wxChoice   *mChannelChoice;
   wxTextCtrl *mOffsetText;
   wxTextCtrl *mPercentText;
   wxComboBox *mRateText;

   int         mNumEncodings;
   ArrayOf<int> mEncodingSubtype;

   DECLARE_EVENT_TABLE()
};

// This function leaves outTracks empty as an indication of error,
// but may also throw FileException to make use of the application's
// user visible error reporting.
void ImportRaw(const AudacityProject &project, wxWindow *parent, const wxString &fileName,
              WaveTrackFactory *trackFactory, TrackHolders &outTracks)
{
   outTracks.clear();
   int encoding = 0; // Guess Format
   sf_count_t offset = 0;
   double rate = 44100.0;
   double percent = 100.0;
   TrackHolders results;
   auto updateResult = ProgressResult::Success;

   {
      SF_INFO sndInfo;
      unsigned numChannels = 0;

      try {
         // Yes, FormatClassifier currently handles filenames in UTF8 format only, that's
         // a TODO ...
         FormatClassifier theClassifier(fileName.utf8_str());
         encoding = theClassifier.GetResultFormatLibSndfile();
         numChannels = theClassifier.GetResultChannels();
         offset = 0;
      } catch (...) {
         // Something went wrong in FormatClassifier, use defaults instead.
         encoding = 0;
      }

      if (encoding <= 0) {
         // Unable to guess.  Use mono, 16-bit samples with CPU endianness
         // as the default.
         encoding = SF_FORMAT_RAW | SF_ENDIAN_CPU | SF_FORMAT_PCM_16;
         numChannels = 1;
         offset = 0;
      }

      rate = ProjectSettings::Get( project ).GetRate();

      numChannels = std::max(1u, numChannels);
      ImportRawDialog dlog(parent, encoding, numChannels, (int)offset, rate);
      dlog.ShowModal();
      if (!dlog.GetReturnCode())
         return;

      encoding = dlog.mEncoding;
      numChannels = dlog.mChannels;
      rate = dlog.mRate;
      offset = (sf_count_t)dlog.mOffset;
      percent = dlog.mPercent;

      memset(&sndInfo, 0, sizeof(SF_INFO));
      sndInfo.samplerate = (int)rate;
      sndInfo.channels = (int)numChannels;
      sndInfo.format = encoding | SF_FORMAT_RAW;

      wxFile f;   // will be closed when it goes out of scope
      SFFile sndFile;

      if (f.Open(fileName)) {
         // Even though there is an sf_open() that takes a filename, use the one that
         // takes a file descriptor since wxWidgets can open a file with a Unicode name and
         // libsndfile can't (under Windows).
         sndFile.reset(SFCall<SNDFILE*>(sf_open_fd, f.fd(), SFM_READ, &sndInfo, FALSE));
      }

      if (!sndFile){
         char str[1000];
         sf_error_str((SNDFILE *)NULL, str, 1000);
         wxPrintf("%s\n", str);

         throw FileException{ FileException::Cause::Open, fileName };
      }


      {
         int result = sf_command(sndFile.get(), SFC_SET_RAW_START_OFFSET, &offset, sizeof(offset));
         if (result != 0) {
            char str[1000];
            sf_error_str(sndFile.get(), str, 1000);
            wxPrintf("%s\n", str);

            throw FileException{ FileException::Cause::Read, fileName };
         }
      }
      SFCall<sf_count_t>(sf_seek, sndFile.get(), 0, SEEK_SET);

      auto totalFrames =
         // fraction of a sf_count_t value
         (sampleCount)(sndInfo.frames * percent / 100.0);

      //
      // Sample format:
      //
      // In general, go with the user's preferences.  However, if
      // the file is higher-quality, go with a format which preserves
      // the quality of the original file.
      //

      auto format = ImportFileHandle::ChooseFormat(
         sf_subtype_to_effective_format(encoding));

      results.resize(1);
      auto &channels = results[0];
      channels.resize(numChannels);

      {
         // iter not used outside this scope.
         auto iter = channels.begin();
         for (decltype(numChannels) c = 0; c < numChannels; ++iter, ++c)
            *iter = trackFactory->NewWaveTrack(format, rate);
      }
      const auto firstChannel = channels.begin()->get();
      auto maxBlockSize = firstChannel->GetMaxBlockSize();

      SampleBuffer srcbuffer(maxBlockSize * numChannels, format);
      SampleBuffer buffer(maxBlockSize, format);

      decltype(totalFrames) framescompleted = 0;
      if (totalFrames < 0) {
         wxASSERT(false);
         totalFrames = 0;
      }

      auto msg = XO("Importing %s").Format( wxFileName::FileName(fileName).GetFullName() );

      /* i18n-hint: 'Raw' means 'unprocessed' here and should usually be translated.*/
      ProgressDialog progress(XO("Import Raw"), msg);

      size_t block;
      do {
         block =
            limitSampleBufferSize( maxBlockSize, totalFrames - framescompleted );

         sf_count_t sf_result;
         if (format == int16Sample)
            sf_result = SFCall<sf_count_t>(sf_readf_short, sndFile.get(), (short *)srcbuffer.ptr(), block);
         else
            sf_result = SFCall<sf_count_t>(sf_readf_float, sndFile.get(), (float *)srcbuffer.ptr(), block);

         if (sf_result >= 0) {
            block = sf_result;
         }
         else {
            // This is not supposed to happen, sndfile.h says result is always
            // a count, not an invalid value for error
            throw FileException{ FileException::Cause::Read, fileName };
         }

         if (block) {
            auto iter = channels.begin();
            for(decltype(numChannels) c = 0; c < numChannels; ++iter, ++c) {
               if (format==int16Sample) {
                  for(decltype(block) j=0; j<block; j++)
                     ((short *)buffer.ptr())[j] =
                     ((short *)srcbuffer.ptr())[numChannels*j+c];
               }
               else {
                  for(decltype(block) j=0; j<block; j++)
                     ((float *)buffer.ptr())[j] =
                     ((float *)srcbuffer.ptr())[numChannels*j+c];
               }

               iter->get()->Append(buffer.ptr(), (format == int16Sample)?int16Sample:floatSample, block);
            }
            framescompleted += block;
         }

         updateResult = progress.Update(
            framescompleted.as_long_long(),
            totalFrames.as_long_long()
         );
         if (updateResult != ProgressResult::Success)
            break;
         
      } while (block > 0 && framescompleted < totalFrames);
   }

   if (updateResult == ProgressResult::Failed || updateResult == ProgressResult::Cancelled)
      throw UserException{};

   if (!results.empty() && !results[0].empty()) {
      for (const auto &channel : results[0])
         channel->Flush();
      outTracks.swap(results);
   }
}

//
// ImportRawDialog
//

enum {
   ChoiceID = 9000,
   PlayID
};

BEGIN_EVENT_TABLE(ImportRawDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, ImportRawDialog::OnOK)
   EVT_BUTTON(wxID_CANCEL, ImportRawDialog::OnCancel)
   EVT_BUTTON(PlayID, ImportRawDialog::OnPlay)
   EVT_CHOICE(ChoiceID, ImportRawDialog::OnChoice)
END_EVENT_TABLE()

ImportRawDialog::ImportRawDialog(wxWindow * parent,
                                 int encoding, unsigned channels,
                                 int offset, double rate)
:  wxDialogWrapper(parent, wxID_ANY, XO("Import Raw Data"),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
    mEncoding(encoding),
    mChannels(channels),
    mOffset(offset),
    mRate(rate)
{
   wxASSERT(channels >= 1);

   SetName();

   ShuttleGui S(this, eIsCreating);
   TranslatableStrings encodings;
   int num;
   int selection;
   int endian;
   int i;

   num = sf_num_encodings();
   mNumEncodings = 0;
   mEncodingSubtype.reinit(static_cast<size_t>(num));

   selection = 0;
   for (i=0; i<num; i++) {
      SF_INFO info;

      memset(&info, 0, sizeof(SF_INFO));

      int subtype = sf_encoding_index_to_subtype(i);
      info.format = SF_FORMAT_RAW + SF_ENDIAN_LITTLE + subtype;
      info.channels = 1;
      info.samplerate = 44100;

      if (sf_format_check(&info)) {
         mEncodingSubtype[mNumEncodings] = subtype;
         encodings.push_back( Verbatim( sf_encoding_index_name(i) ) );

         if ((mEncoding & SF_FORMAT_SUBMASK) == subtype)
            selection = mNumEncodings;

         mNumEncodings++;
      }
   }

   TranslatableStrings endians{
      /* i18n-hint: Refers to byte-order.  Don't translate "endianness" if you don't
          know the correct technical word. */
      XO("No endianness") ,
      /* i18n-hint: Refers to byte-order.  Don't translate this if you don't
       know the correct technical word. */
      XO("Little-endian") ,
      /* i18n-hint: Refers to byte-order.  Don't translate this if you don't
         know the correct technical word. */
      XO("Big-endian") ,
      /* i18n-hint: Refers to byte-order.  Don't translate "endianness" if you don't
         know the correct technical word. */
      XO("Default endianness") ,
   };

   switch (mEncoding & (SF_FORMAT_ENDMASK))
   {
      default:
      case SF_ENDIAN_FILE:
         endian = 0;
         break;
      case SF_ENDIAN_LITTLE:
         endian = 1;
         break;
      case SF_ENDIAN_BIG:
         endian = 2;
         break;
      case SF_ENDIAN_CPU:
         endian = 3;
         break;
   }

   TranslatableStrings chans{
      XO("1 Channel (Mono)") ,
      XO("2 Channels (Stereo)") ,
   };
   for (i=2; i<16; i++) {
      chans.push_back( XO("%d Channels").Format( i + 1 ) );
   }

   S.StartVerticalLay(false);
   {
      S.SetBorder(5);
      S.StartTwoColumn();
      {
         mEncodingChoice = S.Id(ChoiceID).AddChoice(XXO("Encoding:"),
                                                    encodings,
                                                    selection);
         mEndianChoice = S.Id(ChoiceID).AddChoice(XXO("Byte order:"),
                                                  endians,
                                                  endian);
         mChannelChoice = S.Id(ChoiceID).AddChoice(XXO("Channels:"),
                                                   chans,
                                                   mChannels - 1);
      }
      S.EndTwoColumn();

      S.SetBorder(5);
      S.StartMultiColumn(3);
      {
         // Offset text
         /* i18n-hint: (noun)*/
         mOffsetText = S.AddTextBox(XXO("Start offset:"),
                                    wxString::Format(wxT("%d"), mOffset),
                                    12);
         S.AddUnits(XO("bytes"));

         // Percent text
         mPercentText = S.AddTextBox(XXO("Amount to import:"),
                                     wxT("100"),
                                     12);
         S.AddUnits(XO("%"));

         // Rate text
         wxArrayStringEx rates;
         for (int i = 0; i < AudioIOBase::NumStandardRates; i++) {
            rates.Add(
               wxString::Format(wxT("%d"), AudioIOBase::StandardRates[i]));
         }

         /* i18n-hint: (noun)*/
         mRateText = S.AddCombo(XXO("Sample rate:"),
                                wxString::Format(wxT("%d"), (int)mRate),
                                rates);
         /* i18n-hint: This is the abbreviation for "Hertz", or
            cycles per second. */
         S.AddUnits(XO("Hz"));
      }
      S.EndMultiColumn();

      //
      // Preview Pane goes here
      //

      S.AddStandardButtons();
      // Find the OK button, and change its text to 'Import'.
      // We MUST set mOK because it is used later.
      mOK = (wxButton *)wxWindow::FindWindowById(wxID_OK, this);
      mOK->SetLabel(_("&Import"));
   }
   S.EndVerticalLay();

   Fit();
   SetSizeHints(GetSize());

   Centre(wxBOTH);
}

ImportRawDialog::~ImportRawDialog()
{
}

void ImportRawDialog::OnOK(wxCommandEvent & WXUNUSED(event))
{
   long l;

   mEncoding = mEncodingSubtype[mEncodingChoice->GetSelection()];
   mEncoding += (mEndianChoice->GetSelection() * 0x10000000);
   mChannels = mChannelChoice->GetSelection() + 1;
   mOffsetText->GetValue().ToLong(&l);
   mOffset = l;
   mPercentText->GetValue().ToDouble(&mPercent);
   mRateText->GetValue().ToDouble(&mRate);

   if (mChannels < 1 || mChannels > 16)
      mChannels = 1;
   if (mOffset < 0)
      mOffset = 0;
   if (mPercent < 0.0)
      mPercent = 0.0;
   if (mPercent > 100.0)
      mPercent = 100.0;
   if (mRate < 100.0)
      mRate = 100.0;
   // Highest preset sample rate supported in Audacity 2.3.0 is 384 kHz
   if (mRate > 384000.0)
      mRate = 384000.0;

   EndModal(true);
}

void ImportRawDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   EndModal(false);
}

void ImportRawDialog::OnPlay(wxCommandEvent & WXUNUSED(event))
{
}

void ImportRawDialog::OnChoice(wxCommandEvent & WXUNUSED(event))
{
   SF_INFO info;

   memset(&info, 0, sizeof(SF_INFO));

   mEncoding = mEncodingSubtype[mEncodingChoice->GetSelection()];
   mEncoding += (mEndianChoice->GetSelection() * 0x10000000);

   info.format = mEncoding | SF_FORMAT_RAW;
   info.channels = mChannelChoice->GetSelection() + 1;
   info.samplerate = 44100;

   //mOK = (wxButton *)wxWindow::FindWindowById(wxID_OK, this);
   if (sf_format_check(&info)) {
      mOK->Enable(true);
      return;
   }

   // Try it with 1-channel
   info.channels = 1;
   if (sf_format_check(&info)) {
      mChannelChoice->SetSelection(0);
      mOK->Enable(true);
      return;
   }

   // Otherwise, this is an unsupported format
   mOK->Enable(false);
}
