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


#include "../Audacity.h"

#include "ImportRaw.h"
#include "Import.h"

#include "../DirManager.h"
#include "../FileFormats.h"
#include "../Internat.h"
#include "../Prefs.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"

#include <cmath>
#include <cstdio>
#include <stdint.h>
#include <vector>

#include <wx/defs.h>
#include <wx/button.h>
#include <wx/choice.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/panel.h>
#include <wx/progdlg.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/timer.h>

#include "RawAudioGuess.h"
#include "MultiFormatReader.h"
#include "SpecPowerMeter.h"
#include "FormatClassifier.h"

#include "sndfile.h"

class ImportRawDialog:public wxDialog {

  public:
   ImportRawDialog(wxWindow * parent,
                   int encoding, int channels,
                   int offset, double rate);
   ~ImportRawDialog();

   void OnOK(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);
   void OnPlay(wxCommandEvent & event);
   void OnChoice(wxCommandEvent & event);

   // in and out
   int mEncoding;
   int mChannels;
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
   wxTextCtrl *mRateText;

   int         mNumEncodings;
   int        *mEncodingSubtype;

   DECLARE_EVENT_TABLE()
};

int ImportRaw(wxWindow *parent, wxString fileName,
              TrackFactory *trackFactory, Track ***outTracks)
{
   int encoding = 0; // Guess Format
   int numChannels = 0;
   sampleFormat format;
   sf_count_t offset = 0;
   sampleCount totalFrames;
   double rate = 44100.0;
   double percent = 100.0;
   SNDFILE *sndFile = NULL;
   SF_INFO sndInfo;
   int result;

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

   ImportRawDialog dlog(parent, encoding, numChannels, (int)offset, rate);
   dlog.ShowModal();
   if (!dlog.GetReturnCode())
      return false;

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

   if (f.Open(fileName)) {
      // Even though there is an sf_open() that takes a filename, use the one that
      // takes a file descriptor since wxWidgets can open a file with a Unicode name and
      // libsndfile can't (under Windows).
      sndFile = sf_open_fd(f.fd(), SFM_READ, &sndInfo, FALSE);
   }

   if (!sndFile){
      // TODO: Handle error
      char str[1000];
      sf_error_str((SNDFILE *)NULL, str, 1000);
      printf("%s\n", str);

      return 0;
   }

   result = sf_command(sndFile, SFC_SET_RAW_START_OFFSET, &offset, sizeof(offset));
   if (result != 0) {
      char str[1000];
      sf_error_str(sndFile, str, 1000);
      printf("%s\n", str);
   }

   sf_seek(sndFile, 0, SEEK_SET);

   totalFrames = (sampleCount)(sndInfo.frames * percent / 100.0);

   //
   // Sample format:
   //
   // In general, go with the user's preferences.  However, if
   // the file is higher-quality, go with a format which preserves
   // the quality of the original file.
   //

   format = (sampleFormat)
      gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleFormat"), floatSample);

   if (format != floatSample &&
       sf_subtype_more_than_16_bits(encoding))
      format = floatSample;

   WaveTrack **channels = new WaveTrack *[numChannels];

   int c;
   for (c = 0; c < numChannels; c++) {
      channels[c] = trackFactory->NewWaveTrack(format, rate);

      if (numChannels > 1)
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

   if (numChannels == 2) {
      channels[0]->SetLinked(true);
   }

   sampleCount maxBlockSize = channels[0]->GetMaxBlockSize();
   int updateResult = eProgressSuccess;

   samplePtr srcbuffer = NewSamples(maxBlockSize * numChannels, format);
   samplePtr buffer = NewSamples(maxBlockSize, format);

   sampleCount framescompleted = 0;

   wxString msg;

   msg.Printf(_("Importing %s"), wxFileName::FileName(fileName).GetFullName().c_str());

   /* i18n-hint: 'Raw' means 'unprocessed' here and should usually be tanslated.*/
   ProgressDialog progress(_("Import Raw"), msg);

   long block;
   do {
      block = maxBlockSize;

      if (block + framescompleted > totalFrames)
         block = totalFrames - framescompleted;

      if (format == int16Sample)
         block = sf_readf_short(sndFile, (short *)srcbuffer, block);
      else
         block = sf_readf_float(sndFile, (float *)srcbuffer, block);

      if (block) {
         for(c=0; c<numChannels; c++) {
            if (format==int16Sample) {
               for(int j=0; j<block; j++)
                  ((short *)buffer)[j] =
                     ((short *)srcbuffer)[numChannels*j+c];
            }
            else {
               for(int j=0; j<block; j++)
                  ((float *)buffer)[j] =
                     ((float *)srcbuffer)[numChannels*j+c];
            }

            channels[c]->Append(buffer, format, block);
         }
         framescompleted += block;
      }

      updateResult = progress.Update((wxULongLong_t)framescompleted,
                                   (wxULongLong_t)totalFrames);
      if (updateResult != eProgressSuccess)
         break;

   } while (block > 0 && framescompleted < totalFrames);

   sf_close(sndFile);

   int res = updateResult;
   if (block < 0)
     res = eProgressFailed;

   if (res == eProgressFailed || res == eProgressCancelled) {
      for (c = 0; c < numChannels; c++)
         delete channels[c];
      delete[] channels;

      // It's a shame we can't return proper error code
      return 0;
   }

   for (c = 0; c < numChannels; c++)
      channels[c]->Flush();
   *outTracks = (Track **)channels;

   return numChannels;
}

//
// ImportRawDialog
//

enum {
   ChoiceID = 9000,
   PlayID
};

BEGIN_EVENT_TABLE(ImportRawDialog, wxDialog)
   EVT_BUTTON(wxID_OK, ImportRawDialog::OnOK)
   EVT_BUTTON(wxID_CANCEL, ImportRawDialog::OnCancel)
   EVT_BUTTON(PlayID, ImportRawDialog::OnPlay)
   EVT_CHOICE(ChoiceID, ImportRawDialog::OnChoice)
END_EVENT_TABLE()

ImportRawDialog::ImportRawDialog(wxWindow * parent,
                                 int encoding, int channels,
                                 int offset, double rate)
:  wxDialog(parent, wxID_ANY, _("Import Raw Data"),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
    mEncoding(encoding),
    mChannels(channels),
    mOffset(offset),
    mRate(rate)
{
   ShuttleGui S(this, eIsCreating);
   wxArrayString encodings;
   wxArrayString endians;
   wxArrayString chans;
   int num;
   int selection;
   int endian;
   int i;

   num = sf_num_encodings();
   mNumEncodings = 0;
   mEncodingSubtype = new int[num];

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
         encodings.Add(LAT1CTOWX(sf_encoding_index_name(i)));

         if ((mEncoding & SF_FORMAT_SUBMASK) == subtype)
            selection = mNumEncodings;

         mNumEncodings++;
      }
   }

   /* i18n-hint: Refers to byte-order.  Don't translate "endianness" if you don't
       know the correct technical word. */
   endians.Add(_("No endianness"));
   /* i18n-hint: Refers to byte-order.  Don't translate this if you don't
    know the correct technical word. */
   endians.Add(_("Little-endian"));
   /* i18n-hint: Refers to byte-order.  Don't translate this if you don't
      know the correct technical word. */
   endians.Add(_("Big-endian"));
   /* i18n-hint: Refers to byte-order.  Don't translate "endianness" if you don't
      know the correct technical word. */
   endians.Add(_("Default endianness"));

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

   chans.Add(_("1 Channel (Mono)"));
   chans.Add(_("2 Channels (Stereo)"));
   for (i=2; i<16; i++) {
      chans.Add(wxString::Format(_("%d Channels"), i + 1));
   }

   S.StartVerticalLay(false);
   {
      S.SetBorder(5);
      S.StartTwoColumn();
      {
         mEncodingChoice = S.Id(ChoiceID).AddChoice(_("Encoding:"),
                                                    encodings[selection],
                                                    &encodings);
         mEndianChoice = S.Id(ChoiceID).AddChoice(_("Byte order:"),
                                                  endians[endian],
                                                  &endians);
         mChannelChoice = S.Id(ChoiceID).AddChoice(_("Channels:"),
                                                   chans[mChannels-1],
                                                   &chans);
      }
      S.EndTwoColumn();

      S.SetBorder(5);
      S.StartMultiColumn(3);
      {
         // Offset text
         /* i18n-hint: (noun)*/
         mOffsetText = S.AddTextBox(_("Start offset:"),
                                    wxString::Format(wxT("%d"), mOffset),
                                    12);
         S.AddUnits(_("bytes"));

         // Percent text
         mPercentText = S.AddTextBox(_("Amount to import:"),
                                     wxT("100"),
                                     12);
         S.AddUnits(wxT("%"));

         // Rate text
         /* i18n-hint: (noun)*/
         mRateText = S.AddTextBox(_("Sample rate:"),
                                  wxString::Format(wxT("%d"), (int)mRate),
                                  12);
         /* i18n-hint: This is the abbreviation for "Hertz", or
            cycles per second. */
         S.AddUnits(_("Hz"));
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
   delete[] mEncodingSubtype;
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
   if (mRate > 100000.0)
      mRate = 100000.0;

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
