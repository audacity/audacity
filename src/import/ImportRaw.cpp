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
#include "ImportUtils.h"

#include "AudioIOBase.h"
#include "FileFormats.h"
#include "Prefs.h"
#include "ProjectRate.h"
#include "SelectFile.h"
#include "ShuttleGui.h"
#include "UserException.h"
#include "WaveTrack.h"
#include "ProgressDialog.h"

#include <cmath>
#include <stdint.h>
#include <vector>
#include <algorithm>

#include <wx/crt.h>
#include <wx/defs.h>
#include <wx/button.h>
#include <wx/choice.h>
#include <wx/combobox.h>
#include <wx/panel.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>

// #include "RawAudioGuess.h"
#include "FormatClassifier.h"
#include "sndfile.h"

/// Decoder for 8 kHz, 1-bit CVSD (e.g., Bluetooth SCO) streams.
class CVSDDecoder {
private:
   float mAccumulator = 0.0f;
   float mStepSize = 10.0f;
   const float mMinStep = 10.0f;
   const float mMaxStep = 1000.0f;
   const float mDecay = 0.999f;
   int mHistory = 0;

public:
   void DecodeByte(uint8_t byte, float *outBuffer)
   {
      for (int i = 0; i < 8; ++i) {
         const int bit = (byte >> i) & 1;

         mAccumulator *= 0.999f;

         if (bit == 1)
            mAccumulator += mStepSize;
         else
            mAccumulator -= mStepSize;

         if (mAccumulator > 32767.0f)
            mAccumulator = 32767.0f;
         if (mAccumulator < -32768.0f)
            mAccumulator = -32768.0f;

         mHistory = ((mHistory << 1) | bit) & 0x0F;

         if (mHistory == 0x0F || mHistory == 0x00)
            mStepSize = std::min(mStepSize + 10.0f, mMaxStep);
         else
            mStepSize = std::max(mStepSize * mDecay, mMinStep);

         outBuffer[i] = mAccumulator / 32768.0f;
      }
   }
};

class ImportRawDialog final : public wxDialogWrapper {

public:
   ImportRawDialog(wxWindow * parent, const wxString & fileName);
   ~ImportRawDialog();

   void OnOK(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);
   void OnPlay(wxCommandEvent & event);
   void OnDetect(wxCommandEvent & event);
   void OnChoice(wxCommandEvent & event);

   static int mEncoding;
   static unsigned mChannels;
   static long long mOffset;
   static double mRate;
   static double mPercent;

private:

   wxButton   *mOK;
   wxChoice   *mEncodingChoice;
   wxChoice   *mEndianChoice;
   wxChoice   *mChannelChoice;
   wxTextCtrl *mOffsetText;
   wxTextCtrl *mPercentText;
   wxComboBox *mRateText;

   std::vector<int> mEncodingSubtype;

   wxString mFileName;

   DECLARE_EVENT_TABLE()
};

int ImportRawDialog::mEncoding = SF_FORMAT_RAW | SF_ENDIAN_CPU | SF_FORMAT_PCM_16;
unsigned ImportRawDialog::mChannels = 1;
long long ImportRawDialog::mOffset = 0;
double ImportRawDialog::mRate = 0;
double ImportRawDialog::mPercent = 100.;

void ImportRaw(const AudacityProject &project, wxWindow *parent, const wxString &fileName,
              WaveTrackFactory *trackFactory, TrackHolders &outTracks)
{
   outTracks.clear();

   TrackListHolder trackList;
   auto updateResult = ProgressResult::Success;

   {
      if (ImportRawDialog::mRate < 100.)
         ImportRawDialog::mRate = ProjectRate::Get(project).GetRate();

      ImportRawDialog dlog(parent, fileName);
      dlog.ShowModal();
      if (!dlog.GetReturnCode())
         return;

      const int encoding = dlog.mEncoding;
      unsigned numChannels = dlog.mChannels;
      double rate = dlog.mRate;
      sf_count_t offset = (sf_count_t)dlog.mOffset;
      double percent = dlog.mPercent;

      SF_INFO sndInfo = { 0 };
      sndInfo.samplerate = (int)rate;
      sndInfo.channels = (int)numChannels;
      sndInfo.format = encoding | SF_FORMAT_RAW;

      wxFile f;   
      SFFile sndFile;

      if ((encoding & SF_FORMAT_SUBMASK) == 0x9999) {
         if (!f.Open(fileName)) {
            throw FileException{ FileException::Cause::Open, fileName };
         }
         f.Seek(offset);
      } else {
         if (f.Open(fileName)) {
            sndFile.reset(SFCall<SNDFILE*>(sf_open_fd, f.fd(), SFM_READ, &sndInfo, FALSE));
         }
         
         if (!sndFile) {
            char str[1000];
            sf_error_str((SNDFILE *)NULL, str, 1000);
            wxPrintf("%s\n", str);
            throw FileException{ FileException::Cause::Open, fileName };
         }
         
         int result = sf_command(sndFile.get(), SFC_SET_RAW_START_OFFSET, &offset, sizeof(offset));
         if (result != 0) {
            char str[1000];
            sf_error_str(sndFile.get(), str, 1000);
            wxPrintf("%s\n", str);
            throw FileException{ FileException::Cause::Read, fileName };
         }
         SFCall<sf_count_t>(sf_seek, sndFile.get(), 0, SEEK_SET);
      }

      auto totalFrames = (sampleCount)(sndInfo.frames * percent / 100.0);
      
      if ((encoding & SF_FORMAT_SUBMASK) == 0x9999) {
         wxFileOffset fileLength = 0;
         if (f.IsOpened()) {
            fileLength = f.Length();
         }
         
         if (fileLength > offset) {
            totalFrames = (sampleCount)((fileLength - offset) * 8 * (percent / 100.0));
         } else {
            totalFrames = 0; 
         }
      }

      if (totalFrames < 0) {
         totalFrames = 0;
      }

      const auto format = ImportUtils::ChooseFormat(sf_subtype_to_effective_format(encoding));
      trackList = trackFactory->CreateMany(numChannels, format, rate);
      const auto maxBlockSize = (*trackList->Any<WaveTrack>().begin())->GetMaxBlockSize();

      SampleBuffer srcbuffer(maxBlockSize * numChannels, format);
      SampleBuffer buffer(maxBlockSize, format);

      decltype(totalFrames) framescompleted = 0;

      auto msg = XO("Importing %s").Format( wxFileName::FileName(fileName).GetFullName() );
      ProgressDialog progress(XO("Import Raw"), msg);

   size_t block;
   CVSDDecoder decoder;
   uint8_t rawByte;
   float decodedSamples[8];

      do {
         block = limitSampleBufferSize( maxBlockSize, totalFrames - framescompleted );

         if ((encoding & SF_FORMAT_SUBMASK) == 0x9999) {
            size_t bytesToRead = block / 8;
            if (bytesToRead == 0 && block > 0)
               bytesToRead = 1;

            size_t bytesRead = 0;
            size_t samplesGenerated = 0;

            while (bytesRead < bytesToRead && f.Read(&rawByte, 1) == 1) {
               decoder.DecodeByte(rawByte, decodedSamples);
               for (int s = 0; s < 8; ++s) {
                  for (unsigned c = 0; c < numChannels; ++c) {
                     ((float *)srcbuffer.ptr())[numChannels * samplesGenerated + c] = decodedSamples[s];
                  }
                  ++samplesGenerated;
               }
               ++bytesRead;
            }
            block = samplesGenerated;

         } else {
            sf_count_t sf_result;
            if (format == int16Sample)
               sf_result = SFCall<sf_count_t>(sf_readf_short, sndFile.get(), (short *)srcbuffer.ptr(), block);
            else
               sf_result = SFCall<sf_count_t>(sf_readf_float, sndFile.get(), (float *)srcbuffer.ptr(), block);

            if (sf_result >= 0) {
               block = sf_result;
            } else {
               throw FileException{ FileException::Cause::Read, fileName };
            }
         }

         if (block) {
            size_t c = 0;
            ImportUtils::ForEachChannel(*trackList, [&](auto& channel)
            {
               if ((encoding & SF_FORMAT_SUBMASK) == 0x9999) {
                  for (size_t j = 0; j < block; ++j)
                     ((float *)buffer.ptr())[j] = ((float *)srcbuffer.ptr())[numChannels * j + c];
                  channel.AppendBuffer(buffer.ptr(), floatSample, block, 1, floatSample);
               } else {
                  if (format == int16Sample) {
                     for (size_t j = 0; j < block; ++j)
                        ((short *)buffer.ptr())[j] = ((short *)srcbuffer.ptr())[numChannels * j + c];
                  } else {
                     for (size_t j = 0; j < block; ++j)
                        ((float *)buffer.ptr())[j] = ((float *)srcbuffer.ptr())[numChannels * j + c];
                  }
                  channel.AppendBuffer(buffer.ptr(),
                     ((format == int16Sample) ? int16Sample : floatSample), block,
                     1, sf_subtype_to_effective_format(encoding));
               }
               ++c;
            });
         }
         
         framescompleted += block;
         
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

   ImportUtils::FinalizeImport(outTracks, move(*trackList));
}

static int getEndianChoice(int sfFormat) {
   switch (sfFormat & SF_FORMAT_ENDMASK)
   {
      default:
      case SF_ENDIAN_FILE: return 0;
      case SF_ENDIAN_LITTLE: return 1;
      case SF_ENDIAN_BIG: return 2;
      case SF_ENDIAN_CPU: return 3;
   }
}

enum {
   ChoiceID = 9000,
   PlayID,
   DetectID,
};

BEGIN_EVENT_TABLE(ImportRawDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, ImportRawDialog::OnOK)
   EVT_BUTTON(wxID_CANCEL, ImportRawDialog::OnCancel)
   EVT_BUTTON(PlayID, ImportRawDialog::OnPlay)
   EVT_BUTTON(DetectID, ImportRawDialog::OnDetect)
   EVT_CHOICE(ChoiceID, ImportRawDialog::OnChoice)
END_EVENT_TABLE()

ImportRawDialog::ImportRawDialog(wxWindow * parent, const wxString & fileName)
:  wxDialogWrapper(parent, wxID_ANY, XO("Import Raw Data"),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
   mFileName(fileName)
{
   wxASSERT(0 < mChannels && mChannels <= 16);

   SetName();

   wxFileName wfn{ fileName };
   wxString windowTitle = XO("%s: %s").Format(GetTitle(), wfn.GetFullName()).Translation();
   wxDialog::SetTitle(windowTitle);

   ShuttleGui S(this, eIsCreating);
   TranslatableStrings encodings;

   int num = sf_num_encodings();

   int selection = 0;
   for (int i = 0; i < num; i++) {
      SF_INFO info = { 0 };

      int subtype = sf_encoding_index_to_subtype(i);
      info.format = SF_FORMAT_RAW + SF_ENDIAN_LITTLE + subtype;
      info.channels = 1;
      info.samplerate = 44100;

      if (sf_format_check(&info)) {
         mEncodingSubtype.push_back(subtype);
         encodings.push_back( Verbatim( sf_encoding_index_name(i) ) );

         if ((mEncoding & SF_FORMAT_SUBMASK) == subtype)
            selection = mEncodingSubtype.size() - 1;
      }
   }

   int cvsdSubtype = 0x9999;
   mEncodingSubtype.push_back(cvsdSubtype);
   encodings.push_back( XO("CVSD (Bluetooth Voice)") );
   if ((mEncoding & SF_FORMAT_SUBMASK) == cvsdSubtype) {
      selection = mEncodingSubtype.size() - 1;
   }

   TranslatableStrings endians{
      XO("No endianness") ,
      XO("Little-endian") ,
      XO("Big-endian") ,
      XO("Default endianness") ,
   };

   int endian = getEndianChoice(mEncoding);

   TranslatableStrings chans{
      XO("1 Channel (Mono)") ,
      XO("2 Channels (Stereo)") ,
   };
   for (int i = 2; i < 16; i++) {
      chans.push_back( XO("%d Channels").Format( i + 1 ) );
   }

   S.StartVerticalLay(false);
   {
      S.SetBorder(5);
      S.StartTwoColumn();
      {
         mEncodingChoice = S.Id(ChoiceID).AddChoice(XXO("Encoding:"), encodings, selection);
         mEndianChoice = S.Id(ChoiceID).AddChoice(XXO("Byte order:"), endians, endian);
         mChannelChoice = S.Id(ChoiceID).AddChoice(XXO("Channels:"), chans, mChannels - 1);
      }
      S.EndTwoColumn();

      S.SetBorder(5);
      S.StartMultiColumn(3);
      {
         mOffsetText = S.AddTextBox(XXO("Start offset:"), wxString::Format(wxT("%lld"), (long long)mOffset), 12);
         S.AddUnits(XO("bytes"));

         mPercentText = S.AddTextBox(XXO("Amount to import:"), wxT("100"), 12);
         S.AddUnits(XO("%"));

         wxArrayStringEx rates;
         for (int i = 0; i < AudioIOBase::NumStandardRates; i++) {
            rates.Add(wxString::Format(wxT("%d"), AudioIOBase::StandardRates[i]));
         }

         mRateText = S.AddCombo(XXO("Sample rate:"), wxString::Format(wxT("%d"), (int)mRate), rates);
         S.AddUnits(XO("Hz"));
      }
      S.EndMultiColumn();

      S.SetBorder(5);
      S.StartTwoColumn();
      {
         S.Id(DetectID).AddButton(XXO("Detect"));
         S.AddStandardButtons();
      }
      S.EndTwoColumn();

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
   long long l;

   mEncoding = mEncodingSubtype[mEncodingChoice->GetSelection()];
   mEncoding += (mEndianChoice->GetSelection() * 0x10000000);
   mChannels = mChannelChoice->GetSelection() + 1;
   mOffsetText->GetValue().ToLongLong(&l);
   mOffset = l;
   mPercentText->GetValue().ToDouble(&mPercent);
   mRateText->GetValue().ToDouble(&mRate);

   if (mChannels < 1 || mChannels > 16) mChannels = 1;
   if (mOffset < 0) mOffset = 0;
   if (mPercent < 0.0) mPercent = 0.0;
   if (mPercent > 100.0) mPercent = 100.0;
   if (mRate < 100.0) mRate = 100.0;
   if (mRate > 384000.0) mRate = 384000.0;

   EndModal(true);
}

void ImportRawDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   EndModal(false);
}

void ImportRawDialog::OnPlay(wxCommandEvent & WXUNUSED(event))
{
}

void ImportRawDialog::OnDetect(wxCommandEvent & event)
{
   try {
      FormatClassifier theClassifier(mFileName.utf8_str());
      mEncoding = theClassifier.GetResultFormatLibSndfile();
      mChannels = theClassifier.GetResultChannels();
   } catch (...) {
      return;
   }

   int selection = 0;
   auto iter = std::find(mEncodingSubtype.begin(), mEncodingSubtype.end(), mEncoding & SF_FORMAT_SUBMASK);
   if (iter != mEncodingSubtype.end())   
       selection = std::distance(mEncodingSubtype.begin(), iter);

   int endian = getEndianChoice(mEncoding);

   mEncodingChoice->SetSelection(selection);
   mEndianChoice->SetSelection(endian);
   mChannelChoice->SetSelection(mChannels - 1);

   OnChoice(event);
}

void ImportRawDialog::OnChoice(wxCommandEvent & WXUNUSED(event))
{
   if (mEncodingSubtype[mEncodingChoice->GetSelection()] == 0x9999) {
      mOK->Enable(true); 
      return;
   }

   SF_INFO info;
   memset(&info, 0, sizeof(SF_INFO));

   mEncoding = mEncodingSubtype[mEncodingChoice->GetSelection()];
   mEncoding += (mEndianChoice->GetSelection() * 0x10000000);

   info.format = mEncoding | SF_FORMAT_RAW;
   info.channels = mChannelChoice->GetSelection() + 1;
   info.samplerate = 44100;

   if (sf_format_check(&info)) {
      mOK->Enable(true);
      return;
   }

   info.channels = 1;
   if (sf_format_check(&info)) {
      mChannelChoice->SetSelection(0);
      mOK->Enable(true);
      return;
   }

   mOK->Enable(false);
}