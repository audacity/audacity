/**********************************************************************

  Audacity: A Digital Audio Editor

  Benchmark.cpp

  Dominic Mazzoni

*******************************************************************//**

\class BenchmarkDialog
\brief BenchmarkDialog is used for measuring performance and accuracy
of the BlockFile system.

*//*******************************************************************/


#include "Audacity.h"
#include "Benchmark.h"

#include <wx/app.h>
#include <wx/log.h>
#include <wx/textctrl.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/dialog.h>
#include <wx/filedlg.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/timer.h>
#include <wx/utils.h>
#include <wx/valgen.h>
#include <wx/valtext.h>
#include <wx/intl.h>

#include "DirManager.h"
#include "ShuttleGui.h"
#include "Project.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "Sequence.h"
#include "Prefs.h"
#include "ProjectSettings.h"
#include "ViewInfo.h"

#include "FileNames.h"
#include "widgets/AudacityMessageBox.h"
#include "widgets/wxPanelWrapper.h"

class BenchmarkDialog final : public wxDialogWrapper
{
public:
   // constructors and destructors
   BenchmarkDialog( wxWindow *parent, const ProjectSettings &settings );

   void MakeBenchmarkDialog();

private:
   // WDR: handler declarations
   void OnRun( wxCommandEvent &event );
   void OnSave( wxCommandEvent &event );
   void OnClear( wxCommandEvent &event );
   void OnClose( wxCommandEvent &event );

   void Printf(const TranslatableString &str);
   void HoldPrint(bool hold);
   void FlushPrint();

   const ProjectSettings &mSettings;

   bool      mHoldPrint;
   wxString  mToPrint;

   wxString  mBlockSizeStr;
   wxString  mDataSizeStr;
   wxString  mNumEditsStr;
   wxString  mRandSeedStr;

   bool      mBlockDetail;
   bool      mEditDetail;

   wxTextCtrl  *mText;

private:
   DECLARE_EVENT_TABLE()
};

void RunBenchmark( wxWindow *parent, const ProjectSettings &settings )
{
   /*
   int action = AudacityMessageBox(
XO("This will close all project windows (without saving)\nand open the Audacity Benchmark dialog.\n\nAre you sure you want to do this?"),
      XO("Benchmark"),
      wxYES_NO | wxICON_EXCLAMATION,
      NULL);

   if (action != wxYES)
      return;

   for ( auto pProject : AllProjects{} )
      GetProjectFrame( *pProject ).Close();
   */

   BenchmarkDialog dlog{ parent, settings };

   dlog.CentreOnParent();

   dlog.ShowModal();
}

//
// BenchmarkDialog
//

enum {
   RunID = 1000,
   BSaveID,
   ClearID,
   StaticTextID,
   BlockSizeID,
   DataSizeID,
   NumEditsID,
   RandSeedID
};

BEGIN_EVENT_TABLE(BenchmarkDialog, wxDialogWrapper)
   EVT_BUTTON( RunID,   BenchmarkDialog::OnRun )
   EVT_BUTTON( BSaveID,  BenchmarkDialog::OnSave )
   EVT_BUTTON( ClearID, BenchmarkDialog::OnClear )
   EVT_BUTTON( wxID_CANCEL, BenchmarkDialog::OnClose )
END_EVENT_TABLE()

BenchmarkDialog::BenchmarkDialog(
   wxWindow *parent, const ProjectSettings &settings)
   :
      /* i18n-hint: Benchmark means a software speed test */
      wxDialogWrapper( parent, 0, XO("Benchmark"),
                wxDefaultPosition, wxDefaultSize,
                wxDEFAULT_DIALOG_STYLE |
                wxRESIZE_BORDER)
   , mSettings{ settings }
{
   SetName();

   mBlockSizeStr = wxT("64");
   mNumEditsStr = wxT("100");
   mDataSizeStr = wxT("32");
   mRandSeedStr = wxT("234657");

   mBlockDetail = false;
   mEditDetail = false;

   HoldPrint(false);

   MakeBenchmarkDialog();
}

// WDR: handler implementations for BenchmarkDialog

void BenchmarkDialog::OnClose(wxCommandEvent & WXUNUSED(event))
{
   EndModal(0);
}

void BenchmarkDialog::MakeBenchmarkDialog()
{
   ShuttleGui S(this, eIsCreating);

   // Strings don't need to be translated because this class doesn't
   // ever get used in a stable release.

   S.StartVerticalLay(true);
   {
      S.SetBorder(8);
      S.StartMultiColumn(4);
      {
         //
         S.Id(BlockSizeID)
            .Validator<wxTextValidator>(wxFILTER_NUMERIC, &mBlockSizeStr)
            .AddTextBox(XO("Disk Block Size (KB):"),
                                             wxT(""),
                                             12);

         //
         S.Id(NumEditsID)
            .Validator<wxTextValidator>(wxFILTER_NUMERIC, &mNumEditsStr)
            .AddTextBox(XO("Number of Edits:"),
                                            wxT(""),
                                            12);

         //
         S.Id(DataSizeID)
            .Validator<wxTextValidator>(wxFILTER_NUMERIC, &mDataSizeStr)
            .AddTextBox(XO("Test Data Size (MB):"),
                                            wxT(""),
                                            12);

         ///
         S.Id(RandSeedID)
            .Validator<wxTextValidator>(wxFILTER_NUMERIC, &mRandSeedStr)
            /* i18n-hint: A "seed" is a number that initializes a
               pseudorandom number generating algorithm */
            .AddTextBox(XO("Random Seed:"),
                                            wxT(""),
                                            12);

      }
      S.EndMultiColumn();

      //
      S.Validator<wxGenericValidator>(&mBlockDetail)
         .AddCheckBox(XO("Show detailed info about each block file"),
                           false);

      //
      S.Validator<wxGenericValidator>(&mEditDetail)
         .AddCheckBox(XO("Show detailed info about each editing operation"),
                           false);

      //
      mText = S.Id(StaticTextID)
         /* i18n-hint noun */
         .Name(XO("Output"))
         .Style( wxTE_MULTILINE | wxTE_READONLY | wxTE_RICH )
         .MinSize( { 500, 200 } )
         .AddTextWindow(wxT(""));

      //
      S.SetBorder(10);
      S.StartHorizontalLay(wxALIGN_LEFT | wxEXPAND, false);
      {
         S.StartHorizontalLay(wxALIGN_LEFT, false);
         {
            S.Id(RunID).AddButton(XO("Run"), wxALIGN_CENTRE, true);
            S.Id(BSaveID).AddButton(XO("Save"));
            /* i18n-hint verb; to empty or erase */
            S.Id(ClearID).AddButton(XO("Clear"));
         }
         S.EndHorizontalLay();

         S.StartHorizontalLay(wxALIGN_CENTER, true);
         {
            // Spacer
         }
         S.EndHorizontalLay();

         S.StartHorizontalLay(wxALIGN_NOT | wxALIGN_LEFT, false);
         {
            /* i18n-hint verb */
            S.Id(wxID_CANCEL).AddButton(XO("Close"));
         }
         S.EndHorizontalLay();
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   Fit();
   SetSizeHints(GetSize());
}

void BenchmarkDialog::OnSave( wxCommandEvent & WXUNUSED(event))
{
/* i18n-hint: Benchmark means a software speed test;
   leave untranslated file extension .txt */
   auto fName = XO("benchmark.txt").Translation();

   fName = FileNames::SelectFile(FileNames::Operation::Export,
      XO("Export Benchmark Data as:"),
      wxEmptyString,
      fName,
      wxT("txt"),
      { FileNames::TextFiles },
      wxFD_SAVE | wxRESIZE_BORDER,
      this);

   if (fName.empty())
      return;

   mText->SaveFile(fName);
}

void BenchmarkDialog::OnClear(wxCommandEvent & WXUNUSED(event))
{
   mText->Clear();
}

void BenchmarkDialog::Printf(const TranslatableString &str)
{
   auto s = str.Translation();
   mToPrint += s;
   if (!mHoldPrint)
      FlushPrint();
}

void BenchmarkDialog::HoldPrint(bool hold)
{
   mHoldPrint = hold;

   if (!mHoldPrint)
      FlushPrint();
}

void BenchmarkDialog::FlushPrint()
{
   while(mToPrint.length() > 100) {
      mText->AppendText(mToPrint.Left(100));
      mToPrint = mToPrint.Right(mToPrint.length() - 100);
   }
   if (mToPrint.length() > 0)
      mText->AppendText(mToPrint);
   mToPrint = wxT("");
}

void BenchmarkDialog::OnRun( wxCommandEvent & WXUNUSED(event))
{
   TransferDataFromWindow();

   if (!Validate())
      return;

   // This code will become part of libaudacity,
   // and this class will be phased out.
   long blockSize, numEdits, dataSize, randSeed;

   mBlockSizeStr.ToLong(&blockSize);
   mNumEditsStr.ToLong(&numEdits);
   mDataSizeStr.ToLong(&dataSize);
   mRandSeedStr.ToLong(&randSeed);

   if (blockSize < 1 || blockSize > 1024) {
      AudacityMessageBox(
         XO("Block size should be in the range 1 - 1024 KB.") );
      return;
   }

   if (numEdits < 1 || numEdits > 10000) {
      AudacityMessageBox(
         XO("Number of edits should be in the range 1 - 10000.") );
      return;
   }

   if (dataSize < 1 || dataSize > 2000) {
      AudacityMessageBox(
         XO("Test data size should be in the range 1 - 2000 MB.") );
      return;
   }

   bool editClipCanMove = true;
   gPrefs->Read(wxT("/GUI/EditClipCanMove"), &editClipCanMove);
   gPrefs->Write(wxT("/GUI/EditClipCanMove"), false);
   gPrefs->Flush();

   // Remember the old blocksize, so that we can restore it later.
   auto oldBlockSize = Sequence::GetMaxDiskBlockSize();
   Sequence::SetMaxDiskBlockSize(blockSize * 1024);

   const auto cleanup = finally( [&] {
      Sequence::SetMaxDiskBlockSize(oldBlockSize);
      gPrefs->Write(wxT("/GUI/EditClipCanMove"), editClipCanMove);
      gPrefs->Flush();
   } );

   wxBusyCursor busy;

   HoldPrint(true);

   ZoomInfo zoomInfo(0.0, ZoomInfo::GetDefaultZoom());
   auto dd = DirManager::Create();
   const auto t =
      TrackFactory{ mSettings, dd, &zoomInfo }.NewWaveTrack(int16Sample);

   t->SetRate(1);

   srand(randSeed);

   size_t nChunks, chunkSize;
   //chunkSize = 7500 + (rand() % 1000);
   chunkSize = 200 + (rand() % 100);
   nChunks = (dataSize * 1048576) / (chunkSize*sizeof(short));
   while(nChunks < 20 || chunkSize > ((unsigned long)(blockSize)*1024)/4) {
      chunkSize = std::max( size_t(1), (chunkSize / 2) + (rand() % 100) );
      nChunks = (dataSize * 1048576) / (chunkSize*sizeof(short));
   }

   // The chunks are the pieces we move around in the test.
   // They are (and are supposed to be) a different size to
   // the blocks that make the blockfiles.  That way we get to
   // do some testing of when edit chunks cross blockfile boundaries.
   Printf( XO("Using %ld chunks of %ld samples each, for a total of %.1f MB.\n")
      .Format( nChunks, chunkSize, nChunks*chunkSize*sizeof(short)/1048576.0 ) );

   int trials = numEdits;

   using Shorts = ArrayOf < short > ;
   Shorts small1{ nChunks };
   Shorts block{ chunkSize };

   Printf( XO("Preparing...\n") );

   wxTheApp->Yield();
   FlushPrint();

   int v;
   int bad;
   int z;
   long elapsed;
   wxString tempStr;
   wxStopWatch timer;

   for (size_t i = 0; i < nChunks; i++) {
      v = short(rand());
      small1[i] = v;
      for (size_t b = 0; b < chunkSize; b++)
         block[b] = v;

      t->Append((samplePtr)block.get(), int16Sample, chunkSize);
   }
   t->Flush();

   // This forces the WaveTrack to flush all of the appends (which is
   // only necessary if you want to access the Sequence class directly,
   // as we're about to do).
   t->GetEndTime();

   if (t->GetClipByIndex(0)->GetSequence()->GetNumSamples() != nChunks * chunkSize) {
      Printf( XO("Expected len %ld, track len %lld.\n")
         .Format(
            nChunks * chunkSize,
            t->GetClipByIndex(0)->GetSequence()->GetNumSamples()
               .as_long_long() ) );
      goto fail;
   }

   Printf( XO("Performing %d edits...\n").Format( trials ) );
   wxTheApp->Yield();
   FlushPrint();

   timer.Start();
   for (z = 0; z < trials; z++) {
      // First chunk to cut
      // 0 <= x0 < nChunks
      const size_t x0 = rand() % nChunks;

      // Number of chunks to cut
      // 1 <= xlen <= nChunks - x0
      const size_t xlen = 1 + (rand() % (nChunks - x0));
      if (mEditDetail)
         Printf( XO("Cut: %ld - %ld \n")
            .Format( x0 * chunkSize, (x0 + xlen) * chunkSize) );

      Track::Holder tmp;
      try {
         tmp = t->Cut(double (x0 * chunkSize), double ((x0 + xlen) * chunkSize));
      }
      catch (const AudacityException&) {
         Printf( XO("Trial %d\n").Format( z ) );
         Printf( XO("Cut (%ld, %ld) failed.\n")
            .Format( (x0 * chunkSize), (x0 + xlen) * chunkSize) );
         Printf( XO("Expected len %ld, track len %lld.\n")
            .Format(
               nChunks * chunkSize,
               t->GetClipByIndex(0)->GetSequence()->GetNumSamples()
                  .as_long_long() ) );
         goto fail;
      }

      // Position to paste
      // 0 <= y0 <= nChunks - xlen
      const size_t y0 = rand() % (nChunks - xlen + 1);

      if (mEditDetail)
         Printf( XO("Paste: %ld\n").Format( y0 * chunkSize ) );

      try {
         t->Paste((double)(y0 * chunkSize), tmp.get());
      }
      catch (const AudacityException&) {
         Printf( XO("Trial %d\nFailed on Paste.\n").Format( z ) );
         goto fail;
      }

      if (t->GetClipByIndex(0)->GetSequence()->GetNumSamples() != nChunks * chunkSize) {
         Printf( XO("Trial %d\n").Format( z ) );
         Printf( XO("Expected len %ld, track len %lld.\n")
            .Format(
               nChunks * chunkSize,
               t->GetClipByIndex(0)->GetSequence()->GetNumSamples()
                  .as_long_long() ) );
         goto fail;
      }

      // Permute small1 correspondingly to the cut and paste
      auto first = &small1[0];
      if (x0 + xlen < nChunks)
         std::rotate( first + x0, first + x0 + xlen, first + nChunks );
      std::rotate( first + y0, first + nChunks - xlen, first + nChunks );
   }

   elapsed = timer.Time();

   if (mBlockDetail) {
      auto seq = t->GetClipByIndex(0)->GetSequence();
      seq->DebugPrintf(seq->GetBlockArray(), seq->GetNumSamples(), &tempStr);
      mToPrint += tempStr;
   }
   Printf( XO("Time to perform %d edits: %ld ms\n").Format( trials, elapsed ) );
   FlushPrint();
   wxTheApp->Yield();


#if 0
   Printf( XO("Checking file pointer leaks:\n") );
   Printf( XO("Track # blocks: %ld\n").Format( t->GetBlockArray()->size() ) );
   Printf( XO("Disk # blocks: \n") );
   system("ls .audacity_temp/* | wc --lines");
#endif

   Printf( XO("Doing correctness check...\n") );
   FlushPrint();
   wxTheApp->Yield();

   bad = 0;
   timer.Start();
   for (size_t i = 0; i < nChunks; i++) {
      v = small1[i];
      t->Get((samplePtr)block.get(), int16Sample, i * chunkSize, chunkSize);
      for (size_t b = 0; b < chunkSize; b++)
         if (block[b] != v) {
            bad++;
            if (bad < 10)
               Printf( XO("Bad: chunk %ld sample %ld\n").Format( i, b ) );
            b = chunkSize;
         }
   }
   if (bad == 0)
      Printf( XO("Passed correctness check!\n") );
   else
      Printf( XO("Errors in %d/%ld chunks\n").Format( bad, nChunks ) );

   elapsed = timer.Time();

   Printf( XO("Time to check all data: %ld ms\n").Format( elapsed ) );
   Printf( XO("Reading data again...\n") );

   wxTheApp->Yield();
   FlushPrint();

   timer.Start();

   for (size_t i = 0; i < nChunks; i++) {
      v = small1[i];
      t->Get((samplePtr)block.get(), int16Sample, i * chunkSize, chunkSize);
      for (size_t b = 0; b < chunkSize; b++)
         if (block[b] != v)
            bad++;
   }

   elapsed = timer.Time();

   Printf( XO("Time to check all data (2): %ld ms\n").Format( elapsed ) );

   Printf( XO("At 44100 Hz, 16-bits per sample, the estimated number of\n simultaneous tracks that could be played at once: %.1f\n" )
      .Format( (nChunks*chunkSize/44100.0)/(elapsed/1000.0) ) );

   goto success;

 fail:
   Printf( XO("TEST FAILED!!!\n") );

 success:

   dd.reset();

   Printf( XO("Benchmark completed successfully.\n") );
   HoldPrint(false);
}
