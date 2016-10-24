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

#include <wx/log.h>
#include <wx/textctrl.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/dialog.h>
#include <wx/filedlg.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/timer.h>
#include <wx/utils.h>
#include <wx/valgen.h>
#include <wx/valtext.h>
#include <wx/intl.h>

#include "ShuttleGui.h"
#include "Project.h"
#include "WaveTrack.h"
#include "Sequence.h"
#include "Prefs.h"

#include "FileDialog.h"

class BenchmarkDialog final : public wxDialogWrapper
{
public:
   // constructors and destructors
   BenchmarkDialog( wxWindow *parent );

   void MakeBenchmarkDialog();

private:
   // WDR: handler declarations
   void OnRun( wxCommandEvent &event );
   void OnSave( wxCommandEvent &event );
   void OnClear( wxCommandEvent &event );
   void OnClose( wxCommandEvent &event );

   void Printf(const wxChar *format, ...);
   void HoldPrint(bool hold);
   void FlushPrint();

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

void RunBenchmark(wxWindow *parent)
{
   /*
   int action = wxMessageBox(wxT("This will close all project windows "
                               "(without saving)\n"
                               "and open the Audacity Benchmark dialog.\n\n"
                               "Are you sure you want to do this?"),
                             wxT("Benchmark"),
                             wxYES_NO | wxICON_EXCLAMATION,
                             NULL);

   if (action != wxYES)
      return;

   CloseAllProjects();
   */

   BenchmarkDialog dlog(parent);

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

BenchmarkDialog::BenchmarkDialog(wxWindow *parent):
      wxDialogWrapper( parent, 0, wxT("Benchmark"),
                wxDefaultPosition, wxDefaultSize,
                wxDEFAULT_DIALOG_STYLE |
                wxRESIZE_BORDER)
{
   SetName(GetTitle());

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
   wxControl *item;

   // Strings don't need to be translated because this class doesn't
   // ever get used in a stable release.

   S.StartVerticalLay(true);
   {
      S.SetBorder(8);
      S.StartMultiColumn(4);
      {
         //
         item = S.Id(BlockSizeID).AddTextBox(wxT("Disk Block Size (KB):"),
                                             wxT(""),
                                             12);
         item->SetValidator(wxTextValidator(wxFILTER_NUMERIC,
                                         &mBlockSizeStr));

         //
         item = S.Id(NumEditsID).AddTextBox(wxT("Number of Edits:"),
                                            wxT(""),
                                            12);
         item->SetValidator(wxTextValidator(wxFILTER_NUMERIC,
                                         &mNumEditsStr));

         //
         item = S.Id(DataSizeID).AddTextBox(wxT("Test Data Size (MB):"),
                                            wxT(""),
                                            12);
         item->SetValidator(wxTextValidator(wxFILTER_NUMERIC,
                                         &mDataSizeStr));

         ///
         item = S.Id(RandSeedID).AddTextBox(wxT("Random Seed:"),
                                            wxT(""),
                                            12);
         item->SetValidator(wxTextValidator(wxFILTER_NUMERIC,
                                         &mRandSeedStr));

      }
      S.EndMultiColumn();

      //
      item = S.AddCheckBox(wxT("Show detailed info about each block file"),
                           wxT("false"));
      item->SetValidator(wxGenericValidator(&mBlockDetail));

      //
      item = S.AddCheckBox(wxT("Show detailed info about each editing operation"),
                           wxT("false"));
      item->SetValidator(wxGenericValidator(&mEditDetail));

      //
      mText = S.Id(StaticTextID).AddTextWindow(wxT(""));
      mText->SetName(wxT("Output"));
      mText->SetSizeHints(wxSize(500,200));

      //
      S.SetBorder(10);
      S.StartHorizontalLay(wxALIGN_LEFT | wxEXPAND, false);
      {
         S.StartHorizontalLay(wxALIGN_LEFT, false);
         {
            S.Id(RunID).AddButton(wxT("Run"))->SetDefault();
            S.Id(BSaveID).AddButton(wxT("Save"));
            S.Id(ClearID).AddButton(wxT("Clear"));
         }
         S.EndHorizontalLay();

         S.StartHorizontalLay(wxALIGN_CENTER, true);
         {
            // Spacer
         }
         S.EndHorizontalLay();

         S.StartHorizontalLay(wxALIGN_NOT | wxALIGN_LEFT, false);
         {
            S.Id(wxID_CANCEL).AddButton(wxT("Close"));
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
   wxString fName = wxT("benchmark.txt");

   fName = FileSelector(wxT("Export Benchmark Data As:"),
                        wxEmptyString,
                        fName,
                        wxT("txt"),
                        wxT("*.txt"),
                        wxFD_SAVE | wxRESIZE_BORDER,
                        this);

   if (fName == wxT(""))
      return;

   mText->SaveFile(fName);
}

void BenchmarkDialog::OnClear(wxCommandEvent & WXUNUSED(event))
{
   mText->Clear();
}

void BenchmarkDialog::Printf(const wxChar *format, ...)
{
   va_list argptr;
   va_start(argptr, format);

   wxString s = wxString::FormatV(format, argptr);
   mToPrint += s;
   if (!mHoldPrint)
      FlushPrint();

   va_end(argptr);
}

void BenchmarkDialog::HoldPrint(bool hold)
{
   mHoldPrint = hold;

   if (!mHoldPrint)
      FlushPrint();
}

void BenchmarkDialog::FlushPrint()
{
   while(mToPrint.Length() > 100) {
      mText->AppendText(mToPrint.Left(100));
      mToPrint = mToPrint.Right(mToPrint.Length() - 100);
   }
   if (mToPrint.Length() > 0)
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
      wxMessageBox(wxT("Block size should be in the range 1 - 1024 KB."));
      return;
   }

   if (numEdits < 1 || numEdits > 10000) {
      wxMessageBox(wxT("Number of edits should be in the range 1 - 10000."));
      return;
   }

   if (dataSize < 1 || dataSize > 2000) {
      wxMessageBox(wxT("Test data size should be in the range 1 - 2000 MB."));
      return;
   }

   bool editClipCanMove = true;
   gPrefs->Read(wxT("/GUI/EditClipCanMove"), &editClipCanMove);
   gPrefs->Write(wxT("/GUI/EditClipCanMove"), false);
   gPrefs->Flush();

   // Rememebr the old blocksize, so that we can restore it later.
   auto oldBlockSize = Sequence::GetMaxDiskBlockSize();
   const auto cleanup = finally([=]
      { Sequence::SetMaxDiskBlockSize(oldBlockSize); }
   );
   Sequence::SetMaxDiskBlockSize(blockSize * 1024);

   wxBusyCursor busy;

   HoldPrint(true);

   ZoomInfo zoomInfo(0.0, ZoomInfo::GetDefaultZoom());
   auto dd = std::make_shared<DirManager>();
   const auto t = TrackFactory{ dd, &zoomInfo }.NewWaveTrack(int16Sample);

   t->SetRate(1);

   srand(randSeed);

   int nChunks, chunkSize;
   //chunkSize = 7500 + (rand() % 1000);
   chunkSize = 200 + (rand() % 100);
   nChunks = (dataSize * 1048576) / (chunkSize*sizeof(short));
   while(nChunks < 20 || chunkSize > (blockSize*1024)/4) {
      chunkSize = (chunkSize / 2) + (rand() % 100);
      nChunks = (dataSize * 1048576) / (chunkSize*sizeof(short));
   }

   // The chunks are the pieces we move around in the test.
   // They are (and are supposed to be) a different size to
   // the blocks that make the blockfiles.  That way we get to
   // do some testing of when edit chunks cross blockfile boundaries.
   Printf(wxT("Using %d chunks of %d samples each, for a total of ")
          wxT("%.1f MB.\n"),
          nChunks, chunkSize, nChunks*chunkSize*sizeof(short)/1048576.0);

   int trials = numEdits;

   short *small1 = new short[nChunks];
   short *small2 = new short[nChunks];
   short *block = new short[chunkSize];

   Printf(wxT("Preparing...\n"));

   wxTheApp->Yield();
   FlushPrint();

   int i, b, v;
   int bad;
   int z;
   long elapsed;
   wxString tempStr;
   wxStopWatch timer;

   for (i = 0; i < nChunks; i++) {
      v = short(rand());
      small1[i] = v;
      for (b = 0; b < chunkSize; b++)
         block[b] = v;

      t->Append((samplePtr)block, int16Sample, chunkSize);
   }
   t->Flush();

   // This forces the WaveTrack to flush all of the appends (which is
   // only necessary if you want to access the Sequence class directly,
   // as we're about to do).
   t->GetEndTime();

   if (t->GetClipByIndex(0)->GetSequence()->GetNumSamples() != nChunks * chunkSize) {
      Printf(wxT("Expected len %d, track len %lld.\n"), nChunks * chunkSize,
             t->GetClipByIndex(0)->GetSequence()->GetNumSamples().as_long_long());
      goto fail;
   }

   Printf(wxT("Performing %d edits...\n"), trials);
   wxTheApp->Yield();
   FlushPrint();

   timer.Start();
   for (z = 0; z < trials; z++) {
      int x0 = rand() % nChunks;
      int xlen = 1 + (rand() % (nChunks - x0));
      if (mEditDetail)
         Printf(wxT("Cut: %d - %d \n"), x0 * chunkSize, (x0 + xlen) * chunkSize);

      auto tmp = t->Cut(double (x0 * chunkSize), double ((x0 + xlen) * chunkSize));
      if (!tmp) {
         Printf(wxT("Trial %d\n"), z);
         Printf(wxT("Cut (%d, %d) failed.\n"), (x0 * chunkSize),
                (x0 + xlen) * chunkSize);
         Printf(wxT("Expected len %d, track len %lld.\n"), nChunks * chunkSize,
                t->GetClipByIndex(0)->GetSequence()->GetNumSamples().as_long_long());
         goto fail;
      }

      int y0 = rand() % (nChunks - xlen);
      if (mEditDetail)
         Printf(wxT("Paste: %d\n"), y0 * chunkSize);

      if (!t->Paste((double)(y0 * chunkSize), tmp.get()))
      {
         Printf(wxT("Trial %d\nFailed on Paste.\n"), z);
         goto fail;
      }

      if (t->GetClipByIndex(0)->GetSequence()->GetNumSamples() != nChunks * chunkSize) {
         Printf(wxT("Trial %d\n"), z);
         Printf(wxT("Expected len %d, track len %lld.\n"), nChunks * chunkSize,
                t->GetClipByIndex(0)->GetSequence()->GetNumSamples().as_long_long());
         goto fail;
      }
      // Copy
      for (i = 0; i < xlen; i++)
         small2[i] = small1[x0 + i];
      // Delete
      for (i = 0; i < (nChunks - x0 - xlen); i++)
         small1[x0 + i] = small1[x0 + xlen + i];
      // Insert
      for (i = 0; i < (nChunks - xlen - y0); i++)
         small1[nChunks - i - 1] = small1[nChunks - i - 1 - xlen];
      // Paste
      for (i = 0; i < xlen; i++)
         small1[y0 + i] = small2[i];
   }

   elapsed = timer.Time();

   if (mBlockDetail) {
      t->GetClipByIndex(0)->GetSequence()->DebugPrintf(&tempStr);
      mToPrint += tempStr;
   }
   Printf(wxT("Time to perform %d edits: %ld ms\n"), trials, elapsed);
   FlushPrint();
   wxTheApp->Yield();


#if 0
   Printf(wxT("Checking file pointer leaks:\n"));
   Printf(wxT("Track # blocks: %d\n"), t->GetBlockArray()->Count());
   Printf(wxT("Disk # blocks: \n"));
   system("ls .audacity_temp/* | wc --lines");
#endif

   Printf(wxT("Doing correctness check...\n"));
   FlushPrint();
   wxTheApp->Yield();

   bad = 0;
   timer.Start();
   for (i = 0; i < nChunks; i++) {
      v = small1[i];
      t->Get((samplePtr)block, int16Sample, i * chunkSize, chunkSize);
      for (b = 0; b < chunkSize; b++)
         if (block[b] != v) {
            bad++;
            if (bad < 10)
               Printf(wxT("Bad: chunk %d sample %d\n"), i, b);
            b = chunkSize;
         }
   }
   if (bad == 0)
      Printf(wxT("Passed correctness check!\n"));
   else
      Printf(wxT("Errors in %d/%d chunks\n"), bad, nChunks);

   elapsed = timer.Time();

   Printf(wxT("Time to check all data: %ld ms\n"), elapsed);
   Printf(wxT("Reading data again...\n"));

   wxTheApp->Yield();
   FlushPrint();

   timer.Start();

   for (i = 0; i < nChunks; i++) {
      v = small1[i];
      t->Get((samplePtr)block, int16Sample, i * chunkSize, chunkSize);
      for (b = 0; b < chunkSize; b++)
         if (block[b] != v)
            bad++;
   }

   elapsed = timer.Time();

   Printf(wxT("Time to check all data (2): %ld ms\n"), elapsed);

   Printf(wxT("At 44100 Hz, 16-bits per sample, the estimated number of\n")
          wxT("simultaneous tracks that could be played at once: %.1f\n"),
          (nChunks*chunkSize/44100.0)/(elapsed/1000.0));

   goto success;

 fail:
   Printf(wxT("TEST FAILED!!!\n"));

 success:
   delete[]small1;
   delete[]small2;
   delete[]block;

   dd.reset();

   Printf(wxT("Benchmark completed successfully.\n"));
   HoldPrint(false);

   gPrefs->Write(wxT("/GUI/EditClipCanMove"), editClipCanMove);
   gPrefs->Flush();
}
