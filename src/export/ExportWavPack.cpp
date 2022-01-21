/**********************************************************************

Audacity: A Digital Audio Editor

ExportWavPack.cpp

Subhradeep Chakraborty

This program is distributed under the GNU General Public License, version 2.
A copy of this license is included with this source.

Based on ExportOGG.cpp by:
Joshua Haberman

**********************************************************************/

#ifdef USE_WAVPACK

#include "Export.h"

#include <wavpack.h>
#include <wx/log.h>
#include <wx/checkbox.h>

#include "../ShuttleGui.h"
#include "../ProjectSettings.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/ProgressDialog.h"
#include "wxFileNameWrapper.h"
#include "../Tags.h"
#include "Mix.h"
#include "Prefs.h"

//---------------------------------------------------------------------------
// ExportWavPackOptions
//---------------------------------------------------------------------------

#define ID_HYBRID_MODE 9000

class ExportWavPackOptions final : public wxPanelWrapper
{
public:

   ExportWavPackOptions(wxWindow *parent, int format);
   virtual ~ExportWavPackOptions();

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

   void OnHybridMode(wxCommandEvent& evt);

private:
   wxCheckBox *mHybridMode;
   wxCheckBox *mCreateCorrectionFile;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(ExportWavPackOptions, wxPanelWrapper)
   EVT_CHECKBOX(ID_HYBRID_MODE, ExportWavPackOptions::OnHybridMode)
END_EVENT_TABLE()

ExportWavPackOptions::ExportWavPackOptions(wxWindow *parent, int WXUNUSED(format))
: wxPanelWrapper(parent, wxID_ANY)
{
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);

   TransferDataToWindow();
}

ExportWavPackOptions::~ExportWavPackOptions()
{
   TransferDataFromWindow();
}

const TranslatableStrings ExportQualityNames{
   XO("Low Quality(Fast)") ,
   XO("High Quality(Slow)") ,
   XO("Very High Quality(Slowest)") ,
};

const std::vector< int > ExportQualityValues{
   0,
   1,
   2,
};

void ExportWavPackOptions::PopulateOrExchange(ShuttleGui & S)
{
   bool hybridMode = false;
   bool createCorrectionFile = false;

   S.StartVerticalLay();
   {
      S.StartHorizontalLay(wxEXPAND);
      {
         S.SetSizerProportion(1);
         S.StartMultiColumn(2, wxCENTER);
         {
            S.TieNumberAsChoice(
               XXO("Quality"),
               {wxT("/FileFormats/WavPackEncodeQuality"),
                1},
               ExportQualityNames,
               &ExportQualityValues
            );

            mHybridMode = S.Id(ID_HYBRID_MODE).TieCheckBox( XXO("Hybrid Mode"), hybridMode);
            mCreateCorrectionFile = S.Disable(!hybridMode).TieCheckBox( XXO("Create Correction(.wvc) File"), createCorrectionFile);
         }
         S.EndMultiColumn();
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();
}

bool ExportWavPackOptions::TransferDataToWindow()
{
   return true;
}

bool ExportWavPackOptions::TransferDataFromWindow()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   gPrefs->Write(wxT("/FileFormats/WavPackCreateCorrectionFile"), mCreateCorrectionFile->GetValue());
   gPrefs->Flush();

   return true;
}

void ExportWavPackOptions::OnHybridMode(wxCommandEvent&)
{
   bool hybridMode = false;
   hybridMode = mHybridMode->GetValue();
   mCreateCorrectionFile->Enable(hybridMode);

   gPrefs->Write(wxT("/FileFormats/WavPackHybridMode"), hybridMode);
   gPrefs->Flush();
};

//---------------------------------------------------------------------------
// ExportWavPack
//---------------------------------------------------------------------------

class ExportWavPack final : public ExportPlugin
{
public:

   ExportWavPack();

   void OptionsCreate(ShuttleGui &S, int foramt) override;

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

};

ExportWavPack::ExportWavPack()
:  ExportPlugin()
{
   AddFormat();
   SetFormat(wxT("WavPack"),0);
   AddExtension(wxT("wv"),0);
   SetMaxChannels(2,0);
   SetCanMetaData(true,0);
   SetDescription(XO("WavPack Files"),0);
}

ProgressResult ExportWavPack::Export(AudacityProject *project,
                       std::unique_ptr<ProgressDialog> &pDialog,
                       unsigned numChannels,
                       const wxFileNameWrapper &fName,
                       bool selectionOnly,
                       double t0,
                       double t1,
                       MixerSpec *mixerSpec,
                       const Tags *metadata,
                       int WXUNUSED(subformat))
{
   return ProgressResult::Success;
}

void ExportWavPack::OptionsCreate(ShuttleGui &S, int format)
{
   S.AddWindow( safenew ExportWavPackOptions{ S.GetParent(), format } );
}

static Exporter::RegisteredExportPlugin sRegisteredPlugin{ "WavPack",
   []{ return std::make_unique< ExportWavPack >(); }
};

#endif
