/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.
   License: GPL v2.  See License.txt.

   Labeler.cpp
   Hugo Flores Garcia
   Aldo Aguilar

******************************************************************/
/**

\class Labeler
\brief Labeler is an effect for labeling audio components in a track

TODO: add a more thorough description

*/
/*******************************************************************/

#include "Labeler.h"

#include <cstddef>
#include <string>
#include <wx/stattext.h>

#include "../FileNames.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"

#include "LoadEffects.h"

#include <torch/script.h>

const ComponentInterfaceSymbol EffectLabeler::Symbol{XO("Audio Labeler")};

// register audio Labeler
namespace {
BuiltinEffectsModule::Registration<EffectLabeler> reg;
}

// register event handlers
BEGIN_EVENT_TABLE(EffectLabeler, wxEvtHandler)
EVT_BUTTON(wxID_ANY, EffectLabeler::OnLoadButton)
END_EVENT_TABLE()

EffectLabeler::EffectLabeler() { SetLinearEffectFlag(false); }

EffectLabeler::~EffectLabeler() {}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectLabeler::GetSymbol() { return Symbol; }

TranslatableString EffectLabeler::GetDescription() {
  return XO("Audio Labelling!"); // TODO
}

wxString EffectLabeler::ManualPage() {
  return wxT("Audio Labeler"); // TODO
}

// EffectDefinitionInterface implementation

EffectType EffectLabeler::GetType() { return EffectTypeProcess; }

// EffectClientInterface implementation

// Effect implementation

// ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
// performs a forward pass through the deep model, and writes
// the output to new tracks.
bool EffectLabeler::ProcessOne(WaveTrack *leader, double tStart, double tEnd) {
  if (leader == NULL)
    return false;

  wxString labelTrackName(leader->GetName() + " Labels");
  std::shared_ptr<AddedAnalysisTrack> labelTrack =
      AddAnalysisTrack(labelTrackName);

  sampleFormat origFmt = leader->GetSampleFormat();
  int origRate = leader->GetRate();

  // Initiate processing buffer, most likely shorter than
  // the length of the selection being processed.
  Floats buffer{leader->GetMaxBlockSize()};

  // get each of the blocks we will process
  for (BlockIndex block : GetBlockIndices(leader, tStart, tEnd)) {
    // Get a blockSize of samples (smaller than the size of the buffer)
    sampleCount samplePos = block.first;
    size_t blockSize = block.second;

    // get a torch tensor from the leader track
    torch::Tensor input =
        BuildMultichannelTensor(leader, buffer.get(), samplePos, blockSize)
            .sum(0, true, torch::kFloat);
    // resample!
    input = mModel->Resample(input, origRate, mModel->GetSampleRate());

    // forward pass!
    torch::jit::IValue output = ForwardPass(input);

    // split forward pass output into output tensor and timestamps
    auto [modelOutput, timestamps] = mModel->ToTimestamps(output);

    // write the block's label to the label track
    double blockStart = leader->LongSamplesToTime(samplePos);
    sampleCount blockEndSamples = samplePos + (sampleCount)blockSize;
    double blockEnd = leader->LongSamplesToTime(blockEndSamples);

    TensorToLabelTrack(modelOutput, labelTrack, blockStart, blockEnd, timestamps);

    // Update the Progress meter
    double tPos = leader->LongSamplesToTime(samplePos);
    if (TrackProgress(mCurrentTrackNum, (tPos - tStart) / (tEnd - tStart)))
      return false;
  }
  labelTrack->Commit();
  return true;
}

void EffectLabeler::PostProcessSources(
    std::vector<WaveTrack::Holder> &sourceTracks, sampleFormat fmt,
    int sampleRate) {
  // flush all output track buffers
  // convert to the original rate and format
  for (std::shared_ptr<WaveTrack> track : sourceTracks) {
    track->Flush();
    track->ConvertToSampleFormat(fmt);
    track->Resample(sampleRate);
    AddToOutputTracks(track);

    // if the parent track used to be stereo,
    // make the source mono anyway
    mOutputTracks->GroupChannels(*track, 1);
  }
}

// UI stuff
void EffectLabeler::PopulateOrExchange(ShuttleGui &S) {
  S.StartVerticalLay(wxCENTER, true);
  {

    PopulateMetadata(S);

    S.StartHorizontalLay(wxCENTER, false);
    {
      mLoadModelBtn = S.AddButton(XXO("&Import Model..."));

      std::string modelDesc;
      if (mModel->IsLoaded())
        modelDesc = "loaded model successfully";
      else
        modelDesc = "pls load a model!";

      mDescription = S.AddVariableText(
          TranslatableString(wxString(modelDesc).c_str(), {}));
    }
    S.EndHorizontalLay();
  }
  S.EndVerticalLay();
}

void EffectLabeler::AddMetadataEntry(ShuttleGui &S, std::string desc,
                                     std::string key) {
  S.StartHorizontalLay(wxLEFT, false);
  {
    wxStaticText *descText =
        S.AddVariableText(TranslatableString(wxString(desc).c_str(), {}));

    wxFont font = descText->GetFont();
    font = font.MakeBold();
    descText->SetFont(font);

    std::string value = mModel->QueryMetadata(key.c_str());
    wxStaticText *text =
        S.AddVariableText(TranslatableString(wxString(value).c_str(), {}));

    mMetadataFields[key] = text;
  }
  S.EndHorizontalLay();
}

void EffectLabeler::PopulateMetadata(ShuttleGui &S) {
  // TODO: this does not take into account the possible
  // depth of the metadata.
  rapidjson::Document document = mModel->GetMetadata();
  S.StartVerticalLay(wxCENTER, false);
  {
    // TODO: bold not working, nicer table style
    AddMetadataEntry(S, "Model Name:", "name");
    AddMetadataEntry(S, "Separation Sample Rate:", "sample_rate");
    AddMetadataEntry(S, "Domain:", "domain");
    AddMetadataEntry(S, "Number of Labels:", "n_src");
    AddMetadataEntry(S, "Output Sources:", "labels");
  }
  S.EndVerticalLay();
}

void EffectLabeler::UpdateMetadataFields() {
  for (auto pair : mMetadataFields) {
    std::string key = pair.first;
    wxStaticText *field = pair.second;

    std::string value = mModel->QueryMetadata(key.c_str());
    field->SetLabel(wxString(value));
    field->SetName(wxString(value));
  }
}

void EffectLabeler::OnLoadButton(wxCommandEvent &WXUNUSED(event)) {
  auto path = FileNames::SelectFile(
      FileNames::Operation::Import, XO("Load Audio Labeler Model"),
      wxEmptyString, wxEmptyString, wxT("ts"),
      {FileNames::FileType(XO("TorchScript Files"), {wxT("ts")}, true)},
      wxFD_OPEN | wxRESIZE_BORDER, nullptr);

  if (path.empty())
    return;

  // attempt load deep learning model
  // TODO: what's the fallback when the model doesn't load?
  wxString descStr;
  if (!mModel->Load(path.ToStdString())) {
    descStr = wxString("error loading model :(");
  } else {
    descStr = wxString("loaded model succesfully!");
    std::vector<std::string> classList = mModel->GetLabels();

    for (size_t i = 0; i < classList.size(); i++) {
      std::cout << classList[i];
      mClasses.emplace_back(wxString(classList[i]));
    }
  }

  mDescription->SetLabel(descStr);
  mDescription->SetName(descStr);

  UpdateMetadataFields();
}

void EffectLabeler::TensorToLabelTrack(
    torch::Tensor output, std::shared_ptr<AddedAnalysisTrack> labelTrack,
    double tStart, double tEnd, torch::Tensor timestamps) {
  // TODO: add an internal check to make sure dim-1 of output nad timestamps
  // match
  timestamps += tStart;
  for (int i = 0; i < output.size(0); i++) {
    double tStartCurrLabel = timestamps[i][0].item().to<double>();
    double tEndCurrLabel = (timestamps[i][1].item().to<double>() > tEnd)
                               ? tEnd
                               : timestamps[i][1].item().to<double>();

    // finding the corresponding class for times
    torch::Tensor currentProbits = output[i];
    wxString classLabel =
        mClasses[torch::argmax(currentProbits).item().to<int>()];

    SelectedRegion blockRegion = SelectedRegion(tStartCurrLabel, tEndCurrLabel);
    labelTrack->get()->AddLabel(blockRegion, classLabel);
  }
}