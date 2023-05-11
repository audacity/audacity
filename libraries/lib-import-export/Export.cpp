/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.cpp

  Dominic Mazzoni

*******************************************************************//**

\class Export
\brief Main class to control the export function.

*//*****************************************************************/

#include "Export.h"

#include <numeric>

#include "BasicUI.h"
#include "Mix.h"
#include "Project.h"
#include "WaveTrack.h"
#include "ProjectRate.h"
#include "wxFileNameWrapper.h"
#include "StretchingSequence.h"

#include "ExportUtils.h"

namespace {
   const auto PathStart = L"Exporters";

   using ExportPluginFactories = std::vector< Exporter::ExportPluginFactory >;
   ExportPluginFactories &sFactories()
   {
      static ExportPluginFactories theList;
      return theList;
   }
}

Registry::GroupItemBase &Exporter::ExporterItem::Registry()
{
   static Registry::GroupItem<Registry::DefaultTraits> registry{ PathStart };
   return registry;
}

Exporter::ExporterItem::ExporterItem(
   const Identifier &id, const Exporter::ExportPluginFactory &factory )
   : SingleItem{ id }
   , mFactory{ factory }
{}

Exporter::RegisteredExportPlugin::RegisteredExportPlugin(
   const Identifier &id,
   const ExportPluginFactory &factory,
   const Registry::Placement &placement )
   : RegisteredItem{
      factory ? std::make_unique< ExporterItem >( id, factory ) : nullptr,
      placement
   }
{
}

ExportTaskBuilder::ExportTaskBuilder() { }
ExportTaskBuilder::~ExportTaskBuilder() { }

ExportTaskBuilder& ExportTaskBuilder::SetFileName(const wxFileName& filename)
{
   mFileName = filename;
   return *this;
}

ExportTaskBuilder& ExportTaskBuilder::SetRange(double t0, double t1, bool selectedOnly) noexcept
{
   mT0 = t0;
   mT1 = t1;
   mSelectedOnly = selectedOnly;
   return *this;
}

ExportTaskBuilder& ExportTaskBuilder::SetParameters(ExportProcessor::Parameters parameters) noexcept
{
   mParameters = std::move(parameters);
   return *this;
}

ExportTaskBuilder& ExportTaskBuilder::SetNumChannels(unsigned int numChannels) noexcept
{
   mNumChannels = numChannels;
   return *this;
}

ExportTaskBuilder& ExportTaskBuilder::SetPlugin(const ExportPlugin* plugin, int format) noexcept
{
   mPlugin = plugin;
   mFormat = format;
   return *this;
}

ExportTaskBuilder& ExportTaskBuilder::SetMixerSpec(MixerOptions::Downmix *mixerSpec) noexcept
{
   mMixerSpec = mixerSpec;
   return *this;
}

ExportTaskBuilder& ExportTaskBuilder::SetSampleRate(double sampleRate) noexcept
{
   mSampleRate = sampleRate;
   return *this;
}

ExportTaskBuilder& ExportTaskBuilder::SetTags(const Tags *tags) noexcept
{
   mTags = tags;
   return *this;
}

ExportTask ExportTaskBuilder::Build(AudacityProject& project)
{
   //File rename stuff should be moved out to somewhere else...
   auto filename = mFileName;

   //For safety, if the file already exists we use temporary filename
   //and replace original one export succeeded
   int suffix = 0;
   while (filename.FileExists()) {
      filename.SetName(mFileName.GetName() +
                        wxString::Format(wxT("%d"), suffix));
      suffix++;
   }

   auto processor = mPlugin->CreateProcessor(mFormat);
   if(!processor->Initialize(project,
      mParameters,
      mFileName.GetFullPath(),
      mT0, mT1, mSelectedOnly,
      mSampleRate, mMixerSpec ? mMixerSpec->GetNumChannels() : mNumChannels,
      mMixerSpec,
      mTags))
   {
      return ExportTask([](ExportProcessorDelegate&){ return ExportResult::Cancelled; });
   }

   return ExportTask([actualFilename = filename,
      targetFilename = mFileName,
      processor = std::shared_ptr<ExportProcessor>(processor.release())]
      (ExportProcessorDelegate& delegate)
      {
         auto result = ExportResult::Error;
         auto cleanup = finally( [&] {
            if(result == ExportResult::Success || result == ExportResult::Stopped)
            {
               if (actualFilename != targetFilename)
               {
                  //may fail...
                  ::wxRenameFile(actualFilename.GetFullPath(),
                     targetFilename.GetFullPath(),
                     true);
               }
            }
            else
               ::wxRemoveFile(actualFilename.GetFullPath());
         } );
         result = processor->Process(delegate);
         return result;
      });
}

Exporter::Exporter( AudacityProject &project )
: mProject{ &project }
{
   using namespace Registry;
   static OrderingPreferenceInitializer init{
      PathStart,
      { {wxT(""), wxT("PCM,MP3,OGG,FLAC,MP2,CommandLine,FFmpeg") } },
   };

   // build the list of export plugins.
   for ( const auto &factory : sFactories() )
      mPlugins.emplace_back( factory() );

   struct MyVisitor final : Visitor {
      MyVisitor()
      {
         // visit the registry to collect the plug-ins properly
         // sorted
         GroupItem<Registry::DefaultTraits> top{ PathStart };
         Registry::Visit( *this, &top, &ExporterItem::Registry() );
      }

      void Visit( SingleItem &item, const Path &path ) override
      {
         mPlugins.emplace_back(
            static_cast<ExporterItem&>( item ).mFactory() );
      }

      ExportPluginArray mPlugins;
   } visitor;

   mPlugins.swap( visitor.mPlugins );
}

Exporter::~Exporter()
{
}

const ExportPluginArray &Exporter::GetPlugins()
{
   return mPlugins;
}

void Exporter::Configure(const wxFileName &filename,
                         int pluginIndex,
                         int formatIndex,
                         int sampleRate,
                         const ExportProcessor::Parameters& parameters)
{
   mFilename = filename;
   mFormat = pluginIndex;
   mSubFormat = formatIndex;
   mParameters = parameters;
   mSampleRate = sampleRate;
}

bool Exporter::SetExportRange(double t0, double t1, bool selectedOnly, bool skipSilenceAtBeginning)
{
   double earliestBegin = t0;
   double latestEnd = t1;
   int numSelected = 0;

   auto &tracks = TrackList::Get(*mProject);

   const auto range = ExportUtils::FindExportWaveTracks(tracks, selectedOnly);

   for (auto tr : range) {
      numSelected++;

      if (tr->GetOffset() < earliestBegin) {
         earliestBegin = tr->GetOffset();
      }

      if (tr->GetEndTime() > latestEnd) {
         latestEnd = tr->GetEndTime();
      }
   }

   if (numSelected == 0) {
      return false;
   }

   mT0 = t0;
   mT1 = t1;
   mSelectedOnly = selectedOnly;
   mNumSelected = numSelected;
   mMono = std::all_of(range.begin(), range.end(), [](const WaveTrack *pTrack){
      return IsMono(*pTrack) && pTrack->GetPan() == 0.0;
   });

   // The skipping of silent space could be cleverer and take 
   // into account clips.
   // As implemented now, it can only skip initial silent space that 
   // has no clip before it, and terminal silent space that has no clip 
   // after it.
   if (t0 < earliestBegin){
      // Bug 1904 
      // Previously we always skipped initial silent space.
      // Now skipping it is an opt-in option.
      if (skipSilenceAtBeginning)
         mT0 = earliestBegin;
   }

   // We still skip silent space at the end
   if (mT1 > latestEnd)
      mT1 = latestEnd;

   return true;
}

ExportTask Exporter::CreateExportTask()
{
   return CreateExportTask(mParameters);
}

ExportTask Exporter::CreateExportTask(const ExportProcessor::Parameters& parameters,
                       unsigned numChannels, int sampleRate,
                       const FileExtension &type, const wxString & filename,
                       bool selectedOnly, double t0, double t1)
{
   int i = -1;
   for (const auto& pPlugin : mPlugins)
   {
      ++i;
      for (int j = 0; j < pPlugin->GetFormatCount(); j++)
      {
         auto formatInfo = pPlugin->GetFormatInfo(j);
         if (formatInfo.format.IsSameAs(type, false))
         {
            mChannels = numChannels;
            mSelectedOnly = selectedOnly;
            mT0 = t0;
            mT1 = t1;
            
            Configure(filename, i, j, sampleRate, parameters);
            return CreateExportTask(parameters);
         }
      }
   }
   return {};
}



ExportTask Exporter::CreateExportTask(const ExportProcessor::Parameters& parameters)
{
   //File rename stuff should be moved out to somewhere else...

   auto filename = mFilename;

   //For safety, if the file already exists we use temporary filename
   //and replace original one export succeeded
   int suffix = 0;
   while (filename.FileExists()) {
      filename.SetName(mFilename.GetName() +
                        wxString::Format(wxT("%d"), suffix));
      suffix++;
   }

   auto processor = mPlugins[mFormat]->CreateProcessor(mSubFormat);
   if(!processor->Initialize(*mProject,
      parameters,
      mFilename.GetFullPath(),
      mT0, mT1, mSelectedOnly,
      mSampleRate,
      mMixerSpec ? mMixerSpec->GetNumChannels() : mChannels,
      mMixerSpec.get()))
   {
      return ExportTask([](ExportProcessorDelegate&){ return ExportResult::Cancelled; });
   }

   return ExportTask([actualFilename = filename,
      mixer = mMixerSpec.release(),// Get rid of mixerspec
      targetFilename = mFilename,
      processor = processor.release()]
      (ExportProcessorDelegate& delegate)
      {
         auto result = ExportResult::Error;
         auto cleanup = finally( [&] {
            delete processor;
            delete mixer;
            if(result == ExportResult::Success || result == ExportResult::Stopped)
            {
               if (actualFilename != targetFilename)
               {
                  //may fail...
                  ::wxRenameFile(actualFilename.GetFullPath(),
                     targetFilename.GetFullPath(),
                     true);
               }
            }
            else
               ::wxRemoveFile(actualFilename.GetFullPath());
         } );
         result = processor->Process(delegate);
         return result;
      });
}

int Exporter::GetAutoExportFormat() {
   return mFormat;
}

int Exporter::GetAutoExportSubFormat() {
   return mSubFormat;
}

int Exporter::GetAutoExportSampleRate()
{
   return mSampleRate;
}

wxFileName Exporter::GetAutoExportFileName() {
   return mFilename;
}

ExportProcessor::Parameters Exporter::GetAutoExportParameters()
{
   return mParameters;
}


MixerOptions::Downmix* Exporter::CreateMixerSpec()
{
   constexpr auto MaxExportChannels = 32u;

   const auto exportTracks = ExportUtils::FindExportWaveTracks(TrackList::Get( *mProject ), mSelectedOnly);
   mMixerSpec = std::make_unique<MixerOptions::Downmix>(
      std::accumulate(
         exportTracks.begin(),
         exportTracks.end(),
         0,
         [](int sum, const auto& track) { return sum + track->NChannels(); }),
      // JKC: This is an attempt to fix a 'watching brief' issue, where the slider is
      // sometimes not slidable.  My suspicion is that a mixer may incorrectly
      // state the number of channels - so we assume there are always at least two.
      // The downside is that if someone is exporting to a mono device, the dialog
      // will allow them to output to two channels. Hmm.  We may need to revisit this.
      // STF (April 2016): AMR (narrowband) and MP3 may export 1 channel.
      std::clamp(mPlugins[mFormat]->GetFormatInfo(mSubFormat).maxChannels,
         1u,
         MaxExportChannels));
   return mMixerSpec.get();
}

Exporter::DownMixMode Exporter::SetUseStereoOrMonoOutput()
{
   // Clean up ... should never happen
   mMixerSpec.reset();
   
   const unsigned channels = mMono ? 1 : 2;
   mChannels =
      std::min(channels, mPlugins[mFormat]->GetFormatInfo(mSubFormat).maxChannels);
   
   wxString exportFormat = mPlugins[mFormat]->GetFormatInfo(mSubFormat).format;
   
   if(mNumSelected > 1 || channels > mChannels)
   {
      //TODO: violates OCP
      if(exportFormat == wxT("CL") || exportFormat == wxT("FFMPEG"))
         return DownMixMode::FormatDefined;

      return mChannels == 1 ? DownMixMode::Mono : DownMixMode::Stereo;
   }
   return DownMixMode::None;
}

bool Exporter::CanMetaData() const
{
   return mPlugins[mFormat]->GetFormatInfo(mSubFormat).canMetaData;
}

void ShowDiskFullExportErrorDialog(const wxFileNameWrapper &fileName)
{
   BasicUI::ShowErrorDialog( {},
      XO("Warning"),
      FileException::WriteFailureMessage(fileName),
      "Error:_Disk_full_or_not_writable"
   );
}

void ShowExportErrorDialog(const TranslatableString& message,
      const TranslatableString& caption,
      bool allowReporting)
{
   ShowExportErrorDialog(message, caption, {}, allowReporting);
}

void ShowExportErrorDialog(const TranslatableString& message,
   const TranslatableString& caption,
   const ManualPageID& helpPageId,
   bool allowReporting)
{
   using namespace BasicUI;
   ShowErrorDialog( {},
      caption,
      message,
      helpPageId,
      ErrorDialogOptions { allowReporting ? ErrorDialogType::ModalErrorReport : ErrorDialogType::ModalError });
}
