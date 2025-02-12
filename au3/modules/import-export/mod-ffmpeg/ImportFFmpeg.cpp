/**********************************************************************

Audacity: A Digital Audio Editor

ImportFFmpeg.cpp

Copyright 2008  LRN
Based on ImportFLAC.cpp by Sami Liedes and transcode_sample.c by ANYwebcam Pty Ltd
Licensed under the GNU General Public License v2 or later

*//****************************************************************//**

\class FFmpegImportFileHandle
\brief An ImportFileHandle for FFmpeg data

*//****************************************************************//**

\class FFmpegImportPlugin
\brief An ImportPlugin for FFmpeg data

*//*******************************************************************/

// #include "FFmpeg.h"
#include "lib-ffmpeg-support/FFmpegFunctions.h"

#include <wx/log.h>

#define DESC XO("FFmpeg-compatible files")

//TODO: remove non-audio extensions
static const auto exts = {
    wxT("4xm"),
    wxT("MTV"),
    wxT("roq"),
    wxT("aac"),
    wxT("ac3"),
    wxT("aif"),
    wxT("aiff"),
    wxT("afc"),
    wxT("aifc"),
    wxT("al"),
    wxT("amr"),
    wxT("apc"),
    wxT("ape"),
    wxT("apl"),
    wxT("mac"),
    wxT("asf"),
    wxT("wmv"),
    wxT("wma"),
    wxT("au"),
    wxT("avi"),
    wxT("avs"),
    wxT("bethsoftvid"),
    wxT("c93"),
    wxT("302"),
    wxT("daud"),
    wxT("dsicin"),
    wxT("dts"),
    wxT("dv"),
    wxT("dxa"),
    wxT("ea"),
    wxT("cdata"),
    wxT("ffm"),
    wxT("film_cpk"),
    wxT("flac"),
    wxT("flic"),
    wxT("flv"),
    wxT("gif"),
    wxT("gxf"),
    wxT("idcin"),
    wxT("image2"),
    wxT("image2pipe"),
    wxT("cgi"),
    wxT("ipmovie"),
    wxT("nut"),
    wxT("lmlm4"),
    wxT("m4v"),
    wxT("mkv"),
    wxT("mm"),
    wxT("mmf"),
    wxT("mov"),
    wxT("mp4"),
    wxT("m4a"),
    wxT("m4r"),
    wxT("3gp"),
    wxT("3g2"),
    wxT("mj2"),
    wxT("mp3"),
    wxT("mpc"),
    wxT("mpc8"),
    wxT("mpg"),
    wxT("mpeg"),
    wxT("ts"),
    wxT("mpegtsraw"),
    wxT("mpegvideo"),
    wxT("msnwctcp"),
    wxT("ul"),
    wxT("mxf"),
    wxT("nsv"),
    wxT("nuv"),
    wxT("ogg"),
    wxT("opus"),
    wxT("psxstr"),
    wxT("pva"),
    wxT("redir"),
    wxT("rl2"),
    wxT("rm"),
    wxT("ra"),
    wxT("rv"),
    wxT("rtsp"),
    wxT("s16be"),
    wxT("sw"),
    wxT("s8"),
    wxT("sb"),
    wxT("sdp"),
    wxT("shn"),
    wxT("siff"),
    wxT("vb"),
    wxT("son"),
    wxT("smk"),
    wxT("sol"),
    wxT("swf"),
    wxT("thp"),
    wxT("tiertexseq"),
    wxT("tta"),
    wxT("txd"),
    wxT("u16be"),
    wxT("uw"),
    wxT("ub"),
    wxT("u8"),
    wxT("vfwcap"),
    wxT("vmd"),
    wxT("voc"),
    wxT("wav"),
    wxT("wc3movie"),
    wxT("wsaud"),
    wxT("wsvqa"),
    wxT("wv")
};

// all the includes live here by default
#include "libraries/lib-import-export/Import.h"
#include "libraries/lib-tags/Tags.h"
#include "WaveTrack.h"
#include "libraries/lib-import-export/ImportPlugin.h"
#include "libraries/lib-import-export/ImportUtils.h"
#include "libraries/lib-import-export/ImportProgressListener.h"

class FFmpegImportFileHandle;

/// A representative of FFmpeg loader in
/// the Audacity import plugin list
class FFmpegImportPlugin final : public ImportPlugin
{
public:
    FFmpegImportPlugin()
        : ImportPlugin(FileExtensions(exts.begin(), exts.end()))
    {
    }

    ~FFmpegImportPlugin() { }

    wxString GetPluginStringID() override { return wxT("libav"); }
    TranslatableString GetPluginFormatDescription() override;

    TranslatableString FailureHint() const override
    {
        return !FFmpegFunctions::Load()
               ? XO("Try installing FFmpeg.\n") : TranslatableString {};
    }

    ///! Probes the file and opens it if appropriate
    std::unique_ptr<ImportFileHandle> Open(
        const FilePath& Filename, AudacityProject*) override;
};

struct StreamContext final
{
    int StreamIndex { -1 };

    std::unique_ptr<AVCodecContextWrapper> CodecContext;

    int InitialChannels { 0 };
    sampleFormat SampleFormat { floatSample };

    bool Use { true };
};

///! Does actual import, returned by FFmpegImportPlugin::Open
class FFmpegImportFileHandle final : public ImportFileHandle
{
public:
    FFmpegImportFileHandle(const FilePath& name);
    ~FFmpegImportFileHandle();

    ///! Format initialization
    ///\return true if successful, false otherwise
    bool Init();
    ///! Codec initialization
    ///\return true if successful, false otherwise
    bool InitCodecs();

    TranslatableString GetFileDescription() override;
    ByteCount GetFileUncompressedBytes() override;

    void Import(
        ImportProgressListener& progressListener, WaveTrackFactory* trackFactory, TrackHolders& outTracks, Tags* tags,
        std::optional<LibFileFormats::AcidizerTags>& outAcidTags) override;

    FilePath GetFilename() const override;

    void Cancel() override;

    void Stop() override;

    ///! Writes decoded data into WaveTracks.
    ///\param sc - stream context
    void WriteData(StreamContext* sc, const AVPacketWrapper* packet);

    ///! Writes extracted metadata to tags object
    ///\param avf - file context
    ///\ tags - Audacity tags object
    void WriteMetadata(Tags* tags);

    ///! Retrieves metadata from FFmpeg and converts to wxString
    ///\param avf - file context
    ///\ tags - Audacity tags object
    ///\ tag - name of tag to set
    ///\ name - name of metadata item to retrieve
    void GetMetadata(Tags& tags, const wxChar* tag, const char* name);

    ///! Called by Import.cpp
    ///\return number of readable streams in the file
    wxInt32 GetStreamCount() override
    {
        return static_cast<wxInt32>(mStreamContexts.size());
    }

    ///! Called by Import.cpp
    ///\return array of strings - descriptions of the streams
    const TranslatableStrings& GetStreamInfo() override
    {
        return mStreamInfo;
    }

    ///! Called by Import.cpp
    ///\param StreamID - index of the stream in mStreamInfo and mStreamContexts
    ///\param Use - true if this stream should be imported, false otherwise
    void SetStreamUsage(wxInt32 StreamID, bool Use) override
    {
        if (StreamID < static_cast<wxInt32>(mStreamContexts.size())) {
            mStreamContexts[StreamID].Use = Use;
        }
    }

private:
    // Construct this member first, so it is destroyed last, so the functions
    // remain loaded while other members are destroyed
    const std::shared_ptr<FFmpegFunctions> mFFmpeg = FFmpegFunctions::Load();

    std::vector<StreamContext> mStreamContexts;

    std::unique_ptr<AVFormatContextWrapper> mAVFormatContext;

    TranslatableStrings mStreamInfo;     //!< Array of stream descriptions. After Init() and before Import(), same size as mStreamContexts

    wxInt64 mProgressPos = 0;                //!< Current timestamp, file position or whatever is used as first argument for Update()
    wxInt64 mProgressLen = 1;                //!< Duration, total length or whatever is used as second argument for Update()

    bool mCancelled = false;                     //!< True if importing was canceled by user
    bool mStopped = false;                       //!< True if importing was stopped by user
    const FilePath mName;
    std::vector<TrackListHolder> mStreams;
};

TranslatableString FFmpegImportPlugin::GetPluginFormatDescription()
{
    return DESC;
}

std::unique_ptr<ImportFileHandle> FFmpegImportPlugin::Open(
    const FilePath& filename, AudacityProject*)
{
    auto ffmpeg = FFmpegFunctions::Load();

    //Check if we're loading explicitly supported format
    wxString extension = filename.AfterLast(wxT('.'));
    if (SupportsExtension(extension)) {
        //Audacity is trying to load something that is declared as
        //officially supported by this plugin.
        //If we don't have FFmpeg configured - tell the user about it.
        //Since this will be happening often, use disableable "FFmpeg not found" dialog
        //insdead of usual AudacityMessageBox()
        bool newsession = NewImportingSession.Read();
        if (!ffmpeg) {
            // auto dontShowDlg = FFmpegNotFoundDontShow.Read();
            if (/*dontShowDlg == 0 &&*/ newsession) {
                NewImportingSession.Write(false);
                gPrefs->Flush();
                // FFmpegNotFoundDialog { nullptr }.ShowModal();

                ffmpeg = FFmpegFunctions::Load();
            }
        }
    }
    if (!ffmpeg) {
        return nullptr;
    }

    // Construct the handle only after any reloading of ffmpeg functions
    auto handle = std::make_unique<FFmpegImportFileHandle>(filename);

    // Open the file for import
    bool success = handle->Init();

    if (!success) {
        return nullptr;
    }

    return handle;
}

static Importer::RegisteredImportPlugin registered{ "FFmpeg",
                                                    std::make_unique< FFmpegImportPlugin >()
};

FFmpegImportFileHandle::FFmpegImportFileHandle(const FilePath& name)
    : mName{name}
{
}

bool FFmpegImportFileHandle::Init()
{
    if (!mFFmpeg) {
        return false;
    }

    mAVFormatContext = mFFmpeg->CreateAVFormatContext();

    const auto err = mAVFormatContext->OpenInputContext(mName, nullptr, AVDictionaryWrapper(*mFFmpeg));

    if (err != AVIOContextWrapper::OpenResult::Success) {
        wxLogError(wxT("FFmpeg : AVFormatContextWrapper::OpenInputContext() failed for file %s"), mName);
        return false;
    }

    if (!InitCodecs()) {
        return false;
    }

    return true;
}

bool FFmpegImportFileHandle::InitCodecs()
{
    for (unsigned int i = 0; i < mAVFormatContext->GetStreamsCount(); i++) {
        const AVStreamWrapper* stream = mAVFormatContext->GetStream(i);

        if (stream->IsAudio()) {
            const AVCodecIDFwd id = mAVFormatContext->GetStream(i)->GetAVCodecID();

            auto codec = mFFmpeg->CreateDecoder(id);
            auto name = mFFmpeg->avcodec_get_name(id);

            if (codec == NULL) {
                wxLogError(
                    wxT("FFmpeg : CreateDecoder() failed. Index[%02d], Codec[%02x - %s]"),
                    i, id, name);
                //FFmpeg can't decode this stream, skip it
                continue;
            }

            auto codecContextPtr = stream->GetAVCodecContext();

            if (codecContextPtr->Open(codecContextPtr->GetCodec()) < 0) {
                wxLogError(wxT("FFmpeg : Open() failed. Index[%02d], Codec[%02x - %s]"), i, id, name);
                //Can't open decoder - skip this stream
                continue;
            }

            const int channels = codecContextPtr->GetChannels();
            const sampleFormat preferredFormat
                =codecContextPtr->GetPreferredAudacitySampleFormat();

            auto codecContext = codecContextPtr.get();

            mStreamContexts.emplace_back(
                StreamContext { stream->GetIndex(), std::move(codecContextPtr),
                                channels, preferredFormat, true });

            // Stream is decodeable and it is audio. Add it and its description to the arrays
            int duration = 0;
            if (stream->GetDuration() > 0) {
                duration = stream->GetDuration() * stream->GetTimeBase().num / stream->GetTimeBase().den;
            } else {
                duration = mAVFormatContext->GetDuration() / AUDACITY_AV_TIME_BASE;
            }

            wxString bitrate;
            if (codecContext->GetBitRate() > 0) {
                bitrate.Printf(wxT("%d"), (int)codecContext->GetBitRate());
            } else {
                bitrate.Printf(wxT("?"));
            }

            AVDictionaryWrapper streamMetadata = stream->GetMetadata();

            auto lang = std::string(streamMetadata.Get("language", {}));

            auto strinfo = XO(
/* i18n-hint: "codec" is short for a "coder-decoder" algorithm */
                "Index[%02x] Codec[%s], Language[%s], Bitrate[%s], Channels[%d], Duration[%d]")
                           .Format(
                stream->GetIndex(),
                name,
                lang,
                bitrate,
                (int)codecContext->GetChannels(),
                (int)duration);

            mStreamInfo.push_back(strinfo);
        }
        //for video and unknown streams do nothing
    }
    //It doesn't really returns false, but GetStreamCount() will return 0 if file is composed entirely of unreadable streams
    return true;
}

TranslatableString FFmpegImportFileHandle::GetFileDescription()
{
    return DESC;
}

auto FFmpegImportFileHandle::GetFileUncompressedBytes() -> ByteCount
{
    // TODO: Get Uncompressed byte count.
    return 0;
}

void FFmpegImportFileHandle::Import(
    ImportProgressListener& progressListener, WaveTrackFactory* trackFactory,
    TrackHolders& outTracks, Tags* tags,
    std::optional<LibFileFormats::AcidizerTags>&)
{
    outTracks.clear();
    mCancelled = false;
    mStopped = false;

    //! This may break the correspondence with mStreamInfo
    mStreamContexts.erase(std::remove_if(mStreamContexts.begin(), mStreamContexts.end(), [](const StreamContext& ctx) {
        return !ctx.Use;
    }), mStreamContexts.end());

    for (unsigned s = 0; s < mStreamContexts.size(); ++s) {
        const StreamContext& sc = mStreamContexts[s];

        const auto format = ImportUtils::ChooseFormat(sc.SampleFormat);
        auto tracks = trackFactory->CreateMany(sc.InitialChannels, format, sc.CodecContext->GetSampleRate());

        // Handles the start_time by creating silence. This may or may not be correct.
        // There is a possibility that we should ignore first N milliseconds of audio instead. I do not know.
        /// TODO: Nag FFmpeg devs about start_time until they finally say WHAT is this and HOW to handle it.

        int64_t stream_delay = 0;

        const int64_t streamStartTime
            =mAVFormatContext->GetStream(sc.StreamIndex)->GetStartTime();

        if (streamStartTime != int64_t(AUDACITY_AV_NOPTS_VALUE) && streamStartTime > 0) {
            stream_delay = streamStartTime;

            wxLogDebug(
                wxT("Stream %d start_time = %lld, that would be %f milliseconds."),
                s, (long long)streamStartTime, double(streamStartTime) / 1000);
        }
        if (stream_delay > 0) {
            for (auto track : *tracks) {
                track->InsertSilence(0, double(stream_delay) / AUDACITY_AV_TIME_BASE);
            }
        }

        mStreams.push_back(tracks);
    }

    // This is the heart of the importing process

    // Read frames.
    for (std::unique_ptr<AVPacketWrapper> packet;
         (packet = mAVFormatContext->ReadNextPacket()) != nullptr
         && !mCancelled && !mStopped;) {
        // Find a matching StreamContext
        auto streamContextIt = std::find_if(
            mStreamContexts.begin(), mStreamContexts.end(),
            [index = packet->GetStreamIndex()](const StreamContext& ctx)
        {
            return ctx.StreamIndex == index;
        });

        if (streamContextIt == mStreamContexts.end()) {
            continue;
        }

        WriteData(&(*streamContextIt), packet.get());
        if (mProgressLen > 0) {
            progressListener.OnImportProgress(static_cast<double>(mProgressPos)
                                              / static_cast<double>(mProgressLen));
        }
    }

    // Flush the decoders.
    if (!mStreamContexts.empty() && !mCancelled) {
        auto emptyPacket = mFFmpeg->CreateAVPacketWrapper();

        for (StreamContext& sc : mStreamContexts) {
            WriteData(&sc, emptyPacket.get());
        }
    }

    if (mCancelled) {
        progressListener.OnImportResult(ImportProgressListener::ImportResult::Cancelled);
        return;
    }

    // Copy audio from mStreams to newly created tracks (destroying mStreams elements in process)
    for (auto& stream : mStreams) {
        ImportUtils::FinalizeImport(outTracks, std::move(*stream));
    }
    mStreams.clear();

    // Save metadata
    WriteMetadata(tags);
    progressListener.OnImportResult(mStopped
                                    ? ImportProgressListener::ImportResult::Stopped
                                    : ImportProgressListener::ImportResult::Success);
}

FilePath FFmpegImportFileHandle::GetFilename() const
{
    return mName;
}

void FFmpegImportFileHandle::Cancel()
{
    if (!mStopped) {
        mCancelled = true;
    }
}

void FFmpegImportFileHandle::Stop()
{
    if (!mCancelled) {
        mStopped = true;
    }
}

void FFmpegImportFileHandle::WriteData(StreamContext* sc, const AVPacketWrapper* packet)
{
    // Find the stream in mStreamContexts array
    auto streamIt = std::find_if(
        mStreamContexts.begin(),
        mStreamContexts.end(),
        [&](StreamContext& context) { return sc == &context; }
        );

    // Stream is not found. This should not really happen
    if (streamIt == mStreamContexts.end()) {
        //VS: Shouldn't this mean import failure?
        return;
    }
    auto stream = mStreams[std::distance(mStreamContexts.begin(), streamIt)];

    const auto nChannels = std::min(sc->CodecContext->GetChannels(), sc->InitialChannels);

    // Write audio into WaveTracks
    if (sc->SampleFormat == int16Sample) {
        auto data = sc->CodecContext->DecodeAudioPacketInt16(packet);
        const auto channelsCount = sc->CodecContext->GetChannels();
        const auto samplesPerChannel = data.size() / channelsCount;

        unsigned chn = 0;
        ImportUtils::ForEachChannel(*stream, [&](auto& channel)
        {
            if (chn >= nChannels) {
                return;
            }

            channel.AppendBuffer(
                reinterpret_cast<samplePtr>(data.data() + chn),
                sc->SampleFormat,
                samplesPerChannel,
                sc->CodecContext->GetChannels(),
                sc->SampleFormat
                );
            ++chn;
        });
    } else if (sc->SampleFormat == floatSample) {
        auto data = sc->CodecContext->DecodeAudioPacketFloat(packet);
        const auto channelsCount = sc->CodecContext->GetChannels();
        const auto samplesPerChannel = data.size() / channelsCount;

        auto channelIndex = 0;
        ImportUtils::ForEachChannel(*stream, [&](auto& channel)
        {
            if (channelIndex >= nChannels) {
                return;
            }

            channel.AppendBuffer(
                reinterpret_cast<samplePtr>(data.data() + channelIndex),
                sc->SampleFormat,
                samplesPerChannel,
                sc->CodecContext->GetChannels(),
                sc->SampleFormat
                );
            ++channelIndex;
        });
    }
    const AVStreamWrapper* avStream = mAVFormatContext->GetStream(sc->StreamIndex);

    int64_t filesize = mFFmpeg->avio_size(mAVFormatContext->GetAVIOContext()->GetWrappedValue());
    // PTS (presentation time) is the proper way of getting current position
    if (
        packet->GetPresentationTimestamp() != AUDACITY_AV_NOPTS_VALUE
        && mAVFormatContext->GetDuration() != AUDACITY_AV_NOPTS_VALUE) {
        auto timeBase = avStream->GetTimeBase();

        mProgressPos
            =packet->GetPresentationTimestamp() * timeBase.num / timeBase.den;

        mProgressLen
            =(mAVFormatContext->GetDuration() > 0
              ? mAVFormatContext->GetDuration() / AUDACITY_AV_TIME_BASE
              : 1);
    }
    // When PTS is not set, use number of frames and number of current frame
    else if (
        avStream->GetFramesCount() > 0 && sc->CodecContext->GetFrameNumber() > 0
        && sc->CodecContext->GetFrameNumber() <= avStream->GetFramesCount()) {
        mProgressPos = sc->CodecContext->GetFrameNumber();
        mProgressLen = avStream->GetFramesCount();
    }
    // When number of frames is unknown, use position in file
    else if (
        filesize > 0 && packet->GetPos() > 0 && packet->GetPos() <= filesize) {
        mProgressPos = packet->GetPos();
        mProgressLen = filesize;
    }
}

void FFmpegImportFileHandle::WriteMetadata(Tags* tags)
{
    Tags temp;

    GetMetadata(temp, TAG_TITLE, "title");
    GetMetadata(temp, TAG_COMMENTS, "comment");
    GetMetadata(temp, TAG_ALBUM, "album");
    GetMetadata(temp, TAG_TRACK, "track");
    GetMetadata(temp, TAG_GENRE, "genre");

    if (wxString(mAVFormatContext->GetInputFormat()->GetName()).Contains("m4a")) {
        GetMetadata(temp, TAG_ARTIST, "artist");
        GetMetadata(temp, TAG_YEAR, "date");
    } else if (wxString(mAVFormatContext->GetInputFormat()->GetName()).Contains("asf")) { /* wma */
        GetMetadata(temp, TAG_ARTIST, "artist");
        GetMetadata(temp, TAG_YEAR, "year");
    } else {
        GetMetadata(temp, TAG_ARTIST, "author");
        GetMetadata(temp, TAG_YEAR, "year");
    }

    if (!temp.IsEmpty()) {
        *tags = temp;
    }
}

void FFmpegImportFileHandle::GetMetadata(Tags& tags, const wxChar* tag, const char* name)
{
    auto metadata = mAVFormatContext->GetMetadata();

    if (metadata.HasValue(name, DICT_IGNORE_SUFFIX)) {
        tags.SetTag(tag, wxString::FromUTF8(std::string(metadata.Get(name, {}, DICT_IGNORE_SUFFIX))));
    }
}

FFmpegImportFileHandle::~FFmpegImportFileHandle()
{
}
