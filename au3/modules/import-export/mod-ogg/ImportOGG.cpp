/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportOGG.cpp

  Joshua Haberman
  Leland Lucius

*//****************************************************************//**

\class ImportFileHandle
\brief An ImportFileHandle for data

  The Ogg format supports multiple logical bitstreams that can be chained
  within the physical bitstream. The sampling rate and number of channels
  can vary between these logical bitstreams. For the moment, we'll ignore
  all but the first logical bitstream.

  Ogg also allows for an arbitrary number of channels. Luckily, so does
  Audacity. We'll call the first channel LeftChannel, the second
  RightChannel, and all others after it MonoChannel.

*//****************************************************************//**

\class OGGImportPlugin
\brief An ImportPlugin for OGG data

*//*******************************************************************/

#include "Import.h"
#include "Tags.h"

#define DESC XO("Ogg Vorbis files")

static const auto exts = {
    wxT("ogg")
};

#include <wx/log.h>
#include <wx/setup.h> // see next comment
/* ffile.h must be included AFTER at least one other wx header that includes
 * wx/setup.h, otherwise #ifdefs erroneously collapse it to nothing. This is
 * a bug in wxWidgets (ffile.h should itself include wx/setup.h), and it
 * was a bitch to track down. */
#include <wx/ffile.h>

#include <vorbis/vorbisfile.h>

#include "WaveTrack.h"
#include "ImportPlugin.h"
#include "ImportProgressListener.h"
#include "ImportUtils.h"

class OggImportPlugin final : public ImportPlugin
{
public:
    OggImportPlugin()
        :  ImportPlugin(FileExtensions(exts.begin(), exts.end()))
    {
    }

    ~OggImportPlugin() { }

    wxString GetPluginStringID() override { return wxT("liboggvorbis"); }
    TranslatableString GetPluginFormatDescription() override;
    std::unique_ptr<ImportFileHandle> Open(
        const FilePath& Filename, AudacityProject*) override;
};

class OggImportFileHandle final : public ImportFileHandleEx
{
public:
    OggImportFileHandle(const FilePath& filename,
                        std::unique_ptr<wxFFile>&& file,
                        std::unique_ptr<OggVorbis_File>&& vorbisFile)
        :  ImportFileHandleEx(filename),
        mFile(std::move(file)),
        mVorbisFile(std::move(vorbisFile))
        , mStreamUsage{static_cast<size_t>(mVorbisFile->links)}
    {
        for (int i = 0; i < mVorbisFile->links; i++) {
            auto strinfo = XO("Index[%02x] Version[%d], Channels[%d], Rate[%ld]")
                           .Format(
                (unsigned int)i,
                mVorbisFile->vi[i].version,
                mVorbisFile->vi[i].channels,
                mVorbisFile->vi[i].rate);
            mStreamInfo.push_back(strinfo);
            mStreamUsage[i] = 0;
        }
    }

    ~OggImportFileHandle();

    TranslatableString GetFileDescription() override;
    ByteCount GetFileUncompressedBytes() override;
    void Import(
        ImportProgressListener& progressListener, WaveTrackFactory* trackFactory, TrackHolders& outTracks, Tags* tags,
        std::optional<LibFileFormats::AcidizerTags>& outAcidTags) override;

    wxInt32 GetStreamCount() override
    {
        if (mVorbisFile) {
            return mVorbisFile->links;
        } else {
            return 0;
        }
    }

    const TranslatableStrings& GetStreamInfo() override
    {
        return mStreamInfo;
    }

    void SetStreamUsage(wxInt32 StreamID, bool Use) override
    {
        if (mVorbisFile) {
            if (StreamID < mVorbisFile->links) {
                mStreamUsage[StreamID] = (Use ? 1 : 0);
            }
        }
    }

private:
    std::unique_ptr<wxFFile> mFile;
    std::unique_ptr<OggVorbis_File> mVorbisFile;

    ArrayOf<int> mStreamUsage;
    TranslatableStrings mStreamInfo;
    std::vector<TrackListHolder> mStreams;
};

TranslatableString OggImportPlugin::GetPluginFormatDescription()
{
    return DESC;
}

std::unique_ptr<ImportFileHandle> OggImportPlugin::Open(
    const FilePath& filename, AudacityProject*)
{
    // Suppress some compiler warnings about unused global variables in the library header
    wxUnusedVar(OV_CALLBACKS_DEFAULT);
    wxUnusedVar(OV_CALLBACKS_NOCLOSE);
    wxUnusedVar(OV_CALLBACKS_STREAMONLY);
    wxUnusedVar(OV_CALLBACKS_STREAMONLY_NOCLOSE);

    auto vorbisFile = std::make_unique<OggVorbis_File>();
    auto file = std::make_unique<wxFFile>(filename, wxT("rb"));

    if (!file->IsOpened()) {
        // No need for a message box, it's done automatically (but how?)
        return nullptr;
    }

    int err = ov_open(file->fp(), vorbisFile.get(), NULL, 0);

    if (err < 0) {
        TranslatableString message;

        switch (err) {
        case OV_EREAD:
            message = XO("Media read error");
            break;
        case OV_ENOTVORBIS:
            message = XO("Not an Ogg Vorbis file");
            break;
        case OV_EVERSION:
            message = XO("Vorbis version mismatch");
            break;
        case OV_EBADHEADER:
            message = XO("Invalid Vorbis bitstream header");
            break;
        case OV_EFAULT:
            message = XO("Internal logic fault");
            break;
        }

        // what to do with message?
        return nullptr;
    }

    return std::make_unique<OggImportFileHandle>(filename, std::move(file), std::move(vorbisFile));
}

static Importer::RegisteredImportPlugin registered{ "OGG",
                                                    std::make_unique< OggImportPlugin >()
};

TranslatableString OggImportFileHandle::GetFileDescription()
{
    return DESC;
}

auto OggImportFileHandle::GetFileUncompressedBytes() -> ByteCount
{
    // TODO:
    return 0;
}

void OggImportFileHandle::Import(
    ImportProgressListener& progressListener, WaveTrackFactory* trackFactory,
    TrackHolders& outTracks, Tags* tags,
    std::optional<LibFileFormats::AcidizerTags>&)
{
    BeginImport();

    outTracks.clear();

    wxASSERT(mFile->IsOpened());

    //Number of streams used may be less than mVorbisFile->links,
    //but this way bitstream matches array index.
    mStreams.reserve(mVorbisFile->links);

    for (int i = 0; i < mVorbisFile->links; ++i) {
        //Stream is not used
        if (mStreamUsage[i] == 0) {
            //This is just a padding to keep bitstream number and
            //array indices matched.
            mStreams.push_back({});
            continue;
        }

        vorbis_info* vi = ov_info(mVorbisFile.get(), i);

        // The format agrees with what is always passed to Append() below
        auto tracks = trackFactory->CreateMany(vi->channels, int16Sample, vi->rate);

        mStreams.push_back(tracks);
    }

    /* The number of bytes to get from the codec in each run */
#define CODEC_TRANSFER_SIZE 4096u

    /* The number of samples to read between calls to the callback.
     * Balance between responsiveness of the GUI and throughput of import. */
#define SAMPLES_PER_CALLBACK 100000

    long bytesRead = 0;
    {
        ArrayOf<short> mainBuffer{ CODEC_TRANSFER_SIZE };

        /* determine endianness (clever trick courtesy of Nicholas Devillard,
         * (http://www.eso.org/~ndevilla/endian/) */
        int testvar = 1, endian;
        if (*(char*)&testvar) {
            endian = 0; // little endian
        } else {
            endian = 1; // big endian
        }
        /* number of samples currently in each channel's buffer */
        long samplesRead = 0;
        int bitstream = 0;
        int samplesSinceLastCallback = 0;

        // You would think that the stream would already be seeked to 0, and
        // indeed it is if the file is legit.  But I had several ogg files on
        // my hard drive that have malformed headers, and this added call
        // causes them to be read correctly.  Otherwise they have lots of
        // zeros inserted at the beginning
        ov_pcm_seek(mVorbisFile.get(), 0);

        do {
            /* get data from the decoder */
            bytesRead = ov_read(mVorbisFile.get(), (char*)mainBuffer.get(),
                                CODEC_TRANSFER_SIZE,
                                endian,
                                2, // word length (2 for 16 bit samples)
                                1, // signed
                                &bitstream);

            if (bytesRead == OV_HOLE) {
                wxFileName ff(GetFilename());
                wxLogError(wxT("Ogg Vorbis importer: file %s is malformed, ov_read() reported a hole"),
                           ff.GetFullName());
                /* http://lists.xiph.org/pipermail/vorbis-dev/2001-February/003223.html
                 * is the justification for doing this - best effort for malformed file,
                 * hence the message.
                 */
                continue;
            } else if (bytesRead < 0) {
                /* Malformed Ogg Vorbis file. */
                /* TODO: Return some sort of meaningful error. */
                wxLogError(wxT("Ogg Vorbis importer: ov_read() returned error %i"),
                           bytesRead);
                break;
            }

            samplesRead = bytesRead / mVorbisFile->vi[bitstream].channels / sizeof(short);

            if (mStreamUsage[bitstream] != 0) {
                /* give the data to the wavetracks */
                unsigned chn = 0;
                ImportUtils::ForEachChannel(**std::next(mStreams.begin(), bitstream), [&](auto& channel)
                {
                    channel.AppendBuffer(
                        (char*)(mainBuffer.get() + chn),
                        int16Sample,
                        samplesRead,
                        mVorbisFile->vi[bitstream].channels,
                        int16Sample
                        );
                    ++chn;
                });
            }

            samplesSinceLastCallback += samplesRead;
            if (samplesSinceLastCallback > SAMPLES_PER_CALLBACK) {
                const auto timeTotal = ov_time_total(mVorbisFile.get(), bitstream);
                if (timeTotal > 0) {
                    progressListener.OnImportProgress(ov_time_tell(mVorbisFile.get()) / timeTotal);
                }
                samplesSinceLastCallback -= SAMPLES_PER_CALLBACK;
            }
        } while (!IsCancelled() && !IsStopped() && bytesRead != 0);
    }

    if (bytesRead < 0) {
        progressListener.OnImportResult(ImportProgressListener::ImportResult::Error);
        return;
    }

    if (IsCancelled()) {
        progressListener.OnImportResult(ImportProgressListener::ImportResult::Cancelled);
        return;
    }

    for (auto& stream : mStreams) {
        ImportUtils::FinalizeImport(outTracks, std::move(*stream));
    }
    mStreams.clear();

    //\todo { Extract comments from each stream? }
    if (mVorbisFile->vc[0].comments > 0) {
        tags->Clear();
        for (int c = 0; c < mVorbisFile->vc[0].comments; c++) {
            wxString comment = UTF8CTOWX(mVorbisFile->vc[0].user_comments[c]);
            wxString name = comment.BeforeFirst(wxT('='));
            wxString value = comment.AfterFirst(wxT('='));
            if (name.Upper() == wxT("DATE") && !tags->HasTag(TAG_YEAR)) {
                long val;
                if (value.length() == 4 && value.ToLong(&val)) {
                    name = TAG_YEAR;
                }
            }
            tags->SetTag(name, value);
        }
    }

    progressListener.OnImportResult(IsStopped()
                                    ? ImportProgressListener::ImportResult::Stopped
                                    : ImportProgressListener::ImportResult::Success);
}

OggImportFileHandle::~OggImportFileHandle()
{
    ov_clear(mVorbisFile.get());
    mFile->Detach();   // so that it doesn't try to close the file (ov_clear()
                       // did that already)
}
