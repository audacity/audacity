#include "LibsndfileTagger.h"
#include "AcidizerTags.h"

#include <array>
#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <memory>
#include <stdexcept>

namespace LibImportExport {
namespace Test {
LibsndfileTagger::LibsndfileTagger(double duration, const std::string& filename)
    : mFilename{filename.empty() ? std::tmpnam(nullptr) : filename}
{
    SF_INFO sfInfo;
    std::memset(&sfInfo, 0, sizeof(sfInfo));
    sfInfo.samplerate = 44100;
    sfInfo.channels = 1;
    sfInfo.format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;
    sfInfo.sections = 1;
    sfInfo.seekable = 1;
    mFile = sf_open(mFilename.c_str(), SFM_WRITE, &sfInfo);
    assert(mFile != nullptr);
    if (duration > 0) {
        // Write zeros using sf_write_float
        sfInfo.frames
            =static_cast<sf_count_t>(std::round(duration * sfInfo.samplerate));
        const auto numItems = sfInfo.channels * sfInfo.frames;
        std::unique_ptr<short[]> zeros { new short[numItems] };
        std::fill(zeros.get(), zeros.get() + numItems, 0);
        const auto written = sf_write_short(mFile, zeros.get(), numItems);
        if (written != numItems) {
            throw std::runtime_error("Failed to write audio to file");
        }
    }
}

LibsndfileTagger::~LibsndfileTagger()
{
    sf_close(mFile);
}

LibsndfileTagger::operator bool() const
{
    return mFile != nullptr;
}

SNDFILE& LibsndfileTagger::ReopenInReadMode()
{
    if (!mFile) {
        throw std::runtime_error("File is not open");
    }

    sf_close(mFile);
    mDistributorData.reset();
    mAcidData.reset();

    SF_INFO sfInfo;
    mFile = sf_open(mFilename.c_str(), SFM_READ, &sfInfo);
    if (!mFile) {
        throw std::runtime_error("Failed to re-open file");
    }
    return *mFile;
}

void LibsndfileTagger::AddAcidizerTags(const Test::AcidizerTags& acidTags)
{
    // Adapted from the ACID chunk readout code in libsndfile and its comment:
    // clang-format off
    /*
    ** The acid chunk goes a little something like this:
    **
    ** 4 bytes          'acid'
    ** 4 bytes (int)     length of chunk starting at next byte
    **
    ** 4 bytes (int)     type of file:
    **        this appears to be a bit mask,however some combinations
    **        are probably impossible and/or qualified as "errors"
    **
    **        0x01 On: One Shot         Off: Loop
    **        0x02 On: Root note is Set Off: No root
    **        0x04 On: Stretch is On,   Off: Strech is OFF
    **        0x08 On: Disk Based       Off: Ram based
    **        0x10 On: ??????????       Off: ????????? (Acidizer puts that ON)
    **
    ** 2 bytes (short)      root note
    **        if type 0x10 is OFF : [C,C#,(...),B] -> [0x30 to 0x3B]
    **        if type 0x10 is ON  : [C,C#,(...),B] -> [0x3C to 0x47]
    **         (both types fit on same MIDI pitch albeit different octaves, so who cares)
    **
    ** 2 bytes (short)      ??? always set to 0x8000
    ** 4 bytes (float)      ??? seems to be always 0
    ** 4 bytes (int)        number of beats
    ** 2 bytes (short)      meter denominator   //always 4 in SF/ACID
    ** 2 bytes (short)      meter numerator     //always 4 in SF/ACID
    **                      //are we sure about the order?? usually its num/denom
    ** 4 bytes (float)      tempo
    **
    */
    // clang-format on

    SF_LOOP_INFO loopInfo {};
    loopInfo.bpm = acidTags.bpm.value_or(0.);
    loopInfo.loop_mode = acidTags.isOneShot ? SF_LOOP_NONE : SF_LOOP_FORWARD;

    SF_CHUNK_INFO chunk;
    std::memset(&chunk, 0, sizeof(chunk));
    std::snprintf(chunk.id, sizeof(chunk.id), "acid");
    chunk.id_size = 4;
    // All sizes listed above except the first two:
    chunk.datalen = 4 + 2 + 2 + 4 + 4 + 2 + 2 + 4;
    mAcidData = std::make_unique<uint8_t[]>(chunk.datalen);
    std::memset(mAcidData.get(), 0, chunk.datalen);
    chunk.data = mAcidData.get();

    // The type has 4 bytes, of which we may only set the 1st bit to 1 if the
    // loop is one-shot:
    if (acidTags.isOneShot) {
        auto type = reinterpret_cast<uint32_t*>(mAcidData.get());
        *type |= 0x00000001;
    } else if (acidTags.beats.has_value()) {
        auto numBeats = reinterpret_cast<uint32_t*>(mAcidData.get() + 12);
        *numBeats = *acidTags.beats;
    } else {
        assert(acidTags.bpm.has_value());
        auto tempo = reinterpret_cast<float*>(mAcidData.get() + 20);
        *tempo = *acidTags.bpm;
    }

    // Set the meter denominator 2 bytes to 4:
    auto numerator = reinterpret_cast<uint16_t*>(mAcidData.get() + 16);
    *numerator |= 0x0004;
    auto denominator = reinterpret_cast<uint16_t*>(mAcidData.get() + 18);
    *denominator |= 0x0004;

    const auto result = sf_set_chunk(mFile, &chunk);
    assert(result == SF_ERR_NO_ERROR);
}

void LibsndfileTagger::AddDistributorInfo(const std::string& distributor)
{
    const uint32_t distributorSize = distributor.size();
    // Why we didn't use `auto` the line above:
    static_assert(sizeof(distributorSize) == 4);
    SF_CHUNK_INFO chunk;
    std::snprintf(chunk.id, sizeof(chunk.id), "LIST");
    chunk.id_size = 4;
    constexpr std::array<char, 4> listTypeID = { 'I', 'N', 'F', 'O' };
    constexpr std::array<char, 4> distributorTypeID = { 'I', 'D', 'S', 'T' };
    chunk.datalen = sizeof(listTypeID) + sizeof(distributorTypeID)
                    + sizeof(distributorSize) + distributorSize;
    // A trick taken from libsndfile's source code, probably to ensure that
    // the rest of the data stays word-aligned:
    while (chunk.datalen & 3) {
        ++chunk.datalen;
    }
    mDistributorData = std::make_unique<uint8_t[]>(chunk.datalen);
    chunk.data = mDistributorData.get();
    auto data = mDistributorData.get();
    std::memset(chunk.data, 0, chunk.datalen);
    auto pos = 0;

    std::memcpy(data + pos, listTypeID.data(), sizeof(listTypeID));

    pos += sizeof(listTypeID);
    std::memcpy(data + pos, distributorTypeID.data(), sizeof(distributorTypeID));

    pos += sizeof(distributorTypeID);
    std::memcpy(data + pos, &distributorSize, sizeof(distributorSize));

    pos += sizeof(distributorSize);
    std::memcpy(data + pos, distributor.data(), distributorSize);

    const auto result = sf_set_chunk(mFile, &chunk);
    assert(result == SF_ERR_NO_ERROR);
}
} // namespace Test
} // namespace LibImportExport
