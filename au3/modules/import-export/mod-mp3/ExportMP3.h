/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMP3.h

  Dominic Mazzoni

**********************************************************************/

#pragma once

/* --------------------------------------------------------------------------*/

#include "FileNames.h"
#include "Mix.h"
#include "libraries/lib-import-export/ExportPlugin.h"
#include <memory>

#include <lame/lame.h>

#include <wx/dynlib.h>
#include <wx/ffile.h>

enum MP3RateMode : unsigned {
    MODE_SET = 0,
    MODE_VBR,
    MODE_ABR,
    MODE_CBR,
};

#if defined(__WXMSW__) || defined(__WXMAC__)
#define MP3_EXPORT_BUILT_IN 1
#endif

class TranslatableString;

//----------------------------------------------------------------------------
// Get MP3 library version
//----------------------------------------------------------------------------
TranslatableString GetMP3Version(bool prompt);

class ExportMP3 final : public ExportPlugin
{
public:

    ExportMP3();

    bool CheckFileName(wxFileName& filename, int format) const override;

    int GetFormatCount() const override;
    FormatInfo GetFormatInfo(int) const override;

    std::unique_ptr<ExportOptionsEditor>
    CreateOptionsEditor(int, ExportOptionsEditor::Listener* listener) const override;

    std::unique_ptr<ExportProcessor> CreateProcessor(int format) const override;

    std::vector<std::string> GetMimeTypes(int) const override;

    // bool ParseConfig(
    //     int formatIndex, const rapidjson::Value& document, ExportProcessor::Parameters& parameters) const override;
};

class MP3ExportOptionsEditor final : public ExportOptionsEditor
{
    std::vector<ExportOption> mOptions;
    std::unordered_map<int, ExportValue> mValues;
    Listener* mListener{ nullptr };
public:

    explicit MP3ExportOptionsEditor(Listener* listener);

    int GetOptionsCount() const override;
    bool GetOption(int index, ExportOption& option) const override;
    bool SetValue(int id, const ExportValue& value) override;
    bool GetValue(int id, ExportValue& value) const override;
    SampleRateList GetSampleRateList() const override;
    void Load(const audacity::BasicSettings& config) override;
    void Store(audacity::BasicSettings& config) const override;

private:
    void OnModeChange(const std::string& mode);
};

#ifndef DISABLE_DYNAMIC_LOADING_LAME

typedef lame_global_flags* lame_init_t(void);
typedef int lame_init_params_t(lame_global_flags*);
typedef const char* get_lame_version_t(void);

typedef int CDECL lame_encode_buffer_ieee_float_t(
    lame_t gfp,
    const float pcm_l[],
    const float pcm_r[],
    const int nsamples,
    unsigned char* mp3buf,
    const int mp3buf_size);

typedef int CDECL lame_encode_buffer_interleaved_ieee_float_t(
    lame_t gfp,
    const float pcm[],
    const int nsamples,
    unsigned char* mp3buf,
    const int mp3buf_size);

typedef int lame_encode_flush_t(
    lame_global_flags* gf,
    unsigned char* mp3buf,
    int size);

typedef int lame_close_t(lame_global_flags*);

typedef int lame_set_in_samplerate_t(lame_global_flags*, int);
typedef int lame_set_out_samplerate_t(lame_global_flags*, int);
typedef int lame_set_num_channels_t(lame_global_flags*, int);
typedef int lame_set_quality_t(lame_global_flags*, int);
typedef int lame_set_brate_t(lame_global_flags*, int);
typedef int lame_set_VBR_t(lame_global_flags*, vbr_mode);
typedef int lame_set_VBR_q_t(lame_global_flags*, int);
typedef int lame_set_VBR_min_bitrate_kbps_t(lame_global_flags*, int);
typedef int lame_set_mode_t(lame_global_flags*, MPEG_mode);
typedef int lame_set_preset_t(lame_global_flags*, int);
typedef int lame_set_error_protection_t(lame_global_flags*, int);
typedef int lame_set_disable_reservoir_t(lame_global_flags*, int);
typedef int lame_set_bWriteVbrTag_t(lame_global_flags*, int);
typedef size_t lame_get_lametag_frame_t(const lame_global_flags*, unsigned char* buffer, size_t size);
typedef void lame_mp3_tags_fid_t(lame_global_flags*, FILE*);

#endif // DISABLE_DYNAMIC_LOADING_LAME

#if defined(__WXMSW__)
// An alternative solution to give Windows an additional chance of writing the tag before
// falling bato to lame_mp3_tag_fid().  The latter can have DLL sharing issues when mixing
// Debug/Release builds of Audacity and the lame DLL.
typedef unsigned long beWriteInfoTag_t(lame_global_flags*, char*);

// We use this to determine if the user has selected an older, Blade API only, lame_enc.dll
// so we can be more specific about why their library isn't acceptable.
typedef struct    {
    // BladeEnc DLL Version number

    uint8_t byDLLMajorVersion;
    uint8_t byDLLMinorVersion;

    // BladeEnc Engine Version Number

    uint8_t byMajorVersion;
    uint8_t byMinorVersion;

    // DLL Release date

    uint8_t byDay;
    uint8_t byMonth;
    uint16_t wYear;

    // BladeEnc	Homepage URL

    char zHomepage[129];

    uint8_t byAlphaLevel;
    uint8_t byBetaLevel;
    uint8_t byMMXEnabled;

    uint8_t btReserved[125];
} be_version;
typedef void beVersion_t(be_version*);
#endif

class MP3Exporter
{
public:
    enum AskUser
    {
        No,
        Maybe,
        Yes
    };

    MP3Exporter();
    ~MP3Exporter();

#ifndef DISABLE_DYNAMIC_LOADING_LAME
    bool LoadEncoderLibrary(AskUser askuser);
    bool ValidLibraryLoaded();
#endif // DISABLE_DYNAMIC_LOADING_LAME

    /* These global settings keep state over the life of the object */
    void SetMode(int mode);
    void SetBitrate(int rate);
    void SetQuality(int q /*, int r*/);

    /* Virtual methods that must be supplied by library interfaces */

    /* initialize the library interface */
    bool InitLibrary(wxString libpath);
    bool InitLibraryInternal();
    bool InitLibraryExternal(wxString libpath);
    void FreeLibrary();

    /* get library info */
    wxString GetLibraryVersion();
    wxString GetLibraryName();
    wxString GetLibraryPath();
    FileNames::FileTypes GetLibraryTypes();

    /* returns the number of samples PER CHANNEL to send for each call to EncodeBuffer */
    int InitializeStream(unsigned channels, int sampleRate);

    /* In bytes. must be called AFTER InitializeStream */
    int GetOutBufferSize();

    /* returns the number of bytes written. input is interleaved if stereo*/
    int EncodeBuffer(float inbuffer[], unsigned char outbuffer[]);
    int EncodeRemainder(float inbuffer[], int nSamples, unsigned char outbuffer[]);

    int EncodeBufferMono(float inbuffer[], unsigned char outbuffer[]);
    int EncodeRemainderMono(float inbuffer[], int nSamples, unsigned char outbuffer[]);

    int FinishStream(unsigned char outbuffer[]);
    void CancelEncoding();

    bool PutInfoTag(wxFFile& f, wxFileOffset off);

private:
    bool mLibIsExternal;

#ifndef DISABLE_DYNAMIC_LOADING_LAME
    wxString mLibPath;
    wxDynamicLibrary lame_lib;
    bool mLibraryLoaded;
#endif // DISABLE_DYNAMIC_LOADING_LAME

#if defined(__WXMSW__)
    TranslatableString mBladeVersion;
#endif

    bool mEncoding;
    int mMode;
    int mBitrate;
    int mQuality;
    //int mRoutine;

#ifndef DISABLE_DYNAMIC_LOADING_LAME
    /* function pointers to the symbols we get from the library */
    lame_init_t* lame_init;
    lame_init_params_t* lame_init_params;
    lame_encode_buffer_ieee_float_t* lame_encode_buffer_ieee_float;
    lame_encode_buffer_interleaved_ieee_float_t* lame_encode_buffer_interleaved_ieee_float;
    lame_encode_flush_t* lame_encode_flush;
    lame_close_t* lame_close;
    get_lame_version_t* get_lame_version;

    lame_set_in_samplerate_t* lame_set_in_samplerate;
    lame_set_out_samplerate_t* lame_set_out_samplerate;
    lame_set_num_channels_t* lame_set_num_channels;
    lame_set_quality_t* lame_set_quality;
    lame_set_brate_t* lame_set_brate;
    lame_set_VBR_t* lame_set_VBR;
    lame_set_VBR_q_t* lame_set_VBR_q;
    lame_set_VBR_min_bitrate_kbps_t* lame_set_VBR_min_bitrate_kbps;
    lame_set_mode_t* lame_set_mode;
    lame_set_preset_t* lame_set_preset;
    lame_set_error_protection_t* lame_set_error_protection;
    lame_set_disable_reservoir_t* lame_set_disable_reservoir;
    lame_set_bWriteVbrTag_t* lame_set_bWriteVbrTag;
    lame_get_lametag_frame_t* lame_get_lametag_frame;
    lame_mp3_tags_fid_t* lame_mp3_tags_fid;
#if defined(__WXMSW__)
    beWriteInfoTag_t* beWriteInfoTag;
    beVersion_t* beVersion;
#endif
#endif // DISABLE_DYNAMIC_LOADING_LAME

    lame_global_flags* mGF;

    static const int mSamplesPerChunk = 220500;
    // See lame.h/lame_encode_buffer() for further explanation
    // As coded here, this should be the worst case.
    static const int mOutBufferSize
        =mSamplesPerChunk * (320 / 8) / 8 + 4 * 1152 * (320 / 8) / 8 + 512;

    // See MAXFRAMESIZE in libmp3lame/VbrTag.c for explanation of 2880.
    unsigned char mInfoTagBuf[2880];
    size_t mInfoTagLen;
};

class MP3ExportProcessor final : public ExportProcessor
{
    struct
    {
        TranslatableString status;
        unsigned channels;
        double t0;
        double t1;
        MP3Exporter exporter;
        wxFFile outFile;
        ArrayOf<char> id3buffer;
        unsigned long id3len;
        wxFileOffset infoTagPos;
        size_t bufferSize;
        int inSamples;
        std::unique_ptr<Mixer> mixer;
    } context;

public:
    bool Initialize(AudacityProject& project, const Parameters& parameters, const wxFileNameWrapper& filename, double t0, double t1,
                    bool selectedOnly, double sampleRate, unsigned channels, MixerOptions::Downmix* mixerSpec, const Tags* tags) override;

    ExportResult Process(ExportProcessorDelegate& delegate) override;

private:

    static int AskResample(int bitrate, int rate, int lowrate, int highrate);
    static unsigned long AddTags(ArrayOf<char>& buffer, bool* endOfFile, const Tags* tags);
#ifdef USE_LIBID3TAG
    static void AddFrame(struct id3_tag* tp, const wxString& n, const wxString& v, const char* name);
#endif
};
