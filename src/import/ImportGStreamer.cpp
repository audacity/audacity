/**********************************************************************

Audacity: A Digital Audio Editor

ImportGStreamer.cpp

Copyright 2008  LRN
Based on ImportFFmpeg.cpp by LRN

Rework for gstreamer 1.0 by LLL

Licensed under the GNU General Public License v2 or later

*//****************************************************************//**

\class GStreamerImportFileHandle
\brief An ImportFileHandle for GStreamer data

*//****************************************************************//**

\class GStreamerImportPlugin
\brief An ImportPlugin for GStreamer data

*//*******************************************************************/

#include "../Audacity.h"	// needed before GStreamer.h // for USE_* macros

#if defined(USE_GSTREAMER)
#include "ImportGStreamer.h"

#include <wx/window.h>
#include <wx/log.h>

#define DESC XO("GStreamer-compatible files")


// On Windows we don't have configure script to turn this on or off,
// so let's use msw-specific pragma to add required libraries.
// Of course, library search path still has to be updated manually
#if defined(__WXMSW__)
#   pragma comment(lib,"gstreamer-1.0.lib")
#   pragma comment(lib,"gstapp-1.0.lib")
#   pragma comment(lib,"gstbase-1.0.lib")
#   pragma comment(lib,"glib-2.0.lib")
#   pragma comment(lib,"gobject-2.0.lib")
#endif

// all the includes live here by default
#include "../Tags.h"
#include "../WaveTrack.h"

extern "C"
{
//   #include <gio/gio.h>
   #include <gst/app/gstappsink.h>
   #include <gst/audio/audio-format.h>
}

// Convenience macros
#define AUDCTX "audacity::context"
#define GETCTX(o) (GStreamContext *) g_object_get_data(G_OBJECT((o)), AUDCTX)
#define SETCTX(o, c) g_object_set_data(G_OBJECT((o)), AUDCTX, (gpointer) (c))
#define WARN(e, msg) GST_ELEMENT_WARNING((e), STREAM, FAILED, msg, (NULL));

// Capabilities that Audacity can handle
//
// This resolves to: (on little endian)
//
//    "audio/x-raw, "
//    "format = (string) {S16LE, S24_32LE, F32LE}, "
//    "rate = (int) [ 1, max ]",
//    "channels = (int) [ 1, max ]"

static GstStaticCaps supportedCaps =
   GST_STATIC_CAPS(
      GST_AUDIO_CAPS_MAKE(
         "{"
            GST_AUDIO_NE(S16) ", "
            GST_AUDIO_NE(S24_32) ", "
            GST_AUDIO_NE(F32)
         "}"
      )
   );

struct g_mutex_locker
{
   explicit g_mutex_locker(GMutex &mutex_)
      : mutex(mutex_)
   {
      g_mutex_lock(&mutex);
   }

   ~g_mutex_locker()
   {
      g_mutex_unlock(&mutex);
   }

   GMutex &mutex;
};

template<typename T, void(*Fn)(T*)> struct Deleter {
   inline void operator() (void *p) const
   {
      if (p)
         Fn(static_cast<T*>(p));
   }
};
using GstString = std::unique_ptr < gchar, Deleter<void, g_free> > ;
using GErrorHandle = std::unique_ptr < GError, Deleter<GError, g_error_free> > ;

using ParseFn = void (*)(GstMessage *message, GError **gerror, gchar **debug);
inline void GstMessageParse(ParseFn fn, GstMessage *msg, GErrorHandle &err, GstString &debug)
{
   GError *error;
   gchar *string;
   fn(msg, &error, &string);
   err.reset(error);
   debug.reset(string);
}

// Context used for private stream data
struct GStreamContext
{
   GstElement    *mConv{};         // Audio converter
   GstElement    *mSink{};         // Application sink
   bool           mUse{};          // True if this stream should be imported
   NewChannelGroup mChannels;     // Array of WaveTrack pointers, one for each channel
   unsigned       mNumChannels{};  // Number of channels
   gdouble        mSampleRate{};   // Sample rate
   GstString      mType;         // Audio type
   sampleFormat   mFmt{ floatSample };          // Sample format
   gint64         mPosition{};     // Start position of stream
   gint64         mDuration{};     // Duration of stream
   GstElement    *mPipeline{};

   GStreamContext() {}
   ~GStreamContext()
   {
      // Remove the appsink element
      if (mSink)
      {
         gst_bin_remove(GST_BIN(mPipeline), mSink);
      }

      // Remove the audioconvert element
      if (mConv)
      {
         gst_bin_remove(GST_BIN(mPipeline), mConv);
      }
   }
};

// For RAII on gst objects
template<typename T> using GstObjHandle =
   std::unique_ptr < T, Deleter<void, gst_object_unref > > ;

///! Does actual import, returned by GStreamerImportPlugin::Open
class GStreamerImportFileHandle final : public ImportFileHandle
{
public:
   GStreamerImportFileHandle(const wxString & name);
   virtual ~GStreamerImportFileHandle();

   ///! Format initialization
   ///\return true if successful, false otherwise
   bool Init();

   TranslatableString GetFileDescription() override;
   ByteCount GetFileUncompressedBytes() override;

   ///! Called by Import.cpp
   ///\return number of readable audio streams in the file
   wxInt32 GetStreamCount() override;

   ///! Called by Import.cpp
   ///\return array of strings - descriptions of the streams
   const TranslatableStrings &GetStreamInfo() override;

   ///! Called by Import.cpp
   ///\param index - index of the stream in mStreamInfo and mStreams arrays
   ///\param use - true if this stream should be imported, false otherwise
   void SetStreamUsage(wxInt32 index, bool use) override;

   ///! Imports audio
   ///\return import status (see Import.cpp)
   int Import(TrackFactory *trackFactory,
              TrackHolders &outTracks,
              Tags *tags) override;

   // =========================================================================
   // Handled within the gstreamer threads
   // =========================================================================

   ///! Called when a pad-added signal comes in from UriDecodeBin
   ///\param pad - source pad of uridecodebin that will not be serving any data
   void OnPadAdded(GstPad *pad);

   ///! Called when a pad-removed signal comes in from UriDecodeBin
   ///\param pad - source pad of uridecodebin that will not be serving any data
   void OnPadRemoved(GstPad *pad);

   ///! Called when a message comes through GStreamer message bus
   ///\param success - will be set to true if successful
   ///\return true if the loop should be terminated
   bool ProcessBusMessage(bool & success);

   ///! Called when a tag message comes in from the appsink
   ///\param appsink - Specific sink that received the message
   ///\param tags - List of tags
   void OnTag(GstAppSink *appsink, GstTagList *tags);

   ///! Called when a NEW samples are queued
   ///\param c - stream context
   ///\param sample - gstreamer sample
   void OnNewSample(GStreamContext *c, GstSample *sample);

private:
   TranslatableStrings     mStreamInfo;   //!< Array of stream descriptions. Length is the same as mStreams
   Tags                    mTags;         //!< Tags to be passed back to Audacity
   TrackFactory           *mTrackFactory; //!< Factory to create tracks when samples arrive

   GstString               mUri;          //!< URI of file
   GstObjHandle<GstElement> mPipeline;    //!< GStreamer pipeline
   GstObjHandle<GstBus>    mBus;          //!< Message bus
   GstElement             *mDec;          //!< uridecodebin element
   bool                    mAsyncDone;    //!< true = 1st async-done message received

   GMutex                  mStreamsLock;  //!< Mutex protecting the mStreams array
   std::vector<std::unique_ptr<GStreamContext>> mStreams;      //!< Array of pointers to stream contexts
};

/// A representative of GStreamer loader in
/// the Audacity import plugin list
class GStreamerImportPlugin final : public ImportPlugin
{
public:
   ///! Constructor
   GStreamerImportPlugin();

   ///! Destructor
   virtual ~GStreamerImportPlugin();

   TranslatableString GetPluginFormatDescription() override;

   wxString GetPluginStringID() override;

   FileExtensions GetSupportedExtensions() override;

   ///! Probes the file and opens it if appropriate
   std::unique_ptr<ImportFileHandle> Open(
      const wxString &Filename, AudacityProject*) override;
};

// ============================================================================
// Initialization
// ============================================================================

// ----------------------------------------------------------------------------
// Instantiate GStreamerImportPlugin and add to the list of known importers

static
Importer::RegisteredImportPlugin{ "GStreamer",
   []() -> std::unique_ptr< ImportPlugin > {
   wxLogMessage(_TS("Audacity is built against GStreamer version %d.%d.%d-%d"),
                GST_VERSION_MAJOR,
                GST_VERSION_MINOR,
                GST_VERSION_MICRO,
                GST_VERSION_NANO);

   // Initialize gstreamer
   GErrorHandle error;
   bool initError;
   {
      int argc = 0;
      char **argv = NULL;
      GError *ee;
      initError = !gst_init_check(&argc, &argv, &ee);
      error.reset(ee);
   }
   if ( initError )
   {
      wxLogMessage(wxT("Failed to initialize GStreamer. Error %d: %s"),
                   error.get()->code,
                   wxString::FromUTF8(error.get()->message));
      return {};
   }

   guint major, minor, micro, nano;
   gst_version(&major, &minor, &micro, &nano);
   wxLogMessage(wxT("Linked to GStreamer version %d.%d.%d-%d"),
                major,
                minor,
                micro,
                nano);

   // Instantiate plugin
   auto plug = std::make_unique<GStreamerImportPlugin>();

   // No supported extensions...no gstreamer plugins installed
   if (plug->GetSupportedExtensions().size() == 0)
      return {};

   // Add to list of importers
   return std::move(plug);
}() } registered;

// ============================================================================
// GStreamerImportPlugin Class
// ============================================================================

// ----------------------------------------------------------------------------
// Constructor
GStreamerImportPlugin::GStreamerImportPlugin()
:  ImportPlugin( {} )
{
}

// ----------------------------------------------------------------------------
// Destructor
GStreamerImportPlugin::~GStreamerImportPlugin()
{
}

// ----------------------------------------------------------------------------
// Return the plugin description
TranslatableString
GStreamerImportPlugin::GetPluginFormatDescription()
{
   return DESC;
}

// ----------------------------------------------------------------------------
// Return the plugin name
wxString
GStreamerImportPlugin::GetPluginStringID()
{
   return wxT("gstreamer");
}

// Obtains a list of supported extensions from typefind factories
// TODO: improve the list. It is obviously incomplete.
FileExtensions
GStreamerImportPlugin::GetSupportedExtensions()
{
   // We refresh the extensions each time this is called in case the
   // user had installed additional gstreamer plugins while Audacity
   // was active.
   mExtensions.clear();

   // Gather extensions from all factories that support audio
   {
      std::unique_ptr < GList, Deleter<GList, gst_plugin_feature_list_free> >
         factories{ gst_type_find_factory_get_list() };

      for (GList *list = factories.get(); list != NULL; list = g_list_next(list))
      {
         GstTypeFindFactory *factory = GST_TYPE_FIND_FACTORY(list->data);

         // We need the capabilities to determine if it handles audio
         GstCaps *caps = gst_type_find_factory_get_caps(factory);
         if (!caps)
         {
            continue;
         }

         // Check each structure in the caps for audio
         for (guint c = 0, clen = gst_caps_get_size(caps); c < clen; c++)
         {
            // Bypass if it isn't for audio
            GstStructure *str = gst_caps_get_structure(caps, c);
            if (!g_str_has_prefix(gst_structure_get_name(str), "audio"))
            {
               continue;
            }

            // This factory can handle audio, so get the extensions
            const gchar *const *extensions = gst_type_find_factory_get_extensions(factory);
            if (!extensions)
            {
               continue;
            }

            // Add each extension to the list
            for (guint i = 0; extensions[i] != NULL; i++)
            {
               wxString extension = wxString::FromUTF8(extensions[i]);
               if (mExtensions.Index(extension, false) == wxNOT_FOUND)
               {
                  mExtensions.push_back(extension);
               }
            }
         }
      }
   }

   // Get them in a decent order
   mExtensions.Sort();

   // Log it for debugging
   wxString extensions = wxT("Extensions:");
   for (size_t i = 0; i < mExtensions.size(); i++)
   {
      extensions = extensions + wxT(" ") + mExtensions[i];
   }
   wxLogMessage(wxT("%s"), extensions);

   return mExtensions;
}

// ----------------------------------------------------------------------------
// Open the file and return an importer "file handle"
std::unique_ptr<ImportFileHandle> GStreamerImportPlugin::Open(
   const wxString &filename, AudacityProject*)
{
   auto handle = std::make_unique<GStreamerImportFileHandle>(filename);

   // Initialize the handle
   if (!handle->Init())
   {
      return nullptr;
   }

   // This std::move is needed to "upcast" the pointer type
   return std::move(handle);
}

// ============================================================================
// GStreamerImportFileHandle Class
// ============================================================================

// ----------------------------------------------------------------------------
// The following methods/functions run within gstreamer thread contexts.  No
// interaction with wxWidgets should be allowed.  See explanation at the top
// of this file.
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
// Filter out any video streams
//
// LRN found that by doing this here, video streams aren't decoded thus
// reducing the time/processing needed to extract the audio streams.
//
// This "gint" is really GstAutoplugSelectResult enum
static gint
GStreamerAutoplugSelectCallback(GstElement * WXUNUSED(element),
                                GstPad * WXUNUSED(pad),
                                GstCaps * WXUNUSED(caps),
                                GstElementFactory *factory,
                                gpointer WXUNUSED(data))
{
   // Check factory class
   const gchar *fclass = gst_element_factory_get_klass(factory);

   // Skip video decoding
   if (g_strrstr(fclass,"Video"))
   {
      return 2;   // GST_AUTOPLUG_SELECT_SKIP
   }

   return 0;      // GST_AUTOPLUG_SELECT_TRY
}

// ----------------------------------------------------------------------------
// Handle the "pad-added" signal from uridecodebin
static void
GStreamerPadAddedCallback(GstElement * WXUNUSED(element),
                          GstPad *pad,
                          gpointer data)
{
   ((GStreamerImportFileHandle *) data)->OnPadAdded(pad);

   return;
}

// ----------------------------------------------------------------------------
// Handle the "pad-removed" signal from uridecodebin
static void
GStreamerPadRemovedCallback(GstElement * WXUNUSED(element),
                            GstPad *pad,
                            gpointer data)
{
   ((GStreamerImportFileHandle *) data)->OnPadRemoved(pad);

   return;
}

// ----------------------------------------------------------------------------
// Handle the "NEW-sample" signal from uridecodebin
inline void GstSampleUnref(GstSample *p) { gst_sample_unref(p); } // I can't use the static function name directly...

static GstFlowReturn
GStreamerNewSample(GstAppSink *appsink, gpointer data)
{
   // Don't let C++ exceptions propagate through GStreamer
   return GuardedCall< GstFlowReturn > ( [&] {
      GStreamerImportFileHandle *handle = (GStreamerImportFileHandle *)data;
      static GMutex mutex;

      // Get the sample
      std::unique_ptr < GstSample, Deleter< GstSample, GstSampleUnref> >
         sample{ gst_app_sink_pull_sample(appsink) };

      // We must single thread here to prevent concurrent use of the
      // Audacity track functions.
      g_mutex_locker locker{ mutex };

      handle->OnNewSample(GETCTX(appsink), sample.get());

      return GST_FLOW_OK;
   }, MakeSimpleGuard(GST_FLOW_ERROR) );
}

// ----------------------------------------------------------------------------
// AppSink callbacks are used instead of signals to reduce processing
static GstAppSinkCallbacks AppSinkCallbacks =
{
   NULL,                   // eos
   NULL,                   // new_preroll
   GStreamerNewSample      // new_sample
};

static GstAppSinkCallbacks AppSinkBitBucket =
{
   NULL,                   // eos
   NULL,                   // new_preroll
   NULL                    // new_sample
};

inline void GstCapsUnref(GstCaps *p) { gst_caps_unref(p); } // I can't use the static function name directly...
using GstCapsHandle = std::unique_ptr < GstCaps, Deleter<GstCaps, GstCapsUnref> >;

// ----------------------------------------------------------------------------
// Handle the "pad-added" message
void
GStreamerImportFileHandle::OnPadAdded(GstPad *pad)
{
   GStreamContext *c{};

   {
      // Retrieve the stream caps...skip stream if unavailable
      GstCaps *caps = gst_pad_get_current_caps(pad);
      GstCapsHandle handle{ caps };

      if (!caps)
      {
         WARN(mPipeline.get(), ("OnPadAdded: unable to retrieve stream caps"));
         return;
      }

      // Get the caps structure...no need to release
      GstStructure *str = gst_caps_get_structure(caps, 0);
      if (!str)
      {
         WARN(mPipeline.get(), ("OnPadAdded: unable to retrieve caps structure"));
         return;
      }

      // Only accept audio streams...no need to release
      const gchar *name = gst_structure_get_name(str);
      if (!g_strrstr(name, "audio"))
      {
         WARN(mPipeline.get(), ("OnPadAdded: bypassing '%s' stream", name));
         return;
      }

      {
         // Allocate a NEW stream context
         auto uc = std::make_unique<GStreamContext>();
         c = uc.get();
         if (!c)
         {
            WARN(mPipeline.get(), ("OnPadAdded: unable to allocate stream context"));
            return;
         }

         // Set initial state
         c->mUse = true;

         // Always add it to the context list to keep the number of contexts
         // in sync with the number of streams
         g_mutex_locker{ mStreamsLock };
         // Pass the buck from uc
         mStreams.push_back(std::move(uc));
      }

      c->mPipeline = mPipeline.get();

      // Need pointer to context during pad removal (pad-remove signal)
      SETCTX(pad, c);

      // Save the stream's start time and duration
      gst_pad_query_position(pad, GST_FORMAT_TIME, &c->mPosition);
      gst_pad_query_duration(pad, GST_FORMAT_TIME, &c->mDuration);

      // Retrieve the number of channels and validate
      gint channels = -1;
      gst_structure_get_int(str, "channels", &channels);
      if (channels <= 0)
      {
         WARN(mPipeline.get(), ("OnPadAdded: channel count is invalid %d", channels));
         return;
      }
      c->mNumChannels = channels;

      // Retrieve the sample rate and validate
      gint rate = -1;
      gst_structure_get_int(str, "rate", &rate);
      if (rate <= 0)
      {
         WARN(mPipeline.get(), ("OnPadAdded: sample rate is invalid %d", rate));
         return;
      }
      c->mSampleRate = (double)rate;

      c->mType.reset(g_strdup(name));
      if (!c->mType)
      {
         WARN(mPipeline.get(), ("OnPadAdded: unable to allocate audio type"));
         return;
      }

      // Done with capabilities
   }

   // Create audioconvert element
   c->mConv = gst_element_factory_make("audioconvert", NULL);
   if (!c->mConv)
   {
      WARN(mPipeline.get(), ("OnPadAdded: failed to create audioconvert element"));
      return;
   }

   // Create appsink element
   c->mSink = gst_element_factory_make("appsink", NULL);
   if (!c->mSink)
   {
      WARN(mPipeline.get(), ("OnPadAdded: failed to create appsink element"));
      return;
   }
   SETCTX(c->mSink, c);

   // Set the appsink callbacks and add the context pointer
   gst_app_sink_set_callbacks(GST_APP_SINK(c->mSink), &AppSinkCallbacks, this, NULL);

   {
      // Set the capabilities that we desire
      GstCaps *caps = gst_static_caps_get(&supportedCaps);
      GstCapsHandle handle{ caps };
      if (!caps)
      {
         WARN(mPipeline.get(), ("OnPadAdded: failed to create static caps"));
         return;
      }
      gst_app_sink_set_caps(GST_APP_SINK(c->mSink), caps);
   }

   // Do not sync to the clock...process as quickly as possible
   gst_base_sink_set_sync(GST_BASE_SINK(c->mSink), FALSE);

   // Don't drop buffers...allow queue to build unfettered
   gst_app_sink_set_drop(GST_APP_SINK(c->mSink), FALSE);

   // Add both elements to the pipeline
   gst_bin_add_many(GST_BIN(mPipeline.get()), c->mConv, c->mSink, NULL);

   // Link them together
   if (!gst_element_link(c->mConv, c->mSink))
   {
      WARN(mPipeline.get(), ("OnPadAdded: failed to link audioconvert and appsink"));
      return;
   }

   // Link the audiconvert sink pad to the src pad
   GstPadLinkReturn ret = GST_PAD_LINK_OK;
   {
      GstObjHandle<GstPad> convsink{ gst_element_get_static_pad(c->mConv, "sink") };
      if (convsink)
         ret = gst_pad_link(pad, convsink.get());
      if (!convsink || ret != GST_PAD_LINK_OK)
      {
         WARN(mPipeline.get(), ("OnPadAdded: failed to link uridecodebin to audioconvert - %d", ret));
         return;
      }
   }

   // Synchronize audioconvert state with parent
   if (!gst_element_sync_state_with_parent(c->mConv))
   {
      WARN(mPipeline.get(), ("OnPadAdded: unable to sync audioconvert state"));
      return;
   }

   // Synchronize appsink state with parent
   if (!gst_element_sync_state_with_parent(c->mSink))
   {
      WARN(mPipeline.get(), ("OnPadAdded: unable to sync appaink state"));
      return;
   }

   return;
}

// ----------------------------------------------------------------------------
// Process the "pad-removed" signal from uridecodebin
void
GStreamerImportFileHandle::OnPadRemoved(GstPad *pad)
{
   GStreamContext *c = GETCTX(pad);

   // Set audioconvert and appsink states to NULL
   gst_element_set_state(c->mSink, GST_STATE_NULL);
   gst_element_set_state(c->mConv, GST_STATE_NULL);

   // Unlink audioconvert -> appsink
   gst_element_unlink(c->mConv, c->mSink);

   // Remove the pads from the pipeilne
   gst_bin_remove_many(GST_BIN(mPipeline.get()), c->mConv, c->mSink, NULL);

   // And reset context
   c->mConv = NULL;
   c->mSink = NULL;

   return;
}

// ----------------------------------------------------------------------------
// Handle the "NEW-sample" message
void
GStreamerImportFileHandle::OnNewSample(GStreamContext *c, GstSample *sample)
{
   // Allocate NEW tracks
   //
   // It is done here because, at least in the case of chained oggs,
   // not all streams are known ahead of time.
   if (c->mChannels.empty())
   {
      // Get the sample format...no need to release caps or structure
      GstCaps *caps = gst_sample_get_caps(sample);
      GstStructure *str = gst_caps_get_structure(caps, 0);
      const gchar *fmt = gst_structure_get_string(str, "format");
      if (!fmt)
      {
         WARN(mPipeline.get(), ("OnNewSample: missing audio format"));
         return;
      }

      // Determinate sample format based on negotiated format
      if (strcmp(fmt, GST_AUDIO_NE(S16)) == 0)
      {
         c->mFmt = int16Sample;
      }
      else if (strcmp(fmt, GST_AUDIO_NE(S24_32)) == 0)
      {
         c->mFmt = int24Sample;
      }
      else if (strcmp(fmt, GST_AUDIO_NE(F32)) == 0)
      {
         c->mFmt = floatSample;
      }
      else
      {
         // This shouldn't really happen since audioconvert will only give us
         // the formats we said we could handle.
         WARN(mPipeline.get(), ("OnNewSample: unrecognized sample format %s", fmt));
         return;
      }

      // Allocate the track array
      c->mChannels.resize(c->mNumChannels);
      if (c->mChannels.size() != c->mNumChannels)
      {
         WARN(mPipeline.get(), ("OnNewSample: unable to allocate track array"));
         return;
      }

      // Allocate all channels
      for (int ch = 0; ch < c->mNumChannels; ch++)
      {
         // Create a track
         c->mChannels[ch] = mTrackFactory->NewWaveTrack(c->mFmt, c->mSampleRate);
         if (!c->mChannels[ch])
         {
            WARN(mPipeline.get(), ("OnNewSample: unable to create track"));
            return;
         }
      }
   }

   // Get the buffer for the sample...no need to release
   GstBuffer *buffer = gst_sample_get_buffer(sample);
   if (!buffer)
   {
      // No buffer...not sure if this is an error or not,
      // but we can't do anything else, so just bail silently.
      return;
   }

   // Map the buffer
   GstMapInfo info;
   if (!gst_buffer_map(buffer, &info, GST_MAP_READ))
   {
      WARN(mPipeline.get(), ("OnNewSample: mapping buffer failed"));
      return;
   }
   auto cleanup = finally([&]{
      // Release buffer
      gst_buffer_unmap(buffer, &info);
   });

   // Cache a few items
   auto nChannels = c->mNumChannels;
   sampleFormat fmt = c->mFmt;
   samplePtr data = (samplePtr) info.data;
   size_t samples = info.size / nChannels / SAMPLE_SIZE(fmt);

   // Add sample data to tracks...depends on interleaved src data
   for (int chn = 0; chn < nChannels; chn++)
   {
      // Append one channel
      c->mChannels[chn]->Append(data,
                                fmt,
                                samples,
                                nChannels);

      // Bump src to next channel
      data += SAMPLE_SIZE(fmt);
   }

   return;
}

// ----------------------------------------------------------------------------
// The following methods run within the main thread.
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
// Constructor
GStreamerImportFileHandle::GStreamerImportFileHandle(const wxString & name)
:  ImportFileHandle(name)
{
   mDec = NULL;
   mTrackFactory = NULL;
   mAsyncDone = false;

   g_mutex_init(&mStreamsLock);
}

// ----------------------------------------------------------------------------
// Destructor
GStreamerImportFileHandle::~GStreamerImportFileHandle()
{
   // Make sure the pipeline isn't running
   if (mPipeline)
   {
      gst_element_set_state(mPipeline.get(), GST_STATE_NULL);
   }

   // Delete all of the contexts
   if (mStreams.size())
   {
      // PRL: is the FIFO destruction order important?
      // If not, then you could simply clear mStreams.

      {
         g_mutex_locker locker{ mStreamsLock };
         while (mStreams.size() > 0)
         {
            // remove context from the array
            mStreams.erase(mStreams.begin());
         }
      }

      // Done with the context array
   }

   // Release the decoder
   if (mDec != NULL)
   {
      gst_bin_remove(GST_BIN(mPipeline.get()), mDec);
   }

   g_mutex_clear(&mStreamsLock);
}

// ----------------------------------------------------------------------------
// Return number of readable audio streams in the file
wxInt32
GStreamerImportFileHandle::GetStreamCount()
{
   return mStreamInfo.size();
}

// ----------------------------------------------------------------------------
// Return array of strings - descriptions of the streams
const TranslatableStrings &
GStreamerImportFileHandle::GetStreamInfo()
{
   return mStreamInfo;
}

// ----------------------------------------------------------------------------
// Mark streams to process as selected by the user
void
GStreamerImportFileHandle::SetStreamUsage(wxInt32 index, bool use)
{
   g_mutex_locker locker{ mStreamsLock };
   if ((guint) index < mStreams.size())
   {
      GStreamContext *c = mStreams[index].get();
      c->mUse = use;
   }
}

// ----------------------------------------------------------------------------
// Initialize importer
bool
GStreamerImportFileHandle::Init()
{
   // Create a URI from the filename
   mUri.reset(g_strdup_printf("file:///%s", mFilename.ToUTF8().data()));
   if (!mUri)
   {
      wxLogMessage(wxT("GStreamerImport couldn't create URI"));
      return false;
   }

   // Create a pipeline
   mPipeline.reset(gst_pipeline_new("pipeline"));

   // Get its bus
   mBus.reset(gst_pipeline_get_bus(GST_PIPELINE(mPipeline.get())));

   // Create uridecodebin and set up signal handlers
   mDec = gst_element_factory_make("uridecodebin", "decoder");
   g_signal_connect(mDec, "autoplug-select", G_CALLBACK(GStreamerAutoplugSelectCallback), (gpointer) this);
   g_signal_connect(mDec, "pad-added", G_CALLBACK(GStreamerPadAddedCallback), (gpointer) this);
   g_signal_connect(mDec, "pad-removed", G_CALLBACK(GStreamerPadRemovedCallback), (gpointer) this);

   // Set the URI
   g_object_set(G_OBJECT(mDec), "uri", mUri, NULL);

   // Add the decoder to the pipeline
   if (!gst_bin_add(GST_BIN(mPipeline.get()), mDec))
   {
      AudacityMessageBox(
         XO("Unable to add decoder to pipeline"),
         XO("GStreamer Importer"));

      // Cleanup expected to occur in destructor
      return false;
   }

   // Run the pipeline
   GstStateChangeReturn state = gst_element_set_state(mPipeline.get(), GST_STATE_PAUSED);
   if (state == GST_STATE_CHANGE_FAILURE)
   {
      AudacityMessageBox(
         XO("Unable to set stream state to paused."),
         XO("GStreamer Importer"));
      return false;
   }

   // Collect info while the stream is prerolled
   //
   // Unfortunately, for some files this may cause a slight "pause" in the GUI
   // without a progress dialog appearing.  Not much can be done about it other
   // than throwing up an additional progress dialog and displaying two dialogs
   // may be confusing to the users.

   // Process messages until we get an error or the ASYNC_DONE message is received
   bool success;
   while (ProcessBusMessage(success) && success)
   {
      // Give wxWidgets a chance to do housekeeping
      wxSafeYield();
   }

   // Build the stream info array
   g_mutex_locker locker{ mStreamsLock };
   for (guint i = 0; i < mStreams.size(); i++)
   {
      GStreamContext *c = mStreams[i].get();

      // Create stream info string
      auto strinfo = XO("Index[%02d], Type[%s], Channels[%d], Rate[%d]")
         .Format(
            (unsigned int) i,
            wxString::FromUTF8(c->mType.get()),
            (int) c->mNumChannels,
            (int) c->mSampleRate );
      mStreamInfo.push_back(strinfo);
   }

   return success;
}

// ----------------------------------------------------------------------------
// Return file dialog filter description
TranslatableString
GStreamerImportFileHandle::GetFileDescription()
{
   return DESC;
}

// ----------------------------------------------------------------------------
// Return number of uncompressed bytes in file...doubtful this is possible
auto
GStreamerImportFileHandle::GetFileUncompressedBytes() -> ByteCount
{
   return 0;
}

// ----------------------------------------------------------------------------
// Import streams
int
GStreamerImportFileHandle::Import(TrackFactory *trackFactory,
                                  TrackHolders &outTracks,
                                  Tags *tags)
{
   outTracks.clear();

   // Save track factory pointer
   mTrackFactory = trackFactory;

   // Create the progress dialog
   CreateProgress();

   // Block streams that are to be bypassed
   bool haveStreams = false;
   {
      g_mutex_locker locker{ mStreamsLock };
      for (guint i = 0; i < mStreams.size(); i++)
      {
         GStreamContext *c = mStreams[i].get();

         // Did the user choose to skip this stream?
         if (!c->mUse)
         {
            // Get the audioconvert sink pad and unlink
            {
               GstObjHandle<GstPad> convsink{ gst_element_get_static_pad(c->mConv, "sink") };

               {
                  GstObjHandle<GstPad> convpeer{ gst_pad_get_peer(convsink.get()) };
                  gst_pad_unlink(convpeer.get(), convsink.get());
               }

               // Set bitbucket callbacks so the prerolled sample won't get processed
               // when we change the state to PLAYING
               gst_app_sink_set_callbacks(GST_APP_SINK(c->mSink), &AppSinkBitBucket, this, NULL);

               // Set state to playing for conv and sink so EOS gets processed
               gst_element_set_state(c->mConv, GST_STATE_PLAYING);
               gst_element_set_state(c->mSink, GST_STATE_PLAYING);

               // Send an EOS event to the pad to force them to drain
               gst_pad_send_event(convsink.get(), gst_event_new_eos());

               // Resync state with pipeline
               gst_element_sync_state_with_parent(c->mConv);
               gst_element_sync_state_with_parent(c->mSink);

               // Done with the pad
            }

            // Unlink audioconvert and appsink
            gst_element_unlink(c->mConv, c->mSink);

            // Remove them from the bin
            gst_bin_remove_many(GST_BIN(mPipeline.get()), c->mConv, c->mSink, NULL);

            // All done with them
            c->mConv = NULL;
            c->mSink = NULL;

            continue;
         }

         // We have a stream to process
         haveStreams = true;
      }
   }

   // Can't do much if we don't have any streams to process
   if (!haveStreams)
   {
      AudacityMessageBox(
         XO("File doesn't contain any audio streams."),
         XO("GStreamer Importer"));
      return ProgressResult::Failed;
   }

   // Get the ball rolling...
   GstStateChangeReturn state = gst_element_set_state(mPipeline.get(), GST_STATE_PLAYING);
   if (state == GST_STATE_CHANGE_FAILURE)
   {
      AudacityMessageBox(
         XO("Unable to import file, state change failed."),
         XO("GStreamer Importer"));
      return ProgressResult::Failed;
   }

   // Get the duration of the stream
   gint64 duration;
   gst_element_query_duration(mPipeline.get(), GST_FORMAT_TIME, &duration);

   // Handle bus messages and update progress while files is importing
   bool success = true;
   int updateResult = ProgressResult::Success;
   while (ProcessBusMessage(success) && success && updateResult == ProgressResult::Success)
   {
      gint64 position;

      // Update progress indicator and give user chance to abort
      if (gst_element_query_position(mPipeline.get(), GST_FORMAT_TIME, &position))
      {
         updateResult = mProgress->Update((wxLongLong_t) position,
                                          (wxLongLong_t) duration);
      }
   }

   // Disable pipeline
   gst_element_set_state(mPipeline.get(), GST_STATE_NULL);

   // Something bad happened
   if (!success || updateResult == ProgressResult::Failed || updateResult == ProgressResult::Cancelled)
   {
      return updateResult;
   }

   // Grab the streams lock
   g_mutex_locker locker{ mStreamsLock };

   // Copy audio from mChannels to newly created tracks (destroying mChannels in process)
   int trackindex = 0;
   for (guint s = 0; s < mStreams.size(); s++)
   {
      GStreamContext *c = mStreams[s].get();
      if (c->mNumChannels)
      {
         for (int ch = 0; ch < c->mNumChannels; ch++)
            c->mChannels[ch]->Flush();
         outTracks.push_back(std::move(c->mChannels));
      }
   }

   // Set any tags found in the stream
   *tags = mTags;

   return updateResult;
}

// ----------------------------------------------------------------------------
// Message handlers
// ----------------------------------------------------------------------------

inline void GstMessageUnref(GstMessage *p) { gst_message_unref(p); }

// ----------------------------------------------------------------------------
// Retrieve and process a bus message
bool
GStreamerImportFileHandle::ProcessBusMessage(bool & success)
{
   bool cont = true;

   // Default to no errors
   success = true;

   // Get the next message
   std::unique_ptr < GstMessage, Deleter < GstMessage, GstMessageUnref > >
      msg{ gst_bus_timed_pop(mBus.get(), 100 * GST_MSECOND) };

   if (!msg)
   {
      // Timed out...not an error
      return cont;
   }

#if defined(_DEBUG)
   {
      GstString objname;
      if (msg->src != NULL)
      {
         objname.reset(gst_object_get_name(msg->src));
      }

      wxLogMessage(wxT("GStreamer: Got %s%s%s"),
         wxString::FromUTF8(GST_MESSAGE_TYPE_NAME(msg.get())),
         objname ? wxT(" from ") : wxT(""),
         objname ? wxString::FromUTF8(objname.get()) : wxT(""));
   }
#endif

   // Handle based on message type
   switch (GST_MESSAGE_TYPE(msg.get()))
   {
      // Handle error message from gstreamer
      case GST_MESSAGE_ERROR:
      {
         GErrorHandle err;
         GstString debug;

         GstMessageParse(gst_message_parse_error, msg.get(), err, debug);
         if (err)
         {
            wxString m;

            m.Printf(wxT("%s%s%s"),
               wxString::FromUTF8(err.get()->message),
               debug ? wxT("\n") : wxT(""),
               debug ? wxString::FromUTF8(debug.get()) : wxT(""));
            auto msg = XO("GStreamer Error: %s").Format( m );
#if defined(_DEBUG)
            AudacityMessageBox( msg );
#else
            wxLogMessage( msg.Debug() );
#endif
         }

         success = false;
         cont = false;
      }
      break;

      // Handle warning message from gstreamer
      case GST_MESSAGE_WARNING:
      {
         GErrorHandle err;
         GstString debug;
         GstMessageParse(gst_message_parse_warning, msg.get(), err, debug);

         if (err)
         {
            wxLogMessage(wxT("GStreamer Warning: %s%s%s"),
                         wxString::FromUTF8(err.get()->message),
                         debug ? wxT("\n") : wxT(""),
                         debug ? wxString::FromUTF8(debug.get()) : wxT(""));
         }
      }
      break;

      // Handle warning message from gstreamer
      case GST_MESSAGE_INFO:
      {
         GErrorHandle err;
         GstString debug;

         GstMessageParse(gst_message_parse_info, msg.get(), err, debug);
         if (err)
         {
            wxLogMessage(wxT("GStreamer Info: %s%s%s"),
                         wxString::FromUTF8(err.get()->message),
                         debug ? wxT("\n") : wxT(""),
                         debug ? wxString::FromUTF8(debug.get()) : wxT(""));
         }
      }
      break;

      // Handle metadata tags
      case GST_MESSAGE_TAG:
      {
         GstTagList *tags = NULL;
         auto cleanup = finally([&]{
            // Done with list
            if(tags) gst_tag_list_unref(tags);
         });

         // Retrieve tag list from message...just ignore failure
         gst_message_parse_tag(msg.get(), &tags);
         if (tags)
         {
            // Go process the list
            OnTag(GST_APP_SINK(GST_MESSAGE_SRC(msg.get())), tags);
         }
      }
      break;

      // Pre-roll is done...will happen for each group
      // (like with chained OGG files)
      case GST_MESSAGE_ASYNC_DONE:
      {
         // If this is the first async-done message, then tell
         // caller to end loop, but leave it active so that
         // gstreamer threads can still queue up.
         //
         // We'll receive multiple async-done messages for chained
         // ogg files, so ignore the message the 2nd and subsequent
         // occurrences.
         if (!mAsyncDone)
         {
            cont = false;
            mAsyncDone = true;
         }

      }
      break;

      // End of the stream (and all sub-streams)
      case GST_MESSAGE_EOS:
      {
         // Terminate loop
         cont = false;
      }
      break;
   }

   return cont;
}

// ----------------------------------------------------------------------------
// Handle the "tag" message
void
GStreamerImportFileHandle::OnTag(GstAppSink * WXUNUSED(appsink), GstTagList *tags)
{
   // Collect all of the associates tags
   for (guint i = 0, icnt = gst_tag_list_n_tags(tags); i < icnt; i++)
   {
      wxString string;

      // Get tag name...should always succeed...no need to release
      const gchar *name = gst_tag_list_nth_tag_name(tags, i);
      if (!name)
      {
         continue;
      }

      // For each tag, determine its type and retrieve if possible
      for (guint j = 0, jcnt = gst_tag_list_get_tag_size(tags, name); j < jcnt; j++)
      {
         const GValue *val;

         val = gst_tag_list_get_value_index(tags, name, j);

         if (G_VALUE_HOLDS_STRING(val))
         {
            string = wxString::FromUTF8(g_value_get_string(val));
         }
         else if (G_VALUE_HOLDS_UINT(val))
         {
            string.Printf(wxT("%u"), (unsigned int) g_value_get_uint(val));
         }
         else if (G_VALUE_HOLDS_DOUBLE(val))
         {
            string.Printf(wxT("%g"), g_value_get_double(val));
         }
         else if (G_VALUE_HOLDS_BOOLEAN(val))
         {
            string = g_value_get_boolean(val) ? wxT("true") : wxT("false");
         }
         else if (GST_VALUE_HOLDS_DATE_TIME(val))
         {
            GstDateTime *dt = (GstDateTime *) g_value_get_boxed(val);
            GstString str{ gst_date_time_to_iso8601_string(dt) };
            string = wxString::FromUTF8(str.get());
         }
         else if (G_VALUE_HOLDS(val, G_TYPE_DATE))
         {
            GstString str{ gst_value_serialize(val) };
            string = wxString::FromUTF8(str.get());
         }
         else
         {
            wxLogMessage(wxT("Tag %s has unhandled type: %s"),
                         wxString::FromUTF8(name),
                         wxString::FromUTF8(G_VALUE_TYPE_NAME(val)));
            continue;
         }

         // Translate known tag names
         wxString tag;
         if (strcmp(name, GST_TAG_TITLE) == 0)
         {
            tag = TAG_TITLE;
         }
         else if (strcmp(name, GST_TAG_ARTIST) == 0)
         {
            tag = TAG_ARTIST;
         }
         else if (strcmp(name, GST_TAG_ALBUM) == 0)
         {
            tag = TAG_ALBUM;
         }
         else if (strcmp(name, GST_TAG_TRACK_NUMBER) == 0)
         {
            tag = TAG_TRACK;
         }
         else if (strcmp(name, GST_TAG_DATE) == 0)
         {
            tag = TAG_YEAR;
         }
         else if (strcmp(name, GST_TAG_GENRE) == 0)
         {
            tag = TAG_GENRE;
         }
         else if (strcmp(name, GST_TAG_COMMENT) == 0)
         {
            tag = TAG_COMMENTS;
         }
         else
         {
            tag = wxString::FromUTF8(name);
         }

         if (jcnt > 1)
         {
            tag.Printf(wxT("%s:%d"), tag, j);
         }

         // Store the tag
         mTags.SetTag(tag, string);
      }
   }
}
#endif //USE_GSTREAMER
