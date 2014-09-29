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

#include "../Audacity.h"	// needed before GStreamer.h
#include <wx/window.h>
#include <wx/log.h>

#if defined(USE_GSTREAMER)

#define DESC _("GStreamer-compatible files")


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
#include "../SampleFormat.h"
#include "../Tags.h"
#include "../Internat.h"
#include "../WaveTrack.h"
#include "Import.h"
#include "ImportPlugin.h"
#include "ImportGStreamer.h"

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
// This resolves to: (on little endien)
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

// Context used for private stream data
struct GStreamContext
{
   GstElement    *mConv;         // Audio converter
   GstElement    *mSink;         // Application sink
   bool           mUse;          // True if this stream should be imported
   WaveTrack    **mChannels;     // Array of WaveTrack pointers, one for each channel
   gint           mNumChannels;  // Number of channels
   gdouble        mSampleRate;   // Sample rate
   gchar         *mType;         // Audio type
   sampleFormat   mFmt;          // Sample format
   gint64         mPosition;     // Start position of stream
   gint64         mDuration;     // Duration of stream
};

///! Does actual import, returned by GStreamerImportPlugin::Open
class GStreamerImportFileHandle : public ImportFileHandle
{
public:
   GStreamerImportFileHandle(const wxString & name);
   virtual ~GStreamerImportFileHandle();

   ///! Format initialization
   ///\return true if successful, false otherwise
   bool Init();

   wxString GetFileDescription();
   int GetFileUncompressedBytes();

   ///! Called by Import.cpp
   ///\return number of readable audio streams in the file
   wxInt32 GetStreamCount();

   ///! Called by Import.cpp
   ///\return array of strings - descriptions of the streams
   wxArrayString *GetStreamInfo();

   ///! Called by Import.cpp
   ///\param index - index of the stream in mStreamInfo and mStreams arrays
   ///\param use - true if this stream should be imported, false otherwise
   void SetStreamUsage(wxInt32 index, bool use);

   ///! Imports audio
   ///\return import status (see Import.cpp)
   int Import(TrackFactory *trackFactory,
              Track ***outTracks,
              int *outNumTracks,
              Tags *tags);

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

   ///! Called when a new samples are queued
   ///\param c - stream context
   ///\param sample - gstreamer sample
   void OnNewSample(GStreamContext *c, GstSample *sample);

private:
   wxArrayString           mStreamInfo;   //!< Array of stream descriptions. Length is the same as mStreams
   Tags                    mTags;         //!< Tags to be passed back to Audacity
   TrackFactory           *mTrackFactory; //!< Factory to create tracks when samples arrive

   gchar                  *mUri;          //!< URI of file
   GstElement             *mPipeline;     //!< GStreamer pipeline
   GstBus                 *mBus;          //!< Message bus
   GstElement             *mDec;          //!< uridecodebin element
   bool                    mAsyncDone;    //!< true = 1st async-done message received

   GMutex                  mStreamsLock;  //!< Mutex protecting the mStreams array
   GPtrArray              *mStreams;      //!< Array of pointers to stream contexts
};

/// A representative of GStreamer loader in
/// the Audacity import plugin list
class GStreamerImportPlugin : public ImportPlugin
{
public:
   ///! Constructor
   GStreamerImportPlugin();

   ///! Destructor
   virtual ~GStreamerImportPlugin();

   wxString GetPluginFormatDescription();

   wxString GetPluginStringID();

   wxArrayString GetSupportedExtensions();

   ///! Probes the file and opens it if appropriate
   ImportFileHandle *Open(wxString Filename);
};

// ============================================================================
// Initialization
// ============================================================================

// ----------------------------------------------------------------------------
// Instantiate GStreamerImportPlugin and add to the list of known importers
void
GetGStreamerImportPlugin(ImportPluginList *importPluginList,
                         UnusableImportPluginList * WXUNUSED(unusableImportPluginList))
{
   wxLogMessage(wxT("Audacity is built against GStreamer version %d.%d.%d-%d"),
                GST_VERSION_MAJOR,
                GST_VERSION_MINOR,
                GST_VERSION_MICRO,
                GST_VERSION_NANO);

   // Initializa gstreamer
   GError *error;
   int argc = 0;
   char **argv = NULL;
   if (!gst_init_check(&argc, &argv, &error))
   {
      wxLogMessage(wxT("Failed to initialize GStreamer. Error %d: %s"),
                   error->code,
                   wxString::FromUTF8(error->message).c_str());
      g_error_free(error);
      return;
   }

   guint major, minor, micro, nano;
   gst_version(&major, &minor, &micro, &nano);
   wxLogMessage(wxT("Linked to GStreamer version %d.%d.%d-%d"),
                major,
                minor,
                micro,
                nano);

   // Instantiate plugin
   GStreamerImportPlugin *plug = new GStreamerImportPlugin();

   // No supported extensions...no gstreamer plugins installed
   if (plug->GetSupportedExtensions().GetCount() == 0)
   {
      delete plug;
      return;
   }

   // Add to list of importers
   importPluginList->Append(plug);
}

// ============================================================================
// GStreamerImportPlugin Class
// ============================================================================

// ----------------------------------------------------------------------------
// Constructor
GStreamerImportPlugin::GStreamerImportPlugin()
:  ImportPlugin(wxArrayString())
{
}

// ----------------------------------------------------------------------------
// Destructor
GStreamerImportPlugin::~GStreamerImportPlugin()
{
}

// ----------------------------------------------------------------------------
// Return the plugin description
wxString
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
wxArrayString
GStreamerImportPlugin::GetSupportedExtensions()
{
   // We refresh the extensions each time this is called in case the
   // user had installed additional gstreamer plugins while Audacity
   // was active.
   mExtensions.Empty();

   // Gather extensions from all factories that support audio
   GList *factories = gst_type_find_factory_get_list();
   for (GList *list = factories; list != NULL; list = g_list_next(list))
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
            if (mExtensions.Index(extension.c_str(), false) == wxNOT_FOUND)
            {
               mExtensions.Add(extension);
            }
         }
      }
   }
   gst_plugin_feature_list_free(factories);

   // Get them in a decent order
   mExtensions.Sort();

   // Log it for debugging
   wxString extensions = wxT("Extensions:");
   for (size_t i = 0; i < mExtensions.GetCount(); i++)
   {
      extensions = extensions + wxT(" ") + mExtensions[i];
   }
   wxLogMessage(wxT("%s"), extensions.c_str());

   return mExtensions;
}

// ----------------------------------------------------------------------------
// Open the file and return an importer "file handle"
ImportFileHandle *
GStreamerImportPlugin::Open(wxString filename)
{
   GStreamerImportFileHandle *handle = new GStreamerImportFileHandle(filename);

   // Initialize the handle
   if (!handle->Init())
   {
      delete handle;
      return NULL;
   }

   return handle;
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
// Handle the "new-sample" signal from uridecodebin
static GstFlowReturn
GStreamerNewSample(GstAppSink *appsink, gpointer data)
{
   GStreamerImportFileHandle *handle = (GStreamerImportFileHandle *) data;
   static GMutex mutex;

   // Get the sample
   GstSample *sample = gst_app_sink_pull_sample(appsink);

   // We must single thread here to prevent concurrent use of the
   // Audacity track functions.
   g_mutex_lock(&mutex);
   handle->OnNewSample(GETCTX(appsink), sample);
   g_mutex_unlock(&mutex);

   // Release the sample
   gst_sample_unref(sample);

   return GST_FLOW_OK;
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

// ----------------------------------------------------------------------------
// Handle the "pad-added" message
void
GStreamerImportFileHandle::OnPadAdded(GstPad *pad)
{
   // Retrieve the stream caps...skip stream if unavailable
   GstCaps *caps = gst_pad_get_current_caps(pad);
   if (!caps)
   {
      WARN(mPipeline, ("OnPadAdded: unable to retrieve stream caps"));
      return;
   }

   // Get the caps structure...no need to release
   GstStructure *str = gst_caps_get_structure(caps, 0);
   if (!str)
   {
      WARN(mPipeline, ("OnPadAdded: unable to retrieve caps structure"));
      gst_caps_unref(caps);
      return;
   }

   // Only accept audio streams...no need to release
   const gchar *name = gst_structure_get_name(str);
   if (!g_strrstr(name, "audio"))
   {
      WARN(mPipeline, ("OnPadAdded: bypassing '%s' stream", name));
      gst_caps_unref(caps);
      return;
   }

   // Allocate a new stream context
   GStreamContext *c = g_new0(GStreamContext, 1);
   if (!c)
   {
      WARN(mPipeline, ("OnPadAdded: unable to allocate stream context"));
      gst_caps_unref(caps);
      return;
   }

   // Set initial state
   c->mUse = true;

   // Always add it to the context list to keep the number of contexts
   // in sync with the number of streams
   g_mutex_lock(&mStreamsLock);
   g_ptr_array_add(mStreams, c);
   g_mutex_unlock(&mStreamsLock);

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
      WARN(mPipeline, ("OnPadAdded: channel count is invalid %d", channels));
      gst_caps_unref(caps);
      return;
   }
   c->mNumChannels = channels;

   // Retrieve the sample rate and validate
   gint rate = -1;
   gst_structure_get_int(str, "rate", &rate);
   if (rate <= 0)
   {
      WARN(mPipeline, ("OnPadAdded: sample rate is invalid %d", rate));
      gst_caps_unref(caps);
      return;
   }
   c->mSampleRate = (double) rate;

   c->mType = g_strdup(name);
   if (c->mType == NULL)
   {
      WARN(mPipeline, ("OnPadAdded: unable to allocate audio type"));
      gst_caps_unref(caps);
      return;
   }

   // Done with capabilities
   gst_caps_unref(caps);

   // Create audioconvert element
   c->mConv = gst_element_factory_make("audioconvert", NULL);
   if (!c->mConv)
   {
      WARN(mPipeline, ("OnPadAdded: failed to create audioconvert element"));
      return;
   }

   // Create appsink element
   c->mSink = gst_element_factory_make("appsink", NULL);
   if (!c->mSink)
   {
      WARN(mPipeline, ("OnPadAdded: failed to create appsink element"));
      return;
   }
   SETCTX(c->mSink, c);

   // Set the appsink callbacks and add the context pointer
   gst_app_sink_set_callbacks(GST_APP_SINK(c->mSink), &AppSinkCallbacks, this, NULL);

   // Set the capabilities that we desire
   caps = gst_static_caps_get(&supportedCaps);
   if (!caps)
   {
      WARN(mPipeline, ("OnPadAdded: failed to create static caps"));
      return;
   }
   gst_app_sink_set_caps(GST_APP_SINK(c->mSink), caps);
   gst_caps_unref(caps);

   // Do not sync to the clock...process as quickly as possible
   gst_base_sink_set_sync(GST_BASE_SINK(c->mSink), FALSE);

   // Don't drop buffers...allow queue to build unfettered
   gst_app_sink_set_drop(GST_APP_SINK(c->mSink), FALSE);

   // Add both elements to the pipeline
   gst_bin_add_many(GST_BIN(mPipeline), c->mConv, c->mSink, NULL);

   // Link them together
   if (!gst_element_link(c->mConv, c->mSink))
   {
      WARN(mPipeline, ("OnPadAdded: failed to link autioconvert and appsink"));
      return;
   }

   // Link the audiconvert sink pad to the src pad
   GstPadLinkReturn ret = GST_PAD_LINK_OK;
   GstPad *convsink = gst_element_get_static_pad(c->mConv, "sink");
   if (convsink)
   {
      ret = gst_pad_link(pad, convsink);
      gst_object_unref(convsink);
   }
   if (!convsink || ret != GST_PAD_LINK_OK)
   {
      WARN(mPipeline, ("OnPadAdded: failed to link uridecodebin to audioconvert - %d", ret));
      return;
   }

   // Synchronize audioconvert state with parent
   if (!gst_element_sync_state_with_parent(c->mConv))
   {
      WARN(mPipeline, ("OnPadAdded: unable to sync audioconvert state"));
      return;
   }

   // Synchronize appsink state with parent
   if (!gst_element_sync_state_with_parent(c->mSink))
   {
      WARN(mPipeline, ("OnPadAdded: unable to sync appaink state"));
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
   gst_bin_remove_many(GST_BIN(mPipeline), c->mConv, c->mSink, NULL);

   // And reset context
   c->mConv = NULL;
   c->mSink = NULL;

   return;
}

// ----------------------------------------------------------------------------
// Handle the "new-sample" message
void
GStreamerImportFileHandle::OnNewSample(GStreamContext *c, GstSample *sample)
{
   // Allocate new tracks
   //
   // It is done here because, at least in the case of chained oggs,
   // not all streams are known ahead of time.
   if (c->mChannels == NULL)
   {
      // Get the sample format...no need to release caps or structure
      GstCaps *caps = gst_sample_get_caps(sample);
      GstStructure *str = gst_caps_get_structure(caps, 0);
      const gchar *fmt = gst_structure_get_string(str, "format");
      if (!fmt)
      {
         WARN(mPipeline, ("OnNewSample: missing audio format"));
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
         WARN(mPipeline, ("OnNewSample: unrecognized sample format %s", fmt));
         return;
      }

      // Allocate the track array
      c->mChannels = new WaveTrack *[c->mNumChannels];
      if (!c->mChannels)
      {
         WARN(mPipeline, ("OnNewSample: unable to allocate track array"));
         return;
      }

      // Allocate all channels
      for (int ch = 0; ch < c->mNumChannels; ch++)
      {
         // Create a track
         c->mChannels[ch] = mTrackFactory->NewWaveTrack(c->mFmt, c->mSampleRate);
         if (!c->mChannels[ch])
         {
            WARN(mPipeline, ("OnNewSample: unable to create track"));
            return;
         }
      }

      // Set to stereo if there's exactly 2 channels
      if (c->mNumChannels == 2)
      {
         c->mChannels[0]->SetChannel(Track::LeftChannel);
         c->mChannels[1]->SetChannel(Track::RightChannel);
         c->mChannels[0]->SetLinked(true);
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
      WARN(mPipeline, ("OnNewSample: mapping buffer failed"));
      return;
   }

   // Cache a few items
   int nChannels = c->mNumChannels;
   sampleFormat fmt = c->mFmt;
   samplePtr data = (samplePtr) info.data;
   sampleCount samples = info.size / nChannels / SAMPLE_SIZE(fmt);

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

   // Release buffer
   gst_buffer_unmap(buffer, &info);

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
   mUri = NULL;
   mPipeline = NULL;
   mBus = NULL;
   mDec = NULL;
   mTrackFactory = NULL;
   mAsyncDone = false;

   g_mutex_init(&mStreamsLock);
   mStreams = NULL;
}

// ----------------------------------------------------------------------------
// Destructor
GStreamerImportFileHandle::~GStreamerImportFileHandle()
{
   // Make sure the pipeline isn't running
   if (mPipeline)
   {
      gst_element_set_state(mPipeline, GST_STATE_NULL);
   }

   // Delete all of the contexts
   if (mStreams)
   {
      g_mutex_lock(&mStreamsLock);
      while (mStreams->len > 0)
      {
         // Get and remove context from the array
         GStreamContext *c = (GStreamContext*) g_ptr_array_index(mStreams, 0);
         g_ptr_array_remove(mStreams, (gpointer) c);

         // Remove any remaining channel data...will only happen if an error
         // occurred during streaming.
         if (c->mChannels)
         {
            for (int ch = 0; ch < c->mNumChannels; ch++)
            {
               if (c->mChannels[ch])
               {
                  delete c->mChannels[ch];
               }
            }
            delete[] c->mChannels;
         }

         // Remove the appsink element
         if (c->mSink)
         {
            gst_bin_remove(GST_BIN(mPipeline), c->mSink);
         }

         // Remove the audioconvert element
         if (c->mConv)
         {
            gst_bin_remove(GST_BIN(mPipeline), c->mConv);
         }

         // Free the audio type
         if (c->mType)
         {
            g_free(c->mType);
         }

         // And finally get rid of the context
         g_free(c);
      }
      g_mutex_unlock(&mStreamsLock);

      // Done with the context array
      g_ptr_array_free(mStreams, TRUE);
   }

   // Release the decoder
   if (mDec != NULL)
   {
      gst_bin_remove(GST_BIN(mPipeline), mDec);
   }

   // Release the bus
   if (mBus != NULL)
   {
      gst_object_unref(mBus);
   }

   // Release the pipeline
   if (mPipeline != NULL)
   {
      gst_object_unref(mPipeline);
   }

   // Release the URI
   if (mUri != NULL)
   {
      g_free(mUri);
   }
}

// ----------------------------------------------------------------------------
// Return number of readable audio streams in the file
wxInt32
GStreamerImportFileHandle::GetStreamCount()
{
   return mStreamInfo.GetCount();
}

// ----------------------------------------------------------------------------
// Return array of strings - descriptions of the streams
wxArrayString *
GStreamerImportFileHandle::GetStreamInfo()
{
   return &mStreamInfo;
}

// ----------------------------------------------------------------------------
// Mark streams to process as selected by the user
void
GStreamerImportFileHandle::SetStreamUsage(wxInt32 index, bool use)
{
   g_mutex_lock(&mStreamsLock);
   if ((guint) index < mStreams->len)
   {
      GStreamContext *c = (GStreamContext *)
         g_ptr_array_index(mStreams, (guint) index);
      c->mUse = use;
   }
   g_mutex_unlock(&mStreamsLock);
}

// ----------------------------------------------------------------------------
// Initialize importer
bool
GStreamerImportFileHandle::Init()
{
   // Create a URI from the filename
   mUri = g_strdup_printf("file:///%s", mFilename.ToUTF8().data());
   if (!mUri)
   {
      wxLogMessage(wxT("GStreamerImport couldn't create URI"));
      return false;
   }

   // Create the stream context array
   mStreams = g_ptr_array_new();
   if (!mStreams)
   {
      wxLogMessage(wxT("GStreamerImport couldn't create context array"));
      return false;
   }

   // Create a pipeline
   mPipeline = gst_pipeline_new("pipeline");

   // Get its bus
   mBus = gst_pipeline_get_bus(GST_PIPELINE(mPipeline));

   // Create uridecodebin and set up signal handlers
   mDec = gst_element_factory_make("uridecodebin", "decoder");
   g_signal_connect(mDec, "autoplug-select", G_CALLBACK(GStreamerAutoplugSelectCallback), (gpointer) this);
   g_signal_connect(mDec, "pad-added", G_CALLBACK(GStreamerPadAddedCallback), (gpointer) this);
   g_signal_connect(mDec, "pad-removed", G_CALLBACK(GStreamerPadRemovedCallback), (gpointer) this);

   // Set the URI
   g_object_set(G_OBJECT(mDec), "uri", mUri, NULL);

   // Add the decoder to the pipeline
   if (!gst_bin_add(GST_BIN(mPipeline), mDec))
   {
      wxMessageBox(wxT("Unable to add decoder to pipeline"),
                   wxT("GStreamer Importer"));

      // Cleanup expected to occur in destructor
      return false;
   }

   // Run the pipeline
   GstStateChangeReturn state = gst_element_set_state(mPipeline, GST_STATE_PAUSED);
   if (state == GST_STATE_CHANGE_FAILURE)
   {
      wxMessageBox(wxT("Unable to set stream state to paused."),
                   wxT("GStreamer Importer"));
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
   g_mutex_lock(&mStreamsLock);
   for (guint i = 0; i < mStreams->len; i++)
   {
      GStreamContext *c = (GStreamContext *) g_ptr_array_index(mStreams, i);

      // Create stream info string
      wxString strinfo;
      strinfo.Printf(wxT("Index[%02d], Type[%s], Channels[%d], Rate[%d]"),
                     i,
                     wxString::FromUTF8(c->mType).c_str(),
                     c->mNumChannels,
                     (int) c->mSampleRate);
      mStreamInfo.Add(strinfo);
   }
   g_mutex_unlock(&mStreamsLock);

   return success;
}

// ----------------------------------------------------------------------------
// Return file dialog filter description
wxString
GStreamerImportFileHandle::GetFileDescription()
{
   return DESC;
}

// ----------------------------------------------------------------------------
// Return number of uncompressed bytes in file...doubtful this is possible
int
GStreamerImportFileHandle::GetFileUncompressedBytes()
{
   return 0;
}

// ----------------------------------------------------------------------------
// Import streams
int
GStreamerImportFileHandle::Import(TrackFactory *trackFactory,
                                  Track ***outTracks,
                                  int *outNumTracks,
                                  Tags *tags)
{
   // Save track factory pointer
   mTrackFactory = trackFactory;

   // Create the progrress dialog
   CreateProgress();

   // Block streams that are to be bypassed
   g_mutex_lock(&mStreamsLock);
   bool haveStreams = false;
   for (guint i = 0; i < mStreams->len; i++)
   {
      GStreamContext *c = (GStreamContext *) g_ptr_array_index(mStreams, i);

      // Did the user choose to skip this stream?
      if (!c->mUse)
      {
         // Get the audioconvert sink pad and unlink
         GstPad *convsink = gst_element_get_static_pad(c->mConv, "sink");
         GstPad *convpeer = gst_pad_get_peer(convsink);
         gst_pad_unlink(convpeer, convsink);
         gst_object_unref(convpeer);

         // Set bitbucket callbacks so the prerolled sample won't get processed
         // when we change the state to PLAYING
         gst_app_sink_set_callbacks(GST_APP_SINK(c->mSink), &AppSinkBitBucket, this, NULL);

         // Set state to playing for conv and sink so EOS gets processed
         gst_element_set_state(c->mConv, GST_STATE_PLAYING);
         gst_element_set_state(c->mSink, GST_STATE_PLAYING);

         // Send an EOS event to the pad to force them to drain
         gst_pad_send_event(convsink, gst_event_new_eos());

         // Resync state with pipeline
         gst_element_sync_state_with_parent(c->mConv);
         gst_element_sync_state_with_parent(c->mSink);

         // Done with the pad
         gst_object_unref(convsink);

         // Unlink audioconvert and appsink
         gst_element_unlink(c->mConv, c->mSink);

         // Remove them from the bin
         gst_bin_remove_many(GST_BIN(mPipeline), c->mConv, c->mSink, NULL);

         // All done with them
         c->mConv = NULL;
         c->mSink = NULL;

         continue;
      }

      // We have a stream to process
      haveStreams = true;
   }
   g_mutex_unlock(&mStreamsLock);

   // Can't do much if we don't have any streams to process
   if (!haveStreams)
   {
      wxMessageBox(wxT("File doesn't contain any audio streams."),
                   wxT("GStreamer Importer"));
      return eProgressFailed;
   }

   // Get the ball rolling...
   GstStateChangeReturn state = gst_element_set_state(mPipeline, GST_STATE_PLAYING);
   if (state == GST_STATE_CHANGE_FAILURE)
   {
      wxMessageBox(wxT("Unable to import file, state change failed."),
                   wxT("GStreamer Importer"));
      return eProgressFailed;
   }

   // Get the duration of the stream
   gint64 duration;
   gst_element_query_duration(mPipeline, GST_FORMAT_TIME, &duration);

   // Handle bus messages and update progress while files is importing
   bool success = true;
   int updateResult = eProgressSuccess;
   while (ProcessBusMessage(success) && success && updateResult == eProgressSuccess)
   {
      gint64 position;

      // Update progress indicator and give user chance to abort
      if (gst_element_query_position(mPipeline, GST_FORMAT_TIME, &position))
      {
         updateResult = mProgress->Update((wxLongLong_t) position,
                                          (wxLongLong_t) duration);
      }
   }

   // Disable pipeline
   gst_element_set_state(mPipeline, GST_STATE_NULL);

   // Something bad happened
   if (!success || updateResult == eProgressFailed || updateResult == eProgressCancelled)
   {
      return updateResult;
   }

   // Grah the streams lock
   g_mutex_lock(&mStreamsLock);

   // Count the total number of tracks collected
   *outNumTracks = 0;
   for (guint s = 0; s < mStreams->len; s++)
   {
      GStreamContext *c = (GStreamContext*)g_ptr_array_index(mStreams, s);
      if (c->mChannels)
      {
         *outNumTracks += c->mNumChannels;
      }
   }

   // Create new tracks
   *outTracks = new Track *[*outNumTracks];

   // Copy audio from mChannels to newly created tracks (destroying mChannels in process)
   int trackindex = 0;
   for (guint s = 0; s < mStreams->len; s++)
   {
      GStreamContext *c = (GStreamContext*)g_ptr_array_index(mStreams, s);
      if (c->mChannels)
      {
         for (int ch = 0; ch < c->mNumChannels; ch++)
         {
            c->mChannels[ch]->Flush();
            (*outTracks)[trackindex++] = c->mChannels[ch];
         }

         delete [] c->mChannels;
         c->mChannels = NULL;
      }
   }
   g_mutex_unlock(&mStreamsLock);

   // Set any tags found in the stream
   *tags = mTags;

   return updateResult;
}

// ----------------------------------------------------------------------------
// Message handlers
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
// Retrieve and process a bus message
bool
GStreamerImportFileHandle::ProcessBusMessage(bool & success)
{
   bool cont = true;

   // Default to no errors
   success = true;

   // Get the next message
   GstMessage *msg = gst_bus_timed_pop(mBus, 100 * GST_MSECOND);
   if (!msg)
   {
      // Timed out...not an error
      return cont;
   }

#if defined(__WXDEBUG__)
   gchar *objname = NULL;
   if (msg->src != NULL)
   {
      objname = gst_object_get_name(msg->src);
   }

   wxLogMessage(wxT("GStreamer: Got %s%s%s"),
                wxString::FromUTF8(GST_MESSAGE_TYPE_NAME(msg)).c_str(),
                objname ? wxT(" from ") : wxT(""),
                objname ? wxString::FromUTF8(objname).c_str() : wxT(""));

   if (objname != NULL)
   {
      g_free(objname);
   }
#endif

   // Handle based on message type
   switch (GST_MESSAGE_TYPE(msg))
   {
      // Handle error message from gstreamer
      case GST_MESSAGE_ERROR:
      {
         GError *err = NULL;
         gchar *debug = NULL;

         gst_message_parse_error(msg, &err, &debug);
         if (err)
         {
            wxString m;

            m.Printf(wxT("%s%s%s"),
               wxString::FromUTF8(err->message).c_str(),
               debug ? wxT("\n") : wxT(""),
               debug ? wxString::FromUTF8(debug).c_str() : wxT(""));
#if defined(_DEBUG)
            wxMessageBox(m, wxT("GStreamer Error:"));
#else
            wxLogMessage(wxT("GStreamer Error: %s"), m.c_str());
#endif
            g_error_free(err);
         }

         if (debug)
         {
            g_free(debug);
         }

         success = false;
         cont = false;
      }
      break;

      // Handle warning message from gstreamer
      case GST_MESSAGE_WARNING:
      {
         GError *err = NULL;
         gchar *debug = NULL;

         gst_message_parse_warning(msg, &err, &debug);

         if (err)
         {
            wxLogMessage(wxT("GStreamer Warning: %s%s%s"),
                         wxString::FromUTF8(err->message).c_str(),
                         debug ? wxT("\n") : wxT(""),
                         debug ? wxString::FromUTF8(debug).c_str() : wxT(""));

            g_error_free(err);
         }

         if (debug)
         {
            g_free(debug);
         }
      }
      break;

      // Handle warning message from gstreamer
      case GST_MESSAGE_INFO:
      {
         GError *err = NULL;
         gchar *debug = NULL;

         gst_message_parse_info(msg, &err, &debug);
         if (err)
         {
            wxLogMessage(wxT("GStreamer Info: %s%s%s"),
                         wxString::FromUTF8(err->message).c_str(),
                         debug ? wxT("\n") : wxT(""),
                         debug ? wxString::FromUTF8(debug).c_str() : wxT(""));

            g_error_free(err);
         }

         if (debug)
         {
            g_free(debug);
         }
      }
      break;

      // Handle metadata tags
      case GST_MESSAGE_TAG:
      {
         GstTagList *tags = NULL;

         // Retrieve tag list from message...just ignore failure
         gst_message_parse_tag(msg, &tags);
         if (tags)
         {
            // Go process the list
            OnTag(GST_APP_SINK(GST_MESSAGE_SRC(msg)), tags);

            // Done with list
            gst_tag_list_unref(tags);
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

   // Release the message
   gst_message_unref(msg);

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
            string.Printf(wxT("%u"), g_value_get_uint(val));
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
            gchar *str = gst_date_time_to_iso8601_string(dt);

            string = wxString::FromUTF8(str).c_str();

            g_free(str);
         }
         else if (G_VALUE_HOLDS(val, G_TYPE_DATE))
         {
            gchar *str = gst_value_serialize(val);

            string = wxString::FromUTF8(str).c_str();

            g_free(str);
         }
         else
         {
            wxLogMessage(wxT("Tag %s has unhandled type: %s"),
                         wxString::FromUTF8(name).c_str(),
                         wxString::FromUTF8(G_VALUE_TYPE_NAME(val)).c_str());
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
            tag = wxString::FromUTF8(name).c_str();
         }

         if (jcnt > 1)
         {
            tag.Printf(wxT("%s:%d"), tag.c_str(), j);
         }

         // Store the tag
         mTags.SetTag(tag, string);
      }
   }
}
#endif //USE_GSTREAMER
