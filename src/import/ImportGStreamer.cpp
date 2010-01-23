/**********************************************************************

Audacity: A Digital Audio Editor

ImportGStreamer.cpp

Copyright 2008  LRN
Based on ImportFFmpeg.cpp by LRN
Licensed under the GNU General Public License v2 or later

*//****************************************************************//**

\class GStreamerImportFileHandle
\brief An ImportFileHandle for GStreamer data

*//****************************************************************//**

\class GStreamerImportPlugin
\brief An ImportPlugin for GStreamer data

*//*******************************************************************/

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include "../Audacity.h"	// needed before GStreamer.h
#include "../GStreamerLoader.h"		// which brings in gst.h
#ifndef WX_PRECOMP
// Include your minimal set of headers here, or wx.h
#include <wx/window.h>
#endif

#define DESC _("GStreamer-compatible files")

#if defined(USE_GSTREAMER)
// all the includes live here by default
#include "Import.h"
#include "ImportGStreamer.h"
#include "../Tags.h"
#include "../Internat.h"
#include "../WaveTrack.h"
#include "ImportPlugin.h"
extern "C" {
#include <gio/gio.h>
#include <gst/app/gstappsink.h>
}
#include "stdint.h" //for int16_t

// A stucture, we create and maintain one for each audio stream
struct GStreamContext {
   gchar       *mAConvName;   // Name of an audio converter element that serves this stream
   gchar       *mASinkName;   // Name of an AppSink element that passes this stream to Audacity
   GstElement  *mConv;        // Audio converter
   GstElement  *mSink;        // Application sink
   bool         mUse;         // True if this stream should be imported, False if it should not be
   WaveTrack  **mChannels;    // Array of WaveTrack pointers, one for each channel
   gint         mNumChannels; // Number of channels
   gdouble      mSampleRate;  // Sample rate
   gint         mWidth;       // Alignment
   gint         mDepth;       // Sample resolution
   GstClockTime mLastTime;     // For synchronization
};

class GStreamerImportFileHandle;

/// A representative of GStreamer loader in
/// the Audacity import plugin list
class GStreamerImportPlugin : public ImportPlugin
{
public:
   GStreamerImportPlugin():
      ImportPlugin(wxArrayString())
      {
         if (GStreamerInst != NULL)
         {
           mExtensions = GStreamerInst->GetExtensions();
         }
         else
         {
           // It should be an empty list, but i've noticed that empty list doesn't work here
           mExtensions.Add(wxT("wav"));
         }
      }

   ~GStreamerImportPlugin() { }

   wxString GetPluginFormatDescription();

   ///! Probes the file and opens it if appropriate
   ImportFileHandle *Open(wxString Filename);
};

///! Does actual import, returned by GStreamerImportPlugin::Open
class GStreamerImportFileHandle : public ImportFileHandle
{

public:
   GStreamerImportFileHandle(const wxString & name);
   ~GStreamerImportFileHandle();

   ///! Format initialization
   ///\param nameIsUri - true if mName should be treated as uri, false if mName is a filename
   ///\return true if successful, false otherwise
   bool Init(bool nameIsUri);

   wxString GetFileDescription();
   int GetFileUncompressedBytes();

   ///! Imports audio
   ///\return import status (see Import.cpp)
   int Import(TrackFactory *trackFactory, Track ***outTracks,
      int *outNumTracks, Tags *tags);

   ///! Called when pad-added signal comes in from UriDecodeBin (as GstElement)
   ///\param uridecodebin - element that sent the signal
   ///\param pad - source pad of uridecodebin that will be serving the data
   void OnNewStream(GstElement *uridecodebin, GstPad *pad);

   ///! Called when unknown-pad signal comes in from UriDecodeBin
   ///\param uridecodebin - element that sent the signal
   ///\param pad - source pad of uridecodebin that will not be serving any data
   ///\param caps - information about the stream
   void OnUnknownStream(GstElement *uridecodebin, GstPad *pad, GstCaps *caps);

   ///! Adds new stream context
   ///\return pointer to newly-allocated context
   GStreamContext *AddNewStream();

   ///! Removes stream context
   ///\param index - index of a context in context array
   void DeleteStream(guint index);

   ///! Called when a message comes through GStreamer message bus
   ///\param bus - a bus that served the message
   ///\param message - a message
   ///\return true if we want to continue to watch the bus, false otherwise
   gboolean OnBusMessage(GstBus *bus, GstMessage *message);

   ///! Called when new data comes in from the pipeline
   ///\param sc - stream context
   ///\param buffer - GStreamer Audio Buffer
   ///\return eImportSuccess if everything is OK
   int WriteData(GStreamContext *sc, GstBuffer *buffer);

   ///! Called by the pipeline when GST_MESSAGE_TAG arrives
   ///\param list - list of tags
   ///\param tag - a tag in the list
   void WriteTags(const GstTagList *list, const gchar *tag);

   ///! Called by Import.cpp
   ///\return number of readable audio streams in the file
   wxInt32 GetStreamCount()
   {
      return mScs->len;
   }
   
   ///! Called by Import.cpp
   ///\return array of strings - descriptions of the streams
   wxArrayString *GetStreamInfo()
   {
      return mStreamInfo;
   }

   ///! Called by Import.cpp
   ///\param StreamID - index of the stream in mStreamInfo and mScs arrays
   ///\param Use - true if this stream should be imported, false otherwise
   void SetStreamUsage(wxInt32 StreamID, bool Use)
   {
      if ((guint)StreamID < mScs->len)
      {
         GStreamContext *c = (GStreamContext*)g_ptr_array_index(mScs,StreamID);
         c->mUse = Use;
      }
   }

   //! Convenience function - processes all the messages on the bus
   void ProcessMessages()
   {
      GstMessage *msg = gst_bus_pop(mBus);
      while (msg != NULL)
      {
         OnBusMessage(mBus,msg);
         msg = gst_bus_pop(mBus);
      }
   }

private:

   GstElement             *mPipeline;      //!< GStreamer pipeline
   GstBus                 *mBus;           //!< Message bus
   GstElement             *mDec;           //!< uridecodebin element
   GstStaticCaps           mStaticCaps;    //!< Decribes our input capabilities

   GPtrArray              *mScs;           //!< Array of pointers to stream contexts.
   wxArrayString          *mStreamInfo;    //!< Array of stream descriptions. Length is the same as mScs

   wxString                mName;          //!< Source name

   Track                ***mOutTracks;     //!< Tracks to be passed back to Audacity
   int                    *mOutNumTracks;  //!< Number of tracks to be passed back to Audacity
   Tags                    mTags;          //!< Tags to be passed back to Audacity
};

void GetGStreamerImportPlugin(ImportPluginList *importPluginList,
                           UnusableImportPluginList *unusableImportPluginList)
{
   importPluginList->Append(new GStreamerImportPlugin);
}

wxString GStreamerImportPlugin::GetPluginFormatDescription()
{
   return DESC;
}

ImportFileHandle *GStreamerImportPlugin::Open(wxString filename)
{
   if (!GStreamerInst || !GStreamerInst->Loaded())
      return NULL;

   GStreamerImportFileHandle *handle = new GStreamerImportFileHandle(filename);

   // Open the file for import  
   bool success = handle->Init(false);
   if (!success) {
      delete handle;
      return NULL;
   }

   return handle;
}

GStreamerImportFileHandle::GStreamerImportFileHandle(const wxString & name)
:  ImportFileHandle(name)
{

   mStreamInfo = new wxArrayString();
   mName = name;

   mPipeline = mDec = NULL;
   mScs = g_ptr_array_new();

   memset(&mStaticCaps,0,sizeof(mStaticCaps));
   mStaticCaps.string = (               // We are guaranteed to get audio in following format:
        "audio/x-raw-int, "             // Audaciy can handle floats too, but ints should be ok.
        "signed = (boolean) { TRUE }, " // I don't think that Audacity supports unsigned ints.
        "width = (int) { 16, 32 }, "    // 2- (for 16-bit depth) or 4-byte (for 24-bit depth) alignment.
        "depth = (int) { 16, 24 }, "    // 16- and 24-bit depth. 32-bit ints are not supported.
        "rate = (int) [ 1, MAX ], "     // AFAIK Audacity supports any sample rate.
        "channels = (int) [ 1, MAX ], " // Same here - Audacity supports any number of channel.
        "endianness = (int) BYTE_ORDER" // I think 'BYTE_ORDER' means 'native'. Right?
        );
}

GStreamerImportFileHandle::~GStreamerImportFileHandle()
{
   delete mStreamInfo;
   if (mBus != NULL)
     gst_object_unref(mBus);
   while (mScs->len > 0)
     DeleteStream(0);
   g_ptr_array_free(mScs,TRUE);
   if (mPipeline != NULL)
   {
      gst_object_unref(GST_OBJECT(mPipeline));
   }
}

// Transforms a taglist to a string
void ConvertTagsToString(const GstTagList *list, const gchar *tag, gpointer user_data)
{
   GString *gstring = (GString*)user_data;
   guint valuecount = gst_tag_list_get_tag_size(list, tag);
   const gchar *strvalue = NULL;
   GValue value = {0};

   // Get a GValue of the tag
   if (gst_tag_list_copy_value(&value, list, tag))
   {
     GType valtype = G_VALUE_TYPE(&value);
     const gchar *gtypename = g_type_name(valtype);

     GValue value_str = {0};
     // Transform it to string GValue
     g_value_init(&value_str, G_TYPE_STRING);
     if (g_value_transform(&value, &value_str))
     {
        // Get the string representation
        strvalue = g_value_get_string(&value_str);
        if (strvalue)
           g_string_append_printf(gstring, "%s[%s] ", tag, strvalue, NULL);
     }
   }
}

void GStreamerWriteTags(const GstTagList *list, const gchar *tag, gpointer user_data)
{
   GStreamerImportFileHandle *handle = (GStreamerImportFileHandle*)user_data;
   handle->WriteTags(list, tag);
}

// Writes tags to mTags member
void GStreamerImportFileHandle::WriteTags(const GstTagList *list, const gchar *tag)
{
   guint valuecount = gst_tag_list_get_tag_size(list, tag);
   const gchar *strvalue = NULL;

   GValue value = {0};

   if (gst_tag_list_copy_value(&value, list, tag))
   {
      GValue value_str = {0};
      g_value_init(&value_str, G_TYPE_STRING);
      if (g_value_transform(&value, &value_str))
      {
         strvalue = g_value_get_string(&value_str);
         if (strvalue)
         {
            wxString wxstrvalue = wxString::FromUTF8(strvalue);
            if (g_utf8_collate(tag, GST_TAG_TITLE) == 0)
               mTags.SetTag(TAG_TITLE, wxstrvalue);
            else if (g_utf8_collate(tag, GST_TAG_ARTIST) == 0)
               mTags.SetTag(TAG_ARTIST, wxstrvalue);
            else if (g_utf8_collate(tag, GST_TAG_ALBUM) == 0)
               mTags.SetTag(TAG_ALBUM, wxstrvalue);
            else if (g_utf8_collate(tag, GST_TAG_TRACK_NUMBER) == 0)
               mTags.SetTag(TAG_TRACK, wxstrvalue);
            else if (g_utf8_collate(tag, GST_TAG_DATE) == 0)
               mTags.SetTag(TAG_YEAR, wxstrvalue);
            else if (g_utf8_collate(tag, GST_TAG_GENRE) == 0)
               mTags.SetTag(TAG_GENRE, wxstrvalue);
            else if (g_utf8_collate(tag, GST_TAG_COMMENT) == 0)
               mTags.SetTag(TAG_COMMENTS, wxstrvalue);
         }
      }
   }
}

// Message handling
gboolean GStreamerImportFileHandle::OnBusMessage(GstBus *bus, GstMessage *message)
{
   if (message != NULL && message->src != NULL)
   {
      gchar *objname = gst_object_get_name(message->src);
      if (objname != NULL)
      {
        wxLogMessage(wxT("GStreamer: Got message %s from object %s"), wxString::FromUTF8(GST_MESSAGE_TYPE_NAME(message)).c_str() ,wxString::FromUTF8(objname).c_str());
      }
      g_free(objname);
   }
   else
     if (message)
        wxLogMessage(wxT("GStreamer: Got message %s"), wxString::FromUTF8(GST_MESSAGE_TYPE_NAME(message)).c_str());

   GError *err;
   gchar *debug;
   GstTagList *storedTaglist;

   switch (GST_MESSAGE_TYPE(message))
   {
   case GST_MESSAGE_ERROR:
      gst_message_parse_error(message, &err, &debug);
      wxLogMessage(wxT("GStreamer Error: %s - %s"), wxString::FromUTF8(err->message).c_str(), wxString::FromUTF8(debug).c_str());
      g_error_free(err);
      g_free(debug);
      return FALSE;
      break;
   case GST_MESSAGE_WARNING:
      gst_message_parse_warning(message, &err, &debug);
      wxLogMessage(wxT("GStreamer Warning: %s - %s"), wxString::FromUTF8(err->message).c_str(), wxString::FromUTF8(debug).c_str());
      g_error_free(err);
      g_free(debug);
      break;
   case GST_MESSAGE_INFO:
      gst_message_parse_info(message, &err, &debug);
      wxLogMessage(wxT("GStreamer Info: %s - %s"), wxString::FromUTF8(err->message).c_str(), wxString::FromUTF8(debug).c_str());
      g_error_free(err);
      g_free(debug);
      break;
   case GST_MESSAGE_STATE_CHANGED:
      GstState oldstate, newstate, pending;
      gst_message_parse_state_changed(message, &oldstate, &newstate, &pending);
      wxLogMessage(wxT("GStreamer State: %d -> %d ... %d"), oldstate, newstate, pending);
      break;
   case GST_MESSAGE_ASYNC_DONE:
      wxLogMessage(wxT("GStreamer: Asynchronous state change is done"));
      return FALSE;
      break;
   case GST_MESSAGE_EOS:
      wxLogMessage(wxT("GStreamer: End of the source stream"));
      gst_element_set_state(mPipeline, GST_STATE_NULL);
      return FALSE;
      break;
   case GST_MESSAGE_TAG:
      GstTagList *tags;
      tags = NULL;
      gst_message_parse_tag(message, &tags);
      storedTaglist = (GstTagList*)g_object_get_data(G_OBJECT(message->src), "Audacity::StoredTagList");
      if (storedTaglist)
      {
         gst_tag_list_insert(storedTaglist, tags, GST_TAG_MERGE_APPEND);
      }
      gst_tag_list_foreach(tags, GStreamerWriteTags, (gpointer)this);
      break;
   default:
      /* unhandled message */
      break;
   }
 
  /* we want to be notified again the next time there is a message
    * on the bus, so returning TRUE (FALSE means we want to stop watching
    * for messages on the bus and our callback should not be called again)
    */
   return TRUE;
}

gboolean GStreamerBusCallback(GstBus *bus, GstMessage *message, gpointer data)
{
   GStreamerImportFileHandle *handle = (GStreamerImportFileHandle*)data;
   return handle->OnBusMessage(bus, message);
}

void GStreamerUnknownStreamCallback(GstElement *uridecodebin, GstPad *pad, GstCaps *caps, gpointer data)
{
   GStreamerImportFileHandle *handle = (GStreamerImportFileHandle*)data;
   handle->OnUnknownStream(uridecodebin, pad, caps);
}

// Event handler for audioconv'es sink pads, reroutes tags to to the message bus
gboolean GStreamerConvEventCallback(GstPad *pad, GstEvent *event)
{
   gboolean ret;
   GstTagList *taglist;
   GstObject *element = gst_pad_get_parent(pad);

   switch (GST_EVENT_TYPE (event)) {
     case GST_EVENT_TAG:
       gst_event_parse_tag (event, &taglist);
       gst_element_post_message (GST_ELEMENT(element), gst_message_new_tag (element, gst_tag_list_copy(taglist)));
       break;
   }
   gst_object_unref(element);
   return gst_pad_event_default (pad, event);
}

// Called when there is going to be no more pads, i.e. demuxer found and autoplugged all streams
void GStreamerNoMorePadsCallback(GstElement *gstelement, gpointer data)
{
   GStreamerImportFileHandle *handle = (GStreamerImportFileHandle*)data;
 
   gpointer itdata;
   gboolean done;
   GstIterator *it;

   it = gst_element_iterate_src_pads(gstelement);
   done = FALSE;

   while (!done) {
     switch (gst_iterator_next (it, &itdata)) {
       case GST_ITERATOR_OK:
         handle->OnNewStream(gstelement, GST_PAD(itdata));
         gst_object_unref (itdata);
         break;
       case GST_ITERATOR_RESYNC:
         // Rollback changes to items
         gst_iterator_resync (it);
         break;
       case GST_ITERATOR_ERROR:
         // Wrong parameter were given
         done = TRUE;
         break;
       case GST_ITERATOR_DONE:
         done = TRUE;
         break;
     }
   }

   gst_iterator_free(it);
}

// This "gint" is really GstAutoplugSelectResult enum, but we may not have gstplay-enum.h, so just use gint
gint GStreamerAutoplugSelectCallback(GstElement *element, GstPad *pad, GstCaps *caps, GstElementFactory *factory, gpointer data)
{
   // Check factory class
   const gchar *fclass = gst_element_factory_get_klass(factory);
   gboolean quit = FALSE;
   // Skip video decoding
   if (g_strrstr(fclass,"Video"))
      return 2;
   return 0;
}

// Called for each new stream
void GStreamerImportFileHandle::OnNewStream(GstElement *uridecodebin, GstPad *pad)
{
   GstCaps *caps;
   GstStructure *str;
   gboolean quit;
   const gchar *name;

   caps = gst_pad_get_caps(pad);
   str = gst_caps_get_structure(caps, 0);

   // Check stream type
   name = gst_structure_get_name(str);
   // Only accept audio streams
   quit = FALSE;
   if (!g_strrstr(name, "audio"))
   {
      quit = TRUE;
   }
   gst_caps_unref (caps);
   if (quit)
     return;

   // Create unique name
   gchar *mAConvName = g_strdup_printf("aconv-%d",mScs->len);
   // Create an audioconv   
   GstElement *mConv = gst_element_factory_make("audioconvert", mAConvName);
   // Get it's sink pad
   GstPad *convpad = gst_element_get_static_pad(mConv, "sink");
   GstTagList *storedTaglist = gst_tag_list_new();
   g_object_set_data(G_OBJECT(mConv), "Audacity::StoredTagList", (gpointer)storedTaglist);

   gst_pad_set_event_function(convpad, GStreamerConvEventCallback);

   // Add it to the pipeline
   gst_bin_add(GST_BIN(mPipeline), mConv);

   GstPadLinkReturn ret = gst_pad_link(pad, convpad);
   gst_object_unref(convpad);
   if (ret)
   {
      // TODO: insert wxLogMessage here
      g_print("Failed to link uridecodebin to audioconvert - %d\n",ret);
      g_free(mAConvName);
      gst_bin_remove(GST_BIN(mPipeline), mConv);
      gst_object_unref(mConv);
      return;
   }

   gchar *mASinkName = g_strdup_printf("asink-%d",mScs->len);
   GstElement *mSink = gst_element_factory_make("appsink", mASinkName);
   // Set up sink properties
   caps = gst_static_caps_get(&mStaticCaps);
   gst_app_sink_set_caps(GST_APP_SINK(mSink), caps);          //!< NULL by default? (only accept data that matches caps)
   gst_caps_unref(caps);
   gst_base_sink_set_sync(GST_BASE_SINK(mSink), FALSE);       //!< TRUE by default (sync to the clock)
   gst_app_sink_set_drop(GST_APP_SINK(mSink), FALSE);         //!< FALSE by default (don't drop buffers when queue is full)
   gst_app_sink_set_emit_signals(GST_APP_SINK(mSink), FALSE); //!< FALSE by default (work in blocking mode)
   //gst_app_sink_set_max_buffers(GST_APP_SINK(mSink), 0);     //!< 0 by default, which means unlimited buffering

   gst_bin_add(GST_BIN(mPipeline), mSink);

   if (!gst_element_link(mConv, mSink))
   {
      g_print("Failed to link autioconvert to appsink\n");
      g_free(mAConvName);
      gst_bin_remove(GST_BIN(mPipeline), mConv);
      gst_object_unref(mConv);
      g_free(mASinkName);
      gst_bin_remove(GST_BIN(mPipeline), mSink);
      gst_object_unref(mSink);
   }

   // Run newly added elements
   GstStateChangeReturn statechange;
   statechange = gst_element_set_state(mConv, GST_STATE_PAUSED);
   statechange = gst_element_set_state(mSink, GST_STATE_PAUSED);
   
   // Allocate memory
   GStreamContext *c = g_new0(GStreamContext,1);
   c->mAConvName = mAConvName;
   c->mASinkName = mASinkName;
   c->mConv = mConv;
   c->mSink = mSink;
   // Add new stream context to context array
   g_ptr_array_add(mScs,c);
}

// For unknown streams just dump info to the debug log
void GStreamerImportFileHandle::OnUnknownStream(GstElement *uridecodebin, GstPad *pad, GstCaps *caps)
{
   GstStructure *str;
   str = gst_caps_get_structure(caps, 0);
   gst_structure_foreach(str, LogStructure, NULL);
}

// Free memory occupied by the stream and remove it
void GStreamerImportFileHandle::DeleteStream(guint index)
{
   if (index < mScs->len)
   {
      GStreamContext *str = (GStreamContext*)g_ptr_array_index(mScs,index);
      g_free(str->mAConvName);
      g_free(str->mASinkName);
      gst_bin_remove(GST_BIN(mPipeline), str->mSink);
      gst_bin_remove(GST_BIN(mPipeline), str->mConv);
      gst_object_unref(GST_OBJECT(str->mSink));
      gst_object_unref(GST_OBJECT(str->mConv));
      g_ptr_array_remove(mScs,(gpointer)str);
      g_free(str);
   }
}

// Pipeline initialization
bool GStreamerImportFileHandle::Init(bool nameIsUri)
{
   // Create a pipeline
   mPipeline = gst_pipeline_new("pipeline");
   // Get it's bus an add a message watch to it
   mBus = gst_pipeline_get_bus(GST_PIPELINE(mPipeline));   

   // Set up source location
   gchar *name_utf8 = g_strdup(mName.ToUTF8());
   gchar *name_with_proto_utf8;
   if (!nameIsUri)
      name_with_proto_utf8 = g_strdup_printf("file:///%s",name_utf8);
   
   // FIXME: it is possible that g_filename_from_utf8 should be used to convert uri to filesystem encoding
/*
   GError *err = NULL;
   gchar *name_filesystem = g_filename_from_utf8(name_with_proto_utf8, -1, NULL, NULL, &err);
   g_free(name_with_proto_utf8);
   if (err != NULL)
   {
      return false;
   }
*/
   
   // Create uridecodebin and set up signal handlers
   mDec = gst_element_factory_make("uridecodebin", "decoder");
   g_object_set_data(G_OBJECT(mDec), "Audacity.Object", (gpointer)this);
   g_signal_connect(mDec, "no-more-pads", G_CALLBACK(GStreamerNoMorePadsCallback), (gpointer)this);
   g_signal_connect(mDec, "unknown-type", G_CALLBACK(GStreamerUnknownStreamCallback), (gpointer)this);
   g_signal_connect(mDec, "autoplug-select", G_CALLBACK(GStreamerAutoplugSelectCallback), (gpointer)this);
   
   if (!nameIsUri)
   {
      g_object_set(G_OBJECT(mDec), "uri", name_with_proto_utf8, NULL);
      g_free(name_with_proto_utf8);
   }
   else
      g_object_set(G_OBJECT(mDec), "uri", name_utf8, NULL);

   g_free(name_utf8);

   // Add everything into the pipeline
   gst_bin_add_many(GST_BIN(mPipeline), mDec, NULL);

   // Run the pipeline
   GstStateChangeReturn statechange = gst_element_set_state(mPipeline, GST_STATE_PAUSED);

   while (TRUE)
   {
      GstMessage *msg = gst_bus_pop(mBus);
      if (msg)
      {
        if (!OnBusMessage(mBus,msg))
          break;
      }
   }

   // Make streaminfo strings for stream selector
   for (gint i = 0; i < mScs->len; i++)
   {
      wxString sinfo;
      GString *gstring_tags, *gstring_caps;
      GstTagList *storedTaglist;

      GstPad *convpad;
      GstCaps *ncaps;
      GstStructure *str;

      gstring_tags = g_string_new(NULL);
      gstring_caps = g_string_new(NULL);

      GStreamContext *c = (GStreamContext*) g_ptr_array_index(mScs, i);
      // Retrieve the taglist for that pad
      storedTaglist = (GstTagList *)g_object_get_data(G_OBJECT(c->mConv), "Audacity::StoredTagList");
      // Remove it from the pad so it won't accumulate tags anymore
      g_object_set_data(G_OBJECT(c->mConv), "Audacity::StoredTagList", NULL);

      // Convert taglist to string
      gst_tag_list_foreach(storedTaglist,ConvertTagsToString, (gpointer)gstring_tags);
      
      // Get negotiated caps
      convpad = gst_element_get_static_pad(c->mConv, "sink");
      ncaps = gst_pad_get_negotiated_caps(convpad);
      str = gst_caps_get_structure(ncaps, 0);

      // Convert it to gstring
      gst_structure_foreach(str, LogStructure, (gpointer)gstring_caps);
      
      sinfo.Printf(wxT("%s negotiated as: %s"), wxString::FromUTF8(gstring_tags->str).c_str(), wxString::FromUTF8(gstring_caps->str).c_str());
      mStreamInfo->Add(sinfo);
      
      gst_caps_unref(ncaps);
      gst_object_unref(convpad);
      gst_object_unref(storedTaglist);
      g_string_free(gstring_tags, TRUE);
      g_string_free(gstring_caps, TRUE);
   }

   return true;
}

wxString GStreamerImportFileHandle::GetFileDescription()
{
   return DESC;
}

int GStreamerImportFileHandle::GetFileUncompressedBytes()
{
   // TODO: Get Uncompressed byte count. Is it possible for GStreamer?
   return 0;
}

int GStreamerImportFileHandle::Import(TrackFactory *trackFactory,
              Track ***outTracks,
              int *outNumTracks,
              Tags *tags)
{
   CreateProgress();

   mOutTracks = outTracks;
   mOutNumTracks = outNumTracks;
/*
   // Dumps pipeline to file. At the moment it's for debugging only.
   FILE *f = fopen("gstreamer.import.pipeline.xml", "wb");

   xmlDocPtr cur = gst_xml_write (mPipeline);
   int memsize;
   xmlChar *mem;
   xmlDocDumpMemory (cur, &mem, &memsize);
   fwrite(mem,1,memsize,f);

   fclose(f);
*/
/*   
   for (guint i = 0; i < mScs->len; i++)
   {
     GStreamContext *sc = (GStreamContext *)g_ptr_array_index(mScs, i);
     if (!sc->mUse)
     {
       DeleteStream(i);
       i--;
     }
   }
*/
   // For each stream
   for (guint i = 0; i < mScs->len; i++)
   {
      GStreamContext *sc = (GStreamContext *)g_ptr_array_index(mScs, i);
      // For used streams - get actual stream caps
      if (sc->mUse)
      {
         GstPad *pad = gst_element_get_static_pad(sc->mSink,"sink");
         if (pad != NULL)
         {
            GstCaps *caps = gst_pad_get_negotiated_caps(pad);
            if (caps != NULL)
            {
               gint capcount = gst_caps_get_size(caps);
               GstStructure *str = gst_caps_get_structure(caps, 0);
               gint channels = -1;
               gst_structure_get_int(str, "channels", &channels);
               gint rate = 0;
               gst_structure_get_int(str, "rate", &rate);
               sc->mNumChannels = channels;
               sc->mSampleRate = (double)rate;
               gst_structure_get_int(str, "width", &sc->mWidth);
               gst_structure_get_int(str, "depth", &sc->mDepth);
               gst_caps_unref(caps);
               if (channels <= 0) sc->mUse = false;
            }
            gst_object_unref(pad);
         }
         else sc->mUse = false;
      }
      // For used streams that got negotiated properly - allocate stuff
      if (sc->mUse)
      {
         sc->mChannels = new WaveTrack *[sc->mNumChannels];
         
         sampleFormat sfmt = int16Sample;
         switch(sc->mDepth)
         {
         case 16:
           sfmt = int16Sample;
           break;
         case 24:
           sfmt = int24Sample;
           break;
         default:
           ;// TODO: Error? We do not support 32-bit int samples, do we?
         }

         for (int ch = 0; ch < sc->mNumChannels; ch++)
         {
            sc->mChannels[ch] = trackFactory->NewWaveTrack(sfmt, sc->mSampleRate);
            if (sc->mNumChannels == 2)
            {
               switch (ch)
               {
               case 0:
                  sc->mChannels[ch]->SetChannel(Track::LeftChannel);
                  sc->mChannels[ch]->SetLinked(true);
                  break;
               case 1:
                  sc->mChannels[ch]->SetChannel(Track::RightChannel);
                  sc->mChannels[ch]->SetTeamed(true);
                  break;
               }
            }
            else
            {
               sc->mChannels[ch]->SetChannel(Track::MonoChannel);
            }
         }
      }
      // I'm not sure this is going to work with queue limit set to zero...
      else
      {
        gst_app_sink_set_drop(GST_APP_SINK(sc->mSink), true);
      }
   }

   // The result of Import() to be returend. It will be something other than zero if user canceled or some error appears.
   int res = eProgressSuccess;

   // Run the pipeline
   GstStateChangeReturn statechange = gst_element_set_state(mPipeline, GST_STATE_PLAYING);
   if (statechange == GST_STATE_CHANGE_FAILURE)
      res = eProgressFailed;

   
   
   
   // This is the heart of the importing process
   gboolean doIt = TRUE;
   while (doIt && res == eProgressSuccess)
   {
      doIt = FALSE;
      // Keep an eye on any messages we could have
      ProcessMessages();

      guint sinkToPull = 0;
      GstClockTime lowestTime = -1;
      GstFormat fmt = GST_FORMAT_TIME;
      // Find a sink with lowest last timestamp (the one that lags behind the most)
      for (guint i = 0; i < mScs->len; i++)
      {
         GStreamContext *c = (GStreamContext *)g_ptr_array_index(mScs, i);
         if (c->mUse)
         {
            if (c->mLastTime < lowestTime || lowestTime < 0)
            {
              sinkToPull = i;
              lowestTime = c->mLastTime;
            }
         }
      }
      // Pull the data from that sink
      GStreamContext *c = (GStreamContext *)g_ptr_array_index(mScs, sinkToPull);
      GstBuffer *incbuf = gst_app_sink_pull_buffer(GST_APP_SINK(c->mSink));
      if (incbuf)
      {
        c->mLastTime = GST_BUFFER_TIMESTAMP(incbuf);
        doIt = TRUE;
        res = WriteData(c, incbuf);
        gst_buffer_unref(incbuf);
      }
   }

   // Something bad happened - destroy everything!
   if (res == eProgressFailed || res == eProgressCancelled)
   {
      for (guint s = 0; s < mScs->len; s++)
      {
         GStreamContext *c = (GStreamContext *)g_ptr_array_index(mScs, s);
         delete[] c->mChannels;
      }

      return res;
   }

   *outNumTracks = 0;
   for (guint s = 0; s < mScs->len; s++)
   {
      GStreamContext *c = (GStreamContext*)g_ptr_array_index(mScs,s);
      if (c->mUse)
        *outNumTracks += c->mNumChannels;
   }

   // Create new tracks
   *outTracks = new Track *[*outNumTracks];

   // Copy audio from mChannels to newly created tracks (destroying mChannels elements in process)
   int trackindex = 0;
   for (guint s = 0; s < mScs->len; s++)
   {
      GStreamContext *c = (GStreamContext*)g_ptr_array_index(mScs,s);
      if (c->mUse)
      {
         for(int ch = 0; ch < c->mNumChannels; ch++)
         {
            c->mChannels[ch]->Flush();
            (*outTracks)[trackindex++] = c->mChannels[ch];
         }
         delete[] c->mChannels;
         c->mChannels = NULL;
         c->mUse = false;
      }
   }

   // Pray that copycon works correctly
   *tags = mTags;

   return eProgressSuccess;
}

int GStreamerImportFileHandle::WriteData(GStreamContext *sc,GstBuffer *buffer)
{
   size_t pos = 0;

   int updateResult = eProgressSuccess;

   gpointer data = GST_BUFFER_DATA(buffer);
   guint length = GST_BUFFER_SIZE(buffer);
   
   // Allocate the buffer to store audio.
   int nChannels = sc->mNumChannels;
   int16_t **tmp16 = NULL;
   int32_t **tmp32 = NULL;
   // For 16-bit samples use 16-bit output buffer
   if (sc->mWidth == 16)
   {
      tmp16 = (int16_t**)malloc(sizeof(int16_t*)*nChannels);
      for (int chn = 0; chn < nChannels; chn++)
      {
         tmp16[chn] = (int16_t*)malloc(sizeof(int16_t) * length/sizeof(int16_t)/nChannels);
      }
   }
   // For 24- and 32-bit samples use 32-bit output buffer
   else if (sc->mWidth == 24 || sc->mWidth == 32)
   {
      tmp32 = (int32_t**)malloc(sizeof(int32_t*)*nChannels);
      for (int chn = 0; chn < nChannels; chn++)
      {
         tmp32[chn] = (int32_t*)malloc(sizeof(int32_t) * length/sizeof(int32_t)/nChannels);
      }      
   }

   // Separate the channels
   int16_t *int16_data = NULL;
   int32_t *int32_data = NULL;
   int index = 0;
   // Samples may be 16-, 24- or 32-bit, but input buffer's width is independant
   // I'm assuming that we'll never get width=16 and depth=24 or 32, that's impossible
   // Or width=32 and depth=16, that's stupid
   if (sc->mWidth == 16)
   {
      int16_data = (int16_t*)data;
      for (pos = 0; pos < length/sizeof(int16_t);)
      {
         for (int chn = 0; chn < nChannels; chn++)
         {
            tmp16[chn][index] = int16_data[pos];
            pos++;
         }
         index++;
      }
   }
   else if (sc->mWidth == 24 || sc->mWidth == 32)
   {
      int32_data = (int32_t*)data;
      for (pos = 0; pos < length/sizeof(int32_t);)
      {
         for (int chn = 0; chn < nChannels; chn++)
         {
            tmp32[chn][index] = int32_data[pos];
            pos++;
         }
         index++;
      }
   }

   // Write audio into WaveTracks
   if (sc->mDepth == 16)
   {
      for (int chn = 0; chn < nChannels; chn++)
      {
         sc->mChannels[chn]->Append((samplePtr)tmp16[chn],int16Sample,index);
         free(tmp16[chn]);
      }
      free(tmp16);
   }
   else if (sc->mDepth == 24 || sc->mDepth == 32)
   {
      for (int chn = 0; chn < nChannels; chn++)
      {
         sc->mChannels[chn]->Append((samplePtr)tmp32[chn],int24Sample,index);
         free(tmp32[chn]);
      }
      free(tmp32);
   }
   
   // Try to update the progress indicator (and see if user wants to stop or cancel)
   GstFormat fmt = GST_FORMAT_TIME;
   gint64 tpos, tlen;
   if (gst_element_query_position (mPipeline, &fmt, &tpos) &&
       gst_element_query_duration (mPipeline, &fmt, &tlen) )
   {
      updateResult = mProgress->Update((wxLongLong)tpos, (wxLongLong)tlen);
      if (((gdouble)tpos/tlen) >= 0.98103365830952650)
      {
        if (tpos > 0)
          tpos = 0;
      }
      if (updateResult != 1)
      {
         if (updateResult == 0)
         {
           updateResult = eProgressCancelled;
         }
         else
           updateResult = eProgressStopped;
         GstStateChangeReturn statechange = gst_element_set_state(mPipeline, GST_STATE_NULL);
      }
      else
        updateResult = eProgressSuccess;
   }
   
   return updateResult;
}

#endif //USE_GSTREAMER
