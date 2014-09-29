 /**********************************************************************

  Audacity: A Digital Audio Editor

  LV2Effect.h

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

**********************************************************************/

#include "../../Audacity.h"

#if defined(USE_LV2)

#include <queue>

#if defined(__WXMSW__)
#include <float.h>
#define isfinite _finite
#define isnan _isnan
#elif defined(__WXMAC__)
#else
//#define isfinite std::isfinite
//#define isnam std::isnan
#endif


#include <wx/wxprec.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/dynarray.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/tokenzr.h>
#include <wx/intl.h>
#include <wx/scrolwin.h>

#include "../Effect.h"
#include "LoadLV2.h"
#include "LV2Effect.h"
#include "LV2PortGroup.h"
#include "../../Internat.h"
#include "lv2_event_helpers.h"

#include "lilv/lilv.h"

#include <wx/arrimpl.cpp>

WX_DEFINE_OBJARRAY(LV2PortArray);

LV2Effect::LV2Effect(const LilvPlugin *data,
                     const std::set<wxString> & categories)
:  mValid(true),
   mCategories(categories),
   mMidiInput(0),
   mLatencyPortIndex(-1)
{

   // We don't support any features at all, so if the plugin requires
   // any we skip it.
   LilvNodes *req = lilv_plugin_get_required_features(data);
   size_t nFeatures = lilv_nodes_size(req);
   lilv_nodes_free(req);
   if (nFeatures > 0)
   {
      mValid = false;
      return;
   }

   mData = data;
   pluginName = GetString(lilv_plugin_get_name(mData), true);

   fInBuffer = NULL;
   fOutBuffer = NULL;

   mLength = 0;

   // Allocate buffers for the port indices and the default control values
   int numPorts = lilv_plugin_get_num_ports(mData);
   float *minimumValues = new float [numPorts];
   float *maximumValues = new float [numPorts];
   float *defaultValues = new float [numPorts];

   // Retrieve the port ranges for all ports (some values may be NaN)
   lilv_plugin_get_port_ranges_float(mData, minimumValues,
                                     maximumValues, defaultValues);

   // Get info about all ports
   for (int i = 0; i < numPorts; i++)
   {
      const LilvPort *port = lilv_plugin_get_port_by_index(mData, i);
      LV2Port internalPort;
      internalPort.mIndex = lilv_port_get_index(mData, port);

      // Get the port name
      LilvNode *tmpName = lilv_port_get_name(mData, port);
      internalPort.mName = GetString(tmpName);
      lilv_node_free(tmpName);

      // Get the scale points
      LilvScalePoints* points = lilv_port_get_scale_points(mData, port);
      LILV_FOREACH(scale_points, j, points)
      {
         const LilvScalePoint *point = lilv_scale_points_get(points, j);

         internalPort.mScaleValues.Add(lilv_node_as_float(lilv_scale_point_get_value(point)));
         internalPort.mScaleLabels.Add(GetString(lilv_scale_point_get_label(point)));
      }
      lilv_scale_points_free(points);

      // Get the groups
      LilvNodes *groups = lilv_port_get_value(mData, port, gPortGroup);
      if (groups)
      {
         LilvNode *group = lilv_nodes_get_first(groups);
         wxString uri = GetString(group);

         wxString label;
         const LilvNode *name = lilv_world_get(gWorld, group, gName, NULL);
         if (name)
         {
            label = GetString(name);
         }
         else
         {
            // Shouldn't happen, but provide something
            label = uri;
         }
         lilv_nodes_free(groups);

         // Check for new group
         if (mPortGroups.find(uri) == mPortGroups.end())
         {
            mPortGroups[uri] = LV2PortGroup(label);
         }
#if 0
         // Get subgroup
         //
         // LLL:  This isn't right...must find or construct a plugin with
         //       subgroups.
         LilvNodes *subgroup = lilv_node_get_value(mData, port, gSubGroupOf);
         if (subgroups)
         {
            LilvNode *subgroup = lilv_nodes_get_first(subgroups);
            wxString uri = GetString(subgroup);
            const LilvNode *subgroup = lilv_world_get(gWorld, group, gSubGroupOf, NULL);
            wxString label = GetString(name);
            lilv_nodes_free(subgroup);
         }
         else
#endif
         {
            mRootGroup.AddSubGroup(mPortGroups[uri]);
         }
         mPortGroups[uri].AddParameter(i);

      }
      else
      {
         mRootGroup.AddParameter(i);
      }

      // Get the port type
      if (lilv_port_is_a(mData, port, gAudioPortClass))
      {
         if (lilv_port_is_a(mData, port, gInputPortClass))
         {
            mAudioInputs.Add(internalPort);
         }
         else if (lilv_port_is_a(mData, port, gOutputPortClass))
         {
            mAudioOutputs.Add(internalPort);
         }
      }
      else if (lilv_port_is_a(mData, port, gControlPortClass) &&
               lilv_port_is_a(mData, port, gInputPortClass))
      {
         internalPort.mControlBuffer = float(1.0);
         internalPort.mMin = minimumValues[i];
         internalPort.mMax = maximumValues[i];
         internalPort.mDefault = defaultValues[i];
         if (isfinite(defaultValues[i]))
         {
            internalPort.mControlBuffer = defaultValues[i];
         }
         else if (isfinite(minimumValues[i]))
         {
            internalPort.mControlBuffer = minimumValues[i];
         }
         else if (isfinite(maximumValues[i]))
         {
            internalPort.mControlBuffer = maximumValues[i];
         }

         if (lilv_port_has_property(mData, port, gPortToggled))
         {
            internalPort.mToggle = true;
         }
         else if (lilv_port_has_property(mData, port, gPortIsInteger))
         {
            internalPort.mInteger = true;
         }
         else if (lilv_port_has_property(mData, port, gPortIsSampleRate))
         {
            internalPort.mSampleRate = true;
         }
         else if (lilv_port_has_property(mData, port, gPortIsEnumeration))
         {
            internalPort.mEnumeration = true;
         }

         mControlInputs.Add(internalPort);
      }
      else if (lilv_port_is_a(mData, port, gControlPortClass) &&
               lilv_port_is_a(mData, port, gOutputPortClass))
      {
         // If there is more than one latency port, the plugin is invalid
         if (lilv_port_has_property(mData, port, gPortIsLatency))
         {
            if (mLatencyPortIndex >= 0)
            {
               mValid = false;
               continue;
            }
            mLatencyPortIndex = i;
         }
         else if (!lilv_port_has_property(mData, port, gPortIsOptional))
         {
            mControlOutputs.Add(internalPort);
         }
      }
      else if (lilv_port_is_a(mData, port, gMidiPortClass) &&
               lilv_port_is_a(mData, port, gInputPortClass))
      {
         // If there is more than one MIDI input port, the plugin is invalid
         if (mMidiInput)
         {
            mValid = false;
            continue;
         }
         mMidiInput = new LV2Port(internalPort);
      }
      else
      {
         // Unknown port type, we set the invalid flag
 //        mValid = false;
      }
   }

   delete [] minimumValues;
   delete [] maximumValues;
   delete [] defaultValues;

   // MIDI synths may not have any audio inputs.
   if (mMidiInput && mAudioInputs.GetCount() > 0)
   {
      mValid = false;
   }

   // Determine whether the plugin is a generator, effect or analyser
   // depending on the number of ports of each type (not completely accurate,
   // but works most of the time)
   int flags = PLUGIN_EFFECT;
   if (mAudioInputs.GetCount() == 0)
   {
      flags |= INSERT_EFFECT;
   }
   else if (mAudioOutputs.GetCount() == 0)
   {
      flags |= ANALYZE_EFFECT;
   }
   else
   {
      flags |= PROCESS_EFFECT;
   }

   SetEffectFlags(flags);
}

LV2Effect::~LV2Effect()
{
   if (mMidiInput)
   {
      delete mMidiInput;
   }
}

wxString LV2Effect::GetEffectName()
{
   if (mControlInputs.GetCount() > 0)
   {
      return pluginName + wxT("...");
   }
   else
   {
      return pluginName;
   }
}

std::set<wxString> LV2Effect::GetEffectCategories()
{
   return mCategories;
}

wxString LV2Effect::GetEffectIdentifier()
{
   wxStringTokenizer st(pluginName, wxT(" "));
   wxString id;

   // CamelCase the name
   while (st.HasMoreTokens())
   {
      wxString tok = st.GetNextToken();

      id += tok.Left(1).MakeUpper() + tok.Mid(1);
   }

   return id;
}

wxString LV2Effect::GetEffectAction()
{
   return wxString::Format(_("Performing Effect: %s"),
                           pluginName.c_str());
}

bool LV2Effect::Init()
{
   mBlockSize = 0;
   mainRate = 0;

   TrackListOfKindIterator iter(Track::Wave, mTracks);
   Track *left = iter.First();
   while(left)
   {
      if (mainRate == 0)
      {
         mainRate = (int)(((WaveTrack *)left)->GetRate() + 0.5);
      }

      if (left->GetLinked())
      {
         Track *right = iter.Next();

         if (((WaveTrack *)left)->GetRate() !=
             ((WaveTrack *)right)->GetRate())
         {
            wxMessageBox(_("Sorry, Plug-in Effects cannot be performed on stereo tracks where the individual channels of the track do not match."));
            return false;
         }
      }

      left = iter.Next();
   }

   if (mainRate <= 0)
   {
      mainRate = (int)(mProjectRate + 0.5);
   }

   return true;
}

bool LV2Effect::PromptUser()
{
   if (mControlInputs.GetCount() > 0)
   {
      double length = mT1 > mT0 ? mT1 - mT0 : sDefaultGenerateLen;
      double noteLength = length / 2;
      unsigned char noteVelocity = 64;
      unsigned char noteKey = 64;

      LV2EffectDialog dlog(this, mParent, mData, mainRate, length,
                           noteLength, noteVelocity, noteKey);
      dlog.CentreOnParent();
      dlog.ShowModal();

      if (!dlog.GetReturnCode())
      {
         return false;
      }

      mLength = dlog.GetLength();
      mNoteLength = dlog.GetNoteLength();
      mNoteVelocity = dlog.GetNoteVelocity();
      mNoteKey = dlog.GetNoteKey();
   }

   return true;
}

bool LV2Effect::Process()
{
   CopyInputTracks();
   bool bGoodResult = true;

   TrackListIterator iter(mOutputTracks);
   int count = 0;
   Track *left = iter.First();
   Track *right = NULL;
   while (left)
   {
      sampleCount lstart = 0, rstart = 0;
      sampleCount len;
      GetSamples((WaveTrack *)left, &lstart, &len);

      right = NULL;
      if (left->GetLinked() && mAudioInputs.GetCount() > 1)
      {
         right = iter.Next();
         GetSamples((WaveTrack *)right, &rstart, &len);
      }

      if (mAudioInputs.GetCount() < 2 && right)
      {
         // If the effect is mono, apply to each channel separately

         bGoodResult = ProcessStereo(count, (WaveTrack *)left, NULL,
                                     lstart, 0, len) &&
            ProcessStereo(count, (WaveTrack *)right, NULL,
                          rstart, 0, len);
      }
      else
      {
         bGoodResult = ProcessStereo(count,
                                     (WaveTrack *)left, (WaveTrack *)right,
                                      lstart, rstart, len);
      }

      if (!bGoodResult)
      {
         break;
      }

      left = iter.Next();
      count++;
   }

   ReplaceProcessedTracks(bGoodResult);

   return bGoodResult;
}

bool LV2Effect::ProcessStereo(int count,
                              WaveTrack *left,
                              WaveTrack *right,
                              sampleCount lstart,
                              sampleCount rstart,
                              sampleCount len)
{
   /* Allocate buffers */
   if (mBlockSize == 0)
   {
      mBlockSize = left->GetMaxBlockSize() * 2;

      fInBuffer = new float *[mAudioInputs.GetCount()];
      for (size_t i = 0; i < mAudioInputs.GetCount(); i++)
      {
         fInBuffer[i] = new float[mBlockSize];
      }

      fOutBuffer = new float *[mAudioOutputs.GetCount()];
      for (size_t i = 0; i < mAudioOutputs.GetCount(); i++)
      {
         fOutBuffer[i] = new float[mBlockSize];
      }
   }

   /* Instantiate the plugin */
   LilvInstance *handle = lilv_plugin_instantiate(mData,
                                                  left->GetRate(),
                                                  gLV2Features);
   if (!handle)
   {
      wxMessageBox(wxString::Format(_("Unable to load plug-in %s"), pluginName.c_str()));
      return false;
   }

   /* Write the Note On to the MIDI event buffer and connect it */
   LV2_Event_Buffer *midiBuffer = NULL;
   int noteOffTime;
   if (mMidiInput)
   {
      midiBuffer = lv2_event_buffer_new(40, 2);
      LV2_Event_Iterator iter;
      lv2_event_begin(&iter, midiBuffer);
      uint8_t noteOn[] = { 0x90, mNoteKey, mNoteVelocity };
      lv2_event_write(&iter, 0, 0, 1, 3, noteOn);
      noteOffTime = mNoteLength * left->GetRate();
      if (noteOffTime < len && noteOffTime < mBlockSize) {
         uint8_t noteOff[] = { 0x80, mNoteKey, 64 };
         lv2_event_write(&iter, noteOffTime, 0, 1, 3, noteOff);
      }
      lilv_instance_connect_port(handle, mMidiInput->mIndex, midiBuffer);
   }

   for (size_t p = 0; p < mAudioInputs.GetCount(); p++)
   {
      lilv_instance_connect_port(handle, mAudioInputs[p].mIndex, fInBuffer[p]);
   }

   for (size_t p = 0; p < mAudioOutputs.GetCount(); p++)
   {
      lilv_instance_connect_port(handle, mAudioOutputs[p].mIndex, fOutBuffer[p]);
   }

   for (size_t p = 0; p < mControlInputs.GetCount(); p++)
   {
      lilv_instance_connect_port(handle, mControlInputs[p].mIndex,
                                 &mControlInputs[p].mControlBuffer);
   }

   for (size_t p = 0; p < mControlOutputs.GetCount(); p++)
   {
      lilv_instance_connect_port(handle, mControlOutputs[p].mIndex,
                                 &mControlOutputs[p].mControlBuffer);
   }

   float latency = 0.0;
   if (mLatencyPortIndex >= 0)
   {
      lilv_instance_connect_port(handle, mLatencyPortIndex, &latency);
   }

   lilv_instance_activate(handle);

   // Actually perform the effect here

   sampleCount originalLen = len;
   sampleCount ls = lstart;
   sampleCount rs = rstart;
   sampleCount ols = ls;
   sampleCount ors = rs;
   bool noteOver = false;

   sampleCount delayed = 0;
   sampleCount delay = 0;
   bool cleared = false;

   while (len || delayed)
   {
      int block = mBlockSize;

      if (len)
      {
         if (block > len)
         {
            block = len;
         }

         if (left &&  mAudioInputs.GetCount() > 0)
         {
            left->Get((samplePtr)fInBuffer[0], floatSample, ls, block);
         }

         if (right && mAudioInputs.GetCount() > 1)
         {
            right->Get((samplePtr)fInBuffer[1], floatSample, rs, block);
         }
      }
      else if (delayed)
      {
         // At the end if we don't have enough left for a whole block
         if (block > delayed)
         {
            block = delayed;
         }

         // Clear the input buffer so that we only pass zeros to the effect.
         if (!cleared)
         {
            for (int i = 0; i < mBlockSize; i++)
            {
               fInBuffer[0][i] = 0.0;
            }

            if (right)
            {
               memcpy(fInBuffer[1], fOutBuffer[0], mBlockSize);
            }
            cleared = true;
         }
      }

      lilv_instance_run(handle, block);

      if (delayed == 0 && latency != 0)
      {
         delayed = delay = latency;
      }

      if (delay >= block)
      {
         delay -= block;
      }
      else if (delay > 0)
      {
         sampleCount oblock = block - delay;
         if (left && mAudioOutputs.GetCount() > 0)
         {
            left->Set((samplePtr)(fOutBuffer[0] + delay), floatSample, ols, oblock);
         }

         if (right && mAudioOutputs.GetCount() > 1)
         {
            right->Set((samplePtr)(fOutBuffer[1] + delay), floatSample, ors, oblock);
         }
         ols += oblock;
         ors += oblock;
         delay = 0;
      }
      else
      {
         if (left && mAudioOutputs.GetCount() > 0)
         {
            left->Set((samplePtr)fOutBuffer[0], floatSample, ols, block);
         }

         if (right && mAudioOutputs.GetCount() > 1)
         {
            right->Set((samplePtr)fOutBuffer[1], floatSample, ors, block);
         }
         ols += block;
         ors += block;
      }

      if (len)
      {
         len -= block;
         noteOffTime -= block;

         // Clear the event buffer and add the note off event if needed
         if (mMidiInput)
         {
            lv2_event_buffer_reset(midiBuffer, 1,
                                   (uint8_t *)midiBuffer +
                                   sizeof(LV2_Event_Buffer));

            if (!noteOver && noteOffTime < len && noteOffTime < block)
            {
               LV2_Event_Iterator iter;
               lv2_event_begin(&iter, midiBuffer);
               uint8_t noteOff[] = { 0x80, mNoteKey, 64 };
               lv2_event_write(&iter, noteOffTime, 0, 1, 3, noteOff);
               noteOver = true;
            }
         }
      }
      else if (delayed)
      {
         delayed -= block;
      }
      ls += block;
      rs += block;

      if (mAudioInputs.GetCount() > 1)
      {
         if (TrackGroupProgress(count, (ls-lstart)/(double)originalLen))
         {
            return false;
         }
      }
      else
      {
         if (TrackProgress(count, (ls-lstart)/(double)originalLen))
         {
            return false;
         }
      }

   }

   lilv_instance_deactivate(handle);
   lilv_instance_free(handle);

   return true;
}

void LV2Effect::End()
{
   unsigned long i;

   if (fInBuffer)
   {
      for (i = 0; i < mAudioInputs.GetCount(); i++)
      {
         if (fInBuffer[i])
         {
            delete [] fInBuffer[i];
         }
      }
      delete [] fInBuffer;
      fInBuffer = NULL;
   }

   if (fOutBuffer)
   {
      for (i = 0; i < mAudioOutputs.GetCount(); i++)
      {
         if (fOutBuffer[i])
         {
            delete [] fOutBuffer[i];
         }
      }
      delete [] fOutBuffer;
      fOutBuffer = NULL;
   }
}


bool LV2Effect::IsValid()
{
   return mValid;
}


LV2PortArray & LV2Effect::GetControls()
{
   return mControlInputs;
}


bool LV2Effect::IsSynth()
{
   return (mMidiInput != 0);
}


bool LV2Effect::SetNote(sampleCount len,
                        unsigned char velocity, unsigned char key)
{
   if (velocity == 0 || velocity > 127 || key > 127)
   {
      return false;
   }

   mNoteLength = len;
   mNoteVelocity = velocity;
   mNoteKey = key;

   return true;
}

const LV2PortGroup & LV2Effect::GetRootGroup()
{
   return mRootGroup;
}

wxString LV2Effect::GetString(const LilvNode *node)
{
   return wxString::FromUTF8(lilv_node_as_string(node));
}

wxString LV2Effect::GetString(LilvNode *node, bool free)
{
   wxString str = GetString(node);
   if (free)
   {
      lilv_node_free(node);
   }

   return str;
}

// This should be moved to its own source file, it's in LadspaEffect.cpp
// as well
class LV2Slider:public wxSlider
{
public:
   LV2Slider(wxWindow *parent, wxWindowID id,
             int value, int minValue, int maxValue,
             const wxPoint & pos = wxDefaultPosition,
             const wxSize & size = wxDefaultSize,
             long style = wxSL_HORIZONTAL,
             const wxValidator & validator = wxDefaultValidator,
             const wxString & name = wxSliderNameStr)
   : wxSlider(parent, id, value, minValue, maxValue,
              pos, size, style, validator, name)
   {
   };

   void OnSetFocus(wxFocusEvent & event)
   {
      wxScrolledWindow *p = (wxScrolledWindow *) GetParent();
      wxRect r = GetRect();
      wxRect rv = p->GetRect();
      rv.y = 0;

      event.Skip();

      int y;
      int yppu;
      p->GetScrollPixelsPerUnit(NULL, &yppu);

      if (r.y >= rv.y && r.GetBottom() <= rv.GetBottom())
      {
         return;
      }

      if (r.y < rv.y)
      {
         p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
         y = r.y / yppu;
      }
      else
      {
         p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
         y = (r.GetBottom() - rv.GetBottom() + yppu) / yppu;
      }

      p->Scroll(-1, y);
   };

private:
   DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(LV2Slider, wxSlider)
    EVT_SET_FOCUS(LV2Slider::OnSetFocus)
END_EVENT_TABLE()

class LV2TextCtrl:public wxTextCtrl
{
public:
   LV2TextCtrl(wxWindow *parent, wxWindowID id,
               const wxString & value = wxEmptyString,
               const wxPoint& pos = wxDefaultPosition,
               const wxSize& size = wxDefaultSize,
               long style = 0,
               const wxValidator & validator = wxDefaultValidator,
               const wxString & name = wxTextCtrlNameStr)
   :  wxTextCtrl(parent, id, value,
                 pos, size, style, validator, name)
   {
   };

   void OnSetFocus(wxFocusEvent & event)
   {
      wxScrolledWindow *p = (wxScrolledWindow *) GetParent();
      wxRect r = GetRect();
      wxRect rv = p->GetRect();
      rv.y = 0;

      event.Skip();

      int y;
      int yppu;
      p->GetScrollPixelsPerUnit(NULL, &yppu);

      if (r.y >= rv.y && r.GetBottom() <= rv.GetBottom())
      {
         return;
      }

      if (r.y < rv.y)
      {
         p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
         y = r.y / yppu;
      }
      else
      {
         p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
         y = (r.GetBottom() - rv.GetBottom() + yppu) / yppu;
      }

      p->Scroll(-1, y);
   };

private:
   DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(LV2TextCtrl, wxTextCtrl)
   EVT_SET_FOCUS(LV2TextCtrl::OnSetFocus)
END_EVENT_TABLE()

static const int LADSPA_SECONDS_ID = 13101;

BEGIN_EVENT_TABLE(LV2EffectDialog, wxDialog)
    EVT_BUTTON(wxID_OK, LV2EffectDialog::OnOK)
    EVT_BUTTON(wxID_CANCEL, LV2EffectDialog::OnCancel)
    EVT_BUTTON(ID_EFFECT_PREVIEW, LV2EffectDialog::OnPreview)
    EVT_SLIDER(wxID_ANY, LV2EffectDialog::OnSlider)
    EVT_TEXT(wxID_ANY, LV2EffectDialog::OnTextCtrl)
    EVT_CHECKBOX(wxID_ANY, LV2EffectDialog::OnCheckBox)
    EVT_CHOICE(wxID_ANY, LV2EffectDialog::OnChoiceCtrl)
END_EVENT_TABLE()

IMPLEMENT_CLASS(LV2EffectDialog, wxDialog)

LV2EffectDialog::LV2EffectDialog(LV2Effect *effect,
                                 wxWindow  *parent,
                                 const LilvPlugin *data,
                                 int sampleRate,
                                 double length,
                                 double WXUNUSED(noteLength),
                                 unsigned char WXUNUSED(noteVelocity),
                                 unsigned char WXUNUSED(noteKey))
:  wxDialog(parent, wxID_ANY,
            mEffect->GetString(lilv_plugin_get_name(data)),
            wxDefaultPosition, wxSize(500, -1),
            wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
   mEffect(effect),
   mData(data),
   mControls(effect->GetControls()),
   mSampleRate(sampleRate),
   mLength(length)
{

#if defined(__WXMSW__)
   // On Windows, for some reason, wxWindows calls OnTextCtrl during creation
   // of the text control, and LV2EffectDialog::OnTextCtrl calls HandleText,
   // which assumes all the mFields have been initialized.
   // This can give us a bad pointer crash, so manipulate inSlider to
   // no-op HandleText during creation.
   inSlider = true;
#else
   inSlider = false;
#endif
   inText = false;
   inText = true;

   // Allocate memory for the user parameter controls
   int ctrlcnt = (int) mControls.GetCount();
   mToggles = new wxCheckBox*[ctrlcnt];
   mSliders = new wxSlider*[ctrlcnt];
   mFields = new wxTextCtrl*[ctrlcnt];
   mLabels = new wxStaticText*[ctrlcnt];
   mEnums = new wxChoice*[ctrlcnt];

   wxControl *item;

   wxBoxSizer *vSizer = new wxBoxSizer(wxVERTICAL);

   // Add information about the plugin
   LilvNode *tmpValue = lilv_plugin_get_author_name(data);
   if (tmpValue)
   {
      wxString author(_("Author: ") + mEffect->GetString(tmpValue));
      item = new wxStaticText(this, wxID_ANY, author);
      vSizer->Add(item, 0, wxALL, 5);
      lilv_node_free(tmpValue);
   }

   wxScrolledWindow *w = new wxScrolledWindow(this,
                                              wxID_ANY,
                                              wxDefaultPosition,
                                              wxDefaultSize,
                                              wxVSCROLL | wxTAB_TRAVERSAL);

   // Try to give the window a sensible default/minimum size
   w->SetMinSize(wxSize(
      wxMax(600, parent->GetSize().GetWidth() * 2/3),
      parent->GetSize().GetHeight() / 2));

   w->SetScrollRate(0, 20);
   vSizer->Add(w, 1, wxEXPAND|wxALL, 5);

   // Preview, OK, & Cancel buttons
   vSizer->Add(CreateStdButtonSizer(this, ePreviewButton|eCancelButton|eOkButton), 0, wxEXPAND);

   SetSizer(vSizer);

   wxSizer *paramSizer =
      new wxStaticBoxSizer(wxVERTICAL, w, _("Effect Settings"));

   wxFlexGridSizer *gridSizer =
      new wxFlexGridSizer(5, 0, 0);
   gridSizer->AddGrowableCol(3);

   // Now add the length control
   if (mEffect->GetEffectFlags() & INSERT_EFFECT)
   {
      item = new wxStaticText(w, 0, _("Length (seconds)"));
      gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
      mSeconds = new wxTextCtrl(w, LADSPA_SECONDS_ID, Internat::ToDisplayString(length));
      mSeconds->SetName(_("Length (seconds)"));
      gridSizer->Add(mSeconds, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
      gridSizer->Add(1, 1, 0);
      gridSizer->Add(1, 1, 0);
      gridSizer->Add(1, 1, 0);
      ConnectFocus(mSeconds);
   }

   // The note controls if the plugin is a synth
   if (mEffect->IsSynth())
   {
      // Note length control
      item = new wxStaticText(w, wxID_ANY, _("Note length (seconds)"));
      gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);
      mNoteSeconds = new wxTextCtrl(w, LADSPA_SECONDS_ID, Internat::ToDisplayString(length / 2));
      mNoteSeconds->SetName(_("Note length (seconds)"));
      gridSizer->Add(mNoteSeconds, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
      gridSizer->Add(1, 1, 0);
      gridSizer->Add(1, 1, 0);
      gridSizer->Add(1, 1, 0);
      ConnectFocus(mNoteSeconds);

      // Note velocity control
      item = new wxStaticText(w, wxID_ANY, _("Note velocity"));
      gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);
      mNoteVelocity = new wxTextCtrl(w, LADSPA_SECONDS_ID, Internat::ToDisplayString(64));
      mNoteVelocity->SetName(_("Note velocity"));
      gridSizer->Add(mNoteVelocity, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
      gridSizer->Add(1, 1, 0);
      gridSizer->Add(1, 1, 0);
      gridSizer->Add(1, 1, 0);
      ConnectFocus(mNoteVelocity);

      // Note key control
      item = new wxStaticText(w, wxID_ANY, _("Note key"));
      gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);
      mNoteKey = new wxTextCtrl(w, LADSPA_SECONDS_ID, Internat::ToDisplayString(64));
      mNoteKey->SetName(_("Note key"));
      gridSizer->Add(mNoteKey, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
      gridSizer->Add(1, 1, 0);
      gridSizer->Add(1, 1, 0);
      gridSizer->Add(1, 1, 0);
      ConnectFocus(mNoteKey);
   }

   paramSizer->Add(gridSizer, 1, wxEXPAND | wxALL, 5);

   // Create user parameter controls
   std::queue<const LV2PortGroup *> groups;
   groups.push(&mEffect->GetRootGroup());

   while (!groups.empty())
   {
      const LV2PortGroup* pg = groups.front();
      groups.pop();

      if (pg->GetName() != wxEmptyString)
      {
         wxSizer *groupSizer =
            new wxStaticBoxSizer(wxVERTICAL, w, pg->GetName());
         paramSizer->Add(groupSizer, 0, wxEXPAND | wxALL, 5);
         gridSizer = new wxFlexGridSizer(5, 0, 0);
         gridSizer->AddGrowableCol(3);
         groupSizer->Add(gridSizer, 1, wxEXPAND | wxALL, 5);
      }

      const LV2PortGroupArray & subgroups = pg->GetSubGroups();
      LV2PortGroupArray::const_iterator si;
      for (si = subgroups.begin(); si != subgroups.end(); si++)
      {
         if ((*si)->GetParameters().GetCount() != 0)
         {
            groups.push(*si);
         }
      }

      const wxArrayInt & params = pg->GetParameters();
      for (size_t pi = 0; pi < params.GetCount(); pi++)
      {
         int p = params[pi];
         if (p >= ctrlcnt)
         {
            continue;
         }
         LV2Port & port = mControls[p];

         wxString labelText = port.mName;
         item = new wxStaticText(w, 0, labelText + wxT(":"));
         gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

         wxString fieldText;

         if (port.mToggle)
         {
            mToggles[p] = new wxCheckBox(w, p, wxT(""));
            mToggles[p]->SetName(labelText);
            mToggles[p]->SetValue(port.mControlBuffer > 0);
            gridSizer->Add(mToggles[p], 0, wxALL, 5);
            ConnectFocus(mToggles[p]);

            gridSizer->Add(1, 1, 0);
            gridSizer->Add(1, 1, 0);
            gridSizer->Add(1, 1, 0);
         }
         else if (port.mEnumeration)
         {
            mEnums[p] = new wxChoice(w, p);
            mEnums[p]->SetName(labelText);
            mEnums[p]->Append(port.mScaleLabels);
            int s;
            for (s = (int) port.mScaleValues.GetCount() - 1; s >= 0; s--)
            {
               if (port.mControlBuffer >= port.mScaleValues[s])
               {
                  break;
               }
            }
            if (s < 0)
            {
               s = 0;
            }
            mEnums[p]->SetSelection(s);
            gridSizer->Add(mEnums[p], 0, wxALL | wxEXPAND, 5);
            ConnectFocus(mEnums[p]);

            gridSizer->Add(1, 1, 0);
            gridSizer->Add(1, 1, 0);
            gridSizer->Add(1, 1, 0);
         }
         else
         {
            if (port.mInteger)
            {
               fieldText.Printf(wxT("%d"), (int)(port.mControlBuffer + 0.5));
            }
            else
            {
               fieldText = Internat::ToDisplayString(port.mControlBuffer);
            }

            mFields[p] = new wxTextCtrl(w, p, fieldText);
            mFields[p]->SetName(labelText);
            gridSizer->Add(mFields[p], 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
            ConnectFocus(mFields[p]);

            wxString bound;
            double lower = 0.0;
            double upper = 0.0;
            bool haslo = false;
            bool hashi = false;
            bool forceint = false;
            wxString loLabel;
            wxString hiLabel;
#if 0
            ScalePointMap::const_iterator iter =
               scalePoints.find(port.mIndex);

            if (!wxNaN(port.mMin))
            {
               lower = port.mMin;
               haslo = true;
               if (iter != scalePoints.end())
               {
                  std::map<float, wxString>::const_iterator iter2 =
                     iter->second.find(lower);
                  if (iter2 != iter->second.end())
                  {
                     loLabel = iter2->second;
                  }
               }
            }

            if (!isnan(port.mMax))
            {
               upper = port.mMax;
               hashi = true;
               if (iter != scalePoints.end())
               {
                  std::map<float, wxString>::const_iterator iter2 =
                     iter->second.find(upper);
                  if (iter2 != iter->second.end())
                  {
                     hiLabel = iter2->second;
                  }
               }
            }
#endif
            if (port.mSampleRate)
            {
               lower *= mSampleRate * 1000;
               upper *= mSampleRate;
               forceint = true;
            }

            wxString str;
            if (haslo)
            {
               str = loLabel;
               if (str.IsEmpty())
               {
                  if (port.mInteger || forceint)
                  {
                     str.Printf(wxT("%d"), (int)(lower + 0.5));
                  }
                  else
                  {
                     str = Internat::ToDisplayString(lower);
                  }
               }
               item = new wxStaticText(w, wxID_ANY, str);
               gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);
            }
            else
            {
               gridSizer->Add(1, 1, 0);
            }

            mSliders[p] =
               new wxSlider(w, p,
                            0, 0, 1000,
                            wxDefaultPosition,
                            wxSize(200, -1));
            mSliders[p]->SetName(labelText);
            gridSizer->Add(mSliders[p], 0, wxALIGN_CENTER_VERTICAL | wxEXPAND | wxALL, 5);
            ConnectFocus(mSliders[p]);

            if (hashi)
            {
               str = hiLabel;
               if (str.IsEmpty())
               {
                  if (port.mInteger || forceint)
                  {
                     str.Printf(wxT("%d"), (int)(upper + 0.5));
                  }
                  else
                  {
                     str = Internat::ToDisplayString(upper);
                  }
               }
               item = new wxStaticText(w, wxID_ANY, str);
               gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 5);
            }
            else
            {
               gridSizer->Add(1, 1, 0);
            }
         }
      }
   }

   // Set all of the mSliders based on the value in the
   // text mFields
   inSlider = false; // Now we're ready for HandleText to actually do something.
   HandleText();

   w->SetSizer(paramSizer);

   Layout();
   Fit();
   SetSizeHints(GetSize());
}

LV2EffectDialog::~LV2EffectDialog()
{
   delete [] mSliders;
   delete [] mFields;
   delete [] mLabels;
}

void LV2EffectDialog::OnCheckBox(wxCommandEvent &event)
{
   int p = event.GetId();

   mControls[p].mControlBuffer = mToggles[p]->GetValue();
}

void LV2EffectDialog::OnSlider(wxCommandEvent &event)
{
   int p = event.GetId();

   // if we don't add the following three lines, changing
   // the value of the slider will change the text, which
   // will change the slider, and so on.  This gets rid of
   // the implicit loop.
   if (inText)
      return;
   inSlider = true;

   float val;
   float lower = float(0.0);
   float upper = float(10.0);
   float range;
   bool forceint = false;

   if (isfinite(mControls[p].mMin))
   {
      lower = mControls[p].mMin;
   }

   if (isfinite(mControls[p].mMax))
   {
      upper = mControls[p].mMax;
   }

   if (mControls[p].mSampleRate)
   {
      lower *= mSampleRate;
      upper *= mSampleRate;
      forceint = true;
   }

   range = upper - lower;

   val = (mSliders[p]->GetValue() / 1000.0) * range + lower;

   // Force the value to an integer if requested
   wxString str;
   if (mControls[p].mInteger || forceint)
   {
      str.Printf(wxT("%d"), (int)(val + 0.5));
   }
   else
   {
      str = Internat::ToDisplayString(val);
   }

   mFields[p]->SetValue(str);

   mControls[p].mControlBuffer = val;

   inSlider = false;
}

void LV2EffectDialog::OnChoiceCtrl(wxCommandEvent & WXUNUSED(event))
{
//   HandleText();
}

void LV2EffectDialog::OnTextCtrl(wxCommandEvent & WXUNUSED(event))
{
   HandleText();
}

void LV2EffectDialog::HandleText()
{
   // if we don't add the following three lines, changing
   // the value of the slider will change the text, which
   // will change the slider, and so on.  This gets rid of
   // the implicit loop.

   if (inSlider)
   {
      return;
   }
   inText = true;

   for (uint32_t p = 0; p < mControls.GetCount(); p++)
   {
      double dval;
      float val;
      float lower = float(0.0);
      float upper = float(10.0);
      float range;

      if (mControls[p].mToggle)
      {
         continue;
      }

      if (mControls[p].mEnumeration)
      {
         continue;
      }

      dval = Internat::CompatibleToDouble(mFields[p]->GetValue());
      val = dval;

      if (!isnan(mControls[p].mMin))
      {
         lower = mControls[p].mMin;
      }

      if (!isnan(mControls[p].mMax))
      {
         upper = mControls[p].mMax;
      }

      if (mControls[p].mSampleRate)
      {
         lower *= mSampleRate;
         upper *= mSampleRate;
      }
      range = upper - lower;

      if (val < lower)
      {
         val = lower;
      }

      if (val > upper)
      {
         val = upper;
      }

      mControls[p].mControlBuffer = val;

      mSliders[p]->SetValue((int)(((val-lower)/range) * 1000.0 + 0.5));
   }

   inText = false;
}

void LV2EffectDialog::OnOK(wxCommandEvent & WXUNUSED(event))
{
   EndModal(TRUE);
}

void LV2EffectDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   EndModal(FALSE);
}

void LV2EffectDialog::OnPreview(wxCommandEvent & WXUNUSED(event))

{
   mEffect->Preview();
}

void LV2EffectDialog::ConnectFocus(wxControl *c)
{
   c->GetEventHandler()->Connect(wxEVT_SET_FOCUS,
                                 wxFocusEventHandler(LV2EffectDialog::ControlSetFocus));
}

void LV2EffectDialog::DisconnectFocus(wxControl *c)
{
   c->GetEventHandler()->Disconnect(wxEVT_SET_FOCUS,
                                    wxFocusEventHandler(LV2EffectDialog::ControlSetFocus));
}

void LV2EffectDialog::ControlSetFocus(wxFocusEvent &event)
{
   wxControl *c = (wxControl *) event.GetEventObject();
   wxScrolledWindow *p = (wxScrolledWindow *) c->GetParent();
   wxRect r = c->GetRect();
   wxRect rv = p->GetRect();
   rv.y = 0;

   event.Skip();

   int y;
   int yppu;
   p->GetScrollPixelsPerUnit(NULL, &yppu);

   if (r.y >= rv.y && r.GetBottom() <= rv.GetBottom())
   {
      return;
   }

   if (r.y < rv.y)
   {
      p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
      y = r.y / yppu;
   }
   else
   {
      p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
      y = (r.GetBottom() - rv.GetBottom() + yppu) / yppu;
   }

   p->Scroll(-1, y);
};

double LV2EffectDialog::GetLength()
{
   if (mEffect->GetEffectFlags() & INSERT_EFFECT)
   {
      mLength = Internat::CompatibleToDouble(mSeconds->GetValue());
   }

   return mLength;
}


double LV2EffectDialog::GetNoteLength()
{
   if (mEffect->IsSynth())
   {
      return Internat::CompatibleToDouble(mNoteSeconds->GetValue());
   }
   return 0;
}

unsigned char LV2EffectDialog::GetNoteVelocity()
{
   if (mEffect->IsSynth())
   {
      double velocity =
         Internat::CompatibleToDouble(mNoteVelocity->GetValue());

      if (velocity < 1)
      {
         return 1;
      }

      if (velocity > 127)
      {
         return 127;
      }

      return (unsigned char)velocity;
   }
   return 64;
}

unsigned char LV2EffectDialog::GetNoteKey()
{
   if (mEffect->IsSynth())
   {
      double key =
         Internat::CompatibleToDouble(mNoteKey->GetValue());

      if (key < 1)
      {
         return 1;
      }

      if (key > 127)
      {
         return 127;
      }

      return (unsigned char)key;
   }
   return 64;
}

#endif
