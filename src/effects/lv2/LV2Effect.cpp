/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2Effect.h

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

**********************************************************************/

#include <cmath>
#include <queue>

#include "../Audacity.h"

#include <wx/wxprec.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/tokenzr.h>
#include <wx/intl.h>
#include <wx/scrolwin.h>

#if defined(USE_SLV2)

#include "../Effect.h"
#include "LoadLV2.h"
#include "LV2Effect.h"
#include "LV2PortGroup.h"
#include "../Internat.h"
#include "lv2_event_helpers.h"

LV2Effect::LV2Effect(SLV2Plugin data,
                     const std::set<wxString>& categories)
   : mValid(true),
     mCategories(categories),
     mMidiInput(0),
     mScalePointsRetrieved(false),
     mPortGroupsRetrieved(false) {
   
   // We don't support any features at all, so if the plugin requires
   // any we skip it.
   SLV2Values req = slv2_plugin_get_required_features(data);
   size_t nFeatures = slv2_values_size(req);
   slv2_values_free(req);
   if (nFeatures > 0) {
      mValid = false;
      return;
   }

   mData = data;
   pluginName = 
      wxString::FromUTF8(slv2_value_as_string(slv2_plugin_get_name(mData)));
   
   fInBuffer = NULL;
   fOutBuffer = NULL;
   
   mLength = 0;

   uint32_t p;
   
   // Allocate buffers for the port indices and the default control values
   uint32_t numPorts = slv2_plugin_get_num_ports(mData);
   float* minimumValues = new float [numPorts];
   float* maximumValues = new float [numPorts];
   float* defaultValues = new float [numPorts];
   
   // Retrieve the port ranges for all ports (some values may be NaN)
   slv2_plugin_get_port_ranges_float(mData, minimumValues, 
                                     maximumValues, defaultValues);
   
   // Get info about all ports
   for(p = 0; p < numPorts; p++) {
      SLV2Port port = slv2_plugin_get_port_by_index(mData, p);
      LV2Port internalPort;
      internalPort.mIndex = p;

      // Get the port name
      SLV2Value tmpName = slv2_port_get_name(data, port);
      internalPort.mName = LAT1CTOWX(slv2_value_as_string(tmpName));
      slv2_value_free(tmpName);
      
      // Get the port type
      if (slv2_port_is_a(mData, port, gAudioPortClass)) {
         if (slv2_port_is_a(mData, port, gInputPortClass))
            mAudioInputs.push_back(internalPort);
         else if (slv2_port_is_a(mData, port, gOutputPortClass))
            mAudioOutputs.push_back(internalPort);
      }

      else if (slv2_port_is_a(mData, port, gControlPortClass) &&
          slv2_port_is_a(mData, port, gInputPortClass)) {
         internalPort.mControlBuffer = float(1.0);
         internalPort.mMin = minimumValues[p];
         internalPort.mMax = maximumValues[p];
         internalPort.mDefault = defaultValues[p];
         if (std::isfinite(defaultValues[p]))
            internalPort.mControlBuffer = defaultValues[p];
         else if (std::isfinite(minimumValues[p]))
            internalPort.mControlBuffer = minimumValues[p];
         else if (std::isfinite(maximumValues[p]))
            internalPort.mControlBuffer = maximumValues[p];
         if (slv2_port_has_property(data, port, gPortToggled))
            internalPort.mToggle = true;
         if (slv2_port_has_property(data, port, gPortIsInteger))
            internalPort.mInteger = true;
         if (slv2_port_has_property(data, port, gPortIsSampleRate))
            internalPort.mSampleRate = true;

         mControlInputs.push_back(internalPort);
      }
      
      else if (slv2_port_is_a(mData, port, gMidiPortClass) &&
               slv2_port_is_a(mData, port, gInputPortClass)) {
         // If there are more than one MIDI input ports, the plugin is invalid
         if (mMidiInput) {
            mValid = false;
            continue;
         }
         mMidiInput = new LV2Port(internalPort);
      }
      
      else {
         // Unknown port type, we set the invalid flag
         mValid = false;
      }
   }
   
   delete [] minimumValues;
   delete [] maximumValues;
   delete [] defaultValues;
   
   // MIDI synths may not have any audio inputs.
   if (mMidiInput && mAudioInputs.size() > 0)
      mValid = false;
   
   // Determine whether the plugin is a generator, effect or analyser 
   // depending on the number of ports of each type (not completely accurate,
   // but works most of the time)
   int flags = PLUGIN_EFFECT;
   if (mAudioInputs.size() == 0)
      flags |= INSERT_EFFECT;
   else if (mAudioOutputs.size() == 0)
      flags |= ANALYZE_EFFECT;
   else
      flags |= PROCESS_EFFECT;

   SetEffectFlags(flags);
}

LV2Effect::~LV2Effect()
{
   if (mMidiInput)
      delete mMidiInput;
}

wxString LV2Effect::GetEffectName()
{
   if (mControlInputs.size() > 0)
      return pluginName + wxT("...");
   else
      return pluginName;
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
   while (st.HasMoreTokens()) {
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
   while(left) {
      if (mainRate == 0)
         mainRate = (int)(((WaveTrack *)left)->GetRate() + 0.5);
      
      if (left->GetLinked()) {
         Track *right = iter.Next();
         
         if (((WaveTrack *)left)->GetRate() !=
             ((WaveTrack *)right)->GetRate()) {
            wxMessageBox(_("Sorry, Plug-in Effects cannot be performed on stereo tracks where the individual channels of the track do not match."));
            return false;
         }
      }
      
      left = iter.Next();
   }

   if (mainRate<=0)
      mainRate = (int)(mProjectRate + 0.5);

   return true;
}

bool LV2Effect::PromptUser()
{
   if (mControlInputs.size() > 0) {
      double length = mT1 > mT0 ? mT1 - mT0 : sDefaultGenerateLen;
      double noteLength = length / 2;
      unsigned char noteVelocity = 64;
      unsigned char noteKey = 64;

      LV2EffectDialog dlog(this, mParent, mData, mainRate, length,
                           noteLength, noteVelocity, noteKey);
      dlog.CentreOnParent();
      dlog.ShowModal();
      
      if (!dlog.GetReturnCode())
         return false;

      mLength = dlog.GetLength();
      mNoteLength = dlog.GetNoteLength();
      mNoteVelocity = dlog.GetNoteVelocity();
      mNoteKey = dlog.GetNoteKey();
   }
   return true;
}

bool LV2Effect::Process()
{
   this->CopyInputWaveTracks(); // Set up mOutputWaveTracks.
   bool bGoodResult = true;
   
   TrackListIterator iter(mOutputWaveTracks);
   int count = 0;
   Track *left = iter.First();
   Track *right;
   while(left) {
      sampleCount lstart, rstart;
      sampleCount len;
      GetSamples((WaveTrack *)left, &lstart, &len);
      
      right = NULL;
      if (left->GetLinked() && mAudioInputs.size() > 1) {
         right = iter.Next();         
         GetSamples((WaveTrack *)right, &rstart, &len);
      }

      if (mAudioInputs.size() < 2 && right) {
         // If the effect is mono, apply to each channel separately

         bGoodResult = ProcessStereo(count, (WaveTrack *)left, NULL,
                                     lstart, 0, len) && 
            ProcessStereo(count, (WaveTrack *)right, NULL,
                          rstart, 0, len);
      }
      else bGoodResult = ProcessStereo(count,
                                       (WaveTrack *)left, (WaveTrack *)right,
                                       lstart, rstart, len);
      if (!bGoodResult)
         break;
   
      left = iter.Next();
      count++;
   }

   this->ReplaceProcessedWaveTracks(bGoodResult); 
   return bGoodResult;
}

bool LV2Effect::ProcessStereo(int count, WaveTrack *left, WaveTrack *right,
                              sampleCount lstart, 
                              sampleCount rstart,
                              sampleCount len)
{
   
   /* Allocate buffers */
   if (mBlockSize == 0) {
      mBlockSize = left->GetMaxBlockSize() * 2;

      fInBuffer = new float *[mAudioInputs.size()];
      unsigned long i;
      for (i = 0; i < mAudioInputs.size(); i++)
         fInBuffer[i] = new float[mBlockSize];
      fOutBuffer = new float *[mAudioOutputs.size()];
      for (i = 0; i < mAudioOutputs.size(); i++)
         fOutBuffer[i] = new float[mBlockSize];
   }

   /* Instantiate the plugin */
   SLV2Instance handle = slv2_plugin_instantiate(mData, left->GetRate(), 
                                                 gLV2Features);
   
   /* Write the Note On to the MIDI event buffer and connect it */
   LV2_Event_Buffer* midiBuffer;
   int noteOffTime;
   if (mMidiInput) {
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
      slv2_instance_connect_port(handle, mMidiInput->mIndex, midiBuffer);
   }

   unsigned long p;
   for(p = 0; p < mAudioInputs.size(); p++) {
      slv2_instance_connect_port(handle, mAudioInputs[p].mIndex, fInBuffer[p]);
   }
   for(p = 0; p < mAudioOutputs.size(); p++) {
      slv2_instance_connect_port(handle, mAudioOutputs[p].mIndex, fOutBuffer[p]);
   }
   for (p = 0; p < mControlInputs.size(); p++) {
      slv2_instance_connect_port(handle, mControlInputs[p].mIndex, 
                                 &mControlInputs[p].mControlBuffer);
   }
   for (p = 0; p < mControlOutputs.size(); p++) {
      slv2_instance_connect_port(handle, mControlOutputs[p].mIndex, 
                                 &mControlOutputs[p].mControlBuffer);
   }
   
   slv2_instance_activate(handle);

   // Actually perform the effect here

   sampleCount originalLen = len;
   sampleCount ls = lstart;
   sampleCount rs = rstart;
   bool noteOver = false;
   while (len) {
      int block = mBlockSize;
      if (block > len)
         block = len;
      
      if (left &&  mAudioInputs.size() > 0) {
         left->Get((samplePtr)fInBuffer[0], floatSample, ls, block);
      }
      if (right && mAudioInputs.size() > 1) {
         right->Get((samplePtr)fInBuffer[1], floatSample, rs, block);
      }
      
      slv2_instance_run(handle, block);
      
      if (left && mAudioOutputs.size() > 0) {
         left->Set((samplePtr)fOutBuffer[0], floatSample, ls, block);
      }
      
      if (right && mAudioOutputs.size() > 1) {
         right->Set((samplePtr)fOutBuffer[1], floatSample, rs, block);
      }
      
      len -= block;
      noteOffTime -= block;
      ls += block;
      rs += block;
      
      // Clear the event buffer and add the note off event if needed
      if (mMidiInput) {
         lv2_event_buffer_reset(midiBuffer, 1, 
                                (uint8_t*)midiBuffer + 
                                sizeof(LV2_Event_Buffer));
         if (!noteOver && noteOffTime < len && noteOffTime < block) {
            LV2_Event_Iterator iter;
            lv2_event_begin(&iter, midiBuffer);
            uint8_t noteOff[] = { 0x80, mNoteKey, 64 };
            lv2_event_write(&iter, noteOffTime, 0, 1, 3, noteOff);
            noteOver = true;
         }
      }
      
      if (mAudioInputs.size() > 1) {
         if (TrackGroupProgress(count, (ls-lstart)/(double)originalLen))
            return false;
      }
      else {
         if (TrackProgress(count, (ls-lstart)/(double)originalLen))
            return false;
      }
      
   }
   
   slv2_instance_deactivate(handle);
   slv2_instance_free(handle);
   
   return true;
}

void LV2Effect::End()
{
   unsigned long i;

   if (fInBuffer) {
      for (i = 0; i < mAudioInputs.size(); i++) {
         if (fInBuffer[i]) {
            delete [] fInBuffer[i];
         }
      }
      delete [] fInBuffer;
      fInBuffer = NULL;
   }

   if (fOutBuffer) {
      for (i = 0; i < mAudioOutputs.size(); i++) {
         if (fOutBuffer[i]) {
            delete [] fOutBuffer[i];
         }
      }
      delete [] fOutBuffer;
      fOutBuffer = NULL;
   }
}


bool LV2Effect::IsValid() {
   return mValid;
}


std::vector<LV2Port>& LV2Effect::GetControls() {
   return mControlInputs;
}


bool LV2Effect::IsSynth() {
   return (mMidiInput != 0);
}


bool LV2Effect::SetNote(sampleCount len, 
                        unsigned char velocity, unsigned char key) {
   if (velocity == 0 || velocity > 127 || key > 127)
      return false;
   mNoteLength = len;
   mNoteVelocity = velocity;
   mNoteKey = key;
   return true;
}


const ScalePointMap& LV2Effect::GetScalePoints() {

   if (!mScalePointsRetrieved) {
      
      char scalePointQuery[] = 
         "PREFIX : <http://lv2plug.in/ns/lv2core#>\n"
         "SELECT ?index, ?value, ?label WHERE {\n"
         "<> :port ?port.\n"
         "?port a :ControlPort.\n"
         "?port a :InputPort.\n"
         "?port :index ?index.\n"
         "?port :scalePoint ?point.\n"
         "?point rdf:value ?value.\n"
         "?point rdfs:label ?label.\n"
         "}";
      
      SLV2Values portIndices = slv2_plugin_query_variable(mData, 
                                                          scalePointQuery, 0);
      SLV2Values pointValues = slv2_plugin_query_variable(mData, 
                                                          scalePointQuery, 1);
      SLV2Values pointLabels = slv2_plugin_query_variable(mData, 
                                                          scalePointQuery, 2);
      
      size_t nScalePoints = slv2_values_size(portIndices);
      for (size_t i = 0; i < nScalePoints; ++i) {
         uint32_t idx = slv2_value_as_int(slv2_values_get_at(portIndices, i));
         float value = slv2_value_as_float(slv2_values_get_at(pointValues, i));
         wxString label = wxString::FromUTF8(slv2_value_as_string(slv2_values_get_at(pointLabels, i)));
         mScalePoints[idx][value] = label;
      }
      slv2_values_free(portIndices);
      slv2_values_free(pointValues);
      slv2_values_free(pointLabels);
      
      mScalePointsRetrieved = true;
   }
   
   return mScalePoints;
}


const LV2PortGroup& LV2Effect::GetPortGroups() {

   if (!mPortGroupsRetrieved) {
      
      // Find all port groups with ports in them.
      char portGroupQuery[] = 
         "PREFIX : <http://lv2plug.in/ns/lv2core#>\n"
         "PREFIX pg: <http://ll-plugins.nongnu.org/lv2/ext/portgroups#>\n"
         "SELECT ?index, ?uri, ?label WHERE {\n"
         "<> :port ?port.\n"
         "?port :index ?index.\n"
         "?port pg:membership ?ms.\n"
         "?ms pg:group ?uri.\n"
         "?uri rdfs:label ?label.\n"
         "}";
      
      SLV2Values portIndices = slv2_plugin_query_variable(mData, 
                                                          portGroupQuery, 0);
      SLV2Values groupUris = slv2_plugin_query_variable(mData, 
                                                          portGroupQuery, 1);
      SLV2Values groupLabels = slv2_plugin_query_variable(mData, 
                                                          portGroupQuery, 2);
      
      std::map<wxString, LV2PortGroup> portGroups;
      std::vector<bool> inGroup(mControlInputs.size(), false);
      size_t nMemberships = slv2_values_size(portIndices);
      for (size_t i = 0; i < nMemberships; ++i) {
         uint32_t idx = slv2_value_as_int(slv2_values_get_at(portIndices, i));
         uint32_t p;
         for (p = 0; p < mControlInputs.size(); ++p) {
            if (mControlInputs[p].mIndex == idx)
               break;
         }
         if (p == mControlInputs.size())
            continue;
         wxString uri = wxString::FromUTF8(slv2_value_as_string(slv2_values_get_at(groupUris, i)));
         wxString label = wxString::FromUTF8(slv2_value_as_string(slv2_values_get_at(groupLabels, i)));
         std::map<wxString, LV2PortGroup>::iterator iter = 
            portGroups.find(uri);
         if (iter == portGroups.end())
            portGroups[uri] = LV2PortGroup(label);
         portGroups[uri].AddParameter(p);
         inGroup[p] = true;
      }
      slv2_values_free(portIndices);
      slv2_values_free(groupUris);
      slv2_values_free(groupLabels);
      
      // Add all ports that aren't in any port groups to the root group.
      for (uint32_t p = 0; p < mControlInputs.size(); ++p) {
         if (!inGroup[p])
            mRootGroup.AddParameter(p);
      }
      
      // Find all subgroup relationships.
      char subGroupQuery[] = 
         "PREFIX : <http://lv2plug.in/ns/lv2core#>\n"
         "PREFIX pg: <http://ll-plugins.nongnu.org/lv2/ext/portgroups#>\n"
         "SELECT ?sub, ?parent WHERE {\n"
         "?sub pg:subgroupOf ?parent.\n"
         "}";
      
      SLV2Values subs = slv2_plugin_query_variable(mData, subGroupQuery, 0);
      SLV2Values parents = slv2_plugin_query_variable(mData, subGroupQuery, 1);
      size_t nSubgroups = slv2_values_size(subs);
      for (size_t i = 0; i < nSubgroups; ++i) {
         wxString parent = 
            wxString::FromUTF8(slv2_value_as_uri(slv2_values_get_at(parents, i)));
         wxString sub = 
            wxString::FromUTF8(slv2_value_as_uri(slv2_values_get_at(subs, i)));
         std::map<wxString, LV2PortGroup>::iterator iter = 
            portGroups.find(parent);
         std::map<wxString, LV2PortGroup>::iterator iter2 = 
            portGroups.find(sub);
         if (iter != portGroups.end() && iter2 != portGroups.end()) {
            iter->second.AddSubGroup(iter2->second);
         }
      }
      slv2_values_free(subs);
      slv2_values_free(parents);

      // Make all groups subgroups of the root group.
      std::map<wxString, LV2PortGroup>::iterator iter;
      for (iter = portGroups.begin(); iter != portGroups.end(); ++iter)
         mRootGroup.AddSubGroup(iter->second);
      
      mPortGroupsRetrieved = true;
   }
   
   std::queue<const LV2PortGroup*> groups;
   groups.push(&mRootGroup);
   while (!groups.empty()) {
      const LV2PortGroup* g = groups.front();
      groups.pop();
      const std::vector<LV2PortGroup>& subs = g->GetSubGroups();
      for (std::vector<LV2PortGroup>::const_iterator iter = subs.begin();
           iter != subs.end(); ++iter)
         groups.push(&*iter);
   }
   
   return mRootGroup;
}


// This should be moved to its own source file, it's in LadspaEffect.cpp 
// as well
class LV2Slider:public wxSlider
{
 public:
   LV2Slider(wxWindow *parent, wxWindowID id,
             int value, int minValue, int maxValue,
             const wxPoint& pos = wxDefaultPosition,
             const wxSize& size = wxDefaultSize,
             long style = wxSL_HORIZONTAL,
             const wxValidator& validator = wxDefaultValidator,
             const wxString& name = wxSliderNameStr)
      : wxSlider(parent, id, value, minValue, maxValue,
                 pos, size, style, validator, name)
   {
   };

   void OnSetFocus(wxFocusEvent &event)
   {
      wxScrolledWindow *p = (wxScrolledWindow *) GetParent();
      wxRect r = GetRect();
      wxRect rv = p->GetRect();
      rv.y = 0;

      event.Skip();

      int y;
      int yppu;
      p->GetScrollPixelsPerUnit(NULL, &yppu);

      if (r.y >= rv.y && r.GetBottom() <= rv.GetBottom()) {
         return;
      }

      if (r.y < rv.y) {
         p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
         y = r.y / yppu;
      }
      else {
         p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
         y = (r.GetBottom() - rv.GetBottom() + yppu) / yppu;
      }

      p->Scroll(-1, y);
   };

   DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(LV2Slider, wxSlider)
    EVT_SET_FOCUS(LV2Slider::OnSetFocus)
END_EVENT_TABLE()

class LV2TextCtrl:public wxTextCtrl
{
 public:
   LV2TextCtrl(wxWindow *parent, wxWindowID id,
            const wxString& value = wxEmptyString,
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize, long style = 0,
            const wxValidator& validator = wxDefaultValidator,
            const wxString& name = wxTextCtrlNameStr)
   : wxTextCtrl(parent, id, value,
                pos, size, style, validator, name)
   {
   };

   void OnSetFocus(wxFocusEvent &event)
   {
      wxScrolledWindow *p = (wxScrolledWindow *) GetParent();
      wxRect r = GetRect();
      wxRect rv = p->GetRect();
      rv.y = 0;

      event.Skip();

      int y;
      int yppu;
      p->GetScrollPixelsPerUnit(NULL, &yppu);

      if (r.y >= rv.y && r.GetBottom() <= rv.GetBottom()) {
         return;
      }

      if (r.y < rv.y) {
         p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
         y = r.y / yppu;
      }
      else {
         p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
         y = (r.GetBottom() - rv.GetBottom() + yppu) / yppu;
      }

      p->Scroll(-1, y);
   };

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
END_EVENT_TABLE()

IMPLEMENT_CLASS(LV2EffectDialog, wxDialog)

LV2EffectDialog::LV2EffectDialog(LV2Effect *eff,
                                 wxWindow * parent,
                                 SLV2Plugin data,
                                 int sampleRate,
                                 double length,
                                 double noteLength,
                                 unsigned char noteVelocity,
                                 unsigned char noteKey)
   :wxDialog(parent, -1, 
             LAT1CTOWX(slv2_value_as_string(slv2_plugin_get_name(data))),
             wxDefaultPosition, wxDefaultSize,
             wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
    effect(eff),
    mControls(eff->GetControls())
{
   mLength = length;
   this->mData = data;
   this->sampleRate = sampleRate;
   #ifdef __WXMSW__
      // On Windows, for some reason, wxWindows calls OnTextCtrl during creation
      // of the text control, and LV2EffectDialog::OnTextCtrl calls HandleText,
      // which assumes all the fields have been initialized.
      // This can give us a bad pointer crash, so manipulate inSlider to
      // no-op HandleText during creation.
      inSlider = true;
   #else
      inSlider = false;
   #endif
   inText = false;
   
   // Allocate memory for the user parameter controls
   toggles = new wxCheckBox*[mControls.size()];
   sliders = new wxSlider*[mControls.size()];
   fields = new wxTextCtrl*[mControls.size()];
   labels = new wxStaticText*[mControls.size()];
   
   wxControl *item;

   wxBoxSizer *vSizer = new wxBoxSizer(wxVERTICAL);
   
   // Add information about the plugin
   SLV2Value tmpValue = slv2_plugin_get_author_name(data);
   if (tmpValue) {
      const char* author = slv2_value_as_string(tmpValue);
      item = new wxStaticText(this, 0,
                              wxString(_("Author: "))+LAT1CTOWX(author));
      vSizer->Add(item, 0, wxALL, 5);
      slv2_value_free(tmpValue);
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
   
   const LV2PortGroup& rootGroup = eff->GetPortGroups();
   const ScalePointMap& scalePoints = eff->GetScalePoints();
   
   // Now add the length control
   if (effect->GetEffectFlags() & INSERT_EFFECT) {
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
   if (effect->IsSynth()) {
      
      // Note length control
      item = new wxStaticText(w, 0, _("Note length (seconds)"));
      gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);
      mNoteSeconds = new wxTextCtrl(w, LADSPA_SECONDS_ID, Internat::ToDisplayString(length / 2));
      mNoteSeconds->SetName(_("Note length (seconds)"));
      gridSizer->Add(mNoteSeconds, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
      gridSizer->Add(1, 1, 0);
      gridSizer->Add(1, 1, 0);
      gridSizer->Add(1, 1, 0);
      ConnectFocus(mNoteSeconds);
      
      // Note velocity control
      item = new wxStaticText(w, 0, _("Note velocity"));
      gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);
      mNoteVelocity = new wxTextCtrl(w, LADSPA_SECONDS_ID, Internat::ToDisplayString(64));
      mNoteVelocity->SetName(_("Note velocity"));
      gridSizer->Add(mNoteVelocity, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
      gridSizer->Add(1, 1, 0);
      gridSizer->Add(1, 1, 0);
      gridSizer->Add(1, 1, 0);
      ConnectFocus(mNoteVelocity);

      // Note key control
      item = new wxStaticText(w, 0, _("Note key"));
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
   std::queue<const LV2PortGroup*> groups;
   groups.push(&rootGroup);
   
   while (!groups.empty()) {
      
      const LV2PortGroup* pg = groups.front();
      groups.pop();
      
      if (pg->GetName() != wxT("")) {
         wxSizer *groupSizer =
            new wxStaticBoxSizer(wxVERTICAL, w, pg->GetName());
         paramSizer->Add(groupSizer, 0, wxEXPAND | wxALL, 5);
         gridSizer = new wxFlexGridSizer(5, 0, 0);
         gridSizer->AddGrowableCol(3);
         groupSizer->Add(gridSizer, 1, wxEXPAND | wxALL, 5);
      }
      
      std::vector<LV2PortGroup>::const_iterator iter;
      for (iter = pg->GetSubGroups().begin(); iter != pg->GetSubGroups().end();
           ++iter) {
         groups.push(&*iter);
      }
      
      const std::vector<uint32_t>& params = pg->GetParameters();
      for (uint32_t k = 0; k < params.size(); ++k) {
         uint32_t p = params[k];
         
         wxString labelText = mControls[p].mName;
         item = new wxStaticText(w, 0, labelText + wxT(":"));
         gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);
         
         wxString fieldText;
         
         if (mControls[p].mToggle) {
            toggles[p] = new wxCheckBox(w, p, wxT(""));
            toggles[p]->SetName(labelText);
            toggles[p]->SetValue(mControls[p].mControlBuffer > 0);
            gridSizer->Add(toggles[p], 0, wxALL, 5);
            ConnectFocus(toggles[p]);
            
            gridSizer->Add(1, 1, 0);
            gridSizer->Add(1, 1, 0);
            gridSizer->Add(1, 1, 0);
         }
         
         else {
            if (mControls[p].mInteger)
               fieldText.Printf(wxT("%d"), (int)(mControls[p].mControlBuffer + 0.5));
            else
               fieldText = Internat::ToDisplayString(mControls[p].mControlBuffer);
            
            fields[p] = new wxTextCtrl(w, p, fieldText);
            fields[p]->SetName(labelText);
            gridSizer->Add(fields[p], 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
            ConnectFocus(fields[p]);
            
            wxString bound;
            double lower = 0.0;
            double upper = 0.0;
            bool haslo = false;
            bool hashi = false;
            bool forceint = false;
            wxString loLabel;
            wxString hiLabel;
            
            ScalePointMap::const_iterator iter = 
               scalePoints.find(mControls[p].mIndex);
            
            if (!std::isnan(mControls[p].mMin)) {
               lower = mControls[p].mMin;
               haslo = true;
               if (iter != scalePoints.end()) {
                  std::map<float, wxString>::const_iterator iter2 =
                     iter->second.find(lower);
                  if (iter2 != iter->second.end()) {
                     loLabel = iter2->second;
                  }
               }
            }
            
            if (!std::isnan(mControls[p].mMax)) {
               upper = mControls[p].mMax;
               hashi = true;
               if (iter != scalePoints.end()) {
                  std::map<float, wxString>::const_iterator iter2 =
                     iter->second.find(upper);
                  if (iter2 != iter->second.end())
                     hiLabel = iter2->second;
               }
            }
            
            if (mControls[p].mSampleRate) {
               lower *= sampleRate * 1000;
               upper *= sampleRate;
               forceint = true;
            }
            
            wxString str;
            if (haslo) {
               str = loLabel;
               if (str.IsEmpty()) {
                  if (mControls[p].mInteger || forceint)
                     str.Printf(wxT("%d"), (int)(lower + 0.5));
                  else
                     str = Internat::ToDisplayString(lower);
               }
               item = new wxStaticText(w, 0, str);
               gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);
            }
            else {
               gridSizer->Add(1, 1, 0);
            }
            
            sliders[p] =
               new wxSlider(w, p,
                            0, 0, 1000,
                            wxDefaultPosition,
                            wxSize(200, -1));
            sliders[p]->SetName(labelText);
            gridSizer->Add(sliders[p], 0, wxALIGN_CENTER_VERTICAL | wxEXPAND | wxALL, 5);
            ConnectFocus(sliders[p]);
            
            if (hashi) {
               str = hiLabel;
               if (str.IsEmpty()) {
                  if (mControls[p].mInteger || forceint)
                     str.Printf(wxT("%d"), (int)(upper + 0.5));
                  else
                     str = Internat::ToDisplayString(upper);
               }
               item = new wxStaticText(w, 0, str);
               gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 5);
            }
            else {
               gridSizer->Add(1, 1, 0);
            }
         }
      }
   }
   
   // Set all of the sliders based on the value in the
   // text fields
   inSlider = false; // Now we're ready for HandleText to actually do something.
   HandleText();
   
   w->SetSizer(paramSizer);

   Layout();
   Fit();
   SetSizeHints(GetSize());
}

LV2EffectDialog::~LV2EffectDialog()
{
   delete[]sliders;
   delete[]fields;
   delete[]labels;
}

void LV2EffectDialog::OnCheckBox(wxCommandEvent &event)
{
   int p = event.GetId();

   mControls[p].mControlBuffer = toggles[p]->GetValue();
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
   
   if (std::isfinite(mControls[p].mMin))
      lower = mControls[p].mMin;
   if (std::isfinite(mControls[p].mMax))
      upper = mControls[p].mMax;
   if (mControls[p].mSampleRate) {
      lower *= sampleRate;
      upper *= sampleRate;
      forceint = true;
   }
   
   range = upper - lower;

   val = (sliders[p]->GetValue() / 1000.0) * range + lower;

   // Force the value to an integer if requested
   wxString str;
   if (mControls[p].mInteger || forceint)
      str.Printf(wxT("%d"), (int)(val + 0.5));
   else
      str = Internat::ToDisplayString(val);

   fields[p]->SetValue(str);

   mControls[p].mControlBuffer = val;

   inSlider = false;
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
      return;
   inText = true;
   
   for (uint32_t p = 0; p < mControls.size(); p++) {
      
      double dval;
      float val;
      float lower = float(0.0);
      float upper = float(10.0);
      float range;

      if (mControls[p].mToggle)
         continue;

      dval = Internat::CompatibleToDouble(fields[p]->GetValue());
      val = dval;
      
      
      
      if (!std::isnan(mControls[p].mMin))
         lower = mControls[p].mMin;
      if (!std::isnan(mControls[p].mMax))
         upper = mControls[p].mMax;
      if (mControls[p].mSampleRate) {
         lower *= sampleRate;
         upper *= sampleRate;
      }         
      range = upper - lower;

      if (val < lower)
         val = lower;
      if (val > upper)
         val = upper;

      mControls[p].mControlBuffer = val;

      sliders[p]->SetValue((int)(((val-lower)/range) * 1000.0 + 0.5));      
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
   effect->Preview();
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

   if (r.y >= rv.y && r.GetBottom() <= rv.GetBottom()) {
      return;
   }

   if (r.y < rv.y) {
      p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
      y = r.y / yppu;
   }
   else {
      p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
      y = (r.GetBottom() - rv.GetBottom() + yppu) / yppu;
   }

   p->Scroll(-1, y);
};

double LV2EffectDialog::GetLength()
{
   if (effect->GetEffectFlags() & INSERT_EFFECT) {
      mLength = Internat::CompatibleToDouble(mSeconds->GetValue());
   }

   return mLength;
}


double LV2EffectDialog::GetNoteLength() {
   if (effect->IsSynth()) {
      return Internat::CompatibleToDouble(mNoteSeconds->GetValue());
   }
   return 0;
}

unsigned char LV2EffectDialog::GetNoteVelocity() {
   if (effect->IsSynth()) {
      double velocity = 
         Internat::CompatibleToDouble(mNoteVelocity->GetValue());
      if (velocity < 1)
         return 1;
      if (velocity > 127)
         return 127;
      return (unsigned char)velocity;
   }
   return 64;
}

unsigned char LV2EffectDialog::GetNoteKey() {
   if (effect->IsSynth()) {
      double key = 
         Internat::CompatibleToDouble(mNoteKey->GetValue());
      if (key < 1)
         return 1;
      if (key > 127)
         return 127;
      return (unsigned char)key;
   }
   return 64;
}

#endif
