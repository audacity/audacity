/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2Effect.h

  Audacity(R) is copyright (c) 1999-2013 Audacity Team.
  License: GPL v2.  See License.txt.

*********************************************************************/

class wxSlider;
class wxStaticText;
class wxTextCtrl;
class wxCheckBox;

#include <map>
#include <vector>
#include <wx/dialog.h>
#include <wx/dynarray.h>

#include <lilv/lilv.h>

#include "../Effect.h"
#include "LV2PortGroup.h"


/** A structure that contains information about a single LV2 plugin port. */
struct LV2Port
{
   LV2Port()
      : mToggle(false),
        mInteger(false),
        mSampleRate(false),
        mEnumeration(false)
   {
   }
   
   uint32_t mIndex;
   wxString mName;
   float mMin;
   float mMax;
   float mDefault;
   float mControlBuffer;
   bool mToggle;
   bool mInteger;
   bool mSampleRate;
   bool mEnumeration;
   LilvPort *mPort;

   // ScalePoints
   wxArrayDouble mScaleValues;
   wxArrayString mScaleLabels;
};

WX_DECLARE_OBJARRAY(LV2Port, LV2PortArray);

/** The main LV2 plugin class. It handles loading and applying a 
    single plugin. */
class LV2Effect:public Effect
{
public:
   
   /** Create an LV2Effect from an LV2 data handle and a category set. */
   LV2Effect(const LilvPlugin *plug,
             const std::set<wxString> & categories = std::set<wxString>());
   virtual ~LV2Effect();

   /** Get the name of the effect. */
   virtual wxString GetEffectName();
   
   /** Get the categories of the effect. */
   virtual std::set<wxString> GetEffectCategories();

   /** Get the effect identifier (for internal use). */
   virtual wxString GetEffectIdentifier();
   
   /** Get the action string. */
   virtual wxString GetEffectAction();
   
   virtual bool Init();

   virtual bool PromptUser();
   
   virtual bool Process();
   
   virtual void End();
   
   bool IsValid();

   /** Return a list of LV2Ports for the input parameters. */
   LV2PortArray & GetControls();
   
   /** Return true if the plugin is a synth (MIDI input), false if not. */
   bool IsSynth();

   /** Modify the note settings for the plugin (only for synths). */
   bool SetNote(sampleCount len, unsigned char velocity, unsigned char key);
   
   /** Get the port group tree for the plugin. */
   const LV2PortGroup & GetRootGroup();

   wxString GetString(const LilvNode *node);
   wxString GetString(LilvNode *node, bool free);

private:
   bool ProcessStereo(int count, WaveTrack *left, WaveTrack *right,
                      sampleCount lstart, sampleCount rstart,
                      sampleCount len);

   bool mValid;
   wxString pluginName;

   const LilvPlugin *mData;
   sampleCount mBlockSize;
   float **fInBuffer;
   float **fOutBuffer;
   int mainRate;

   std::set<wxString> mCategories;
   
   LV2PortArray mControlInputs;
   LV2PortArray mControlOutputs;
   LV2PortArray mAudioInputs;
   LV2PortArray mAudioOutputs;
   LV2Port *mMidiInput;
   int mLatencyPortIndex;
   
   sampleCount mNoteLength;
   unsigned char mNoteVelocity;
   unsigned char mNoteKey;
   
   LV2PortGroup mRootGroup;
   std::map<wxString, LV2PortGroup> mPortGroups;
};


/** The control dialog for an LV2 plugin. */
class LV2EffectDialog:public wxDialog
{
   DECLARE_DYNAMIC_CLASS(LV2EffectDialog)

public:
   LV2EffectDialog(LV2Effect *effect,
                   wxWindow *parent,
                   const LilvPlugin *data,
                   int sampleRate,
                   double length,
                   double noteLength,
                   unsigned char noteVelocity,
                   unsigned char noteKey);

   ~LV2EffectDialog();

   void OnCheckBox(wxCommandEvent & event);
   void OnSlider(wxCommandEvent & event);
   void OnTextCtrl(wxCommandEvent & event);
   void OnChoiceCtrl(wxCommandEvent & event);
   void OnOK(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);
   void OnPreview(wxCommandEvent & event);
   void ControlSetFocus(wxFocusEvent & event);

   double GetLength();
   double GetNoteLength();
   unsigned char GetNoteVelocity();
   unsigned char GetNoteKey();

   DECLARE_EVENT_TABLE()

private:
   void HandleText();
   void ConnectFocus(wxControl *c);
   void DisconnectFocus(wxControl *c);

private:
   LV2Effect *mEffect;
   const LilvPlugin *mData;
   LV2PortArray & mControls;
   int mSampleRate;
   double mLength;

   bool inSlider;
   bool inText;

   wxSlider **mSliders;
   wxTextCtrl **mFields;
   wxStaticText **mLabels;
   wxCheckBox **mToggles;
   wxChoice **mEnums;
   wxTextCtrl *mSeconds;
   wxTextCtrl *mNoteSeconds;
   wxTextCtrl *mNoteVelocity;
   wxTextCtrl *mNoteKey;
};
