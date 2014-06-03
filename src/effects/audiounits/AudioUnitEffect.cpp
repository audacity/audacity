/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioUnitEffect.cpp

  Dominic Mazzoni

*******************************************************************//**

\class AudioUnitEffect
\brief An Effect class that handles a wide range of effects.  ??Mac only??

*//****************************************************************//**

\class AudioUnitGUIControl
\brief Used with AudioUnitEffect

*//****************************************************************//**

\class AudioUnitDialog
\brief A Dialog used with AudioUnitEffect

*//*******************************************************************/

#include "AudioUnitEffect.h"

#include <wx/defs.h>
#include <wx/button.h>
#include <wx/control.h>
#include <wx/frame.h>
#include <wx/panel.h>
#include <wx/sizer.h>
#include <wx/settings.h>
#include <wx/tokenzr.h>

#if ((wxMAJOR_VERSION == 2) && (wxMINOR_VERSION < 6))
#error Audio Units support requires wxMac 2.6
#endif

#include <wx/mac/private.h>

//
// Forward declarations of GUI classes
//

class AudioUnitGUIControl : public wxControl
{
 public:
   inline AudioUnitGUIControl(wxWindow *parent, wxWindowID id,
                              wxPoint pos, wxSize size,
                              ControlRef controlRef) {
      Create(parent, id, pos, size, controlRef);
   }

   virtual ~AudioUnitGUIControl();
   bool Create(wxWindow *parent, wxWindowID id,
               wxPoint pos, wxSize size,
               ControlRef controlRef);
   void OnMouse(wxMouseEvent &event);

 private:
   short GetModifiers(wxMouseEvent &event);
   DECLARE_EVENT_TABLE()
};

class AudioUnitDialog : public wxDialog
{
 public:
   AudioUnitDialog(wxWindow *parent, wxWindowID id, wxString title,
                   AudioUnit unit,
                   AudioUnitCarbonView carbonView,
                   Effect *effect);
   virtual ~AudioUnitDialog();

   void RemoveHandler();

   void OnOK(wxCommandEvent &event);
   void OnCancel(wxCommandEvent &event);
   void OnPreview(wxCommandEvent &event);

 private:
   AudioUnit             mUnit;
   AudioUnitGUIControl  *mGUIControl;
   wxBoxSizer           *mMainSizer;
   Effect               *mEffect;

   EventHandlerRef       mHandlerRef;
   EventHandlerUPP       mHandlerUPP;

   DECLARE_EVENT_TABLE()
};

//
// AudioUnitEffect
//

AudioUnitEffect::AudioUnitEffect(wxString name, Component component):
   mName(name),
   mComponent(component)
{
   SetEffectFlags(PLUGIN_EFFECT | PROCESS_EFFECT);

   mUnit = NULL;
}

AudioUnitEffect::~AudioUnitEffect()
{
   if (mUnit) {
      CloseComponent(mUnit);
   }
}

wxString AudioUnitEffect::GetEffectName()
{
   return mName;
}

std::set<wxString> AudioUnitEffect::GetEffectCategories()
{
   return std::set<wxString>();
}

wxString AudioUnitEffect::GetEffectIdentifier()
{
   wxStringTokenizer st(mName, wxT(" "));
   wxString id;

   // CamelCase the name
   while (st.HasMoreTokens()) {
      wxString tok = st.GetNextToken();

      id += tok.Left(1).MakeUpper() + tok.Mid(1);
   }

   return id;
}

wxString AudioUnitEffect::GetEffectAction()
{
   return wxString::Format(_("Performing Effect: %s"),
                           mName.c_str());
}

bool AudioUnitEffect::Init()
{
   if (!mUnit) {
      OpenAComponent(mComponent, &mUnit);
   }

   if (!mUnit) {
      return false;
   }

   mSupportsMono = SetRateAndChannels(mUnit, 1, mProjectRate);
   mSupportsStereo = SetRateAndChannels(mUnit, 2, mProjectRate);

   if (!mSupportsMono && !mSupportsStereo) {

      mSupportsMono = SetRateAndChannels(mUnit, 1, 44100.0);
      mSupportsStereo = SetRateAndChannels(mUnit, 2, 44100.0);

      if (!mSupportsMono && !mSupportsStereo) {
         printf("Audio Unit doesn't support mono or stereo.\n");
         return false;
      }
   }

   return true;
}

bool AudioUnitEffect::PromptUser()
{
   OSErr result;
   ComponentDescription desc;
   Component carbonViewComponent = NULL;
   AudioUnitCarbonView carbonView = NULL;

   GetComponentInfo(mComponent, &desc, 0, 0, 0);
   carbonViewComponent = GetCarbonViewComponent(desc.componentSubType);
   result = OpenAComponent(carbonViewComponent, &carbonView);
   if (result != 0) {
      printf("Couldn't open carbon view component\n");
      return false;
   }

   AudioUnitDialog dlog(mParent, -1, mName,
                        mUnit, carbonView, this);
   dlog.CentreOnParent();
   dlog.ShowModal();

   CloseComponent(carbonView);

   return (dlog.GetReturnCode() == wxID_OK);
}

bool AudioUnitEffect::Process()
{
   bool bGoodResult = true;

   CopyInputTracks();

   TrackListIterator iter(mOutputTracks);

   int count = 0;
   Track *left = iter.First();
   Track *right;
   while(left) {
      sampleCount lstart, rstart;
      sampleCount len;
      GetSamples((WaveTrack *)left, &lstart, &len);

      right = NULL;
      if (left->GetLinked() && mSupportsStereo) {
         right = iter.Next();
         GetSamples((WaveTrack *)right, &rstart, &len);
      }

      bool success = false;

      if (!mSupportsStereo && right) {
         // If the effect is mono, apply to each channel separately

         success = ProcessStereo(count, (WaveTrack *)left, NULL,
                                 lstart, 0, len);
         if (success)
            success = ProcessStereo(count, (WaveTrack *)right, NULL,
                                    rstart, 0, len);
      }
      else success = ProcessStereo(count,
                                   (WaveTrack *)left, (WaveTrack *)right,
                                   lstart, rstart, len);
      if (!success) {
         bGoodResult = false;
         break;
      }

      left = iter.Next();
      count++;
   }

   ReplaceProcessedTracks(bGoodResult);

   return bGoodResult;
}

void AudioUnitEffect::End()
{
}

bool AudioUnitEffect::SetRateAndChannels(AudioUnit unit,
                                         int numChannels,
                                         Float64 sampleRate)
{
   AudioStreamBasicDescription  streamFormat;
   ComponentResult              auResult;

   auResult = AudioUnitSetProperty(unit, kAudioUnitProperty_SampleRate,
                                   kAudioUnitScope_Global, 0,
                                   &sampleRate, sizeof(Float64));
   if (auResult != 0) {
      printf("Didn't accept sample rate\n");
      return false;
   }

   streamFormat.mSampleRate = sampleRate;
   streamFormat.mFormatID = kAudioFormatLinearPCM;
   streamFormat.mFormatFlags = kAudioFormatFlagsNativeFloatPacked |
                               kAudioFormatFlagIsNonInterleaved;
   streamFormat.mBitsPerChannel = 32;
   streamFormat.mChannelsPerFrame = numChannels;
   streamFormat.mFramesPerPacket = 1;
   streamFormat.mBytesPerFrame = 4;
   streamFormat.mBytesPerPacket = 4;

   auResult = AudioUnitSetProperty(unit, kAudioUnitProperty_StreamFormat,
                                   kAudioUnitScope_Input, 0,
                                   &streamFormat,
                                   sizeof(AudioStreamBasicDescription));

   if (auResult != 0)
      return false;

   auResult = AudioUnitSetProperty(unit, kAudioUnitProperty_StreamFormat,
                                   kAudioUnitScope_Output, 0,
                                   &streamFormat,
                                   sizeof(AudioStreamBasicDescription));

   if (auResult != 0)
      return false;

   auResult = AudioUnitSetProperty(unit, kAudioUnitProperty_StreamFormat,
                                   kAudioUnitScope_Global, 0,
                                   &streamFormat,
                                   sizeof(AudioStreamBasicDescription));

   if (auResult != 0) {
      printf("Didn't accept global stream format\n");
      return false;
   }

   return true;
}

bool AudioUnitEffect::CopyParameters(AudioUnit srcUnit, AudioUnit dstUnit)
{
   ComponentResult auResult;
   int numParameters, i;
   AudioUnitParameterID *parameters;
   Float32 parameterValue;
   UInt32 size;

   // Get number of parameters by passing NULL in the data field and
   // getting back the size of the parameter list

   size = 0;
   auResult = AudioUnitGetProperty(srcUnit, kAudioUnitProperty_ParameterList,
                                   kAudioUnitScope_Global, 0,
                                   NULL, &size);
   if (auResult != 0) {
      printf("Couldn't get number of parameters\n");
      return false;
   }

   // Now get the list of all parameter IDs

   numParameters = size / sizeof(AudioUnitParameterID);
   parameters = new AudioUnitParameterID[numParameters];
   auResult = AudioUnitGetProperty(srcUnit, kAudioUnitProperty_ParameterList,
                                   kAudioUnitScope_Global, 0,
                                   parameters, &size);
   if (auResult != 0) {
      printf("Couldn't get parameter list\n");
      delete[] parameters;
      return false;
   }

   // Copy the parameters from the main unit to the unit specific to
   // this track

   for(i=0; i<numParameters; i++) {
      auResult = AudioUnitGetParameter(srcUnit, parameters[i],
                                       kAudioUnitScope_Global, 0,
                                       &parameterValue);
      if (auResult != 0) {
         printf("Couldn't get parameter %d: ID=%d\n", i, (int)parameters[i]);
         continue;
      }

      auResult = AudioUnitSetParameter(dstUnit, parameters[i],
                                       kAudioUnitScope_Global, 0,
                                       parameterValue, 0);
      if (auResult != 0)
         printf("Couldn't set parameter %d: ID=%d\n", i, (int)parameters[i]);
   }

   delete[] parameters;

   return true;
}

bool AudioUnitEffect::ProcessStereo(int count,
                                    WaveTrack *left, WaveTrack *right,
                                    sampleCount lstart,
                                    sampleCount rstart,
                                    sampleCount len)
{
   int numChannels = (right != NULL ? 2 : 1);
   Float64 sampleRate = left->GetRate();
   AudioUnit trackUnit = mUnit;
   AURenderCallbackStruct callbackStruct;
   AudioTimeStamp timeStamp;
   AudioBufferList *bufferList = NULL;
   ComponentResult auResult;
   int waveTrackBlockSize;
   UInt32 size;
   UInt32 unitBlockSize;
   float *leftBuffer;
   float *rightBuffer;
   bool success = true;

   if (!SetRateAndChannels(trackUnit, numChannels, sampleRate)) {
      printf("Unable to setup audio unit for channels=%d rate=%.1f\n",
             numChannels, sampleRate);
      return false;
   }

   unitBlockSize = 0;
   size = sizeof(UInt32);
   auResult = AudioUnitGetProperty(trackUnit,
                                   kAudioUnitProperty_MaximumFramesPerSlice,
                                   kAudioUnitScope_Global,
                                   0,
                                   &unitBlockSize,
                                   &size);

   if (unitBlockSize == 0 || auResult != 0) {
      printf("Warning: didn't get audio unit's MaximumFramesPerSlice\n");
      printf("Trying to set MaximumFramesPerSlice to 512\n");
      unitBlockSize = 512;
      auResult = AudioUnitSetProperty(trackUnit,
                                      kAudioUnitProperty_MaximumFramesPerSlice,
                                      kAudioUnitScope_Global,
                                      0,
                                      &unitBlockSize,
                                      sizeof(UInt32));
      if (auResult != 0)
         printf("Unable to set MaximumFramesPerSlice, rendering may fail...\n");
   }

   auResult = AudioUnitInitialize(trackUnit);
   if (auResult != 0) {
      printf("Couldn't initialize audio unit\n");
      return false;
   }

   auResult = AudioUnitReset(trackUnit, kAudioUnitScope_Global, 0);
   if (auResult != 0) {
      printf("Reset failed.\n");
      AudioUnitUninitialize(trackUnit);
      return false;
   }

   callbackStruct.inputProc = SimpleAudioRenderCallback;
   callbackStruct.inputProcRefCon = this;
   auResult = AudioUnitSetProperty(trackUnit,
                                   kAudioUnitProperty_SetRenderCallback,
                                   kAudioUnitScope_Input,
                                   0,
                                   &callbackStruct,
                                   sizeof(AURenderCallbackStruct));
   if (auResult != 0) {
      printf("Setting input render callback failed.\n");
      AudioUnitUninitialize(trackUnit);
      return false;
   }

   memset(&timeStamp, 0, sizeof(AudioTimeStamp));
   timeStamp.mSampleTime = 0; // This is a double-precision number that should
                              // accumulate the number of frames processed so far
   timeStamp.mFlags = kAudioTimeStampSampleTimeValid;

   waveTrackBlockSize = left->GetMaxBlockSize() * 2;

   leftBuffer = NULL;
   rightBuffer = NULL;
   mLeftBufferForCallback = NULL;
   mRightBufferForCallback = NULL;

   if (left) {
      leftBuffer = new float[waveTrackBlockSize];
   }

   if (right) {
      rightBuffer = new float[waveTrackBlockSize];
   }

   bufferList = (AudioBufferList *)malloc(sizeof(UInt32) +
                                          numChannels * sizeof(AudioBuffer));
   if (!bufferList)
   {
      printf("Setting input render callback failed.\n");
      AudioUnitUninitialize(trackUnit);
      // free allocated memory
      if (leftBuffer) {
         delete[] leftBuffer;
      }
      if (rightBuffer) {
         delete[] rightBuffer;
      }
      return false;
   }
   bufferList->mNumberBuffers = numChannels;

   sampleCount originalLen = len;
   sampleCount ls = lstart;
   sampleCount rs = rstart;
   while (len) {
      int block = waveTrackBlockSize;
      if (block > len) {
         block = len;
      }

      if (left) {
         left->Get((samplePtr)leftBuffer, floatSample, ls, block);
      }

      if (right) {
         right->Get((samplePtr)rightBuffer, floatSample, rs, block);
      }

      success = DoRender(trackUnit, numChannels, leftBuffer, rightBuffer,
                         block, unitBlockSize, &timeStamp, bufferList);
      if (!success) {
         break;
      }

      if (left) {
         left->Set((samplePtr)leftBuffer, floatSample, ls, block);
      }

      if (right) {
         right->Set((samplePtr)rightBuffer, floatSample, rs, block);
      }

      len -= block;
      ls += block;
      rs += block;

      if (left && right) {
         if (TrackGroupProgress(count, (ls-lstart)/(double)originalLen)) {
            success = false;
            break;
         }
      }
      else {
         if (TrackProgress(count, (ls-lstart)/(double)originalLen)) {
            success = false;
            break;
         }
      }
   }

   if (bufferList) {
      free(bufferList);
   }

   if (leftBuffer) {
      delete[] leftBuffer;
   }

   if (rightBuffer) {
      delete[] rightBuffer;
   }

   AudioUnitUninitialize(trackUnit);

   return success;
}

bool AudioUnitEffect::DoRender(AudioUnit unit,
                               int numChannels,
                               float *leftBuffer,
                               float *rightBuffer,
                               int len,
                               int unitBlockSize,
                               AudioTimeStamp *timeStamp,
                               AudioBufferList *bufferList)
{
   AudioUnitRenderActionFlags flags;
   ComponentResult auResult;
   int block;

   for (int i = 0; i < len; i += block) {
      block = unitBlockSize;
      if (i + block > len) {
         block = len - i;
      }

      for (int j = 0; j<numChannels; j++) {
         bufferList->mBuffers[j].mNumberChannels = 1;
         bufferList->mBuffers[j].mData = NULL;
         bufferList->mBuffers[j].mDataByteSize = sizeof(float) * block;
      }

      if (leftBuffer) {
         mLeftBufferForCallback = &leftBuffer[i];
         bufferList->mBuffers[0].mData = mLeftBufferForCallback;
      }

      if (rightBuffer) {
         mRightBufferForCallback = &rightBuffer[i];
         bufferList->mBuffers[1].mData = mRightBufferForCallback;
      }

      flags = 0;

      auResult = AudioUnitRender(unit, &flags, timeStamp,
                                 0, block, bufferList);
      if (auResult != 0) {
         printf("Render failed: %d %4.4s\n", (int)auResult, (char *)&auResult);
         return false;
      }

      timeStamp->mSampleTime += block;
   }

   return true;
}

// static
OSStatus AudioUnitEffect::SimpleAudioRenderCallback
                           (void                            *inRefCon,
                            AudioUnitRenderActionFlags      *inActionFlags,
                            const AudioTimeStamp            *inTimeStamp,
                            UInt32                          inBusNumber,
                            UInt32                          inNumFrames,
                            AudioBufferList                 *ioData)
{
   AudioUnitEffect *This = (AudioUnitEffect *)inRefCon;

   if (This->mLeftBufferForCallback) {
      ioData->mBuffers[0].mData = This->mLeftBufferForCallback;
   }

   if (This->mRightBufferForCallback) {
      ioData->mBuffers[1].mData = This->mRightBufferForCallback;
   }

   return 0;
}

Component AudioUnitEffect::GetCarbonViewComponent(OSType subtype)
{
   ComponentDescription desc;
   Component component;

   desc.componentType = kAudioUnitCarbonViewComponentType; // 'auvw'
   desc.componentSubType = subtype;
   desc.componentManufacturer = 0;
   desc.componentFlags = 0;
   desc.componentFlagsMask = 0;

   // First see if we can find a carbon view designed specifically for this
   // plug-in:

   component = FindNextComponent(NULL, &desc);
   if (component)
      return component;

   // If not, grab the generic carbon view, which will create a GUI for
   // any Audio Unit.

   desc.componentSubType = kAUCarbonViewSubType_Generic;
   component = FindNextComponent(NULL, &desc);

   return component;
}

//
// AudioUnitGUIControl methods
//

BEGIN_EVENT_TABLE(AudioUnitGUIControl, wxControl)
   EVT_MOUSE_EVENTS(AudioUnitGUIControl::OnMouse)
END_EVENT_TABLE()

AudioUnitGUIControl::~AudioUnitGUIControl()
{
   // Don't want to dispose of it...
   m_peer = NULL;
}

bool AudioUnitGUIControl::Create(wxWindow *parent, wxWindowID id,
                                 wxPoint pos, wxSize size,
                                 ControlRef controlRef)
{
   m_macIsUserPane = FALSE ;

   if ( !wxControl::Create(parent, id, pos, size,
                           0, wxDefaultValidator,
                           wxString(wxT("AudioUnitControl"))) )
      return false;

   m_peer = new wxMacControl(this, controlRef);

   Rect outBounds;
   GetControlBounds(controlRef, &outBounds);

   pos.x = outBounds.left;
   pos.y = outBounds.top;
   size.x = outBounds.right - outBounds.left;
   size.y = outBounds.bottom - outBounds.top;

   MacPostControlCreate(pos, size);

   return true;
}

short AudioUnitGUIControl::GetModifiers(wxMouseEvent &event)
{
   short modifiers = 0;

   if ( !event.m_leftDown && !event.m_rightDown )
      modifiers  |= btnState ;

   if ( event.m_shiftDown )
      modifiers |= shiftKey ;

   if ( event.m_controlDown )
      modifiers |= controlKey ;

   if ( event.m_altDown )
      modifiers |= optionKey ;

   if ( event.m_metaDown )
      modifiers |= cmdKey ;

   return modifiers;
}

void AudioUnitGUIControl::OnMouse(wxMouseEvent &event)
{
   int x = event.m_x ;
   int y = event.m_y ;

   MacClientToRootWindow( &x , &y ) ;

   Point          localwhere ;
   ControlHandle  control;

   localwhere.h = x ;
   localwhere.v = y ;

   short modifiers = GetModifiers(event);

   if (event.GetEventType() == wxEVT_LEFT_DOWN ||
       event.GetEventType() == wxEVT_LEFT_DCLICK ) {
    #if ((wxMAJOR_VERSION == 2) && (wxMINOR_VERSION <= 4))
      WindowRef rootWindow = (WindowRef)GetParent()->MacGetRootWindow();
    #else
      WindowRef rootWindow = (WindowRef)GetParent()->MacGetTopLevelWindowRef();
    #endif

      ControlPartCode code;

      code = FindControl(localwhere,
                         rootWindow,
                         &control);

      if (code) {
         Rect outBounds;
         GetControlBounds((ControlRef)control, &outBounds);

         code = ::HandleControlClick(control,
                                     localwhere, modifiers,
                                     (ControlActionUPP)-1) ;
      }
   }
}

//
// AudioUnitDialog methods
//

// Event handler to capture the window close event
static const EventTypeSpec eventList[] =
{
   {kEventClassWindow, kEventWindowClose},
};

static pascal OSStatus EventHandler(EventHandlerCallRef handler, EventRef event, void *data)
{
   OSStatus result = eventNotHandledErr;

   AudioUnitDialog *dlg = (AudioUnitDialog *)data;

   if (GetEventClass(event) == kEventClassWindow && GetEventKind(event) == kEventWindowClose) {
      dlg->RemoveHandler();
      dlg->Close();
      result = noErr;
   }

   return result;
}


void EventListener(void *inUserData, AudioUnitCarbonView inView,
                   const AudioUnitParameter *inParameter,
                   AudioUnitCarbonViewEventID inEvent,
                   const void *inEventParam)
{
   // We're not actually using this yet...
}

enum {
   PreviewID = 1
};

BEGIN_EVENT_TABLE(AudioUnitDialog, wxDialog)
   EVT_BUTTON(wxID_OK, AudioUnitDialog::OnOK)
   EVT_BUTTON(wxID_CANCEL, AudioUnitDialog::OnCancel)
   EVT_BUTTON(PreviewID, AudioUnitDialog::OnPreview)
END_EVENT_TABLE()

AudioUnitDialog::AudioUnitDialog(wxWindow *parent, wxWindowID id,
                                 wxString title,
                                 AudioUnit unit,
                                 AudioUnitCarbonView carbonView,
                                 Effect *effect):
   mUnit(unit),
   mEffect(effect),
   mHandlerUPP(NULL),
   mHandlerRef(NULL)
{
   long style = wxDEFAULT_DIALOG_STYLE;

  #if ((wxMAJOR_VERSION == 2) && (wxMINOR_VERSION <= 4))

   wxDialog::Create(parent, id, title,
                    wxDefaultPosition, wxSize(500, 400),
                    style, title);

  #else

   // wxMac 2.5 version, all of this is just to attempt to turn off
   // compositing...

   // From wxDialog::Create
   SetExtraStyle(GetExtraStyle() | wxTOPLEVEL_EX_DIALOG);
   style |= wxTAB_TRAVERSAL;

   // From wxTopLevelWindow::Create
   wxTopLevelWindow::Init();
   m_windowStyle = style;

   // We added this line because many plug-ins are not happy with
   // compositing...Requires a patch to wx such that the line
   // "attr |= kWindowCompositingAttribute" only happens
   // if m_macUsesCompositing is true...
   //
   // LL:  Unfortunately, wxWidgets 2.7+ doesn't have a direct
   //      way to disable compositing, so bypass it until a way
   //      is found.
#if !wxCHECK_VERSION(2, 7, 0)
   m_macUsesCompositing = false;
#endif

   // Rest of wxTopLevelWindow::Create
   SetName(title);
   m_windowId = id == -1 ? NewControlId() : id;
   MacCreateRealWindow(title, wxDefaultPosition, wxSize(500, 400),
                       MacRemoveBordersFromStyle(style) , title) ;
   SetBackgroundColour(wxSystemSettings::GetColour(wxSYS_COLOUR_3DFACE));
   wxTopLevelWindows.Append(this);
   if ( parent )
      parent->AddChild(this);

  #endif

   //
   // On to our own code
   //

   WindowRef windowRef = (WindowRef)MacGetWindowRef();
   ComponentResult auResult;
   ControlRef rootControl = NULL;
   GetRootControl(windowRef, &rootControl);

   int width = 500;
   int height = 400;
   Float32Point location = {0, 0};
   Float32Point size = {width, height};
   ControlRef audioUnitControl = NULL;

   auResult = AudioUnitCarbonViewCreate(carbonView,
                                        unit,
                                        windowRef,
                                        rootControl,
                                        &location,
                                        &size,
                                        &audioUnitControl);

   AudioUnitCarbonViewSetEventListener(carbonView, EventListener, this);

   mMainSizer = new wxBoxSizer(wxVERTICAL);

   if (auResult == 0) {
      mGUIControl = new AudioUnitGUIControl(this, -1,
                                            wxPoint(0, 0),
                                            wxSize(width, height),
                                            audioUnitControl);

      // Eventually, to handle resizing controls, call:
      // AudioUnitCarbonViewSetEventListener with the event:
      //   kEventControlBoundsChanged

      mMainSizer->Add(mGUIControl, 1, wxEXPAND);

      mGUIControl->SetFocus();
   }
   else
      mGUIControl = NULL;

   wxBoxSizer *hSizer = new wxBoxSizer(wxHORIZONTAL);

   wxButton *preview = new wxButton(this, PreviewID, _("Pre&view"));
   hSizer->Add(preview, 0, wxALL, 10);

   hSizer->Add(10, 10);

   wxButton *ok = new wxButton(this, wxID_OK, _("OK"));
   ok->SetDefault();
   ok->SetFocus();
   hSizer->Add(ok, 0, wxALL, 10);

   wxButton *cancel = new wxButton(this, wxID_CANCEL, _("&Cancel"));
   hSizer->Add(cancel, 0, wxALL, 10);

   mMainSizer->Add(hSizer, 0, wxALIGN_CENTER);

   this->SetAutoLayout(true);
   this->SetSizer(mMainSizer);
   mMainSizer->Fit(this);
   mMainSizer->SetSizeHints(this);

   // Some VST effects do not work unless the default handler is removed since
   // it captures many of the events that the plugins need.  But, it must be
   // done last since proper window sizing will not occur otherwise.
   ::RemoveEventHandler((EventHandlerRef)MacGetEventHandler());

   // Install a bare minimum handler so we can capture the window close event.  If
   // it's not captured, we will crash at Audacity termination since the window
   // is still on the wxWidgets toplevel window lists, but it's already gone.
   mHandlerUPP = NewEventHandlerUPP(EventHandler);
   InstallWindowEventHandler(windowRef,
                             mHandlerUPP,
                             GetEventTypeCount(eventList),
                             eventList,
                             this,
                             &mHandlerRef);

}

AudioUnitDialog::~AudioUnitDialog()
{
   RemoveHandler();
}

void AudioUnitDialog::RemoveHandler()
{
   if (mHandlerRef) {
      ::RemoveEventHandler(mHandlerRef);
      mHandlerRef = NULL;
      MacInstallTopLevelWindowEventHandler();
   }

   if (mHandlerUPP) {
      DisposeEventHandlerUPP(mHandlerUPP);
      mHandlerUPP = NULL;
   }
}

void AudioUnitDialog::OnOK(wxCommandEvent &event)
{
   EndModal(wxID_OK);
}

void AudioUnitDialog::OnCancel(wxCommandEvent &event)
{
   EndModal(wxID_CANCEL);
}

void AudioUnitDialog::OnPreview(wxCommandEvent &event)
{
   mEffect->Preview();
}
