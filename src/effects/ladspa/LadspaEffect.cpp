/**********************************************************************

  Audacity: A Digital Audio Editor

  LadspaEffect.cpp

  Dominic Mazzoni

  This class implements a Ladspa Plug-in effect.

*******************************************************************//**

\class LadspaEffect
\brief An Effect that calls up a LADSPA plug in, i.e. many possible
effects from this one class.

*//****************************************************************//**

\class LadspaEffectDialog
\brief Dialog used with Effect

*//*******************************************************************/


#include "../../Audacity.h"

#include "ladspa.h"

#include <wx/wxprec.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/dynlib.h>
#include <wx/filename.h>
#include <wx/log.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/tokenzr.h>
#include <wx/intl.h>
#include <wx/scrolwin.h>

#include "../Effect.h"          // Audacity Effect base class
#include "LadspaEffect.h"       // This class's header file
#include "../../Internat.h"

// ============================================================================
//
// Module registration entry point
//
// This is the symbol that Audacity looks for when the module is built as a
// dynamic library.
//
// When the module is builtin to Audacity, we use the same function, but it is
// declared static so as not to clash with other builtin modules.
//
// ============================================================================
DECLARE_MODULE_ENTRY(AudacityModule)
{
   // Create and register the importer
   return new LadspaEffectsModule(moduleManager, path);
}

// ============================================================================
//
// Register this as a builtin module
//
// ============================================================================
DECLARE_BUILTIN_MODULE(LadspaBuiltin);

// ============================================================================
//
// LadspaEffectsModule
//
// ============================================================================
LadspaEffectsModule::LadspaEffectsModule(ModuleManagerInterface *moduleManager,
                                     const wxString *path)
{
   mModMan = moduleManager;
   if (path)
   {
      mPath = *path;
   }
}

LadspaEffectsModule::~LadspaEffectsModule()
{
}

// ============================================================================
// IdentInterface implementation
// ============================================================================

wxString LadspaEffectsModule::GetID()
{
   // Can be anything, but this is a v4 UUID
   return L"3ebd9fb9-c020-4c0d-a786-d6a914e55e31";
}

wxString LadspaEffectsModule::GetPath()
{
   return mPath;
}

wxString LadspaEffectsModule::GetName()
{
   return _("Ladspa Effect Module");
}

wxString LadspaEffectsModule::GetVendor()
{
   return _("The Audacity Team");
}

wxString LadspaEffectsModule::GetVersion()
{
   // This "may" be different if this were to be maintained as a separate DLL
   return LADSPAEFFECTS_VERSION;
}

wxString LadspaEffectsModule::GetDescription()
{
   return _("Provides Ladspa Effects");
}

// ============================================================================
// ModuleInterface implementation
// ============================================================================

bool LadspaEffectsModule::Initialize()
{
   // Nothing to do here
   return true;
}

void LadspaEffectsModule::Terminate()
{
   // Nothing to do here
   return;
}

bool LadspaEffectsModule::AutoRegisterPlugins(PluginManagerInterface & WXUNUSED(pm))
{
   return false;
}

wxArrayString LadspaEffectsModule::FindPlugins(PluginManagerInterface & pm)
{
#if defined(USE_LIBLRDF) && defined(EFFECT_CATEGORIES)

   EffectManager& em = EffectManager::Get();
   wxArrayString rdfPathList;
   wxString rdfPathVar;
   wxArrayString rdfFiles;

   InitCategoryMap();
   lrdf_init();

   rdfPathVar = wxGetenv(wxT("LADSPA_RDF_PATH"));
   if (rdfPathVar != wxT(""))
      wxGetApp().AddMultiPathsToPathList(rdfPathVar, rdfPathList);

#ifdef __WXGTK__
   wxGetApp().AddUniquePathToPathList(wxT("/usr/share/ladspa/rdf"),
                                      rdfPathList);
   wxGetApp().AddUniquePathToPathList(wxT("/usr/local/share/ladspa/rdf"),
                                      rdfPathList);
#endif

#ifdef __WXMAC__
   wxGetApp().AddUniquePathToPathList(wxT("/usr/share/ladspa/rdf"),
                                      rdfPathList);
   // XXX Maybe other Mac paths here?
#endif

#ifdef __WXMSW__
   //wxGetApp().AddUniquePathToPathList(wxT("WINDOWS LRDF PATH"),
   //                                   rdfPathList);
   // XXX Other Windows paths here.
#endif

   // Add the Audacity paths so we get ladspa.rdfs if we are using a local
   // liblrdf
   for(i=0; i<audacityPathList.GetCount(); i++) {
      wxString prefix = audacityPathList[i] + wxFILE_SEP_PATH;
      wxGetApp().AddUniquePathToPathList(prefix + wxT("rdf"),
                                         rdfPathList);
   }

   wxGetApp().FindFilesInPathList(wxT("*.rdf"), rdfPathList, rdfFiles);
   wxGetApp().FindFilesInPathList(wxT("*.rdfs"), rdfPathList, rdfFiles);
   for(size_t i = 0; i < rdfFiles.GetCount(); ++i) {
      wxString fileUri(wxT("file://"));
      fileUri += rdfFiles[i];
      lrdf_read_file(fileUri.mb_str(wxConvUTF8));
   }


   // Add all plugin categories found by LRDF
   lrdf_uris* cats =
      lrdf_get_all_subclasses("http://ladspa.org/ontology#Plugin");
   if (cats) {

      // Add the categories and find the plugins belonging to them
      for (size_t i = 0; i < cats->count; ++i) {
         char* label = lrdf_get_label(cats->items[i]);
         if (!label)
            continue;
         wxString uri = MapCategoryUri(wxString::FromAscii(cats->items[i]));
         em.AddCategory(uri, wxString::FromUTF8(label));
         std::free(label);

         lrdf_uris* plugs = lrdf_get_instances(cats->items[i]);
         if (plugs) {
            for (size_t j = 0; j < plugs->count; ++j) {
               unsigned long uid = lrdf_get_uid(plugs->items[j]);
               gPluginCategories.insert(std::make_pair(uid, uri));
            }
            lrdf_free_uris(plugs);
         }
      }

      // And their relationships
      for (size_t i = 0; i < cats->count; ++i) {
         EffectCategory* p =
            em.LookupCategory(MapCategoryUri(wxString::FromAscii(cats->
                                                                 items[i])));
         if (!p)
            continue;
         lrdf_uris* subs = lrdf_get_subclasses(cats->items[i]);
         if (subs) {
            for (size_t j = 0; j < subs->count; ++j) {
               EffectCategory* c =
                  em.LookupCategory(MapCategoryUri(wxString::FromAscii(subs->items[j])));
               if (c)
                  em.AddCategoryParent(c, p);
            }
            lrdf_free_uris(subs);
         }
      }

      lrdf_free_uris(cats);

   }

#endif

   wxArrayString pathList;
   wxArrayString files;
   wxString pathVar;

   // Check for the LADSPA_PATH environment variable
   pathVar = wxString::FromUTF8(getenv("LADSPA_PATH"));
   if (!pathVar.empty())
   {
      wxStringTokenizer tok(pathVar);
      while (tok.HasMoreTokens())
      {
         pathList.Add(tok.GetNextToken());
      }
   }

#if defined(__WXMAC__)

   pathList.Add(wxT("~/Library/Audio/Plug-Ins/LADSPA"));
   pathList.Add(wxT("/Library/Audio/Plug-Ins/LADSPA"));

   // Recursively scan for all shared objects
   pm.FindFilesInPathList(wxT("*.so"), pathList, files);

#elif defined(__WXMSW__)

   // Recursively scan for all DLLs
   pm.FindFilesInPathList(wxT("*.dll"), pathList, files);

#else
   
   pathList.Add(wxT("~/.ladspa"));
   pathList.Add(wxT("/usr/local/lib/ladspa"));
   pathList.Add(wxT("/usr/lib/ladspa"));
   pathList.Add(wxT(LIBDIR) wxT("/ladspa"));

   // Recursively scan for all shared objects
   pm.FindFilesInPathList(wxT("*.so"), pathList, files);

#endif

   return files;
}

bool LadspaEffectsModule::RegisterPlugin(PluginManagerInterface & pm, const wxString & path)
{
   // Since we now have builtin VST support, ignore the VST bridge as it
   // causes duplicate menu entries to appear.
   wxFileName f(path);
   if (f.GetName().CmpNoCase(wxT("vst-bridge")) == 0) {
      return false;
   }

   // As a courtesy to some plug-ins that might be bridges to
   // open other plug-ins, we set the current working
   // directory to be the plug-in's directory.

   wxString saveOldCWD = ::wxGetCwd();
   wxString prefix = ::wxPathOnly(path);
   ::wxSetWorkingDirectory(prefix);

   int index = 0;
   LADSPA_Descriptor_Function mainFn = NULL;
   wxDynamicLibrary lib;
   if (lib.Load(path, wxDL_NOW)) {
      wxLogNull logNo;

      mainFn = (LADSPA_Descriptor_Function) lib.GetSymbol(wxT("ladspa_descriptor"));

      if (mainFn) {
         const LADSPA_Descriptor *data;

         for (data = mainFn(index); data; data = mainFn(++index)) {
#if defined(USE_LIBLRDF) && defined(EFFECT_CATEGORIES)
            std::set<wxString> categories;
            std::multimap<unsigned long, wxString>::const_iterator iter;
            iter = gPluginCategories.lower_bound(data->UniqueID);
            for ( ; (iter != gPluginCategories.end() &&
                     iter->first == data->UniqueID); ++iter)
               categories.insert(iter->second);
#endif
            LadspaEffect effect(path, index);
            if (effect.Startup()) {
               pm.RegisterEffectPlugin(this, &effect);
               effect.Shutdown();
            }
         }
      }
   }

   if (lib.IsLoaded()) {
      lib.Unload();
   }

   ::wxSetWorkingDirectory(saveOldCWD);

   return index > 0;
}

void *LadspaEffectsModule::CreateInstance(const PluginID & ID, const wxString & path)
{
   // For us, the ID is two words.
   // 1)  The Ladspa descriptor index
   // 2)  The library's path
   long index;
   ID.BeforeFirst(wxT(' ')).ToLong(&index);

   return new LadspaEffect(path, (int) index);
}

LadspaEffect::LadspaEffect(const wxString & path, int index)
{
   mPath = path;
   mIndex = index;
   mData = NULL;

   mHost = NULL;
   mMaster = NULL;
   mReady = false;

   mAudioIns = 0;
   mAudioOuts = 0;
   mNumInputControls = 0;
   mSampleRate = 44100;

   mInputPorts = NULL;
   mOutputPorts = NULL;
   mInputControls = NULL;
   mOutputControls = NULL;

   mLatencyPort = -1;

   mDlg = NULL;
}

LadspaEffect::~LadspaEffect()
{
   if (mInputPorts)
   {
      delete [] mInputPorts;
   }

   if (mOutputPorts)
   {
      delete [] mOutputPorts;
   }

   if (mInputControls)
   {
      delete [] mInputControls;
   }

   if (mOutputControls)
   {
      delete [] mOutputControls;
   }
}

//
// IdentInterface implementation
//
wxString LadspaEffect::GetID()
{
   return wxString::Format(wxT("%d %s"), mIndex, mPath.c_str());
}

wxString LadspaEffect::GetPath()
{
   return mPath;
}

wxString LadspaEffect::GetName()
{
   return LAT1CTOWX(mData->Name);
}

wxString LadspaEffect::GetVendor()
{
   return LAT1CTOWX(mData->Maker);
}

wxString LadspaEffect::GetVersion()
{
   return _("N/A");
}

wxString LadspaEffect::GetDescription()
{
   return  _("Audio In: ") +
           wxString::Format(wxT("%d"), mAudioIns) + 
           _(", Audio Out: ") +
           wxString::Format(wxT("%d"), mAudioOuts);
}

//
// EffectIdentInterface implementation
//
EffectType LadspaEffect::GetType()
{
   if (mAudioIns == 0 && mAudioOuts == 0)
   {
      return EffectTypeNone;
   }

   if (mAudioIns == 0)
   {
      return EffectTypeGenerate;
   }

   if (mAudioOuts == 0)
   {
      return EffectTypeAnalyze;
   }

   return EffectTypeProcess;
}

wxString LadspaEffect::GetFamily()
{
   return LADSPAEFFECTS_FAMILY;
}

bool LadspaEffect::IsInteractive()
{
   return mInteractive;
}

bool LadspaEffect::IsDefault()
{
   return false;
}

bool LadspaEffect::IsLegacy()
{
   return false;
}

bool LadspaEffect::IsRealtimeCapable()
{
   return GetType() == EffectTypeProcess;
}

//
// EffectClientInterface Implementation
//
void LadspaEffect::SetHost(EffectHostInterface *host)
{
   mHost = host;
}

bool LadspaEffect::Startup()
{
   if (!Load())
   {
      return false;
   }

   mInputPorts = new unsigned long [mData->PortCount];
   mOutputPorts = new unsigned long [mData->PortCount];
   mInputControls = new float [mData->PortCount];
   mOutputControls = new float [mData->PortCount];

   for (unsigned long p = 0; p < mData->PortCount; p++)
   {
      LADSPA_PortDescriptor d = mData->PortDescriptors[p];
      // Collect the audio ports
      if (LADSPA_IS_PORT_AUDIO(d))
      {
         if (LADSPA_IS_PORT_INPUT(d)) 
         {
            mInputPorts[mAudioIns++] = p;
         }
         else if (LADSPA_IS_PORT_OUTPUT(d))
         {
            mOutputPorts[mAudioOuts++] = p;
         }
      }
      // Determine the port's default value
      else if (LADSPA_IS_PORT_CONTROL(d) && LADSPA_IS_PORT_INPUT(d))
      {
         mInteractive = true;

         float val = float(1.0);
         LADSPA_PortRangeHint hint = mData->PortRangeHints[p];

         if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor) && val < hint.LowerBound)
         {
            val = hint.LowerBound;
         }

         if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor) && val > hint.UpperBound)
         {
            val = hint.UpperBound;
         }

         if (LADSPA_IS_HINT_DEFAULT_MINIMUM(hint.HintDescriptor))
         {
            val = hint.LowerBound;
         }

         if (LADSPA_IS_HINT_DEFAULT_LOW(hint.HintDescriptor))
         {
            val = hint.LowerBound * 0.75f + hint.UpperBound * 0.25f;
         }

         if (LADSPA_IS_HINT_DEFAULT_MIDDLE(hint.HintDescriptor))
         {
            val = hint.LowerBound * 0.5f + hint.UpperBound * 0.5f;
         }

         if (LADSPA_IS_HINT_DEFAULT_HIGH(hint.HintDescriptor))
         {
            val = hint.LowerBound * 0.25f + hint.UpperBound * 0.75f;
         }

         if (LADSPA_IS_HINT_DEFAULT_MAXIMUM(hint.HintDescriptor))
         {
            val = hint.UpperBound;
         }

         if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor))
         {
            val *= mSampleRate;
         }

         if (LADSPA_IS_HINT_DEFAULT_0(hint.HintDescriptor))
         {
            val = 0.0f;
         }

         if (LADSPA_IS_HINT_DEFAULT_1(hint.HintDescriptor))
         {
            val = 1.0f;
         }

         if (LADSPA_IS_HINT_DEFAULT_100(hint.HintDescriptor))
         {
            val = 100.0f;
         }

         if (LADSPA_IS_HINT_DEFAULT_440(hint.HintDescriptor))
         {
            val = 440.0f;
         }

         mNumInputControls++;
         mInputControls[p] = val;
      }
      // Ladspa effects have a convention of providing latency on an output
      // control port whose name is "latency".
      else if (LADSPA_IS_PORT_CONTROL(d) && LADSPA_IS_PORT_OUTPUT(d))
      {
         if (strcmp(mData->PortNames[p], "latency") == 0)
         {
            mLatencyPort = p;
            mOutputControls[p] = 0;
         }
      }
   }

   // mHost will be null during registration
   if (mHost)
   {
      mHost->GetSharedConfig(wxT("Settings"), wxT("BufferSize"), mUserBlockSize, 8192);
      mBlockSize = mUserBlockSize;

      bool haveDefaults;
      mHost->GetPrivateConfig(wxT("Default"), wxT("Initialized"), haveDefaults, false);
      if (!haveDefaults)
      {
         SaveParameters(wxT("Default"));
         mHost->SetPrivateConfig(wxT("Default"), wxT("Initialized"), true);
      }

      LoadParameters(wxT("Current"));
   }

   return true;
}

bool LadspaEffect::Shutdown()
{
   if (mInputPorts)
   {
      delete [] mInputPorts;
      mInputPorts = NULL;
   }

   if (mOutputPorts)
   {
      delete [] mOutputPorts;
      mOutputPorts = NULL;
   }

   if (mInputControls)
   {
      delete [] mInputControls;
      mInputControls = NULL;
   }

   if (mOutputControls)
   {
      delete [] mOutputControls;
      mOutputControls = NULL;
   }

   return true;
}

int LadspaEffect::GetAudioInCount()
{
   return mAudioIns;
}

int LadspaEffect::GetAudioOutCount()
{
   return mAudioOuts;
}

int LadspaEffect::GetMidiInCount()
{
   return 0;
}

int LadspaEffect::GetMidiOutCount()
{
   return 0;
}

void LadspaEffect::SetSampleRate(sampleCount rate)
{
   mSampleRate = rate;
}

sampleCount LadspaEffect::GetBlockSize(sampleCount maxBlockSize)
{
#if 0
   // TODO:  Allow user to specify the max blocksize
   if (mUserBlockSize > maxBlockSize)
   {
      mBlockSize = maxBlockSize;
   }
   else
   {
      mBlockSize = mUserBlockSize;
   }
#endif

   return mBlockSize;
}

sampleCount LadspaEffect::GetLatency()
{
   if (mLatencyPort >= 0 && !mLatencyDone)
   {
      mLatencyDone = true;
      return mOutputControls[mLatencyPort] * 2;
   }

   return 0;
}

sampleCount LadspaEffect::GetTailSize()
{
   return 0;
}

bool LadspaEffect::IsReady()
{
   return mReady;
}

bool LadspaEffect::ProcessInitialize()
{
   /* Instantiate the plugin */
   if (!mReady)
   {
      mMaster = InitInstance(mSampleRate);
      if (!mMaster)
      {
         return false;
      }
      mReady = true;
   }

   mLatencyDone = false;

   return true;
}

bool LadspaEffect::ProcessFinalize()
{
   if (mReady)
   {
      mReady = false;

      FreeInstance(mMaster);
      mMaster = NULL;
   }

   return true;
}

sampleCount LadspaEffect::ProcessBlock(float **inbuf, float **outbuf, sampleCount size)
{
   for (int i = 0; i < mAudioIns; i++)
   {
      mData->connect_port(mMaster, mInputPorts[i], inbuf[i]);
   }

   for (int i = 0; i < mAudioOuts; i++)
   {
      mData->connect_port(mMaster, mOutputPorts[i], outbuf[i]);
   }

   mData->run(mMaster, size);

   return size;
}

bool LadspaEffect::RealtimeInitialize()
{
   return true;
}

bool LadspaEffect::RealtimeFinalize()
{
   for (size_t i = 0, cnt = mSlaves.GetCount(); i < cnt; i++)
   {
      FreeInstance(mSlaves[i]);
   }
   mSlaves.Clear();

   return true;
}

bool LadspaEffect::RealtimeSuspend()
{
   return true;
}

bool LadspaEffect::RealtimeResume()
{
   return true;
}

sampleCount LadspaEffect::RealtimeProcess(int group,
                                          float **inbuf,
                                          float **outbuf,
                                          sampleCount numSamples)
{
   if (group < 0 || group >= (int) mSlaves.GetCount())
   {
      return 0;
   }

   for (int i = 0; i < mAudioIns; i++)
   {
      mData->connect_port(mSlaves[group], mInputPorts[i], inbuf[i]);
   }

   for (int i = 0; i < mAudioOuts; i++)
   {
      mData->connect_port(mSlaves[group], mOutputPorts[i], outbuf[i]);
   }

   mData->run(mSlaves[group], numSamples);

   return numSamples;
}

bool LadspaEffect::RealtimeAddProcessor(int numChannels, float sampleRate)
{
   LADSPA_Handle slave = InitInstance(sampleRate);
   if (!slave)
   {
      return false;
   }

   mSlaves.Add(slave);
   mSlaveChannels.Add(numChannels);

   return true;
}

bool LadspaEffect::ShowInterface(void *parent)
{
   if (mNumInputControls == 0) {
      return false;
   }

   double length = 0;
   
   if (!IsRealtimeCapable())
   {
      length = mHost->GetDuration();
   }

   if (!mDlg)
   {
      mDlg = new LadspaEffectDialog(this, (wxWindow *) parent, mData, mInputControls, mSampleRate, length);
      mDlg->CentreOnParent();
   }

   if (IsRealtimeCapable())
   {
      mDlg->Show(!mDlg->IsShown());
      return true;
   }

   mDlg->ShowModal();
   bool ret = mDlg->GetReturnCode() != 0;
   if (ret)
   {
      mHost->SetDuration(mDlg->GetLength());
   }
   mDlg->Destroy();
   mDlg = NULL;


   return ret;
}

//
// LadspaEffect Implementation
//
bool LadspaEffect::Load()
{
   if (mLib.IsLoaded())
   {
      return true;
   }

   LADSPA_Descriptor_Function mainFn = NULL;
   if (mLib.Load(mPath, wxDL_NOW))
   {
      wxLogNull logNo;

      mainFn = (LADSPA_Descriptor_Function) mLib.GetSymbol(wxT("ladspa_descriptor"));
      if (mainFn)
      {
         mData = mainFn(mIndex);
         return true;
      }
   }

   if (mLib.IsLoaded())
   {
      mLib.Unload();
   }

   return false;
}

void LadspaEffect::Unload()
{
   if (mLib.IsLoaded())
   {
      mLib.Unload();
   }
}

void LadspaEffect::LoadParameters(const wxString & group)
{
   wxString value;

   if (mHost->GetPrivateConfig(group, wxT("Value"), value, wxEmptyString))
   {
      wxStringTokenizer st(value, wxT(','));
      if (st.CountTokens() == mData->PortCount)
      {
         for (unsigned long p = 0; st.HasMoreTokens(); p++)
         {
            double val = 0.0;
            st.GetNextToken().ToDouble(&val);

            if (val >= -1.0 && val <= 1.0)
            {
               mInputControls[p] = val;
            }
         }
      }
   }
}

void LadspaEffect::SaveParameters(const wxString & group)
{
   wxString parms;
   for (unsigned long p = 0; p < mData->PortCount; p++)
   {
      parms += wxString::Format(wxT(",%f"), mInputControls[p]);
   }

   mHost->SetPrivateConfig(group, wxT("Value"), parms.Mid(1));
}

LADSPA_Handle LadspaEffect::InitInstance(float sampleRate)
{
   /* Instantiate the plugin */
   LADSPA_Handle handle = mData->instantiate(mData, sampleRate);
   if (!handle)
   {
      return NULL;
   }

   for (unsigned long p = 0; p < mData->PortCount; p++)
   {
      LADSPA_PortDescriptor d = mData->PortDescriptors[p];
      if (LADSPA_IS_PORT_CONTROL(d))
      {
         if (LADSPA_IS_PORT_INPUT(d))
         {
            mData->connect_port(handle, p, &mInputControls[p]);
         }
         else
         {
            mData->connect_port(handle, p, &mOutputControls[p]);
         }
      }
   }

   if (mData->activate)
   {
      mData->activate(handle);
   }

   return handle;
}

void LadspaEffect::FreeInstance(LADSPA_Handle handle)
{
   if (mData->deactivate)
   {
      mData->deactivate(handle);
   }

   mData->cleanup(handle);
}

class Slider:public wxSlider
{
 public:
   Slider(wxWindow *parent, wxWindowID id,
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

   void OnSetFocus(wxFocusEvent & evt)
   {
      wxScrolledWindow *p = (wxScrolledWindow *) GetParent();
      wxRect r = GetRect();
      wxRect rv = p->GetRect();
      rv.y = 0;

      evt.Skip();

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

BEGIN_EVENT_TABLE(Slider, wxSlider)
    EVT_SET_FOCUS(Slider::OnSetFocus)
END_EVENT_TABLE()

class TextCtrl:public wxTextCtrl
{
 public:
   TextCtrl(wxWindow *parent, wxWindowID id,
            const wxString& value = wxEmptyString,
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize, long style = 0,
            const wxValidator& validator = wxDefaultValidator,
            const wxString& name = wxTextCtrlNameStr)
   : wxTextCtrl(parent, id, value,
                pos, size, style, validator, name)
   {
   };

   void OnSetFocus(wxFocusEvent & evt)
   {
      wxScrolledWindow *p = (wxScrolledWindow *) GetParent();
      wxRect r = GetRect();
      wxRect rv = p->GetRect();
      rv.y = 0;

      evt.Skip();

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

BEGIN_EVENT_TABLE(TextCtrl, wxTextCtrl)
   EVT_SET_FOCUS(TextCtrl::OnSetFocus)
END_EVENT_TABLE()

const int LADSPA_SECONDS_ID = 13101;

BEGIN_EVENT_TABLE(LadspaEffectDialog, wxDialog)
   EVT_BUTTON(wxID_APPLY, LadspaEffectDialog::OnApply)
   EVT_BUTTON(wxID_OK, LadspaEffectDialog::OnOK)
   EVT_BUTTON(wxID_CANCEL, LadspaEffectDialog::OnCancel)
   EVT_BUTTON(ePreviewID, LadspaEffectDialog::OnPreview)
   EVT_BUTTON(eDefaultsID, LadspaEffectDialog::OnDefaults)
   EVT_SLIDER(wxID_ANY, LadspaEffectDialog::OnSlider)
   EVT_TEXT(wxID_ANY, LadspaEffectDialog::OnTextCtrl)
   EVT_CHECKBOX(wxID_ANY, LadspaEffectDialog::OnCheckBox)
END_EVENT_TABLE()

IMPLEMENT_CLASS(LadspaEffectDialog, wxDialog)

LadspaEffectDialog::LadspaEffectDialog(LadspaEffect *eff,
                                       wxWindow * parent,
                                       const LADSPA_Descriptor *data,
                                       float *inputControls,
                                       sampleCount sampleRate,
                                       double length)
   :wxDialog(parent, -1, LAT1CTOWX(data->Name),
             wxDefaultPosition, wxDefaultSize,
             wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
    mEffect(eff)
{
   mNumParams = 0;
   mData = data;
   mInputControls = inputControls;
   mSampleRate = sampleRate;
   #ifdef __WXMSW__
      // On Windows, for some reason, wxWidgets calls OnTextCtrl during creation
      // of the text control, and LadspaEffectDialog::OnTextCtrl calls HandleText,
      // which assumes all the mFields have been initialized.
      // This can give us a bad pointer crash, so manipulate inSlider to
      // no-op HandleText during creation.
      inSlider = true;
   #else
      inSlider = false;
   #endif
   inText = false;

   mToggles = new wxCheckBox*[mData->PortCount];
   mSliders = new wxSlider*[mData->PortCount];
   mFields = new wxTextCtrl*[mData->PortCount];
   mLabels = new wxStaticText*[mData->PortCount];
   mPorts = new unsigned long [mData->PortCount];

   unsigned long p;
   for(p=0; p<mData->PortCount; p++) {
      LADSPA_PortDescriptor d = mData->PortDescriptors[p];
      if (LADSPA_IS_PORT_CONTROL(d) &&
          LADSPA_IS_PORT_INPUT(d)) {
         mPorts[mNumParams] = p;
         mNumParams++;
      }
   }

   wxControl *item;

   wxBoxSizer *vSizer = new wxBoxSizer(wxVERTICAL);

   if (mData->Maker &&
       mData->Maker[0] &&
       LAT1CTOWX(mData->Maker) != wxString(_("None"))) {
      item = new wxStaticText(this, 0,
                              wxString(_("Author: "))+LAT1CTOWX(mData->Maker));
      vSizer->Add(item, 0, wxALL, 5);
   }

   if (mData->Copyright &&
       mData->Copyright[0] &&
       LAT1CTOWX(mData->Copyright) != wxString(_("None"))) {

      item = new wxStaticText(this, 0,
                              LAT1CTOWX(mData->Copyright));
      vSizer->Add(item, 0, wxALL, 5);
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
   if (mEffect->IsRealtimeCapable()) {
      vSizer->Add(CreateStdButtonSizer(this, eApplyButton | eCancelButton | eDefaultsButton), 0, wxEXPAND);
   }
   else {
      vSizer->Add(CreateStdButtonSizer(this, ePreviewButton | eDefaultsButton | eCancelButton | eOkButton), 0, wxEXPAND);
   }

   SetSizer(vSizer);

   wxSizer *paramSizer =
      new wxStaticBoxSizer(wxVERTICAL, w, _("Effect Settings"));

   wxFlexGridSizer *gridSizer =
      new wxFlexGridSizer(5, 0, 0);
   gridSizer->AddGrowableCol(3);

   for (p = 0; p < mNumParams; p++) {
      wxString labelText = LAT1CTOWX(mData->PortNames[mPorts[p]]);
      item = new wxStaticText(w, 0, labelText + wxT(":"));
      gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

      wxString fieldText;
      LADSPA_PortRangeHint hint = mData->PortRangeHints[mPorts[p]];

      if (LADSPA_IS_HINT_TOGGLED(hint.HintDescriptor)) {
         mToggles[p] = new wxCheckBox(w, p, wxT(""));
         mToggles[p]->SetName(labelText);
         mToggles[p]->SetValue(mInputControls[mPorts[p]] > 0);
         gridSizer->Add(mToggles[p], 0, wxALL, 5);
         ConnectFocus(mToggles[p]);

         gridSizer->Add(1, 1, 0);
         gridSizer->Add(1, 1, 0);
         gridSizer->Add(1, 1, 0);
      }
      else {
         wxString bound;
         double lower = 0.0;
         double upper = 0.0;
         bool haslo = false;
         bool hashi = false;
         bool forceint = false;

         if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor)) {
            lower = hint.LowerBound;
            haslo = true;
         }
         if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor)) {
            upper = hint.UpperBound;
            hashi = true;
         }
         if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor)) {
            lower *= mSampleRate;
            upper *= mSampleRate;
            forceint = true;
         }

         if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor) || forceint)
            fieldText.Printf(wxT("%d"), (int)(mInputControls[mPorts[p]] + 0.5));
         else
            fieldText = Internat::ToDisplayString(mInputControls[mPorts[p]]);

         mFields[p] = new wxTextCtrl(w, p, fieldText);
         mFields[p]->SetName(labelText);
         gridSizer->Add(mFields[p], 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
         ConnectFocus(mFields[p]);

         wxString str;
         if (haslo) {
            if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor) || forceint)
               str.Printf(wxT("%d"), (int)(lower + 0.5));
            else
               str = Internat::ToDisplayString(lower);
            item = new wxStaticText(w, 0, str);
            gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);
         }
         else {
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

         if (hashi) {
            if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor) || forceint)
               str.Printf(wxT("%d"), (int)(upper + 0.5));
            else
               str = Internat::ToDisplayString(upper);
            item = new wxStaticText(w, 0, str);
            gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 5);
         }
         else {
            gridSizer->Add(1, 1, 0);
         }
      }
   }

   // Now add the length control
   if (mEffect->GetType() == EffectTypeGenerate) {
      item = new wxStaticText(w, 0, _("Length (seconds):"));
      gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);
      mSeconds = new NumericTextCtrl(NumericConverter::TIME, w, 
                                  wxID_ANY,
                                  _("hh:mm:ss + milliseconds"),
                                  length,
                                  mSampleRate,
                                  wxDefaultPosition,
                                  wxDefaultSize,
                                  true);
      mSeconds->SetName(_("Duration"));
      mSeconds->EnableMenu();
      gridSizer->Add(mSeconds, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
      gridSizer->Add(1, 1, 0);
      gridSizer->Add(1, 1, 0);
      gridSizer->Add(1, 1, 0);
      ConnectFocus(mSeconds);
   }

   // Set all of the mSliders based on the value in the
   // text mFields
   inSlider = false; // Now we're ready for HandleText to actually do something.
   mEffect->LoadParameters(wxT("Current"));
//   HandleText();

   paramSizer->Add(gridSizer, 1, wxEXPAND | wxALL, 5);
   w->SetSizer(paramSizer);

   Layout();
   Fit();
   SetSizeHints(GetSize());
}

LadspaEffectDialog::~LadspaEffectDialog()
{
   delete[]mToggles;
   delete[]mSliders;
   delete[]mFields;
   delete[]mLabels;
   delete[]mPorts;
}

void LadspaEffectDialog::OnCheckBox(wxCommandEvent & evt)
{
   int p = evt.GetId();

   mInputControls[mPorts[p]] = mToggles[p]->GetValue();
}

void LadspaEffectDialog::OnSlider(wxCommandEvent & evt)
{
   int p = evt.GetId();

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

   LADSPA_PortRangeHint hint = mData->PortRangeHints[mPorts[p]];
   if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor))
      lower = hint.LowerBound;
   if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor))
      upper = hint.UpperBound;
   if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor)) {
      lower *= mSampleRate;
      upper *= mSampleRate;
      forceint = true;
   }

   range = upper - lower;

   val = (mSliders[p]->GetValue() / 1000.0) * range + lower;

   wxString str;
   if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor) || forceint)
      str.Printf(wxT("%d"), (int)(val + 0.5));
   else
      str = Internat::ToDisplayString(val);

   mFields[p]->SetValue(str);

   mInputControls[mPorts[p]] = val;

   inSlider = false;
}

void LadspaEffectDialog::OnTextCtrl(wxCommandEvent & WXUNUSED(evt))
{
   HandleText();
}

void LadspaEffectDialog::HandleText()
{
   // if we don't add the following three lines, changing
   // the value of the slider will change the text, which
   // will change the slider, and so on.  This gets rid of
   // the implicit loop.

   if (inSlider)
      return;
   inText = true;
   for (unsigned long p = 0; p < mNumParams; p++) {
      double dval;
      float val;
      float lower = float(0.0);
      float upper = float(10.0);
      float range;

      LADSPA_PortRangeHint hint = mData->PortRangeHints[mPorts[p]];
      if (LADSPA_IS_HINT_TOGGLED(hint.HintDescriptor)) {
         continue;
      }

      dval = Internat::CompatibleToDouble(mFields[p]->GetValue());
      val = dval;

      if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor))
         lower = hint.LowerBound;
      if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor))
         upper = hint.UpperBound;
      if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor)) {
         lower *= mSampleRate;
         upper *= mSampleRate;
      }
      range = upper - lower;

      if (val < lower)
         val = lower;
      if (val > upper)
         val = upper;

      mInputControls[mPorts[p]] = val;

      mSliders[p]->SetValue((int)(((val-lower)/range) * 1000.0 + 0.5));
   }

   inText = false;
}

void LadspaEffectDialog::OnApply(wxCommandEvent & WXUNUSED(evt))
{
#if defined(__WXMAC__)
   Close();
#else
   Show(false);
#endif
   mEffect->SaveParameters(wxT("Current"));

   mEffect->mHost->Apply();
}

void LadspaEffectDialog::OnOK(wxCommandEvent & WXUNUSED(evt))
{
   mEffect->SaveParameters(wxT("Current"));

   EndModal(TRUE);
}

void LadspaEffectDialog::OnCancel(wxCommandEvent & WXUNUSED(evt))
{
   if (IsModal())
   {
      EndModal(FALSE);
   }
   else
   {
#if defined(__WXMAC__)
      Close();
#else
      Show(false);
#endif
   }
}

void LadspaEffectDialog::OnPreview(wxCommandEvent & WXUNUSED(evt))
{
   mEffect->mHost->Preview();
}

void LadspaEffectDialog::OnDefaults(wxCommandEvent & WXUNUSED(evt))
{
   mEffect->LoadParameters(wxT("Default"));
   RefreshParaemters();
}

void LadspaEffectDialog::RefreshParaemters()
{
   for (unsigned long p = 0; p < mNumParams; p++) {
      LADSPA_PortRangeHint hint = mData->PortRangeHints[mPorts[p]];

      if (LADSPA_IS_HINT_TOGGLED(hint.HintDescriptor)) {
         mToggles[p]->SetValue(mInputControls[mPorts[p]] > 0);
      }
      else {
         wxString bound;
         double lower = 0.0;
         double upper = 0.0;
         bool haslo = false;
         bool hashi = false;
         bool forceint = false;

         if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor)) {
            lower = hint.LowerBound;
            haslo = true;
         }
         if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor)) {
            upper = hint.UpperBound;
            hashi = true;
         }
         if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor)) {
            lower *= mSampleRate;
            upper *= mSampleRate;
            forceint = true;
         }

         wxString fieldText;
         if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor) || forceint)
            fieldText.Printf(wxT("%d"), (int)(mInputControls[mPorts[p]] + 0.5));
         else
            fieldText = Internat::ToDisplayString(mInputControls[mPorts[p]]);
         mFields[p]->ChangeValue(fieldText);
      }
   }
   HandleText();
}

void LadspaEffectDialog::ConnectFocus(wxControl *c)
{
   c->GetEventHandler()->Connect(wxEVT_SET_FOCUS,
                                 wxFocusEventHandler(LadspaEffectDialog::ControlSetFocus));
}

void LadspaEffectDialog::DisconnectFocus(wxControl *c)
{
   c->GetEventHandler()->Disconnect(wxEVT_SET_FOCUS,
                                 wxFocusEventHandler(LadspaEffectDialog::ControlSetFocus));
}

void LadspaEffectDialog::ControlSetFocus(wxFocusEvent & evt)
{
   wxControl *c = (wxControl *) evt.GetEventObject();
   wxScrolledWindow *p = (wxScrolledWindow *) c->GetParent();
   wxRect r = c->GetRect();
   wxRect rv = p->GetRect();
   rv.y = 0;

   evt.Skip();

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

double LadspaEffectDialog::GetLength()
{
   if (mEffect->GetType() == EffectTypeGenerate) {
      return mSeconds->GetValue();
   }

   return 0;
}
