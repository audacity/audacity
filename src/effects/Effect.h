/**********************************************************************

  Audacity: A Digital Audio Editor

  Effect.h

  Dominic Mazzoni
  Vaughan Johnson

**********************************************************************/

#ifndef __AUDACITY_EFFECT__
#define __AUDACITY_EFFECT__

#include <set>

#include <wx/dynarray.h>
#include <wx/intl.h> 
#include <wx/string.h>

class wxDialog;
class wxWindow;

#include "../WaveTrack.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../Internat.h"
#include "../widgets/ProgressDialog.h"

class TimeWarper;

#define PLUGIN_EFFECT   0x0001
#define BUILTIN_EFFECT  0x0002
// ADVANCED_EFFECT was introduced for Lynn Allan's 'CleanSpeech'
// it allows the list of effects to be filtered to exclude
// the advanced effects.
// Left in when CLEANSPEECH was removed, as it may be useful at some point.
#define ADVANCED_EFFECT 0x0004
// HIDDEN_EFFECT allows an item to be excluded from the effects
// menu
#define HIDDEN_EFFECT   0x0008

#define INSERT_EFFECT   0x0010
#define PROCESS_EFFECT  0x0020
#define ANALYZE_EFFECT  0x0040
#define ALL_EFFECTS     0x00FF

// Flag used to disable prompting for configuration
// parameteres.
#define CONFIGURED_EFFECT 0x8000

//CLEAN-ME: Rogue value to skip unwanted effects in a chain.
//lda: SKIP_EFFECT_MILLISECOND is a rogue value, used where a millisecond
//time is required to indicate "Don't do this effect".
//It is used in SpikeCleaner and TruncSilence.
//MERGE: Maybe we can remove this now that we have configurable batch
//and so can just drop the steps we don't want?
#define SKIP_EFFECT_MILLISECOND 99999

class AUDACITY_DLL_API Effect {
 // 
 // public methods
 //
 // Used by the outside program to determine properties of an effect and
 // apply the effect to one or more tracks.
 // 
 public:
   // Each subclass of Effect should override this method.
   // This name will go in the menu bar;
   // append "..." if your effect pops up a dialog
   virtual wxString GetEffectName() = 0;

   // Each subclass of Effect should override this method.
   // This should return the category of this effect, which
   // will determine where in the menu it's placed.
#ifdef EFFECT_CATEGORIES
   virtual std::set<wxString> GetEffectCategories() = 0;
#endif

   // Previously optional. Now required to identify effects for Chain support.
   // Each subclass of Effect should override this method.
   // This should be human-readable, but should NOT be translated.  Use wxT(""), not _("").
   virtual wxString GetEffectIdentifier() = 0;
   
   // Each subclass of Effect should override this method.
   // This name will go in the progress dialog, but can be used
   // elsewhere, and it should describe what is being done.
   // For example, if the effect is "Filter", the action is
   // "Filtering", or if the effect is "Bass Boost", the action
   // is "Boosting Bass Frequencies".
   virtual wxString GetEffectAction() = 0;

   // Each subclass of Effect should override this method. 
   // This description will go in the History state. 
   // Override to include effect parameters, so typically useful only after PromptUser. 
   virtual wxString GetEffectDescription() { 
      // Default provides effect name. 
      return wxString::Format(_("Applied effect: %s"), 
                              this->GetEffectName().c_str()); 
   } 
	 
   // Return flags which tell you what kind of effect this is.
   // It will be either a built-in or a plug-in effect, and it
   // will be one of Insert, Process, or Analyze.
   virtual int GetEffectFlags() {
      // Default of BUILTIN_EFFECT | PROCESS_EFFECT | ADVANCED_EFFECT (set in constructor) - 
      // covers most built-in effects.
      return mFlags;
   }

   // Return true if the effect supports processing via batch chains.
   virtual bool SupportsChains() {
      // All builtin effect support chains (???)
      return (mFlags & BUILTIN_EFFECT) != 0;
   }

   // Called to set or retrieve parameter values.  Return true if successful.
   virtual bool TransferParameters( Shuttle & WXUNUSED(shuttle) ) {
      return true;
   }

   void SetEffectFlags( int NewFlags )
   {
      mFlags = NewFlags;
   }

   // The Effect class fully implements the Preview method for you.
   // Only override it if you need to do preprocessing or cleanup.
   virtual void Preview(bool dryOnly = false);

   // Most effects just use the previewLength, but time-stretching/compressing 
   // effects need to use a different input length, so override this method. 
   virtual double CalcPreviewInputLength(double previewLength);

   // Get an unique ID assigned to each registered effect.
   // The first effect will have ID zero.
   int GetID() {
      return mID;
   }

   // Returns true on success.  Will only operate on tracks that
   // have the "selected" flag set to true, which is consistent with
   // Audacity's standard UI.
   bool DoEffect(wxWindow *parent, int flags, double projectRate, TrackList *list,
                 TrackFactory *factory, double *t0, double *t1, wxString params);

   wxString GetPreviewName();

   //ANSWER-ME: Isn't this pointless?
   //    None of the built-in functions has an ampersand in the result of 
   //    GetEffectName(), the only strings on which this method is used. 
   //    In fact, the example 'E&qualizer' does not exist in the code!
   // Strip ampersand ('&' char) from string. This effectively removes the
   // shortcut from the string ('E&qualizer' becomes 'Equalizer'). This is
   // important for sorting.
   static wxString StripAmpersand(const wxString& str);

 //
 // protected virtual methods
 //
 // Each subclass of Effect overrides one or more of these methods to
 // do its processing.
 //
 protected:
   // The constructor is called once by each subclass at the beginning of the program.
   // Avoid allocating memory or doing time-consuming processing here.
   Effect();

   virtual ~Effect();
 
   // Called once each time an effect is called.  Perform any initialization;
   // make sure that the effect can be performed on the selected tracks and
   // return false otherwise
   virtual bool Init() {
      return true;
   }

   // If necessary, open a dialog to get parameters from the user.
   // This method will not always be called (for example if a user
   // repeats an effect) but if it is called, it will be called
   // after Init.
   virtual bool PromptUser() {
      return true;
   }
   // Check whether effect should be skipped
   // Typically this is only useful in automation, for example
   // detecting that zero noise reduction is to be done,
   // or that normalisation is being done without Dc bias shift 
   // or amplitude modification
   virtual bool CheckWhetherSkipEffect() { return false; }
      
   // Actually do the effect here.
   virtual bool Process() = 0;

   // clean up any temporary memory
   virtual void End() {
   }


 //
 // protected data
 //
 // The Effect base class will set these variables, some or all of which
 // may be needed by any particular subclass of Effect.
 //
 protected:
   wxWindow       *mParent;
   ProgressDialog *mProgress;
   double         mProjectRate; // Sample rate of the project - new tracks should
                               // be created with this rate...
   TrackFactory   *mFactory;
   TrackList      *mTracks;      // the complete list of all tracks
   TrackList      *mOutputTracks; // used only if CopyInputTracks() is called.
   double         mT0;
   double         mT1;
   TimeWarper     *mWarper;

 //
 // protected methods
 //
 // These methods can be used by subclasses of Effect in order to display a
 // progress dialog or perform common calculations
 //
 protected:
   // The Progress methods all return true if the user has cancelled;
   // you should exit immediately if this happens (cleaning up memory
   // is okay, but don't try to undo).
 
   // Pass a fraction between 0.0 and 1.0
   bool TotalProgress(double frac);
   
   // Pass a fraction between 0.0 and 1.0, for the current track
   // (when doing one track at a time)
   bool TrackProgress(int whichTrack, double frac, wxString = wxT(""));
 
   // Pass a fraction between 0.0 and 1.0, for the current track group
   // (when doing stereo groups at a time)
   bool TrackGroupProgress(int whichGroup, double frac);

   int GetNumWaveTracks() { return mNumTracks; }

   int GetNumWaveGroups() { return mNumGroups; }

   // Calculates the start time and selection length in samples
   void GetSamples(WaveTrack *track, sampleCount *start, sampleCount *len);

   void SetTimeWarper(TimeWarper *warper);
   TimeWarper *GetTimeWarper();

 //
 // protected static data
 //
 // Preferences shared by all effects
 //
 protected:
   static double sDefaultGenerateLen;
   int mFlags;
   double mLength;

   // type of the tracks on mOutputTracks
   int mOutputTracksType;
 
 //
 // private methods
 //
   // Use these two methods to copy the input tracks to mOutputTracks, if 
   // doing the processing on them, and replacing the originals only on success (and not cancel).
   void CopyInputTracks(int trackType = Track::Wave);

   // If bGoodResult, replace mWaveTracks tracks in mTracks with successfully processed 
   // mOutputTracks copies, get rid of old mWaveTracks, and set mWaveTracks to mOutputTracks. 
   // Else clear and delete mOutputTracks copies.
   void ReplaceProcessedTracks(const bool bGoodResult);

   // Use this to append a new output track.
   void AddToOutputTracks(Track *t);

 // Used only by the base Effect class
 //
 private:
   void CountWaveTracks();
 
 //
 // private data
 //
 // Used only by the base Effect class
 //
 private:
   wxArrayPtrVoid mIMap;
   wxArrayPtrVoid mOMap;

   int mNumTracks; //v This is really mNumWaveTracks, per CountWaveTracks() and GetNumWaveTracks().
   int mNumGroups;

   int mID;
   
   friend class BatchCommands;// so can call PromptUser.
   friend class EffectManager;// so it can delete effects and access mID.

};


// Base dialog for generate effect

#define ID_EFFECT_PREVIEW ePreviewID

// Base dialog for regular effect
class AUDACITY_DLL_API EffectDialog:public wxDialog
{
public:
   // constructors and destructors
   EffectDialog(wxWindow * parent,
                const wxString & title,
                int type = PROCESS_EFFECT,
                int flags = wxDEFAULT_DIALOG_STYLE,
                int additionalButtons = 0);

   void Init();

   virtual void PopulateOrExchange(ShuttleGui & S);
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();
   virtual bool Validate();
   virtual void OnPreview(wxCommandEvent & event);

private:
   int mType;
   int mAdditionalButtons;
};

// Utility functions

float TrapFloat(float x, float min, float max);
double TrapDouble(double x, double min, double max);
long TrapLong(long x, long min, long max);

#endif

