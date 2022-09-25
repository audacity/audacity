/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NyquistProgram.h

  Dominic Mazzoni

  Paul Licameli split from Nyquist.h

**********************************************************************/
#ifndef __AUDACITY_EFFECT_NYQUIST_PROGRAM__
#define __AUDACITY_EFFECT_NYQUIST_PROGRAM__

#include "NyquistParser.h"
#include "NyquistEnvironment.h"
#include "../../widgets/wxPanelWrapper.h"

struct NyquistProgram : NyquistParser
{
   using NyquistParser::NyquistParser;

   // When there is a more general EffectContext for all effects, these will
   // move into that
   struct EffectContext {
      const TrackList *const mInputTracks{};
      TrackList *const mOutputTracks{};
      wxWindow *const mUIParent{};
      const int mNumWaveGroups;
      const bool mIsPreviewing;

      // spectral selection
      const double      mF0;
      const double      mF1;

      // time selection -- which may be modified by processing!
      double            &mT0;
      double            &mT1;

      bool              &mDebug; // When true, debug window is shown.
   };

   struct Context {
      EffectContext     &mContext;

      const unsigned    mNumSelectedChannels;
      const bool        mAcceptsAll{};
      const bool        mExternal{};

      wxString          mProps;
      int               mTrackIndex{ 0 };
      unsigned          mCount{ 0 };
      bool              mProjectChanged{ false };
      double            mOutputTime{ 0 };

      // Keep track of whether the current track is first selected in its
      // sync-lock group (we have no idea what the length of the returned
      // audio will be, so we have to handle sync-lock group behavior the
      // "old" way).
      bool              mFirstInGroup{ true };

   };

   wxString          mCmd;      // the command to be processed
   bool              mHelpFileExists;
   FilePath          mHelpPage;

   NyquistControls &GetControls() { return mControls; }
   const NyquistControls &GetControls() const { return mControls; }
   NyquistBindings &GetBindings() { return mBindings; }
   const NyquistBindings &GetBindings() const { return mBindings; }

   bool Parse(wxInputStream & stream);

   // All state is externalized into context,
   // so the member function can be const
   bool Process(const AudacityProject *project,
      NyquistEnvironment &environment, Context &context,
      EffectSettings &settings) const;
   bool ProcessOne(NyquistEnvironment &environment, Context &context,
      NyquistTrack &nyquistTrack) const;

   std::pair<bool, FilePath> CheckHelpPage() const;
   static wxString NyquistToWxString(const char *nyqString);
};

class NyquistOutputDialog final : public wxDialogWrapper
{
public:
   NyquistOutputDialog(wxWindow * parent, wxWindowID id,
                       const TranslatableString & title,
                       const TranslatableString & prompt,
                       const TranslatableString &message);

private:
   void OnOk(wxCommandEvent & event);

private:
   DECLARE_EVENT_TABLE()
};
#endif
