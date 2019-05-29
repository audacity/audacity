/**********************************************************************

Audacity: A Digital Audio Editor

ProjectSettings.h

Paul Licameli split from AudacityProject.h

**********************************************************************/

#ifndef __AUDACITY_PROJECT_SETTINGS__
#define __AUDACITY_PROJECT_SETTINGS__

#include "ClientData.h" // to inherit
#include "Prefs.h" // to inherit

class AudacityProject;

///\brief Holds various per-project settings values, including the sample rate,
/// and sends events to the project when certain values change
class ProjectSettings final
   : public ClientData::Base
   , private PrefsListener
{
public:
   static ProjectSettings &Get( AudacityProject &project );
   static const ProjectSettings &Get( const AudacityProject &project );
   
   ProjectSettings( AudacityProject &project );

   sampleFormat GetDefaultFormat() const { return mDefaultFormat; }

   double GetRate() const { return mRate; }
   void SetRate( double value ) { mRate = value; }

   bool GetTracksFitVerticallyZoomed() const { return mTracksFitVerticallyZoomed; } //lda
   void SetTracksFitVerticallyZoomed(bool flag) { mTracksFitVerticallyZoomed = flag; } //lda

   bool GetShowId3Dialog() const { return mShowId3Dialog; } //lda
   void SetShowId3Dialog(bool flag) { mShowId3Dialog = flag; } //lda

   bool GetNormalizeOnLoad() const { return mNormalizeOnLoad; } //lda
   void SetNormalizeOnLoad(bool flag) { mNormalizeOnLoad = flag; } //lda

   bool IsSyncLocked() const;
   void SetSyncLock(bool flag);
   
   // Snap To

   void SetSnapTo(int snap);
   int GetSnapTo() const;

   // Selection Format

   void SetSelectionFormat(const NumericFormatSymbol & format);
   const NumericFormatSymbol & GetSelectionFormat() const;

   // Spectral Selection Formats

   void SetFrequencySelectionFormatName(const NumericFormatSymbol & format);
   const NumericFormatSymbol & GetFrequencySelectionFormatName() const;

   void SetBandwidthSelectionFormatName(const NumericFormatSymbol & format);
   const NumericFormatSymbol & GetBandwidthSelectionFormatName() const;

   bool IsSoloSimple() const { return mSoloPref == wxT("Simple"); }
   bool IsSoloNone() const { return mSoloPref == wxT("None"); }

   bool EmptyCanBeDirty() const { return mEmptyCanBeDirty; }

   bool GetShowSplashScreen() const { return mShowSplashScreen; }

private:
   void UpdatePrefs() override;

   AudacityProject &mProject;

   NumericFormatSymbol mSelectionFormat;
   NumericFormatSymbol mFrequencySelectionFormatName;
   NumericFormatSymbol mBandwidthSelectionFormatName;

   wxString mSoloPref;

   double mRate;

   sampleFormat mDefaultFormat;
   int mSnapTo;
   
   bool mTracksFitVerticallyZoomed{ false };  //lda
   bool mShowId3Dialog{ true }; //lda
   bool mNormalizeOnLoad;  //lda
   bool mIsSyncLocked{ false };
   bool mEmptyCanBeDirty;
   bool mShowSplashScreen;
};

#endif
