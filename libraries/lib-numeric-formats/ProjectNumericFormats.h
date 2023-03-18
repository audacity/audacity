/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file ProjectNumericFormats.cpp
 
 Paul Licameli split from ProjectSettings.cpp
 
 **********************************************************************/
#ifndef __AUDACITY_PROJECT_NUMERIC_FORMATS__
#define __AUDACITY_PROJECT_NUMERIC_FORMATS__

#include "ClientData.h"
#include "ComponentInterfaceSymbol.h"
#include "Observer.h"

class AudacityProject;

struct ProjectNumericFormatsEvent {
   const enum Type : int {
      ChangedSelectionFormat,
      ChangedAudioTimeFormat,
      ChangedFrequencyFormat,
      ChangedBandwidthFormat,
   } type;
   const NumericFormatSymbol oldValue;
   const NumericFormatSymbol newValue;
};

class NUMERIC_FORMATS_API ProjectNumericFormats final
   : public ClientData::Base
   , public Observer::Publisher<ProjectNumericFormatsEvent>
{
public:
   static ProjectNumericFormats &Get(AudacityProject &project);
   static const ProjectNumericFormats &Get(const AudacityProject &project);

   ProjectNumericFormats();
   ~ProjectNumericFormats() override;

   // Selection Format
   void SetSelectionFormat(const NumericFormatSymbol & format);
   const NumericFormatSymbol & GetSelectionFormat() const;

   // AudioTime format
   void SetAudioTimeFormat(const NumericFormatSymbol & format);
   const NumericFormatSymbol & GetAudioTimeFormat() const;

   // Spectral Selection Formats
   void SetFrequencySelectionFormatName(const NumericFormatSymbol & format);
   const NumericFormatSymbol & GetFrequencySelectionFormatName() const;

   void SetBandwidthSelectionFormatName(const NumericFormatSymbol & format);
   const NumericFormatSymbol & GetBandwidthSelectionFormatName() const;

private:
   NumericFormatSymbol mSelectionFormat;
   NumericFormatSymbol mFrequencySelectionFormatName;
   NumericFormatSymbol mBandwidthSelectionFormatName;
   NumericFormatSymbol mAudioTimeFormat;
};

#endif
