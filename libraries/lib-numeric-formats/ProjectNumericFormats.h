/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file ProjectNumericFormats.cpp
 
 Paul Licameli split from ProjectSettings.cpp
 
 **********************************************************************/
#ifndef __AUDACITY_PROJECT_NUMERIC_FORMATS__
#define __AUDACITY_PROJECT_NUMERIC_FORMATS__

#include "ClientData.h"
#include "ComponentInterfaceSymbol.h"
#include "NumericConverterType.h"

class AudacityProject;

class NUMERIC_FORMATS_API ProjectNumericFormats final : public ClientData::Base
{
public:
   static ProjectNumericFormats &Get(AudacityProject &project);
   static const ProjectNumericFormats &Get(const AudacityProject &project);

   explicit ProjectNumericFormats(const AudacityProject& project);
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

   NumericFormatSymbol LookupFormat(const NumericConverterType& type, const wxString& identifier);

private:
   const AudacityProject& mProject;
   
   NumericFormatSymbol mSelectionFormat;
   NumericFormatSymbol mFrequencySelectionFormatName;
   NumericFormatSymbol mBandwidthSelectionFormatName;
   NumericFormatSymbol mAudioTimeFormat;
};

#endif
