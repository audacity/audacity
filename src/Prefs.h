/**********************************************************************

  Audacity: A Digital Audio Editor

  Prefs.h

  Dominic Mazzoni
  Markus Meyer

  Audacity uses wxWidgets' wxFileConfig class to handle preferences.
  In Audacity versions prior to 1.3.1, it used wxConfig, which would
  store the prefs in a platform-dependent way (e.g. in the registry
  on Windows). Now it always stores the settings in a configuration file
  in the Audacity Data Directory.

  Every time we read a preference, we need to specify the default
  value for that preference, to be used if the preference hasn't
  been set before.

  So, to avoid code duplication, we provide functions in this file
  to read and write preferences which have a nonobvious default
  value, so that if we later want to change this value, we only
  have to change it in one place.

  See Prefs.cpp for a (complete?) list of preferences we keep
  track of...

**********************************************************************/
#ifndef __AUDACITY_PREFS__
#define __AUDACITY_PREFS__

#include "Audacity.h"

#include "../include/audacity/ComponentInterface.h"
#include "MemoryX.h" // for wxArrayStringEx
#include "widgets/FileConfig.h"

#include <memory>
#include <wx/event.h> // to declare custom event types

class wxFileName;

void InitPreferences( std::unique_ptr<FileConfig> uPrefs );
void FinishPreferences();

extern AUDACITY_DLL_API FileConfig *gPrefs;
extern int gMenusDirty;


struct ByColumns_t{};
extern ByColumns_t ByColumns;

/// A table of EnumValueSymbol that you can access by "row" with
/// operator [] but also allowing access to the "columns" of internal or
/// translated strings, and also allowing convenient column-wise construction
class EnumValueSymbols : public std::vector< EnumValueSymbol >
{
public:
   EnumValueSymbols() = default;
   EnumValueSymbols( std::initializer_list<EnumValueSymbol> symbols )
     : vector( symbols )
   {}

   // columnwise constructor; arguments must have same size
   // (Implicit constructor takes initial tag argument to avoid unintended
   // overload resolution to the inherited constructor taking
   // initializer_list, in the case that each column has exactly two strings)
   EnumValueSymbols(
      ByColumns_t,
      const TranslatableStrings &msgids,
      wxArrayStringEx internals
   );

   const TranslatableStrings &GetMsgids() const;
   const wxArrayStringEx &GetInternals() const;

private:
   mutable TranslatableStrings mMsgids;
   mutable wxArrayStringEx mInternals;
};

/// Packages a table of user-visible choices each with an internal code string,
/// a preference key path, and a default choice
class ChoiceSetting
{
public:
   ChoiceSetting(
      const wxString &key,
      EnumValueSymbols symbols,
      long defaultSymbol = -1
   )
      : mKey{ key }

      , mSymbols{ std::move( symbols ) }

      , mDefaultSymbol{ defaultSymbol }
   {
      wxASSERT( defaultSymbol < (long)mSymbols.size() );
   }

   const wxString &Key() const { return mKey; }
   const EnumValueSymbol &Default() const;
   const EnumValueSymbols &GetSymbols() const { return mSymbols; }

   wxString Read() const;

   // new direct use is discouraged but it may be needed in legacy code:
   // use a default in case the preference is not defined, which may not be
   // the default-default stored in this object.
   wxString ReadWithDefault( const wxString & ) const;

   bool Write( const wxString &value ); // you flush gPrefs afterward

   void SetDefault( long value );

protected:
   size_t Find( const wxString &value ) const;
   virtual void Migrate( wxString& );

   const wxString mKey;

   const EnumValueSymbols mSymbols;

   // stores an internal value
   mutable bool mMigrated { false };

   long mDefaultSymbol;
};

/// Extends ChoiceSetting with a corresponding table of integer codes
/// (generally not equal to their table positions),
/// and optionally an old preference key path that stored integer codes, to be
/// migrated into one that stores internal string values instead
class EnumSettingBase : public ChoiceSetting
{
public:
   EnumSettingBase(
      const wxString &key,
      EnumValueSymbols symbols,
      long defaultSymbol,

      std::vector<int> intValues, // must have same size as symbols
      const wxString &oldKey = {}
   );

protected:

   // Read and write the encoded values
   int ReadInt() const;

   // new direct use is discouraged but it may be needed in legacy code:
   // use a default in case the preference is not defined, which may not be
   // the default-default stored in this object.
   int ReadIntWithDefault( int defaultValue ) const;

   bool WriteInt( int code ); // you flush gPrefs afterward

   size_t FindInt( int code ) const;
   void Migrate( wxString& ) override;

private:
   std::vector<int> mIntValues;
   const wxString mOldKey;
};

/// Adapts EnumSettingBase to a particular enumeration type
template< typename Enum >
class EnumSetting : public EnumSettingBase
{
public:

   EnumSetting(
      const wxString &key,
      EnumValueSymbols symbols,
      long defaultSymbol,

      std::vector< Enum > values, // must have same size as symbols
      const wxString &oldKey = {}
   )
      : EnumSettingBase{
         key, symbols, defaultSymbol,
         { values.begin(), values.end() },
         oldKey
      }
   {}

   // Wrap ReadInt() and ReadIntWithDefault() and WriteInt()
   Enum ReadEnum() const
   { return static_cast<Enum>( ReadInt() ); }

   // new direct use is discouraged but it may be needed in legacy code:
   // use a default in case the preference is not defined, which may not be
   // the default-default stored in this object.
   Enum ReadEnumWithDefault( Enum defaultValue ) const
   {
      auto integer = static_cast<int>(defaultValue);
      return static_cast<Enum>( ReadIntWithDefault( integer ) );
   }

   bool WriteEnum( Enum value )
   { return WriteInt( static_cast<int>( value ) ); }

};

// An event emitted by the application when the Preference dialog commits
// changes
wxDECLARE_EVENT(EVT_PREFS_UPDATE, wxCommandEvent);

// Invoke UpdatePrefs() when Preference dialog commits changes.
class AUDACITY_DLL_API PrefsListener
{
public:
   PrefsListener();
   virtual ~PrefsListener();

   // Called when all preferences should be updated.
   virtual void UpdatePrefs() = 0;

protected:
   // Called when only selected preferences are to be updated.
   // id is some value generated by wxNewId() that identifies the portion
   // of preferences.
   // Default function does nothing.
   virtual void UpdateSelectedPrefs( int id );

private:
   struct Impl;
   std::unique_ptr<Impl> mpImpl;
};

/// Return the config file key associated with a warning dialog identified
/// by internalDialogName.  When the box is checked, the value at the key
/// becomes false.
wxString WarningDialogKey(const wxString &internalDialogName);

/*
 Meant to be statically constructed.  A callback to repopulate configuration
 files after a reset.
 */
struct PreferenceInitializer {
   PreferenceInitializer();
   virtual ~PreferenceInitializer();
   virtual void operator () () = 0;

   static void ReinitializeAll();
};

#endif
