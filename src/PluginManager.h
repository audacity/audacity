/**********************************************************************

  Audacity: A Digital Audio Editor

  PluginManager.h

  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_PLUGINMANAGER_H__
#define __AUDACITY_PLUGINMANAGER_H__

#include <wx/defs.h>
#include <wx/dynarray.h>
#include <wx/fileconf.h>
#include <wx/string.h>

///////////////////////////////////////////////////////////////////////////////
//
// PluginManager
//
///////////////////////////////////////////////////////////////////////////////

typedef enum
{
   PluginTypeAll,
   PluginTypeVST,
   PluginTypeLadspa
} PluginType;

class PluginManager
{
 public:
   PluginManager();
   virtual ~PluginManager();

   void Open();
   void Close();

   static PluginManager & Get(bool refresh = false);

   wxString Read(const wxString & key, const wxString & def);
   long Read(const wxString & key, long def);

   void Write(const wxString & key, const wxString & val);
   void Write(const wxString & key, long val);

   bool HasType(const wxString & type);
   void PurgeType(const wxString & type);

   int GetPluginCount(const wxString & type);
   wxString GetPlugin(const wxString & type, int index);

   wxString GetFirstPlugin(const wxString & type);
   wxString GetNextPlugin(const wxString & type);

   bool IsRegistered(const wxString & type, const wxString & path);
   void RegisterPlugin(const wxString & type, const wxString & path);

   bool IsPluginEnabled(const wxString & type, const wxString & path);
   void EnablePlugin(const wxString & type, const wxString & path, bool enable);

private:
   bool IsDirty();
   void SetDirty(bool dirty = true);
   wxFileConfig *mConfig;

   bool mDirty;
   int mCurrentIndex;
};

#endif /* __AUDACITY_LOADMODULES_H__ */

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3

