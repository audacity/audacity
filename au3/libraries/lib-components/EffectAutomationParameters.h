/**********************************************************************

   Audacity: A Digital Audio Editor

   EffectAutomationParameters.h
   (defining CommandParameters)

   Leland Lucius

   Copyright (c) 2014, Audacity Team
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

   3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
   FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
   COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
   BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
   POSSIBILITY OF SUCH DAMAGE.

**********************************************************************/

#ifndef __AUDACITY_COMMAND_PARAMETERS_H__
#define __AUDACITY_COMMAND_PARAMETERS_H__

#include <locale.h>

#include <wx/cmdline.h> // for wxCmdLineParser::ConvertStringToArgs
#include <wx/fileconf.h> // to inherit
#include <algorithm>

#include "ComponentInterface.h"
#include "ComponentInterfaceSymbol.h"

/**
\brief CommandParameters, derived from wxFileConfig, is essentially doing
the same things as the SettingsVisitor classes.  It does text <-> binary conversions of
parameters.  It does not seem to be using actual file read/writing.

Should it be converted to using SettingsVisitor?  Probably yes.  SettingsVisitor leads to shorter code.
And SettingsVisitor is more multi-functional since SettingsVisitor can report on signature, do the work of
wxWidget validators, and can create default dialogs.  However until that conversion is
done, we need this class, and we use a pointer to one from within a SettingsVisitor when interfacing
with the code that still uses it.
*/
class COMPONENTS_API CommandParameters final : public wxFileConfig
{
public:
    CommandParameters(const wxString& parms = {})
        :  wxFileConfig(wxEmptyString,
                        wxEmptyString,
                        wxEmptyString,
                        wxEmptyString,
                        0)
    {
        SetExpandEnvVars(false);
        SetParameters(parms);
    }

    virtual ~CommandParameters();

    virtual bool HasGroup(const wxString& strName) const override
    {
        return wxFileConfig::HasGroup(NormalizeName(strName));
    }

    virtual bool HasEntry(const wxString& strName) const override
    {
        return wxFileConfig::HasEntry(NormalizeName(strName));
    }

    virtual bool DoReadString(const wxString& key, wxString* pStr) const override
    {
        return wxFileConfig::DoReadString(NormalizeName(key), pStr);
    }

    virtual bool DoReadLong(const wxString& key, long* pl) const override
    {
        return wxFileConfig::DoReadLong(NormalizeName(key), pl);
    }

    virtual bool DoReadDouble(const wxString& key, double* pd) const override
    {
        wxString str;
        if (Read(key, &str)) {
            struct lconv* info = localeconv();
            wxString dec
                =info ? wxString::FromUTF8(info->decimal_point) : wxString(".");

            str.Replace(wxT(","), dec);
            str.Replace(wxT("."), dec);

            return str.ToDouble(pd);
        }

        return false;
    }

    virtual bool DoWriteString(const wxString& key, const wxString& szValue) override
    {
        return wxFileConfig::DoWriteString(NormalizeName(key), szValue);
    }

    virtual bool DoWriteLong(const wxString& key, long lValue) override
    {
        return wxFileConfig::DoWriteLong(NormalizeName(key), lValue);
    }

    virtual bool DoWriteDouble(const wxString& key, double value) override
    {
        return DoWriteString(key, wxString::Format(wxT("%.8g"), value));
    }

    bool ReadFloat(const wxString& key, float* pf) const
    {
        double d = *pf;
        bool success = Read(key, &d);
        if (success) {
            *pf = (float)d;
        }
        return success;
    }

    bool ReadFloat(const wxString& key, float* pf, float defVal) const
    {
        if (!ReadFloat(key, pf)) {
            *pf = defVal;
        }
        return true;
    }

    bool WriteFloat(const wxString& key, float f)
    {
        return Write(key, f);
    }

    // For reading old config files with enumeration names that have been
    // changed in later versions.  Pair a string with an index into the other
    // list of non-obsolete names.
    using ObsoleteMap = std::pair< wxString, size_t >;

    bool ReadEnum(const wxString& key, int* pi,
                  const EnumValueSymbol choices[], size_t nChoices,
                  const ObsoleteMap obsoletes[] = nullptr,
                  size_t nObsoletes = 0) const
    {
        wxString s;
        if (!wxFileConfig::Read(key, &s)) {
            return false;
        }
        *pi = std::find(choices, choices + nChoices,
                        EnumValueSymbol { s, {} }) - choices;
        if (*pi == (int)nChoices) {
            *pi = -1;
        }
        if (*pi < 0 && obsoletes) {
            auto index = std::find_if(obsoletes, obsoletes + nObsoletes,
                                      [&](const ObsoleteMap& entry){
                return entry.first == s;
            })
                         - obsoletes;
            if (index < (int)nObsoletes) {
                *pi = (int)obsoletes[index].second;
            }
        }
        return true;
    }

    bool ReadEnum(const wxString& key, int* pi, int defVal,
                  const EnumValueSymbol choices[], size_t nChoices,
                  const ObsoleteMap obsoletes[] = nullptr,
                  size_t nObsoletes = 0) const
    {
        if (!ReadEnum(key, pi, choices, nChoices, obsoletes, nObsoletes)) {
            *pi = defVal;
        }
        return true;
    }

    bool WriteEnum(const wxString& key, int value,
                   const EnumValueSymbol choices[], size_t nChoices)
    {
        if (value < 0 || value >= (int)nChoices) {
            return false;
        }

        return wxFileConfig::Write(key, choices[value].Internal());
    }

    bool ReadAndVerify(const wxString& key, float* val, float defVal, float min, float max) const
    {
        ReadFloat(key, val, defVal);
        return *val >= min && *val <= max;
    }

    bool ReadAndVerify(const wxString& key, double* val, double defVal, double min, double max) const
    {
        Read(key, val, defVal);
        return *val >= min && *val <= max;
    }

    bool ReadAndVerify(const wxString& key, int* val, int defVal, int min, int max) const
    {
        Read(key, val, defVal);
        return *val >= min && *val <= max;
    }

    bool ReadAndVerify(const wxString& key, long* val, long defVal, long min, long max) const
    {
        Read(key, val, defVal);
        return *val >= min && *val <= max;
    }

    bool ReadAndVerify(const wxString& key, bool* val, bool defVal, bool = false, bool = false) const
    {
        Read(key, val, defVal);
        return true;
    }

    bool ReadAndVerify(const wxString& key, wxString* val, const wxString& defVal,
                       const wxString& = {}, const wxString& = {}) const
    {
        Read(key, val, defVal);
        return true;
    }

    bool ReadAndVerify(const wxString& key, int* val, int defVal,
                       const EnumValueSymbol choices[], size_t nChoices,
                       const ObsoleteMap obsoletes[] = nullptr,
                       size_t nObsoletes = 0) const
    {
        ReadEnum(key, val, defVal, choices, nChoices, obsoletes, nObsoletes);
        return *val != wxNOT_FOUND;
    }

    bool GetParameters(wxString& parms)
    {
        wxFileConfig::SetPath(wxT("/"));

        wxString str;
        wxString key;

        long ndx = 0;
        bool res = wxFileConfig::GetFirstEntry(key, ndx);
        while (res)
        {
            wxString val;
            if (!wxFileConfig::Read(key, &val)) {
                return false;
            }

            str += key + wxT("=\"") + Escape(val) + wxT("\" ");

            res = wxFileConfig::GetNextEntry(key, ndx);
        }
        str.Trim();

        parms = str;

        return true;
    }

    bool SetParameters(const wxString& parms)
    {
        wxFileConfig::SetPath(wxT("/"));

        auto parsed = wxCmdLineParser::ConvertStringToArgs(parms);

        for (size_t i = 0, cnt = parsed.size(); i < cnt; i++) {
            wxString key = parsed[i].BeforeFirst(wxT('=')).Trim(false).Trim(true);
            wxString val = parsed[i].AfterFirst(wxT('=')).Trim(false).Trim(true);

            if (!wxFileConfig::Write(key, Unescape(val))) {
                return false;
            }
        }

        return true;
    }

    static wxString NormalizeName(const wxString& name)
    {
        wxString cleaned = name;

        cleaned.Trim(true).Trim(false);
        cleaned.Replace(wxT(" "), wxT("_"));
        cleaned.Replace(wxT("/"), wxT("_"));
        cleaned.Replace(wxT("\\"), wxT("_"));
        cleaned.Replace(wxT(":"), wxT("_"));
        cleaned.Replace(wxT("="), wxT("_"));

        return cleaned;
    }

    wxString Escape(wxString val)
    {
        val.Replace(wxT("\\"), wxT("\\\\"), true);
        val.Replace(wxT("\""), wxT("\\\""), true);
        val.Replace(wxT("\n"), wxT("\\n"), true);

        return val;
    }

    wxString Unescape(wxString val)
    {
        val.Replace(wxT("\\n"), wxT("\n"), true);
        val.Replace(wxT("\\\""), wxT("\""), true);
        val.Replace(wxT("\\\\"), wxT("\\"), true);

        return val;
    }
};

#endif
