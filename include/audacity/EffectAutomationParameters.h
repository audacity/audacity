/**********************************************************************

   Audacity: A Digital Audio Editor

   EffectAutomationParameters.h

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

#ifndef __AUDACITY_EFFECTAUTOMATIONPARAMETERS_H__
#define __AUDACITY_EFFECTAUTOMATIONPARAMETERS_H__

#include <locale.h>

#include <wx/cmdline.h>
#include <wx/fileconf.h>
#include <wx/intl.h>

class EffectAutomationParameters : public wxFileConfig
{
public:
   EffectAutomationParameters(const wxString & parms = wxEmptyString)
   :  wxFileConfig(wxEmptyString,
                   wxEmptyString,
                   wxEmptyString,
                   wxEmptyString,
                   0)
   {
      SetParameters(parms);
   }

   virtual ~EffectAutomationParameters()
   {
   }

   virtual bool HasGroup(const wxString & strName) const
   {
      return wxFileConfig::HasGroup(NormalizeName(strName));
   }

   virtual bool HasEntry(const wxString& strName) const
   {
      return wxFileConfig::HasEntry(NormalizeName(strName));
   }

   virtual bool DoReadString(const wxString & key, wxString *pStr) const
   {
      return wxFileConfig::DoReadString(NormalizeName(key), pStr);
   }

   virtual bool DoReadLong(const wxString & key, long *pl) const
   {
      return wxFileConfig::DoReadLong(NormalizeName(key), pl);
   }

   virtual bool DoReadDouble(const wxString & key, double *pd) const
   {
      wxString str;
      if (Read(key, &str))
      {
         struct lconv *info = localeconv();
         wxString dec = info ? wxString::FromUTF8(info->decimal_point) : wxT(".");

         str.Replace(wxT(","), dec);
         str.Replace(wxT("."), dec);

         return str.ToDouble(pd);
      }

      return false;
   }

   virtual bool DoWriteString(const wxString & key, const wxString & szValue)
   {
      return wxFileConfig::DoWriteString(NormalizeName(key), szValue);
   }

   virtual bool DoWriteLong(const wxString & key, long lValue)
   {
      return wxFileConfig::DoWriteLong(NormalizeName(key), lValue);
   }

   virtual bool DoWriteDouble(const wxString & key, double value)
   {
      return DoWriteString(key, wxString::Format(wxT("%.12f"), value));
   }

   bool ReadFloat(const wxString & key, float *pf) const
   {
      double d = *pf;
      bool success = Read(key, &d);
      if (success)
      {
         *pf = (float) d;
      }
      return success;
   }

   bool ReadFloat(const wxString & key, float *pf, float defVal) const
   {
      if (!ReadFloat(key, pf))
      {
         *pf = defVal;
      }
      return true;
   }

   bool WriteFloat(const wxString & key, float f)
   {
      return Write(key, f);
   }

   bool ReadEnum(const wxString & key, int *pi, const wxArrayString & choices) const
   {
      wxString s;
      if (!wxFileConfig::Read(key, &s))
      {
         return false;
      }
      *pi = choices.Index(s);
      return true;
   }

   bool ReadEnum(const wxString & key, int *pi, int defVal, const wxArrayString & choices) const
   {
      if (!ReadEnum(key, pi, choices))
      {
         *pi = defVal;
      }
      return true;
   }

   bool ReadEnum(const wxString & key, int *pi, const wxString & defVal, const wxArrayString & choices) const
   {
      if (!ReadEnum(key, pi, choices))
      {
         *pi = choices.Index(defVal);
      }
      return true;
   }

   bool WriteEnum(const wxString & key, int value, const wxArrayString & choices)
   {
      if (value < 0 || value >= (int) choices.GetCount())
      {
         return false;
      }

      return wxFileConfig::Write(key, choices[value]);
   }

   bool ReadAndVerify(const wxString & key, float *val, float defVal, float min, float max) const
   {
      ReadFloat(key, val, defVal);
      return (*val >= min && *val <= max);
   }

   bool ReadAndVerify(const wxString & key, double *val, double defVal, double min, double max) const
   {
      Read(key, val, defVal);
      return (*val >= min && *val <= max);
   }

   bool ReadAndVerify(const wxString & key, int *val, int defVal, int min, int max) const
   {
      Read(key, val, defVal);
      return (*val >= min && *val <= max);
   }

   bool ReadAndVerify(const wxString & key, long *val, long defVal, long min, long max) const
   {
      Read(key, val, defVal);
      return (*val >= min && *val <= max);
   }

   bool ReadAndVerify(const wxString & key, bool *val, bool defVal) const
   {
      Read(key, val, defVal);
      return true;
   }

   bool ReadAndVerify(const wxString & key, wxString *val, const wxString & defVal) const
   {
      Read(key, val, defVal);
      return true;
   }

   bool ReadAndVerify(const wxString & key, int *val, int defVal, const wxArrayString & choices) const
   {
      ReadEnum(key, val, defVal, choices);
      return (*val != wxNOT_FOUND);
   }

   bool ReadAndVerify(const wxString & key, int *val, const wxString & defVal, const wxArrayString & choices) const
   {
      ReadEnum(key, val, defVal, choices);
      return (*val != wxNOT_FOUND);
   }

   bool GetParameters(wxString & parms)
   {
      wxFileConfig::SetPath(wxT("/"));

      wxString str;
      wxString key;

      long ndx = 0;
      bool res = wxFileConfig::GetFirstEntry(key, ndx);
      while (res)
      {
         wxString val;
         if (!wxFileConfig::Read(key, &val))
         {
            return false;
         }

         str += key + wxT("=\"") + Escape(val) + wxT("\" ");

         res = wxFileConfig::GetNextEntry(key, ndx);
      }
      str.Trim();

      parms = str;

      return true;
   }

   bool SetParameters(const wxString & parms)
   {
      wxFileConfig::SetPath(wxT("/"));

      wxArrayString parsed = wxCmdLineParser::ConvertStringToArgs(parms);

      for (size_t i = 0, cnt = parsed.GetCount(); i < cnt; i++)
      {
         wxString key = parsed[i].BeforeFirst(wxT('=')).Trim(false).Trim(true);
         wxString val = parsed[i].AfterFirst(wxT('=')).Trim(false).Trim(true);

         if (!wxFileConfig::Write(key, Unescape(val)))
         {
            return false;
         }
      }

      return true;
   }

   wxString NormalizeName(const wxString & name) const
   {
      wxString cleaned = name;

      cleaned.Trim(true).Trim(false);
      cleaned.Replace(wxT(" "), wxT("_"));
      cleaned.Replace(wxT("/"), wxT("_"));
      cleaned.Replace(wxT("\\"), wxT("_"));
      cleaned.Replace(wxT(":"), wxT("_"));

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
