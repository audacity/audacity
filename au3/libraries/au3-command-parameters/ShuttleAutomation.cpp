/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ShuttleAutomation.cpp

  Paul Licameli split from Shuttle.cpp

**********************************************************************/

#include "ShuttleAutomation.h"
#include "EffectAutomationParameters.h"

EffectParameterMethods::~EffectParameterMethods() = default;

// ShuttleGetAutomation gets from the shuttle into typically a string.
ConstSettingsVisitor& ShuttleGetAutomation::Optional(const bool& var)
{
    pOptionalFlag = &var;
    return *this;
}

void ShuttleGetAutomation::Define(bool var, const wxChar* key,
                                  bool, bool, bool, bool)
{
    if (!ShouldSet()) {
        return;
    }
    mpEap->Write(key, var);
}

void ShuttleGetAutomation::Define(int var, const wxChar* key,
                                  int, int, int, int)
{
    if (!ShouldSet()) {
        return;
    }
    mpEap->Write(key, var);
}

void ShuttleGetAutomation::Define(size_t var, const wxChar* key,
                                  int, int, int, int)
{
    if (!ShouldSet()) {
        return;
    }
    mpEap->Write(key, (int)var);
}

void ShuttleGetAutomation::Define(double var, const wxChar* key,
                                  float, float, float, float)
{
    if (!ShouldSet()) {
        return;
    }
    mpEap->WriteFloat(key, var);
}

void ShuttleGetAutomation::Define(float var, const wxChar* key,
                                  float, float, float, float)
{
    if (!ShouldSet()) {
        return;
    }
    mpEap->WriteFloat(key, var);
}

void ShuttleGetAutomation::Define(double var, const wxChar* key,
                                  double, double, double, double)
{
    if (!ShouldSet()) {
        return;
    }
    mpEap->Write(key, var);
}

void ShuttleGetAutomation::Define(const wxString& var, const wxChar* key,
                                  wxString, wxString, wxString, wxString)
{
    if (!ShouldSet()) {
        return;
    }
    mpEap->Write(key, var);
}

void ShuttleGetAutomation::DefineEnum(int var, const wxChar* key,
                                      int, const EnumValueSymbol strings[], size_t)
{
    if (!ShouldSet()) {
        return;
    }
    mpEap->Write(key, strings[var].Internal());
}

SettingsVisitor& ShuttleSetAutomation::Optional(bool& var)
{
    pOptionalFlag = &var;
    return *this;
}

// Tests for parameter being optional.
// Prepares for next parameter by clearing the pointer.
// If the parameter is optional, finds out if it was actually provided.
// i.e. could it be got from a macro?
// The result goes into the flag variable, so we typically ignore the result.
bool ShuttleSetAutomation::CouldGet(const wxString& key)
{
    // Not optional?  Can get as we will get the default, at worst.
    if (!pOptionalFlag) {
        return true;
    }
    bool result = mpEap->HasEntry(key);
    *pOptionalFlag = result;
    pOptionalFlag = NULL;
    return result;
}

void ShuttleSetAutomation::Define(bool& var,     const wxChar* key, const bool vdefault, const bool vmin, const bool vmax, const bool vscl)
{
    CouldGet(key);
    if (!bOK) {
        return;
    }
    // Use of temp in this and related functions is to handle the case of
    // only committing values if all values pass verification.
    bool temp =var;
    bOK = mpEap->ReadAndVerify(key, &temp, vdefault);
    if (bWrite && bOK) {
        var = temp;
    }
}

void ShuttleSetAutomation::Define(int& var,      const wxChar* key, const int vdefault, const int vmin, const int vmax, const int vscl)
{
    CouldGet(key);
    if (!bOK) {
        return;
    }
    int temp =var;
    bOK = mpEap->ReadAndVerify(key, &temp, vdefault, vmin, vmax);
    if (bWrite && bOK) {
        var = temp;
    }
}

void ShuttleSetAutomation::Define(size_t& var,      const wxChar* key, const int vdefault, const int vmin, const int vmax, const int vscl)
{
    CouldGet(key);
    if (!bOK) {
        return;
    }
    int temp = var;
    bOK = mpEap->ReadAndVerify(key, &temp, vdefault, vmin, vmax);
    if (bWrite && bOK) {
        var = temp;
    }
}

void ShuttleSetAutomation::Define(float& var,   const wxChar* key, const float vdefault, const float vmin, const float vmax,
                                  const float vscl)
{
    CouldGet(key);
    if (!bOK) {
        return;
    }
    float temp = var;
    bOK = mpEap->ReadAndVerify(key, &temp, vdefault, vmin, vmax);
    if (bWrite && bOK) {
        var = temp;
    }
}

void ShuttleSetAutomation::Define(double& var,   const wxChar* key, const float vdefault, const float vmin, const float vmax,
                                  const float vscl)
{
    CouldGet(key);
    if (!bOK) {
        return;
    }
    double temp = var;
    bOK = mpEap->ReadAndVerify(key, &temp, vdefault, vmin, vmax);
    if (bWrite && bOK) {
        var = temp;
    }
}

void ShuttleSetAutomation::Define(double& var,   const wxChar* key, const double vdefault, const double vmin, const double vmax,
                                  const double vscl)
{
    CouldGet(key);
    if (!bOK) {
        return;
    }
    double temp = var;
    bOK = mpEap->ReadAndVerify(key, &temp, vdefault, vmin, vmax);
    if (bWrite && bOK) {
        var = temp;
    }
}

void ShuttleSetAutomation::Define(wxString& var, const wxChar* key, const wxString vdefault, const wxString vmin, const wxString vmax,
                                  const wxString vscl)
{
    CouldGet(key);
    if (!bOK) {
        return;
    }
    wxString temp = var;
    bOK = mpEap->ReadAndVerify(key, &temp, vdefault);
    if (bWrite && bOK) {
        var = temp;
    }
}

void ShuttleSetAutomation::DefineEnum(int& var, const wxChar* key, const int vdefault, const EnumValueSymbol strings[], size_t nStrings)
{
    CouldGet(key);
    if (!bOK) {
        return;
    }
    int temp = var;
    bOK = mpEap->ReadAndVerify(key, &temp, vdefault, strings, nStrings);
    if (bWrite && bOK) {
        var = temp;
    }
}

SettingsVisitor& ShuttleDefaults::Optional(bool& var)
{
    var = true;
    pOptionalFlag = nullptr;
    return *this;
}

SettingsVisitor& ShuttleDefaults::OptionalY(bool& var)
{
    var = true;
    pOptionalFlag = nullptr;
    return *this;
}

SettingsVisitor& ShuttleDefaults::OptionalN(bool& var)
{
    var = false;
    pOptionalFlag = nullptr;
    return *this;
}

void ShuttleDefaults::Define(bool& var, const wxChar*, bool vdefault,
                             bool, bool, bool)
{
    var = vdefault;
}

void ShuttleDefaults::Define(int& var, const wxChar*, int vdefault,
                             int, int, int)
{
    var = vdefault;
}

void ShuttleDefaults::Define(size_t& var, const wxChar*, int vdefault,
                             int, int, int)
{
    var = vdefault;
}

void ShuttleDefaults::Define(float& var, const wxChar*, float vdefault,
                             float, float, float)
{
    var = vdefault;
}

void ShuttleDefaults::Define(double& var, const wxChar*, float vdefault,
                             float, float, float)
{
    var = vdefault;
}

void ShuttleDefaults::Define(double& var, const wxChar*,  double vdefault,
                             double, double, double)
{
    var = vdefault;
}

void ShuttleDefaults::Define(wxString& var, const wxChar*, wxString vdefault,
                             wxString, wxString, wxString)
{
    var = vdefault;
}

void ShuttleDefaults::DefineEnum(int& var, const wxChar*, int vdefault,
                                 const EnumValueSymbol [], size_t)
{
    var = vdefault;
}
