/**********************************************************************

  Audacity: A Digital Audio Editor

  ShuttleGetDefinition.cpp

  Paul Licameli split this out of Shuttle.cpp

**********************************************************************/

#include "ShuttleGetDefinition.h"
#include "ComponentInterfaceSymbol.h"

bool ShuttleGetDefinition::IsOptional()
{
    bool result = pOptionalFlag != NULL;
    pOptionalFlag = NULL;
    return result;
}

// Definition distinguishes optional from not.
ConstSettingsVisitor& ShuttleGetDefinition::Optional(const bool& var)
{
    pOptionalFlag = &var;
    return *this;
}

ShuttleGetDefinition::ShuttleGetDefinition(CommandMessageTarget& target)
    : CommandMessageTargetDecorator{target}
{
}

// JSON definitions.
void ShuttleGetDefinition::Define(bool, const wxChar* key,
                                  bool vdefault, bool, bool, bool)
{
    StartStruct();
    AddItem(wxString(key), "key");
    AddItem("bool", "type");
    if (IsOptional()) {
        AddItem("unchanged", "default");
    } else {
        AddItem(vdefault ? "True" : "False", "default");
    }
    EndStruct();
}

void ShuttleGetDefinition::Define(int, const wxChar* key,
                                  int vdefault, int, int, int)
{
    StartStruct();
    AddItem(wxString(key), "key");
    AddItem("int", "type");
    if (IsOptional()) {
        AddItem("unchanged", "default");
    } else {
        AddItem((double)vdefault, "default");
    }
    EndStruct();
}

void ShuttleGetDefinition::Define(size_t, const wxChar* key,
                                  int vdefault, int, int, int)
{
    StartStruct();
    AddItem(wxString(key), "key");
    AddItem("size_t", "type");
    if (IsOptional()) {
        AddItem("unchanged", "default");
    } else {
        AddItem((double)vdefault, "default");
    }
    EndStruct();
}

void ShuttleGetDefinition::Define(float, const wxChar* key,
                                  float vdefault, float, float, float)
{
    StartStruct();
    AddItem(wxString(key), "key");
    AddItem("float", "type");
    if (IsOptional()) {
        AddItem("unchanged", "default");
    } else {
        AddItem((double)vdefault, "default");
    }
    EndStruct();
}

void ShuttleGetDefinition::Define(double, const wxChar* key,
                                  float vdefault, float, float, float)
{
    StartStruct();
    AddItem(wxString(key), "key");
    AddItem("float", "type");
    if (IsOptional()) {
        AddItem("unchanged", "default");
    } else {
        AddItem((double)vdefault, "default");
    }
    EndStruct();
}

void ShuttleGetDefinition::Define(double, const wxChar* key,
                                  double vdefault, double, double, double)
{
    StartStruct();
    AddItem(wxString(key), "key");
    AddItem("double", "type");
    if (IsOptional()) {
        AddItem("unchanged", "default");
    } else {
        AddItem((double)vdefault, "default");
    }
    EndStruct();
}

void ShuttleGetDefinition::Define(const wxString&, const wxChar* key,
                                  wxString vdefault, wxString, wxString, wxString)
{
    StartStruct();
    AddItem(wxString(key), "key");
    AddItem("string", "type");
    if (IsOptional()) {
        AddItem("unchanged", "default");
    } else {
        AddItem(vdefault, "default");
    }
    EndStruct();
}

void ShuttleGetDefinition::DefineEnum(int, const wxChar* key,
                                      int vdefault, const EnumValueSymbol strings[], size_t nStrings)
{
    StartStruct();
    AddItem(wxString(key), "key");
    AddItem("enum", "type");
    if (IsOptional()) {
        AddItem("unchanged", "default");
    } else {
        AddItem(strings[vdefault].Internal(), "default");
    }
    StartField("enum");
    StartArray();
    for ( size_t i = 0; i < nStrings; i++ ) {
        AddItem(strings[i].Internal());
    }
    EndArray();
    EndField();
    EndStruct();
}
