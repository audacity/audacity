/**********************************************************************

  Audacity: A Digital Audio Editor

  ShuttleGetDefinition.cpp

  Paul Licameli split this out of Shuttle.cpp

**********************************************************************/

#include "ShuttleGetDefinition.h"
#include "ComponentInterfaceSymbol.h"

bool ShuttleGetDefinition::IsOptional(){
   bool result = pOptionalFlag !=NULL;
   pOptionalFlag = NULL;
   return result;
}

// Definition distinguishes optional from not.
ShuttleParams & ShuttleGetDefinition::Optional( bool & var ){
   pOptionalFlag = &var;
   return *this;
};

ShuttleGetDefinition::ShuttleGetDefinition( CommandMessageTarget & target ) : CommandMessageTargetDecorator( target )
{
}

// JSON definitions.
void ShuttleGetDefinition::Define( bool & var,     const wxChar * key, const bool vdefault, const bool vmin, const bool vmax, const bool vscl )
{
   StartStruct();
   AddItem( wxString(key), "key" );
   AddItem( "bool", "type" );
   if( IsOptional() )
      AddItem( "unchanged", "default" );
   else
      AddItem( vdefault ? "True" : "False", "default" );
   EndStruct();
}

void ShuttleGetDefinition::Define( int & var,      const wxChar * key, const int vdefault, const int vmin, const int vmax, const int vscl )
{
   StartStruct();
   AddItem( wxString(key), "key" );
   AddItem( "int", "type" );
   if( IsOptional() )
      AddItem( "unchanged", "default" );
   else
      AddItem( (double)vdefault, "default"  );
   EndStruct();
}

void ShuttleGetDefinition::Define( size_t & var,      const wxChar * key, const int vdefault, const int vmin, const int vmax, const int vscl )
{
   StartStruct();
   AddItem( wxString(key), "key" );
   AddItem( "size_t", "type" );
   if( IsOptional() )
      AddItem( "unchanged", "default" );
   else
      AddItem( (double)vdefault, "default"  );
   EndStruct();
   
}

void ShuttleGetDefinition::Define( float & var,   const wxChar * key, const float vdefault, const float vmin, const float vmax, const float vscl )
{
   StartStruct();
   AddItem( wxString(key), "key" );
   AddItem( "float", "type" );
   if( IsOptional() )
      AddItem( "unchanged", "default" );
   else
      AddItem( (double)vdefault, "default"  );
   EndStruct();
}

void ShuttleGetDefinition::Define( double & var,   const wxChar * key, const float vdefault, const float vmin, const float vmax, const float vscl )
{
   StartStruct();
   AddItem( wxString(key), "key" );
   AddItem( "float", "type" );
   if( IsOptional() )
      AddItem( "unchanged", "default" );
   else
      AddItem( (double)vdefault, "default"  );
   EndStruct();
}

void ShuttleGetDefinition::Define( double & var,   const wxChar * key, const double vdefault, const double vmin, const double vmax, const double vscl )
{
   StartStruct();
   AddItem( wxString(key), "key" );
   AddItem( "double", "type" );
   if( IsOptional() )
      AddItem( "unchanged", "default" );
   else
      AddItem( (double)vdefault, "default"  );
   EndStruct();
}


void ShuttleGetDefinition::Define( wxString &var, const wxChar * key, const wxString vdefault, const wxString vmin, const wxString vmax, const wxString vscl )
{
   StartStruct();
   AddItem( wxString(key), "key" );
   AddItem( "string", "type" );
   if( IsOptional() )
      AddItem( "unchanged", "default" );
   else
      AddItem( vdefault, "default"  );
   EndStruct();
}


void ShuttleGetDefinition::DefineEnum( int &var,
                                      const wxChar * key, const int vdefault,
                                      const EnumValueSymbol strings[], size_t nStrings )
{
   StartStruct();
   AddItem( wxString(key), "key" );
   AddItem( "enum", "type" );
   if( IsOptional() )
      AddItem( "unchanged", "default" );
   else
      AddItem( strings[vdefault].Internal(), "default"  );
   StartField( "enum" );
   StartArray();
   for( size_t i = 0; i < nStrings; i++ )
      AddItem( strings[i].Internal() );
   EndArray();
   EndField();
   EndStruct();
}
