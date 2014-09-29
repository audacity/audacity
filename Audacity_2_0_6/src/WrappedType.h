/**********************************************************************

  WrappedType.h

  James Crook
  (C) Audacity Developers, 2007

  wxWidgets license. See Licensing.txt

*************************************************************************/

#ifndef __WRAPPED_TYPE__
#define __WRAPPED_TYPE__

#include "Audacity.h"

enum teWrappedType
{
   eWrappedNotSet,
   eWrappedString,
   eWrappedInt,
   eWrappedDouble,
   eWrappedBool,
   eWrappedEnum
};

class wxString;
class wxArrayString;
class ShuttlePrefs;

class AUDACITY_DLL_API WrappedType
{
public:

   WrappedType( wxString & InStr ){ SetTo(InStr);   };
   WrappedType( int & InInt ){      SetTo(InInt);   };
   WrappedType( double & InDouble ){SetTo(InDouble);};
   WrappedType( bool & InBool ){    SetTo(InBool);  };
   WrappedType(){ Init();}

   void SetTo( wxString & InStr ){ Init();mpStr=&InStr;       eWrappedType = eWrappedString;}
   void SetTo( int & InInt ){      Init();mpInt=&InInt;       eWrappedType = eWrappedInt;};
   void SetTo( double & InDouble ){Init();mpDouble=&InDouble; eWrappedType = eWrappedDouble;};
   void SetTo( bool & InBool ){    Init();mpBool=&InBool;     eWrappedType = eWrappedBool;};

   bool IsString();

   wxString ReadAsString();
   int ReadAsInt();
   double ReadAsDouble();
   bool ReadAsBool();

   void WriteToAsString( const wxString & InStr);
   void WriteToAsInt( const int InInt);
   void WriteToAsDouble( const double InDouble);
   void WriteToAsBool( const bool InBool);

   bool ValuesMatch( const WrappedType & W );
   void WriteToAsWrappedType( const WrappedType & W );

public :
   void Init();

   teWrappedType eWrappedType;
   wxString * mpStr;
   int * mpInt;
   double * mpDouble;
   bool * mpBool;

};

#endif
