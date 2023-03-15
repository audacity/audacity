/**********************************************************************

  WrappedType.h

  James Crook
  (C) Audacity Developers, 2007

  wxWidgets license. See Licensing.txt

*************************************************************************/

#ifndef __WRAPPED_TYPE__
#define __WRAPPED_TYPE__

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

class COMMAND_PARAMETERS_API WrappedType
{
public:

   explicit WrappedType( wxString & InStr )
      : eWrappedType{ eWrappedString }, mpStr{ &InStr }
   {}
   explicit WrappedType( int & InInt )
      : eWrappedType{ eWrappedInt }, mpInt{ &InInt }
   {}
   explicit WrappedType( double & InDouble )
      : eWrappedType{ eWrappedDouble }, mpDouble{ &InDouble }
   {}
   explicit WrappedType( bool & InBool )
      : eWrappedType{ eWrappedBool }, mpBool{ &InBool }
   {}
   explicit WrappedType()
      : eWrappedType{ eWrappedNotSet }
   {}

   bool IsString();

   wxString ReadAsString();
   int ReadAsInt();
   double ReadAsDouble();
   bool ReadAsBool();

   void WriteToAsString( const wxString & InStr);
   void WriteToAsInt( const int InInt);
   void WriteToAsDouble( const double InDouble);
   void WriteToAsBool( const bool InBool);

public :

   const teWrappedType eWrappedType;
   wxString *const mpStr {};
   int *const mpInt {};
   double *const mpDouble {};
   bool *const mpBool {};

};

#endif
