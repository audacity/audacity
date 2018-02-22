/**********************************************************************

  Audacity: A Digital Audio Editor

  Shuttle.h

  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_SHUTTLE__
#define __AUDACITY_SHUTTLE__



class Enums {
public:
   static const wxString * GetDbChoices();
   static const int    NumDbChoices;
   static const double Db2Signal[];
   static const wxString DbChoices[];
};

class WrappedType;

class Shuttle /* not final */ {
 public:
   // constructors and destructors
   Shuttle();
   virtual ~Shuttle() {}

 public:
   bool mbStoreInClient;
   wxString mValueString;
   // Even though virtual, mostly the transfer functions won't change
   // for special kinds of archive.
   virtual bool TransferBool( const wxString & Name, bool & bValue, const bool & bDefault );
   virtual bool TransferFloat( const wxString & Name, float & fValue, const float &fDefault );
   virtual bool TransferDouble( const wxString & Name, double & dValue, const double &dDefault );
   virtual bool TransferInt( const wxString & Name, int & iValue, const int &iDefault );
   virtual bool TransferInt( const wxString & Name, wxLongLong_t & iValue, const wxLongLong_t &iDefault );
   virtual bool TransferLongLong( const wxString & Name, wxLongLong_t & iValue, const wxLongLong_t &iDefault );
   virtual bool TransferString( const wxString & Name, wxString & strValue, const wxString &strDefault );
   virtual bool TransferEnum( const wxString & Name, int & iValue,
      const int nChoices, const wxString * pFirstStr);
   virtual bool TransferWrappedType( const wxString & Name, WrappedType & W );
   // We expect the ExchangeWithMaster function to change from one type of
   // archive to another.
   virtual bool ExchangeWithMaster(const wxString & Name);
};

class ShuttleCli final : public Shuttle
{
public:
   wxString mParams;
   ShuttleCli(){ mParams = wxT("") ;}
   virtual ~ShuttleCli() {}
   bool ExchangeWithMaster(const wxString & Name) override;
};

#endif
