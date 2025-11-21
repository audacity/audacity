/*!********************************************************************

Audacity: A Digital Audio Editor

@file TrackAttachment.h
@brief abstract base class for structures that user interface associates with tracks

Paul Licameli split from CommonTrackPanelCell.h

**********************************************************************/

#ifndef __AUDACITY_TRACK_ATTACHMENT__
#define __AUDACITY_TRACK_ATTACHMENT__

#include <wx/chartype.h>
#include <memory>

#include "ClientData.h"

class Track;
class XMLAttributeValueView;
class XMLWriter;

class TRACK_API TrackAttachment /* not final */ : public ClientData::Base
{
public:
    virtual ~TrackAttachment();

    //! Copy state, for undo/redo purposes
    //*! The default does nothing */
    virtual void CopyTo(Track& track) const;

    //! Object may be shared among tracks but hold a special back-pointer to one of them; reassign it
    /*! default does nothing */
    virtual void Reparent(const std::shared_ptr<Track>& parent);

    //! Serialize persistent attributes
    /*! default does nothing */
    virtual void WriteXMLAttributes(XMLWriter&) const;

    //! Deserialize an attribute, returning true if recognized
    /*! default recognizes no attributes, and returns false */
    virtual bool HandleXMLAttribute(
        const std::string_view& attr, const XMLAttributeValueView& valueView);
};

#endif
