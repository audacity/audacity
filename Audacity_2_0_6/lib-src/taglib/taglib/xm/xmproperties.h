/***************************************************************************
    copyright           : (C) 2011 by Mathias Panzenböck
    email               : grosser.meister.morti@gmx.net
 ***************************************************************************/

/***************************************************************************
 *   This library is free software; you can redistribute it and/or modify  *
 *   it  under the terms of the GNU Lesser General Public License version  *
 *   2.1 as published by the Free Software Foundation.                     *
 *                                                                         *
 *   This library is distributed in the hope that it will be useful, but   *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   Lesser General Public License for more details.                       *
 *                                                                         *
 *   You should have received a copy of the GNU Lesser General Public      *
 *   License along with this library; if not, write to the Free Software   *
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,            *
 *   MA  02110-1301  USA                                                   *
 ***************************************************************************/

#ifndef TAGLIB_XMPROPERTIES_H
#define TAGLIB_XMPROPERTIES_H

#include "taglib.h"
#include "tstring.h"
#include "audioproperties.h"

namespace TagLib {
  namespace XM {
    class Properties : public AudioProperties {
      friend class File;
    public:
      /*! Flag bits. */
      enum {
        LinearFreqTable = 1 // otherwise its the amiga freq. table
      };

      Properties(AudioProperties::ReadStyle propertiesStyle);
      virtual ~Properties();

      int length()     const;
      int bitrate()    const;
      int sampleRate() const;
      int channels()   const;

      ushort lengthInPatterns() const;
      ushort version()          const;
      ushort restartPosition()  const;
      ushort patternCount()     const;
      ushort instrumentCount()  const;
      uint   sampleCount()      const;
      ushort flags()            const;
      ushort tempo()            const;
      ushort bpmSpeed()         const;

      void setChannels(int channels);

      void setLengthInPatterns(ushort lengthInPatterns);
      void setVersion(ushort version);
      void setRestartPosition(ushort restartPosition);
      void setPatternCount(ushort patternCount);
      void setInstrumentCount(ushort instrumentCount);
      void setSampleCount(uint sampleCount);
      void setFlags(ushort flags);
      void setTempo(ushort tempo);
      void setBpmSpeed(ushort bpmSpeed);

    private:
      Properties(const Properties&);
      Properties &operator=(const Properties&);

      class PropertiesPrivate;
      PropertiesPrivate *d;
    };
  }
}

#endif
