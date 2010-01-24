/***************************************************************************
    copyright            : (C) 2002 - 2008 by Scott Wheeler
    email                : wheeler@kde.org
 ***************************************************************************/

/***************************************************************************
 *   This library is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License version   *
 *   2.1 as published by the Free Software Foundation.                     *
 *                                                                         *
 *   This library is distributed in the hope that it will be useful, but   *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   Lesser General Public License for more details.                       *
 *                                                                         *
 *   You should have received a copy of the GNU Lesser General Public      *
 *   License along with this library; if not, write to the Free Software   *
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  *
 *   USA                                                                   *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#ifndef TAGLIB_RELATIVEVOLUMEFRAME_H
#define TAGLIB_RELATIVEVOLUMEFRAME_H

#include <tlist.h>
#include <id3v2frame.h>
#include "taglib_export.h"

namespace TagLib {

  namespace ID3v2 {

    //! An ID3v2 relative volume adjustment frame implementation

    /*!
     * This is an implementation of ID3v2 relative volume adjustment.  The
     * presence of this frame makes it possible to specify an increase in volume
     * for an audio file or specific audio tracks in that file.
     *
     * Multiple relative volume adjustment frames may be present in the tag
     * each with a unique identification and describing volume adjustment for
     * different channel types.
     */

    class TAGLIB_EXPORT RelativeVolumeFrame : public Frame
    {
      friend class FrameFactory;

    public:

      /*!
       * This indicates the type of volume adjustment that should be applied.
       */
      enum ChannelType {
        //! A type not enumerated below
        Other        = 0x00,
        //! The master volume for the track
        MasterVolume = 0x01,
        //! The front right audio channel
        FrontRight   = 0x02,
        //! The front left audio channel
        FrontLeft    = 0x03,
        //! The back right audio channel
        BackRight    = 0x04,
        //! The back left audio channel
        BackLeft     = 0x05,
        //! The front center audio channel
        FrontCentre  = 0x06,
        //! The back center audio channel
        BackCentre   = 0x07,
        //! The subwoofer audio channel
        Subwoofer    = 0x08
      };

      //! Struct that stores the relevant values for ID3v2 peak volume

      /*!
       * The peak volume is described as a series of bits that is padded to fill
       * a block of bytes.  These two values should always be updated in tandem.
       */
      struct PeakVolume
      {
        /*!
         * Constructs an empty peak volume description.
         */
        PeakVolume() : bitsRepresentingPeak(0) {}
        /*!
         * The number of bits (in the range of 0 to 255) used to describe the
         * peak volume.
         */
        unsigned char bitsRepresentingPeak;
        /*!
         * The array of bits (represented as a series of bytes) used to describe
         * the peak volume.
         */
        ByteVector peakVolume;
      };

      /*!
       * Constructs a RelativeVolumeFrame.  The relevant data should be set
       * manually.
       */
      RelativeVolumeFrame();

      /*!
       * Constructs a RelativeVolumeFrame based on the contents of \a data.
       */
      RelativeVolumeFrame(const ByteVector &data);

      /*!
       * Destroys the RelativeVolumeFrame instance.
       */
      virtual ~RelativeVolumeFrame();

      /*!
       * Returns the frame's identification.
       *
       * \see identification()
       */
      virtual String toString() const;

      /*!
       * Returns a list of channels with information currently in the frame.
       */
      List<ChannelType> channels() const;

      /*!
       * \deprecated Always returns master volume.
       */
      ChannelType channelType() const;

      /*!
       * \deprecated This method no longer has any effect.
       */
      void setChannelType(ChannelType t);

      /*
       * There was a terrible API goof here, and while this can't be changed to
       * the way it appears below for binary compaibility reasons, let's at
       * least pretend that it looks clean.
       */

#ifdef DOXYGEN

      /*!
       * Returns the relative volume adjustment "index".  As indicated by the
       * ID3v2 standard this is a 16-bit signed integer that reflects the
       * decibils of adjustment when divided by 512.
       *
       * This defaults to returning the value for the master volume channel if
       * available and returns 0 if the specified channel does not exist.
       *
       * \see setVolumeAdjustmentIndex()
       * \see volumeAjustment()
       */
      short volumeAdjustmentIndex(ChannelType type = MasterVolume) const;

      /*!
       * Set the volume adjustment to \a index.  As indicated by the ID3v2
       * standard this is a 16-bit signed integer that reflects the decibils of
       * adjustment when divided by 512.
       *
       * By default this sets the value for the master volume.
       *
       * \see volumeAdjustmentIndex()
       * \see setVolumeAjustment()
       */
      void setVolumeAdjustmentIndex(short index, ChannelType type = MasterVolume);

      /*!
       * Returns the relative volume adjustment in decibels.
       *
       * \note Because this is actually stored internally as an "index" to this
       * value the value returned by this method may not be identical to the
       * value set using setVolumeAdjustment().
       *
       * This defaults to returning the value for the master volume channel if
       * available and returns 0 if the specified channel does not exist.
       *
       * \see setVolumeAdjustment()
       * \see volumeAdjustmentIndex()
       */
      float volumeAdjustment(ChannelType type = MasterVolume) const;

      /*!
       * Set the relative volume adjustment in decibels to \a adjustment.
       *
       * By default this sets the value for the master volume.
       *
       * \note Because this is actually stored internally as an "index" to this
       * value the value set by this method may not be identical to the one
       * returned by volumeAdjustment().
       *
       * \see setVolumeAdjustment()
       * \see volumeAdjustmentIndex()
       */
      void setVolumeAdjustment(float adjustment, ChannelType type = MasterVolume);

      /*!
       * Returns the peak volume (represented as a length and a string of bits).
       *
       * This defaults to returning the value for the master volume channel if
       * available and returns 0 if the specified channel does not exist.
       *
       * \see setPeakVolume()
       */
      PeakVolume peakVolume(ChannelType type = MasterVolume) const;

      /*!
       * Sets the peak volume to \a peak.
       *
       * By default this sets the value for the master volume.
       *
       * \see peakVolume()
       */
      void setPeakVolume(const PeakVolume &peak, ChannelType type = MasterVolume);

#else

      // BIC: Combine each of the following pairs of functions (or maybe just
      // rework this junk altogether).

      short volumeAdjustmentIndex(ChannelType type) const;
      short volumeAdjustmentIndex() const;

      void setVolumeAdjustmentIndex(short index, ChannelType type);
      void setVolumeAdjustmentIndex(short index);

      float volumeAdjustment(ChannelType type) const;
      float volumeAdjustment() const;

      void setVolumeAdjustment(float adjustment, ChannelType type);
      void setVolumeAdjustment(float adjustment);

      PeakVolume peakVolume(ChannelType type) const;
      PeakVolume peakVolume() const;

      void setPeakVolume(const PeakVolume &peak, ChannelType type);
      void setPeakVolume(const PeakVolume &peak);

#endif

      /*!
       * Returns the identification for this frame.
       */
      String identification() const;

      /*!
       * Sets the identification of the frame to \a s. The string
       * is used to identify the situation and/or device where this
       * adjustment should apply.
       */
      void setIdentification(const String &s);

    protected:
      virtual void parseFields(const ByteVector &data);
      virtual ByteVector renderFields() const;

    private:
      RelativeVolumeFrame(const ByteVector &data, Header *h);
      RelativeVolumeFrame(const RelativeVolumeFrame &);
      RelativeVolumeFrame &operator=(const RelativeVolumeFrame &);

      class RelativeVolumeFramePrivate;
      RelativeVolumeFramePrivate *d;
    };

  }
}
#endif
