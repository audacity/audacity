#ifndef TRACK_TYPE_H
#define TRACK_TYPE_H

#include <QtCore/QtCore>
#include <QtQml/qqmlregistration.h>

class TrackType
{
   Q_GADGET
   QML_NAMED_ELEMENT(TrackType)
   QML_UNCREATABLE("")

public:
   enum class Type {
      Mono,
      Stereo,
      Video,
      Label
   };

   Q_ENUM(Type)
};

#endif // TRACK_TYPE_H
