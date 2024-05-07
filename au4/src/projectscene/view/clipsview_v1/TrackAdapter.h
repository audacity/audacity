#pragma once

#include <QQmlEngine>

class Track;

class TrackAdapterBase : public QObject
{
    Q_OBJECT
    QML_ELEMENT
                                QML_UNCREATABLE("")
public:

    using QObject::QObject;

    template<typename TrackType>
    static
    std::enable_if_t<!std::is_abstract_v<TrackType>, TrackAdapterBase*>
    Create(TrackType& track, QObject* parent = nullptr);

    template<typename TrackType>
    static
    std::enable_if_t<std::is_abstract_v<TrackType>, TrackAdapterBase*>
    Create(TrackType& track, QObject* parent = nullptr) { return nullptr; }

    TrackAdapterBase(const TrackAdapterBase&) = delete;
    TrackAdapterBase& operator=(const TrackAdapterBase&) = delete;
    TrackAdapterBase(TrackAdapterBase&&) = delete;
    TrackAdapterBase& operator=(TrackAdapterBase&&) = delete;

    ~TrackAdapterBase() override;

    virtual Track* GetTrack() = 0;
    virtual QString GetType() = 0;
};
