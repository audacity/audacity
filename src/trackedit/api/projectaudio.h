/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <cstddef>
#include <cstdint>
#include <map>
#include <memory>
#include <vector>

#include <QObject>
#include <QVariantList>
#include <QVariantMap>

#include "au3-math/SampleCount.h"

#include "internal/projecteditstate.h"

class Mixer;
class WaveTrack;

namespace muse::extensions {
class ByteBuffer;
}

namespace au::trackedit::api {
struct ProjectAudioClip;

namespace detail {
class AudioChunkObject final : public QObject
{
    Q_OBJECT
    Q_PROPERTY(uint channelCount READ channelCount CONSTANT)
    Q_PROPERTY(qulonglong sampleCount READ sampleCount CONSTANT)
    Q_PROPERTY(double sampleRate READ sampleRate CONSTANT)
    Q_PROPERTY(QVariantList channels READ channels CONSTANT)

public:
    AudioChunkObject(AudioFormat format, size_t samples, QObject* parent);

    uint channelCount() const;
    qulonglong sampleCount() const;
    double sampleRate() const;
    QVariantList channels() const;
    const AudioFormat& format() const;
    muse::extensions::ByteBuffer* buffer(size_t channel) const;
    bool released() const;
    bool reusable() const;
    void reuse();

    Q_INVOKABLE void release();

private:
    AudioFormat m_format;
    size_t m_samples = 0;
    std::vector<muse::extensions::ByteBuffer*> m_buffers;
    QVariantList m_channels;
    bool m_released = false;
};

class AudioReaderObject final : public QObject
{
    Q_OBJECT
    Q_PROPERTY(uint channelCount READ channelCount CONSTANT)
    Q_PROPERTY(double sampleRate READ sampleRate CONSTANT)

public:
    AudioReaderObject(std::shared_ptr<EditState> state, std::shared_ptr<WaveTrack> source, AudioFormat format, double start, double end,
                      QObject* parent);
    ~AudioReaderObject() override;

    uint channelCount() const;
    double sampleRate() const;

    Q_INVOKABLE QObject* read();
    Q_INVOKABLE QObject* read(qulonglong requested);

private:
    std::shared_ptr<EditState> m_state;
    std::shared_ptr<WaveTrack> m_source;
    AudioFormat m_format;
    sampleCount m_position{ 0 };
    sampleCount m_end{ 0 };
    std::unique_ptr<Mixer> m_mixer;
    AudioChunkObject* m_chunk = nullptr;
};

class AudioClipObject final : public QObject
{
    Q_OBJECT
    Q_PROPERTY(double start READ start CONSTANT)
    Q_PROPERTY(double end READ end CONSTANT)
    Q_PROPERTY(QString title READ title CONSTANT)

public:
    AudioClipObject(std::shared_ptr<EditState> state, size_t trackIndex, size_t clipIndex, QObject* parent);

    double start() const;
    double end() const;
    QString title() const;

    Q_INVOKABLE bool updateTitle(const QString& title);

private:
    const ProjectAudioClip& source() const;

    std::shared_ptr<EditState> m_state;
    size_t m_trackIndex = 0;
    size_t m_clipIndex = 0;
};

class AudioTrackObject final : public QObject
{
    Q_OBJECT
    Q_PROPERTY(QString name READ name CONSTANT)
    Q_PROPERTY(uint channelCount READ channelCount CONSTANT)
    Q_PROPERTY(double sampleRate READ sampleRate CONSTANT)
    Q_PROPERTY(double start READ start CONSTANT)
    Q_PROPERTY(double end READ end CONSTANT)
    Q_PROPERTY(QVariantList clips READ clips CONSTANT)

public:
    AudioTrackObject(std::shared_ptr<EditState> state, size_t index, bool selectionOnly, QObject* parent);

    QString name() const;
    uint channelCount() const;
    double sampleRate() const;
    double start() const;
    double end() const;
    QVariantList clips() const;
    size_t index() const;
    const std::shared_ptr<EditState>& state() const;

    Q_INVOKABLE QObject* openReader(const QVariantMap& options);

private:
    const ProjectAudioTrack& input() const;

    std::shared_ptr<EditState> m_state;
    size_t m_index = 0;
    bool m_selectionOnly = false;
    mutable std::map<size_t, QObject*> m_clipWrappers;
};

class AudioWriterObject final : public QObject
{
    Q_OBJECT
    Q_PROPERTY(uint channelCount READ channelCount CONSTANT)
    Q_PROPERTY(double sampleRate READ sampleRate CONSTANT)

public:
    AudioWriterObject(std::shared_ptr<EditState> state, AudioFormat format, std::shared_ptr<WaveTrack> audio, QObject* parent);

    uint channelCount() const;
    double sampleRate() const;

    Q_INVOKABLE QObject* createChunk(qulonglong samples);
    Q_INVOKABLE bool write(QObject* object);
    Q_INVOKABLE bool replace(QObject* object, double start, double end);
    Q_INVOKABLE bool addTrack(const QString& name, double start);

private:
    void route();

    std::shared_ptr<EditState> m_state;
    AudioFormat m_format;
    std::shared_ptr<WaveTrack> m_audio;
    bool m_routed = false;
};
} // namespace detail
} // namespace au::trackedit::api
