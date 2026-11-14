/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <map>
#include <memory>

#include <QObject>
#include <QVariantList>
#include <QVariantMap>

namespace au::trackedit::api::detail {
struct EditState;

class ProjectSelectionObject final : public QObject
{
    Q_OBJECT
    Q_PROPERTY(double start READ start)
    Q_PROPERTY(double end READ end)
    Q_PROPERTY(double duration READ duration)
    Q_PROPERTY(QVariantList audioTracks READ audioTracks)
    Q_PROPERTY(QVariantList labelTracks READ labelTracks)

public:
    explicit ProjectSelectionObject(std::shared_ptr<EditState> state, QObject* parent);

    void refresh();

    double start() const;
    double end() const;
    double duration() const;
    QVariantList audioTracks() const;
    QVariantList labelTracks() const;

private:
    std::shared_ptr<EditState> m_state;
    QVariantList m_audioTracks;
    QVariantList m_labelTracks;
    std::map<size_t, QObject*> m_audioWrappers;
    std::map<size_t, QObject*> m_labelWrappers;
};

class ProjectEditObject final : public QObject
{
    Q_OBJECT
    Q_PROPERTY(QObject * selection READ selection CONSTANT)
    Q_PROPERTY(QVariantList audioTracks READ audioTracks CONSTANT)
    Q_PROPERTY(QVariantList labelTracks READ labelTracks CONSTANT)

public:
    ProjectEditObject(std::shared_ptr<EditState> state, QObject* parent);
    ~ProjectEditObject() override;

    QObject* selection() const;
    QVariantList audioTracks() const;
    QVariantList labelTracks() const;

    Q_INVOKABLE bool setSelection(const QVariantMap& value);
    Q_INVOKABLE QObject* createAudioWriter(const QVariantMap& value);
    Q_INVOKABLE bool transformAudio(QObject* track, const QVariantMap& options, QObject* processor, QObject* task);
    Q_INVOKABLE QObject* createLabelTrack(const QString& name);
    Q_INVOKABLE void commit();
    Q_INVOKABLE void abort();

private:
    void abandon();

    std::shared_ptr<EditState> m_state;
    ProjectSelectionObject* m_selection = nullptr;
    QVariantList m_audioTracks;
    QVariantList m_labelTracks;
};
} // namespace au::trackedit::api::detail
