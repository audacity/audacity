/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <cstddef>
#include <map>
#include <memory>

#include <QObject>
#include <QVariantList>

namespace au::trackedit::api {
struct ProjectLabel;

namespace detail {
struct EditState;

class LabelObject final : public QObject
{
    Q_OBJECT
    Q_PROPERTY(double start READ start CONSTANT)
    Q_PROPERTY(double end READ end CONSTANT)
    Q_PROPERTY(QString text READ text CONSTANT)

public:
    LabelObject(std::shared_ptr<EditState> state, size_t trackIndex, size_t labelIndex, QObject* parent);

    double start() const;
    double end() const;
    QString text() const;

    Q_INVOKABLE bool update(double start, double end, const QString& text);
    Q_INVOKABLE bool remove();

private:
    const ProjectLabel& source() const;

    std::shared_ptr<EditState> m_state;
    size_t m_trackIndex = 0;
    size_t m_labelIndex = 0;
    bool m_removed = false;
};

class LabelTrackObject final : public QObject
{
    Q_OBJECT
    Q_PROPERTY(QString name READ name CONSTANT)
    Q_PROPERTY(QVariantList labels READ labels CONSTANT)

public:
    LabelTrackObject(std::shared_ptr<EditState> state, size_t index, bool intersectingLabelsOnly, QObject* parent);

    QString name() const;
    QVariantList labels() const;
    size_t index() const;
    const std::shared_ptr<EditState>& state() const;

    Q_INVOKABLE bool addLabel(double start, double end, const QString& text);

private:
    std::shared_ptr<EditState> m_state;
    size_t m_index = 0;
    bool m_intersectingOnly = false;
    mutable std::map<size_t, QObject*> m_labelWrappers;
};

class AddedLabelTrackObject final : public QObject
{
    Q_OBJECT

public:
    AddedLabelTrackObject(std::shared_ptr<EditState> state, size_t index, QObject* parent);

    Q_INVOKABLE bool addLabel(double start, double end, const QString& text);

private:
    std::shared_ptr<EditState> m_state;
    size_t m_index = 0;
};
} // namespace detail
} // namespace au::trackedit::api
