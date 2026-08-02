/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <functional>
#include <memory>
#include <string>

#include <QJSValue>
#include <QObject>

#include "trackedit/api/audiotransform.h"

struct EffectSettings;
class EffectOutputTracks;

namespace au::trackedit::api {
class ProjectEditSession;
}

namespace au::effects::extensions {
class ExtensionEffect;
class ExtensionEffectInstance;

class ExtensionEffectTask final : public QObject, public trackedit::api::AudioTransformTask
{
    Q_OBJECT
    Q_PROPERTY(bool cancelled READ cancelled)

public:
    ExtensionEffectTask(std::function<bool(double, std::string)> progress);

    bool cancelled() const override;
    Q_INVOKABLE bool report(double fraction, const QString& message = {}) override;
    void refresh();

private:
    bool m_cancelled = false;
    std::function<bool(double, std::string)> m_progress;
    double m_fraction = 0.0;
    std::string m_message;
};

class ExtensionPromiseObserver final : public QObject
{
    Q_OBJECT

public:
    using QObject::QObject;

    Q_INVOKABLE void resolve(const QJSValue& value);
    Q_INVOKABLE void reject(const QJSValue& reason);

signals:
    void resolved(const QJSValue& value);
    void rejected(const QString& error);
};

class ExtensionEffectRun final : public QObject
{
public:
    using Completion = std::function<void (bool)>;

    ExtensionEffectRun(ExtensionEffect& effect, ExtensionEffectInstance& instance, EffectSettings& settings, Completion completion);
    ~ExtensionEffectRun() override;

    void start();
    void updateProgress();
    bool cancelled() const;
    void abort(std::string error);

private:
    void finish(const QJSValue& result);
    void fail(std::string error);
    void complete(bool success);

    ExtensionEffect& m_effect;
    ExtensionEffectInstance& m_instance;
    EffectSettings& m_settings;
    Completion m_completion;
    ExtensionEffectTask m_task;
    ExtensionPromiseObserver m_observer;
    std::unique_ptr<EffectOutputTracks> m_renderedInputs;
    std::unique_ptr<trackedit::api::ProjectEditSession> m_projectSession;
    QJSValue m_promise;
    bool m_promisePending = false;
    bool m_finished = false;
    bool m_updatingProgress = false;
};
} // namespace au::effects::extensions
