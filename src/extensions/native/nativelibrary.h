/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 */
#pragma once

#include <cstdint>
#include <future>
#include <memory>
#include <vector>

#include <QByteArray>
#include <QJSEngine>
#include <QJSValue>
#if !defined(Q_OS_WIN)
#include <QLibrary>
#endif
#include <QObject>
#include <QPointer>
#include <QString>

#include "nativeextension.h"

namespace muse::extensions {
struct NativeArgument {
    ext_value value{};
    QByteArray string;
    std::shared_ptr<QByteArray> buffer;
};

struct NativeCallData {
    std::vector<NativeArgument> storage;
    std::vector<ext_value> arguments;
    ext_value result{};
    QByteArray returnedString;
    int32_t status = EXT_STATUS_ERROR;

    void releaseBuffers();
};

class NativeLibrary;

class NativeCall final : public QObject
{
    Q_OBJECT

public:
    NativeCall(NativeLibrary& library, QByteArray name, NativeCallData call);
    ~NativeCall() override;

    Q_INVOKABLE void start(const QJSValue& resolve, const QJSValue& reject);

    bool inFlight() const;
    void orphan();

private:
    void complete();

    NativeLibrary* m_library = nullptr;
    ext_dispatch_fn m_dispatch = nullptr;
    QByteArray m_name;
    NativeCallData m_call;
    QJSValue m_resolve;
    QJSValue m_reject;
    std::future<void> m_future;
    bool m_started = false;
};

class NativeLibrary : public QObject
{
    Q_OBJECT

public:
    NativeLibrary(const QString& path, const QByteArray& dispatchSymbol, QObject* parent);
    ~NativeLibrary() override;

    bool isLoaded() const;
    QString errorString() const;
    int32_t dispatch(const QByteArray& name, const ext_value* arguments, uint32_t argumentCount, ext_value* result) const;

    Q_INVOKABLE QJSValue dispatch(const QString& name, const QJSValue& arguments);
    Q_INVOKABLE QJSValue dispatchWorker(const QString& name, const QJSValue& arguments);
    Q_INVOKABLE QJSValue bind(const QString& name);
    Q_INVOKABLE QJSValue bindWorker(const QString& name);

private:
    QJSValue bind(const QString& name, const char* method);
    bool prepare(const QJSValue& arguments, NativeCallData& call) const;
    void execute(const QByteArray& name, NativeCallData& call) const;
    QJSValue resultValue(const NativeCallData& call);
    QJSValue workerPromise(NativeCall* call);

    friend class NativeCall;

#if defined(Q_OS_WIN)
    void* m_libraryHandle = nullptr;
    QString m_loadError;
#else
    QLibrary m_library;
#endif
    ext_dispatch_fn m_dispatch = nullptr;
    QJSValue m_workerPromiseFactory;
};

class NativeHandle final : public QObject
{
    Q_OBJECT

public:
    NativeHandle(void* value, NativeLibrary& library);
    void* value() const;
    NativeLibrary* library() const;

private:
    void* m_value = nullptr;
    QPointer<NativeLibrary> m_library;
};
} // namespace muse::extensions
