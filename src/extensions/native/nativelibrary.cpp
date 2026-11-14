/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 */
#include "nativelibrary.h"

#include <chrono>
#include <utility>

#if defined(Q_OS_WIN)
#include <QDir>

#ifndef NOMINMAX
#define NOMINMAX
#endif
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#endif

#include "global/concurrency/taskscheduler.h"

#include "bytebuffer.h"

using namespace muse::extensions;

namespace {
constexpr quint32 MAX_ARGUMENT_COUNT = 256;

#if defined(Q_OS_WIN)
QString windowsErrorString(DWORD error)
{
    wchar_t* message = nullptr;
    const DWORD length = FormatMessageW(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM
                                        | FORMAT_MESSAGE_IGNORE_INSERTS | FORMAT_MESSAGE_MAX_WIDTH_MASK,
                                        nullptr, error, 0, reinterpret_cast<wchar_t*>(&message), 0, nullptr);
    QString result;
    if (length != 0 && message) {
        result = QString::fromWCharArray(message, static_cast<qsizetype>(length)).trimmed();
    }
    if (message) {
        LocalFree(message);
    }
    if (result.isEmpty()) {
        return QStringLiteral("Windows error %1").arg(error);
    }
    return QStringLiteral("%1 (Windows error %2)").arg(result).arg(error);
}

#endif

muse::TaskScheduler& nativeCallScheduler()
{
    // Leaked on purpose: the scheduler's destructor joins its workers, which
    // would block application exit on a hung native call. The pool outlives
    // every library; process exit reclaims the threads.
    static auto* scheduler = new muse::TaskScheduler(2);
    return *scheduler;
}

QJSValue boundFunction(QJSEngine* engine, QObject* library, const QString& name, const char* method)
{
    const QString source = QStringLiteral(
        "(function(library, name, method) { return function() { "
        "return library[method](name, Array.prototype.slice.call(arguments)); }; })");
    return engine->evaluate(source).call({ engine->newQObject(library), name, QString::fromUtf8(method) });
}

QString statusText(const NativeCallData& call)
{
    if (!call.returnedString.isEmpty()) {
        return QString::fromUtf8(call.returnedString);
    }
    switch (call.status) {
    case EXT_STATUS_UNKNOWN_CALL:
        return QStringLiteral("Unknown native call");
    case EXT_STATUS_INVALID_ARGUMENT:
        return QStringLiteral("Invalid native call arguments");
    default:
        return QStringLiteral("Native call failed");
    }
}

int32_t dispatchCall(ext_dispatch_fn dispatch, const QByteArray& name, const ext_value* arguments, uint32_t argumentCount,
                     ext_value* result)
{
    if (!dispatch || !result) {
        return EXT_STATUS_ERROR;
    }
    try {
        return dispatch(name.constData(), arguments, argumentCount, result);
    } catch (...) {
        return EXT_STATUS_ERROR;
    }
}

void executeCall(ext_dispatch_fn dispatch, const QByteArray& name, NativeCallData& call)
{
    call.result = {};
    call.status
        = dispatchCall(dispatch, name, call.arguments.empty() ? nullptr : call.arguments.data(),
                       static_cast<uint32_t>(call.arguments.size()), &call.result);
    if (call.result.type == EXT_VALUE_STRING && call.result.as_string) {
        call.returnedString = call.result.as_string;
    }
}
} // namespace

void NativeCallData::releaseBuffers()
{
    for (auto& argument : storage) {
        argument.buffer.reset();
    }
}

NativeLibrary::NativeLibrary(const QString& path, const QByteArray& dispatchSymbol, QObject* parent)
    : QObject(parent)
{
#if defined(Q_OS_WIN)
    // Special case for windows because DLLs do not have runpath equivalent of dylib/so
    // When loading a library QLibrary doesn't add the current dll directory to the library search path
    // resulting in a loading failure if a DLL is linked to another DLL stored in the same directory

    // LoadLibraryExW(..LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR | LOAD_LIBRARY_SEARCH_DEFAULT_DIRS) does exactly this
    const QString nativePath = QDir::toNativeSeparators(path);
    m_libraryHandle = LoadLibraryExW(reinterpret_cast<LPCWSTR>(nativePath.utf16()), nullptr,
                                     LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR | LOAD_LIBRARY_SEARCH_DEFAULT_DIRS);
    if (!m_libraryHandle) {
        m_loadError = windowsErrorString(GetLastError());
        return;
    }
    m_dispatch = reinterpret_cast<ext_dispatch_fn>(
        GetProcAddress(static_cast<HMODULE>(m_libraryHandle), dispatchSymbol.constData()));
#else
    m_library.setFileName(path);
    // Required while orphaned worker calls retain m_dispatch
    m_library.setLoadHints(QLibrary::ResolveAllSymbolsHint | QLibrary::PreventUnloadHint);
    if (m_library.load()) {
        m_dispatch = reinterpret_cast<ext_dispatch_fn>(m_library.resolve(dispatchSymbol.constData()));
    }
#endif
}

NativeLibrary::~NativeLibrary()
{
    for (NativeCall* call : findChildren<NativeCall*>()) {
        if (call->inFlight()) {
            call->orphan();
        } else {
            delete call;
        }
    }
}

bool NativeLibrary::isLoaded() const
{
    return m_dispatch != nullptr;
}

QString NativeLibrary::errorString() const
{
#if defined(Q_OS_WIN)
    return m_libraryHandle && !m_dispatch ? QStringLiteral("The native library has no dispatcher") : m_loadError;
#else
    return m_library.isLoaded() && !m_dispatch ? QStringLiteral("The native library has no dispatcher") : m_library.errorString();
#endif
}

int32_t NativeLibrary::dispatch(const QByteArray& name, const ext_value* arguments, uint32_t argumentCount, ext_value* result) const
{
    return dispatchCall(m_dispatch, name, arguments, argumentCount, result);
}

QJSValue NativeLibrary::bind(const QString& name)
{
    return bind(name, "dispatch");
}

QJSValue NativeLibrary::bindWorker(const QString& name)
{
    return bind(name, "dispatchWorker");
}

QJSValue NativeLibrary::bind(const QString& name, const char* method)
{
    QJSEngine* engine = qjsEngine(this);
    if (!engine || name.isEmpty()) {
        return {};
    }
    return boundFunction(engine, this, name, method);
}

QJSValue NativeLibrary::dispatch(const QString& name, const QJSValue& arguments)
{
    NativeCallData call;
    QJSEngine* engine = qjsEngine(this);
    if (!engine || name.isEmpty() || !prepare(arguments, call)) {
        if (engine) {
            engine->throwError(QStringLiteral("Unsupported native call argument"));
        }
        return {};
    }
    execute(name.toUtf8(), call);
    return resultValue(call);
}

QJSValue NativeLibrary::dispatchWorker(const QString& name, const QJSValue& arguments)
{
    NativeCallData call;
    QJSEngine* engine = qjsEngine(this);
    if (!engine || name.isEmpty() || !prepare(arguments, call)) {
        if (engine) {
            engine->throwError(QStringLiteral("Unsupported native call argument"));
        }
        return {};
    }
    return workerPromise(new NativeCall(*this, name.toUtf8(), std::move(call)));
}

bool NativeLibrary::prepare(const QJSValue& arguments, NativeCallData& call) const
{
    if (!arguments.isArray()) {
        return false;
    }
    const quint32 count = arguments.property(QStringLiteral("length")).toUInt();
    if (count > MAX_ARGUMENT_COUNT) {
        return false;
    }
    call.storage.resize(count);
    call.arguments.resize(count);
    for (quint32 index = 0; index < count; ++index) {
        NativeArgument& stored = call.storage[index];
        const QJSValue input = arguments.property(index);
        if (input.isUndefined() || input.isNull()) {
            stored.value.type = EXT_VALUE_NONE;
        } else if (input.isBool()) {
            stored.value.type = EXT_VALUE_BOOL;
            stored.value.as_bool = input.toBool();
        } else if (input.isString()) {
            stored.string = input.toString().toUtf8();
            stored.value.type = EXT_VALUE_STRING;
            stored.value.as_string = stored.string.constData();
        } else if (auto* object = input.toQObject()) {
            if (auto* handle = qobject_cast<NativeHandle*>(object); handle && handle->library() == this) {
                stored.value.type = EXT_VALUE_OBJECT;
                stored.value.as_object = handle->value();
            } else if (auto* buffer = qobject_cast<ByteBuffer*>(object)) {
                if (!buffer->isExclusive()) {
                    return false;
                }
                stored.buffer = buffer->storage();
                stored.value.type = EXT_VALUE_BUFFER;
                stored.value.as_buffer.data = stored.buffer->data();
                stored.value.as_buffer.size = static_cast<uint64_t>(stored.buffer->size());
            } else {
                return false;
            }
        } else if (input.isNumber()) {
            stored.value.type = EXT_VALUE_NUMBER;
            stored.value.as_number = input.toNumber();
        } else {
            return false;
        }
        call.arguments[index] = stored.value;
    }
    return true;
}

void NativeLibrary::execute(const QByteArray& name, NativeCallData& call) const
{
    executeCall(m_dispatch, name, call);
}

QJSValue NativeLibrary::resultValue(const NativeCallData& call)
{
    QJSEngine* engine = qjsEngine(this);
    if (!engine) {
        return {};
    }
    if (call.status != EXT_STATUS_OK) {
        engine->throwError(statusText(call));
        return {};
    }
    switch (call.result.type) {
    case EXT_VALUE_NONE:
        return QJSValue(QJSValue::UndefinedValue);
    case EXT_VALUE_BOOL:
        return QJSValue(call.result.as_bool);
    case EXT_VALUE_NUMBER:
        return QJSValue(call.result.as_number);
    case EXT_VALUE_STRING:
        return QJSValue(QString::fromUtf8(call.returnedString));
    case EXT_VALUE_OBJECT: {
        if (!call.result.as_object) {
            return QJSValue(QJSValue::NullValue);
        }
        auto* handle = new NativeHandle(call.result.as_object, *this);
        QJSEngine::setObjectOwnership(handle, QJSEngine::JavaScriptOwnership);
        return engine->newQObject(handle);
    }
    case EXT_VALUE_BUFFER:
    default:
        engine->throwError(QStringLiteral("Unsupported native result"));
        return {};
    }
}

QJSValue NativeLibrary::workerPromise(NativeCall* call)
{
    QJSEngine* engine = qjsEngine(this);
    if (!engine || !call) {
        delete call;
        return {};
    }
    if (!m_workerPromiseFactory.isCallable()) {
        m_workerPromiseFactory
            = engine->evaluate(QStringLiteral(
                                   "(function(call) { return new Promise(function(resolve, reject) { "
                                   "call.start(resolve, reject); }); })"));
    }
    return m_workerPromiseFactory.call({ engine->newQObject(call) });
}

NativeCall::NativeCall(NativeLibrary& library, QByteArray name, NativeCallData call)
    : QObject(&library), m_library(&library), m_dispatch(library.m_dispatch), m_name(std::move(name)), m_call(std::move(call))
{
}

NativeCall::~NativeCall()
{
    if (m_future.valid()) {
        m_future.wait();
    }
}

bool NativeCall::inFlight() const
{
    return m_future.valid() && m_future.wait_for(std::chrono::seconds(0)) != std::future_status::ready;
}

void NativeCall::orphan()
{
    m_library = nullptr;
    m_resolve = QJSValue();
    m_reject = QJSValue();
    setParent(nullptr);
}

void NativeCall::start(const QJSValue& resolve, const QJSValue& reject)
{
    if (m_started) {
        return;
    }
    m_started = true;
    m_resolve = resolve;
    m_reject = reject;
    m_future = nativeCallScheduler().submit([this] {
        executeCall(m_dispatch, m_name, m_call);
        QMetaObject::invokeMethod(
            this,
            [this] {
            complete();
        },
            Qt::QueuedConnection);
    });
}

void NativeCall::complete()
{
    m_call.releaseBuffers();
    if (!m_library) {
        deleteLater();
        return;
    }
    if (m_call.status == EXT_STATUS_OK && m_resolve.isCallable()) {
        m_resolve.call({ m_library->resultValue(m_call) });
    } else if (m_reject.isCallable()) {
        m_reject.call({ QJSValue(statusText(m_call)) });
    }
    deleteLater();
}

NativeHandle::NativeHandle(void* value, NativeLibrary& library)
    : m_value(value), m_library(&library)
{
}

void* NativeHandle::value() const
{
    return m_value;
}

NativeLibrary* NativeHandle::library() const
{
    return m_library.data();
}
