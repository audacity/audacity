/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 */
#include "bytebuffer.h"

#include <QJSEngine>

using namespace muse::extensions;

namespace {
void throwSizeError(const QObject* object)
{
    if (auto* engine = qjsEngine(object)) {
        engine->throwError(QJSValue::RangeError, QStringLiteral("Buffer size does not match"));
    }
}

void throwInUseError(const QObject* object)
{
    if (auto* engine = qjsEngine(object)) {
        engine->throwError(QStringLiteral("Buffer is in use by a native call"));
    }
}
} // namespace

ByteBuffer::ByteBuffer(size_t byteLength, QObject* parent)
    : QObject(parent), m_data(std::make_shared<QByteArray>(static_cast<qsizetype>(byteLength), '\0'))
{
}

qulonglong ByteBuffer::byteLength() const
{
    return static_cast<qulonglong>(m_data->size());
}

QByteArray ByteBuffer::copyToArrayBuffer() const
{
    if (!isExclusive()) {
        throwInUseError(this);
        return {};
    }
    return *m_data;
}

void ByteBuffer::copyFromArrayBuffer(const QByteArray& buffer)
{
    if (buffer.size() != m_data->size()) {
        throwSizeError(this);
        return;
    }
    if (!isExclusive()) {
        throwInUseError(this);
        return;
    }
    *m_data = buffer;
}

void* ByteBuffer::data()
{
    return m_data->data();
}

const void* ByteBuffer::data() const
{
    return m_data->constData();
}

std::shared_ptr<QByteArray> ByteBuffer::storage() const
{
    return m_data;
}

bool ByteBuffer::isExclusive() const
{
    return m_data.use_count() == 1;
}
