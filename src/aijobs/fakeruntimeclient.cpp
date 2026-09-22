/*
 * Audacity: A Digital Audio Editor
 */
#include "fakeruntimeclient.h"

using namespace au::aijobs;

RuntimeStatus FakeRuntimeClient::status() const
{
    return RuntimeStatus::Unavailable;
}

int FakeRuntimeClient::protocolVersion() const
{
    return au::aicore::AI_RUNTIME_PROTOCOL_VERSION;
}
