/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <functional>

#include <QMetaObject>
#include <QObject>

#include "framework/global/types/ret.h"
#include "framework/rcommand/icommanddispatcher.h"

namespace au::shared {
//! For handlers that open a modal dialog: run them outside the async queue, which the dialog would otherwise block.
inline muse::rcommand::ICommandDispatcher::AsyncCallBack modalCommand(QObject* context, std::function<muse::Ret()> handler)
{
    return [context, handler = std::move(handler)](const muse::rcommand::Request& request,
                                                   const muse::rcommand::ICommandDispatcher::OnResponse& onResponse) {
        QMetaObject::invokeMethod(context, [request, handler, onResponse]() {
            onResponse(muse::rcommand::make_response(request, handler()));
        }, Qt::QueuedConnection);
    };
}
}
