/*
* Audacity: A Digital Audio Editor
*/
#include "timedrecordingcontroller.h"

#include "../recordcommands.h"

using namespace au::record;
using namespace muse;

static const UriQuery TIMED_RECORDING_SETUP_URI("audacity://record/timedrecording");

void TimedRecordingController::init()
{
    commandDispatcher()->onRequest(this, RECORD_TIMED_RECORDING_COMMAND, [this]() { return openSetupDialog(); });
}

Ret TimedRecordingController::openSetupDialog()
{
    RetVal<Val> rv = interactive()->openSync(TIMED_RECORDING_SETUP_URI);
    return rv.ret;
}
