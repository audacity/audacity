#pragma once

#include <QObject>

#include "iplaycursorcontroller.h"
#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "actions/iactionsdispatcher.h"

namespace au::projectscene {
class PlayCursorController : public IPlayCursorController
{
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;

public:
    PlayCursorController() = default;

    processing::secs_t timePosition() const override;
    void setTimePosition(processing::secs_t newTimePosition) override;
    muse::async::Channel<processing::secs_t> timePositionChanged() const override;

private:

    template<typename T>
    struct Val {
        T val = T();
        muse::async::Channel<T> changed;

        void set(const T& v)
        {
            val = v;
            changed.send(v);
        }
    };

    Val<processing::secs_t> m_timePosition;
};
}
