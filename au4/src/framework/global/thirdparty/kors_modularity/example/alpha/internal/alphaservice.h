#ifndef EXAMPLE_ALPHASERVICE_H
#define EXAMPLE_ALPHASERVICE_H

#include "../ialphaservice.h"

namespace app::alpha {
class AlphaService : public IAlphaService
{
public:
    AlphaService() = default;

    std::string info() const override;
};
}

#endif // EXAMPLE_ALPHASERVICE_H
