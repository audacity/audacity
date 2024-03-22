#include "vitaservice.h"

using namespace app::vita;

std::string VitaService::doSomeThing() const
{
    return "vita_service_data";
}

std::string VitaService::doSomeThingWithAlpha() const
{
    return std::string("vita_service_data") + "_" + alphaService()->info();
}
