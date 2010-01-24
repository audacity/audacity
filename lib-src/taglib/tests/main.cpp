#include <stdexcept>
#include <cppunit/TestResult.h>
#include <cppunit/TestResultCollector.h>
#include <cppunit/TestRunner.h>
#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/BriefTestProgressListener.h>
#include <cppunit/CompilerOutputter.h>

int main(int argc, char* argv[])
{
  std::string testPath = (argc > 1) ? std::string(argv[1]) : "";

  // Create the event manager and test controller
  CppUnit::TestResult controller;

  // Add a listener that colllects test result
  CppUnit::TestResultCollector result;
  controller.addListener(&result);

  // Add a listener that print dots as test run.
  CppUnit::BriefTestProgressListener progress;
  controller.addListener(&progress);

  // Add the top suite to the test runner
  CppUnit::TestRunner runner;
  runner.addTest(CppUnit::TestFactoryRegistry::getRegistry().makeTest());

  try {
    std::cout << "Running "  << testPath;
    runner.run(controller, testPath);

    std::cerr << std::endl;

    // Print test in a compiler compatible format.
    CppUnit::CompilerOutputter outputter(&result, std::cerr);
    outputter.write();
  }
  catch(std::invalid_argument &e){
    std::cerr << std::endl
              << "ERROR: " << e.what()
              << std::endl;
    return 0;
  }

  return result.wasSuccessful() ? 0 : 1;
}
