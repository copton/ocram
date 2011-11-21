#include "logger.h"
#include "dispatcher.h"

#include <iostream>

Logger* Logger::instance = 0;

void Logger::init(const std::string& filename)
{
    instance = new Logger(filename);
}

void Logger::shutdown()
{
    delete instance;
    instance = 0;
}

Logger::Logger(const std::string& filename) :
    logfile(filename.c_str(), std::ios::trunc)
{ }

Logger::~Logger()
{
    logfile.close();
}

LogLine::LogLine() :
    first(true)
{
    (*this)(Dispatcher::instance->get_simulation_time());
}

void Logger::log(const LogLine& line)
{
    logfile << line.getline() << std::endl;
}
