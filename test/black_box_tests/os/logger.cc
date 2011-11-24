#include "logger.h"

#include <iostream>
#include <stdlib.h>

Logger* Logger::instance = 0;

void Logger::init()
{
    std::string filename;
    const char* logFile = getenv("EC_LOG_FILE");
    if (logFile == 0) {
        filename = "/tmp/ec.log";
        std::cerr << "warning: using default log file '" << filename << "'. Set EC_LOG_FILE environment variable to overwrite." << std::endl;
    } else {
        filename = logFile;
    }
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

void Logger::log(const std::string& line)
{
    logfile << line << std::endl;
}

LogLine::LogLine() :
    first(true),
    line(new std::stringstream())
{ }

LogLine::LogLine(LogLine& rhs) :
    first(rhs.first),
    line(rhs.line)
{
    rhs.line = 0;
}

LogLine::~LogLine()
{
    if (line != 0) {
        Logger::instance->log(line->str());
        delete line;
    }
}
