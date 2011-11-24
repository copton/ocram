#ifndef EIXEEGISIPHAOBOMETUG
#define EIXEEGISIPHAOBOMETUG

#include <fstream>
#include <vector>
#include <sstream>

class LogLine;

class Logger {
public:
    static Logger* instance;
    static void init();
    static void shutdown();
       
    void log(const std::string&);

private:
    Logger(const std::string&);
    ~Logger();
    std::ofstream logfile;
};

class LogLine {
public:
    LogLine();
    LogLine(LogLine&);
    ~LogLine();

    template<class T>
    LogLine& operator()(const T& object)
    {
        return log(object);
    }

    template<class T>
    LogLine& log(const T& object)
    {
        if (first) {
            first = false;
        } else {
            *line << ", ";
        }
        *line << object;
        return *this;
    }

private:
    bool first;
    std::stringstream* line;
    void operator=(const LogLine&);
};

template<class T>
class Array {
public:
    Array(T* data, size_t len) :
        data(data), len(len)
    { }

    T* data;
    size_t len;
};

template<class T> Array<T> array(T* value, size_t len)
{
    return Array<T>(value, len);
}

template<class T>
std::ostream& operator<<(std::ostream& os, const Array<T>& array)
{
    os << "<" << array.len << "> 0x" << std::hex;
    for (int i=0; i<array.len; ++i) {
        os << int(array.data[i]);
    }
    os << std::dec;
    return os;
}

#endif
