#ifndef EIXEEGISIPHAOBOMETUG
#define EIXEEGISIPHAOBOMETUG

#include <fstream>
#include <vector>
#include <sstream>

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
    os << "<";
    for (int i=0; i<array.len; ++i) {
        os << array.data[i] << ", ";
    }
    os << ">";
    return os;
}

class LogLine {
public:
    LogLine();

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
            line << ", ";
        }
        line << object;
        return *this;
    }

    std::string getline() const
    {
        return line.str();
    }

private:
    bool first;
    std::stringstream line;

    void operator=(const LogLine&);
    LogLine(const LogLine&);
};

class Logger {
public:
    static Logger* instance;
    static void init(const std::string& logfile);
    static void shutdown();
       
    void log(const LogLine&);

private:
    Logger(const std::string&);
    ~Logger();
    std::ofstream logfile;
};

#endif
