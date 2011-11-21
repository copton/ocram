#ifndef EIXEEGISIPHAOBOMETUG
#define EIXEEGISIPHAOBOMETUG

#include <fstream>
#include <vector>
#include <sstream>


template<class T>
class Array {
public:
    Array(T* values, size_t len) :
        data(&values[0], &values[len])
    { }

    std::vector<T> data;
};

template<class T>
std::ostream& operator<<(std::ostream& os, const Array<T>& array)
{
    os << "<";
    for (typename std::vector<T>::const_iterator i = array.vector.begin(); i != array.vector.end(); ++i) {
        os << *i << ", ";
    }
    os << ">";
    return os;
}

class LogLine {
public:
    LogLine(const std::string& syscall);
    ~LogLine();

    template<class T>
    LogLine& log(const T& object)
    {
        if (first) {
            first = false;
        } else {
            (*line) << ", ";
        }
        (*line) << object;
        return *this;
    }

    std::string getline() const
    {
        return line->str();
    }

private:
    bool first;
    std::stringstream* line;
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
