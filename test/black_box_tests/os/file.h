#ifndef OHJOPAEYOOXOHLAHHOHJ
#define OHJOPAEYOOXOHLAHHOHJ

#include "types.h"
#include <vector>
#include <map>
#include <boost/shared_ptr.hpp>

class File {
public:
    File();

    typedef std::vector<uint8_t> Data;
    
    Data read(size_t offset, size_t len) const;
    error_t write(size_t offset, const Data&);
    size_t size() const;

private:
    std::vector<uint8_t> data;

    File(const File&);
    void operator=(const File&);
};

class FileDescriptor {
public:
    FileDescriptor(boost::shared_ptr<File> file, Mode mode);
    
    error_t read(uint8_t* buffer, size_t buflen, size_t* len);
    error_t write(uint8_t* buffer, size_t len);
    error_t seek(size_t offset);

private:
    boost::shared_ptr<File> file;
    Mode mode;
    size_t offset;
};

class FileSystem {
public:
    FileSystem();
    int open(const std::string& name, Mode mode);
    void close(int handle);

    FileDescriptor resolve(int handle);

private:
    typedef std::map<std::string, boost::shared_ptr<File> > Files;
    Files files;
    typedef std::map<int, FileDescriptor> Descriptors;
    Descriptors descriptors;
    int handle;
};

#endif
