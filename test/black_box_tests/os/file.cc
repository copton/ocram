#include "file.h"
#include <string>

File::File() { }

File::Data File::read(size_t offset, size_t len) const
{
    if (offset + len > data.size()) {
        return Data(data.begin() + offset, data.end());
    } else {
        return Data(data.begin() + offset, data.begin() + offset + len);
    }
}

error_t File::write(size_t offset, const Data& other)
{
    if (offset + other.size() > data.size()) {
        const size_t p = data.size() - offset;
        std::copy(other.begin(), other.begin() + p, data.begin() + offset);
        std::copy(other.begin() + p, other.end(), std::back_inserter(data));
    } else {
        std::copy(other.begin(), other.end(), data.begin() + offset);
    }
    return SUCCESS;
}

size_t File::size() const
{
    return data.size();
}

FileDescriptor::FileDescriptor(boost::shared_ptr<File> file, Mode mode) :
    file(file), mode(mode), offset(0)
{ }

error_t FileDescriptor::read(uint8_t* buffer, size_t buflen, size_t* len)
{
    *len = 0;
    if ((mode & READ) == 0) {
        return FAIL;
    }
    File::Data data(file->read(offset, buflen));
    *len = data.size();
    std::copy(data.begin(), data.end(), buffer);
    return SUCCESS;
}

error_t FileDescriptor::write(uint8_t* data, size_t len)
{
    return file->write(offset, File::Data(&data[0], &data[len]));
}

error_t FileDescriptor::seek(size_t newOffset)
{
    if (newOffset > file->size()) {
        return FAIL;
    }
    offset = newOffset;
    return SUCCESS;
}

FileSystem::FileSystem() :
    handle(0)
{ }

int FileSystem::open(const std::string& name, Mode mode)
{
    Files::iterator file = files.find(name);
    if (file == files.end()) {
        boost::shared_ptr<File> fileptr(new File());
        file = files.insert(std::make_pair(name, fileptr)).first;
    }
    Descriptors::iterator descriptor = descriptors.insert(std::make_pair(handle++, FileDescriptor(file->second, mode))).first;
    return descriptor->first;
}

void FileSystem::close(int handle)
{
    descriptors.erase(handle);
}

FileDescriptor FileSystem::resolve(int handle)
{
    Descriptors::iterator descriptor = descriptors.find(handle);
    assert (descriptor != descriptors.end());
    return descriptor->second;
}
