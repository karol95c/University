#ifndef BITMAPFILEHEADER_H_
#define BITMAPFILEHEADER_H_

#include <cstdint>

#pragma pack(2)

namespace cave
{

struct BitmapFileHeader
{
    char header[2]{'B', 'M'};
    std::int32_t fileSize;
    std::int32_t reserved{0};
    std::int32_t dataOffset;
};

} //namespace cave
#endif /*BITMAPFILEHEADER_H_*/