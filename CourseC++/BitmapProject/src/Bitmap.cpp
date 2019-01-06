#include <fstream>
#include "Bitmap.h"
#include "BitmapFileHeader.h"
#include "BitmapInfoHeader.h"

using namespace cave;

namespace cave
{

    Bitmap::Bitmap(int width, int height) : m_width(width), m_heigth(height), m_pPixels(new std::uint8_t[width*height*3]{})
    {}

    void Bitmap::setPixel(int x, int y, std::uint8_t red, std::uint8_t green, std::uint8_t blue)
    {
        std::uint8_t *pPixel = m_pPixels.get();

        pPixel += (y * 3) * m_width + (x * 3);
        
        pPixel[0] = blue;
        pPixel[1] = green;
        pPixel[2] = red;
        // delete pPixel;
    }
    bool Bitmap::write(std::string fileName)
    {
        BitmapFileHeader fileHeader;
        BitmapInfoHeader infoHeader;
        fileHeader.fileSize = sizeof(fileHeader) + sizeof(infoHeader) + m_heigth * m_width * 3;
        fileHeader.dataOffset = sizeof(fileHeader) + sizeof(infoHeader);
        infoHeader.width = m_width;
        infoHeader.height = m_heigth;

        std::ofstream file;
        file.open(fileName, std::ios::out|std::ios::binary);
        
        if(!file)
        {
            return false;
        }
        
        file.write((char *)&fileHeader, sizeof(fileHeader));
        file.write((char *)&infoHeader, sizeof(infoHeader));
        file.write((char *)m_pPixels.get(), m_heigth *m_width * 3);
        file.close();
        if(!file)
        {
            return false;
        }
        return true; //TODO
    }
    Bitmap::~Bitmap()
    {

    }
}//namespace cave