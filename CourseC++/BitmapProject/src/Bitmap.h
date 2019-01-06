#ifndef BITMAP_H
#define BITMAP_H

#include <string>
#include <cstdint>
#include <memory>

namespace cave
{

class Bitmap
{
    int m_width{0};
    int m_heigth{0};
    std::unique_ptr<std::uint8_t[]> m_pPixels{nullptr};

    public:
    Bitmap(int width, int height);
	void setPixel(int x, int y, std::uint8_t red, std::uint8_t green, std::uint8_t blue);
	bool write(std::string filename);
	virtual ~Bitmap();
};

}// namespace cave

#endif // BITMAP_H