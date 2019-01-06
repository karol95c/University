#include "FractalCreator.h"
#include "Mandelbrot.h"
#include <math.h>
#include <assert.h>

namespace cave
{
    FractalCreator::FractalCreator(int width, int height) : m_width(width), m_height(height),
        m_bitmap(width, height),
        m_zoomList(width, height),
        m_histogram(new int[Mandelbrot::MAX_ITERATIONS]{0}),
        m_fractal(new int[m_width*m_height])
    {
        m_zoomList.add(Zoom(m_width / 2, m_height / 2, 4.0 / m_width));
    }

    FractalCreator::~FractalCreator()
    {}

    void FractalCreator::run(std::string name)
    {

        calculateIteration();
        calculateTotalIterations();
        calculateRangeTotals();
        drawFractal();
        writeBitmap("test.bmp");
    }

    void FractalCreator::calculateIteration()
    {
        for(int y = 0; y < m_height; ++y)
        {
            for (int x = 0; x < m_width; ++x)
            {
                std::pair<double, double>fractals = m_zoomList.doZoom(x, y);

                int iterations = Mandelbrot::getIterations(fractals.first, fractals.second);
                
                m_fractal[y * m_width + x] = iterations;
                if (iterations != Mandelbrot::MAX_ITERATIONS)
                {
                    m_histogram[iterations]++;
                }
            }
        }
    }

    void FractalCreator::calculateRangeTotals()
    {
        int rangeIndex = 0;

        for (int i = 0; i < Mandelbrot::MAX_ITERATIONS; ++i)
        {
            int pixels = m_histogram[i];
            
            if (i >= m_ranges[rangeIndex + 1]) rangeIndex++;

            m_rangeTotals[rangeIndex] += pixels;
        }
        
        int overallTotal = 0;
    }

    void FractalCreator::calculateTotalIterations()
    {
        for (int i = 0; i < Mandelbrot::MAX_ITERATIONS; ++i)
        {
            m_total += m_histogram[i];
        }
    }

	void FractalCreator::drawFractal()
    {
        RGB startColor(0, 0, 0);
        RGB endColor(0, 0, 255);
        RGB diffColor = endColor - startColor;
        for(int y = 0; y < m_height; ++y)
        {
            for (int x = 0; x < m_width; ++x)
            {

                int iterations = m_fractal[y * m_width + x];
                int range = getRange(iterations);
                int rangeTotal = m_rangeTotals[range];
                int rangeStart = m_ranges[range];

                RGB& startColor = m_colors[range];
                RGB& endColor = m_colors[range + 1];
                RGB diffColor = endColor - startColor;

                std::uint8_t red = 0;
                std::uint8_t green = 0;
                std::uint8_t blue = 0;
                
                if (iterations != Mandelbrot::MAX_ITERATIONS)
                {
                    int totalPixels = 0;
                    
                    for (int i = 0; i <= iterations; ++i)
                    {
                        totalPixels += (double)m_histogram[i];
                    }

                    red = startColor.r + diffColor.r * (double)totalPixels/rangeTotal;
                    green = startColor.g + diffColor.g * (double)totalPixels/rangeTotal;
                    blue = startColor.b + diffColor.b * (double)totalPixels/rangeTotal;
                }

                

                m_bitmap.setPixel(x, y, red, green, blue);

                // if (color < min) min = color;
                // if (color > max) max = color;
            }
        }
    }
    void FractalCreator::addRange(double endRange, const RGB& rgb)
    {
        m_ranges.push_back(endRange * Mandelbrot::MAX_ITERATIONS);
        m_colors.push_back(rgb);

        if(m_gotFirstRange)
        {
            m_rangeTotals.push_back(0);
        }
        m_gotFirstRange = true;

    }

    int FractalCreator::getRange(int iterations)const
    {
        int range = 0;

        for(int i=1; i < m_ranges.size(); i++) {

            range = i;

            if(m_ranges[i] > iterations) {
                break;
            }

        }

        range--;

        assert(range > -1);
        assert(range < m_ranges.size());

        return range;
    }

    void FractalCreator::addZoom(const Zoom& zoom)
    {
	    m_zoomList.add(zoom);
    }   

	void FractalCreator::writeBitmap(std::string name)
    {
        m_bitmap.write(name);
    }

}//namespace cave