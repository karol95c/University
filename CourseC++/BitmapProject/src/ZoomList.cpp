#include "ZoomList.h"


namespace cave
{
    ZoomList::ZoomList(int width, int height) : m_width(width), m_height(height) {};

    void ZoomList::add(const Zoom& z)
    {
        zooms.push_back(z);

        m_xCenter += (z.x - m_width/2) * m_scale;
        m_yCenter += (z.y - m_height/2) * m_scale;
        
        m_scale *= z.scale;
    }

    std::pair<double, double> ZoomList::doZoom(int x, int y)
    {
        double xFractal = (x - m_width/2) * m_scale + m_xCenter;
        double yFractal = (y - m_height/2) * m_scale + m_yCenter;
        return std::pair<double, double>(xFractal, yFractal);
    }

} //namespace cave