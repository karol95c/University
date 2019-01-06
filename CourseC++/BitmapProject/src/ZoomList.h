#ifndef ZOOMLIST_H_
#define ZOOMLIST_H_

#include <vector>
#include <utility>
#include "Zoom.h"

namespace cave
{

class ZoomList
{
    double m_xCenter{0};
    double m_yCenter{0};
    double m_scale{1.0};
    int m_width{0};
    int m_height{0};
    std::vector<Zoom> zooms;

    public:
    ZoomList(int, int);
    void add(const Zoom&);
    std::pair<double, double> doZoom(int, int);
};

} //namespace cave

#endif //ZOOMLIST_H_