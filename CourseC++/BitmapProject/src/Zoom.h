#ifndef ZOOM_H_
#define ZOOM_H_


namespace cave
{

struct Zoom{
    int x{0};
    int y{0};
    double scale{0.0};

    Zoom(int cx, int cy, double cscale) : x(cx), y(cy), scale(cscale) {};
};

} //namespace cave

#endif //ZOOM_H_