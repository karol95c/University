#ifndef FRACTALCREATOR_H_
#define FRACTALCREATOR_H_

#include <string>
#include <memory>
#include "Zoom.h"
#include "Bitmap.h"
#include "ZoomList.h"
#include "RGB.h"



namespace cave {

class FractalCreator {
    int m_width;
    int m_height;
    Bitmap m_bitmap;
    ZoomList m_zoomList;
    std::unique_ptr<int[]> m_histogram;
    std::unique_ptr<int[]> m_fractal;
    int m_total{0};

	void calculateIteration();
    void calculateTotalIterations();
	void drawFractal();
	void addZoom(const Zoom& zoom);
	void writeBitmap(std::string name);

public:
	FractalCreator(int width, int height);
	virtual ~FractalCreator();
    void run(std::string name);

};

} /* namespace cave */

#endif /* FRACTALCREATOR_H_ */