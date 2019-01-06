#ifndef FRACTALCREATOR_H_
#define FRACTALCREATOR_H_

#include <string>
#include <memory>
#include "Zoom.h"
#include "Bitmap.h"
#include "ZoomList.h"
#include "RGB.h"
#include <vector>



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
    void calculateRangeTotals();
    void calculateTotalIterations();
	void drawFractal();
	void writeBitmap(std::string name);
    std::vector<double> m_ranges;
    std::vector<RGB> m_colors;
    std::vector<int> m_rangeTotals;
    bool m_gotFirstRange{false};
    int getRange(int iterations) const;

public:
	FractalCreator(int width, int height);
	virtual ~FractalCreator();
    void run(std::string name);
	void addZoom(const Zoom& zoom);
    void addRange(double endRange, const RGB& rgb);

};

} /* namespace cave */

#endif /* FRACTALCREATOR_H_ */