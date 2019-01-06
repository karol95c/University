#ifndef MANDELBROT_H_
#define MANDELBROT_H_

namespace cave {

class Mandelbrot {
public:
	static const int MAX_ITERATIONS = 1000;

public:
	Mandelbrot();
	virtual ~Mandelbrot();

	static int getIterations(double x, double y);
};

} /* namespace cave */

#endif /* MANDELBROT_H_ */