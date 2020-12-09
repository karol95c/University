#include <stdio.h>
#include <vector>
#include <inttypes.h>
#include <algorithm>
#include <cmath>

struct Point
{
    int x;
    int y;

    Point() = default;
    Point(int xC, int yC) : x(xC), y(yC){};

};

static inline long long square(const int x) {return static_cast<long long>(x) * static_cast<long long>(x);}

static inline double dist(const Point a, const Point b)
{
    return sqrt(double(square(a.x - b.x) + square(a.y - b.y)));
}


static inline double countPerimeter(const Point& a, const Point& b, const Point& c)
{
  return dist(a, b) + dist(b, c) + dist(c, a);
}

struct Triangle
{
    Point a;
    Point b;
    Point c;
    double perimeter = INFINITY;

    Triangle(Point paramA, Point paramB, Point paramC) : a(paramA), b(paramB), c(paramC)
    {
        perimeter = countPerimeter(paramA, paramB, paramC);
    }

    Triangle() : perimeter(INFINITY){}


    void setUpTriangle(Point paramA, Point paramB, Point paramC)
    {
        a = paramA;
        b = paramB;
        c = paramC;

        perimeter = countPerimeter(a, b, c);
    }

    void setUpTriangle(Point paramA, Point paramB, Point paramC, double p)
    {
        a = paramA;
        b = paramB;
        c = paramC;

        perimeter = p;
    }

    void setUpTriangle(Triangle *t)
    {
        a = t->a;
        b = t->b;
        c = t->c;

        perimeter = t->perimeter;
    }
};

static inline bool comparePerimeters(const Triangle& res, const Triangle& cand)
{
    return res.perimeter < cand.perimeter;
}

Triangle getMinPerimeter(const Triangle res, const Triangle cand)
{
    return res.perimeter < cand.perimeter ? res : cand;
}

Triangle getMinCornerTriangle(const std::vector<Point>& pointsX)
{
    Triangle r;
    int size = pointsX.size();

    for (int i = 0; i < size; i++)
    {
        for (int j = 0; j < size; j++)
        {
            for (int k = 0; k < size; k++)
            {
                if (i != j && j != k && k != i)
                {
                    double p = countPerimeter(pointsX[i], pointsX[j], pointsX[k]);
                    if (p < r.perimeter)
                    {
                        r.setUpTriangle(pointsX[i], pointsX[j], pointsX[k], p);
                    }
                }
            }
        }
    }

    return r;
}

Triangle merge(std::vector<Point>& temp, const double avg)
{
    std::vector<Point> cornerPoints;
    cornerPoints.clear();

    Triangle res;

    unsigned int size = temp.size();

    for (unsigned int i = 0; i < size; i++)
    {
        cornerPoints.clear();
        cornerPoints.push_back(temp[i]);
        unsigned int j = i + 1;
        while (temp[j].y <= temp[i].y + avg && j < size)
        {
            cornerPoints.push_back(temp[j]);
            j++;
        }

        if (cornerPoints.size() >= 3)
        {
            Triangle brute = getMinCornerTriangle(cornerPoints);
            res = getMinPerimeter(res, brute);
        }
    }

    return res;
}

bool isInRange(const Point& point, const int midX, const double cutPoint)
{
    return point.x >= (midX - cutPoint) && point.x <= (midX + cutPoint);
}

Triangle process(const std::vector<Point>& pointsX, const int begin, const int end)
{
    Triangle res;
    unsigned int n = end - begin;
    if (n < 3)
    {
        return res;
    }

    int mid = begin + n / 2;

    int midX = (pointsX[mid].x + pointsX[mid + 1].x) / 2;
    Triangle minLeft = process(pointsX, begin, mid);
    Triangle minRight = process(pointsX, mid, end);

    res = getMinPerimeter(minLeft, minRight);

    double cutPoint = res.perimeter / 2;

    std::vector <Point> temp;
    int iter = mid;
    while (iter >= begin && isInRange(pointsX[iter], midX, cutPoint))
    {
        temp.push_back(pointsX[iter]);
        iter--;
    }

    iter = mid + 1;
    int size = pointsX.size();
    while (iter < end && iter < size && isInRange(pointsX[iter], midX, cutPoint))
    {
        temp.push_back(pointsX[iter]);
        iter++;
    }

    std::sort(temp.begin(), temp.end(), [](const Point& lhs, const Point& rhs)
    {
        return lhs.y < rhs.y;
    });

    res = getMinPerimeter(res, merge(temp, cutPoint));

    return res;
}

void printfTriangleCord(const Triangle& triangle)
{
    printf("%d %d\n", triangle.a.x, triangle.a.y);
    printf("%d %d\n", triangle.b.x, triangle.b.y);
    printf("%d %d\n", triangle.c.x, triangle.c.y);
}

void findMinTriangle(std::vector<Point>& pointsX)
{
    std::sort(pointsX.begin(), pointsX.end(), [](const Point& lhs, const Point& rhs)
    {
        return lhs.x < rhs.x;
    });

    int size = pointsX.size();
    Triangle res = process(pointsX, 0, size);

    printfTriangleCord(res);
}


int main()
{
    int n = 0;
    int r = scanf("%d", &n);
    (void) r;
    std::vector<Point> pointsX;

    int xTemp = 0;
    int yTemp = 0;
    for (int i = 0; i < n; ++i)
    {
        scanf("%d %d", &xTemp, &yTemp);
        pointsX.push_back(Point(xTemp, yTemp));
    }

    findMinTriangle(pointsX);

    return 0;
}