#include <iostream>

class Point
{   

public:
    Point()
    {
        x = 0;
        y = 0;
    }
    Point(int a, int b)
    {
        x = a;
        y = b;
    }

    std::ostream& operator<<(std::ostream& out)
    {
        out << "x: " << x << " y: " << y;
        return out;
    }

    bool operator==(const Point& other)
    {
        return x == other.x && y == other.y;
    }

    int getX() const
    {
        return x;
    }

    int getY() const 
    {
        return y;
    }

    void setX(int a)
    {
        x = a;
    }

    void setY(int a)
    {
        y = a;
    }

private:
    int x;
    int y;
};