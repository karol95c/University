#include <iostream>
#include <cstring>
#include <vector>
#include <exception>

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

std::ostream& operator<<(std::ostream& out, const Point& p)
{
    out << "x: " << p.getX() << " y: " << p.getY();
    return out;
}

class Figure
{

public:
    Figure()
    {
        id = new std::string();
    }

    Figure(std::string name)
    {
        id = new std::string(name);
    }
    
    std::string getID() const
    {
        return *id;
    }

    Figure(const Figure &F2)
    {
        id = new std::string(F2.getID());
    }

    Figure& operator=(const Figure& other)
    {
        delete id;
        id = new std::string(other.getID());
        return *this;
    }

    std::ostream& operator<<(std::ostream& out)
    {
        out << "ID: " << *id;
        return out;
    }

    ~Figure()
    {
        delete id;
    }

    void setID(std::string name)
    {
        delete id;
        id = new std::string(name);
    }

private:
    std::string *id;


};

class Polygon : public Figure
{
public:
    Polygon()
    {
        last = 0;
        size = 10;
        Points = new Point[size];
    }

    Polygon(int s)
    {
        last = 0;
        size = s;
        Points = new Point[size];
    }

    Point operator[](int i) const
    {
        if (i >= size)
        {
            throw ("Wykroczenie poza zakres tablicy!");
        }
        return Points[i];
    }


    Polygon(const Polygon& W)
    {
        if (size != W.getSize())
        {
            delete[] Points;
            size = W.getSize();
            last = W.getLastIndex();
            Points = new Point[size];
        }


        for (int i = 0; i <= last; i++)
        {
            Points[i] = W[i];
        }
    }

    int getSize() const
    {
        return size;
    }
    int getLastIndex() const
    {
        return last;
    }

    Polygon& operator=(const Polygon& W)
    {
        if (size != W.getSize())
        {
            delete[] Points;
            size = W.getSize();
            last = W.getLastIndex();
            Points = new Point[size];
        }


        for (int i = 0; i <= last; i++)
        {
            Points[i] = W[i];
        }
        return *this;
    }

    Polygon& operator+(Point& P)
    {
        if(last >= size)
        {
            std::cout << "Tablica jest pełna" << std::endl;
        }
        else
        {
            Points[last] = P;
            last++;
        }
        return *this;
    }

    ~Polygon()
    {
        delete[] Points;
    }

private:
    int size;
    int last;
    Point* Points;


};

int main()
{
    Point points[] = {
        Point(0,0),
        Point(1,1),
        Point(1,1),
        Point(2,2),
        Point(3,3),
        Point(4,4)
    };

    std::cout << points[0] <<std::endl;

    return 0;
}