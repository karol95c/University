#include <iostream>
#include <vector>

template<int n> struct Lucas
{
    enum {value = Lucas< n-1 >::value + Lucas< n-2 >::value};
};

template<> struct Lucas<0>
{
    enum {value = 2};
};

template<> struct Lucas<1> 
{
    enum {value = 1};
};


int main()
{
    int x = Lucas<44>::value;
    std::cout << x << std::endl;
    std::vector< int > vec{1,2,3,4,5};
}