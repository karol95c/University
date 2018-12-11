#include <iostream>


template<int N> struct Lucas {
 enum {value = Lucas<N-1>::value+Lucas<N-2>::value};
};

template<> struct Lucas<0> {
 enum {value = 2};
};

template<> struct Lucas<1> {
 enum {value = 1};
};


int main()
{
    int x = Lucas<45>::value;
    std::cout << x << std::endl;
}