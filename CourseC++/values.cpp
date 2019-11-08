#include <iostream>


void check(int &lval)
{
    std::cout << "LValue" << std::endl;
}

void check(int &&rval)
{
    std::cout << "RValue" << std::endl;
}

int main()
{
    check(10);
    int p = 100;
    check(p);
    check(10+20);
    check(p++);
    check(++p);
    return 0;
}