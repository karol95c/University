
#include <iostream>
#include <locale>
#include <random>
#include <chrono>
#include <ctime>
#include <iomanip>
#include <string>
#include <iterator>
#include <codecvt>

struct numSeparator : std::numpunct<char>
{
    char do_thousands_sep()   const { return ' '; }  // separate with spaces
    std::string do_grouping() const { return "\3"; }
};


int main()
{
    double cash = 0;
    std::cout << "Pass an amount values using digits: " << std::endl;
    std::cin >> cash;
    cash *= 100;
    // cash =28202;
    // setlocale(LC_ALL,"");
    std::string local = std::locale("").name().c_str();
    std::string enUtf = "en_US.UTF-8";
    std::cout << "User-preferred locale setting is " << local << '\n';
    std::cout << "---------------------------------------\n";

    std::uniform_int_distribution<> normalDist(-1000000000, 1000000000);
    std::random_device rd;
    std::mt19937_64 generator = std::mt19937_64(rd());
    int rnd = normalDist(generator);
    std::time_t t = std::time(nullptr);
    std::tm tm = *std::localtime(&t);
    
    std::cout.imbue(std::locale(std::locale(""), new numSeparator));
    std::cout << local << '\n';
    std::cout << "money locale: " << std::showbase <<  std::put_money(cash)<< '\n';
    std::cout << "number locale: " << rnd << '\n';
    std::cout << "time: " << std::put_time(&tm, "%c %Z") << '\n';

    std::cout << "---------------------------------------\n";

    std::cout.imbue(std::locale(enUtf));
    std::cout << enUtf << '\n';
    std::cout << "money: " << std::showbase << std::put_money(cash) << '\n';
    std::cout << "number: " << rnd << '\n';
    std::cout << "time: " << std::put_time(&tm, "%c %Z") << '\n';
    return 0; 
}