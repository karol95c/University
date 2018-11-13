#include <set>
#include <list>
#include <string>
#include <vector>
#include <climits>
#include <limits>
#include <iostream>
#include <iterator>
#include <algorithm>
#include <functional>

typedef std::vector<double>::iterator vectorIterator;
typedef std::list<std::string>::iterator listIterator;
typedef std::set<int>::iterator setIterator;

template<class T>
class Range
{
    private:
    T min;
    T max;

    public:
    Range(T a, T b) : min(a), max(b) {}
    bool operator()(T element)
    {
        return element > min and element < max;
    }
};

template<class T>
class AddValue
{
    private:
    T value;

    public:
    AddValue(T a) : value(a) {}
    bool operator()(T& element)
    {
        element += value;
    }
};


class MinMaxVector
{
    private:
    std::pair< vectorIterator, vectorIterator > minmax;
    int i = 0;
    int j = 0;

    public:
    MinMaxVector(vectorIterator a, int it = 0) : minmax(a,a) {};
    std::pair< vectorIterator, vectorIterator > operator()(std::pair< vectorIterator, vectorIterator > m, double element)
    {
        if (element < *minmax.first)
        {
            std::advance(minmax.first, i);
            i = 1;
            ++j;
        }
        else if(element > *minmax.second)
        {
            std::advance(minmax.second, j);
            j = 1;
            ++i;
        }
        else 
        {
           ++i;
           ++j;
        }
        return minmax;
    }
};

class MinMaxSet
{
    private:
    std::pair< setIterator, setIterator > minmax;
    int i = 0;
    int j = 0;

    public:
    MinMaxSet(setIterator a, int it = 0) : minmax(a,a) {};
    std::pair< setIterator, setIterator > operator()(std::pair< setIterator, setIterator > m, int element)
    {
        if (element < *minmax.first)
        {
            std::advance(minmax.first, i);
            i = 1;
            ++j;
        }
        else if(element > *minmax.second)
        {
            std::advance(minmax.second, j);
            j = 1;
            ++i;
        }
        else 
        {
           ++i;
           ++j;
        }
        return minmax;
    }
};

class MinMaxList
{
    private:
    std::pair< listIterator, listIterator > minmax;
    int i = 0;
    int j = 0;

    public:
    MinMaxList(listIterator a, int it = 0) : minmax(a,a) {};
    std::pair< listIterator, listIterator > operator()(std::pair< listIterator, listIterator > m, std::string element)
    {
        if (element < *minmax.first)
        {
            std::advance(minmax.first, i);
            i = 1;
            ++j;
        }
        else if(element > *minmax.second)
        {
            std::advance(minmax.second, j);
            j = 1;
            ++i;
        }
        else 
        {
           ++i;
           ++j;
        }
        return minmax;
    }
};


void printRange(std::vector<double>& V, std::list<std::string>& L, std::set<int>& S)
{
    std::ostream_iterator<double> out_double(std::cout,", ");
    std::ostream_iterator<std::string> out_string (std::cout,", ");
    std::ostream_iterator<int> out_int (std::cout,", ");
    std::cout << "Wypisz wszystkie wartości z zadanego zakresu (większe od a i mniejsze od b):" << std::endl;
    std::copy_if(V.begin(), V.end(), out_double, Range<double>(1.0, 3.0));
    std::cout << std::endl;
    std::copy_if(L.begin(), L.end(), out_string, Range<std::string>("b", "h"));
    std::cout << std::endl;
    std::copy_if(S.begin(), S.end(), out_int, Range<int>(0, 25));
    std::cout << std::endl;
}

void printSum(std::vector<double>& V, std::list<std::string>& L, std::set<int>& S)
{
    std::cout << "Wyznacz sumę/konkatenację wszystkich elementów:" << std::endl;
    std::cout << std::accumulate(V.begin(), V.end(), 0.0, [](const double a, const double b) { return a + b;}) << std::endl;
    std::cout << std::accumulate(L.begin(), L.end(), std::string(), [](const std::string a, const std::string b) { return a + b; }) << std::endl;
    std::cout << std::accumulate(S.begin(), S.end(), 0, [](const int a, const int b) { return a + b; });
    std::cout << std::endl;
}

void printAddValue(std::vector<double>& V, std::list<std::string>& L, std::set<int>& S)
{
    std::cout << "Dodaj wartosc do każdego elementu kontenera: " << std::endl;
    std::for_each(V.begin(), V.end(), AddValue<double>(10.0));
    // std::transform(V.begin(), V.end(), V.begin(), std::bind2nd(std::plus<double>(), 1000.0));
    for (auto v : V) std::cout << v << " " ;
    std::cout << std::endl;
    std::for_each(L.begin(), L.end(), AddValue<std::string>("ABC"));
    for (auto l : L) std::cout << l << " " ;
    std::cout << std::endl;
}

void printAverage(std::vector<double>& V, std::list<std::string>& L, std::set<int>& S)
{
    std::cout << "Wyznasz średnią wartość elementów: " << std::endl;
    auto lambda = [&](double a, double b){return a + b / V.size(); };
    std::cout << std::accumulate(V.begin(), V.end(), 0.0, [&V](double a, double b){return a + b / V.size(); }) << std::endl;
    std::cout << std::accumulate(S.begin(), S.end(), 0.0, [&S](double a, double b){return a + b / S.size();}) <<std::endl;
}
void printMinMax(std::vector<double>& V, std::list<std::string>& L, std::set<int>& S)
{
    std::cout << "Wypisz min i max wartość elementów: " << std::endl;
    auto resultVector = std::accumulate(V.begin(), V.end(), std::pair< vectorIterator, vectorIterator >(),
        MinMaxVector(V.begin()));
    std::cout << *resultVector.first << std::endl;
    std::cout << *resultVector.second << std::endl;
    auto resultSet = std::accumulate(S.begin(), S.end(), std::pair< setIterator, setIterator >(),
        MinMaxSet(S.begin()));
    std::cout << *resultSet.first << std::endl;
    std::cout << *resultSet.second << std::endl;
    auto resultList = std::accumulate(L.begin(), L.end(), std::pair< listIterator, listIterator >(),
        MinMaxList(L.begin()));
    std::cout << *resultList.first << std::endl;
    std::cout << *resultList.second << std::endl;
}

void printMyIdeas(std::vector<double>& V, std::list<std::string>& L, std::set<int>& S)
{
    std::ostream_iterator<double> out_double(std::cout,", ");
    std::ostream_iterator<std::string> out_string (std::cout,", ");
    std::ostream_iterator<int> out_int (std::cout,", ");
    std::cout << "Suma cyfr jest parzysta: " << std::endl;
    std::copy_if(V.begin(), V.end(), out_double, [](double element)
        {
            int sum = 0;
            while (element > 0)
            {
                sum += element / 10;
                element /= 10.0;
            }
            return sum % 2;
        });
    std::cout << std::endl;
    std::cout << "Suma cyfr jest nieparzysta: " << std::endl;
    std::copy_if(S.begin(), S.end(), out_int,
        [](int element)
        {
            int sum = 0;
            while (element > 0)
            {
                sum += element / 10;
                element /= 10;
            }
            return !sum % 2;
        });
    std::cout << std::endl;
}

int main()
{
    std::vector<double> V = {0.4, 2.4, 4.0, 2.1, 5.6, 1.01, 1.0, 3.0, 2.99, 3.01, 50.0, -12.0, 1.5, 6.7};
    std::list<std::string> L = {"alfa", "b", "bravo", "charlie", "delta", "echo", "foxtrot", "golf", "hotel", "india"};
    std::set<int> S = {20, -12, 2, 0, -100, 48, -1, 1, 82, 4};
    printRange(V, L, S);
    printSum(V, L, S);
    printAddValue(V, L, S);
    printAverage(V, L, S);
    printMinMax(V, L, S);
    printMyIdeas(V, L, S);
    // auto lambda = [](std::vector<double>& v, int p, int n)
    //     {
    //         vectorIterator it = v.begin();
    //         std::advance(it, p);
    //         for (it; it != v.end(); std::advance(it, n))
    //         {
    //             std::cout << *it << " ";
    //         }
    //     };
    // lambda(V, 4, 2);
}