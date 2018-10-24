#include <vector>
#include <cstdlib>
#include <ctime>
#include <iostream>
#include <iomanip>
#include <algorithm>
#include <chrono>

class Matrix
{
    private:
    std::vector<std::vector<double>> elements;
    int size;
    
    public:
    Matrix(int p) : 
        size(p),
        elements(std::vector<std::vector<double>>(p, std::vector<double>(p)))
    {
        for (auto& row : elements)
        {
            std::generate(row.begin(), row.end(),
            []()
            {
                return ((double) rand() / (RAND_MAX) * 1.5) + 0.5;
            });
        }
    }

    int getSize() const
    {
        return size;
    }

    double getElement(int i, int j) const
    {
        return elements[i][j];
    }

    double& setElement(int i, int j)
    {
        return elements[i][j];
    }

    void print()
    {
        std::cout << std::fixed;
        std::cout << std::setprecision(3);
        for(auto rows : elements)
        {
            for(auto col : rows)
            {
                std::cout << col << " "; 
            }
            std::cout << std::endl;
        }
    }
};

Matrix multiple(const Matrix& a, const Matrix& b)
{
    int i, j, k;
    int s = a.getSize();
    Matrix res = Matrix(s);
    for (i = 0; i < s; i++) 
    { 
        for (j = 0; j < s; j++) 
        { 
            for (k = 0; k < s; k++)
            {
                res.setElement(i,j) += a.getElement(i, k) * b.getElement(j, k);
            } 
        } 
    }
    return res;
}

double countTime(int size, int n)
{
    double time = 0;
    auto start = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < n; ++i)
    {
        Matrix* a = new Matrix(size);
        Matrix res = multiple(*a, *a);
        delete a;
    }
    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed_time = end - start;
    return elapsed_time.count() / n;
}

int main()
{
    Matrix* test = new Matrix(10);
    test->print();
    std::cout << std::fixed;
    std::cout << std::setprecision(8);
    std::cout << "Średni czas podnoszenia macierzy 10x10 do kwadratu przy 1000 prób: " << countTime(10, 1000) << "s" << std::endl;
    std::cout << "Średni czas podnoszenia macierzy 100x100 do kwadratu przy 100 próbach: " << countTime(100, 100) << "s" << std::endl;
    // std::cout << "Czas podnoszenia macierzy 1000x1000 do kwadratu: " << countTime(1000, 1) << "s" << std::endl; // ~40s
    // std::cout << "Czas podnoszenia macierzy 10000x10000 do kwadratu: " << countTime(10000, 1) << "s" << std::endl;

}