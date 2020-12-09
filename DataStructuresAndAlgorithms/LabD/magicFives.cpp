#include <stdio.h>
#include <inttypes.h>
#include <algorithm>
#include <vector>
#include <math.h> 

void insertionSort(int32_t *values, int size)
{
    int i = 1;
    int j = 0;
    while (i < size)
    {
        int x = values[i];
        j = i - 1;
        while (j >= 0 && values[j] > x)
        {
            values[j+1] = values[j];
            --j;
        } 
        values[j+1] = x;
        ++i;
    }

}
int32_t findKthElement(std::vector<int32_t>& values, unsigned int k)
{
    int size = values.size();

    if (size <= 5)
    {
        insertionSort(&values[0], size);
        return values[k - 1];
    }
    else
    {
        int mod;
        if (size > 5 && size % 5 != 0) mod = 5 - (size % 5);
        else mod = 0;
        size += mod;
        int sets = size / 5;
        std::vector<int32_t> medians(sets);
        for (int i = 0; i < mod; ++i)
        {
            values.push_back(INT32_MAX - 1);
        }
        for (int i = 0; i < sets; ++i)
        {
            insertionSort(&values[5*i], 5);
            medians[i] = values[5*i + 2];
        }
        int m = findKthElement(medians, ceil(medians.size() / 2));
        std::vector<int32_t> smaller;
        std::vector<int32_t> equal;
        std::vector<int32_t> greater;
        for (int i = 0; i < size; ++i)
        {
            if (values[i] < m)
            {
                smaller.push_back(values[i]);
            }
            else if (values[i] == m)
            {
                equal.push_back(values[i]);
            }
            else
            {
                greater.push_back(values[i]);
            }
        }
    
        if (k <= smaller.size()) return findKthElement(smaller, k);
        else if ( k <= (smaller.size() + equal.size())) return m;
        else return findKthElement(greater, k - smaller.size() - equal.size());
    }
    
}

int main()
{
    int n, k;

    scanf ("%d %d", &n, &k);
    int mod;
    if (n > 5 && n % 5 != 0) mod = 5 - (n % 5);
    else mod = 0;
        
    std::vector<int32_t> values(n + mod);
    // int32_t values[n];

    for (int i = 0; i < n; ++i)
    {
        scanf("%d", &values[i]);
    }

    for (int i = 0; i < mod; ++i)
    {
        values[n+i] = INT32_MAX - 1;
    }
    
    int position = findKthElement(values, k);
    printf("%d", position);
    return 0;
}