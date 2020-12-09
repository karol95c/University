#include <stdio.h>
#include <vector>
#include <algorithm>

struct Point
{
    int x;
    int y;
    int value;
};

std::vector<Point> points;
bool compareRowColumn(const Point &a, const Point &b)
{
    return a.y < b.y || (a.y == b.y && a.x < b.x);
}

bool compareColumnRow(const Point &a, const Point &b)
{
    return a.x < b.x || (a.x == b.x && a.y < b.y);
}

int getMax(std::vector<int64_t>& result, int t, std::vector<int>& prev)
{
    int max = -1;
    int idx = 0;
    for (int i = 0; i < t; ++i)
    {
        if (result[i] > max)
        {
            idx = i;
            max = result[i];
        }
    }

    prev[t] = max >= 0 ? idx : -1;
    return max > 0 ? max : 0;
}

int main()
{
    int m, n, k;
    int scanfRes = scanf("%d %d", &m, &n);
    scanfRes = scanf("%d", &k);
    
    points.reserve(k + 1);
    std::vector<int> rabbitPath;
    rabbitPath.reserve(k + 1);
    std::vector<int64_t> result = std::vector<int64_t>(k + 1, -1);
    std::vector<int> N;
    std::vector<int> prev = std::vector<int>(k + 1, -1);
    N.reserve(k + 1);

    Point temp;
    bool corner = false;
    for (int i = 0; i < k; ++i)
    {
        scanfRes = scanf("%d %d %d", &(temp.x), &(temp.y), &(temp.value));
        if(temp.x == m && temp.y == n)
        {
            corner = true;
        }
        points.push_back(temp);
        N.push_back(i);
    }
    if (!corner)
    {
        temp.x = m;
        temp.y = n;
        temp.value = 0;
        points.push_back(temp);
        points[k] = temp;
        N[k] = k;
        k++;
    }

    (void)scanfRes;
    std::sort(points.begin(), points.end(),  compareRowColumn);
    std::sort(N.begin(), N.end(), 
        [&](const int& a, const int& b) -> bool
        { 
            return compareColumnRow(points[a], points[b]);
        });

    for (int j = 0; j < k; ++j)
    {
        result[N[j]] = points[N[j]].value + getMax(result, N[j], prev);
    }

    prev[0] = -1;
    const int res = k - 1;
    int p = res;
    while(p!=-1){
        rabbitPath.push_back(p);
		p=prev[p];
	}

    printf("%ld\n", result[res]);

    const int end = corner ? 0 : 1;
    const int size = rabbitPath.size() - end;
    printf("%d\n", static_cast<int>(size));

    const int forSize = corner ? size - 1 : size;
    for (int i = forSize; i >= end; i--)
    {
        printf("%d %d\n", points[rabbitPath[i]].x, points[rabbitPath[i]].y);
    }
    return 0;
}