#include <stdio.h>
#include <inttypes.h>
#include <vector>

constexpr int MAX_WEIGHT = 100001;
constexpr int COINS = 1000;

struct CoinSum{
    int32_t coin;
    long long int money;
};

int main()
{
    std::vector<CoinSum> result;
    std::vector<int> mass;
    std::vector<int> value;
    mass.reserve(COINS);
    result.reserve(MAX_WEIGHT);
    int weight, numberOfCoins;
    int v, m;
    scanf("%d, %d", &mass, &numberOfCoins);

    for (int i = 0; i < numberOfCoins; ++i)
    {
        scanf ("%d %d", &v, &m);
        mass.push_back(m);
        value.push_back(v);

    }
    CoinSum temp;
    temp.coin = -1;
    temp.money = -1;
    result.push_back(temp);
    for (int i = 1; i <= weight; ++i)
    {
        result.push_back(temp);
        result[i].coin = 0;
        result[i].money = MAX_WEIGHT + 1;
    }

    for (int i = 0; i < numberOfCoins; ++i)
    {
        if (result[mass[i]].money > value[i])
        {
            result[mass[i]].money = value[i];
            result[mass[i]].coin = i;
        }
    }

    for (int i = 0; i <= weight; ++i)
    {
        long long int money = result[i].money;
        if (money > 0)
        {
            for (int j = 0; j < numberOfCoins; ++j)
            {
                if (i + mass[j] <= weight && result[i + mass[j]].money > money + value[j])
                {
                    result[i + mass[j]].coin = j;
                    result[i + mass[j]].money = money + value[i];
                }
            }
        }
    }

    if (result[weight].money == MAX_WEIGHT + 1)
    {
        printf("NIE");
        return 0;
    }
    printf("xD");
    return 0;
}