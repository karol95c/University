var isPrime = true;
for (i = 1; i < 100000; i++)
{
    for (j = 2; j < i / 2; j++)
    {
        if (i % j == 0)
        {
            isPrime = false;
            break;
        }
    }
    if (isPrime) console.log(i);
    else isPrime = true;
}