var sum = 0;
var digit = 0;
var temp = 0;

for (i = 1; i < 100000; i++)
{
  temp = i;
  while(temp > 0)
  {
    digit = temp % 10;
    if (i % digit != 0)
    {
      break;  
    }
    sum += digit;
    temp = Math.floor(temp / 10);
  }

  if (temp == 0  && i % sum == 0)
  {
    console.log(i);
  }
  sum = 0;
}