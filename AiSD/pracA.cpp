// pracowniaA.cpp : Defines the entry point for the console application.
//

//#include "stdafx.h"
#include <iostream>
#include <cstdio>
#include <list>
#include <algorithm>



struct node
{
	int time_in;
	int time_out;
	int child;
	int current;
	int next;

	node()
	{
		time_in = -1;
		time_out = -1;
		child = -1;
		current = -1;
		next = -1;
	}

};

node* Tree;
int counter;

void visit(int i)
{
	int temp;
	Tree[i].time_in = counter;
	temp = Tree[i].child;
	while (temp != -1)
	{
		if (Tree[temp].time_in == -1)
		{
			counter++;
			visit(temp);
		}

		temp = Tree[temp].next;

	}
	counter++;
	Tree[i].time_out = counter;


}

void isPosterity(int v, int u)
{
	if (Tree[u].time_in >= Tree[v].time_in && Tree[u].time_out <= Tree[v].time_out) printf("TAK\n");
	else printf("NIE\n");
}

void loadTree()
{
	int n, q;
	scanf("%d %d", &n, &q);
	Tree = new node[n];
	for (int j = 0; j < n; j++)
	{
		Tree[j] = node();
	}

	counter = 0;

	for (int i = 1; i < n; i++)
	{
		int curr, c;
		int t;
		scanf("%d", &t);
		c = Tree[t - 1].child;
		if (c != -1)
		{
			curr = Tree[t - 1].current;
			Tree[curr].next = i;
			Tree[t - 1].current = i;
		}
		else
		{
			Tree[t - 1].child = i;
			Tree[t - 1].current = i;
		}
	}
	visit(0);
	
	for (int j = 0; j < q; j++)
	{
		int a, b;
		scanf("%d %d", &a, &b);
		isPosterity(a - 1, b - 1);
	}

	delete[] Tree;
}



int main()
{
	loadTree();
	return 0;
}
