#include <stdio.h>
#include <inttypes.h>
#include <stdio.h>

#define MAX_LEFT 10001

struct brick
{
    int16_t l;
    int16_t m;
    int16_t r;
    brick *next;
    brick *prev;
};

static int queueIndex = 0;

brick *removeNode(brick *b)
{
    if (!b->prev && !b->next)
    {
        return nullptr;
    }

    else if (!b->prev)
    {
        b->next->prev = nullptr;
        return b->next;
    }

    else if (!b->next)
    {
        b->prev->next = nullptr;
        return b->prev;
    }
    else
    {
        b->prev->next = b->next;
        b->next->prev = b->prev;
        return b->next;
    }
}

void removeFromList(brick **classes, brick *b)
{
    if (classes[b->l] == b)
    {
        classes[b->l] = removeNode(b);
    }
    else
    {
        removeNode(b);
    }

    b->prev = nullptr;
    b->next = nullptr;
}

void printResult(brick **bfsQueue)
{
    printf("%d\n", queueIndex);

    for (int i = 0; i < queueIndex; ++i)
    {
        printf("%d %d %d\n", bfsQueue[i]->l, bfsQueue[i]->m, bfsQueue[i]->r);
    }
}

void pushElement(brick **bfsQueue, brick *element)
{
    bfsQueue[queueIndex++] = element;
}

void popElement(brick **bfsQueue)
{
    bfsQueue[queueIndex - 1] = nullptr;
    queueIndex--;
}

brick *backElement(brick **bfsQueue)
{
    return bfsQueue[queueIndex - 1];
}

bool emptyQueue(brick **bfsQueue)
{
    if (queueIndex == 0 && bfsQueue[queueIndex] == nullptr)
    {
        return true;
    }
    return false;
}

void pushQueue(brick **queue, brick *b)
{
    pushElement(queue, b);
}

bool makePavement(brick **classes, brick **bfsQueue, int8_t *zeroBricks)
{
    brick *queueBrick = classes[0];

    removeFromList(classes, queueBrick);
    pushQueue(bfsQueue, queueBrick);

    brick *tempBrick = nullptr;
    brick *nextBrick = nullptr;

    while (!emptyQueue(bfsQueue))
    {
        queueBrick = backElement(bfsQueue);
        if (queueBrick->r == 0)
        {
            printResult(bfsQueue);
            return true;
        }
    
        if (classes[queueBrick->r] != nullptr)
        {
            queueBrick = classes[queueBrick->r];
            if (zeroBricks[queueBrick->l] == 1)
            {
                nextBrick = queueBrick;
                while (nextBrick)
                {
                    if (nextBrick->r == 0)
                    {
                        pushElement(bfsQueue, nextBrick);
                        printResult(bfsQueue);
                        return true;
                    }
                    nextBrick = nextBrick->next;
                }
            }

            removeFromList(classes, queueBrick);
            pushQueue(bfsQueue, queueBrick);
        }
        else
        {
            tempBrick = queueBrick;
            popElement(bfsQueue);
            nextBrick = classes[tempBrick->l];
            while (!nextBrick)
            {
                if (emptyQueue(bfsQueue))
                {
                    return false;
                }

                tempBrick = backElement(bfsQueue);
                popElement(bfsQueue);
                nextBrick = classes[tempBrick->l];
            }
            removeFromList(classes, nextBrick);
            pushElement(bfsQueue, nextBrick);
        }
    }
    return false;
}

void addNewBrickToMap(brick **classes, brick *newBrick, int8_t *zeroBricks, bool *rZero)
{
    newBrick->next = nullptr;

    if (newBrick->r == 0)
    {
        zeroBricks[newBrick->l] = 1;
        *rZero = true;
    }

    if (classes[newBrick->l] != nullptr)
    {
        brick *temp = classes[newBrick->l];
        newBrick->next = temp;
        temp->prev = newBrick;
        classes[newBrick->l] = newBrick;
    }
    else
    {
        classes[newBrick->l] = newBrick;
        newBrick->prev = nullptr;
    }
}

int main()
{
    int n = 0;
    int res = scanf("%d", &n);

    bool rZero = 0;
    brick *newBrick = new brick[n];
    brick **queue = new brick *[n];
    brick *tempBrick = &newBrick[0];
    brick **classes = new brick *[MAX_LEFT]{0};
    int8_t *zeroBricks = new int8_t[MAX_LEFT]{0};

    for (int i = 0; i < n; ++i)
    {
        tempBrick = &newBrick[i];
        res = scanf("%" SCNd16" %" SCNd16" %" SCNd16"\n", &tempBrick->l, &tempBrick->m, &tempBrick->r);

        addNewBrickToMap(classes, tempBrick, zeroBricks, &rZero);
    }

    (void)res;

    if (!classes[0] || !rZero)
    {
        printf("BRAK");
    }
    else if (!makePavement(classes, queue, zeroBricks))
    {
        printf("BRAK");
    }

    return 0;
}