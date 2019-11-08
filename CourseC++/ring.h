#ifndef RING_H_
#define RING_H_

#include <iostream>
#include <vector>
#include <initializer_list>

template <typename T>
class Ring
{
    T* mBuffer;
    int mSize;
    int mPos;

    public:
    Ring(int size) : mBuffer(new T[size]), mSize(size), mPos(0) {}
    Ring(std::initializer_list<T> list) : mBuffer(new T[list.size()]), mSize(list.size()), mPos(0)
    {
        for (auto element : list)
        {
            mBuffer[mPos] = element;
            mPos++;
        }
    }

    ~Ring()
    {
        delete[] mBuffer;
    }
    
    class iterator;
    
    void push(T value)
    {
        if(mPos == mSize)
        {
            mPos = 0;
        }
        mBuffer[mPos++] = value;
    }

    void push(std::initializer_list<T> list)
    {
        for (auto element : list)
        {
            push(element);
        }
    }

    int size()
    {
        return mSize;
    }
    
    T& get(int pos)
    {
        return mBuffer[pos];
    }
    
    iterator begin()
    {
        return iterator(0, *this);
    }

    iterator end()
    {
        return iterator(mSize, *this);
    }

};

template <typename T>
class Ring<T>::iterator
{
    int mPos;
    Ring &mRing;
    
    public:
    iterator(int pos, Ring& r) : mPos(pos), mRing(r){}

    iterator &operator++()
    {
        mPos++;
        return *this;
    }

    iterator &operator++(int)
    {
        mPos++;
        return *this;
    }

    T &operator*()
    {
        return mRing.get(mPos);
    }

    bool operator != (const iterator &other) const
    {
        return mPos != other.mPos;
    }
};

#endif // RING_H_