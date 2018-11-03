#include <iostream>
#include <cstdio>
#include <algorithm>


struct Node
{
    long long data;
    int height;
    Node *leftChild;
    Node *rightChild;
    Node(long long& value)
    {
        data = value;
        height = 0;
        leftChild = nullptr;
        rightChild = nullptr;
    }
};

class Tree
{
    public:
    Tree();
    ~Tree();

    Node* root;
    void ins(long long& data);
    bool rem(long long& data);
    static bool res;
    Node* up(long long& data);
    Node* low(long long& data);
    static Node* insert(Node* node, long long& data);
    static Node* remove(Node* node, long long& data);
    static Node* upper(Node* node, long long& data);
    static Node* lower(Node* node, long long& data);
    static Node* minNode(Node* node);
    static Node* singleLR(Node* node);
    static Node* singleRR(Node* node);
    static Node* doubleLR(Node* node);
    static Node* doubleRR(Node* node);
    static void del(Node* node);
 

};

bool Tree::res = true;

int nodeHeight(Node* node)
{
    if (node) return node->height;
    else return -1;
}

Tree::Tree()
{
    root = nullptr;
}
Tree::~Tree()
{
    del(root);
}


void Tree::ins(long long& data)
{
    root = insert(root, data);
}

bool Tree::rem(long long& data)
{
    res = true;
    root = remove(root, data);
    return res;
}

Node* Tree::up(long long& data)
{
    return upper(root, data);
}

Node* Tree::low(long long& data)
{
    return lower(root, data);
}

void Tree::del(Node* node)
{
    if (node)
    {
        del(node->leftChild);
        del(node->rightChild);
        delete node;
    }
}

Node* Tree::insert(Node* node, long long& data)
{
    if (!node)
    {
        return new Node(data);
    }
    else if (data < node->data)
    {
        node->leftChild = insert(node->leftChild, data);
        if (nodeHeight(node->leftChild) - nodeHeight(node->rightChild) > 1)
        {
            if (data < node->leftChild->data) node = singleRR(node);
            else node = doubleRR(node);
        }
    }
    else if (data > node->data)
    {
        node->rightChild = insert(node->rightChild, data);
        if (nodeHeight(node->rightChild) - nodeHeight(node->leftChild) > 1)
        {
            if (data > node->rightChild->data) node = singleLR(node);
            else node = doubleLR(node);
        }
    }
    if (node) node->height = std::max(nodeHeight(node->leftChild), nodeHeight(node->rightChild)) + 1;
    return node;
}



Node* Tree::singleLR(Node* node)
{
    Node* temp = node->rightChild;
    node->rightChild = temp->leftChild;
    temp->leftChild = node;
    node->height = std::max(nodeHeight(node->leftChild), nodeHeight(node->rightChild)) + 1;
    temp->height = std::max(nodeHeight(temp->leftChild), nodeHeight(temp->leftChild)) + 1;
    return temp;
}

Node* Tree::singleRR(Node* node)
{
    Node* temp = node->leftChild;
    node->leftChild = temp->rightChild;
    temp->rightChild = node;
    node->height = std::max(nodeHeight(node->leftChild), nodeHeight(node->rightChild)) + 1;
    temp->height = std::max(nodeHeight(temp->leftChild), nodeHeight(temp->leftChild)) + 1;
    return temp;
}

Node* Tree::doubleLR(Node* node)
{
    node->rightChild = singleRR(node->rightChild);
    return singleLR(node);
}

Node* Tree::doubleRR(Node* node)
{
    node->leftChild = singleLR(node->leftChild);
    return singleRR(node);
}

Node* Tree::remove(Node* node, long long& data)
{
    Node* temp;
    if (!node)
    {
        res = false;
        return nullptr;
    }
    if (data > node->data) node->rightChild = remove(node->rightChild, data);
    else if (data < node->data) node->leftChild = remove(node->leftChild, data);
    else if (node->leftChild && node->rightChild)
    {
        temp = minNode(node->rightChild);
        node->data = temp->data;
        node->rightChild = remove(node->rightChild, node->data);
    }
    else
    {
        temp = node;
        if (!node->leftChild) node = node->rightChild;
        else if (!node->rightChild) node = node->leftChild;
        delete temp;
    }

    if(!node) return node;
    node->height = std::max(nodeHeight(node->leftChild), nodeHeight(node->rightChild)) + 1;

    if (nodeHeight(node->leftChild) - nodeHeight(node->rightChild) > 1)
    {
        if (nodeHeight(node->leftChild->leftChild) - nodeHeight(node->leftChild->rightChild) > -1)
        return singleRR(node);
        else return doubleRR(node);
    }
    else if(nodeHeight(node->rightChild) - nodeHeight(node->leftChild) > 1 )
    {
        if (nodeHeight(node->rightChild->rightChild) - nodeHeight(node->rightChild->leftChild) > -1)
        return singleLR(node);
        else return doubleLR(node);
    }
    return node;
}


Node* Tree::minNode(Node* node)
{
    while (node->leftChild) node = node->leftChild;
    return node;
}


Node* Tree::upper(Node* node, long long& data)
{
    if (!node) return nullptr;
    Node* temp = nullptr;
    if (node->data > data)
    {
        temp = upper(node->leftChild, data);
        if (temp) return temp;
        else return node;
    }
    else if (node->data < data)
    {
        temp = upper(node->rightChild, data);
    }
    else return node;
    return temp;

}

Node* Tree::lower(Node* node, long long& data)
{
    if (!node) return nullptr;
    Node* temp;
    if (node->data < data)
    {
        temp = lower(node->rightChild, data);
        if (temp) return temp;
        else return node;
    }
    else if (node->data > data)
    {
        temp = lower(node->leftChild, data);
    }
    else return node;
    return temp;
}

void display(Node *t)
{
    if(t == nullptr)
         return;
    display(t->leftChild);
    std::cout << t->data << " ";
    display(t->rightChild);
}

int main()
{
    Tree AVL;
    char ch;
    char line[24];
    long long v;
    bool b;
    Node* tmp;
    int N;
    fgets(line, sizeof(line), stdin);
    sscanf(line, "%d", &N);
    for (int i = 0; i < N; i++)
    {
        fgets(line, sizeof(line), stdin);
        sscanf(line, "%c %lld", &ch, &v);

        if (ch == 'I')
        {
            AVL.ins(v);
        }
        if (ch == 'D')
        {
            b = AVL.rem(v);
            if (!b) printf("BRAK\n");
            else printf("OK\n");
        }
        else if (ch == 'U')
        {
            tmp = AVL.up(v);
            if (!tmp) printf("BRAK\n");
            else 
            {
                printf("%lld", tmp->data);
                printf("\n");
            }
        }
        else if (ch == 'L')
        {
            tmp = AVL.low(v);
            if (!tmp) printf("BRAK\n");
            else 
            {
                printf("%lld", tmp->data);
                printf("\n");
            }
        }
                //display(AVL.root);
    }
    return 0;
}
