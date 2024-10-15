class longque {
public:
    void init(int size);
    void finish();
    //1 producer-consumer safe
    void insert(long l) {   
            buff[tail] = l;
            count++;
            tail++;
            if (tail == max) tail = 0;
    }
    long remove();
    bool fullp() { 		
        return count >= max; 
    }
    bool emptyp() { 
        return count <= 0; 
    }
protected:
    int max;
    long *buff;
    int head;
    int tail;
    int count;
};



