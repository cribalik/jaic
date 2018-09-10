#include <cstddef>
#include <cstdlib>

template<int DEFAULT_BLOCK_SIZE>
struct StackAllocator {
  struct Block {
    int n;
    int size;
    Block *next;
    Block *prev;
    std::max_align_t data;
  };
  Block *current;
  Block *first;

  void init() {
    first = current = new Block;
    current->n = 0;
    current->next = 0;
    current->prev = 0;
  }

  template<class T>
  T* push() {
    return (T*)push(sizeof(T));
  }

  template<class T>
  T* push_array(int n) {
    return (T*)push(sizeof(T)*n);
  }

  template<class T>
  void pop() {
    pop(sizeof(T));
  }

  template<class T>
  void pop_array(int n) {
    pop(sizeof(T)*n);
  }

  void* push(int size) {
    size = (size - 1)/sizeof(std::max_align_t) + 1;
    if (current->n + size > current->size) {
      Block *prev = current;
      int new_size = size > DEFAULT_BLOCK_SIZE ? size*2 : DEFAULT_BLOCK_SIZE;
      current = current->next = (Block*)malloc(offsetof(Block, data) + sizeof(std::max_align_t) * new_size);
      current->n = 0;
      current->size = new_size;
      current->next = 0;
      current->prev = prev;
    }
    void *p = &current->data + current->n;
    current->n += size;
    return p;
  }

  void pop(int size) {
    current->n -= (size - 1)/sizeof(std::max_align_t) + 1;;
    if (!current->n) {
      current = current->prev;
      ::free(current->next);
    }
  }

  void free() {
    Block *b = first;
    while (b) {
      Block *next = b->next;
      ::free(b);
      b = next;
    }
  }
};

template<class T, int BLOCK_SIZE>
struct SlotAllocator {
  union Slot {
    T value;
    Slot *next;
  };
  struct Block {
    size_t n;
    Block *next;
    union {void *next; T value;} data[BLOCK_SIZE];
  };

  Block *current;
  Block *first;
  Slot *free_list;

  void init() {
    free_list = 0;
    first = current = new Block;
    current->n = 0;
    current->next = 0;
  }

  void free() {
    Block *b = first;
    while (b) {
      Block *next = b->next;
      delete b;
      b = next;
    }
  }

  T* get() {
    T* t = &current->data[current->n++].value;
    if (current->n == BLOCK_SIZE) {
      current = current->next = new Block;
      current->n = 0;
      current->next = 0;
    }
    return t;
  }

  void put(T* t) {
    ((Slot*)t)->next = free_list;
    free_list = (Slot*)t;
  }
};
