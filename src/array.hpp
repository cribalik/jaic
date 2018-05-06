#ifndef ARRAY_HPP
#define ARRAY_HPP

#ifndef ARRAY_INITIAL_SIZE
  #define ARRAY_INITIAL_SIZE 4
#endif

#ifndef ARRAY_REALLOC
  #include <stdlib.h>
  #define ARRAY_REALLOC realloc
#endif

#ifndef ARRAY_FREE
  #include <stdlib.h>
  #define ARRAY_FREE free
#endif

template<class T>
struct ArrayIter {
  T *t, *end;
};

template<class T>
struct Array {
	typedef T* Iterator;

	T *items;
	int size,cap;

	T& operator[](int i) {return items[i];}
	const T& operator[](int i) const {return items[i];}
};

template<class T>
T* array_last(Array<T> a) {
	return a.items+a.size-1;
}

template<class T>
void array_push(Array<T> &a, T val) {
	array_pushn(a, 1);
	a.items[a.size-1] = val;
}

template<class T>
T array_pop(Array<T> &a) {
	return a.items[--a.size];
}

template<class T>
void array_insertz(Array<T> &a, int i) {
	array_insert(a, i, T());
}

template<class T>
void array_insert(Array<T> &a, int i, T value) {
	array_pushn(a, 1);
	memmove(a.items+i+1, a.items+i, (a.size-i)*sizeof(T));
	a.items[i] = value;
}

template<class T>
void array_resize(Array<T> &a, int newsize) {
	if (newsize > a.size)
		array_pushn(a, newsize-a.size);
	a.size = newsize;
}

template<class T>
void array_reserve(Array<T> &a, int size) {
	int oldsize = a.size;
	if (size > a.size)
		array_pushn(a, size-a.size);
	a.size = oldsize;
}

template<class T>
void array_push(Array<T> &a, T *items, int n) {
	array_pushn(a, n);
	memmove(a.items+a.size-n, items, n*sizeof(T));
}

template<class T>
void array_remove(Array<T> &a, int i) {
	a.items[i] = a.items[a.size-1];
	--a.size;
}

template<class T>
void array_remove_slown(Array<T> &a, int i, int n) {
	memmove(a.items+i, a.items+i+n, (a.size-i-n)*sizeof(T));
	a.size -= n;
}

template<class T>
void array_remove_slow(Array<T> &a, int i) {
	memmove(a.items+i, a.items+i+1, (a.size-i-1)*sizeof(T));
	--a.size;
}

template<class T>
void array_insertn(Array<T> &a, int i, int n) {
	array_pushn(a, n);
	memmove(a.items+i+n, a.items+i, (a.size-i-n)*sizeof(T));
}

template<class T>
void array_pushz(Array<T> &a) {
	array_pushn(a, 1);
	a.items[a.size-1] = T();
}

template<class T>
void array_inserta(Array<T> &a, int i, const T *items, int n) {
	array_pushn(a, n);
	memmove(a.items+i+n, a.items+i, (a.size-i-n)*sizeof(T));
	memcpy(a.items+i, items, n*sizeof(T));
}

template<class T>
T* array_pushn(Array<T> &a, int n) {
	if (a.size+n >= a.cap) {
		int newcap = a.cap ? a.cap*2 : 1;
		while (newcap < a.size+n)
			newcap *= 2;
		a.items = (T*)ARRAY_REALLOC(a.items, newcap * sizeof(T));
		a.cap = newcap;
	}
	a.size += n;
	return a.items + a.size - n;
}

template<class T>
void array_pusha(Array<T> &a, T *items, int n) {
	array_pushn(a, n);
	memcpy(a.items+a.size-n, items, n*sizeof(T));
}

template<class T>
void array_zero(Array<T> &a) {
	memset(a.items, 0, sizeof(T)*a.size);
}

template<class T>
void array_zero(Array<T> &a, int from, int n) {
	memset(a.items + from, 0, sizeof(T)*n);
}

template<class T>
void array_free(Array<T> &a) {
	if (a.items)
		ARRAY_FREE(a.items);
	a.items = 0;
	a.size = a.cap = 0;
}

#ifndef array_find
#define array_find(a, ptr, expr) { \
	for ((ptr) = (a).items; (ptr) < (a).items+(a).size; ++(ptr)) \
		if (expr) \
			break; \
	if ((ptr) == (a).items+(a).size) \
	  (ptr) = 0; \
}
#endif /*array_find*/

#ifndef array_foreach
#define array_foreach(a, ptr) for ((ptr) = (a).items; (ptr) && (ptr) < (a).items+(a).size; ++(ptr))
#endif /*array_foreach*/

#ifndef array_copy
#define array_copy(a, dest) memcpy(dest, (a).items, (a).size*sizeof(*(a).items))
#endif /*array_copy*/

template<class T>
static ArrayIter<T> iter(const Array<T> &a) {
  return {a.items, a.items+a.size};
}

template<class T>
static T* next(ArrayIter<T> &i) {
	if (i.t == i.end) return 0;
  return i.t++;
}

template<class T>
T* begin(Array<T> &a) {
	return a.items;
}

template<class T>
T* end(Array<T> &a) {
	return a.items + a.size;
}

#ifndef For
#define For(container) decltype(container)::Iterator it; for(auto _iterator = iter(container); (it = next(_iterator));)
#endif

#endif