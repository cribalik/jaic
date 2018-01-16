template<class T>
struct Array {
	T *data;
	int size,cap;

	T& operator[](int i) {return data[i];}
	const T& operator[](int i) const {return data[i];}
	operator T*() {return data;}
};

template<class T>
T* array_last(Array<T> a) {
	return a.data+a.size-1;
}

template<class T>
void array_push(Array<T> &a, T val) {
	if (a.size == a.cap) {
		int newcap = a.cap ? a.cap*2 : 1;
		a.data = (T*)realloc(a.data, newcap * sizeof(T));
		a.cap = newcap;
	}
	a.data[a.size++] = val;
}

template<class T>
void array_remove(Array<T> a, int i) {
	a.data[i] = a.data[a.size-1];
	--a.size;
}

template<class T>
T* array_pushn(Array<T> &a, int n) {
	if (a.size+n >= a.cap) {
		int newcap = a.cap ? a.cap*2 : 1;
		while (newcap < a.size+n)
			newcap *= 2;
		a.data = (T*)realloc(a.data, newcap * sizeof(T));
		a.cap = newcap;
	}
	a.size += n;
	return a.data + a.size - n;
}

#define array_find(a, ptr, expr) {for ((ptr) = (a).data; (ptr) < (a).data+(a).size; ++(ptr)) {if (expr) break;} if ((ptr) == (a).data+(a).size) {(ptr) = 0;}}
#define array_foreach(a, ptr) for ((ptr) = (a).data; (ptr) && (ptr) < (a).data+(a).size; ++(ptr))
