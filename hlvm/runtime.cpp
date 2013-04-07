#include <cstdio>
#include <cstdlib>
#include <sys/time.h>
#include <pthread.h>
#include <unistd.h>
#include <map>

using namespace std;

pthread_mutex_t mutex;
map<int, int> sizes;

void add_size(void *key, int val) {
  pthread_mutex_lock(&mutex);
  sizes[long(key)] = val;
  pthread_mutex_unlock(&mutex);
}

int get_size(void *key) {
  pthread_mutex_lock(&mutex);
  int size = sizes[long(key)];
  pthread_mutex_unlock(&mutex);
  return size;
}

extern "C" {
  const bool debug = false;

  void *hlvm_alloc(int n, int m) {
    if (debug)
      printf("hlvm_alloc(%d, %d)\n", n, m);
    if (n*m == 0) {
      if (debug)
	printf("hlvm_alloc(%d, %d) -> %p\n", n, m, (void *)NULL);
      return 0;
    }
    //void *data = calloc(n, m);
    void *data = malloc(n*m);
    if (data == 0) {
      printf("Out of memory\n");
      exit(1);
    }
    if (debug)
      printf("hlvm_alloc(%d, %d) -> %p\n", n, m, data);
    if (debug)
      add_size(data, n*m);
    return data;
  }

  void hlvm_free(void *data) {
    if (data != 0) {
      int size = 0;
      if (debug)
        size = get_size(data);
      if (debug)
        sizes.erase(long(data));
      free(data);
    }
  }

  pthread_attr_t attr;
  pthread_key_t key;
  __thread void *local;

  void hlvm_init() {
    if (debug) {
      printf("hlvm_init()\n");
      printf("Initializing pthread attribute...\n");
    }
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
    if (debug)
      printf("Creating pthread key...\n");
    key = pthread_key_create(&key, NULL);
    pthread_mutex_init(&mutex, NULL);
    if (debug)
      printf("hlvm_init() ends\n");
  }

  void *hlvm_create_thread(void *(*f)(void *), void *x) {
    if (debug)
      printf("hlvm_create_thread(%p, %p)\n", f, x);
    pthread_t *thread = (pthread_t *)hlvm_alloc(1, sizeof(pthread_t));
    pthread_create(thread, &attr, f, x);
    if (debug)
      printf("hlvm_create_thread(%p, %p) -> %p\n", f, x, thread);
    return (void *)thread;
  }

  void hlvm_join_thread(pthread_t *thread) {
    if (debug)
      printf("hlvm_join_thread(%p)\n", thread);
    pthread_join(*thread, NULL);
    hlvm_free(thread);
    if (debug)
      printf("hlvm_join_thread(%p) done\n", thread);
  }

  void *hlvm_create_mutex() {
    if (debug)
      printf("hlvm_create_mutex()\n");
    pthread_mutex_t *mutex =
      (pthread_mutex_t *)hlvm_alloc(1, sizeof(pthread_mutex_t));
    pthread_mutex_init(mutex, NULL);
    if (debug)
      printf("hlvm_create_mutex() -> %p\n", mutex);
    return (void *)mutex;
  }

  void hlvm_lock_mutex(pthread_mutex_t *mutex) {
    pthread_mutex_lock(mutex);
  }

  void hlvm_unlock_mutex(pthread_mutex_t *mutex) {
    pthread_mutex_unlock(mutex);
  }

  void *hlvm_get_thread_local() {
    void *tl = pthread_getspecific(key);
    //printf("hlvm_get_thread_local() -> %p\n", tl);
    return tl;
  }

  void hlvm_set_thread_local(void *ptr) {
    if (debug)
      printf("hlvm_set_thread_local(%p)\n", ptr);
    pthread_setspecific(key, ptr);
    if (debug)
      printf("hlvm_set_thread_local(%p) done\n", ptr);
  }

  // If *ptr is oldval then replace with newval, returning the old *ptr
  int hlvm_cas(int *ptr, int oldval, int newval) {
    return __sync_val_compare_and_swap(ptr, oldval, newval);
  }

  double hlvm_time() {
    struct timeval t;
    gettimeofday(&t, NULL);
    return (double)t.tv_sec + 1e-6 * (double)t.tv_usec;
  }
}
