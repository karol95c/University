#include <stdlib.h>
#include <pthread.h>
#include <errno.h>

typedef struct Sem {
    pthread_mutex_t mutex;
    pthread_cond_t waiters;
    int value;
} Sem_t;

int sem_init(Sem_t *s, int pshared, int value) {
    if (pshared) {
        errno = ENOSYS;
        return -1;
    }

    s->value = value;
    pthread_mutex_init(&s->mutex, NULL);
    pthread_cond_init(&s->waiters, NULL);
    return 0;
}

sem_post(Sem_t *s) {
    pthread_mutex_lock(&s->mutex);
    s->value++;
    if (s->value == 1)
        /* incremented from 0 to 1, so someone is waiting */
        pthread_cond_signal(&s->waiters);
    pthread_cond_signal(&s->waiters); /* See note */

    pthread_mutex_unlock(&s->mutex);
}

sem_wait(Sem_t *s) {
    pthread_mutex_lock(&s->mutex);
    while (s->value == 0) {
        pthread_cond_wait(&s->waiters, &s->mutex);
    }
    s->value--;
    pthread_mutex_unlock(&s->mutex);
}

int main(void){


    return 0;
}