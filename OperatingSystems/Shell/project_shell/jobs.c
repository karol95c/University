#include "shell.h"

typedef struct proc {
  pid_t pid;    /* process identifier */
  int state;    /* RUNNING or STOPPED or FINISHED */
  int exitcode; /* -1 if exit status not yet received */
} proc_t;

typedef struct job {
  pid_t pgid;            /* 0 if slot is free */
  proc_t *proc;          /* array of processes running in as a job */
  struct termios tmodes; /* saved terminal modes */
  int nproc;             /* number of processes */
  int state;             /* changes when live processes have same state */
  char *command;         /* textual representation of command line */
} job_t;

static job_t *jobs = NULL;          /* array of all jobs */
static int njobmax = 1;             /* number of slots in jobs array */
static int tty_fd = -1;             /* controlling terminal file descriptor */
static struct termios shell_tmodes; /* saved shell terminal modes */

static void sigchld_handler(int sig) {
  safe_printf("sigchld_handler: %d\n", sig);
  int old_errno = errno;
  pid_t pid;
  int status;
  /* TODO: Change state (FINISHED, RUNNING, STOPPED) of processes and jobs.
   * Bury all children that finished saving their status in jobs. */
  int i = 0;
  pid_t child;
  int stat;
  child = waitpid(-1, &stat, WUNTRACED | WCONTINUED | WNOHANG);
  job_t *job = jobs;
  int stopped = 0;
  int finished = 0;
  int j = 0;
  safe_printf("sigchld_handler child %d\n", child);

  while (child > 0) {
    for (i = 0; i < njobmax; ++i) {
      job = &jobs[i];
      for (j = 0; j < job->nproc; ++j) {
        if (job->proc[j].pid == child) {
          safe_printf("sigchld_handler found pid %d\n", child);
          job->proc[j].exitcode = stat;
          if (WIFSTOPPED(stat)) {
            job->proc[j].state = STOPPED;
            // safe_printf("stopped\n");
            safe_printf("pid: %d, stopped\n", child);
          } else if (WIFCONTINUED(stat)) {
            if (job->state != FINISHED) {
              job->proc[j].state = RUNNING;
              job->state = RUNNING;
            }
            // safe_printf("continue\n");
            safe_printf("pid: %d, continue\n", child);
          } else if (WIFSIGNALED(status)) {
            job->proc[j].state = FINISHED;
            safe_printf("pid: %d, signaled\n", child);
          } else if (WIFEXITED(status)) {
            job->proc[j].state = FINISHED;
            // safe_printf("finished\n");
            safe_printf("pid: %d, finished\n", child);
          }
        }
        if (STOPPED == job->proc[j].state) {
          ++stopped;
        } else if (FINISHED == job->proc[j].state) {
          ++finished;
        }
      }
      if (stopped == job->nproc) {
        job->state = STOPPED;
      } else if (finished == job->nproc) {
        job->state = FINISHED;
      }
      stopped = 0;
      finished = 0;
    }
    child = waitpid(-1, &stat, WUNTRACED | WCONTINUED | WNOHANG);
  }
  errno = old_errno;
}

/* When pipeline is done, its exitcode is fetched from the last process. */
static int exitcode(job_t *job) { return job->proc[job->nproc - 1].exitcode; }

static int allocjob(void) {
  /* Find empty slot for background job. */
  for (int j = BG; j < njobmax; j++)
    if (jobs[j].pgid == 0)
      return j;

  /* If none found, allocate new one. */
  jobs = realloc(jobs, sizeof(job_t) * (njobmax + 1));
  memset(&jobs[njobmax], 0, sizeof(job_t));
  return njobmax++;
}

static int allocproc(int j) {
  job_t *job = &jobs[j];
  job->proc = realloc(job->proc, sizeof(proc_t) * (job->nproc + 1));
  return job->nproc++;
}

int addjob(pid_t pgid, int bg) {
  int j = bg ? allocjob() : FG;
  job_t *job = &jobs[j];
  /* Initial state of a job. */
  job->pgid = pgid;
  job->state = RUNNING;
  job->command = NULL;
  job->proc = NULL;
  job->nproc = 0;
  job->tmodes = shell_tmodes;
  return j;
}

static void deljob(job_t *job) {
  assert(job->state == FINISHED);
  free(job->command);
  free(job->proc);
  job->pgid = 0;
  job->command = NULL;
  job->proc = NULL;
  job->nproc = 0;
}

static void movejob(int from, int to) {
  assert(jobs[to].pgid == 0);
  memcpy(&jobs[to], &jobs[from], sizeof(job_t));
  memset(&jobs[from], 0, sizeof(job_t));
}

static void mkcommand(char **cmdp, char **argv) {
  if (*cmdp)
    strapp(cmdp, " | ");

  for (strapp(cmdp, *argv++); *argv; argv++) {
    strapp(cmdp, " ");
    strapp(cmdp, *argv);
  }
}

void addproc(int j, pid_t pid, char **argv) {
  assert(j < njobmax);
  job_t *job = &jobs[j];

  int p = allocproc(j);
  proc_t *proc = &job->proc[p];
  /* Initial state of a process. */
  proc->pid = pid;
  proc->state = RUNNING;
  proc->exitcode = -1;
  mkcommand(&job->command, argv);
}

/* Returns job's state.
 * If it's finished, delete it and return exitcode through statusp. */
int jobstate(int j, int *statusp) {
  assert(j < njobmax);
  job_t *job = &jobs[j];
  int state = job->state;

  /* TODO: Handle case where job has finished. */
  if (FINISHED == state) {
    *statusp = exitcode(job);
  }

  return state;
}

char *jobcmd(int j) {
  assert(j < njobmax);
  job_t *job = &jobs[j];
  return job->command;
}

/* Continues a job that has been stopped. If move to foreground was requested,
 * then move the job to foreground and start monitoring it. */
bool resumejob(int j, int bg, sigset_t *mask) {
  if (j < 0) {
    for (j = njobmax - 1; j > 0 && jobs[j].state == FINISHED; j--)
      continue;
  }

  if (j >= njobmax || jobs[j].state == FINISHED)
    return false;

  /* TODO: Continue stopped job. Possibly move job to foreground slot. */
  Kill(jobs[j].pgid, SIGCONT);
  Sigsuspend(mask);
  safe_printf("[%d] continue \'%s\'\n", j, jobcmd(j));
  if (!bg) {
    safe_printf("[%d] continue foreground \'%s\'\n", j, jobcmd(j));
    movejob(j, FG);
    monitorjob(mask);
  } else {
    // while (jobs[j].state != RUNNING)
    // {
    safe_printf("[%d] continue background \'%s\'\n", j, jobcmd(j));
    // }
  }

  return true;
}

/* Kill the job by sending it a SIGTERM. */
bool killjob(int j) {
  if (j >= njobmax || jobs[j].state == FINISHED)
    return false;
  debug("[%d] killing '%s'\n", j, jobs[j].command);

  /* TODO: I love the smell of napalm in the morning. */
  Kill(jobs[j].pgid, SIGTERM);
  Kill(jobs[j].pgid, SIGCONT);

  return true;
}

/* Report state of requested background jobs. Clean up finished jobs. */
void watchjobs(int which) {
  int state = 0;
  int statusp = 0;
  for (int j = BG; j < njobmax; j++) {
    if (jobs[j].pgid == 0)
      continue;

    /* TODO: Report job number, state, command and exit code or signal. */
    state = jobstate(j, &statusp);
    // state = jobs[j].state;

    if (ALL != which && state != which) {
      continue;
    }

    switch (state) {
    case RUNNING:
      safe_printf("[%d] running \'%s\'\n", j, jobcmd(j));
      break;
    case STOPPED:
      safe_printf("[%d] suspended \'%s\'\n", j, jobcmd(j));
      break;
    case FINISHED:
      if (statusp != 0) {
        safe_printf("[%d] killed \'%s\' by signal %d\n", j, jobcmd(j), statusp);
      } else {
        safe_printf("[%d] exited \'%s\', status = %d\n", j, jobcmd(j), statusp);
      }

      deljob(&jobs[j]);
      break;

    default:
      break;
    }
  }
}

/* Monitor job execution. If it gets stopped move it to background.
 * When a job has finished or has been stopped move shell to foreground. */
int monitorjob(sigset_t *mask) {
  int exitcode, state;
  safe_printf("monitorjob\n");
  /* TODO: Following code requires use of Tcsetpgrp of tty_fd. */
  int statusp;
  int i;
  int parent_pgid = getpgrp();
  sigset_t old_mask;
  Tcgetattr(tty_fd, &shell_tmodes);
  Tcsetpgrp(tty_fd, jobs[FG].pgid);
  Tcsetattr(tty_fd, TCSADRAIN, &jobs[FG].tmodes);
  // Signal(SIGTSTP, sigchld_handler);
  // Signal(SIGTSTP, SIG_DFL);
  safe_printf("monitorjob running pid: %d\n",
              jobs[FG].proc[jobs[FG].nproc - 1].pid);
  int start_status = jobs[FG].state;
  while (start_status == jobs[FG].state) {
    Sigsuspend(mask);
  };

  int nproc = jobs[FG].nproc - 1;
  pid_t p2 = jobs[FG].proc[nproc].pid;

  state = jobstate(FG, &statusp);

  if (STOPPED == state) {
    // move to background, move shell to foreground
    int i = allocjob();
    memcpy(&jobs[i], &jobs[FG], sizeof(job_t));
    memset(&jobs[FG], 0, sizeof(job_t));
    // save tmodes when going to background
    Tcgetattr(tty_fd, &jobs[FG].tmodes);
    safe_printf("[%d] suspended \'%s\'\n", i, jobcmd(i));
  } else if (FINISHED == state) {
    exitcode = jobs[FG].proc[jobs[FG].nproc - 1].exitcode;
    deljob(&jobs[FG]);
    // move shell to foreground
  }
  Tcsetattr(tty_fd, TCSADRAIN, &shell_tmodes);
  Tcsetpgrp(tty_fd, parent_pgid);

  return exitcode;
}

/* Called just at the beginning of shell's life. */
void initjobs(void) {
  Signal(SIGCHLD, sigchld_handler);
  jobs = calloc(sizeof(job_t), 1);

  /* Assume we're running in interactive mode, so move us to foreground.
   * Duplicate terminal fd, but do not leak it to subprocesses that execve. */
  assert(isatty(STDIN_FILENO));
  tty_fd = Dup(STDIN_FILENO);
  fcntl(tty_fd, F_SETFD, FD_CLOEXEC);

  /* Take control of the terminal. */
  Tcsetpgrp(tty_fd, getpgrp());

  /* Save default terminal attributes for the shell. */
  Tcgetattr(tty_fd, &shell_tmodes);
}

/* Called just before the shell finishes. */
void shutdownjobs(void) {
  sigset_t mask;
  Sigprocmask(SIG_BLOCK, &sigchld_mask, &mask);

  /* TODO: Kill remaining jobs and wait for them to finish. */
  int i = 0;
  for (int i = 0; i < njobmax; ++i) {
    if (jobs[i].pgid == 0) {
      continue;
    }
    killjob(i);
    while (jobs[i].state != FINISHED) {
      Sigsuspend(&mask);
    }
  }

  watchjobs(FINISHED);

  Sigprocmask(SIG_SETMASK, &mask, NULL);

  Close(tty_fd);
}
