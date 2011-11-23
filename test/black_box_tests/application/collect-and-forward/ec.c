#include <stddef.h>
typedef void (* ThreadExecutionFunction)(void *);
typedef signed char int8_t;
typedef short int int16_t;
typedef int int32_t;
typedef long int int64_t;
typedef unsigned char uint8_t;
typedef unsigned short int uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long int uint64_t;
typedef signed char int_least8_t;
typedef short int int_least16_t;
typedef int int_least32_t;
typedef long int int_least64_t;
typedef unsigned char uint_least8_t;
typedef unsigned short int uint_least16_t;
typedef unsigned int uint_least32_t;
typedef unsigned long int uint_least64_t;
typedef signed char int_fast8_t;
typedef long int int_fast16_t;
typedef long int int_fast32_t;
typedef long int int_fast64_t;
typedef unsigned char uint_fast8_t;
typedef unsigned long int uint_fast16_t;
typedef unsigned long int uint_fast32_t;
typedef unsigned long int uint_fast64_t;
typedef long int intptr_t;
typedef unsigned long int uintptr_t;
typedef long int intmax_t;
typedef unsigned long int uintmax_t;
typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;
typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;
typedef signed long int __int64_t;
typedef unsigned long int __uint64_t;
typedef long int __quad_t;
typedef unsigned long int __u_quad_t;
typedef unsigned long int __dev_t;
typedef unsigned int __uid_t;
typedef unsigned int __gid_t;
typedef unsigned long int __ino_t;
typedef unsigned long int __ino64_t;
typedef unsigned int __mode_t;
typedef unsigned long int __nlink_t;
typedef long int __off_t;
typedef long int __off64_t;
typedef int __pid_t;
typedef struct {
            int __val[2];
        } __fsid_t;
typedef long int __clock_t;
typedef unsigned long int __rlim_t;
typedef unsigned long int __rlim64_t;
typedef unsigned int __id_t;
typedef long int __time_t;
typedef unsigned int __useconds_t;
typedef long int __suseconds_t;
typedef int __daddr_t;
typedef long int __swblk_t;
typedef int __key_t;
typedef int __clockid_t;
typedef void * __timer_t;
typedef long int __blksize_t;
typedef long int __blkcnt_t;
typedef long int __blkcnt64_t;
typedef unsigned long int __fsblkcnt_t;
typedef unsigned long int __fsblkcnt64_t;
typedef unsigned long int __fsfilcnt_t;
typedef unsigned long int __fsfilcnt64_t;
typedef long int __ssize_t;
typedef __off64_t __loff_t;
typedef __quad_t * __qaddr_t;
typedef char * __caddr_t;
typedef long int __intptr_t;
typedef unsigned int __socklen_t;
typedef __ssize_t ssize_t;
typedef long unsigned int size_t;
typedef __gid_t gid_t;
typedef __uid_t uid_t;
typedef __off_t off_t;
typedef __useconds_t useconds_t;
typedef __pid_t pid_t;
typedef __socklen_t socklen_t;
extern int access(const char * __name,
                  int __type) __attribute__((__nothrow__,__nonnull__(1)));
extern int faccessat(int __fd,
                     const char * __file,
                     int __type,
                     int __flag) __attribute__((__nothrow__,__nonnull__(2)));
extern __off_t lseek(int __fd,
                     __off_t __offset,
                     int __whence) __attribute__((__nothrow__));
extern int close(int __fd);
extern ssize_t read(int __fd, void * __buf, size_t __nbytes);
extern ssize_t write(int __fd, const void * __buf, size_t __n);
extern ssize_t pread(int __fd,
                     void * __buf,
                     size_t __nbytes,
                     __off_t __offset);
extern ssize_t pwrite(int __fd,
                      const void * __buf,
                      size_t __n,
                      __off_t __offset);
extern int pipe(int __pipedes[2]) __attribute__((__nothrow__));
extern unsigned int alarm(unsigned int __seconds) __attribute__((__nothrow__));
extern unsigned int sleep(unsigned int __seconds);
extern __useconds_t ualarm(__useconds_t __value,
                           __useconds_t __interval) __attribute__((__nothrow__));
extern int usleep(__useconds_t __useconds);
extern int pause(void);
extern int chown(const char * __file,
                 __uid_t __owner,
                 __gid_t __group) __attribute__((__nothrow__,__nonnull__(1)));
extern int fchown(int __fd,
                  __uid_t __owner,
                  __gid_t __group) __attribute__((__nothrow__));
extern int lchown(const char * __file,
                  __uid_t __owner,
                  __gid_t __group) __attribute__((__nothrow__,__nonnull__(1)));
extern int fchownat(int __fd,
                    const char * __file,
                    __uid_t __owner,
                    __gid_t __group,
                    int __flag) __attribute__((__nothrow__,__nonnull__(2)));
extern int chdir(const char * __path) __attribute__((__nothrow__,__nonnull__(1)));
extern int fchdir(int __fd) __attribute__((__nothrow__));
extern char * getcwd(char * __buf,
                     size_t __size) __attribute__((__nothrow__));
extern char * getwd(char * __buf) __attribute__((__nothrow__,__nonnull__(1),__deprecated__));
extern int dup(int __fd) __attribute__((__nothrow__));
extern int dup2(int __fd, int __fd2) __attribute__((__nothrow__));
extern char * * __environ;
extern int execve(const char * __path,
                  char * const __argv[],
                  char * const __envp[]) __attribute__((__nothrow__,__nonnull__(1, 2)));
extern int fexecve(int __fd,
                   char * const __argv[],
                   char * const __envp[]) __attribute__((__nothrow__,__nonnull__(2)));
extern int execv(const char * __path,
                 char * const __argv[]) __attribute__((__nothrow__,__nonnull__(1, 2)));
extern int execle(const char * __path,
                  const char * __arg, ...) __attribute__((__nothrow__,__nonnull__(1, 2)));
extern int execl(const char * __path,
                 const char * __arg, ...) __attribute__((__nothrow__,__nonnull__(1, 2)));
extern int execvp(const char * __file,
                  char * const __argv[]) __attribute__((__nothrow__,__nonnull__(1, 2)));
extern int execlp(const char * __file,
                  const char * __arg, ...) __attribute__((__nothrow__,__nonnull__(1, 2)));
extern int nice(int __inc) __attribute__((__nothrow__));
extern void _exit(int __status) __attribute__((__noreturn__));
enum {
    _PC_LINK_MAX,
    _PC_MAX_CANON,
    _PC_MAX_INPUT,
    _PC_NAME_MAX,
    _PC_PATH_MAX,
    _PC_PIPE_BUF,
    _PC_CHOWN_RESTRICTED,
    _PC_NO_TRUNC,
    _PC_VDISABLE,
    _PC_SYNC_IO,
    _PC_ASYNC_IO,
    _PC_PRIO_IO,
    _PC_SOCK_MAXBUF,
    _PC_FILESIZEBITS,
    _PC_REC_INCR_XFER_SIZE,
    _PC_REC_MAX_XFER_SIZE,
    _PC_REC_MIN_XFER_SIZE,
    _PC_REC_XFER_ALIGN,
    _PC_ALLOC_SIZE_MIN,
    _PC_SYMLINK_MAX,
    _PC_2_SYMLINKS
};
enum {
    _SC_ARG_MAX,
    _SC_CHILD_MAX,
    _SC_CLK_TCK,
    _SC_NGROUPS_MAX,
    _SC_OPEN_MAX,
    _SC_STREAM_MAX,
    _SC_TZNAME_MAX,
    _SC_JOB_CONTROL,
    _SC_SAVED_IDS,
    _SC_REALTIME_SIGNALS,
    _SC_PRIORITY_SCHEDULING,
    _SC_TIMERS,
    _SC_ASYNCHRONOUS_IO,
    _SC_PRIORITIZED_IO,
    _SC_SYNCHRONIZED_IO,
    _SC_FSYNC,
    _SC_MAPPED_FILES,
    _SC_MEMLOCK,
    _SC_MEMLOCK_RANGE,
    _SC_MEMORY_PROTECTION,
    _SC_MESSAGE_PASSING,
    _SC_SEMAPHORES,
    _SC_SHARED_MEMORY_OBJECTS,
    _SC_AIO_LISTIO_MAX,
    _SC_AIO_MAX,
    _SC_AIO_PRIO_DELTA_MAX,
    _SC_DELAYTIMER_MAX,
    _SC_MQ_OPEN_MAX,
    _SC_MQ_PRIO_MAX,
    _SC_VERSION,
    _SC_PAGESIZE,
    _SC_RTSIG_MAX,
    _SC_SEM_NSEMS_MAX,
    _SC_SEM_VALUE_MAX,
    _SC_SIGQUEUE_MAX,
    _SC_TIMER_MAX,
    _SC_BC_BASE_MAX,
    _SC_BC_DIM_MAX,
    _SC_BC_SCALE_MAX,
    _SC_BC_STRING_MAX,
    _SC_COLL_WEIGHTS_MAX,
    _SC_EQUIV_CLASS_MAX,
    _SC_EXPR_NEST_MAX,
    _SC_LINE_MAX,
    _SC_RE_DUP_MAX,
    _SC_CHARCLASS_NAME_MAX,
    _SC_2_VERSION,
    _SC_2_C_BIND,
    _SC_2_C_DEV,
    _SC_2_FORT_DEV,
    _SC_2_FORT_RUN,
    _SC_2_SW_DEV,
    _SC_2_LOCALEDEF,
    _SC_PII,
    _SC_PII_XTI,
    _SC_PII_SOCKET,
    _SC_PII_INTERNET,
    _SC_PII_OSI,
    _SC_POLL,
    _SC_SELECT,
    _SC_UIO_MAXIOV,
    _SC_IOV_MAX = _SC_UIO_MAXIOV,
    _SC_PII_INTERNET_STREAM,
    _SC_PII_INTERNET_DGRAM,
    _SC_PII_OSI_COTS,
    _SC_PII_OSI_CLTS,
    _SC_PII_OSI_M,
    _SC_T_IOV_MAX,
    _SC_THREADS,
    _SC_THREAD_SAFE_FUNCTIONS,
    _SC_GETGR_R_SIZE_MAX,
    _SC_GETPW_R_SIZE_MAX,
    _SC_LOGIN_NAME_MAX,
    _SC_TTY_NAME_MAX,
    _SC_THREAD_DESTRUCTOR_ITERATIONS,
    _SC_THREAD_KEYS_MAX,
    _SC_THREAD_STACK_MIN,
    _SC_THREAD_THREADS_MAX,
    _SC_THREAD_ATTR_STACKADDR,
    _SC_THREAD_ATTR_STACKSIZE,
    _SC_THREAD_PRIORITY_SCHEDULING,
    _SC_THREAD_PRIO_INHERIT,
    _SC_THREAD_PRIO_PROTECT,
    _SC_THREAD_PROCESS_SHARED,
    _SC_NPROCESSORS_CONF,
    _SC_NPROCESSORS_ONLN,
    _SC_PHYS_PAGES,
    _SC_AVPHYS_PAGES,
    _SC_ATEXIT_MAX,
    _SC_PASS_MAX,
    _SC_XOPEN_VERSION,
    _SC_XOPEN_XCU_VERSION,
    _SC_XOPEN_UNIX,
    _SC_XOPEN_CRYPT,
    _SC_XOPEN_ENH_I18N,
    _SC_XOPEN_SHM,
    _SC_2_CHAR_TERM,
    _SC_2_C_VERSION,
    _SC_2_UPE,
    _SC_XOPEN_XPG2,
    _SC_XOPEN_XPG3,
    _SC_XOPEN_XPG4,
    _SC_CHAR_BIT,
    _SC_CHAR_MAX,
    _SC_CHAR_MIN,
    _SC_INT_MAX,
    _SC_INT_MIN,
    _SC_LONG_BIT,
    _SC_WORD_BIT,
    _SC_MB_LEN_MAX,
    _SC_NZERO,
    _SC_SSIZE_MAX,
    _SC_SCHAR_MAX,
    _SC_SCHAR_MIN,
    _SC_SHRT_MAX,
    _SC_SHRT_MIN,
    _SC_UCHAR_MAX,
    _SC_UINT_MAX,
    _SC_ULONG_MAX,
    _SC_USHRT_MAX,
    _SC_NL_ARGMAX,
    _SC_NL_LANGMAX,
    _SC_NL_MSGMAX,
    _SC_NL_NMAX,
    _SC_NL_SETMAX,
    _SC_NL_TEXTMAX,
    _SC_XBS5_ILP32_OFF32,
    _SC_XBS5_ILP32_OFFBIG,
    _SC_XBS5_LP64_OFF64,
    _SC_XBS5_LPBIG_OFFBIG,
    _SC_XOPEN_LEGACY,
    _SC_XOPEN_REALTIME,
    _SC_XOPEN_REALTIME_THREADS,
    _SC_ADVISORY_INFO,
    _SC_BARRIERS,
    _SC_BASE,
    _SC_C_LANG_SUPPORT,
    _SC_C_LANG_SUPPORT_R,
    _SC_CLOCK_SELECTION,
    _SC_CPUTIME,
    _SC_THREAD_CPUTIME,
    _SC_DEVICE_IO,
    _SC_DEVICE_SPECIFIC,
    _SC_DEVICE_SPECIFIC_R,
    _SC_FD_MGMT,
    _SC_FIFO,
    _SC_PIPE,
    _SC_FILE_ATTRIBUTES,
    _SC_FILE_LOCKING,
    _SC_FILE_SYSTEM,
    _SC_MONOTONIC_CLOCK,
    _SC_MULTI_PROCESS,
    _SC_SINGLE_PROCESS,
    _SC_NETWORKING,
    _SC_READER_WRITER_LOCKS,
    _SC_SPIN_LOCKS,
    _SC_REGEXP,
    _SC_REGEX_VERSION,
    _SC_SHELL,
    _SC_SIGNALS,
    _SC_SPAWN,
    _SC_SPORADIC_SERVER,
    _SC_THREAD_SPORADIC_SERVER,
    _SC_SYSTEM_DATABASE,
    _SC_SYSTEM_DATABASE_R,
    _SC_TIMEOUTS,
    _SC_TYPED_MEMORY_OBJECTS,
    _SC_USER_GROUPS,
    _SC_USER_GROUPS_R,
    _SC_2_PBS,
    _SC_2_PBS_ACCOUNTING,
    _SC_2_PBS_LOCATE,
    _SC_2_PBS_MESSAGE,
    _SC_2_PBS_TRACK,
    _SC_SYMLOOP_MAX,
    _SC_STREAMS,
    _SC_2_PBS_CHECKPOINT,
    _SC_V6_ILP32_OFF32,
    _SC_V6_ILP32_OFFBIG,
    _SC_V6_LP64_OFF64,
    _SC_V6_LPBIG_OFFBIG,
    _SC_HOST_NAME_MAX,
    _SC_TRACE,
    _SC_TRACE_EVENT_FILTER,
    _SC_TRACE_INHERIT,
    _SC_TRACE_LOG,
    _SC_LEVEL1_ICACHE_SIZE,
    _SC_LEVEL1_ICACHE_ASSOC,
    _SC_LEVEL1_ICACHE_LINESIZE,
    _SC_LEVEL1_DCACHE_SIZE,
    _SC_LEVEL1_DCACHE_ASSOC,
    _SC_LEVEL1_DCACHE_LINESIZE,
    _SC_LEVEL2_CACHE_SIZE,
    _SC_LEVEL2_CACHE_ASSOC,
    _SC_LEVEL2_CACHE_LINESIZE,
    _SC_LEVEL3_CACHE_SIZE,
    _SC_LEVEL3_CACHE_ASSOC,
    _SC_LEVEL3_CACHE_LINESIZE,
    _SC_LEVEL4_CACHE_SIZE,
    _SC_LEVEL4_CACHE_ASSOC,
    _SC_LEVEL4_CACHE_LINESIZE,
    _SC_IPV6 = _SC_LEVEL1_ICACHE_SIZE + 50,
    _SC_RAW_SOCKETS,
    _SC_V7_ILP32_OFF32,
    _SC_V7_ILP32_OFFBIG,
    _SC_V7_LP64_OFF64,
    _SC_V7_LPBIG_OFFBIG,
    _SC_SS_REPL_MAX,
    _SC_TRACE_EVENT_NAME_MAX,
    _SC_TRACE_NAME_MAX,
    _SC_TRACE_SYS_MAX,
    _SC_TRACE_USER_EVENT_MAX,
    _SC_XOPEN_STREAMS,
    _SC_THREAD_ROBUST_PRIO_INHERIT,
    _SC_THREAD_ROBUST_PRIO_PROTECT
};
enum {
    _CS_PATH,
    _CS_V6_WIDTH_RESTRICTED_ENVS,
    _CS_GNU_LIBC_VERSION,
    _CS_GNU_LIBPTHREAD_VERSION,
    _CS_V5_WIDTH_RESTRICTED_ENVS,
    _CS_V7_WIDTH_RESTRICTED_ENVS,
    _CS_LFS_CFLAGS = 1000,
    _CS_LFS_LDFLAGS,
    _CS_LFS_LIBS,
    _CS_LFS_LINTFLAGS,
    _CS_LFS64_CFLAGS,
    _CS_LFS64_LDFLAGS,
    _CS_LFS64_LIBS,
    _CS_LFS64_LINTFLAGS,
    _CS_XBS5_ILP32_OFF32_CFLAGS = 1100,
    _CS_XBS5_ILP32_OFF32_LDFLAGS,
    _CS_XBS5_ILP32_OFF32_LIBS,
    _CS_XBS5_ILP32_OFF32_LINTFLAGS,
    _CS_XBS5_ILP32_OFFBIG_CFLAGS,
    _CS_XBS5_ILP32_OFFBIG_LDFLAGS,
    _CS_XBS5_ILP32_OFFBIG_LIBS,
    _CS_XBS5_ILP32_OFFBIG_LINTFLAGS,
    _CS_XBS5_LP64_OFF64_CFLAGS,
    _CS_XBS5_LP64_OFF64_LDFLAGS,
    _CS_XBS5_LP64_OFF64_LIBS,
    _CS_XBS5_LP64_OFF64_LINTFLAGS,
    _CS_XBS5_LPBIG_OFFBIG_CFLAGS,
    _CS_XBS5_LPBIG_OFFBIG_LDFLAGS,
    _CS_XBS5_LPBIG_OFFBIG_LIBS,
    _CS_XBS5_LPBIG_OFFBIG_LINTFLAGS,
    _CS_POSIX_V6_ILP32_OFF32_CFLAGS,
    _CS_POSIX_V6_ILP32_OFF32_LDFLAGS,
    _CS_POSIX_V6_ILP32_OFF32_LIBS,
    _CS_POSIX_V6_ILP32_OFF32_LINTFLAGS,
    _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS,
    _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS,
    _CS_POSIX_V6_ILP32_OFFBIG_LIBS,
    _CS_POSIX_V6_ILP32_OFFBIG_LINTFLAGS,
    _CS_POSIX_V6_LP64_OFF64_CFLAGS,
    _CS_POSIX_V6_LP64_OFF64_LDFLAGS,
    _CS_POSIX_V6_LP64_OFF64_LIBS,
    _CS_POSIX_V6_LP64_OFF64_LINTFLAGS,
    _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS,
    _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS,
    _CS_POSIX_V6_LPBIG_OFFBIG_LIBS,
    _CS_POSIX_V6_LPBIG_OFFBIG_LINTFLAGS,
    _CS_POSIX_V7_ILP32_OFF32_CFLAGS,
    _CS_POSIX_V7_ILP32_OFF32_LDFLAGS,
    _CS_POSIX_V7_ILP32_OFF32_LIBS,
    _CS_POSIX_V7_ILP32_OFF32_LINTFLAGS,
    _CS_POSIX_V7_ILP32_OFFBIG_CFLAGS,
    _CS_POSIX_V7_ILP32_OFFBIG_LDFLAGS,
    _CS_POSIX_V7_ILP32_OFFBIG_LIBS,
    _CS_POSIX_V7_ILP32_OFFBIG_LINTFLAGS,
    _CS_POSIX_V7_LP64_OFF64_CFLAGS,
    _CS_POSIX_V7_LP64_OFF64_LDFLAGS,
    _CS_POSIX_V7_LP64_OFF64_LIBS,
    _CS_POSIX_V7_LP64_OFF64_LINTFLAGS,
    _CS_POSIX_V7_LPBIG_OFFBIG_CFLAGS,
    _CS_POSIX_V7_LPBIG_OFFBIG_LDFLAGS,
    _CS_POSIX_V7_LPBIG_OFFBIG_LIBS,
    _CS_POSIX_V7_LPBIG_OFFBIG_LINTFLAGS,
    _CS_V6_ENV,
    _CS_V7_ENV
};
extern long int pathconf(const char * __path,
                         int __name) __attribute__((__nothrow__,__nonnull__(1)));
extern long int fpathconf(int __fd,
                          int __name) __attribute__((__nothrow__));
extern long int sysconf(int __name) __attribute__((__nothrow__));
extern size_t confstr(int __name,
                      char * __buf,
                      size_t __len) __attribute__((__nothrow__));
extern __pid_t getpid(void) __attribute__((__nothrow__));
extern __pid_t getppid(void) __attribute__((__nothrow__));
extern __pid_t getpgrp(void) __attribute__((__nothrow__));
extern __pid_t __getpgid(__pid_t __pid) __attribute__((__nothrow__));
extern __pid_t getpgid(__pid_t __pid) __attribute__((__nothrow__));
extern int setpgid(__pid_t __pid,
                   __pid_t __pgid) __attribute__((__nothrow__));
extern int setpgrp(void) __attribute__((__nothrow__));
extern __pid_t setsid(void) __attribute__((__nothrow__));
extern __pid_t getsid(__pid_t __pid) __attribute__((__nothrow__));
extern __uid_t getuid(void) __attribute__((__nothrow__));
extern __uid_t geteuid(void) __attribute__((__nothrow__));
extern __gid_t getgid(void) __attribute__((__nothrow__));
extern __gid_t getegid(void) __attribute__((__nothrow__));
extern int getgroups(int __size,
                     __gid_t __list[]) __attribute__((__nothrow__));
extern int setuid(__uid_t __uid) __attribute__((__nothrow__));
extern int setreuid(__uid_t __ruid,
                    __uid_t __euid) __attribute__((__nothrow__));
extern int seteuid(__uid_t __uid) __attribute__((__nothrow__));
extern int setgid(__gid_t __gid) __attribute__((__nothrow__));
extern int setregid(__gid_t __rgid,
                    __gid_t __egid) __attribute__((__nothrow__));
extern int setegid(__gid_t __gid) __attribute__((__nothrow__));
extern __pid_t fork(void) __attribute__((__nothrow__));
extern __pid_t vfork(void) __attribute__((__nothrow__));
extern char * ttyname(int __fd) __attribute__((__nothrow__));
extern int ttyname_r(int __fd,
                     char * __buf,
                     size_t __buflen) __attribute__((__nothrow__,__nonnull__(2)));
extern int isatty(int __fd) __attribute__((__nothrow__));
extern int ttyslot(void) __attribute__((__nothrow__));
extern int link(const char * __from,
                const char * __to) __attribute__((__nothrow__,__nonnull__(1, 2)));
extern int linkat(int __fromfd,
                  const char * __from,
                  int __tofd,
                  const char * __to,
                  int __flags) __attribute__((__nothrow__,__nonnull__(2, 4)));
extern int symlink(const char * __from,
                   const char * __to) __attribute__((__nothrow__,__nonnull__(1, 2)));
extern ssize_t readlink(const char * __restrict __path,
                        char * __restrict __buf,
                        size_t __len) __attribute__((__nothrow__,__nonnull__(1, 2)));
extern int symlinkat(const char * __from,
                     int __tofd,
                     const char * __to) __attribute__((__nothrow__,__nonnull__(1, 3)));
extern ssize_t readlinkat(int __fd,
                          const char * __restrict __path,
                          char * __restrict __buf,
                          size_t __len) __attribute__((__nothrow__,__nonnull__(2, 3)));
extern int unlink(const char * __name) __attribute__((__nothrow__,__nonnull__(1)));
extern int unlinkat(int __fd,
                    const char * __name,
                    int __flag) __attribute__((__nothrow__,__nonnull__(2)));
extern int rmdir(const char * __path) __attribute__((__nothrow__,__nonnull__(1)));
extern __pid_t tcgetpgrp(int __fd) __attribute__((__nothrow__));
extern int tcsetpgrp(int __fd,
                     __pid_t __pgrp_id) __attribute__((__nothrow__));
extern char * getlogin(void);
extern int getlogin_r(char * __name,
                      size_t __name_len) __attribute__((__nonnull__(1)));
extern int setlogin(const char * __name) __attribute__((__nothrow__,__nonnull__(1)));
extern char * optarg;
extern int optind;
extern int opterr;
extern int optopt;
extern int getopt(int ___argc,
                  char * const * ___argv,
                  const char * __shortopts) __attribute__((__nothrow__));
extern int gethostname(char * __name,
                       size_t __len) __attribute__((__nothrow__,__nonnull__(1)));
extern int sethostname(const char * __name,
                       size_t __len) __attribute__((__nothrow__,__nonnull__(1)));
extern int sethostid(long int __id) __attribute__((__nothrow__));
extern int getdomainname(char * __name,
                         size_t __len) __attribute__((__nothrow__,__nonnull__(1)));
extern int setdomainname(const char * __name,
                         size_t __len) __attribute__((__nothrow__,__nonnull__(1)));
extern int vhangup(void) __attribute__((__nothrow__));
extern int revoke(const char * __file) __attribute__((__nothrow__,__nonnull__(1)));
extern int profil(unsigned short int * __sample_buffer,
                  size_t __size,
                  size_t __offset,
                  unsigned int __scale) __attribute__((__nothrow__,__nonnull__(1)));
extern int acct(const char * __name) __attribute__((__nothrow__));
extern char * getusershell(void) __attribute__((__nothrow__));
extern void endusershell(void) __attribute__((__nothrow__));
extern void setusershell(void) __attribute__((__nothrow__));
extern int daemon(int __nochdir,
                  int __noclose) __attribute__((__nothrow__));
extern int chroot(const char * __path) __attribute__((__nothrow__,__nonnull__(1)));
extern char * getpass(const char * __prompt) __attribute__((__nonnull__(1)));
extern int fsync(int __fd);
extern long int gethostid(void);
extern void sync(void) __attribute__((__nothrow__));
extern int getpagesize(void) __attribute__((__nothrow__,const));
extern int getdtablesize(void) __attribute__((__nothrow__));
extern int truncate(const char * __file,
                    __off_t __length) __attribute__((__nothrow__,__nonnull__(1)));
extern int ftruncate(int __fd,
                     __off_t __length) __attribute__((__nothrow__));
extern int brk(void * __addr) __attribute__((__nothrow__));
extern void * sbrk(intptr_t __delta) __attribute__((__nothrow__));
extern long int syscall(long int __sysno, ...) __attribute__((__nothrow__));
extern int lockf(int __fd, int __cmd, __off_t __len);
extern int fdatasync(int __fildes);
extern char * ctermid(char * __s) __attribute__((__nothrow__));
typedef enum {
            SUCCESS = 0, FAIL = 1
        } error_t;
typedef enum {
            READ = 1, WRITE = 2, READ_WRITE = 3
        } Mode;
typedef uint32_t sensor_val_t;
uint32_t os_now();
int os_listen(const char * address);
int os_connect(const char * address);
int os_flash_open(const char * address, Mode mode);
int os_sensor_open(const char * address);
error_t os_flash_seek(int handle, int offset);
void tc_init();
void tc_run();
int tc_run_thread(void (* thread_start_function)());
void tc_join_thread(int handle);
extern void __assert_fail(const char * __assertion,
                          const char * __file,
                          unsigned int __line,
                          const char * __function) __attribute__((__nothrow__,__noreturn__));
extern void __assert_perror_fail(int __errnum,
                                 const char * __file,
                                 unsigned int __line,
                                 const char * __function) __attribute__((__nothrow__,__noreturn__));
extern void __assert(const char * __assertion,
                     const char * __file,
                     int __line) __attribute__((__nothrow__,__noreturn__));
void * memcpy(void * dest, const void * src, size_t n);
typedef struct {
            void * ec_cont;
            error_t ec_result;
            uint8_t * buffer;
            int handle;
            size_t len;
        } ec_frame_tc_flash_write_t;
typedef struct {
            void * ec_cont;
            union {
                ec_frame_tc_flash_write_t tc_flash_write;
            } ec_frames;
            uint8_t * buf;
            error_t ec_tmp_0;
            size_t len;
            int log;
        } ec_frame_log_to_t;
typedef struct {
            void * ec_cont;
            error_t ec_result;
            int handle;
            sensor_val_t * value;
        } ec_frame_tc_sensor_read_t;
typedef struct {
            void * ec_cont; error_t ec_result; uint32_t ms;
        } ec_frame_tc_sleep_t;
typedef struct {
            void * ec_cont;
            union {
                ec_frame_tc_sleep_t tc_sleep;
                ec_frame_tc_sensor_read_t tc_sensor_read;
                ec_frame_log_to_t log_to;
            } ec_frames;
            const char * device;
            unsigned dt;
            error_t ec_tmp_0;
            const char * file;
            int log;
            uint32_t now;
            int sensor;
            sensor_val_t val;
        } ec_frame_collect_run_t;
typedef struct {
            union {
                ec_frame_collect_run_t collect_run;
            } ec_frames;
        } ec_frame_task_collect_t;
typedef struct {
            void * ec_cont;
            error_t ec_result;
            uint8_t * buffer;
            size_t buflen;
            int handle;
            size_t * len;
        } ec_frame_tc_receive_t;
typedef struct {
            void * ec_cont;
            union {
                ec_frame_tc_receive_t tc_receive; ec_frame_log_to_t log_to;
            } ec_frames;
            uint8_t buffer[100];
            const char * channel;
            error_t ec_tmp_0;
            const char * file;
            size_t len;
            int log;
            int socket;
        } ec_frame_receive_run_t;
typedef struct {
            union {
                ec_frame_receive_run_t receive_run;
            } ec_frames;
        } ec_frame_task_receive_t;
typedef struct {
            void * ec_cont;
            error_t ec_result;
            uint8_t * buffer;
            int handle;
            size_t len;
        } ec_frame_tc_send_t;
typedef struct {
            void * ec_cont;
            error_t ec_result;
            uint8_t * buffer;
            size_t buflen;
            int handle;
            size_t * len;
        } ec_frame_tc_flash_read_t;
typedef struct {
            void * ec_cont;
            union {
                ec_frame_tc_send_t tc_send;
            } ec_frames;
            error_t ec_tmp_0;
            int32_t max;
            int32_t min;
            uint8_t payload[2 * sizeof(int32_t)];
            int socket;
        } ec_frame_send_via_t;
typedef struct {
            void * ec_cont;
            union {
                ec_frame_tc_flash_read_t tc_flash_read;
            } ec_frames;
            uint8_t buffer[100];
            int i;
            size_t len;
            int log;
            int32_t * max;
            int32_t * min;
            error_t result;
            int32_t tmp;
        } ec_frame_aggregate_from_t;
typedef struct {
            void * ec_cont;
            union {
                ec_frame_tc_sleep_t tc_sleep;
                ec_frame_aggregate_from_t aggregate_from;
                ec_frame_send_via_t send_via;
            } ec_frames;
            const char * channel;
            unsigned dt;
            const char * file1;
            const char * file2;
            int log1;
            int log2;
            int32_t max;
            int32_t min;
            uint32_t now;
            int socket;
        } ec_frame_send_run_t;
typedef struct {
            union {
                ec_frame_send_run_t send_run;
            } ec_frames;
        } ec_frame_task_send_t;
ec_frame_task_collect_t ec_stack_task_collect;
ec_frame_task_receive_t ec_stack_task_receive;
ec_frame_task_send_t ec_stack_task_send;
void tc_sleep(ec_frame_tc_sleep_t * frame);
void tc_receive(ec_frame_tc_receive_t * frame);
void tc_send(ec_frame_tc_send_t * frame);
void tc_flash_read(ec_frame_tc_flash_read_t * frame);
void tc_flash_write(ec_frame_tc_flash_write_t * frame);
void tc_sensor_read(ec_frame_tc_sensor_read_t * frame);
void ec_thread_1(void * ec_cont)
{
    if (ec_cont != NULL)
    {
        goto * ec_cont;
    }
    ec_stack_task_collect.ec_frames.collect_run.device = "sensor";
    ec_stack_task_collect.ec_frames.collect_run.file = "sensor_log";
    ec_stack_task_collect.ec_frames.collect_run.dt = 50;
    ec_stack_task_collect.ec_frames.collect_run.ec_cont = &&ec_label_task_collect_1;
    goto ec_label_collect_run_0;
ec_label_task_collect_1:
    ;
    0 ? (void) 0 : __assert_fail("0",
                                 "tc.c",
                                 111,
                                 __PRETTY_FUNCTION__);
    return;
ec_label_collect_run_0:
    ;
    ec_stack_task_collect.ec_frames.collect_run.sensor = os_sensor_open(ec_stack_task_collect.ec_frames.collect_run.device);
    ec_stack_task_collect.ec_frames.collect_run.log = os_flash_open(ec_stack_task_collect.ec_frames.collect_run.file,
                                                                    WRITE);
    ec_stack_task_collect.ec_frames.collect_run.now = os_now();
    while (1)
    {
        ec_stack_task_collect.ec_frames.collect_run.ec_frames.tc_sleep.ms = ec_stack_task_collect.ec_frames.collect_run.now + ec_stack_task_collect.ec_frames.collect_run.dt;
        ec_stack_task_collect.ec_frames.collect_run.ec_frames.tc_sleep.ec_cont = &&ec_label_collect_run_2;
        tc_sleep(&ec_stack_task_collect.ec_frames.collect_run.ec_frames.tc_sleep);
        return;
    ec_label_collect_run_2:
        ;
        ec_stack_task_collect.ec_frames.collect_run.now += ec_stack_task_collect.ec_frames.collect_run.dt;
        while (1)
        {
            ec_stack_task_collect.ec_frames.collect_run.ec_frames.tc_sensor_read.handle = ec_stack_task_collect.ec_frames.collect_run.sensor;
            ec_stack_task_collect.ec_frames.collect_run.ec_frames.tc_sensor_read.value = &ec_stack_task_collect.ec_frames.collect_run.val;
            ec_stack_task_collect.ec_frames.collect_run.ec_frames.tc_sensor_read.ec_cont = &&ec_label_collect_run_1;
            tc_sensor_read(&ec_stack_task_collect.ec_frames.collect_run.ec_frames.tc_sensor_read);
            return;
        ec_label_collect_run_1:
            ;
            ec_stack_task_collect.ec_frames.collect_run.ec_tmp_0 = ec_stack_task_collect.ec_frames.collect_run.ec_frames.tc_sensor_read.ec_result;
            if (!(ec_stack_task_collect.ec_frames.collect_run.ec_tmp_0 != SUCCESS))
            {
                break;
            }
            ;
        }
        ec_stack_task_collect.ec_frames.collect_run.ec_frames.log_to.log = ec_stack_task_collect.ec_frames.collect_run.log;
        ec_stack_task_collect.ec_frames.collect_run.ec_frames.log_to.buf = (uint8_t *) &ec_stack_task_collect.ec_frames.collect_run.val;
        ec_stack_task_collect.ec_frames.collect_run.ec_frames.log_to.len = sizeof(sensor_val_t);
        ec_stack_task_collect.ec_frames.collect_run.ec_frames.log_to.ec_cont = &&ec_label_collect_run_3;
        goto ec_label_log_to_0;
    ec_label_collect_run_3:
        ;
    }
    goto * (ec_stack_task_collect.ec_frames.collect_run.ec_cont);
ec_label_log_to_0:
    ;
    while (1)
    {
        ec_stack_task_collect.ec_frames.collect_run.ec_frames.log_to.ec_frames.tc_flash_write.handle = ec_stack_task_collect.ec_frames.collect_run.ec_frames.log_to.log;
        ec_stack_task_collect.ec_frames.collect_run.ec_frames.log_to.ec_frames.tc_flash_write.buffer = ec_stack_task_collect.ec_frames.collect_run.ec_frames.log_to.buf;
        ec_stack_task_collect.ec_frames.collect_run.ec_frames.log_to.ec_frames.tc_flash_write.len = ec_stack_task_collect.ec_frames.collect_run.ec_frames.log_to.len;
        ec_stack_task_collect.ec_frames.collect_run.ec_frames.log_to.ec_frames.tc_flash_write.ec_cont = &&ec_label_log_to_1;
        tc_flash_write(&ec_stack_task_collect.ec_frames.collect_run.ec_frames.log_to.ec_frames.tc_flash_write);
        return;
    ec_label_log_to_1:
        ;
        ec_stack_task_collect.ec_frames.collect_run.ec_frames.log_to.ec_tmp_0 = ec_stack_task_collect.ec_frames.collect_run.ec_frames.log_to.ec_frames.tc_flash_write.ec_result;
        if (!(ec_stack_task_collect.ec_frames.collect_run.ec_frames.log_to.ec_tmp_0 != SUCCESS))
        {
            break;
        }
        ;
    }
    goto * (ec_stack_task_collect.ec_frames.collect_run.ec_frames.log_to.ec_cont);
}
void ec_thread_2(void * ec_cont)
{
    if (ec_cont != NULL)
    {
        goto * ec_cont;
    }
    ec_stack_task_receive.ec_frames.receive_run.channel = "child";
    ec_stack_task_receive.ec_frames.receive_run.file = "receive_log";
    ec_stack_task_receive.ec_frames.receive_run.ec_cont = &&ec_label_task_receive_1;
    goto ec_label_receive_run_0;
ec_label_task_receive_1:
    ;
    0 ? (void) 0 : __assert_fail("0",
                                 "tc.c",
                                 105,
                                 __PRETTY_FUNCTION__);
    return;
ec_label_receive_run_0:
    ;
    ec_stack_task_receive.ec_frames.receive_run.socket = os_listen(ec_stack_task_receive.ec_frames.receive_run.channel);
    ec_stack_task_receive.ec_frames.receive_run.log = os_flash_open(ec_stack_task_receive.ec_frames.receive_run.file,
                                                                    WRITE);
    while (1)
    {
        while (1)
        {
            ec_stack_task_receive.ec_frames.receive_run.ec_frames.tc_receive.handle = ec_stack_task_receive.ec_frames.receive_run.socket;
            ec_stack_task_receive.ec_frames.receive_run.ec_frames.tc_receive.buffer = ec_stack_task_receive.ec_frames.receive_run.buffer;
            ec_stack_task_receive.ec_frames.receive_run.ec_frames.tc_receive.buflen = sizeof(ec_stack_task_receive.ec_frames.receive_run.buffer);
            ec_stack_task_receive.ec_frames.receive_run.ec_frames.tc_receive.len = &ec_stack_task_receive.ec_frames.receive_run.len;
            ec_stack_task_receive.ec_frames.receive_run.ec_frames.tc_receive.ec_cont = &&ec_label_receive_run_1;
            tc_receive(&ec_stack_task_receive.ec_frames.receive_run.ec_frames.tc_receive);
            return;
        ec_label_receive_run_1:
            ;
            ec_stack_task_receive.ec_frames.receive_run.ec_tmp_0 = ec_stack_task_receive.ec_frames.receive_run.ec_frames.tc_receive.ec_result;
            if (!(ec_stack_task_receive.ec_frames.receive_run.ec_tmp_0 != SUCCESS))
            {
                break;
            }
            ;
        }
        ec_stack_task_receive.ec_frames.receive_run.ec_frames.log_to.log = ec_stack_task_receive.ec_frames.receive_run.log;
        ec_stack_task_receive.ec_frames.receive_run.ec_frames.log_to.buf = ec_stack_task_receive.ec_frames.receive_run.buffer;
        ec_stack_task_receive.ec_frames.receive_run.ec_frames.log_to.len = ec_stack_task_receive.ec_frames.receive_run.len;
        ec_stack_task_receive.ec_frames.receive_run.ec_frames.log_to.ec_cont = &&ec_label_receive_run_2;
        goto ec_label_log_to_0;
    ec_label_receive_run_2:
        ;
    }
    goto * (ec_stack_task_receive.ec_frames.receive_run.ec_cont);
ec_label_log_to_0:
    ;
    while (1)
    {
        ec_stack_task_receive.ec_frames.receive_run.ec_frames.log_to.ec_frames.tc_flash_write.handle = ec_stack_task_receive.ec_frames.receive_run.ec_frames.log_to.log;
        ec_stack_task_receive.ec_frames.receive_run.ec_frames.log_to.ec_frames.tc_flash_write.buffer = ec_stack_task_receive.ec_frames.receive_run.ec_frames.log_to.buf;
        ec_stack_task_receive.ec_frames.receive_run.ec_frames.log_to.ec_frames.tc_flash_write.len = ec_stack_task_receive.ec_frames.receive_run.ec_frames.log_to.len;
        ec_stack_task_receive.ec_frames.receive_run.ec_frames.log_to.ec_frames.tc_flash_write.ec_cont = &&ec_label_log_to_1;
        tc_flash_write(&ec_stack_task_receive.ec_frames.receive_run.ec_frames.log_to.ec_frames.tc_flash_write);
        return;
    ec_label_log_to_1:
        ;
        ec_stack_task_receive.ec_frames.receive_run.ec_frames.log_to.ec_tmp_0 = ec_stack_task_receive.ec_frames.receive_run.ec_frames.log_to.ec_frames.tc_flash_write.ec_result;
        if (!(ec_stack_task_receive.ec_frames.receive_run.ec_frames.log_to.ec_tmp_0 != SUCCESS))
        {
            break;
        }
        ;
    }
    goto * (ec_stack_task_receive.ec_frames.receive_run.ec_frames.log_to.ec_cont);
}
void ec_thread_3(void * ec_cont)
{
    if (ec_cont != NULL)
    {
        goto * ec_cont;
    }
    ec_stack_task_send.ec_frames.send_run.channel = "parent";
    ec_stack_task_send.ec_frames.send_run.file1 = "receive_log";
    ec_stack_task_send.ec_frames.send_run.file2 = "sensor_log";
    ec_stack_task_send.ec_frames.send_run.dt = 100;
    ec_stack_task_send.ec_frames.send_run.ec_cont = &&ec_label_task_send_1;
    goto ec_label_send_run_0;
ec_label_task_send_1:
    ;
    0 ? (void) 0 : __assert_fail("0", "tc.c", 99, __PRETTY_FUNCTION__);
    return;
ec_label_send_run_0:
    ;
    ec_stack_task_send.ec_frames.send_run.log1 = os_flash_open(ec_stack_task_send.ec_frames.send_run.file1,
                                                               READ);
    ec_stack_task_send.ec_frames.send_run.log2 = os_flash_open(ec_stack_task_send.ec_frames.send_run.file2,
                                                               READ);
    ec_stack_task_send.ec_frames.send_run.socket = os_connect(ec_stack_task_send.ec_frames.send_run.channel);
    ec_stack_task_send.ec_frames.send_run.now = os_now();
    while (1)
    {
        ec_stack_task_send.ec_frames.send_run.ec_frames.tc_sleep.ms = ec_stack_task_send.ec_frames.send_run.now + ec_stack_task_send.ec_frames.send_run.dt;
        ec_stack_task_send.ec_frames.send_run.ec_frames.tc_sleep.ec_cont = &&ec_label_send_run_1;
        tc_sleep(&ec_stack_task_send.ec_frames.send_run.ec_frames.tc_sleep);
        return;
    ec_label_send_run_1:
        ;
        ec_stack_task_send.ec_frames.send_run.now += ec_stack_task_send.ec_frames.send_run.dt;
        ec_stack_task_send.ec_frames.send_run.min = 0x7fffffff;
        ec_stack_task_send.ec_frames.send_run.max = 0xffffffff;
        ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.log = ec_stack_task_send.ec_frames.send_run.log1;
        ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.min = &ec_stack_task_send.ec_frames.send_run.min;
        ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.max = &ec_stack_task_send.ec_frames.send_run.max;
        ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.ec_cont = &&ec_label_send_run_2;
        goto ec_label_aggregate_from_0;
    ec_label_send_run_2:
        ;
        ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.log = ec_stack_task_send.ec_frames.send_run.log2;
        ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.min = &ec_stack_task_send.ec_frames.send_run.min;
        ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.max = &ec_stack_task_send.ec_frames.send_run.max;
        ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.ec_cont = &&ec_label_send_run_3;
        goto ec_label_aggregate_from_0;
    ec_label_send_run_3:
        ;
        ec_stack_task_send.ec_frames.send_run.ec_frames.send_via.socket = ec_stack_task_send.ec_frames.send_run.socket;
        ec_stack_task_send.ec_frames.send_run.ec_frames.send_via.min = ec_stack_task_send.ec_frames.send_run.min;
        ec_stack_task_send.ec_frames.send_run.ec_frames.send_via.max = ec_stack_task_send.ec_frames.send_run.max;
        ec_stack_task_send.ec_frames.send_run.ec_frames.send_via.ec_cont = &&ec_label_send_run_4;
        goto ec_label_send_via_0;
    ec_label_send_run_4:
        ;
    }
    goto * (ec_stack_task_send.ec_frames.send_run.ec_cont);
ec_label_aggregate_from_0:
    ;
    ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.ec_frames.tc_flash_read.handle = ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.log;
    ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.ec_frames.tc_flash_read.buffer = ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.buffer;
    ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.ec_frames.tc_flash_read.buflen = sizeof(ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.buffer);
    ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.ec_frames.tc_flash_read.len = &ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.len;
    ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.ec_frames.tc_flash_read.ec_cont = &&ec_label_aggregate_from_1;
    tc_flash_read(&ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.ec_frames.tc_flash_read);
    return;
ec_label_aggregate_from_1:
    ;
    ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.result = ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.ec_frames.tc_flash_read.ec_result;
    ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.result == SUCCESS ? (void) 0 : __assert_fail("result == SUCCESS",
                                                                                                                "tc.c",
                                                                                                                45,
                                                                                                                __PRETTY_FUNCTION__);
    ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.len % sizeof(int32_t) == 0 ? (void) 0 : __assert_fail("(len % sizeof(int32_t)) == 0",
                                                                                                                         "tc.c",
                                                                                                                         46,
                                                                                                                         __PRETTY_FUNCTION__);
    ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.len < sizeof(ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.buffer) ? (void) 0 : __assert_fail("len < sizeof(buffer)",
                                                                                                                                                                                  "tc.c",
                                                                                                                                                                                  47,
                                                                                                                                                                                  __PRETTY_FUNCTION__);
    ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.result = os_flash_seek(ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.log,
                                                                                          0);
    ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.result == SUCCESS ? (void) 0 : __assert_fail("result == SUCCESS",
                                                                                                                "tc.c",
                                                                                                                50,
                                                                                                                __PRETTY_FUNCTION__);
    for (ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.i = 0; ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.i < ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.len / sizeof(int32_t); ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.i++)
    {
        memcpy(&ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.tmp,
               ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.buffer + ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.i * sizeof(int32_t),
               sizeof(int32_t));
        if (ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.tmp < *ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.min)
        {
            *ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.min = ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.tmp;
        }
        if (ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.tmp > *ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.max)
        {
            *ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.max = ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.tmp;
        }
    }
    goto * (ec_stack_task_send.ec_frames.send_run.ec_frames.aggregate_from.ec_cont);
ec_label_send_via_0:
    ;
    memcpy(ec_stack_task_send.ec_frames.send_run.ec_frames.send_via.payload,
           &ec_stack_task_send.ec_frames.send_run.ec_frames.send_via.min,
           sizeof(int32_t));
    memcpy(ec_stack_task_send.ec_frames.send_run.ec_frames.send_via.payload + sizeof(int32_t),
           &ec_stack_task_send.ec_frames.send_run.ec_frames.send_via.max,
           sizeof(int32_t));
    while (1)
    {
        ec_stack_task_send.ec_frames.send_run.ec_frames.send_via.ec_frames.tc_send.handle = ec_stack_task_send.ec_frames.send_run.ec_frames.send_via.socket;
        ec_stack_task_send.ec_frames.send_run.ec_frames.send_via.ec_frames.tc_send.buffer = ec_stack_task_send.ec_frames.send_run.ec_frames.send_via.payload;
        ec_stack_task_send.ec_frames.send_run.ec_frames.send_via.ec_frames.tc_send.len = sizeof(ec_stack_task_send.ec_frames.send_run.ec_frames.send_via.payload);
        ec_stack_task_send.ec_frames.send_run.ec_frames.send_via.ec_frames.tc_send.ec_cont = &&ec_label_send_via_1;
        tc_send(&ec_stack_task_send.ec_frames.send_run.ec_frames.send_via.ec_frames.tc_send);
        return;
    ec_label_send_via_1:
        ;
        ec_stack_task_send.ec_frames.send_run.ec_frames.send_via.ec_tmp_0 = ec_stack_task_send.ec_frames.send_run.ec_frames.send_via.ec_frames.tc_send.ec_result;
        if (!(ec_stack_task_send.ec_frames.send_run.ec_frames.send_via.ec_tmp_0 != SUCCESS))
        {
            break;
        }
        ;
    }
    goto * (ec_stack_task_send.ec_frames.send_run.ec_frames.send_via.ec_cont);
}
