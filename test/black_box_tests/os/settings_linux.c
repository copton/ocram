#include "settings.h"

#include "logger.h"

#include <stdlib.h>

int32_t settings_dispatcher_max_time()
{
    static int32_t result = -2;
    if (result == -2) {
        char* maxtimestr = getenv("EC_MAX_TIME");
        result = -1;                                                                                              
        if (maxtimestr == 0) {
           logger_log(WARNING, "program runs for ever. Set EC_MAX_TIME to force program termination eventually.");

        } else {
            result = strtol(maxtimestr, NULL, 10);
            if (result <= 0) {
                logger_log(ERROR, "EC_MAX_TIME must be a positive, non-zero integeger.");
                exit(1);
            } else {
                logger_log(INFO, "using EC_MAX_TIME %u", result);
            }
        }
    }
    return result;
}

unsigned int settings_random_seed()
{
    static int result = -1;
    if (result == -1) {  
        char* seedstr= getenv("EC_RANDOM_SEED");
        if (seedstr == 0) {
            logger_log(WARNING, "using default seed. Set EC_RANDOM_SEED environment variable to overwrite.");
            result = 0;
        } else {
            long int seed = strtol(seedstr, NULL, 10);
            if (seed < 0) {
                logger_log(ERROR, "EC_RANDOM_SEED must be a positive integeger.");
                exit(1);
            } else {
                result = seed;
            }
            logger_log(INFO, "using EC_RANDOM_SEED %u", result);
        }   
    }
    return result;
}

const char* settings_logger_file()
{
    static const char* result = NULL;
    if (result == NULL) {
        const char* logFile = getenv("EC_LOG_FILE");
        if (logFile == NULL) {
            result = "/tmp/ec.log";
            logger_log(WARNING, "using default log file '%s'. Set EC_LOG_FILE environment variable to overwrite.", result);
        } else {
            result = logFile;
            logger_log(INFO,"using EC_LOG_FILE '%s'", result);
        }
    }
    return result;
}
